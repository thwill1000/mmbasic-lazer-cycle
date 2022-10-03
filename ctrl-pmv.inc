' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For PicoMite VGA, MMBasic 5.07.05
'
' PicoGAME / PicoMiteVGA controller code.
'
' Note 1:
'   This code is written for readability, not for performance.
'   If necessary it can be made faster by:
'    - inlining constants to avoid overhead of "variable" lookup,
'      this will also reduce the amount of RAM required.
'    - unrolling FOR loops.
'    - shortening variable names.
'    - using different functions to read ports A and B so that pin numbers
'      can be hard-coded.
'    - in extremis inlining the controller read functions to avoid the overhead
'      of a subroutine call.
'
' Note 2:
'   Variables with CAPITALISED names are constants, this includes arrays
'   which have to be declared with DIM and where constant-ness is not enforced
'   by the MMBasic interpreter.

If Mm.Info(Option Base) <> 0 Then Error "OPTION BASE 0 required"

' Button values as returned by controller read functions.
Const BTN_A%      = 1  ' FIRE on joystick and "B" on SNES.
Const BTN_B%      = 2  ' "Y" on SNES.
Const BTN_SELECT% = 4
Const BTN_START%  = 8
Const BTN_UP%     = 16
Const BTN_DOWN%   = 32
Const BTN_LEFT%   = 64
Const BTN_RIGHT%  = 128
Const BTN_C%      = 256 ' "A" on SNES.
Const BTN_D%      = 512 ' "X" on SENS.
Const BTN_E%      = 1204 ' Left-Bumper on SNES.
Const BTN_F%      = 2048 ' Right-Bumper on SNES.

If InStr(Mm.Device$, "PicoMite") > 0 Then

' Pin mappings for ATARI 1-button joystick.
Dim ATARI_FIRE_PIN%(1)  = ( Mm.Info(PinNo GP14), Mm.Info(PinNo GP28) )
Dim ATARI_UP_PIN%(1)    = ( Mm.Info(PinNo  GP0), Mm.Info(PinNo GP28) )
Dim ATARI_DOWN_PIN%(1)  = ( Mm.Info(PinNo  GP1), Mm.Info(PinNo  GP1) )
Dim ATARI_LEFT_PIN%(1)  = ( Mm.Info(PinNo  GP2), Mm.Info(PinNo  GP2) )
Dim ATARI_RIGHT_PIN%(1) = ( Mm.Info(PinNo  GP3), Mm.Info(PinNo  GP3) )

' Pin mappings for NES/SNES gamepad.
Dim BTN_CLOCK_PIN%(1) = ( Mm.Info(PinNo  GP3), Mm.Info(PinNo GP22) )
Dim BTN_DATA_PIN%(1)  = ( Mm.Info(PinNo  GP1), Mm.Info(PinNo  GP4) )
Dim BTN_LATCH_PIN%(1) = ( Mm.Info(PinNo  GP2), Mm.Info(PinNo  GP5) )

EndIf

' The NES standard specifies a 12 micro-second pulse, but all the controllers
' I've tested work with 1 micro-second, and possibly less.
Const BTN_PULSE_DURATION! = 0.001 ' 1 micro-second

Dim CONTROLLER_IDS$(3) Length 8 = ("keys_cursor%", "atari%", "nes%", "snes%")
Dim CONTROLLER_NAMES$(3) Length 15 = ("Cursor Keys", "Atari Joystick", "NES Gamepad" , "SNES Gamepad")

' When a key is pressed ctrl.on_key() sets the corresponding byte of this
' 256-byte map to 1. When ctrl.keydown(i%) is called the corresponding
' byte is read and set to 0. Note that a 256-bit map could be used but would
' be slower.
Dim ctrl.key_map%(31)

' +----------------------------------------------------------------------------+
' |                         READING GAME CONTROLLERS                           |
' +----------------------------------------------------------------------------+

' Initialises keyboard reading via ON KEY.
Sub ctrl.init_keys()
  ctrl.term_keys()
  On Key ctrl.on_key()
End Sub

' Terminates keyboard reading via ON_KEY.
Sub ctrl.term_keys()
  On Key 0
  Do While Inkey$ <> "" : Loop
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
End Sub

Sub ctrl.on_key()
  Local ch$ = Inkey$
  If ch$ <> "" Then Poke Var ctrl.key_map%(), Asc(ch$), 1
End Sub

Function ctrl.keydown%(i%)
  ctrl.keydown% = Peek(Var ctrl.key_map%(), i%)
  Poke Var ctrl.key_map%(), i%, 0
End Function

' Calls xxx_open() subroutine for the named controller if it exists.
Sub ctrl.open(ctrl$)
  Local open_fn$ = Choice(Mid$(ctrl$, Len(ctrl$)) = "%", Left$(ctrl$, Len(ctrl$) - 1), ctrl$)
  Cat open_fn$, "_open"
  On Error Skip 1
  Call open_fn$
  If Mm.ErrMsg$ <> "" Then
    If Mm.ErrMsg$ <> "Unknown user subroutine" Then Error Mm.ErrMsg$
  EndIf
End Sub

' Calls xxx_close() subroutine for the named controller if it exists.
Sub ctrl.close(ctrl$)
  Local open_fn$ = Choice(Mid$(ctrl$, Len(ctrl$)) = "%", Left$(ctrl$, Len(ctrl$) - 1), ctrl$)
  Cat open_fn$, "_close"
  On Error Skip 1
  Call open_fn$
  If Mm.ErrMsg$ <> "" Then
    If Mm.ErrMsg$ <> "Unknown user subroutine" Then Error Mm.ErrMsg$
  EndIf
End Sub

' Waits a specified duration for the user to press START on a (S)NES gamepad
' connected to Port A, or FIRE on an ATARI joystick connected to Port A or
' SPACE on the keyboard.
'
' Note that this function deliberately does not clear the keyboard buffer before
' checking to see if SPACE has been pressed; if desired this can be done by
' calling ctrl.clear_keys().
'
' @param   duration%  duration in milliseconds; if 0 then indefinite.
' @return             "nes_a%" if the (S)NES START button is pressed,
'                     or "atari_a%" if the ATARI FIRE button is pressed,
'                     or "keys_cursor%" if the SPACE key is pressed,
'                     or "" if none of the above before the duration expires.
Function poll_for_controller$(duration%)
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF)

  ' Just check the keyboard if we are not on a PicoMite.
  If InStr(Mm.Device$, "PicoMite") = 0 Then
    Do While poll_for_controller$ = "" And Timer < expires%)
      poll_for_controller$ = Choice(keys_cursor%() = BTN_A%, "keys_cursor%", "")
    Loop
    Exit Function
  EndIf

  Local read_fn$(2) = ("nes_a%", "atari_a%", "keys_cursor%")
  Local expected%(2) = (BTN_START%, BTN_A%, BTN_A%)
  Local i%, t%
  Do
    For i% = 0 To Bound(read_fn$(), 1)
      ctrl.open(read_fn$(i%))
      t% = Min(expires%, Timer + 100)
      ' Try to read the controller multiple times within a 100ms window.
      Do
        If Call(read_fn$(i%)) = expected%(i%) Then
          poll_for_controller$ = read_fn$(i%)
          Exit Function
        EndIf
      Loop Until Timer >= t%
    Next
  Loop Until Timer >= expires%
End Function

' Opens port A connected to an ATARI 1-button joystick.
Sub atari_a_open()
  SetPin ATARI_UP_PIN%(0), DIn
  SetPin ATARI_DOWN_PIN%(0), DIn
  SetPin ATARI_LEFT_PIN%(0), DIn
  SetPin ATARI_RIGHT_PIN%(0), DIn
  SetPin ATARI_FIRE_PIN%(0), DIn
End Sub

' Opens port B connected to an ATARI 1-button joystick.
Sub atari_b_open()
  SetPin ATARI_UP_PIN%(1), DIn
  SetPin ATARI_DOWN_PIN%(1), DIn
  SetPin ATARI_LEFT_PIN%(1), DIn
  SetPin ATARI_RIGHT_PIN%(1), DIn
  SetPin ATARI_FIRE_PIN%(1), DIn
End Sub

' Reads port A connected to an ATARI 1-button joystick.
'
' @return  bitmap corresponding to current controller state:
'            7 6 5 4 3 2 1 0
'            | | | | | | | |
'            | | | | | | | +-- Fire Button
'            | | | | | | +---- unused
'            | | | | | +------ unused
'            | | | | +-------- unused
'            | | | +---------- Up
'            | | +------------ Down
'            | +-------------- Left
'            +---------------- Right
Function atari_a%()
  Inc atari_a%, Not Pin(ATARI_FIRE_PIN%(0))
  Inc atari_a%, Not Pin(ATARI_UP_PIN%(0)) * 16
  Inc atari_a%, Not Pin(ATARI_DOWN_PIN%(0)) * 32
  Inc atari_a%, Not Pin(ATARI_LEFT_PIN%(0)) * 64
  Inc atari_a%, Not Pin(ATARI_RIGHT_PIN%(0)) * 128
End Function

' Reads port B connected to an ATARI 1-button joystick.
Function atari_b%()
  Inc atari_b%, Not Pin(ATARI_FIRE_PIN%(1))
  Inc atari_b%, Not Pin(ATARI_UP_PIN%(1)) * 16
  Inc atari_b%, Not Pin(ATARI_DOWN_PIN%(1)) * 32
  Inc atari_b%, Not Pin(ATARI_LEFT_PIN%(1)) * 64
  Inc atari_b%, Not Pin(ATARI_RIGHT_PIN%(1)) * 128
End Function

' Reads the keyboard as if it were a controller.
'
' Note that the PicoMite has no KEYDOWN function so we are limited to
' reading a single keypress from the input buffer and cannot handle multiple
' simultaneous keys or properly handle a key being pressed and not released.
'
' @return  bitmap corresponding to key read:
'            7 6 5 4 3 2 1 0
'            | | | | | | | |
'            | | | | | | | +-- Space
'            | | | | | | +---- unused
'            | | | | | +------ unused
'            | | | | +-------- unused
'            | | | +---------- Up
'            | | +------------ Down
'            | +-------------- Left
'            +---------------- Right
Function keys_cursor%()
  keys_cursor% = keys_cursor% Or ctrl.keydown%(32)  * BTN_A%
  keys_cursor% = keys_cursor% Or ctrl.keydown%(128) * BTN_UP%
  keys_cursor% = keys_cursor% Or ctrl.keydown%(129) * BTN_DOWN%
  keys_cursor% = keys_cursor% Or ctrl.keydown%(130) * BTN_LEFT%
  keys_cursor% = keys_cursor% Or ctrl.keydown%(131) * BTN_RIGHT%
End Function

' Opens port A connected to a SNES gamepad.
Sub snes_a_open()
  nes_a_open()
End Sub

' Opens port B connected to a SNES gamepad.
Sub snes_b_open()
  nes_b_open()
End Sub

' Reads port A connected to a SNES gamepad.
'
' @return  bitmap corresponding to current controller state:
'            11 10 9 8 7 6 5 4 3 2 1 0
'            |  |  | | | | | | | | | |
'            |  |  | | | | | | | | | +-- Button B
'            |  |  | | | | | | | | +---- Button Y
'            |  |  | | | | | | | +------ Select
'            |  |  | | | | | | +-------- Start
'            |  |  | | | | | +---------- Up
'            |  |  | | | | +------------ Down
'            |  |  | | | +-------------- Left
'            |  |  | | +---------------- Right
'            |  |  | +------------------ Button A
'            |  |  +-------------------- Button X
'            |  +----------------------- Left Bumper
'            +-------------------------- Right Bumper
Function snes_a%()
  Local i%
  Pulse BTN_LATCH_PIN%(0), BTN_PULSE_DURATION!
  For i% = 0 To 11
    Inc snes_a%, Not Pin(BTN_DATA_PIN%(0)) * 2^i%
    Pulse BTN_CLOCK_PIN%(0), BTN_PULSE_DURATION!
  Next
End Function

' Reads port B connected to a SNES gamepad.
Function snes_b%()
  Local i%
  Pulse BTN_LATCH_PIN%(1), BTN_PULSE_DURATION!
  For i% = 0 To 11
    Inc snes_b%, Not Pin(BTN_DATA_PIN%(1)) * 2^i%
    Pulse BTN_CLOCK_PIN%(1), BTN_PULSE_DURATION!
  Next
End Function

' Opens port A connected to a NES gamepad.
Sub nes_a_open()
  SetPin BTN_LATCH_PIN%(0), Dout
  SetPin BTN_CLOCK_PIN%(0), Dout
  SetPin BTN_DATA_PIN%(0), Din
  Pin(BTN_LATCH_PIN%(0)) = 0
  Pin(BTN_CLOCK_PIN%(0)) = 0
End Sub

' Opens port B connected to a NES gamepad.
Sub nes_b_open()
  SetPin BTN_LATCH_PIN%(1), Dout
  SetPin BTN_CLOCK_PIN%(1), Dout
  SetPin BTN_DATA_PIN%(1), Din
  Pin(BTN_LATCH_PIN%(1)) = 0
  Pin(BTN_CLOCK_PIN%(1)) = 0
End Sub

' Reads port A connected to a NES gamepad.
'
' @return  bitmap corresponding to current controller state:
'            7 6 5 4 3 2 1 0
'            | | | | | | | |
'            | | | | | | | +-- Button A
'            | | | | | | +---- Button B
'            | | | | | +------ Select
'            | | | | +-------- Start
'            | | | +---------- Up
'            | | +------------ Down
'            | +-------------- Left
'            +---------------- Right
'
' Note that in theory it should not be necessary to pulse the clock pin after
' reading bit 7 (Right), but in practice there are some clone NES controllers
' which return the wrong value on subsequent reads of bit 7 if it not pulsed.
Function nes_a%()
  Local i%
  Pulse BTN_LATCH_PIN%(0), BTN_PULSE_DURATION!
  For i% = 0 To 7
    Inc nes_a%, Not Pin(BTN_DATA_PIN%(0)) * 2^i%
    Pulse BTN_CLOCK_PIN%(0), BTN_PULSE_DURATION!
  Next
End Function

' Reads port B connected to a NES gamepad.
Function nes_b%()
  Local i%
  Pulse BTN_LATCH_PIN%(1), BTN_PULSE_DURATION!
  For i% = 0 To 7
    Inc nes_b%, Not Pin(BTN_DATA_PIN%(1)) * 2^i%
    Pulse BTN_CLOCK_PIN%(1), BTN_PULSE_DURATION!
  Next
End Function

' Gets a string representation of bits read from a controller.
'
' @param ctrl$  controller Id from CONTROLLER_IDS$().
' @param bits%  controller state returned by controller read function.
Function ctrl_bits_to_string$(ctrl$, bits%)
  ' These are the names of the buttons on a SNES controller;
  ' we adjust them in the SELECT CASE below for NES/ATARI names.
  Static BUTTONS$(11) = ("B","Y","Select","Start","Up","Down","Left","Right","A","X","L","R")

  If bits% = 0 Then Exit Function

  ctrl_bits_to_string$ = Str$(bits%) + " = "
  Local count%, i%, s$
  For i% = 0 To Bound(BUTTONS$(), 1)
    If bits% And 2^i% Then
      s$ = BUTTONS$(i%)
      Select Case ctrl$
        Case "atari_a%", "atari_b%"
          If s$ = "B" Then s$ = "Fire"
        Case "nes_a$", "nes_b%"
          If s$ = "B" Then s$ = "A"
          If s$ = "Y" Then s$ = "B"
        Case "keys_cursor%", "snes_a%", "snes_b%"
          ' No extra manipulation.
        Case Else
          Error "Unknown controller: " + ctrl$
      End Select
      If count% > 0 Then Cat ctrl_bits_to_string$, ", "
      Cat ctrl_bits_to_string$, s$
      Inc count%
    EndIf
  Next
End Function
