' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For PicoMite VGA, MMBasic 5.07.05

' Note 1:
'   This code is written for readability, not for performance.
'   If necessary it can be made faster by:
'    - inlining constants to avoid overhead of "variable" lookup,
'      this will also reduce the amount of RAM required.
'    - unrolling FOR loops.
'    - shortening variable names.
'    - using different functions to read ports A and B so that pin numbers
'      can be hard-coded.
'    - in extremis inlining the xxx_read%() functions to avoid the overhead
'      of a subroutine call.
'
' Note 2:
'   Variables with CAPITALISED names are constants, this includes arrays
'   which have to be declared with DIM and where constant-ness is not enforced
'   by the MMBasic interpreter.

If Mm.Info(Option Base) <> 0 Then Error "OPTION BASE 0 required"

Const PORT_A% = 0
Const PORT_B% = 1

' Button values as returned by xxx_read%() functions.
Const NES_A%      = 1  ' FIRE on joystick and "B" on SNES.
Const NES_B%      = 2  ' "Y" on SNES.
Const NES_SELECT% = 4
Const NES_START%  = 8
Const NES_UP%     = 16
Const NES_DOWN%   = 32
Const NES_LEFT%   = 64
Const NES_RIGHT%  = 128
Const SNES_A%     = 256
Const SNES_X%     = 512
Const SNES_LB%    = 1204
Const SNES_RB%    = 2048

If InStr(Mm.Device$, "PicoMite") > 0 Then

' Pin mappings for ATARI 1-button joystick.
Dim ATARI_FIRE_PIN%(1)  = ( Mm.Info(PinNo GP14), Mm.Info(PinNo GP28) )
Dim ATARI_UP_PIN%(1)    = ( Mm.Info(PinNo  GP0), Mm.Info(PinNo GP28) )
Dim ATARI_DOWN_PIN%(1)  = ( Mm.Info(PinNo  GP1), Mm.Info(PinNo  GP1) )
Dim ATARI_LEFT_PIN%(1)  = ( Mm.Info(PinNo  GP2), Mm.Info(PinNo  GP2) )
Dim ATARI_RIGHT_PIN%(1) = ( Mm.Info(PinNo  GP3), Mm.Info(PinNo  GP3) )

' Pin mappings for NES/SNES gamepad.
Dim NES_CLOCK_PIN%(1) = ( Mm.Info(PinNo  GP3), Mm.Info(PinNo GP22) )
Dim NES_DATA_PIN%(1)  = ( Mm.Info(PinNo  GP1), Mm.Info(PinNo  GP4) )
Dim NES_LATCH_PIN%(1) = ( Mm.Info(PinNo  GP2), Mm.Info(PinNo  GP5) )

EndIf

' The NES standard specifies a 12 micro-second pulse, but all the controllers
' I've tested work with 1 micro-second, and possibly less.
Const NES_PULSE_DURATION! = 0.001 ' 1 micro-second

Dim CONTROLLER_IDS$(3) Length 8 = ( "keyboard", "atari", "nes", "snes" )
Dim CONTROLLER_NAMES$(3) Length 15 = ( "Keyboard", "Atari Joystick", "NES Gamepad" , "SNES Gamepad" )

' +----------------------------------------------------------------------------+
' |                         READING GAME CONTROLLERS                           |
' +----------------------------------------------------------------------------+

' Waits a specified duration for the user to press START on a (S)NES gamepad
' connected to Port A, or FIRE on an ATARI joystick connected to Port A or
' SPACE on the keyboard.
'
' Note that this function deliberately does not clear the keyboard buffer before
' checking to see if SPACE has been pressed; if desired this can be done by
' calling keyboard_init().
'
' @param   duration%  duration in milliseconds; if 0 then indefinite.
' @return             "nes" if the (S)NES START button is pressed,
'                     or "atari" if the ATARI FIRE button is pressed,
'                     or "keyboard" if the SPACE key is pressed,
'                     or "" if none of the above before the duration expires.
Function poll_for_controller$(duration%)
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF)

  ' Short-circuit if we are not on a PicoMite.
  If InStr(Mm.Device$, "PicoMite") = 0 Then
    Do While poll_for_controller$ = "" And Timer < expires%)
      poll_for_controller$ = Choice(keyboard_read%(PORT_A%) = NES_A%, "keyboard", "")
    Loop
    Exit Function
  EndIf

  Do While poll_for_controller$ = "" And Timer < expires%)
    If poll_for_controller$ = "" Then
      nes_init(PORT_A%)
      poll_for_controller$ = Choice(nes_read%(PORT_A%) = NES_START%, "nes", "")
    EndIf
    If poll_for_controller$ = "" Then
      atari_init(PORT_A%)
      poll_for_controller$ = Choice(atari_read%(PORT_A%) = NES_A%, "atari", "")
    EndIf
    If poll_for_controller$ = "" Then
      poll_for_controller$ = Choice(keyboard_read%(PORT_A%) = NES_A%, "keyboard", "")
    EndIf
  Loop
End Function

' Initialises a port connected to an ATARI 1-button joystick.
'
' @param idx%   controller port: only port A = 0 is supported.
Sub atari_init(idx%)
  SetPin ATARI_UP_PIN%(idx%), DIn
  SetPin ATARI_DOWN_PIN%(idx%), DIn
  SetPin ATARI_LEFT_PIN%(idx%), DIn
  SetPin ATARI_RIGHT_PIN%(idx%), DIn
  SetPin ATARI_FIRE_PIN%(idx%), DIn
End Sub

' Reads a port connected to an ATARI 1-button joystick.
'
' @param idx%   controller port: only port A = 0 is supported.
' @return       bitmap corresponding to current controller state:
'                 7 6 5 4 3 2 1 0
'                 | | | | | | | |
'                 | | | | | | | +-- Fire Button
'                 | | | | | | +---- unused
'                 | | | | | +------ unused
'                 | | | | +-------- unused
'                 | | | +---------- Up
'                 | | +------------ Down
'                 | +-------------- Left
'                 +---------------- Right
Function atari_read%(idx%)
  Inc atari_read%, Not Pin(ATARI_FIRE_PIN%(idx%))
  Inc atari_read%, Not Pin(ATARI_UP_PIN%(idx%)) * 16
  Inc atari_read%, Not Pin(ATARI_DOWN_PIN%(idx%)) * 32
  Inc atari_read%, Not Pin(ATARI_LEFT_PIN%(idx%)) * 64
  Inc atari_read%, Not Pin(ATARI_RIGHT_PIN%(idx%)) * 128
End Function

' Initialises the keyboard by clearing the input buffer.
Sub keyboard_init(idx%)
  Do While Inkey$ <> "" : Loop
End Sub

' Reads the keyboard as if it were a controller.
'
' Note that the PicoMite has no KEYDOWN function so we are limited to
' reading a single keypress from the input buffer and cannot handle multiple
' simultaneous keys or properly handle a key being pressed and not released.
'
' @param idx%   if not 0 then this function will return 0.
' @return       bitmap corresponding to key read:
'                 7 6 5 4 3 2 1 0
'                 | | | | | | | |
'                 | | | | | | | +-- Space
'                 | | | | | | +---- unused
'                 | | | | | +------ unused
'                 | | | | +-------- unused
'                 | | | +---------- Up
'                 | | +------------ Down
'                 | +-------------- Left
'                 +---------------- Right
Function keyboard_read%(idx%)
  If idx% <> PORT_A% Then Exit Function
  Select Case Asc(Inkey$)
    Case 32  : keyboard_read% = NES_A%
    Case 128 : keyboard_read% = NES_UP%
    Case 129 : keyboard_read% = NES_DOWN%
    Case 130 : keyboard_read% = NES_LEFT%
    Case 131 : keyboard_read% = NES_RIGHT%
  End Select
End Function

' Initialises a port connected to a SNES gamepad.
' - this is the same intialisation as an NES gamepad.
'
' @param idx%   controller port: A = 0 or B = 1.
Sub snes_init(idx%)
  nes_init(idx%)
End Sub

' Reads a port connected to a SNES gamepad.
'
' @param idx%   controller port: A = 0 or B = 1.
' @return       bitmap corresponding to current controller state:
'                 11 10 9 8 7 6 5 4 3 2 1 0
'                 |  |  | | | | | | | | | |
'                 |  |  | | | | | | | | | +-- Button B
'                 |  |  | | | | | | | | +---- Button Y
'                 |  |  | | | | | | | +------ Select
'                 |  |  | | | | | | +-------- Start
'                 |  |  | | | | | +---------- Up
'                 |  |  | | | | +------------ Down
'                 |  |  | | | +-------------- Left
'                 |  |  | | +---------------- Right
'                 |  |  | +------------------ Button A
'                 |  |  +-------------------- Button X
'                 |  +----------------------- Left Bumper
'                 +-------------------------- Right Bumper
Function snes_read%(idx%)
  Local i%
  Pulse NES_LATCH_PIN%(idx%), NES_PULSE_DURATION!
  For i% = 0 To 11
    Inc snes_read%, Not Pin(NES_DATA_PIN%(idx%)) * 2^i%
    Pulse NES_CLOCK_PIN%(idx%), NES_PULSE_DURATION!
  Next
End Function

' Initialises a port connected to a NES gamepad.
'
' @param idx%   controller port: A = 0 or B = 1.
Sub nes_init(idx%)
  SetPin NES_LATCH_PIN%(idx%), Dout
  SetPin NES_CLOCK_PIN%(idx%), Dout
  SetPin NES_DATA_PIN%(idx%), Din
  Pin(NES_LATCH_PIN%(idx%)) = 0
  Pin(NES_CLOCK_PIN%(idx%)) = 0
End Sub

' Reads a port connected to a NES gamepad.
'
' @param idx%   controller port: A = 0 or B = 1.
' @return       bitmap corresponding to current controller state:
'                 7 6 5 4 3 2 1 0
'                 | | | | | | | |
'                 | | | | | | | +-- Button A
'                 | | | | | | +---- Button B
'                 | | | | | +------ Select
'                 | | | | +-------- Start
'                 | | | +---------- Up
'                 | | +------------ Down
'                 | +-------------- Left
'                 +---------------- Right
'
' Note that in theory it should not be necessary to pulse the clock pin after
' reading bit 7 (Right), but in practice there are some clone NES controllers
' which return the wrong value on subsequent reads of bit 7 if it not pulsed.
Function nes_read%(idx%)
  Local i%
  Pulse NES_LATCH_PIN%(idx%), NES_PULSE_DURATION!
  For i% = 0 To 7
    Inc nes_read%, Not Pin(NES_DATA_PIN%(idx%)) * 2^i%
    Pulse NES_CLOCK_PIN%(idx%), NES_PULSE_DURATION!
  Next
End Function

' Gets a string representation of bits read from a controller.
'
' @param id$    controller Id from CONTROLLER_IDS$().
' @param bits%  controller state returned by xxx_read%() function.
Function ctrl_bits_to_string$(id$, bits%)
  ' These are the names of the buttons on a SNES controller;
  ' we adjust them in the SELECT CASE below for NES/ATARI names.
  Static BUTTONS$(11) = ("B","Y","Select","Start","Up","Down","Left","Right","A","X","L","R")

  If bits% = 0 Then Exit Function

  ctrl_bits_to_string$ = Str$(bits%) + " = "
  Local count%, i%, s$
  For i% = 0 To Bound(BUTTONS$(), 1)
    If bits% And 2^i% Then
      s$ = BUTTONS$(i%)
      Select Case id$
        Case "atari"
          If s$ = "B" Then s$ = "Fire"
        Case "nes"
          If s$ = "B" Then s$ = "A"
          If s$ = "Y" Then s$ = "B"
        Case "keyboard", "snes"
          ' No extra manipulation.
        Case Else
          Error "Unknown controller: " + id$
      End Select
      If count% > 0 Then Cat ctrl_bits_to_string$, ", "
      Cat ctrl_bits_to_string$, s$
      Inc count%
    EndIf
  Next
End Function
