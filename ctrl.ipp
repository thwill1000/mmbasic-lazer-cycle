' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
'
' MMBasic Controller Library
'
'!ifndef SPTRANS
Error "File 'ctrl.ipp' requires transpiling"
'!endif
'!ifdef PICOMITE
  '!set CTRL_PLATFORM_SET
' Preprocessor flag PICOMITE defined
'!endif
'!ifdef CMM2
  '!ifdef CTRL_PLATFORM_SET
    '!error "File 'ctrl.ipp' was transpiled with more than one of the CMM2, MMB4L or PICOMITE flags set"
  '!endif
  '!set CTRL_PLATFORM_SET
' Preprocessor flag CMM2 defined
'!endif
'!ifdef MMB4L
  '!ifdef CTRL_PLATFORM_SET
    '!error "File 'ctrl.ipp' was transpiled with more than one of the CMM2, MMB4L or PICOMITE flags set"
  '!endif
  '!set CTRL_PLATFORM_SET
' Preprocessor flag MMB4L defined
'!endif
'!ifndef CTRL_PLATFORM_SET
  '!error "File 'ctrl.ipp' was transpiled without CMM2, MMB4L or PICOMITE flags set"
'!endif
'!ifdef CTRL_ONE_PLAYER
' Preprocessor flag CTRL_ONE_PLAYER defined
'!endif
'!ifdef CTRL_NO_CURSORS
' Preprocessor flag CTRL_NO_CURSORS defined
'!endif
'!ifdef CTRL_NO_SNES
' Preprocessor flag CTRL_NO_SNES defined
'!endif

Const ctrl.VERSION = 902  ' 0.9.2

' Button values as returned by controller read functions.
Const ctrl.R      = &h01
Const ctrl.START  = &h02
Const ctrl.HOME   = &h04
Const ctrl.SELECT = &h08
Const ctrl.L      = &h10
Const ctrl.DOWN   = &h20
Const ctrl.RIGHT  = &h40
Const ctrl.UP     = &h80
Const ctrl.LEFT   = &h100
Const ctrl.ZR     = &h200
Const ctrl.X      = &h400
Const ctrl.A      = &h800
Const ctrl.Y      = &h1000
Const ctrl.B      = &h2000
Const ctrl.ZL     = &h4000

Const ctrl.OPEN  = -1
Const ctrl.CLOSE = -2
Const ctrl.SOFT_CLOSE = -3

' The NES standard specifies a 12 micro-second pulse, but all the controllers
' I've tested work with 1 micro-second, and possibly less.
Const ctrl.PULSE = 0.001 ' 1 micro-second

' When a key is down the corresponding byte of this 256-byte map is set,
' when the key is up then it is unset.
'
' Note that when using INKEY$ (as opposed to the CMM2 'KEYDOWN' function or
' the PicoMiteVGA 'ON PS2' command) to read the keyboard we cannot detect
' keyup events and instead automatically clear a byte after it is read.
Dim ctrl.key_map%(31 + Mm.Info(Option Base))

'!ifdef CMM2
' Timer number configured for reading the KEYDOWN state on the CMM2.
Dim ctrl.tick_nbr%
'!endif

' Initialises keyboard reading.
'
' @param  period%  CMM2 only - interval to read KEYDOWN state, default 40 ms.
' @param  nbr%     CMM2 only - timer nbr to read KEYDOWN state, default 4.
Sub ctrl.init_keys(period%, nbr%)
  ctrl.term_keys()
'!ifdef CMM2
  ctrl.tick_nbr% = Choice(nbr% = 0, 4, nbr%)
  SetTick Choice(period% = 0, 40, period%), ctrl.on_tick(), ctrl.tick_nbr%
'!endif
'!ifndef CMM2
  On Key ctrl.on_key()
'!endif
End Sub

'!ifdef CMM2
' Note there is little point in calling KeyDown(0) to determine the number of
' keys that are down, hardware limitations mean it's unlikely ever to be > 4
' and if a given key isn't down it just returns 0 so we harmlessly set that
' byte in the key map.
Sub ctrl.on_tick()
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
  Poke Var ctrl.key_map%(), KeyDown(1), 1
  Poke Var ctrl.key_map%(), KeyDown(2), 1
  Poke Var ctrl.key_map%(), KeyDown(3), 1
  Poke Var ctrl.key_map%(), KeyDown(4), 1
End Sub
'!endif

'!ifndef CMM2
Sub ctrl.on_key()
  Poke Var ctrl.key_map%(), Asc(Inkey$), 1
End Sub
'!endif

' Terminates keyboard reading.
Sub ctrl.term_keys()
'!ifdef CMM2
  If ctrl.tick_nbr% <> 0 Then SetTick 0, 0, ctrl.tick_nbr%
'!endif
'!ifndef CMM2
  On Key 0
'!endif
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
  Do While Inkey$ <> "" : Loop
End Sub

Function ctrl.keydown%(i%)
  ctrl.keydown% = Peek(Var ctrl.key_map%(), i%)
'!ifndef CMM2
  Poke Var ctrl.key_map%(), i%, 0
'!endif
End Function

'!ifndef CTRL_NO_CURSORS

' Reads the keyboard as if it were a controller.
Sub keys_cursor(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A
  Inc x%, ctrl.keydown%(128) * ctrl.UP
  Inc x%, ctrl.keydown%(129) * ctrl.DOWN
  Inc x%, ctrl.keydown%(130) * ctrl.LEFT
  Inc x%, ctrl.keydown%(131) * ctrl.RIGHT
End Sub

'!endif ' CTRL_NO_CURSORS

Function ctrl.poll_multiple$(ctrls$(), mask%, duration%)
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF), i%
  Do
    For i% = Bound(ctrls$(), 0) To Bound(ctrls$(), 1)
      If ctrl.poll_single%(ctrls$(i%), mask%) Then
        ctrl.poll_multiple$ = ctrls$(i%)
        Exit Do
      EndIf
    Next
  Loop While Timer < expires%
End Function

' Opens, polls (for a maximum of 5ms) and closes a controller.
'
' @param  ctrl$  controller driver function.
' @param  mask%  bit mask to match against.
' @return        1 if any of the bits in the mask match what is read from the
'                controller, otherwise 0.
Function ctrl.poll_single%(ctrl$, mask%)
  On Error Ignore
  Call ctrl$, ctrl.OPEN
  If Mm.ErrNo = 0 Then
    Local key%, t% = Timer + 5
    Do
      Call ctrl$, key%
      If key% And mask% Then
        ctrl.poll_single% = 1
        ' Wait for user to release key.
        Do While key% <> 0 : Call ctrl$, key% : Loop
        Exit Do
      EndIf
    Loop While Timer < t%
    Call ctrl$, ctrl.SOFT_CLOSE
  EndIf
  On Error Abort
End Function

'!ifdef PICOMITE

' Atari joystick on PicoGAME Port A.
Sub atari_a(x%)
  Select Case x%
    Case Is >= 0
      x% =    Not Pin(GP14) * ctrl.A
      Inc x%, Not Pin(GP0)  * ctrl.UP
      Inc x%, Not Pin(GP1)  * ctrl.DOWN
      Inc x%, Not Pin(GP2)  * ctrl.LEFT
      Inc x%, Not Pin(GP3)  * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP14, DIn
      SetPin GP0, DIn
      SetPin GP1, DIn
      SetPin GP2, DIn
      SetPin GP3, DIn
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' Atari joystick on PicoGAME Port B.
Sub atari_b(x%)
  Select Case x%
    Case Is >= 0
      x% =    Not Pin(GP15) * ctrl.A
      Inc x%, Not Pin(GP28) * ctrl.UP
      Inc x%, Not Pin(GP4)  * ctrl.DOWN
      Inc x%, Not Pin(GP5)  * ctrl.LEFT
      Inc x%, Not Pin(GP22) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP15, DIn
      SetPin GP28, DIn
      SetPin GP4, DIn
      SetPin GP5, DIn
      SetPin GP22, DIn
    End Select
End Sub

'!endif ' CTRL_ONE_PLAYER

'!ifndef CTRL_NO_SNES

' SNES gamepad on PicoGAME Port A.
Sub snes_a(x%)
  Select Case x%
    Case Is >= 0
      Pulse GP2, ctrl.PULSE
      x% =    Not Pin(GP1) * ctrl.B      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.Y      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.SELECT : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.START  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.UP     : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.DOWN   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.LEFT   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.RIGHT  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.A      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.X      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.L      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.R      : Pulse GP3, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      nes_a(ctrl.OPEN)
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' SNES gamepad on PicoGAME Port B.
Sub snes_b(x%)
  Select Case x%
    Case Is >= 0
      Pulse GP5, ctrl.PULSE
      x% =    Not Pin(GP4) * ctrl.B      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.Y      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.SELECT : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.START  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.UP     : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.DOWN   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.LEFT   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.RIGHT  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.A      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.X      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.L      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.R      : Pulse GP22, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      nes_b(ctrl.OPEN)
  End Select
End Sub

'!endif ' !ifndef CTRL_ONE_PLAYER

'!endif ' !ifndef CTRL_NO_SNES

' Reads port A connected to a NES gamepad.
'
' Note that the extra pulse after reading bit 7 (Right) should not be necessary,
' but in practice some NES clone controllers require it to behave correctly.
Sub nes_a(x%)
  Select Case x%
    Case Is >= 0
      Pulse GP2, ctrl.PULSE
      x% =    Not Pin(GP1) * ctrl.A      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.B      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.SELECT : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.START  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.UP     : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.DOWN   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.LEFT   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.RIGHT  : Pulse GP3, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin GP2, Dout ' Latch
      SetPin GP3, Dout ' Clock
      SetPin GP1, Din  ' Data
      Pin(GP2) = 0
      Pin(GP3) = 0
  End Select
End Sub

'!ifndef CTRL_ONE_PLAYER

' NES gamepad on PicoGAME Port B.
Sub nes_b(x%)
  Select Case x%
    Case Is >= 0
      Pulse GP5, ctrl.PULSE
      x% =    Not Pin(GP4) * ctrl.A      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.B      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.SELECT : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.START  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.UP     : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.DOWN   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.LEFT   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.RIGHT  : Pulse GP22, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin GP5,  Dout ' Latch
      SetPin GP22, Dout ' Clock
      SetPin GP4,  Din  ' Data
      Pin(GP5) = 0
      Pin(GP22) = 0
  End Select
End Sub

'!endif ' CTRL_ONE_PLAYER

'!endif ' PICOMITE

'!ifdef CMM2

' Atari joystick port on CMM2 Deluxe G2.
Sub atari_dx(x%)
  Select Case x%
    Case Is >= 0
      x% =    Not Pin(32) * ctrl.A
      Inc x%, Not Pin(33) * ctrl.B
      Inc x%, Not Pin(35) * ctrl.UP
      Inc x%, Not Pin(36) * ctrl.DOWN
      Inc x%, Not Pin(38) * ctrl.LEFT
      Inc x%, Not Pin(40) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin 32, Din, PullUp
      SetPin 33, Din, PullUp
      SetPin 35, Din, PullUp
      SetPin 36, Din, PullUp
      SetPin 38, Din, PullUp
      SetPin 40, Din, PullUp
  End Select
End Sub

' NES gamepad attached USING ADAPTER to Atari joystick port on CMM2 Deluxe G2.
'
' IMPORTANT! the adapter is required to swap the Male DB9 (CMM2) +5V supply on
' Pin 7 to Pin 6 on the Female DB9 (Gamepad).
'
'   Pin 38: Latch, Pin 40: Clock, Pin 36: Data
Sub nes_dx(x%)
  Select Case x%
    Case Is >= 0
      Pulse 38, ctrl.PULSE
      x% =    Not Pin(36) * ctrl.A      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.B      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.SELECT : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.START  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.UP     : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.DOWN   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.LEFT   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.RIGHT  : Pulse 40, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin 38, Dout
      SetPin 40, Dout
      SetPin 36, Din
      Pin(38) = 0
      Pin(40) = 0
  End Select
End Sub

'!ifndef CTRL_NO_SNES

' SNES gamepad attached USING ADAPTER to Atari joystick port on CMM2 Deluxe G2.
'
' IMPORTANT! the adapter is required to swap the Male DB9 (CMM2) +5V supply on
' Pin 7 to Pin 6 on the Female DB9 (Gamepad).
'
'   Pin 38: Latch, Pin 40: Clock, Pin 36: Data
Sub snes_dx(x%)
  Select Case x%
    Case Is >= 0
      Pulse 38, ctrl.PULSE
      x% =    Not Pin(36) * ctrl.B      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.Y      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.SELECT : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.START  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.UP     : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.DOWN   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.LEFT   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.RIGHT  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.A      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.X      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.L      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.R      : Pulse 40, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      nes_dx(ctrl.OPEN)
  End Select
End Sub

'!endif '!ifndef CTRL_NO_SNES

' Wii Classic gamepad on I2C1.
Sub wii_classic_1(x%)
  If x% >= 0 Then
    x% = Classic(B, 1)
    If x% = &h7FFF Then x% = 0 ' Ignore this glitch.
    Exit Sub
  EndIf

  Static is_open% = 0
  Select Case x%
    Case ctrl.OPEN
      If Not is_open% Then Controller Classic Open 1 : is_open% = 1
    Case ctrl.CLOSE
      If is_open% Then Controller Classic Close 1 : is_open% = 0
    Case ctrl.SOFT_CLOSE
      ' Do nothing
  End Select
End Sub

' Wii Classic gamepad on I2C2.
Sub wii_classic_2(x%)
  If x% >= 0 Then
    x% = Classic(B, 2)
    If x% = &h7FFF Then x% = 0 ' Ignore this glitch.
    Exit Sub
  EndIf

  Static is_open% = 0
  Select Case x%
    Case ctrl.OPEN
      If Not is_open% Then Controller Classic Open 2 : is_open% = 1
    Case ctrl.CLOSE
      If is_open% Then Controller Classic Close 2 : is_open% = 0
    Case ctrl.SOFT_CLOSE
      ' Do nothing
  End Select
End Sub

' Wii Classic gamepad on I2C3.
Sub wii_classic_3(x%)
  If x% >= 0 Then
    x% = Classic(B, 3)
    If x% = &h7FFF Then x% = 0 ' Ignore this glitch.
    Exit Sub
  EndIf

  Static is_open% = 0
  Select Case x%
    Case ctrl.OPEN
      If Not is_open% Then Controller Classic Open 3 : is_open% = 1
    Case ctrl.CLOSE
      If is_open% Then Controller Classic Close 3 : is_open% = 0
    Case ctrl.SOFT_CLOSE
      ' Do nothing
  End Select
End Sub

'!endif ' CMM2
