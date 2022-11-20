' Transpiled on 20-11-2022 16:18:16

' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

Option Base 0
Option Default None
Option Explicit On

' Option LcdPanel NoConsole

' BEGIN:     #Include "ctrl.ipp" -----------------------------------------------
' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
'
' MMBasic Controller Library
'
' Preprocessor flag CMM2 defined
' Preprocessor flag CTRL_NO_SNES defined
' Preprocessor flag CTRL_USE_KEYDOWN defined

Const ctrl.VERSION = 904  ' 0.9.4

' Button values as returned by controller driver subroutines.
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

' Timer number configured for reading the KEYDOWN state on the CMM2.
Dim ctrl.tick_nbr%

' Initialises keyboard reading.
'
' @param  period%  CMM2 only - interval to read KEYDOWN state, default 40 ms.
' @param  nbr%     CMM2 only - timer nbr to read KEYDOWN state, default 4.
Sub ctrl.init_keys(period%, nbr%)
  ctrl.term_keys()
  ctrl.tick_nbr% = Choice(nbr% = 0, 4, nbr%)
  SetTick Choice(period% = 0, 40, period%), ctrl.on_tick(), ctrl.tick_nbr%
End Sub

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

' Terminates keyboard reading.
Sub ctrl.term_keys()
  If ctrl.tick_nbr% <> 0 Then SetTick 0, 0, ctrl.tick_nbr%
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
  Do While Inkey$ <> "" : Loop
End Sub

Function ctrl.keydown%(i%)
  ctrl.keydown% = Peek(Var ctrl.key_map%(), i%)
End Function

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
        Do While key% : Pause 5 : Call ctrl$, key% : Loop
        Exit Do
      EndIf
    Loop While Timer < t%
    Call ctrl$, ctrl.SOFT_CLOSE
  EndIf
  On Error Abort
End Function

' Gets a string representation of bits read from a controller driver.
'
' @param  x%  bits returned by driver.
' @return     string representation.
Function ctrl.bits_to_string$(x%)
  Static BUTTONS$(14) = ("R","Start","Home","Select","L","Down","Right","Up","Left","ZR","X","A","Y","B","ZL")

  If x% = 0 Then
    ctrl.bits_to_string$ = "No buttons down"
    Exit Function
  EndIf

  ctrl.bits_to_string$ = Str$(x%) + " = "
  Local count%, i%, s$
  For i% = 0 To Bound(BUTTONS$(), 1)
    If x% And 2^i% Then
      s$ = BUTTONS$(i%)
      If count% > 0 Then Cat ctrl.bits_to_string$, ", "
      Cat ctrl.bits_to_string$, s$
      Inc count%
    EndIf
  Next
End Function

' Reads the keyboard as if it were a controller.
'
' Note that the PicoMite has no KEYDOWN function so we are limited to
' reading a single keypress from the input buffer and cannot handle multiple
' simultaneous keys or properly handle a key being pressed and not released.
Sub keys_cursor(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A
  Inc x%, ctrl.keydown%(128) * ctrl.UP
  Inc x%, ctrl.keydown%(129) * ctrl.DOWN
  Inc x%, ctrl.keydown%(130) * ctrl.LEFT
  Inc x%, ctrl.keydown%(131) * ctrl.RIGHT
End Sub

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

Sub wii_internal(i2c%, x%, type%)
  Static is_open%(3)
  If is_open%(i2c%) = -1 Then Error "I2C" + Str$(i2c%) + " failed to open" : Exit Sub

  If x% >= 0 Then
    Select Case is_open%(i2c%)
      Case &hA4200101
        x% = Classic(B, i2c%)
        If x% = &h7FFF Then x% = 0 ' Ignore this glitch.
      Case &hA4200000
        x% =    Nunchuk(Z,  i2c%) * ctrl.A
        Inc x%, Nunchuk(C,  i2c%) * ctrl.B
        Inc x%, (Nunchuk(JY, i2c%) > 170) * ctrl.UP
        Inc x%, (Nunchuk(JY, i2c%) < 90)  * ctrl.DOWN
        Inc x%, (Nunchuk(JX, i2c%) < 90)  * ctrl.LEFT
        Inc x%, (Nunchuk(JX, i2c%) > 170) * ctrl.RIGHT
    End Select
    Exit Sub
  EndIf

  Select Case x%
    Case ctrl.SOFT_CLOSE
      ' Do nothing
      Exit Sub

    Case ctrl.CLOSE
      Select Case is_open%(i2c%)
        Case &hA4200000
          Controller Nunchuk Close i2c%
        Case &hA4200101
          Controller Classic Close i2c%
      End Select
      is_open%(i2c%) = 0
      Exit Sub

    Case ctrl.OPEN
      If is_open%(i2c%) Then Exit Sub
      is_open%(i2c%) = -1
      Controller Nunchuk Open i2c%
      If Mm.ErrNo Then
        If InStr(Mm.ErrMsg$, "already OPEN") Then
          Error "I2C" + Str$(i2c%) + " already open"
        Else
          Error "I2C" + Str$(i2c%) + " not connected"
        EndIf
        Exit Sub
      EndIf
      is_open%(i2c%) = Nunchuk(T, i2c%)
      Select Case is_open%(i2c%)
        Case &hA4200000
          If Not(type% And &h01) Then
            Controller Nunchuk Close i2c%
            is_open%(i2c%) = -1
            Error "Nunchuck controller on I2C" + Str$(i2c%) + " not supported"
          EndIf
        Case &hA4200101
          Controller Nunchuk Close i2c%
          If Not(type% And &h10) Then
            is_open%(i2c%) = -1
            Error "Classic controller on I2C" + Str$(i2c%) + " not supported"
          Else
            Controller Classic Open i2c%
          EndIf
        Case Else
          Controller Nunchuck Close i2c%
          is_open%(i2c%) = -1
          Error "Unrecognised controller on I2C" + Str$(i2c%)
      End Select
  End Select
End Sub

' Wii Nunchuk OR Classic gamepad on I2C1.
Sub wii_any_1(x%)
  wii_internal(1, x%, &h11)
End Sub

' Wii Nunchuk OR Classic gamepad on I2C2.
Sub wii_any_2(x%)
  wii_internal(2, x%, &h11)
End Sub

' Wii Nunchuk OR Classic gamepad on I2C3.
Sub wii_any_3(x%)
  wii_internal(3, x%, &h11)
End Sub

' Wii Classic gamepad on I2C1.
Sub wii_classic_1(x%)
  wii_internal(1, x%, &h10)
End Sub

' Wii Classic gamepad on I2C2.
Sub wii_classic_2(x%)
  wii_internal(2, x%, &h10)
End Sub

' Wii Classic gamepad on I2C3.
Sub wii_classic_3(x%)
  wii_internal(3, x%, &h10)
End Sub

' Wii Nunchuk on I2C1.
Sub wii_nunchuk_1(x%)
  wii_internal(1, x%, &h01)
End Sub

' Wii Nunchuk on I2C2.
Sub wii_nunchuk_2(x%)
  wii_internal(2, x%, &h01)
End Sub

' Wii Nunchuk on I2C3.
Sub wii_nunchuk_3(x%)
  wii_internal(3, x%, &h01)
End Sub

' END:       #Include "ctrl.ipp" -----------------------------------------------
' BEGIN:     #Include "sound.inc" ----------------------------------------------
' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

' These would be constants but MMBasic does not support constant arrays
Dim sound.F!(127)
Dim sound.NO_MUSIC%(1)  = (&h0000000000000000, &hFFFFFFFF00000000)
Dim sound.FX_NONE%(1)   = (&hFFFFFFFFFFFFFFFF, &hFFFFFFFFFFFFFFFF)
Dim sound.FX_BLART%(1)  = (&hFFFFFF0036373C3D, &hFFFFFFFFFFFFFFFF)
Dim sound.FX_SELECT%(1) = (&hFFFFFFFF0048443C, &hFFFFFFFFFFFFFFFF)
Dim sound.FX_DIE%(3)    = (&h4748494A4B4C4D4E, &h3F40414243444546, &h0038393A3B3C3D3E, &hFFFFFFFFFFFFFFFF)
Dim sound.FX_WIPE%(3)   = (&h3F3E3D3C3B3A3938, &h4746454443424140, &h004E4D4C4B4A4948, &hFFFFFFFFFFFFFFFF)

Dim sound.music_start_ptr%
Dim sound.music_ptr%
Dim sound.fx_enabled% = 1
Dim sound.fx_ptr% = Peek(VarAddr sound.FX_NONE%())

' Initilises sound library.
Sub sound.init()
  Local i%

  ' sound.F!(0) - rest - 10 Hz, which should be inaudible.
  ' sound.F!(1) - C0   - 16.35 Hz
  sound.F!(0) = 10.0
  For i% = 1 To 127
    sound.F!(i%) = 440 * 2^((i% - 58) / 12.0)
  Next

  Restore sound.music_data
  Local count%
  Read count%
  Local num_channels%
  Read num_channels%
  count% = count% \ 8
  Dim sound.MUSIC%(count%)
  For i% = 1 To count%
    Read sound.MUSIC%(i% - 1)
  Next

  sound.start_music(sound.MUSIC%())

  ' Music and sound effects are played on SetTick interrupts.
  SetTick 200, sound.music_int, 1
  SetTick 40, sound.fx_int, 2
End Sub

' Enables/disables sound fx.
Sub sound.enable_fx(z%)
  sound.fx_enabled% = z%
End Sub

' Gets the current music and sound fx enabled state.
'
' @return  if bit 0 is set then sound fx are enabled,
'          if bit 1 is set then music is enabled.
Function sound.get_state%()
  sound.get_state% = sound.fx_enabled%
  Inc sound.get_state%, (sound.music_start_ptr% = Peek(VarAddr sound.MUSIC%())) * 2
End Function

' Starts a music score.
Sub sound.start_music(music%())
  sound.music_start_ptr% = Peek(VarAddr music%())
  sound.music_ptr% = sound.music_start_ptr%
End Sub

' Called from interrupt to play next note of music.
Sub sound.music_int()
  Local n% = Peek(Byte sound.music_ptr%)
  If n% = 255 Then sound.music_ptr% = sound.music_start_ptr% : Exit Sub
  Play Sound 1, B, S, sound.F!(n%), 15
  Play Sound 2, B, S, sound.F!(Peek(Byte sound.music_ptr% + 1)), 15
  Play Sound 3, B, S, sound.F!(Peek(Byte sound.music_ptr% + 2)), 15
  Inc sound.music_ptr%, 3
End Sub

' Starts a new sound effect.
Sub sound.start_fx(fx%(), wait_%)
  If Not sound.fx_enabled% Then Exit Sub
  If wait_% Then sound.wait_for_fx()
  sound.fx_ptr% = Peek(VarAddr fx%())

  ' Wait for first note of new sound effect to play.
  Do While sound.fx_ptr% = Peek(VarAddr fx%()) : Loop
End Sub

' Waits for current sound effect to end.
Sub sound.wait_for_fx()
  If Not sound.fx_enabled% Then Exit Sub
  Do While Peek(Byte sound.fx_ptr%) <> &hFF : Loop
End Sub

' Called from interrupt to play next note of current sound effect.
Sub sound.fx_int()
  Local n% = Peek(Byte sound.fx_ptr%)
  If n% = 255 Then Exit Sub
  Play Sound 4, B, S, sound.F!(n%), (n% <> 0) * 25
  Inc sound.fx_ptr%
End Sub

sound.music_data:

Data 792   ' Number of bytes of music data.
Data 3     ' Number of channels.
Data &h3135000034000033, &h3500253D00313D00, &h00283D00283D0025, &h2A3D00293D002935
Data &h3D002C3D002A3D00, &h002E3D002E00002C, &h314100304000303F, &h4100293F00313D00
Data &h002A3C002A410029, &h253D002C3F002C3F, &h3D002C3D00253D00, &h00313D00313D002C
Data &h3135000034003133, &h3500253D00313D00, &h00283D00283D0025, &h2A3D00293D002935
Data &h3D002C3D002A3D00, &h002E3D002E3D002C, &h273700263800263A, &h41002B3D00273A00
Data &h002E3F002E41002B, &h2C3F00273A00273D, &h3F002A3F002C3F00, &h00293F00293F002A
Data &h2535002734002733, &h3500313D00253D00, &h00283D00283D0031, &h2A3D00293D002935
Data &h3D002C3D002A3D00, &h002E3D002E00002C, &h314100304000303F, &h4100293F00313D00
Data &h002A3C002A410029, &h313D002C3F002C3F, &h3D002C3D00313D00, &h00250000253D002C
Data &h3D4100253F00253D, &h41313D3F00003D31, &h00003D2F3B410000, &h3A4100003D2F3B3F
Data &h412E3A3F00003D2E, &h00003D2D39410000, &h384100003D2D393F, &h412C383F00003D2C
Data &h2C383C2C38410000, &h003D00003F00003F, &h3D202C3D00003D00, &h00003525313D202C
Data &h3538000037000036, &h3800363A31353831, &h2C00352C35380035, &h3538000037000036
Data &h3800363A31353831, &h2C00412C35380035, &h003A00003800003D, &h3F2A003D2A003C2A
Data &h2C003F2C00412A00, &h00382C003F2C003D, &h382C003825003825, &h3100353100382C00
Data &h3538000037000036, &h3800363A31353831, &h2C00352C35380035, &h3538000037000036
Data &h3800363A31353831, &h2C00382C35380035, &h003C32003B32003A, &h3C33000033003C33
Data &h27003A27003C3300, &h0038270033270037, &h382C00380000382C, &h2E00352E00380000
Data &h3538300037300036, &h3800363A31353831, &h2C00352C35380035, &h3538000037000036
Data &h3800363A31353831, &h2C00412C35380035, &h003A00003800003D, &h3F2A003D2A003C2A
Data &h2C003F2C00412A00, &h003D2C003F2C003D, &h3D2C003D25003D25, &h31003831003D2C00
Data &h363D000038000037, &h3D2A363A00003D2A, &h00003A2B373D0000, &h383800003A2B373D
Data &h442C384100003D2C, &h0000412935440000, &h363A00003829353D, &h3D2A363D00003A2A
Data &h00003F2C38410000, &h3D3D00003F2C383F, &h3D2C383D313D3D31, &h25313D25313D2C38
Data &hFFFF000000000000, &hFFFFFFFFFFFFFFFF, &hFFFFFFFFFFFFFFFF
' END:       #Include "sound.inc" ----------------------------------------------
' BEGIN:     #Include "highscr.inc" --------------------------------------------
' Copyright (c) 2022 Thomacs Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

Const highscr.MAX_NAME_LEN% = 8

Dim highscr.values$(9) Length highscr.MAX_NAME_LEN% + 7

Sub highscr.load(filename$, defaults$())
  ' It is convenient to always fill with the defaults
  ' even if we end up overwriting them immediately.
  If Bound(highscr.values$(), 1) <> Bound(defaults$()) Then
    Error "Invalid number of high-score defaults"
  EndIf
  Local i%
  For i% = 0 To Bound(highscr.values$())
    highscr.values$(i%) = defaults$(i%)
  Next

  ' If there is no SD Card then do not load high-scores.
  If Not highscr.has_sdcard%() Then Exit Sub

  ' Check filename$ is absolute.
  Local ok% = 0
  Select Case UCase$(Left$(filename$, 1))
    Case "/", "\" : ok% = 1
    Case "A" To "Z" : If Mid$(filename$, 2, 1) = ":" Then ok% = 1
  End Select
  If Not ok% Then Error "Expected absolute file path '" + filename$ + "'"

  ' Create any parent directories for filename$ if they do not exist.
  Local ch$, parent$
  For i% = 1 To Len(filename$)
    ch$ = Mid$(filename$, i%, 1)
    If InStr("/\", ch$) And parent$ <> "" And UCase$(parent$) <> "A:" Then
      Select Case Mm.Info(FileSize parent$)
        Case -2   : ' Is a directory, do nothing.
        Case -1   : MkDir parent$
        Case Else : Error "Expected directory but found file '" + parent$ + "'"
      End Select
    EndIf
    Cat parent$, ch$
  Next

  ' Check file exists and is not a directory.
  Select Case Mm.Info(FileSize filename$)
    Case -2: Error "Expected file but found directory '" + parent$ + "'"
    Case -1: Exit Sub ' File does not exist.
  End Select

  ' Read the file.
  Open filename$ For Input As #1
  For i% = 0 To Bound(highscr.values$())
    Line Input #1, highscr.values$(i%)
    ok% = ok% And Field$(highscr.values$(i%), 1) <> ""
    ok% = ok% And Field$(highscr.values$(i%), 2) <> ""
    ok% = ok% And Val(Field$(highscr.values$(i%), 2)) > 0
    If Not ok% Then Error "Invalid high-score file '" + filename$ + "'"
  Next
  Close #1
End Sub

Function highscr.has_sdcard%()
  If Mm.Device$ = "MMBasic for Windows" Then
    highscr.has_sdcard% = 1
  Else
    highscr.has_sdcard% = (UCase$(Mm.Info$(SdCard)) = "READY")
  EndIf
End Function

' Assumes parent directory of filename$ already exists.
Sub highscr.save(filename$)
  ' If there is no SD Card then do not save high-scores.
  If Not highscr.has_sdcard%() Then Exit Sub

  Open filename$ For Output As #1
  Local i%
  For i% = 0 To Bound(highscr.values$())
    Print #1, highscr.values$(i%)
  Next
  Close #1
End Sub

' Shows the high-score table for a specified duration or until the user presses
' START/FIRE/SPACE.
'
' @param  ctrls$     controllers to poll.
' @param  duration%  duration in milliseconds; if 0 then indefinite.
' @return            controller id if user pressed button/key,
'                    empty string if the duration expired.
Function highscr.show_table$(ctrls$(), duration%)
  Const ch$ = Chr$(205)
  Text X_OFFSET%, Y_OFFSET% - 95, ch$ + ch$ + " HIGH SCORES " + ch$ + ch$, "CT", 1, 1, Rgb(White)

  Local col_idx%, ctrl$, i%, name$, score$, y%
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF)
  Local colours%(3) = (Rgb(Red), Rgb(Yellow), Rgb(Cyan), Rgb(Green))

  ctrl.init_keys()
  Do While Timer < expires% And highscr.show_table$ = ""

    For i% = 0 To Bound(highscr.values$(), 1) + 1
      If i% <= Bound(highscr.values$(), 1) Then
        name$ = str.rpad$(Field$(highscr.values$(i%), 1), highscr.MAX_NAME_LEN%)
        score$ = str.lpad$(Field$(highscr.values$(i%), 2), 5)
        y% = Y_OFFSET% - 75 + 15 * i%
        Text X_OFFSET%, y%, score$ + "  " + name$, "CT", 1, 1, colours%(col_idx%)
      EndIf
      col_idx% = (col_idx% + 1) Mod 4
    Next

    If Not(InStr(Mm.Device$, "PicoMite")) Then Page Copy 1 To 0, B

    highscr.show_table$ = ctrl.poll_multiple$(ctrls$(), ctrl.A Or ctrl.B Or ctrl.START, 200)
  Loop
End Function

' Provides UI for editing an entry in the high-score table.
'
' @param  player%   player id, counting from 1.
' @param  idx%      index of entry in the highscr.values$ array.
' @param  colour_%  player colour.
' @param  ctrl$     controller id for given player.
Sub highscr.edit(player%, idx%, colour_%, ctrl$)
  ' 256-bit map of characters that can be entered into the high-score table
  ' via gamepad or joystick. For each character 0-255 the character is allowed
  ' if the corresponding bit is set.
'  Static ALLOWED_CHARS%(3) = (&h03FF440100000000, &h000000007FFFFFF, &h0000020080001E00, &h8000000000000000)
  Static ALLOWED_CHARS%(3) = (&h03FF440100000000, &h000000007FFFFFF, &h0000020080001E00, &h0)

  Local ch$ = Chr$(205)
  Text X_OFFSET%, Y_OFFSET% - 95, ch$ + ch$ + " HIGH SCORES " + ch$ + ch$, "CT", 1, 1, Rgb(White)

  ' Draw current high-score table.
  Local i%, name$, score$, x%, y%
  For i% = 0 To Bound(highscr.values$(), 1)
    name$ = str.rpad$(Field$(highscr.values$(i%), 1), highscr.MAX_NAME_LEN%)
    score$ = str.lpad$(Field$(highscr.values$(i%), 2), 5)
    y% = Y_OFFSET% - 75 + 15 * i%
    Text X_OFFSET%, y%, score$ + "  " + name$, "CT", 1, 1, Choice(i% = idx%, colour_%, Rgb(Blue))
  Next

  ' Initialise footer text.
  Local y_footer% = y% + 25
  Local footer$(3) Length 40
  footer$(1) = "   Use * * * * and FIRE to confirm   "
  Poke Var footer$(1), 8, 146
  Poke Var footer$(1), 10, 147
  Poke Var footer$(1), 12, 148
  Poke Var footer$(1), 14, 149
  footer$(2) = " Or, type name and ENTER to confirm  "
  footer$(3) = "     Press FIRE again to confirm     "

  If Not(InStr(Mm.Device$, "PicoMite")) Then Page Copy 1 To 0, B

  Local count% = 0, key%, p% = 1, t% = Timer, bg%, fg%, state%
  y% = Y_OFFSET% - 75 + 15 * idx%
  name$ = Space$(highscr.MAX_NAME_LEN%)

  ctrl.term_keys() ' Regain control of the keyboard for INKEY$
  Call ctrl$, ctrl.OPEN
  Do While highscr.get_input%(ctrl$) : Loop ' Wait for player to release controller.

  ' PEEKing the font doesn't work in MMB4W.
  Local font_width% = Choice(Mm.Device$ = "MMBasic for Windows", 8, Peek(Byte Mm.Info(Font Address 1)))
  Local confirm% = 0
  Do
    If Timer > t% + 500 Then
      count% = (count% + 1) Mod 10
      t% = Timer
    EndIf

    For i% = 1 To highscr.MAX_NAME_LEN%
      bg% = Choice((i% = p%) And (count% And &b1), colour_%, Rgb(Black))
      fg% = Choice((i% = p%) And (count% And &b1), Rgb(Black), colour_%)
      ch$ = Mid$(name$, i%, 1)
      x% = X_OFFSET% + (i% - 1.5) * font_width%
      Text x%, y%, ch$, , 1, 1, fg%, bg%
    Next

    If confirm% > 0 Then
      Text X_OFFSET%, y_footer%, footer$(3), "CT", 1, 1, Rgb(Black), Rgb(White)
      Inc confirm%, -1
    Else If count% < 5 Then
      Text X_OFFSET%, y_footer%, footer$(1), "CT", 1, 1, Rgb(White), Rgb(Black)
    Else
      Text X_OFFSET%, y_footer%, footer$(2), "CT", 1, 1, Rgb(White), Rgb(Black)
    EndIf

    If Not(InStr(Mm.Device$, "PicoMite")) Then Page Copy 1 To 0, B
    Pause 100

    key% = highscr.get_input%(ctrl$)
    If key% > 0 Then
      state% = 1
      Select Case key%
        Case &h01 ' Fire
          If confirm% > 0 Then
            key% = -1 ' So we exit the DO LOOP.
          Else
            confirm% = 50
          EndIf
        Case &h08 ' Backspace
          If p% > 1 Then
            name$ = Left$(name$, p% - 2) + Mid$(name$, p%) + " "
            Inc p%, -1
          Else
            state% = 2
          EndIf
        Case &h7F ' Delete
          name$ = Left$(name$, p% - 1) + Mid$(name$, p% + 1) + " "
        Case &h0A, &h0D ' LF and CR
          key% = -1 ' So we exit the DO LOOP.
        Case &h20 To &h7E
          Poke Var name$, p%, key%
          If p% < highscr.MAX_NAME_LEN% Then Inc p%, 1
        Case &h80, &h81 ' Up and down arrows
          i% = Asc(Mid$(name$, p%, 1))
          If i% = 0 Then i% = 64 ' @
          Do
            Inc i%, Choice(key% = &h80, 1, -1)
            If i% < 0 Then i% = 255
            If i% > 255 Then i% = 0
          Loop Until bits.get%(ALLOWED_CHARS%(i% \ 64), i% Mod 64)
          Poke Var name$, p%, i%
        Case &h82 ' Left arrow
          If p% > 1 Then Inc p%, -1 Else state% = 2
        Case &h83 ' Right arrow
          If p% < highscr.MAX_NAME_LEN% Then Inc p%, 1 Else state% = 2
        Case Else
          state% = 0
      End Select
    End If

    If state% > 0 Then
      If state% = 1 Then sound.start_fx(sound.FX_SELECT%()) Else sound.start_fx(sound.FX_BLART%())
      If key% <> &h01 Then confirm% = 0 ' So message is cleared
      state% = 0
    EndIf

    ' Sanity check.
    If Len(name$) <> highscr.MAX_NAME_LEN% Then Error "Invalid name length"

  Loop While key% <> -1

  ' Delete the footer text.
  Text X_OFFSET%, y_footer%, Space$(40), "CT", 1, 1, Rgb(White), Rgb(Black)

  ' Don't allow empty names.
  name$ = str.trim$(name$)
  If name$ = "" Then name$ = "PLAYER " + Str$(player%)

  highscr.values$(idx%) = name$ + ", " + Field$(highscr.values$(idx%), 2)
End Sub

' Gets player input to edit entry in the high-score table.
'
' @param  ctrl$    controller for given player.
' @return          ASCII code of key pressed, or negative of value read
'                  from player's configured controller port.
Function highscr.get_input%(ctrl$)
  ' Note that controller initialisation should have already occurred.
  Local key% = Asc(UCase$(Inkey$))
  If key% = 0 Then
    Call ctrl$, key%
    Select Case key%
      Case ctrl.A, ctrl.B : highscr.get_input% = &h01 ' Magic value to use for fire
      Case ctrl.UP        : highscr.get_input% = &h80 ' Up arrow
      Case ctrl.DOWN      : highscr.get_input% = &h81 ' Down arrow
      Case ctrl.LEFT      : highscr.get_input% = &h82 ' Left arrow
      Case ctrl.RIGHT     : highscr.get_input% = &h83 ' Right arrow
    End Select

    ' Wait for controller/key to be released.
    Do While key% : Pause 5 : Call ctrl$, key% : Loop
  Else
    highscr.get_input% = key%
  EndIf
End Function

' Gets bit i% of x%.
Function bits.get%(x%, i%)
  If i% < 0 Or i% > 63 Then Error "i% out of 0 .. 63 range"
  bits.get% = (x% And 1 << i%) <> 0
End Function

Function str.lpad$(s$, x%)
  str.lpad$ = s$
  If Len(s$) < x% Then str.lpad$ = Space$(x% - Len(s$)) + s$
End Function

Function str.rpad$(s$, x%)
  str.rpad$ = s$
  If Len(s$) < x% Then str.rpad$ = s$ + Space$(x% - Len(s$))
End Function

' Returns a copy of s$ with leading and trailing spaces removed.
Function str.trim$(s$)
  Local st%, en%
  For st% = 1 To Len(s$)
    If Peek(Var s$, st%) <> 32 Then Exit For
  Next
  For en% = Len(s$) To 1 Step -1
    If Peek(Var s$, en%) <> 32 Then Exit For
  Next
  If en% >= st% Then str.trim$ = Mid$(s$, st%, en% - st% + 1)
End Function
' END:       #Include "highscr.inc" --------------------------------------------
' BEGIN:     #Include "menu.inc" -----------------------------------------------
' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

' Shows the game settings menu.
'
' @param[in]      ui_ctrl$    controller driver to use to navigate the menu.
' @param[in,out]  ctrls$()    on entry the current controller driver per player,
'                             on exit the new controller driver per player.
' @param[in]      colours%()  the player colours
' @return                     1 to continue, or 0 to QUIT.
Function menu.show%(ui_ctrl$, ctrls$(), colours%())
  Const x% = X_OFFSET% - 100
  Static initialised% = 0
  Local key%, i%, item% = 0, update% = 1
  Local sounds$(3) = ("MUSIC & FX", "MUSIC ONLY", "FX ONLY   ", "NONE      ")
  Local sound_setting% = 3 - sound.get_state%()

  If Not initialised% Then
    Dim menu.ctrls$(Bound(ctrls$(), 1)) Length 32
    initialised% = 1
  EndIf
  For i% = Bound(ctrls$(), 0) To Bound(ctrls$(), 1)
    menu.ctrls$(i%) = ctrls$(i%)
  Next

  ' Initialise menu.ctrls$(0) if all entries are currently "no" or "ai" control.
  Local count% = 0
  For i% = Bound(menu.ctrls$(), 0) To Bound(menu.ctrls$(), 1)
    Inc count%, Not InStr("ai_control|no_control", menu.ctrls$(i%))
  Next
  If count% = 0 Then menu.ctrls$(0) = ui_ctrl$

  ' Text X_OFFSET%, Y_OFFSET% + 75, "Music by Scott Joplin", "CM", 7, 1, Rgb(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 90, "Game Version " + VERSION$, "CM", 7, 1, Rgb(Cyan)

  ctrl.init_keys()
  Call ui_ctrl$, ctrl.OPEN

  Do

    If update% Then
      Text x%, Y_OFFSET% - 95, "START GAME", , 1, 1, Rgb(White)
      menu.txt_for_controller(x%, Y_OFFSET% - 75, 0, colours%(0))
      menu.txt_for_controller(x%, Y_OFFSET% - 55, 1, colours%(1))
      menu.txt_for_controller(x%, Y_OFFSET% - 35, 2, colours%(2))
      menu.txt_for_controller(x%, Y_OFFSET% - 15, 3, colours%(3))
      Text x%, Y_OFFSET% + 5,  "DIFFICULTY: " + Str$(difficulty%), , 1, 1, Rgb(White)
      Text x%, Y_OFFSET% + 25, "SOUND:      " + sounds$(sound_setting%), , 1, 1, Rgb(White)
      Text x%, Y_OFFSET% + 45, "QUIT", , 1, 1, Rgb(White)
      Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Cyan)
      If Not InStr(Mm.Device$, "PicoMite") Then Page Copy 1 To 0, B
      Pause 200
      update% = 0
    EndIf

    Call ui_ctrl$, key%
    If key% = 0 Then keys_cursor(key%) ' Always respond to the cursor keys.
    If key% = ctrl.B Then key% = ctrl.A
    Select Case key%
      Case ctrl.START
        menu.show% = 1
        Exit Do

      Case ctrl.UP
        If item% > 0 Then
          Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Black)
          Inc item%, -1
          update% = 1
        EndIf

      Case ctrl.DOWN
        If item% < 7 Then
          Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Black)
          Inc item%
          update% = 1
        EndIf

      Case ctrl.LEFT, ctrl.RIGHT, ctrl.A
        Select Case item%
          Case 0
            If key% = ctrl.A Then menu.show% = 1 : Exit Do

          Case 1,2,3,4
            menu.update_controller(item% - 1, Choice(key% = ctrl.LEFT, -1, +1), ui_ctrl$)
            update% = 1

          Case 5 ' Difficulty
            Inc difficulty%, Choice(key% = ctrl.LEFT, -1, 1)
            If difficulty% < 1 Then difficulty% = 5
            If difficulty% > 5 Then difficulty% = 1
            update% = 1

          Case 6 ' Sound
            Inc sound_setting%, Choice(key% = ctrl.LEFT, -1, 1)
            If sound_setting% < 0 Then sound_setting% = 3
            If sound_setting% > 3 Then sound_setting% = 0
            If sound_setting% And &b10 Then
              sound.start_music(sound.NO_MUSIC%())
            Else
              sound.start_music(sound.MUSIC%())
            EndIf
            sound.enable_fx(Not (sound_setting% And &b01))
            update% = 1

          Case 7 ' Quit
            If key% = ctrl.A Then Exit Do

        End Select

    End Select

    If update% = 1 Then sound.start_fx(sound.FX_SELECT%(), 1)

  Loop

  Call ui_ctrl$, ctrl.CLOSE

  ' Copy controller settings back into parameter.
  For i% = Bound(ctrls$(), 0) To Bound(ctrls$(), 1)
    ctrls$(i%) = menu.ctrls$(i%)
  Next
End Function

Sub menu.txt_for_controller(x%, y%, idx%, colour%)
  Local txt$ = "PLAYER " + Str$(idx% + 1) + ":   " + menu.get_controller_name$(idx%)
  Text x%, y%, txt$, , 1, 1, colour%
End Sub

Function menu.get_controller_name$(idx%)
  Local i%
  For i% = Bound(CTRL_DRIVER$(), 0) To Bound(CTRL_DRIVER$(), 1)
    If menu.ctrls$(idx%) = CTRL_DRIVER$(i%) Then
      menu.get_controller_name$ = CTRL_DESCRIPTION$(i%)
      Do While Len(menu.get_controller_name$) < 14 : Cat menu.get_controller_name$, " " : Loop
      Exit Function
    Endif
  Next
  Error "Unknown controller: " + menu.ctrls$(idx%)
End Function

Sub menu.update_controller(idx%, delta%, ui_ctrl$)
  ' Temporarily close UI controller.
  Call ui_ctrl$, ctrl.CLOSE

  ' Find index of currently selected controller.
  Local i%
  For i% = Bound(CTRL_DRIVER$(), 0) To Bound(CTRL_DRIVER$(), 1)
    If menu.ctrls$(idx%) = CTRL_DRIVER$(i%) Then Exit For
  Next
  If i% = Bound(CTRL_DRIVER$(), 1) + 1 Then Error "Unknown controller: " + menu.ctrls$(idx%)

  Local ok%
  Do
    Inc i%, delta%
    If i% < 0 Then i% = Bound(CTRL_DRIVER$(), 1)
    If i% > Bound(CTRL_DRIVER$(), 1) Then i% = 0

    ' Check there is no conflict with other player's controller choice.
    If Not InStr("ai_control|no_control", CTRL_DRIVER$(i%)) Then
      If menu.has_controller%(idx%, CTRL_DRIVER$(i%)) Then Continue Do
    EndIf
    Select Case CTRL_DRIVER$(i%)
      Case "atari_a"   : ok% = Not menu.has_controller%(idx%, "nes_a")
      Case "atari_b"   : ok% = Not menu.has_controller%(idx%, "nes_b")
      Case "atari_dx"  : ok% = Not menu.has_controller%(idx%, "nes_dx")
      Case "nes_a"     : ok% = Not menu.has_controller%(idx%, "atari_a")
      Case "nes_b"     : ok% = Not menu.has_controller%(idx%, "atari_b")
      Case "nes_dx"    : ok% = Not menu.has_controller%(idx%, "atari_dx")
      Case "keys_cegg" : ok% = Not menu.has_controller%(idx%, "keys_azxc", "keys_punc")
      Case "keys_azxc" : ok% = Not menu.has_controller%(idx%, "keys_cegg")
      Case "keys_punc" : ok% = Not menu.has_controller%(idx%, "keys_cegg")
      Case Else        : ok% = 1
    End Select

    ' Check that we can OPEN the controller.
    If ok% Then ok% = menu.can_open_controller%(CTRL_DRIVER$(i%))
  Loop Until ok%

  menu.ctrls$(idx%) = CTRL_DRIVER$(i%)

  ' Restore UI controller.
  Call ui_ctrl$, ctrl.OPEN
End Sub

' Can we OPEN the given controller ?
Function menu.can_open_controller%(ctrl$)
  On Error Ignore
  Call ctrl$, ctrl.OPEN
  On Error Abort
  Local ok% = Mm.ErrNo = 0
  If ok% Then Call ctrl$, ctrl.CLOSE
  menu.can_open_controller% = ok%
End Function

' Is any player other than idx% using the specified controller ?
Function menu.has_controller%(idx%, ctrl1$, ctrl2$)
  Local i%
  For i% = Bound(menu.ctrls$(), 0) To Bound(menu.ctrls$(), 1)
    If i% = idx% Then Continue For
    If menu.ctrls$(i%) = ctrl1$ Then menu.has_controller% = 1
    If menu.ctrls$(i%) = ctrl2$ Then menu.has_controller% = 1
  Next
End Function
' END:       #Include "menu.inc" -----------------------------------------------

Const VERSION$ = "0.9.1"

Select Case Mm.Device$
  Case "Colour Maximite 2", "Colour Maximite 2 G2", "MMBasic for Windows"
    Const HIGHSCORE_FILENAME$ = Mm.Info(Path) + "high-scores/lazer-cycle.csv"
    Const IS_CMM2% = 1
    Mode 7
    Page Write 1
  Case "PicoMite", "PicoMiteVGA"
    If Val(Mm.Info(CpuSpeed)) < 252000000 Then Error "Requires OPTION CPUSPEED 252000 or 378000"
    Const HIGHSCORE_FILENAME$ = "/high-scores/lazer-cycle.csv"
    Const IS_CMM2% = 0
    If Mm.Device$ = "PicoMiteVGA" Then Mode 2
  Case Else
    Error "Unsupported device: " + Mm.Device$
End Select

' Text deliberately padded to string width (40 chars).
If Mm.Device$ = "MMBasic for Windows" Then
  Const START_TEXT$ = "          Press SPACE to play           "
Else
  Const START_TEXT$ = "       Press START, FIRE or SPACE       "
EndIf

Const WIDTH% = Mm.HRes \ 2
Const HEIGHT% = (Mm.VRes - 20) \ 2
Const X_OFFSET% = MM.HRes \ 2
Const Y_OFFSET% = MM.VRes \ 2
Const NORTH% = 0, EAST% = 1, SOUTH% = 2, WEST% = 3
Const MAX_CYCLE_IDX% = 3
Const SCORE_Y% = 2 * HEIGHT% + 4
Const STATE_OK%    = &b000 ' 0; values 1-3 are "imminent death"
Const STATE_DYING% = &b100 ' 4
Const STATE_DEAD%  = &b101 ' 5
Const DIRECTION_MASK% = ctrl.UP Or ctrl.DOWN Or ctrl.LEFT Or ctrl.RIGHT

' These would be constants but MMBasic does not support constant arrays
Dim NEXT_DIR%(7)        = (EAST%, NORTH%, WEST%, SOUTH%, EAST%, NORTH%, WEST%, SOUTH%)
Dim SCORE_X%(3)         = (35, 105, 175, 245)
Dim DIRECTIONS%(3)      = (-WIDTH%, 1, WIDTH%, -1)
Dim COMPASS_TO_CTRL%(3) = (ctrl.UP, ctrl.RIGHT, ctrl.DOWN, ctrl.LEFT)

Dim ui_ctrl$ ' Controller id for controlling the UI.
Dim attract_mode% = 1
Dim score%
Dim difficulty% = 1
Dim frame_duration%
Dim next_frame%

' Each cell of the arena takes up 1 byte:
'   bit  0   - occupied by cycle
'   bits 1-2 - index of cycle
'   bits 3-4 - direction cycle was going in when entered cell
'   bits 5-6 - unused
'   bit  7   - arena wall (other bits will be 0)
Dim arena%(HEIGHT% * WIDTH% \ 8)

Dim cycle.current% ' Current cycle index, set before calling controller subroutines.
Dim cycle.score%(MAX_CYCLE_IDX%)
Dim cycle.nxt%(MAX_CYCLE_IDX%)
Dim cycle.pos%(MAX_CYCLE_IDX%)
Dim cycle.dir%(MAX_CYCLE_IDX%)
Dim cycle.colour%(MAX_CYCLE_IDX%) = (Rgb(Red), Rgb(Yellow), Rgb(Cyan), Rgb(Green))
Dim cycle.ctrl$(MAX_CYCLE_IDX%) Length 32
Dim cycle.ctrl_setting$(MAX_CYCLE_IDX%) Length 32
Dim cycle.state%(MAX_CYCLE_IDX%)

Dim num_alive%
Dim num_humans%

Option Break 4
On Key 3, on_exit

init_globals()
clear_display()
sound.init()
outer_loop()
End

Sub outer_loop()
  Local attract_mode% = 1, i%

  Do
    If attract_mode% Then wipe() : attract_mode% = Not show_title%(5000)
    If attract_mode% Then wipe() : attract_mode% = Not show_instructions%(15000)
    If attract_mode% Then wipe() : attract_mode% = Not show_highscore%(5000)
    If Not attract_mode% Then
      wipe()
      If Not menu.show%(ui_ctrl$, cycle.ctrl_setting$(), cycle.colour%()) Then on_exit()
    EndIf

    wipe()
    init_game(attract_mode%)
    draw_arena()

    If game_loop%() Then
      ' Game loop interrupted after all human players dead.
      attract_mode% = 0
    ElseIf Not attract_mode% Then
      ' Game ended normally whilst not in attract mode.
      show_game_over()
      attract_mode% = Not show_highscore%(5000)
    EndIf

    close_controllers()
  Loop
End Sub

' Break handler to stop music & fx when Ctrl-C pressed.
Sub on_exit()
  Play Stop
  On Key 3, 0
  Option Break 3
  close_controllers()
  Cls
  End
End Sub

' Initialises global variables.
Sub init_globals()
  Local a%, i%, j%

  ' Initialise list of controllers.
  Select Case Mm.Device$
    Case "PicoMite"    : Restore controller_data_pm
    Case "PicoMiteVGA" : Restore controller_data_pmvga
    Case Else          : Restore controller_data_cmm2
  End Select
  Local num_ctrl%, num_poll%
  Read num_ctrl%, num_poll%
  Dim CTRL_DESCRIPTION$(num_ctrl% - 1)
  Dim CTRL_DRIVER$(num_ctrl% - 1)
  Dim CTRLS_TO_POLL$(num_poll% - 1)
  j% = 0
  For i% = 0 To num_ctrl% - 1
    Read CTRL_DESCRIPTION$(i%), CTRL_DRIVER$(i%), a%
    If a% Then
      CTRLS_TO_POLL$(j%) = CTRL_DRIVER$(i%)
      Inc j%
    EndIf
  Next

  ' Initialise controller settings.
  cycle.ctrl_setting$(0) = "ai_control"
  cycle.ctrl_setting$(1) = "ai_control"
  For i% = 2 To MAX_CYCLE_IDX% : cycle.ctrl_setting$(i%) = "no_control" : Next

  ' Initialise high-scores.
  Restore highscore_data
  Local defaults$(Bound(highscr.values$(), 1))
  For i% = 0 To Bound(defaults$(), 1)
    Read defaults$(i%)
  Next
  highscr.load(HIGHSCORE_FILENAME$, defaults$())
End Sub

' Displays the title screen for a specified duration or until the user presses
' START/FIRE/SPACE.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function show_title%(duration%)
  Text X_OFFSET%, Y_OFFSET% - 15, "LAZER CYCLE", "CM", 1, 2, Rgb(White)
  Text X_OFFSET%, Y_OFFSET% + 8, "(c) 2022 Thomas Hugo Williams", "CM", 7, 1, Rgb(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 20, "www.sockpuppetstudios.com", "CM", 7, 1, Rgb(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 40, START_TEXT$, "CM", 1, 1, Rgb(White)
  If IS_CMM2% Then Page Copy 1 To 0, B
  show_title% = wait%(duration%)
End Function

' Displays the instructions screen for a specified duration or until the user presses
' START/FIRE/SPACE.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function show_instructions%(duration%)
  Const ch$ = Chr$(205)
  Local y% = Y_OFFSET% - 95
  Text X_OFFSET%, y%, ch$ + ch$ + " LAZER CYCLE " + ch$ + ch$, "CT", 1, 1, Rgb(White) : Inc y%, 20
  Text X_OFFSET%, y%, "An arcade game for 1-4 players.", "CT", 1, 1, Rgb(Red) : Inc y%, 20
  Text X_OFFSET%, y%, "Pilot your cycle around the", "CT", 1, 1, Rgb(Yellow) : Inc y%, 12
  Text X_OFFSET%, y%, "arena leaving a trail of light", "CT", 1, 1, Rgb(Yellow) : Inc y%, 12
  Text X_OFFSET%, y%, "behind, longest trail wins.", "CT", 1, 1, Rgb(Yellow) : Inc y%, 20
  Text X_OFFSET%, y%, "You are eliminated if you hit the", "CT", 1, 1, Rgb(Cyan) : Inc y%, 12
  Text X_OFFSET%, y%, "arena wall, or one of the trails.", "CT", 1, 1, Rgb(Cyan) : Inc y%, 20
  Text X_OFFSET%, y%, "Use keyboard, joystick or gamepad", "CT", 1, 1, Rgb(Green) : Inc y%, 12
  Text X_OFFSET%, y%, "UP, DOWN, LEFT and RIGHT to steer.", "CT", 1, 1, Rgb(Green) : Inc y%, 20
  Text X_OFFSET%, y%, "Good Luck!", "CT", 1, 1, Rgb(White)
  If IS_CMM2% Then Page Copy 1 To 0, B
  show_instructions% = wait%(duration%)
End Function

' Displays the highscore table for a specified duration or until the user presses
' START/FIRE/SPACE.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function show_highscore%(duration%)
  Local ctrl$ = highscr.show_table$(CTRLS_TO_POLL$(), 5000)
  If ctrl$ <> "" Then
    If ui_ctrl$ = "" Then ui_ctrl$ = ctrl$
    show_highscore% = 1
  EndIf
End Function

Sub clear_display()
   Box 0, 0, Mm.HRes, Mm.VRes, 1, Rgb(Black), Rgb(Black)
   If IS_CMM2% Then Page Copy 1 To 0, B
End Sub

' Waits a specified duration for the user to press START on a (S)NES gamepad
' connected to Port A, or FIRE on an ATARI joystick connected to Port A or
' SPACE on the keyboard.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function wait%(duration%)
  ctrl.init_keys()
  Local ctrl$ = ctrl.poll_multiple$(CTRLS_TO_POLL$(), ctrl.A Or ctrl.B Or ctrl.START, duration%)
  If ctrl$ <> "" Then
    If ui_ctrl$ = "" Then ui_ctrl$ = ctrl$
    wait% = 1
  EndIf
End Function

Sub init_game(attract_mode%)
  frame_duration% = 3 * (5 + (6 - difficulty%))
  num_alive% = 0 ' Incremented later.
  num_humans% = 0
  score% = 0

  ' Initialise the arena.
  Local p_arena% = Peek(VarAddr arena%())
  Memory Set p_arena%, 0, HEIGHT% * WIDTH%
  Memory Set p_arena%, 128, WIDTH%
  Memory Set p_arena% + (HEIGHT% - 1) * WIDTH%, 128, WIDTH%
  Local y%
  For y% = 1 To HEIGHT% - 2
    Poke Byte p_arena% + y% * WIDTH%, 128
    Poke Byte p_arena% + (y% + 1) * WIDTH% - 1, 128
  Next

  ' Initialise game ports and keyboard routines.
  Local i%
  For i% = 0 To MAX_CYCLE_IDX%
    cycle.ctrl$(i%) = Choice(attract_mode%, "ai_control", cycle.ctrl_setting$(i%))
    On Error Ignore
    Call cycle.ctrl$(i%), ctrl.OPEN
    If Mm.ErrNo <> 0 Then cycle.ctrl$(i%) = "no_control"
    On Error Abort
  Next
  ctrl.init_keys()

  ' Initialise cycle state.
  cycle.dir%(0) = EAST%
  cycle.dir%(1) = WEST%
  cycle.dir%(2) = SOUTH%
  cycle.dir%(3) = NORTH%

  cycle.pos%(0) = WIDTH * (HEIGHT% \ 2) + 5
  cycle.pos%(1) = WIDTH% * (HEIGHT% \ 2) + WIDTH% - 6
  cycle.pos%(2) = 5.5 * WIDTH%
  cycle.pos%(3) = WIDTH% * (HEIGHT% - 6) + WIDTH% \ 2

  For i% = 0 To MAX_CYCLE_IDX%
    cycle.score%(i%) = 0
    If cycle.ctrl$(i%) = "no_control" Then
      cycle.pos%(i%) = -1
      cycle.nxt%(i%) = -1
      cycle.state%(i%) = STATE_DEAD%
    Else
      Inc num_alive%
      Inc num_humans%, cycle.ctrl$(i%) <> "ai_control"
      cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(cycle.dir%(i%))
      Poke Byte p_arena% + cycle.pos%(i%), (cycle.dir%(i%) << 3) + (i% << 1) + 1
      Poke Byte p_arena% + cycle.nxt%(i%), (cycle.dir%(i%) << 3) + (i% << 1) + 1
      cycle.state%(i%) = STATE_OK%
    EndIf
  Next
End Sub

Sub draw_arena()
  Local a%, i%, j%
  For i% = 0 To Bound(arena%(), 1) - 1
    a% = arena%(i%)
    If a% = 0 Then Continue For
    For j% = 0 To 7
      If Peek(Var a%, j%) <> 128 Then Continue For
      Pixel 2 * (((i% * 8) Mod WIDTH%) + j%), 2 * ((i% * 8) \ WIDTH%), Rgb(Grey)
    Next
  Next
End Sub

' @return  0 - normal game over
'          1 - game interrupted after all human players died
Function game_loop%()
  Local d%, i%, key%, next_frame% = Timer + frame_duration%

  ' num_alive% = 0
  ' cycle.score%(0) = 3175
  ' cycle.score%(1) = 2175
  ' cycle.score%(2) = 1175
  ' cycle.score%(3) = 975
  ' score% = 3175

  Do While num_alive% > 0
    Inc score%, 1
'    If score% Mod 5 = 0 Then draw_score()

    ' When dying the cycle trail deletes at twice the rate.
    For i% = 0 To MAX_CYCLE_IDX%
      If cycle.state%(i%) = STATE_DYING% Then cycle.dying(i%)
    Next

    ' Draw cycles.
    For i% = 0 To MAX_CYCLE_IDX%
      If Not (cycle.state%(i%) And &b11) Then cycle.draw(i%)
    Next

    If IS_CMM2% Then Page Copy 1 To 0, I

    ' Move cycles.
    For i% = 0 To MAX_CYCLE_IDX%
      If Not (cycle.state%(i%) And &b11) Then cycle.pos%(i%) = cycle.nxt%(i%)
    Next

    ' Determine changes of direction and check for collisions.
    For i% = 0 To MAX_CYCLE_IDX%
      cycle.current% = i%
      Call cycle.ctrl$(i%), key%
      d% = cycle.dir%(i%)
      Select Case key% And DIRECTION_MASK%
        Case ctrl.UP:    If d% <> SOUTH% Then d% = NORTH%
        Case ctrl.DOWN:  If d% <> NORTH% Then d% = SOUTH%
        Case ctrl.LEFT:  If d% <> EAST% Then d% = WEST%
        Case ctrl.RIGHT: If d% <> WEST% Then d% = EAST%
      End Select
      cycle.dir%(i%) = d%
      cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(d%)
      If cycle.state%(i%) <> STATE_DEAD% Then cycle.check_collision(i%)
      If i% = 0 Then draw_controller(i%, key%)
    Next

    ' Wait for next frame.
    Do While Timer < next_frame% : Loop
    Inc next_frame%, frame_duration%

    If num_humans% > 0 Then Continue Do

    ' Check for "attract mode" being interrupted.
    If ctrl.poll_single%(CTRLS_TO_POLL$(score% Mod (Bound(CTRLS_TO_POLL$(), 1) + 1)), ctrl.A Or ctrl.B Or ctrl.START) Then
      If ui_ctrl$ = "" Then ui_ctrl$ = CTRLS_TO_POLL$(score% Mod (Bound(CTRLS_TO_POLL$(), 1) + 1))
      num_alive% = 0
      game_loop% = 1
    EndIf
  Loop

  ' Ensure display updated at end of loop.
  If IS_CMM2% Then Page Copy 1 To 0, B

  ' Wait for current sound effect (if any) to complete.
  sound.wait_for_fx()
End Function

Sub draw_score()
  If num_humans% > 0 Or ((score% \ 100) And &b1) Then
    Local i%, s$ = Str$(score%, 5, 0, "0")
    For i% = 0 To MAX_CYCLE_IDX%
      If cycle.state%(i%) < STATE_DYING% Then
        Text SCORE_X%(i%), SCORE_Y%, s$, , 1, 1, cycle.colour%(i%)
      EndIf
    Next
    Exit Sub
  EndIf

  ' If there are no human players we toggle between the score and the "Press ..." text.
  If score% Mod 100 = 95 Then
    Local i%, sc%
    Text WIDTH%, SCORE_Y%, Space$(40), "C", 1, 1, Rgb(White)
    For i% = 0 To MAX_CYCLE_IDX%
      sc% = Choice(cycle.state%(i%) < STATE_DYING%, score%, (cycle.score%(i%) \ 5) * 5)
      Text SCORE_X%(i%), SCORE_Y%, Str$(sc%, 5, 0, "0"), , 1, 1, cycle.colour%(i%)
    Next
  Else
    Text WIDTH%, SCORE_Y%, START_TEXT$, "C", 1, 1, Rgb(White)
  EndIf
End Sub

Sub draw_controller(idx%, x%)
  Local s$ = cycle.ctrl$(idx%) + ": " + ctrl.bits_to_string$(x%)
  Text 0, SCORE_Y%, Space$(40), , 1, 1, Rgb(White)
  Text 0, SCORE_Y%, s$, , 1, 1, Rgb(White)
End Sub

Sub wipe()
  Local y%
  sound.start_fx(sound.FX_WIPE%(), 1)
  For y% = 0 To Mm.VRes \ 2 Step 5
     Box WIDTH% - y% * 1.2, Mm.VRes \ 2 - y%, 2.4 * y%, 2 * y%, 5, Rgb(Cyan), Rgb(Black)
     If IS_CMM2% Then Page Copy 1 To 0, B
     Pause 30
  Next
  clear_display()
End Sub

' Draw cycle if STATE_OK% or STATE_DYING%.
Sub cycle.draw(idx%)
  Local p% = cycle.pos%(idx%), n% = cycle.nxt%(idx%)
  Line 2*(p% Mod WIDTH%), 2*(p%\WIDTH%), 2*(n% Mod WIDTH%), 2*(n%\WIDTH%), 1, cycle.colour%(idx%) * (cycle.state%(idx%) <> STATE_DYING%)
End Sub

Sub cycle.dying(idx%)
  cycle.draw(idx%)
  cycle.pos%(idx%) = cycle.nxt%(idx%) ' Move
  cycle.current% = idx%
  Local key%
  die_control(key%)
  Select Case key%
    Case ctrl.UP:    cycle.dir%(idx%) = NORTH%
    Case ctrl.DOWN:  cycle.dir%(idx%) = SOUTH%
    Case ctrl.LEFT:  cycle.dir%(idx%) = WEST%
    Case ctrl.RIGHT: cycle.dir%(idx%) = EAST%
  End Select
  cycle.nxt%(idx%) = cycle.pos%(idx%) + DIRECTIONS%(cycle.dir%(idx%))
  cycle.check_collision(idx%)
End Sub

Sub show_game_over()
  ' Sort scores and then round down to nearest 5.
  Local dummy%, i%, idx%(MAX_CYCLE_IDX%), j%, k%, winner%
  Sort cycle.score%(), idx%(), 1
  For i% = 0 To MAX_CYCLE_IDX%
    cycle.score%(i%) = (cycle.score%(i%) \ 5) * 5 ' Round down to nearest 5.
  Next
  winner% = idx%(0)

  Local txt$ = "PLAYER " + Str$(winner% + 1) + " WINS"
  Text X_OFFSET%, Y_OFFSET% - 25, txt$, "CM", 1, 2, cycle.colour%(winner%)
  Text X_OFFSET%, Y_OFFSET% + 5, "SCORE: " + Str$(cycle.score%(0)), "CM", 1, 2, cycle.colour%(winner%)
  If IS_CMM2% Then Page Copy 1 To 0, B
  dummy% = wait%(5000)

  wipe()

  ' Insert into high-score table.
  ' For the moment only the winner can enter a high-score,
  ' to change that make the upper bound of the FOR statement = MAX_CYCLE_IDX%
  Local new_highscore%, player%
  For i% = 0 To 0
    player% = idx%(i%)
    If cycle.ctrl_setting$(player%) = "ai_control" Then Continue For
    For j% = 0 To Bound(highscr.values$())
      If cycle.score%(i%) > Val(Field$(highscr.values$(j%), 2)) Then
        For k% = Bound(highscr.values$(), 1) To j% Step -1
          If k% <> 0 Then highscr.values$(k%) = highscr.values$(k% - 1)
        Next
        highscr.values$(j%) = ", " + Str$(cycle.score%(i%))
        highscr.edit(player% + 1, j%, cycle.colour%(player%), cycle.ctrl_setting$(player%))
        new_highscore% = 1
        Exit For
      EndIf
    Next
  Next
  If new_highscore% Then highscr.save(HIGHSCORE_FILENAME$)
End Sub

Sub ai_control(x%)
  If x% < 0 Then Exit Sub

  Local idx% = cycle.current%
  Local d% = cycle.dir%(idx%)

  ' Random element.
  Local i% = Int(500 * Rnd)
  If i% < 4 Then d% = NEXT_DIR%(i% + idx%)

  ' Avoid collisions.
  Local nxt%
  For i% = 0 To MAX_CYCLE_IDX%
    nxt% = cycle.pos%(idx%) + DIRECTIONS%(d%)
    If Not Peek(Var arena%(), nxt%)) Then Exit For
    d% = NEXT_DIR%(i% + idx%)
  Next

  x% = COMPASS_TO_CTRL%(d%)
End Sub

Sub die_control(x%)
  If x% < 0 Then Exit Sub
  Local bits% = Peek(Var arena%(), cycle.pos%(cycle.current%)) >> 1
  If (bits% And &b11) = cycle.current% Then
    bits% = (bits% >> 2) And &b11
    x% = COMPASS_TO_CTRL%((bits% + 2) Mod 4)
  EndIf
End Sub

Sub no_control(x%)
  x% = 0
End Sub

Sub keys_cegg(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A     ' Space
  Inc x%, ctrl.keydown%(97)  * ctrl.UP    ' A
  Inc x%, ctrl.keydown%(122) * ctrl.DOWN  ' Z
  Inc x%, ctrl.keydown%(44)  * ctrl.LEFT  ' comma
  Inc x%, ctrl.keydown%(46)  * ctrl.RIGHT ' full-stop
End Sub

Sub keys_azxc(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A     ' Space
  Inc x%, ctrl.keydown%(97)  * ctrl.UP    ' A
  Inc x%, ctrl.keydown%(122) * ctrl.DOWN  ' Z
  Inc x%, ctrl.keydown%(120) * ctrl.LEFT  ' X
  Inc x%, ctrl.keydown%(99)  * ctrl.RIGHT ' C
End Sub

Sub keys_punc(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32) * ctrl.A     ' Space
  Inc x%, ctrl.keydown%(39) * ctrl.UP    ' single-quote
  Inc x%, ctrl.keydown%(47) * ctrl.DOWN  ' slash
  Inc x%, ctrl.keydown%(44) * ctrl.LEFT  ' comma
  Inc x%, ctrl.keydown%(46) * ctrl.RIGHT ' full-stop
End Sub

Sub cycle.check_collision(idx%)
  ' Handle dying.
  If cycle.state%(idx%) = STATE_DYING% Then
    Poke Var arena%(), cycle.pos%(idx%), 0
    Local mask% = (idx% << 1) + 1
    If (Peek(Var arena%(), cycle.nxt%(idx%)) And mask%) <> mask% Then
      cycle.ctrl$(idx%) = "no_control"
      cycle.state%(idx%) = STATE_DEAD%
      cycle.pos%(idx%) = -1
    EndIf
    Exit Sub
  EndIf

  ' No collision occurred.
  If Not Peek(Var arena%(), cycle.nxt%(idx%)) Then
    Poke Var arena%(), cycle.nxt%(idx%), (cycle.dir%(idx%) << 3) + (idx% << 1) + 1
    cycle.state%(idx%) = STATE_OK%
    Exit Sub
  EndIf

  ' Collision occured - the player has a couple of frames to change direction.
  Inc cycle.state%(idx%)
  If cycle.state%(idx%) < STATE_DYING% Then Exit Sub

  ' Time to die.
  Inc num_alive%, -1
  If cycle.ctrl$(idx%) <> "ai_control" Then Inc num_humans%, -1
  cycle.ctrl$(idx%) = "die_control"
  cycle.nxt%(idx%) = cycle.pos%(idx%)
  cycle.score%(idx%) = score%
  sound.start_fx(sound.FX_DIE%(), 0)
End Sub

Sub close_controllers()
  Local i%
  For i% = 0 To MAX_CYCLE_IDX%
    close_controller_no_error(cycle.ctrl_setting$(i%))
  Next
End Sub

Sub close_controller_no_error(ctrl$)
  On Error Ignore
  Call ctrl$, ctrl.CLOSE
  On Error Abort
End Sub

controller_data_cmm2:

Data 11, 6
Data "KEYS: CURSOR",   "keys_cursor", 1
Data "KEYS: AZ,.",     "keys_cegg",   0
Data "KEYS: AZXC",     "keys_azxc",   0
Data "KEYS: '/,.",     "keys_punc",   0
Data "JOYSTICK DX",    "atari_dx",    1
Data "NES GAMEPAD DX", "nes_dx",      1
Data "WII CTRL I2C1",  "wii_any_1",   1
Data "WII CTRL I2C2",  "wii_any_2",   1
Data "WII CTRL I2C3",  "wii_any_3",   1
Data "AI",             "ai_control",  0
Data "NONE",           "no_control",  0

controller_data_pm:

Data 7, 2
Data "KEYS: CURSOR", "keys_cursor", 1
Data "KEYS: AZ,.",   "keys_cegg",   0
Data "KEYS: AZXC",   "keys_azxc",   0
Data "KEYS: '/,.",   "keys_punc",   0
Data "GAMEPAD",      "nes_a",       1
Data "AI",           "ai_control",  0
Data "NONE",         "no_control",  0

controller_data_pmvga:

Data 10, 5
Data "KEYS: CURSOR", "keys_cursor", 1
Data "KEYS: AZ,.",   "keys_cegg",   0
Data "KEYS: AZXC",   "keys_azxc",   0
Data "KEYS: '/,.",   "keys_punc",   0
Data "GAMEPAD A",    "nes_a",       1
Data "GAMEPAD B",    "nes_b",       1
Data "JOYSTICK A",   "atari_a",     1
Data "JOYSTICK B",   "atari_b",     1
Data "AI",           "ai_control",  0
Data "NONE",         "no_control",  0

highscore_data:

Data "TOM, 2000"
Data "MICKEY, 1500"
Data "MIKE, 1250"
Data "PETER, 1000"
Data "DAVEY, 800"
Data "JOHN, 600"
Data "PAUL, 400"
Data "GEORGE, 200"
Data "RINGO, 100"
Data "MOOSE, 50"
