' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

' These would be constants but MMBasic does not support constant arrays
Dim sound.F!(127)
Dim sound.NO_MUSIC%(1)  = (&h0000000000000000, &hFFFFFFFF00000000)
Dim sound.FX_NONE%(1)   = (&hFFFFFFFFFFFFFFFF, &hFFFFFFFFFFFFFFFF)
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
