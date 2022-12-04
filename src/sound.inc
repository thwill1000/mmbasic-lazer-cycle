' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

Const sound.MAX_TRACK_LEN% = 1024
Const sound.NUM_MUSIC_CHANNELS% = 3

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
Dim sound.current_track%
Dim sound.num_tracks%
Dim sound.fx_enabled% = 1
Dim sound.fx_ptr% = Peek(VarAddr sound.FX_NONE%())

' Initialises sound library.
Sub sound.init(track1$, track2$)
  Local i%, j%

  ' sound.F!(0) - rest - 10 Hz, which should be inaudible.
  ' sound.F!(1) - C0   - 16.35 Hz
  sound.F!(0) = 10.0
  For i% = 1 To 127
    sound.F!(i%) = 440 * 2^((i% - 58) / 12.0)
  Next

  If track1$ = "" Then Error "No music tracks"
  sound.num_tracks% = 2
  Local tracks$(sound.num_tracks%) Length 32
  tracks$(1) = track1$
  tracks$(2) = Choice(track2$ = "", track1$, track2$)

  ' 2D array indexed (notes, tracks) because that is the way
  ' incrementing a pointer through the data naturally works.
  Dim sound.MUSIC%((sound.MAX_TRACK_LEN% \ 8) - 1, sound.num_tracks% - 1)

  Local count%, num_channels%, track$
  For i% = 1 to sound.num_tracks%
    track$ = tracks$(i%)
    Restore track$
    Read count%, num_channels%
    If count% > sound.MAX_TRACK_LEN% Then
      Local err$ = "Track '" + track$ + "' is too long; "
      Error err$ + "expected " + Str$(sound.MAX_TRACK_LEN%) + " bytes but found " + Str$(count%)
    EndIf
    If num_channels% <> sound.NUM_MUSIC_CHANNELS% Then
      Local err$ = "Track '" + track$ + "' has wrong number of channels; "
      Error err$ + "expected " + Str$(sound.NUM_CHANNELS%) + ", but found " + Str$(num_channels%)
    EndIf
    For j% = 1 To count% \ 8
      Read sound.MUSIC%(j% - 1, i% - 1)
    Next j%
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
  sound.current_track% = 1
End Sub

' Called from interrupt to play next note of music.
Sub sound.music_int()
  Local n% = Peek(Byte sound.music_ptr%)
  If n% < 255 Then
    Play Sound 1, B, S, sound.F!(n%), 15
    Play Sound 2, B, S, sound.F!(Peek(Byte sound.music_ptr% + 1)), 15
    Play Sound 3, B, S, sound.F!(Peek(Byte sound.music_ptr% + 2)), 15
    Inc sound.music_ptr%, 3
    Exit Sub
  EndIf

  If sound.music_start_ptr% = Peek(VarAddr sound.NO_MUSIC%()) Then
    sound.music_ptr% = sound.music_start_ptr%
  Else
    Inc sound.current_track%
    If sound.current_track% > sound.num_tracks% Then sound.current_track% = 1
    sound.music_ptr% = sound.music_start_ptr% + (sound.current_track% - 1) * sound.MAX_TRACK_LEN%
  EndIf
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
