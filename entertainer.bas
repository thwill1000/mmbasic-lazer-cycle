' Plays "The Entertainer" by Scott Joplin
'   - also encodes it as DATA statements for playing outside of this program.
'
' Code Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.03

Option Base 0
Option Default None
Option Explicit On

Option Break 4
On Key 3, on_break

Const FILE$ = "entertainer"
Const SZ% = 256
' Const NUM_CHANNELS% = music.prompt_for_num_channels%()
Const NUM_CHANNELS% = 3

Dim channel1%(SZ%), channel2%(SZ%), channel3%(SZ%), channel4%(SZ%)
Dim err$

If InStr(MM.Device$, "PicoMite") Then Save FILE$ + ".bas"

music.compose()
music.process()
music.write_data()
music.play()

End

' Interrupt routine to stop music and restore default Break Key.
Sub on_break
  Play Stop
  Option Break 3
  End
End Sub

' Prompts user for number of channels to encode / play.
Function music.prompt_for_num_channels%()
  Local s$
  Do
    Line Input "How many channels (1-4)? ", s$
    Print
    Select Case Val(s$)
      Case 1 To 4 : Exit Do
    End Select
  Loop
  music.prompt_for_num_channels% = Val(s$)
End Function

' Fills channels{1-3}% with musical notes for "The Entertainer".
Sub music.compose()
  ' ---------- Line 0 ----------

  music.parse(channel1%(), "qD4,qD#4")

  music.parse(channel2%(), "1-")

  music.parse(channel3%(), "1-")

  ' ---------- Line 1 ----------

  music.parse(channel1%(), "qE4,1C5,qE4,1C5,qE4,qC5")
  music.parse(channel1%(), "2C5,q-,qC5,qD5,qD#5")
  music.parse(channel1%(), "qE5,qC5,qD5,qE5,qE5,qB5,1D5")
  music.parse(channel1%(), "3C5,qD4,qD#4")

  music.parse(channel2%(), "1C4,1C3,1D#3,1E3")
  music.parse(channel2%(), "1F3,1G3,1A4,1B4")
  music.parse(channel2%(), "1C4,1E3,1F3,1G3")
  music.parse(channel2%(), "1C3,1G3,1C4,qC4,q-")

  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")

  ' ---------- Line 2 ----------

  music.parse(channel1%(), "qE4,1C5,qE4,1C5,qE4,qC5")
  music.parse(channel1%(), "3C5,qA5,qG4")
  music.parse(channel1%(), "qF#4,qA5,qC5,qE5,qE5,qD5,qC5,qA5")
  music.parse(channel1%(), "3D5,qD4,qD#4")

  music.parse(channel2%(), "1C4,1C3,1D#3,1E3")
  music.parse(channel2%(), "1F3,1G3,1A4,1C#3")
  music.parse(channel2%(), "1D3,1F#3,1A4,1D3")
  music.parse(channel2%(), "1G3,1F3,1E3,1D3")

  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")

  ' ---------- Line 3 ----------

  music.parse(channel1%(), "qE4,1C5,qE4,1C5,qE4,qC5")
  music.parse(channel1%(), "2C5,q-,qC5,qD5,qD#5")
  music.parse(channel1%(), "qE5,qC5,qD5,qE5,qE5,qB5,1D5")
  music.parse(channel1%(), "2C5,qC5,q-,qC5,qD5")

  music.parse(channel2%(), "1C3,1C4,1D#3,1E3")
  music.parse(channel2%(), "1F3,1G3,1A4,1B4")
  music.parse(channel2%(), "1C4,1E3,1F3,1G3")
  music.parse(channel2%(), "1C4,1G3,2C3")

  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")
  music.parse(channel3%(), "4-")

  ' ---------- Line 4 ----------

  music.parse(channel1%(), "qE5,qC5,qD5,qE5,qE5,qC5,qD5,qC5")
  music.parse(channel1%(), "qE5,qC5,qD5,qE5,qE5,qC5,qD5,qC5")
  music.parse(channel1%(), "qE5,qC5,qD5,qE5,qE5,qB5,1D5")
  music.parse(channel1%(), "2C5,qC5,qE4,qF4,qF#4")

  music.parse(channel2%(), "qC5,q-,qC5,q-,qBb5,q-,qBb5,q-")
  music.parse(channel2%(), "qA5,q-,qA5,q-,qAb5,q-,qAb5,q-")
  music.parse(channel2%(), "qG4,q-,qG4,q-,1G4,1-")
  music.parse(channel2%(), "1-,1G3,qC4,q-,1-")

  music.parse(channel3%(), "qC4,q-,qC4,q-,qBb4,q-,qBb4,q-")
  music.parse(channel3%(), "qA4,q-,qA4,q-,qAb4,q-,qAb4,q-")
  music.parse(channel3%(), "qG3,q-,qG3,q-,1G3,1-")
  music.parse(channel3%(), "1-,1G2,qC3,q-,1-")

  ' ---------- Line 5 ----------

  music.parse(channel1%(), "1G4,qA5,qG4,qG4,qE4,qF4,qF#4")
  music.parse(channel1%(), "1G4,qA5,qG4,qG4,qE5,qC5,qG4")
  music.parse(channel1%(), "qA5,qB5,qC5,qD5,qE5,qD5,qC5,qD5")
  music.parse(channel1%(), "2G4,qG4,qE4,qF4,qF#4")

  music.parse(channel2%(), "1E4,qF4,qE4,qE4,q-,1-")
  music.parse(channel2%(), "1E4,qF4,qE4,qE4,q-,1-")
  music.parse(channel2%(), "4-")
  music.parse(channel2%(), "4-")

  music.parse(channel3%(), "1C4,1-,1G3,1-")
  music.parse(channel3%(), "1C4,1-,1G3,1-")
  music.parse(channel3%(), "2F3,2G3")
  music.parse(channel3%(), "1C3,1G3,1C4,1-")

  ' ---------- Line 6 ----------

  music.parse(channel1%(), "1G4,qA5,qG4,qG4,qE4,qF4,qF#4")
  music.parse(channel1%(), "1G4,qA5,qG4,qG4,qG4,qA5,qA#5")
  music.parse(channel1%(), "1B5,q-,1B5,qA5,qF#4,qD4")
  music.parse(channel1%(), "2G4,qG4,qE4,qF4,qF#4")

  music.parse(channel2%(), "1E4,qF4,qE4,qE4,q-,1-")
  music.parse(channel2%(), "1E4,qF4,qE4,qE4,q-,1-")
  music.parse(channel2%(), "4-")
  music.parse(channel2%(), "4-")

  music.parse(channel3%(), "1C4,1-,1G3,1-")
  music.parse(channel3%(), "1C4,1-,1G3,1C#4")
  music.parse(channel3%(), "2D4,2D3")
  music.parse(channel3%(), "qG3,q-,qG3,q-,1A4,1B4")

  ' ---------- Line 7 ----------

  music.parse(channel1%(), "1G4,qA5,qG4,qG4,qE4,qF4,qF#4")
  music.parse(channel1%(), "1G4,qA5,qG4,qG4,qE5,qC5,qG4")
  music.parse(channel1%(), "qA5,qB5,qC5,qD5,qE5,qD5,qC5,qD5")
  music.parse(channel1%(), "2C5,qC5,qG4,qF#4,qG4")

  music.parse(channel2%(), "1E4,qF4,qE4,qE4,q-,1-")
  music.parse(channel2%(), "1E4,qF4,qE4,qE4,q-,1-")
  music.parse(channel2%(), "4-")
  music.parse(channel2%(), "4-")

  music.parse(channel3%(), "1C4,1-,1G3,1-")
  music.parse(channel3%(), "1C4,1-,1G3,1-")
  music.parse(channel3%(), "2F3,2G3")
  music.parse(channel3%(), "1C3,1G3,1C4,1-")

  ' ---------- Line 8 ----------

  music.parse(channel1%(), "1C5,qA5,qC5,qC5,qA5,qC5,qA5")
  music.parse(channel1%(), "qG4,qC5,qE5,qG5,qG5,qE5,qC5,qG4")
  music.parse(channel1%(), "1A5,1C5,qE5,1D5,qD5")
  music.parse(channel1%(), "3C5,1-")

  music.parse(channel2%(), "qF4,q-,qF4,q-,qF#4,q-,qF#4,q-")
  music.parse(channel2%(), "qG4,q-,qG4,q-,qE4,q-,qE4,q-")
  music.parse(channel2%(), "qF4,q-,qF4,q-,qG4,q-,qG4,q-")
  music.parse(channel2%(), "1C5,1G4,1C4,1-")

  music.parse(channel3%(), "qF3,q-,qF3,q-,qF#3,q-,qF#3,q-")
  music.parse(channel3%(), "qG3,q-,qG3,q-,qE3,q-,qE3,q-")
  music.parse(channel3%(), "qF3,q-,qF3,q-,qG3,q-,qG3,q-")
  music.parse(channel3%(), "1C4,1G3,1C3,1-")
End Sub

' Parses comma separated list of notes.
'
' @param  channel%()  parsed notes are appended to this array.
' @param  s$          comma separated list.
Sub music.parse(channel%(), s$)
  Local s_idx% = 1
  Local ch$ = ","
  Do While ch$ = ","
    If music.parse_note%(channel%(), s$, s_idx%) <> 0 Then
      Error err$ + " : s_idx = " + Str$(s_idx%)
    EndIf
    Inc s_idx%
    ch$ = Mid$(s$, s_idx%, 1)
    Inc s_idx%
  Loop
End Sub

' Parses single note.
'
' @param  channel%()  parsed note is appended to this array.
' @param  s$          note is parsed from this string ...
' @param  s_idx%      ... starting at this index. On exit this will
'                     contain the index of the last character parsed.
' @return             0 on success, -1 on error. Error message will
'                     be in the global err$ variable.
Function music.parse_note%(channel%(), s$, s_idx%)
  music.parse_note% = -1
  Local i%
  Local ch$ = Mid$(s$, s_idx%, 1)

  ' Parse duration.
  Local duration%
  Select Case ch$
    Case "q": duration% = 1
    Case "1", "2", "3", "4": duration% = 2 * Val(ch$)
    Case Else
      err$ = "Syntax error: expected duration"
      Exit Function
  End Select

  Inc s_idx%
  ch$ = Mid$(s$, s_idx%, 1)

  ' Parse note.
  Local n% = 0 ' 0 = Rest, 1 = A0
  Select Case ch$
    Case "A" : n% = 1
    Case "B" : n% = 3
    Case "C" : n% = 4
    Case "D" : n% = 6
    Case "E" : n% = 8
    Case "F" : n% = 9
    Case "G" : n% = 11
    Case "-" : ' Do nothing
    Case Else
      err$ = "Syntax error: expected note"
      Exit Function
  End Select

  If n% = 0 Then
    For i% = 1 To duration%
      LongString Append channel%(), Chr$(0)
    Next
    music.parse_note% = 0
    Exit Function
  EndIf

  Inc s_idx%
  ch$ = Mid$(s$, s_idx%, 1)

  ' Parse b or #.
  Local off% = 0
  Select Case ch$
    Case "b" : off% = -1
    Case "#" : off% = 1
  End Select
  If off% <> 0 Then
    Inc n%, off%
    Inc s_idx%
    ch$ = Mid$(s$, s_idx%, 1)
  EndIf

  ' Parse octave.
  If Not Instr("012345678", ch$) Then
    err$ = "Syntax error: expected octave"
    Exit Function
  EndIf
  Inc n%, 12 * Val(ch$)

  ' Write note into buffer.
  For i% = 1 To duration%
    LongString Append channel%(), Chr$(n%)
  Next

  music.parse_note% = 0
End Function

' Combines the individual channels into a single global music%() array.
Sub music.process()
  ' Terminate each channel with 255 and determine the maximum length.
  Local max_len% = 0, i%, j%
  For i% = 1 To NUM_CHANNELS%
    Execute "LongString Append channel" + Str$(i%) + "%(), Chr$(&hFF)"
    max_len% = Max(max_len%, Eval("LLen(channel" + Str$(i%) + "%())"))
  Next

  ' Pad max_len% to be a multiple of 8.
  Do While max_len% Mod 8 > 0 : Inc max_len% : Loop

  ' Pad each channel with 255 until all the same length.
  For i% = 1 To NUM_CHANNELS%
    Do While Eval("LLen(channel" + Str$(i%) + "%())") < max_len%
      Execute "LongString Append channel" + Str$(i%) + "%(), Chr$(&hFF)"
    Loop
  Next

  ' Combine the channels into a single music buffer.
  Dim music%(1 + (NUM_CHANNELS% * max_len% / 8))
  For i% = 0 To max_len% - 1
    For j% = 1 To NUM_CHANNELS%
      Execute "LongString Append music%(), Chr$(LGetByte(channel" + Str$(j%) + "%(), i%))"
    Next
  Next
End Sub

' Writes music%() array into a file as DATA statements.
Sub music.write_data()
  Local count% = 0, i%, p% = Peek(VarAddr music%()) + 8
  Open FILE$ + ".inc" For Output As #1
  Print #1, "Data " Format$(LLen(music%()), "%-6g") "' Number of bytes of music data."
  Print #1, "Data " Format$(NUM_CHANNELS%,"%-6g") "' Number of channels."
  For i% = 0 To LLen(music%()) - 1 Step 8
    Print #1, Choice(count% = 0, "Data ", ", ");
    Print #1, "&h" Hex$(Peek(Integer p%), 16);
    Inc p%, 8
    Inc count%
    If count% = 4 Then Print #1 : count% = 0
  Next
  Close #1
End Sub

' Plays the contents of the music%() array using interrupts.
Sub music.play()
  Dim music_ptr% = Peek(VarAddr music%()) + 8
  SetTick 200, music.play_interrupt, 1
  Do While music_ptr% <> 0 : Loop
End Sub

' Interrupt routine playing a single half-beat (per channel) from the music%() array.
Sub music.play_interrupt()
  Local i%, n%, halted% = 0
  For i% = 1 To NUM_CHANNELS%
    n% = Peek(Byte music_ptr%)
    Select Case n%
      Case 0
        Play Sound i%, B, O
        Print Str$(i%) ": Rest"
      Case 1 To 254
        Play Sound i%, B, S, 440 * 2^((n% - 49) / 12.0), 15
        Print Str$(i%) ":" 440 * 2^((n% - 49) / 12.0) " hz"
      Case 255
        Play Sound i%, B, O
        Print Str$(i%) ": Halted"
        Inc halted%
    End Select
    Inc music_ptr%
  Next
  If halted% = NUM_CHANNELS% Then music_ptr% = 0 ' Halt
End Sub
