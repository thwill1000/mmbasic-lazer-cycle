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

  ' Show help text.
  Inc y%, 25
  Local y_help% = y%
  Local help$ = "Use "
  For i% = 146 To 149 : Cat help$, Chr$(i%) + " " : Next
  Cat help$, "and FIRE to confirm"
  Text X_OFFSET%, y_help%, help$, "CT", 1, 1, Rgb(White)

  Local cursor% = 1, key%, p% = 1, t% = Timer, bg%, fg%, state%
  y% = Y_OFFSET% - 75 + 15 * idx%
  name$ = str.rpad$("PLAYER " + Str$(player%), highscr.MAX_NAME_LEN%)

  ctrl.term_keys() ' Regain control of the keyboard for INKEY$
  Do While highscr.get_input%(ctrl$) : Loop ' Wait for player to release controller.

  ' PEEKing the font doesn't work in MMB4W.
  Local font_width% = Choice(Mm.Device$ = "MMBasic for Windows", 8, Peek(Byte Mm.Info(Font Address 1)))
  Local confirm% = 0
  Do
    If Timer > t% + 500 Then
      cursor% = Not cursor%
      t% = Timer
    EndIf

    For i% = 1 To highscr.MAX_NAME_LEN%
      bg% = Choice((i% = p%) And cursor%, colour_%, Rgb(Black))
      fg% = Choice((i% = p%) And cursor%, Rgb(Black), colour_%)
      ch$ = Mid$(name$, i%, 1)
      x% = X_OFFSET% + (i% - 1.5) * font_width%
      Text x%, y%, ch$, , 1, 1, fg%, bg%
    Next

    If confirm% > 0 Then
      Inc confirm%, -1
      If confirm% = 0 Then Text X_OFFSET%, y_help%, help$, "CT", 1, 1, Rgb(White), Rgb(Black) ' Restore the help text.
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
            Text X_OFFSET%, y_help%, help$, "CT", 1, 1, Rgb(Black), Rgb(Black)
            Text X_OFFSET%, y_help%, " Press FIRE again to confirm ", "CT", 1, 1, Rgb(Black), Rgb(White)
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
      If key% <> &h01 Then confirm% = 1 ' So message is cleared
      state% = 0
    EndIf

    ' Sanity check.
    If Len(name$) <> highscr.MAX_NAME_LEN% Then Error "Invalid name length"

  Loop While key% <> -1

  ' Delete the help text.
  Text X_OFFSET%, y_help%, help$, "CT", 1, 1, Rgb(Black), Rgb(Black)

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