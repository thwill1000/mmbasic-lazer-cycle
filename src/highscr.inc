' Copyright (c) 2022-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.06

Const highscr.MAX_NAME_LEN% = 8

Dim highscr.values$(9) Length highscr.MAX_NAME_LEN% + 7

' 256-bit map of characters that can be entered into the high-score table
' via gamepad or joystick. For each character 0-255 the character is allowed
' if the corresponding bit is set.
'  Dim highscr.ALLOWED_CHARS%(3) = (&h03FF440100000000, &h000000007FFFFFF, &h0000020080001E00, &h8000000000000000)
Dim highscr.ALLOWED_CHARS%(3) = (&h03FF440100000000, &h000000007FFFFFF, &h0000020080001E00, &h0)

' Initialises high-score library.
'
' @param  filename$    file to read/write high-scores to.
' @param  data_label$  label for DATA to read default high-scores from.
Sub highscr.init(filename$, data_label$)
  Local i%

  ' It is convenient to always fill with the defaults
  ' even if we end up overwriting them immediately.
  Read Save
  Restore data_label$
  For i% = 0 To Bound(highscr.values$(), 1)
    Read highscr.values$(i%)
  Next
  Read Restore

  ' If there is no file-system then do not load high-scores.
  If Not highscr.has_fs%() Then Exit Sub

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

' Does the device have a file-system ?
Function highscr.has_fs%()
  If Mm.Device$ = "MMBasic for Windows" Or InStr(Mm.Device$, "PicoMite") Then
    highscr.has_fs% = 1
  Else
    highscr.has_fs% = (UCase$(Mm.Info$(SdCard)) = "READY")
  EndIf
End Function

' Assumes parent directory of filename$ already exists.
Sub highscr.save(filename$)
  ' If there is no file-system then do not save high-scores.
  If Not highscr.has_fs%() Then Exit Sub

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
' @return            controller driver if user pressed button/key,
'                    empty string if the duration expired.
Function highscr.show_table$(ctrls$(), duration%)
  Const ch$ = Chr$(205), X_OFFSET% = MM.HRes \ 2, Y_OFFSET% = MM.VRes \ 2
  Local col_idx%, i%, name$, score$, y%
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF)
  Local colours%(3) = (Rgb(Red), Rgb(Yellow), Rgb(Cyan), Rgb(Green))

  Cls
  Text X_OFFSET%, Y_OFFSET% - 95, ch$ + ch$ + " HIGH SCORES " + ch$ + ch$, "CT", 1, 1, Rgb(White)

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
' @param  ctrl$     controller driver for given player.
Sub highscr.edit(player%, idx%, colour_%, ctrl$)
  Const HEIGHT% = 16, WIDTH% = 16
  Const X_ORIGIN% = (MM.HRes - 10 * WIDTH%) \ 2
  Const Y_ORIGIN% = Int(5.5 * HEIGHT%)
  Const Y_FOOTER% = Mm.VRes - 16

  Local bg%, ch%, count%, fg%, grid$, i%, key%, p%, t%
  Local name$, s$, space_is_fire%, state%, x%, x_new%, y%, y_new%
  Local footer$(1) Length 40

  ' Initialise footer text.
  footer$(0) = "   Use * * * * and FIRE to select    "
  footer$(1) = " Or, type name and ENTER to confirm  "
  Poke Var footer$(0), 8, 146
  Poke Var footer$(0), 10, 147
  Poke Var footer$(0), 12, 148
  Poke Var footer$(0), 14, 149

  Cls

  ' Draw title.
  s$ = Chr$(205)
  Text Mm.HRes \ 2, 25, s$ + s$ + " " + Str$(idx% + 1) + "   PLACE HIGH SCORE " + s$ + s$, "CT", 1, 1, Rgb(White)
  Select Case idx%
    Case 0 : s$ = "ST"
    Case 1 : s$ = "ND"
    Case 2 : s$ = "RD"
    Case Else : s$ = "TH"
  End Select
  Text Choice(idx% = 9, 100, 96), 25, s$, "CT", 7, 1, Rgb(White)

  ' Draw player number.
  Text Mm.HRes \ 2, 3 * HEIGHT%, "PLAYER " + Str$(player%), "CT", 1, 1, colour_%

  ' Draw the character grid.
  Restore highscr.grid_data
  For y% = 0 To 4
    For x% = 0 To 9
      Read ch%
      Cat grid$, Chr$(ch%)
      If ch% = 10 Then ch% = 32 ' Use space as place-holder for OK / Line Feed.
      If ch% = 8 Then ch% = 149 ' Use left arrow character for backspace.
      Text X_ORIGIN% + x% * WIDTH% + 4, Y_ORIGIN% + y% * HEIGHT% + 3, Chr$(ch%), , 1, 1, Rgb(White)
    Next
  Next
  Text X_ORIGIN% + 9 * WIDTH% + 2, Y_ORIGIN + 4 * HEIGHT% + 5, "OK", , 7, 1, Rgb(White)

  ctrl.term_keys() ' Regain control of the keyboard for INKEY$
  Call ctrl$, ctrl.OPEN
  Do While highscr.get_input%(ctrl$) : Loop ' Wait for player to release controller.

  space_is_fire% = 1 : t% = Timer
  x% = -1 : x_new% = 0 : y% = -1 : y_new% = 0
  Do
    ' Draw current name and flashing cursor.
    p% = Mm.HRes / 2
    Inc p%, -4 * Min(Len(name$) + 1, highscr.MAX_NAME_LEN%)
    Text p% - 8, 4 * HEIGHT%, " ", , 1, 1
    For i% = 1 To Min(Len(name$) + 1, highscr.MAX_NAME_LEN%)
      bg% = Choice((i% = Len(name$) + 1) And (count% And &b1), colour_%, Rgb(Black))
      fg% = Choice((i% = Len(name$) + 1) And (count% And &b1), Rgb(Black), colour_%)
      s$ = Choice(i% = Len(name$) + 1, " ", Mid$(name$, i%, 1))
      Text p%, 4 * HEIGHT%, s$, , 1, 1, fg%, bg%
      Inc p%, 8
    Next
    Text p%, 4 * HEIGHT%, " ", , 1, 1
    Inc p%, 8

    ' Draw selection box.
    If x% <> x_new% Or y% <> y_new% Then
      If x% <> -1 Then
        Box X_ORIGIN% + x% * WIDTH%, Y_ORIGIN% + y% * HEIGHT% + 1, WIDTH%, HEIGHT%, 1, Rgb(Black)
      EndIf
      x% = x_new% : y% = y_new%
      Box X_ORIGIN% + x% * WIDTH%, Y_ORIGIN% + y% * HEIGHT% + 1, WIDTH%, HEIGHT%, 1, colour_%
    EndIf

    ' Draw footer text.
    If Timer > t% + 500 Then
      count% = (count% + 1) Mod 10
      t% = Timer
    EndIf
    Text Mm.HRes \ 2, Y_FOOTER%, footer$(count% >= 5), "CT", 1, 1, Rgb(White), Rgb(Black)

    If Not(InStr(Mm.Device$, "PicoMite")) Then Page Copy 1 To 0, B

    key% = highscr.get_input%(ctrl$)
    state% = key% > 0

    If key% > 0 Then
      If key% <> &h20 Then space_is_fire% = (key% > &h7F)
      If key% = &h20 And space_is_fire% Then key% = ctrl.A << 8
      If key% = ctrl.A << 8 Then key% = Asc(Mid$(grid$, x% + y% * 10 + 1, 1))

      Select Case key%
        Case &h08, &h7F ' Backspace and Delete
          If Len(name$) > 0 Then name$ = Left$(name$, Len(name$) - 1) Else state% = 2
        Case &h0A, &h0D ' LF and CR
          key% = -1 ' So we exit the DO LOOP.
        Case ctrl.UP << 8
          If y% > 0 Then y_new% = y% - 1 Else y_new% = 4
        Case ctrl.DOWN << 8
          If y% < 4 Then y_new% = y% + 1 Else y_new% = 0
        Case ctrl.LEFT << 8
          If x% > 0 Then x_new% = x% - 1 Else x_new% = 9
        Case ctrl.RIGHT << 8
          If x% < 9 Then x_new% = x% + 1 Else x_new% = 0
        Case &h20 To &hFF ' ASCII
          If Len(name$) < highscr.MAX_NAME_LEN% Then Cat name$, Chr$(key%) Else state% = 2
      End Select
    End If

    If state% Then
      If state% = 1 Then sound.play_fx(sound.FX_SELECT%()) Else sound.play_fx(sound.FX_BLART%())
      Pause 150
    EndIf

  Loop While key% <> -1

  ' Delete the footer text.
  Text Mm.HRes \ 2, Y_FOOTER%, Space$(40), "CT", 1, 1, Rgb(White), Rgb(Black)

  ' Don't allow empty names.
  name$ = str.trim$(name$)
  If name$ = "" Then name$ = "PLAYER " + Str$(player%)

  highscr.values$(idx%) = name$ + ", " + Field$(highscr.values$(idx%), 2)
End Sub

highscr.grid_data:
Data  65,  66,  67,  68,  69,  70,  71,  72, 73,  74  ' A .. J
Data  75,  76,  77,  78,  79,  80,  78,  82, 83,  84  ' K .. T
Data  85,  86,  87,  88,  89,  90,  42, 46,  64, 169  ' U .. Z, space, *, period, @
Data  48,  49,  50,  51,  52,  53,  54,  55, 56,  57  ' 0 .. 9
Data  63,  45, 137, 138, 139, 140, 159, 32,   8,  10  ' question-mark, hyphen, diamond, club,
                                                      ' spade, heart, star, backspace, enter
' Gets player input to edit entry in the high-score table.
'
' @param  ctrl$  controller driver to read - should already be OPEN.
' @return        ASCII code of key pressed, or value read from
'                controller shifted left 8 places.
Function highscr.get_input%(ctrl$)
  highscr.get_input% = Asc(UCase$(Inkey$))
  If highscr.get_input% Then
    Select Case highscr.get_input%
      Case &h80 : highscr.get_input% = ctrl.UP << 8
      Case &h81 : highscr.get_input% = ctrl.DOWN << 8
      Case &h82 : highscr.get_input% = ctrl.LEFT << 8
      Case &h83 : highscr.get_input% = ctrl.RIGHT << 8
    End Select
  Else
    Call ctrl$, highscr.get_input%
    If highscr.get_input% = ctrl.B Then highscr.get_input% = ctrl.A
    If highscr.get_input% Then highscr.get_input% = highscr.get_input% << 8
  EndIf
End Function
