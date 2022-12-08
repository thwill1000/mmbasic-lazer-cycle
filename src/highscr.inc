' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

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
' @return            controller driver if user pressed button/key,
'                    empty string if the duration expired.
Function highscr.show_table$(ctrls$(), duration%)
  Const ch$ = Chr$(205), X_OFFSET% = MM.HRes \ 2, Y_OFFSET% = MM.VRes \ 2
  Text X_OFFSET%, Y_OFFSET% - 95, ch$ + ch$ + " HIGH SCORES " + ch$ + ch$, "CT", 1, 1, Rgb(White)

  Local col_idx%, i%, name$, score$, y%
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
' @param  ctrl$     controller driver for given player.
Sub highscr.edit(player%, idx%, colour_%, ctrl$)
  Const X_OFFSET% = MM.HRes \ 2, Y_OFFSET% = MM.VRes \ 2
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
          Loop Until bits.get%(highscr.ALLOWED_CHARS%(i% \ 64), i% Mod 64)
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
' @param  ctrl$    controller driver for given player.
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

    If highscr.get_input% = &h01 Then
      ' If fire was pressed then wait for it to be released.
      Do While key% : Pause 5 : Call ctrl$, key% : Loop
    Else
      ' Otherwise 'Key' presses repeat, but not too quickly.
      Pause 20
    EndIf
  Else
    highscr.get_input% = key%
  EndIf
End Function
