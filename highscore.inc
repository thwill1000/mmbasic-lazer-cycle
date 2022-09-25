' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

Const highscore.MAX_NAME_LEN% = 8

Dim highscore.values$(9) Length highscore.MAX_NAME_LEN% + 7

' Shows the high-score table for a specified duration or until the user presses
' START/FIRE/SPACE.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function highscore.show_table%(duration%)
  Const ch$ = Chr$(205)
  Text X_OFFSET%, Y_OFFSET% - 95, ch$ + ch$ + " HIGH SCORES " + ch$ + ch$, "CT", 1, 1, Rgb(White)

  Local col_idx%, ctrl$, i%, name$, score$, y%
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF)
  Local colours%(3) = (Rgb(Red), Rgb(Yellow), Rgb(Cyan), Rgb(Green))

  keyboard_init()
  Do While Timer < expires% And ctrl$ = ""

    For i% = 0 To Bound(highscore.values$(), 1) + 1
      If i% <= Bound(highscore.values$(), 1) Then
        name$ = str.rpad$(Field$(highscore.values$(i%), 1), highscore.MAX_NAME_LEN%)
        score$ = str.lpad$(Field$(highscore.values$(i%), 2), 5)
        y% = Y_OFFSET% - 75 + 15 * i%
        Text X_OFFSET%, y%, score$ + "  " + name$, "CT", 1, 1, colours%(col_idx%)
      EndIf
      col_idx% = (col_idx% + 1) Mod 4
    Next

    If Not(InStr(Mm.Device$, "PicoMite")) Then Page Copy 1 To 0, B

    ctrl$ = poll_for_controller$(200)
  Loop

  highscore.show_table% = ctrl$ <> ""
End Function

' Provides UI for editing an entry in the high-score table.
'
' @param  idx%     index of entry in the highscore.values$ array.
' @param  player%  player index.
' @param  ctrl$    controller for given player.
Sub highscore.edit(idx%, player%, ctrl$)
  ' 256-bit map of characters that can be entered into the high-score table
  ' via gamepad or joystick. For each character 0-255 the character is allowed
  ' if the corresponding bit is set.
'  Static ALLOWED_CHARS%(3) = (&h03FF440100000000, &h000000007FFFFFF, &h0000020080001E00, &h8000000000000000)
  Static ALLOWED_CHARS%(3) = (&h03FF440100000000, &h000000007FFFFFF, &h0000020080001E00, &h0)

  Local ch$ = Chr$(205)
  Text X_OFFSET%, Y_OFFSET% - 95, ch$ + ch$ + " HIGH SCORES " + ch$ + ch$, "CT", 1, 1, Rgb(White)

  ' Draw current high-score table.
  Local i%, name$, score$, x%, y%
  For i% = 0 To Bound(highscore.values$(), 1)
    name$ = str.rpad$(Field$(highscore.values$(i%), 1), highscore.MAX_NAME_LEN%)
    score$ = str.lpad$(Field$(highscore.values$(i%), 2), 5)
    y% = Y_OFFSET% - 75 + 15 * i%
    Text X_OFFSET%, y%, score$ + "  " + name$, "CT", 1, 1, Choice(i% = idx%, Rgb(Red), Rgb(Blue))
  Next

  Local cursor% = 1, key%, p% = 1, t% = Timer, bg%, fg%, play_sound%
  y% = Y_OFFSET% - 75 + 15 * idx%
  name$ = ""

  ' Re-initialise controller as earlier call to poll_for_controller$() will
  ' have reset pins.
  keyboard_init()
  Select Case ctrl$
    Case "ctrl_nes_a%"   : nes_init(PORT_A%)
    Case "ctrl_nes_b%"   : nes_init(PORT_B%)
    Case "ctrl_atari_a%" : atari_init(PORT_A%)
    Case "ctrl_atari_b%" : atari_init(PORT_B%)
  End Select

  Local font_width% = Peek(Byte Mm.Info(Font Address 1))
  Do
    If Timer > t% + 500 Then
      cursor% = Not cursor%
      t% = Timer
    EndIf

    For i% = 1 To highscore.MAX_NAME_LEN%
      bg% = Choice((i% = p%) And cursor%, Rgb(Red), Rgb(Black))
      fg% = Choice((i% = p%) And cursor%, Rgb(Black), Rgb(Red))
      ch$ = Choice(i% <= Len(name$), Mid$(name$, i%, 1), " ")
      x% = X_OFFSET% + (i% - 1.5) * font_width%
      Text x%, y%, ch$, , 1, 1, fg%, bg%
    Next

    ' Display green check mark if next right/fire will OK the name.
    Inc x%, 2 * font_width%
    Text x%, y%, Chr$(129), , 1, 1, Choice(p% = highscore.MAX_NAME_LEN%, Rgb(Green), Rgb(Black))

    If Not(InStr(Mm.Device$, "PicoMite")) Then Page Copy 1 To 0, B
    Pause 100

    key% = highscore.get_input%(ctrl$)
    If key% > 0 Then
      play_sound% = 1
      Select Case key%
        Case &h08 ' Backspace
          If p% > 1 Then
            name$ = Left$(name$, p% - 2) + Mid$(name$, p%)
            Inc p%, -1
          EndIf
        Case &h7F ' Delete
          name$ = Left$(name$, p% - 1) + Mid$(name$, p% + 1)
        Case &h0A, &h0D ' LF and CR
          key% = -1 ' So we exit the DO LOOP.
        Case &h20 To &h7E
          Do While p% > Len(name$) : Cat name$, " " : Loop
          name$ = Left$(Left$(name$, p% - 1) + Chr$(key%) + Mid$(name$, p%), highscore.MAX_NAME_LEN%)
          If p% < highscore.MAX_NAME_LEN% Then Inc p%, 1
        Case &h80, &h81 ' Up and down arrows
          i% = Asc(Mid$(name$, p%, 1))
          If i% = 0 Then i% = 64 ' @
          Do
            Inc i%, Choice(key% = &h80, 1, -1)
            If i% < 0 Then i% = 255
            If i% > 255 Then i% = 0
          Loop Until bits.get%(ALLOWED_CHARS%(i% \ 64), i% Mod 64)
          Do While p% > Len(name$) : Cat name$, " " : Loop
          name$ = Left$(name$, p% - 1) + Chr$(i%) + Mid$(name$, p% + 1)
        Case &h82 ' Left arrow
          If p% > 1 Then Inc p%, -1
        Case &h83 ' Right arrow
          If p% < highscore.MAX_NAME_LEN% Then Inc p%, 1 Else key% = -1
        Case Else
          play_sound% = 0
      End Select
    End If

    If play_sound% Then
      start_soundfx(Peek(VarAddr SOUNDFX_SELECT%()))
      play_sound% = 0
    EndIf

  Loop While key% <> -1

  Text x%, y%, Chr$(129), , 1, 1, Rgb(Black)
  highscore.values$(idx%) = name$ + ", " + Field$(highscore.values$(idx%), 2)
End Sub

' Gets player input to edit entry in the high-score table.
'
' @param  ctrl$    controller for given player.
' @return          ASCII code of key pressed, or negative of value read
'                  from player's configured controller port.
Function highscore.get_input%(ctrl$)
  highscore.get_input% = Asc(UCase$(Inkey$))
  If highscore.get_input% > 0 Then Exit Function

  Local p%, s$
  Select Case ctrl$
    Case "ctrl_nes_a%"   : p% = PORT_A% : s$ = "nes"
    Case "ctrl_nes_b%"   : p% = PORT_B% : s$ = "nes"
    Case "ctrl_atari_a%" : p% = PORT_A% : s$ = "atari"
    Case "ctrl_atari_b%" : p% = PORT_B% : s$ = "atari"
    Case Else            : Exit Function
  End Select

  ' Note that port initialisation should have already occurred earlier.
  Local k% = Call(s$ + "_read%", p%)
  Select Case k%
    Case NES_A%     : k% = &h83 ' Right arrow
    Case NES_START% : k% = &h0A ' Line feed \n
    Case NES_UP%    : k% = &h80 ' Up arrow
    Case NES_DOWN%  : k% = &h81 ' Down arrow
    Case NES_LEFT%  : k% = &h82 ' Left arrow
    Case NES_RIGHT% : k% = &h83 ' Right arrow
    Case Else       : k% = 0
  End Select
  highscore.get_input% = k%
End Function
