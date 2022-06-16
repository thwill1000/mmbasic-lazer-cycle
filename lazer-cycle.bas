' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.03

Option Base 0
Option Default None
Option Explicit On

' Save "light-bikes.bas"

Const VERSION$ = "0.9.0"

Select Case Mm.Device$
  Case "Colour Maximite 2", "Colour Maximite 2 G2", "MMBasic for Windows"
    Const IS_CMM2% = 1
  Case Else
    Const IS_CMM2% = 0
End Select

If IS_CMM2% Then
  Option Console Serial
  Mode 7
  Page Write 1
EndIf

Const WIDTH% = Mm.HRes \ 2
Const HEIGHT% = (Mm.VRes - 20) \ 2
Const X_OFFSET% = MM.HRes \ 2
Const Y_OFFSET% = MM.VRes \ 2
Const CMD_NONE% = -1, UP% = 5, DOWN% = 6, LEFT% = 7, RIGHT% = 8, FIRE% = 9
Const NORTH% = 0, EAST% = 1, SOUTH% = 2, WEST% = 3
Const MAX_CYCLE_IDX% = 3
Const MAX_NAME_LEN% = 8
Const SCORE_Y% = 2 * HEIGHT% + 4
Const STATE_OK%    = &b000 ' 0; values 1-3 are "imminent death"
Const STATE_DYING% = &b100 ' 4
Const STATE_DEAD%  = &b101 ' 5

' These would be constants but MMBasic does not support constant arrays
Dim FREQUENCY!(127)
Dim NO_MUSIC%(1)      = (&h0000000000000000, &hFFFFFFFF00000000)
Dim SOUNDFX_NOTHING%(1) = (&hFFFFFFFFFFFFFFFF, &hFFFFFFFFFFFFFFFF)
Dim SOUNDFX_EAT%(1)     = (&hFFFFFFFFFF100C04, &hFFFFFFFFFFFFFFFF)
Dim SOUNDFX_DIE%(2)     = (&h0F10111213141516, &h0708090A0B0C0D0E, &hFF00010203040506)
Dim SOUNDFX_WIPE%(2)    = (&h0706050403020100, &h0F0E0D0C0B0A0908, &hFF16151413121110)
Dim NEXT_DIR%(7)        = (EAST%, NORTH%, WEST%, SOUTH%, EAST%, NORTH%, WEST%, SOUTH%)
Dim SCORE_X%(3)         = (35, 105, 175, 245)
Dim DIRECTIONS%(3)      = (-WIDTH%, 1, WIDTH%, -1)
Dim CTRL_NAMES$(7)      = ("KEYS: AZ,.",  "KEYS: AZXC",  "KEYS: IKOP",  "CONTROLLER A", "CONTROLLER B", "JOYSTICK B",     "AI",       "NONE" )
Dim CTRL_SUBS$(7)       = ("ctrl_keys1%", "ctrl_keys2%", "ctrl_keys3%", "ctrl_a%",      "ctrl_b%",      "ctrl_joystick%", "ctrl_ai%", "ctrl_none%")

Dim score%
Dim difficulty% = 1
Dim cmd% = CMD_NONE%
Dim frame_duration%
Dim next_frame%

' Each cell of the arena takes up 1 byte:
'   bit  0    - occupied by cycle
'   bits 1-2 - index of cycle
'   bits 3-4 - direction cycle was going in when entered cycle
'   bits 5-6 - unused
'   bit  7   - arena wall (other bits will be 0)
Dim arena%(HEIGHT% * WIDTH% \ 8)

Dim cycle.score%(MAX_CYCLE_IDX%)
Dim cycle.nxt%(MAX_CYCLE_IDX%)
Dim cycle.pos%(MAX_CYCLE_IDX%)
Dim cycle.dir%(MAX_CYCLE_IDX%)
Dim cycle.colour%(MAX_CYCLE_IDX%) = ( RGB(Red), RGB(Yellow), RGB(Cyan), RGB(Green) )
Dim cycle.ctrl$(MAX_CYCLE_IDX%) Length 32
Dim cycle.ctrl_backup$(MAX_CYCLE_IDX%) Length 32 = ("ctrl_keys2%", "ctrl_ai%", "ctrl_keys3%", "ctrl_ai%")
Dim cycle.state%(MAX_CYCLE_IDX%)

Dim music_start_ptr% = Peek(VarAddr NO_MUSIC%())
Dim music_ptr% = music_start_ptr%
Dim soundfx_flag% = 1
Dim soundfx_ptr% = Peek(VarAddr SOUNDFX_NOTHING%())
Dim num_players%
Dim high_scores$(9) Length MAX_NAME_LEN% + 7
Dim keys%(31)

' Music and sound effects are played on SetTick interrupts.
SetTick 200, play_music, 1
SetTick 40, play_soundfx, 2

On Key on_key()

init_globals()
read_music()
clear_display()
wipe()

music_start_ptr% = Peek(VarAddr MUSIC%())

Do
  show_title()
  wipe()
  show_high_scores(0, 5000)
  wipe()
  show_menu()
  wipe()
  init_game()
  draw_arena()
  game_loop()
  show_game_over()
  wipe()
Loop

End

' Initialises global variables.
Sub init_globals()
  Local i%
  ' FREQUENCY(0) - rest - 10 Hz, which should be inaudible.
  ' FREQUENCY(1) - C0   - 16.35 Hz
  FREQUENCY!(0) = 10.0
  For i% = 1 To 127
    FREQUENCY!(i%) = 440 * 2^((i% - 58) / 12.0)
  Next

  high_scores$(0) = "TOM, 2000"
  high_scores$(1) = "MICKEY, 1500"
  high_scores$(2) = "MIKE, 1250"
  high_scores$(3) = "PETER, 1000"
  high_scores$(4) = "DAVEY, 800"
  high_scores$(5) = "JOHN, 600"
  high_scores$(6) = "PAUL, 400"
  high_scores$(7) = "GEORGE, 200"
  high_scores$(8) = "RINGO, 100"
  high_scores$(9) = "MOOSE, 50"
End Sub

Sub read_music()
  Restore music_data
  Local count%
  Read count%
  Local num_channels%
  Read num_channels%
  count% = count% \ 8
  Dim MUSIC%(count%)
  Local i%
  For i% = 1 To count%
    Read MUSIC%(i% - 1)
  Next
End Sub

Sub show_title()
  Text X_OFFSET%, Y_OFFSET% - 15, "LAZER CYCLE", "CM", 1, 2, RGB(White)
  Text X_OFFSET%, Y_OFFSET% + 8, "(c) 2022 Thomas Hugo Williams", "CM", 7, 1, RGB(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 20, "www.sockpuppetstudios.com", "CM", 7, 1, RGB(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 40, "PRESS " + Choice(IS_CMM2%, "SPACE", "SELECT"), "CM", 1, 1, RGB(White)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait(5000)
End Sub

Sub clear_display()
   Box 0, 0, Mm.HRes, Mm.VRes, 1, RGB(Black), RGB(Black)
   If IS_CMM2% Then Page Copy 1 To 0, B
End Sub

Sub wait(duration%)
  cmd% = CMD_NONE%
  If duration% = 0 Then
    Do While cmd% <> FIRE% : Loop
  Else
    Local expires% = Timer + duration%
    Do While Timer < expires% And cmd% <> FIRE% : Loop
  EndIf
End Sub

Sub show_high_scores(edit%, idx%, player%)
  Const x% = X_OFFSET% - 100
  Local ch$, col%, col_idx%, i%, name$, offset%, score$, y%
  Local expires% = Choice(edit%, &h7FFFFFFFFFFFFFFF, Timer + idx%)

  ch$ = Chr$(205)
  Text X_OFFSET%, Y_OFFSET% - 95, ch$ + ch$ + " HIGH SCORES " + ch$ + ch$, "CT", 1, 1, RGB(White)

  Do While Inkey$ <> "" : Loop

  cmd% = 0
  Do While Timer < expires% And cmd% <> FIRE%

    ' Draw high-score table.
    For i% = 0 To Bound(high_scores$(), 1) + 1
      If i% <= Bound(high_scores$(), 1) Then
        name$ = Field$(high_scores$(i%), 1)
        If edit% And i% = idx% And col_idx% < 2 And Len(name$) < MAX_NAME_LEN% Then Cat name$, "_"
        name$ = str.rpad$(name$, MAX_NAME_LEN%)
        score$ = str.lpad$(Field$(high_scores$(i%), 2), 5)
        y% = Y_OFFSET% - 75 + 15 * i%
        If edit% Then
          col% = Choice(i% = idx%, cycle.colour%(player%), Rgb(White))
        Else
          col% = cycle.colour%(col_idx%)
        EndIf
        Text X_OFFSET%, y%, score$ + "  " + name$, "CT", 1, 1, col%
        Inc y%, 15
      EndIf
      col_idx% = (col_idx% + 1) Mod 4
    Next

    If IS_CMM2% Then Page Copy 1 To 0, B
    Pause 200

    ' Handle input when editing table.
    ch$ = UCase$(Inkey$)
    If edit% And ch$ <> "" Then
      name$ = Field$(high_scores$(idx%), 1)
      score$ = Field$(high_scores$(idx%), 2)
      Select Case Asc(ch$)
        Case &h0A, &h0D
          start_soundfx(Peek(VarAddr SOUNDFX_EAT%()))
          Exit Do
        Case &h08, &h7F
          If Len(name$) > 0 Then
            name$ = Left$(name$, Len(name$) - 1)
            start_soundfx(Peek(VarAddr SOUNDFX_EAT%()))
          EndIf
        Case &h20 To &h7E
          If Len(name$) < 8 Then
            Cat name$, ch$
            start_soundfx(Peek(VarAddr SOUNDFX_EAT%()))
          EndIf
      End Select
      high_scores$(idx%) = name$ + ", " + score$
    End If
  Loop
End Sub

Function str.lpad$(s$, x%)
  str.lpad$ = s$
  If Len(s$) < x% Then str.lpad$ = Space$(x% - Len(s$)) + s$
End Function

Function str.rpad$(s$, x%)
  str.rpad$ = s$
  If Len(s$) < x% Then str.rpad$ = s$ + Space$(x% - Len(s$))
End Function

Sub show_menu()
  Const x% = X_OFFSET% - 100
  Local i%, item% = 0, update% = 1
  Local sounds$(3) = ("MUSIC & FX", "MUSIC ONLY", "FX ONLY   ", "NONE      ")
  Local sound_setting% = Choice(music_start_ptr% = Peek(VarAddr MUSIC%()), 1, 3) - soundfx_flag%

  For i% = 0 To MAX_CYCLE_IDX% : cycle.ctrl$(i%) = cycle.ctrl_backup$(i%) : Next

  ' Text X_OFFSET%, Y_OFFSET% + 75, "Music by Scott Joplin", "CM", 7, 1, RGB(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 90, "Game Version " + VERSION$, "CM", 7, 1, RGB(Cyan)

  Do

    If update% Then
      Text x%, Y_OFFSET% - 95, "START GAME", , 1, 1, RGB(White)
      text_for_controller(x%, Y_OFFSET% - 75, 0)
      text_for_controller(x%, Y_OFFSET% - 55, 1)
      text_for_controller(x%, Y_OFFSET% - 35, 2)
      text_for_controller(x%, Y_OFFSET% - 15, 3)
      Text x%, Y_OFFSET% + 5,  "DIFFICULTY: " + Str$(difficulty%), , 1, 1, RGB(White)
      Text x%, Y_OFFSET% + 25, "SOUND:      " + sounds$(sound_setting%), , 1, 1, RGB(White)
      Text x%, Y_OFFSET% + 45, "QUIT", , 1, 1, RGB(White)
      Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, RGB(Cyan)
      If IS_CMM2% Then Page Copy 1 To 0, B
      Pause 100
      cmd% = CMD_NONE%
      update% = 0
    EndIf

    Select Case cmd%
      Case UP%
        If item% > 0 Then
          Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, RGB(Black)
          Inc item%, -1
          update% = 1
        EndIf

      Case DOWN%
        If item% < 7 Then
          Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, RGB(Black)
          Inc item%
          update% = 1
        EndIf

      Case LEFT%, RIGHT%, FIRE%
        Select Case item%
          Case 0
            If cmd% = FIRE% Then Exit Do

          Case 1,2,3,4
            If cmd% = LEFT% Then dec_controller(item% - 1) Else inc_controller(item% - 1)
            update% = 1

          Case 5 ' Difficulty
            Inc difficulty%, Choice(cmd% = LEFT%, -1, 1)
            If difficulty% < 1 Then difficulty% = 5
            If difficulty% > 5 Then difficulty% = 1
            update% = 1

          Case 6 ' Sound
            Inc sound_setting%, Choice(cmd% = LEFT%, -1, 1)
            If sound_setting% < 0 Then sound_setting% = 3
            If sound_setting% > 3 Then sound_setting% = 0
            music_start_ptr% = Choice(sound_setting% And &b10, Peek(VarAddr NO_MUSIC%()), Peek(VarAddr MUSIC%()))
            music_ptr% = music_start_ptr%
            soundfx_flag% = Not (sound_setting% And &b01)
            update% = 1

          Case 7 ' Quit
            If cmd% = FIRE% Then End

        End Select

      Case Else
        cmd% = CMD_NONE%

    End Select

    If update% = 1 Then start_soundfx(Peek(VarAddr SOUNDFX_EAT%()), 1)

  Loop

  For i% = 0 To MAX_CYCLE_IDX% : cycle.ctrl_backup$(i%) = cycle.ctrl$(i%) : Next
End Sub

Sub text_for_controller(x%, y%, idx%)
  Local txt$ = "PLAYER " + Str$(idx% + 1) + ":   " + get_controller_name$(idx%)
  Text x%, y%, txt$, , 1, 1, cycle.colour%(idx%)
End Sub

Function get_controller_name$(idx%)
  Local i%
  For i% = 0 To Bound(CTRL_SUBS$(), 1)
    If cycle.ctrl$(idx%) = CTRL_SUBS$(i%) Then
      get_controller_name$ = CTRL_NAMES$(i%)
      Do While Len(get_controller_name$) < 12 : Cat get_controller_name$, " " : Loop
      Exit Function
    Endif
  Next
  Error "Unknown controller: " + cycle.ctrl$(idx%)
End Function

Sub dec_controller(idx%)
  Local i%
  For i% = 0 To Bound(CTRL_SUBS$(), 1)
    If cycle.ctrl$(idx%) <> CTRL_SUBS$(i%) Then Continue For
    Inc i%, -1
    If i% < 0 Then i% = Bound(CTRL_SUBS$(), 1)
    cycle.ctrl$(idx%) = CTRL_SUBS$(i%)
    Exit Sub
  Next
  Error "Unknown controller: " + cycle.ctrl$(idx%)
End Sub

Sub inc_controller(idx%)
  Local i%
  For i% = 0 To Bound(CTRL_SUBS$(), 1)
    If cycle.ctrl$(idx%) <> CTRL_SUBS$(i%) Then Continue For
    Inc i%, 1
    If i% > Bound(CTRL_SUBS$(), 1) Then i% = 0
    cycle.ctrl$(idx%) = CTRL_SUBS$(i%)
    Exit Sub
  Next
  Error "Unknown controller: " + cycle.ctrl$(idx%)
End Sub

Sub init_game()
  cmd% = CMD_NONE%
  frame_duration% = 3 * (5 + (6 - difficulty%))
  num_players% = 0 ' Incremented later.
  score% = 0
  Memory Set Peek(VarAddr keys%()), 0, 256

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

  cycle.dir%(0) = EAST%
  cycle.dir%(1) = SOUTH%
  cycle.dir%(2) = WEST%
  cycle.dir%(3) = NORTH%

  cycle.pos%(0) = WIDTH * (HEIGHT% \ 2) + 5
  cycle.pos%(1) = 5.5 * WIDTH%
  cycle.pos%(2) = WIDTH% * (HEIGHT% \ 2) + WIDTH% - 6
  cycle.pos%(3) = WIDTH% * (HEIGHT% - 6) + WIDTH% \ 2

  Local i%
  For i% = 0 To MAX_CYCLE_IDX%
    cycle.score%(i%) = 0
    If cycle.ctrl$(i%) = "ctrl_none%" Then
      cycle.pos%(i%) = -1
      cycle.nxt%(i%) = -1
      cycle.state%(i%) = STATE_DEAD%
    Else
      Inc num_players%
      cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(cycle.dir%(i%))
      Poke Byte p_arena% + cycle.pos%(i%), (cycle.dir%(i%) << 3) + (i% << 1) + 1
      Poke Byte p_arena% + cycle.nxt%(i%), (cycle.dir%(i%) << 3) + (i% << 1) + 1
      cycle.state%(i%) = STATE_OK%
    EndIf
  Next

End Sub

Sub draw_arena()
  Local c% = RGB(Grey), i%
  For i% = 0 To HEIGHT% * WIDTH% - 1
    If Peek(Var arena%(), i%) <> 128 Then Continue For
    Pixel 2 * (i% Mod WIDTH%), 2 * (i% \ WIDTH%), c%
  Next
End Sub

Sub game_loop()
  Local i%
  next_frame% = Timer + frame_duration%

  ' num_players% = 0
  ' cycle.score%(0) = 3175
  ' cycle.score%(1) = 2175
  ' cycle.score%(2) = 1175
  ' cycle.score%(3) = 975
  ' score% = 3175

  Do While num_players% > 0
    Inc score%, 1
    If score% Mod 5 = 0 Then draw_score()

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
      cycle.dir%(i%) = Call(cycle.ctrl$(i%), i%)
      cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(cycle.dir%(i%))
      If cycle.state%(i%) <> STATE_DEAD% Then cycle.check_collision(i%)
    Next

    ' Wait for next frame.
    Do While Timer < next_frame% : Loop
    Inc next_frame%, frame_duration%
  Loop

  ' Ensure display updated at end of loop.
  If IS_CMM2% Then Page Copy 1 To 0, B

  ' Wait for any sound effect to complete.
  Do While Peek(Byte soundfx_ptr%) <> &hFF : Loop
End Sub

Sub draw_score()
  Local i%, s$ = Str$(score%, 5, 0, "0")
  For i% = 0 To MAX_CYCLE_IDX%
    If cycle.state%(i%) < STATE_DYING% Then
      Text SCORE_X%(i%), SCORE_Y%, s$, , 1, 1, cycle.colour%(i%)
    EndIf
  Next
End Sub

Sub wipe()
  Local y%
  start_soundfx(Peek(VarAddr soundfx_wipe%()), 1)
  For y% = 0 To Mm.VRes \ 2 Step 5
     Box WIDTH% - y% * 1.2, Mm.VRes \ 2 - y%, 2.4 * y%, 2 * y%, 5, RGB(Cyan), RGB(Black)
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
  cycle.dir%(idx%) = ctrl_die%(idx%)
  cycle.nxt%(idx%) = cycle.pos%(idx%) + DIRECTIONS%(cycle.dir%(idx%))
  cycle.check_collision(idx%)
End Sub

Sub show_game_over()
  ' Sort scores and then round down to nearest 5.
  Local i%, idx%(MAX_CYCLE_IDX%), j%, k%, winner%
  Sort cycle.score%(), idx%(), 1
  For i% = 0 To MAX_CYCLE_IDX%
    Do While (cycle.score%(i%) Mod 5) <> 0 : Inc cycle.score%(i%), -1 : Loop
  Next
  winner% = idx%(0)

  Local txt$ = "PLAYER " + Str$(winner% + 1) + " WINS"
  Text X_OFFSET%, Y_OFFSET% - 25, txt$, "CM", 1, 2, cycle.colour%(winner%)
  Text X_OFFSET%, Y_OFFSET% + 5, "SCORE: " + Str$(cycle.score%(0)), "CM", 1, 2, cycle.colour%(winner%)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait(5000)

  wipe()

  On Key 0

  ' Insert into high-score table.
  For i% = 0 To MAX_CYCLE_IDX%
    For j% = 0 To Bound(high_scores$())
      If cycle.score%(i%) > Val(Field$(high_scores$(j%), 2)) Then
        For k% = Bound(high_scores$(), 1) To j% Step -1
          If k% <> 0 Then high_scores$(k%) = high_scores$(k% - 1)
        Next
        high_scores$(j%) = ", " + Str$(cycle.score%(i%))
        show_high_scores(1, j%, idx%(i%))
        Exit For
      EndIf
    Next
  Next

  On Key on_key()
End Sub

Function ctrl_ai%(idx%)
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

  ctrl_ai% = d%
End Function

Function ctrl_die%(idx%)
  ctrl_die% = ((Peek(Var arena%(), cycle.pos%(idx%)) >> 3) + 2) Mod 4
End Function

Function ctrl_keys1%(idx%)
  ctrl_keys1% = cycle.dir%(idx%)

  If Peek(Var keys%(), 97) Then       ' A
    ctrl_keys1% = NORTH%
    Poke Var keys%(), 97, 0
  ElseIf Peek(Var keys%(), 122) Then  ' Z
    ctrl_keys1% = SOUTH%
    Poke Var keys%(), 122, 0
  ElseIf Peek(Var keys%(), 44) Then   ' comma 
    ctrl_keys1% = WEST%
    Poke Var keys%(), 44, 0
  ElseIf Peek(Var keys%(), 46) Then   ' full-stop
    ctrl_keys1% = EAST%
    Poke Var keys%(), 46, 0
  EndIf
End Function

Function ctrl_keys2%(idx%)
  ctrl_keys2% = cycle.dir%(idx%)

  If Peek(Var keys%(), 97) Then       ' A
    ctrl_keys2% = NORTH%
    Poke Var keys%(), 97, 0
  ElseIf Peek(Var keys%(), 122) Then  ' Z
    ctrl_keys2% = SOUTH%
    Poke Var keys%(), 122, 0
  ElseIf Peek(Var keys%(), 120) Then  ' X
    ctrl_keys2% = WEST%
    Poke Var keys%(), 120, 0
  ElseIf Peek(Var keys%(), 99) Then   ' C
    ctrl_keys2% = EAST%
    Poke Var keys%(), 99, 0
  EndIf
End Function

Function ctrl_keys3%(idx%)
  ctrl_keys3% = cycle.dir%(idx%)

  If Peek(Var keys%(), 105) Then      ' I
    ctrl_keys3% = NORTH%
    Poke Var keys%(), 105, 0
  ElseIf Peek(Var keys%(), 107) Then  ' K
    ctrl_keys3% = SOUTH%
    Poke Var keys%(), 107, 0
  ElseIf Peek(Var keys%(), 111) Then  ' O
    ctrl_keys3% = WEST%
    Poke Var keys%(), 111, 0
  ElseIf Peek(Var keys%(), 112) Then  ' P
    ctrl_keys3% = EAST%
    Poke Var keys%(), 112, 0
  EndIf
End Function

Function ctrl_none%(idx%)
  ' Do nothing.
End Function

Sub cycle.check_collision(idx%)
  ' Handle dying.
  If cycle.state%(idx%) = STATE_DYING% Then
    Poke Var arena%(), cycle.pos%(idx%), 0
    Local mask% = (idx% << 1) + 1
    If (Peek(Var arena%(), cycle.nxt%(idx%)) And mask%) <> mask% Then
      cycle.ctrl$(idx%) = "ctrl_none%"
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
  Inc num_players%, -1
  cycle.ctrl$(idx%) = "ctrl_die%"
  cycle.nxt%(idx%) = cycle.pos%(idx%)
  cycle.score%(idx%) = score%
  start_soundfx(Peek(VarAddr soundfx_die%()), 0)
End Sub

Sub on_key()
  Local k$ = Inkey$
  Select Case Asc(k$)
    Case 65,  97, 128 : cmd% = UP%    ' A
    Case 90, 122, 129 : cmd% = DOWN%  ' Z
    Case 44,  60, 130 : cmd% = LEFT%  ' <
    Case 46,  62, 131 : cmd% = RIGHT% ' >
    Case 32           : cmd% = FIRE%  ' [SPACE]
  End Select
  Poke Var keys%(), Asc(LCase$(k$)), 1
End Sub

' Called from interrupt to play next note of music.
Sub play_music()
  Local n% = Peek(Byte music_ptr%)
  If n% = 255 Then music_ptr% = music_start_ptr% : Exit Sub
  Play Sound 1, B, S, FREQUENCY!(n%), 15
  Play Sound 2, B, S, FREQUENCY!(Peek(Byte music_ptr% + 1)), 15
  Play Sound 3, B, S, FREQUENCY!(Peek(Byte music_ptr% + 2)), 15
  Inc music_ptr%, 3
End Sub

' Start a new sound effect.
Sub start_soundfx(ptr%, wait_%)
  If Not soundfx_flag% Then Exit Sub

  ' Wait for current sound effect to end.
  If wait_% Then Do While Peek(Byte soundfx_ptr%) <> &hFF : Loop

  soundfx_ptr% = ptr%

  ' Wait for first note of new sound effect to play.
  Do While soundfx_ptr% = ptr% : Loop
End Sub

' Called from interrupt to play next note of current sound effect.
Sub play_soundfx()
  If soundfx_flag% Then
    Local note% = Peek(Byte soundfx_ptr%)
    If note% < &hFF Then
      Play Sound 4, B, s, 440 * 2 ^ ((note% - 2) / 12.0)
      Inc soundfx_ptr%
    Else
      Play Sound 4, B, O
    EndIf
  Else
    Play Sound 4, B, O
  EndIf
End Sub

music_data:

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
