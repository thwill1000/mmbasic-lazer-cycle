' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.03
' Music by Trevor Bailey

Option Base 0
Option Default None
Option Explicit On

' Save "light-bikes.bas"

Const VERSION$ = "1.0.0"

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

' These would be constants but MMBasic does not support constant arrays
Dim SOUNDFX_NOTHING%(1) = (&hFFFFFFFFFFFFFFFF, &hFFFFFFFFFFFFFFFF)
Dim SOUNDFX_EAT%(1)     = (&hFFFFFFFFFF100C04, &hFFFFFFFFFFFFFFFF)
Dim SOUNDFX_DIE%(2)     = (&h0F10111213141516, &h0708090A0B0C0D0E, &hFF00010203040506)
Dim SOUNDFX_WIPE%(2)    = (&h0706050403020100, &h0F0E0D0C0B0A0908, &hFF16151413121110)
Dim MUSIC%(68)
Dim NEXT_DIR%(7)        = (EAST%, NORTH%, WEST%, SOUTH%, EAST%, NORTH%, WEST%, SOUTH%)
Dim SCORE_POS%(3)       = (35, 105, 175, 245)
Dim DIRECTIONS%(3)      = (-WIDTH%, 1, WIDTH%, -1)

Dim score%
Dim difficulty% = 1
Dim game_type% = 2
Dim collision%(HEIGHT% * WIDTH% \ 8)
Dim cmd% = CMD_NONE%
Dim frame_duration%
Dim next_frame%

Dim cycle.score%(MAX_CYCLE_IDX%)
Dim cycle.nxt%(MAX_CYCLE_IDX%)
Dim cycle.pos%(MAX_CYCLE_IDX%)
Dim cycle.dir%(MAX_CYCLE_IDX%)
Dim cycle.col%(MAX_CYCLE_IDX%) = ( RGB(Red), RGB(Yellow), RGB(Cyan), RGB(Green) )

Dim music_flag% = 1
Dim music_ptr% = Peek(VarAddr MUSIC%()) + 4
Dim soundfx_flag% = 1
Dim soundfx_ptr% = Peek(VarAddr SOUNDFX_NOTHING%())

Dim num_players%

' Music and sound effects are played on SetTick interrupts.
SetTick 250, play_music, 1
SetTick 40, play_soundfx, 2

On Key on_key()

read_music()
clear_display()

Do
  wipe()
  show_title()
  wipe()
  show_menu()
  wipe()
  init_game()
  draw_arena()
  game_loop()

  ' Ensure display updated at end of game.
  If IS_CMM2% Then Page Copy 1 To 0, B

  ' Wait for any sound effect to complete.
  Do While Peek(Byte soundfx_ptr%) <> &hFF : Loop

  show_game_over()
Loop

End

Sub read_music()
  Local i%
  Restore music_data
  For i% = 0 To Bound(MUSIC%(), 1)
    Read MUSIC%(i%)
  Next
End Sub

Sub show_title()
  Text X_OFFSET%, Y_OFFSET% - 15, "LIGHT CYCLE", "CM", 1, 2, RGB(White)
  Text X_OFFSET%, Y_OFFSET% + 8, "(c) 2022 Thomas Hugo Williams", "CM", 7, 1, RGB(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 20, "www.sockpuppetstudios.com", "CM", 7, 1, RGB(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 40, "PRESS " + Choice(IS_CMM2%, "SPACE", "SELECT"), "CM", 1, 1, RGB(White)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait()
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

Sub show_menu()
  Const x% = X_OFFSET% - 45
  Local item% = 0, update% = 1

  Text X_OFFSET%, Y_OFFSET% + 70, "Music by Trevor Bailey", "CM", 7, 1, RGB(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 85, "Game Version " + VERSION$, "CM", 7, 1, RGB(Cyan)

  Do

    If update% Then
      Text x%, Y_OFFSET% - 65, "START GAME", , 1, 1, RGB(White)
      Text x%, Y_OFFSET% - 45, "DIFFICULTY: " + Str$(difficulty%), , 1, 1, RGB(White)
      Text x%, Y_OFFSET% - 25, "GAME TYPE:  " + Str$(game_type%), , 1, 1, RGB(White)
      Text x%, Y_OFFSET%  - 5, "MUSIC:    " + Choice(music_flag%, " ON", "OFF"), , 1, 1, RGB(White)
      Text x%, Y_OFFSET% + 15, "SOUND FX: " + Choice(soundfx_flag%, " ON", "OFF"), , 1, 1, RGB(White)
      Text x%, Y_OFFSET% + 35, "QUIT", , 1, 1, RGB(White)
      Text x% - 10, Y_OFFSET% - 65 + item% * 20, Chr$(137), , 1, 1, RGB(Cyan)
      If IS_CMM2% Then Page Copy 1 To 0, B
      Pause 100
      cmd% = CMD_NONE%
      update% = 0
    EndIf

    Select Case cmd%
      Case UP%
        If item% > 0 Then
          Text x% - 10, Y_OFFSET% - 65 + item% * 20, Chr$(137), , 1, 1, RGB(Black)
          Inc item%, -1
          update% = 1
        EndIf

      Case DOWN%
        If item% < 5 Then
          Text x% - 10, Y_OFFSET% - 65 + item% * 20, Chr$(137), , 1, 1, RGB(Black)
          Inc item%
          update% = 1
        EndIf

      Case LEFT%, RIGHT%, FIRE%
        Select Case item%
          Case 0
            If cmd% = FIRE% Then Exit

          Case 1 ' Difficulty
            Inc difficulty%, Choice(cmd% = LEFT%, -1, 1)
            difficulty% = Min(5, Max(difficulty%, 1))
            update% = 1

          Case 2 ' Game type
            Inc game_type%, Choice(cmd% = LEFT%, -1, 1)
            game_type% = Min(2, Max(game_type%, 1))
            update% = 1

          Case 3 ' Music
            music_flag% = Not music_flag%
            update% = 1

          Case 4 ' Sound FX
            soundfx_flag% = Not soundfx_flag%
            update% = 1

          Case 5 ' Quit
            If cmd% = FIRE% Then End

        End Select

      Case Else
        cmd% = CMD_NONE%

    End Select

    If update% = 1 Then start_soundfx(Peek(VarAddr SOUNDFX_EAT%()))

  Loop
End Sub

Sub init_game()
  cmd% = CMD_NONE%
  frame_duration% = 3 * (5 + (6 - difficulty%))
  num_players% = MAX_CYCLE_IDX% + 1
  score% = 0

  ' Initialise the arena.
  Local i%, x%, y%
  For i% = 0 To HEIGHT% * WIDTH% - 1
    x% = i% Mod WIDTH%
    y% = i% \ WIDTH%
    If x% = 0 Or x% = WIDTH% - 1 Or y% = 0 Or y% = HEIGHT% - 1 Then
      Poke Var collision%(), i%, 255
    Else
      Poke Var collision%(), i%, 0
    EndIf
  Next

  cycle.dir%(0) = EAST%
  cycle.dir%(1) = SOUTH%
  cycle.dir%(2) = WEST%
  cycle.dir%(3) = NORTH%

  cycle.pos%(0) = WIDTH * (HEIGHT% \ 2) + 1
  cycle.pos%(1) = 1.5 * WIDTH%
  cycle.pos%(2) = WIDTH% * (HEIGHT% \ 2) + WIDTH% - 2
  cycle.pos%(3) = WIDTH% * (HEIGHT% - 2) + WIDTH% \ 2

  For i% = 0 To MAX_CYCLE_IDX%
    cycle.score%(i%) = 0
    cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(cycle.dir%(i%))
    Poke Var collision%(), cycle.pos%(i%), i% + 1
    Poke Var collision%(), cycle.nxt%(i%), i% + 1
  Next

End Sub

Sub draw_arena()
  Local i%
  For i% = 0 To HEIGHT% * WIDTH% - 1
    If Peek(Var collision%(), i%) = 255 Then
      Pixel 2 * (i% Mod WIDTH%), 2 * (i% \ WIDTH%), RGB(Grey)
    EndIf
  Next
End Sub

Sub game_loop()
  Local i%
  next_frame% = Timer + frame_duration%
  Do
    For i% = 0 To MAX_CYCLE_IDX% : cycle.draw(i%) : Next
    If IS_CMM2% Then Page Copy 1 To 0, I
    For i% = 0 To MAX_CYCLE_IDX% : cycle.move(i%) : Next
    For i% = 0 To MAX_CYCLE_IDX% : cycle.steer(i%) : Next
    If num_players% = 0 Then Exit Do
    update_score()

    ' Wait for next "frame".
    Do While Timer < next_frame% : Loop
    Inc next_frame%, frame_duration%
  Loop
End Sub

Sub update_score()
  Inc score%, 1
  If score% Mod 5 = 0 Then
    Local i%
    For i% = 0 To 3
      If cycle.pos%(i%) >= 0 Then
        Text SCORE_POS%(i%), 2 * HEIGHT% + 4, Str$(score%, 5, 0, "0"), , 1, 1, cycle.col%(i%)
      EndIf
    Next
 EndIf
End Sub

Sub wipe()
  Local y%
  start_soundfx(Peek(VarAddr soundfx_wipe%()))
  For y% = 0 To Mm.VRes \ 2 Step 5
     Box WIDTH% - y% * 1.2, Mm.VRes \ 2 - y%, 2.4 * y%, 2 * y%, 5, RGB(Cyan), RGB(Black)
     If IS_CMM2% Then Page Copy 1 To 0, B
     Pause 30
  Next
  clear_display()
End Sub

Sub cycle.draw(idx%)
  If cycle.pos%(idx%) < 0 Then Exit Sub
  Local x% = cycle.pos%(idx%) Mod WIDTH%
  Local y% = cycle.pos%(idx%) \ WIDTH%
  Local x2% = cycle.nxt%(idx%) Mod WIDTH%
  Local y2% = cycle.nxt%(idx%) \ WIDTH%
  Line 2 * x%, 2 * y%, 2 * x2%, 2 * y2%, 1, cycle.col%(idx%)
End Sub

Sub show_game_over()
  Local winner%
  For winner% = 0 To MAX_CYCLE_IDX%
    If cycle.score%(winner%) = score% Then Exit For
  Next
  Local txt$ = "PLAYER " + Str$(winner% + 1) + " WINS"
  Text X_OFFSET%, Y_OFFSET% - 25, txt$, "CM", 1, 2, cycle.col%(winner%)
  Do While (score% Mod 5) <> 0 : Inc score%, -1 : Loop
  Text X_OFFSET%, Y_OFFSET% + 5, "SCORE: " + Str$(score%), "CM", 1, 2, cycle.col%(winner%)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait(5000)
End Sub

Sub cycle.move(idx%)
  If cycle.pos%(idx%) < 0 Then Exit Sub
  cycle.pos%(idx%) = cycle.nxt%(idx%)
  If cycle.pos%(idx%) < 0 Or cycle.pos%(idx%) >= WIDTH% * HEIGHT% Then Error "cycle.move: " + Str$(idx%) + " : " + Str$(cycle.pos%(idx%))
End Sub

Sub cycle.steer(idx%)
  If cycle.pos%(idx%) < 0 Then Exit Sub

  Local i%

  If idx% = 0 Then
    cycle.steer_player(idx%)
    cycle.nxt%(idx%) = cycle.pos%(idx%) + DIRECTIONS%(cycle.dir%(idx%))
  Else

    ' Random element.
    i% = Int(500 * Rnd)
    If i% < 4 Then cycle.dir%(idx%) = NEXT_DIR%(i% + idx%)

    ' Avoid collisions.
    Local nxt% = cycle.pos%(idx%) + DIRECTIONS%(cycle.dir%(idx%))
    For i% = 0 To 3
      If Not Peek(Var collision%(), nxt%)) Then Exit For
      cycle.dir%(idx%) = NEXT_DIR%(i% + idx%)
      nxt% = cycle.pos%(idx%) + DIRECTIONS%(cycle.dir%(idx%))
    Next

    cycle.nxt%(idx%) = nxt%
  EndIf

  ' No collision occurred.
  If Not Peek(Var collision%(), cycle.nxt%(idx%)) Then
    Poke Var collision%(), cycle.nxt%(idx%), idx% + 1
    Exit Sub
  EndIf

  ' Collision occurred.
  For i% = 0 To WIDTH% * HEIGHT% - 1
    If Peek(Var collision%(), i%) = idx% + 1 Then
      Box 2 * (i% Mod WIDTH%), 2 * (i% \ WIDTH%), 2, 2, 1, Rgb(Black)
      Poke Var collision%(), i%, 0
    EndIf
  Next
  cycle.pos%(idx%) = -1
  cycle.score%(idx%) = score%
  Inc num_players%, -1
  start_soundfx(Peek(VarAddr soundfx_die%()))
Exit Sub

Sub cycle.steer_player(idx%)
  Local d% = cycle.dir%(idx%)
  If game_type% = 1 Then
    Select Case cmd%
      Case LEFT%
        Inc d%, -1
        If d% < 0 Then d% = 3
      Case RIGHT%
        Inc d%, 1
        If d% > 3 Then d% = 0
    End Select
  Else
    Select Case cmd%
      Case UP%    : d% = NORTH%
      Case DOWN%  : d% = SOUTH%
      Case LEFT%  : d% = WEST%
      Case RIGHT% : d% = EAST%
    End Select
  EndIf
  cycle.dir%(idx%) = d%
  cmd% = CMD_NONE%
End Sub

Sub on_key()
  Select Case LCase$(Inkey$)
    Case "a" : cmd% = UP%
    Case "z" : cmd% = DOWN%
    Case "," : cmd% = LEFT%
    Case "." : cmd% = RIGHT%
    Case " " : cmd% = FIRE%
  End Select
End Sub

' Called from interrupt to play next note of music.
Sub play_music()
  If music_flag% Then
    Local note% = Peek(Byte music_ptr%)
    If note% = &hFF Then
      music_ptr% = Peek(VarAddr MUSIC%()) + 4
      note% = Peek(Byte music_ptr%)
    EndIf
    Play Sound 1, B, s, 440 * 2 ^ ((note% - 2) / 12.0), 15
    Inc music_ptr%
  Else
    Play Sound 1, B, O
  EndIf
End Sub

' Start a new sound effect.
Sub start_soundfx(ptr%)
  If Not soundfx_flag% Then Exit Sub

  ' Wait for current sound effect to end.
  Do While Peek(Byte soundfx_ptr%) <> &hFF : Loop

  soundfx_ptr% = ptr%

  ' Wait for first note of new sound effect to play.
  Do While soundfx_ptr% = ptr% : Loop
End Sub

' Called from interrupt to play next note of current sound effect.
Sub play_soundfx()
  If soundfx_flag% Then
    Local note% = Peek(Byte soundfx_ptr%)
    If note% < &hFF Then
      Play Sound 2, B, s, 440 * 2 ^ ((note% - 2) / 12.0)
      Inc soundfx_ptr%
    Else
      Play Sound 2, B, O
    EndIf
  Else
    Play Sound 2, B, O
  EndIf
End Sub

music_data:

Data &h0900090500000220, &h0900090509000905, &h0900090509000905, &h0900090509000905
Data &h1818181509000905, &h181818151818181A, &h151618161818181A, &h1516181613111516
Data &h1818181513111516, &h181818151518151A, &h151618131818181A, &h0C0E0C1315131516
Data &h181818150704000C, &h181818151818181A, &h151618161818181A, &h1516181613111516
Data &h1818181513111516, &h181818151518151A, &h151618131818181A, &h091D051115131516
Data &h1515150E0509051D, &h1515150E15151516, &h1013151315151516, &h1013151310131513
Data &h1515150E00040013, &h1515150E15151516, &h1311101315151516, &h1615131615131115
Data &h1818181500000C18, &h181818151818181A, &h151618161818181A, &h1516181613111516
Data &h1818181513111516, &h181818151518151A, &h151618131818181A, &h0C0E0C1315131516
Data &h181818150704000C, &h181818151818181A, &h151618161818181A, &h1516181613111516
Data &h1818181513111516, &h181818151518151A, &h151618131818181A, &h091D051115131516
Data &h1515150E0509051D, &h1515150E15151516, &h1013151315151516, &h1013151310131513
Data &h1515150E00040013, &h1515150E15151516, &h1311101315151516, &h1615131615131115
Data &h1818181500000C18, &h181818151818181A, &h151618161818181A, &h1516181613111516
Data &h1818181513111516, &h181818151518151A, &h151618131818181A, &h0C0E0C1315131516
Data &h181818150704000C, &h181818151818181A, &h151618161818181A, &h1516181613111516
Data &h1818181513111516, &h181818151518151A, &h151618131818181A, &h091D051115131516
Data &hFFFFFFFF0509051D
