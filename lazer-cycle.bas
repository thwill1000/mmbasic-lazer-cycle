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
Dim CTRL_NAMES$(7)      = ("KEYS: AZ,.",  "KEYS: AZXC",  "KEYS: IKOP",  "CONTROLLER A", "CONTROLLER B", "JOYSTICK B",     "AI",       "NONE" )
Dim CTRL_SUBS$(7)       = ("ctrl_keys1%", "ctrl_keys2%", "ctrl_keys3%", "ctrl_a%",      "ctrl_b%",      "ctrl_joystick%", "ctrl_ai%", "ctrl_none%")

Dim score%
Dim difficulty% = 1
Dim arena%(HEIGHT% * WIDTH% \ 8)
Dim cmd% = CMD_NONE%
Dim frame_duration%
Dim next_frame%

Dim cycle.score%(MAX_CYCLE_IDX%)
Dim cycle.nxt%(MAX_CYCLE_IDX%)
Dim cycle.pos%(MAX_CYCLE_IDX%)
Dim cycle.dir%(MAX_CYCLE_IDX%)
Dim cycle.colour%(MAX_CYCLE_IDX%) = ( RGB(Red), RGB(Yellow), RGB(Cyan), RGB(Green) )
Dim cycle.ctrl$(MAX_CYCLE_IDX%) = ("ctrl_keys2%", "ctrl_ai%", "ctrl_keys3%", "ctrl_ai%")

Dim music_flag% = 1
Dim music_ptr% = Peek(VarAddr MUSIC%()) + 4
Dim soundfx_flag% = 1
Dim soundfx_ptr% = Peek(VarAddr SOUNDFX_NOTHING%())

Dim num_players%
Dim keys%(31)

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
  Text X_OFFSET%, Y_OFFSET% - 15, "LAZER CYCLE", "CM", 1, 2, RGB(White)
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
  Const x% = X_OFFSET% - 100
  Local item% = 0, update% = 1
  Local sounds$(3) = ("MUSIC & FX", "MUSIC ONLY", "FX ONLY   ", "NONE      ")
  Local sound_setting% = Choice(music_flag%, Choice(soundfx_flag%, 0, 1), Choice(soundfx_flag%, 2, 3))

  Text X_OFFSET%, Y_OFFSET% + 75, "Music by Trevor Bailey", "CM", 7, 1, RGB(Cyan)
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
            music_flag% = (sound_setting% = 0 Or sound_setting% = 1) 
            soundfx_flag% = (sound_setting% = 0 Or sound_setting% = 2) 
            update% = 1

          Case 7 ' Quit
            If cmd% = FIRE% Then End

        End Select

      Case Else
        cmd% = CMD_NONE%

    End Select

    If update% = 1 Then start_soundfx(Peek(VarAddr SOUNDFX_EAT%()))

  Loop
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
  Memory Set p_arena%, 255, WIDTH%
  Memory Set p_arena% + (HEIGHT% - 1) * WIDTH%, 255, WIDTH%
  Local y%
  For y% = 1 To HEIGHT% - 2
    Poke Byte p_arena% + y% * WIDTH%, 255
    Poke Byte p_arena% + (y% + 1) * WIDTH% - 1, 255
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
    Else
      Inc num_players%
      cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(cycle.dir%(i%))
      Poke Var arena%(), cycle.pos%(i%), i% + 1
      Poke Var arena%(), cycle.nxt%(i%), i% + 1
    EndIf
  Next

End Sub

Sub draw_arena()
  Local c% = RGB(Grey), i%
  For i% = 0 To HEIGHT% * WIDTH% - 1
    If Peek(Var arena%(), i%) <> 255 Then Continue For
    Pixel 2 * (i% Mod WIDTH%), 2 * (i% \ WIDTH%), c%
  Next
End Sub

Sub game_loop()
  Local i%
  next_frame% = Timer + frame_duration%
  Do
    For i% = 0 To MAX_CYCLE_IDX% : cycle.draw(i%) : Next
    If IS_CMM2% Then Page Copy 1 To 0, I
    For i% = 0 To MAX_CYCLE_IDX% : cycle.move(i%) : Next
    For i% = 0 To MAX_CYCLE_IDX%
      If cycle.pos%(i%) < 0 Then Continue For
      cycle.dir%(i%) = Call(cycle.ctrl$(i%), i%)
      cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(cycle.dir%(i%))
      cycle.check_collision(i%)
    Next

    If num_players% = 0 Then Exit Do

    update_score()

    ' Wait for next "frame".
    Do While Timer < next_frame% : Loop
    Inc next_frame%, frame_duration%
  Loop

  ' Ensure display updated at end of loop.
  If IS_CMM2% Then Page Copy 1 To 0, B

  ' Wait for any sound effect to complete.
  Do While Peek(Byte soundfx_ptr%) <> &hFF : Loop
End Sub

Sub update_score()
  Inc score%, 1
  If score% Mod 5 = 0 Then
    Local i%
    For i% = 0 To 3
      If cycle.pos%(i%) >= 0 Then
        Text SCORE_POS%(i%), 2 * HEIGHT% + 4, Str$(score%, 5, 0, "0"), , 1, 1, cycle.colour%(i%)
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
  Line 2 * x%, 2 * y%, 2 * x2%, 2 * y2%, 1, cycle.colour%(idx%)
End Sub

Sub show_game_over()
  Local winner%
  For winner% = 0 To MAX_CYCLE_IDX%
    If cycle.score%(winner%) = score% Then Exit For
  Next
  Local txt$ = "PLAYER " + Str$(winner% + 1) + " WINS"
  Text X_OFFSET%, Y_OFFSET% - 25, txt$, "CM", 1, 2, cycle.colour%(winner%)
  Do While (score% Mod 5) <> 0 : Inc score%, -1 : Loop
  Text X_OFFSET%, Y_OFFSET% + 5, "SCORE: " + Str$(score%), "CM", 1, 2, cycle.colour%(winner%)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait(5000)
End Sub

Sub cycle.move(idx%)
  If cycle.pos%(idx%) >= 0 Then cycle.pos%(idx%) = cycle.nxt%(idx%)
End Sub

Function ctrl_ai%(idx%)
  Local d% = cycle.dir%(idx%)

  ' Random element.
  Local i% = Int(500 * Rnd)
  If i% < 4 Then d% = NEXT_DIR%(i% + idx%)

  ' Avoid collisions.
  Local nxt%
  For i% = 0 To 3
    nxt% = cycle.pos%(idx%) + DIRECTIONS%(d%)
    If Not Peek(Var arena%(), nxt%)) Then Exit For
    d% = NEXT_DIR%(i% + idx%)
  Next

  ctrl_ai% = d%
End Function

Function ctrl_keys1%(idx%)
  Local d% = cycle.dir%(idx%)

  If Peek(Var keys%(), 97) Then       ' A
    d% = NORTH%
    Poke Var keys%(), 97, 0
  ElseIf Peek(Var keys%(), 122) Then  ' Z
    d% = SOUTH%
    Poke Var keys%(), 122, 0
  ElseIf Peek(Var keys%(), 44) Then   ' comma 
    d% = WEST%
    Poke Var keys%(), 44, 0
  ElseIf Peek(Var keys%(), 46) Then   ' full-stop
    d% = EAST%
    Poke Var keys%(), 46, 0
  EndIf

  ctrl_keys1% = d%
End Function

Function ctrl_keys2%(idx%)
  Local d% = cycle.dir%(idx%)

  If Peek(Var keys%(), 97) Then       ' A
    d% = NORTH%
    Poke Var keys%(), 97, 0
  ElseIf Peek(Var keys%(), 122) Then  ' Z
    d% = SOUTH%
    Poke Var keys%(), 122, 0
  ElseIf Peek(Var keys%(), 120) Then  ' X
    d% = WEST%
    Poke Var keys%(), 120, 0
  ElseIf Peek(Var keys%(), 99) Then   ' C
    d% = EAST%
    Poke Var keys%(), 99, 0
  EndIf

  ctrl_keys2% = d%
End Function

Function ctrl_keys3%(idx%)
  Local d% = cycle.dir%(idx%)

  If Peek(Var keys%(), 105) Then      ' I
    d% = NORTH%
    Poke Var keys%(), 105, 0
  ElseIf Peek(Var keys%(), 107) Then  ' K
    d% = SOUTH%
    Poke Var keys%(), 107, 0
  ElseIf Peek(Var keys%(), 111) Then  ' O
    d% = WEST%
    Poke Var keys%(), 111, 0
  ElseIf Peek(Var keys%(), 112) Then  ' P
    d% = EAST%
    Poke Var keys%(), 112, 0
  EndIf

  ctrl_keys3% = d%
End Function

Function ctrl_none%(idx%)
  ' Do nothing.
End Function

Sub cycle.check_collision(idx%)
  If cycle.pos%(idx%) < 0 Then Exit Sub

  ' No collision occurred.
  If Not Peek(Var arena%(), cycle.nxt%(idx%)) Then
    Poke Var arena%(), cycle.nxt%(idx%), idx% + 1
    Exit Sub
  EndIf

  ' Collision occurred.
  Local i%
  For i% = 0 To WIDTH% * HEIGHT% - 1
    If Peek(Var arena%(), i%) = idx% + 1 Then
      Box 2 * (i% Mod WIDTH%), 2 * (i% \ WIDTH%), 2, 2, 1, Rgb(Black)
      Poke Var arena%(), i%, 0
    EndIf
  Next

  cycle.pos%(idx%) = -1
  cycle.score%(idx%) = score%
  Inc num_players%, -1
  start_soundfx(Peek(VarAddr soundfx_die%()))
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
