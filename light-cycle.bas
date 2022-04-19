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

Const X_OFFSET% = MM.HRes \ 2
Const Y_OFFSET% = MM.VRes \ 2
Const MAX_RADIUS% = Y_OFFSET% - 5
Const MAX_SNAKE_SIZE% = 1000
Const OUTWARD% = 0, CLOCKWISE% = 1, INWARD% = 2, ANTICLOCKWISE% = 3
Const UP% = 5, DOWN% = 6, LEFT% = 7, RIGHT% = 8, FIRE% = 9

' These would be constants but MMBasic does not support constant arrays
Dim SOUNDFX_NOTHING%(1) = (&hFFFFFFFFFFFFFFFF, &hFFFFFFFFFFFFFFFF)
Dim SOUNDFX_EAT%(1)     = (&hFFFFFFFFFF100C04, &hFFFFFFFFFFFFFFFF)
Dim SOUNDFX_DIE%(2)     = (&h0F10111213141516, &h0708090A0B0C0D0E, &hFF00010203040506)
Dim SOUNDFX_WIPE%(2)    = (&h0706050403020100, &h0F0E0D0C0B0A0908, &hFF16151413121110)
Dim MUSIC%(68)

Dim score%
Dim lives% = 3
Dim difficulty% = 1
Dim game_type% = 1
Dim radius% = MAX_RADIUS% \ 2
Dim theta%
Dim snake%(MAX_SNAKE_SIZE%, 1)
Dim head%
Dim tail%
Dim collision%(360 * MAX_RADIUS% / 64)
Dim direction%
Dim cmd%
Dim apple_x%, apple_y%
Dim snake_x%, snake_y%
Dim frame_duration%
Dim next_frame%

Dim music_flag% = 1
Dim music_ptr% = Peek(VarAddr MUSIC%()) + 4
Dim soundfx_flag% = 1
Dim soundfx_ptr% = Peek(VarAddr SOUNDFX_NOTHING%())

' Music and sound effects are played on SetTick interrupts.
SetTick 250, play_music, 1
SetTick 40, play_soundfx, 2

On Key on_key()

Cls
read_music()
draw_border()

Do
  show_title()
  show_menu()
  init_game()
  Do While lives% > 0
    game_loop()
  Loop
  show_game_over()
  show_score()
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
  clear_display()
  Text X_OFFSET%, Y_OFFSET% - 15, "LIGHT CYCLE", "CM", 1, 2, RGB(White)
  Text X_OFFSET%, Y_OFFSET% + 8, "(c) 2022 Thomas Hugo Williams", "CM", 7, 1, RGB(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 20, "www.sockpuppetstudios.com", "CM", 7, 1, RGB(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 40, "PRESS " + Choice(IS_CMM2%, "SPACE", "SELECT"), "CM", 1, 1, RGB(White)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait()
End Sub

Sub clear_display()
  Circle X_OFFSET%, Y_OFFSET%, MAX_RADIUS% + 3, 1, 1, RGB(Black), RGB(Black)
  draw_border()
End Sub

Sub draw_border()
  Circle X_OFFSET%, Y_OFFSET%, MAX_RADIUS% + 5, 2, 1, RGB(Grey)
End Sub

Sub wait(duration%)
  cmd% = 0
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

  clear_display()

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
      cmd% = 0
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
        cmd% = 0

    End Select

    If update% = 1 Then start_soundfx(Peek(VarAddr SOUNDFX_EAT%()))

  Loop
End Sub

Sub init_game()
  lives% = 3
  score% = 0
  head% = 20
  tail% = 0
  frame_duration% = 5 + (6 - difficulty%)
End Sub

Sub game_loop()
  clear_display()
  Text X_OFFSET%, Y_OFFSET%, "LIVES: " + Str$(lives%), "CM", 1, 2, RGB(White)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait(2000)
  wipe()

  init_round()

  Do
    eat_apple()
    If apple_x% = -999 Then new_apple()
    draw_snake()
    If IS_CMM2% Then Page Copy 1 To 0, I
    If Not move_snake%() Then Exit
    check_direction()

    ' Wait for next "frame".
    Do While Timer < next_frame% : Loop
    Inc next_frame%, frame_duration%
  Loop

  die()
End Sub

Sub wipe()
  Local r%
  start_soundfx(Peek(VarAddr soundfx_wipe%()))
  For r% = 10 To MAX_RADIUS% Step 5
    Circle X_OFFSET%, Y_OFFSET%, r% - 5, 5, 1, RGB(Black)
    Circle X_OFFSET%, Y_OFFSET%, r%, 5, 1, RGB(Cyan)
    If IS_CMM2% Then Page Copy 1 To 0, B
    Pause 30
  Next
  clear_display()
  If IS_CMM2% Then Page Copy 1 To 0, B
End Sub

Sub init_round()
  Local i%
  radius% = MAX_RADIUS% \ 2
  theta% = 0
  For i% = 0 To Bound(snake%(), 1)
    snake%(i%, 0) = 0
    snake%(i%, 1) = 0
  Next i%
  snake%(head%, 0) = radius%
  snake%(head%, 1) = theta%
  to_cartesian(radius%, theta%, snake_x%, snake_y%)
  For i% = 0 To Bound(collision%(), 1)
    collision%(i%) = 0
  Next
  direction% = CLOCKWISE%
  cmd% = 0
  apple_x% = -999
  apple_y% = -999
  next_frame% = Timer + frame_duration%
End Sub

Sub to_cartesian(r%, t%, x%, y%)
  Local yy! = Sin(Rad(90 - t%)) * r%
  y% = Cint(yy!)
  x% = Choice(t% <= 180, 1, -1) * Cint(Sqr(Abs(r%^2 - yy!^2)))
End Sub

Sub eat_apple()
  If Abs(snake_x% - apple_x%) < 6 And Abs(snake_y% - apple_y%) < 6 Then
    start_soundfx(Peek(VarAddr SOUNDFX_EAT%()))
    draw_apple(0)
    apple_x% = -999
    Inc score%
    Local i%
    For i% = 1 To 10
      head% = (head% + 1) Mod MAX_SNAKE_SIZE%
      snake%(head%, 0) = radius%
      snake%(head%, 1) = theta%
    Next
  EndIf
End Sub

Sub draw_snake()
  Local x%, y%

  ' Erase tail.
  to_cartesian(snake%(tail%, 0), snake%(tail%, 1), x%, y%)
  Circle x% + X_OFFSET%, y% + Y_OFFSET%, 2, 1, 1, RGB(Black), RGB(Black)

  ' Redraw apple incase we have erased part of it.
  draw_apple(1)

  ' Draw head.
  Circle snake_x% + X_OFFSET%, snake_y% + Y_OFFSET%, 2, 1, 1, RGB(Cyan), RGB(Cyan)

'  Print radius%, theta%, snake_x%, snake_y%
End Sub

Sub die()
  start_soundfx(Peek(VarAddr soundfx_die%()))
  Local i% = head%, x%, y%
  Local sz% = Choice(head% > tail%, head% - tail%, head% + MAX_SNAKE_SIZE% - tail%)
  Local count%
  Do While i% <> tail%
    to_cartesian(snake%(i%, 0), snake%(i%, 1), x%, y%)
    Circle x% + X_OFFSET%, y% + Y_OFFSET%, 2, 1, 1, RGB(Black), RGB(Black)
    Inc i%, -1
    If i% < 0 Then i% = Bound(snake%(), 1)
    Inc count%
    If count% >= sz% \ 100 Then
      draw_border()
      If IS_CMM2% Then Page Copy 1 To 0, B
      count% = 0
    EndIf
    Pause 10
  Loop

  ' Ensure last element of tail is deleted.
  to_cartesian(snake%(tail%, 0), snake%(tail%, 1), x%, y%)
  Circle x% + X_OFFSET%, y% + Y_OFFSET%, 2, 1, 1, RGB(Black), RGB(Black)
  If IS_CMM2% Then Page Copy 1 To 0, B

  ' Wait for the dying sound effect to complete.
  Do While Peek(Byte soundfx_ptr%) <> &hFF : Loop

  Inc lives%, -1
End Sub

Sub show_game_over()
  clear_display()
  Text X_OFFSET%, Y_OFFSET%, "GAME OVER", "CM", 1, 2, RGB(White)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait(5000)
End Sub

Sub show_score()
  clear_display()
  Text X_OFFSET%, Y_OFFSET%, "SCORE: " + Str$(score%), "CM", 1, 2, RGB(White)
  If IS_CMM2% Then Page Copy 1 To 0, B
  wait(5000)
End Sub

Function move_snake%()
  ' Clear the tail from the collision map.
  Local ignored% = set%(snake%(tail%, 0), snake%(tail%, 1), 0)

  head% = (head% + 1) Mod MAX_SNAKE_SIZE%
  tail% = (tail% + 1) Mod MAX_SNAKE_SIZE%

  Select Case direction%
    Case OUTWARD%
      radius% = radius% + 1
      If radius% > MAX_RADIUS% Then Exit Function

    Case INWARD%
      Inc radius%, - 1
      If radius% < 0 Then
        radius% = -radius%
        theta% = (theta% + 180) Mod 360
        direction% = OUTWARD%
      EndIf

    Case CLOCKWISE%
      Inc theta%, -1 ' Correct this
      If theta% = -1 Then theta% = 359

    Case ANTICLOCKWISE%
      Inc theta%
      If theta% = 360 Then theta% = 0
  End Select

  snake%(head%, 0) = radius%
  snake%(head%, 1) = theta%
  to_cartesian(radius%, theta%, snake_x%, snake_y%)

  ' Set the head, if it is already set then return 0.
  move_snake% = Not set%(radius%, theta%, 1)
End Function

Function set%(r%, t%, z%)
  Local t_% = Choice(r% = 0, 0, t%)

  Local bit% = r% * 360 + t_%
  Local byte% = bit% \ 8
  bit% = bit% Mod 8
  Local v% = Peek(Var collision%(), byte%)
  If v% And (1 << bit%) Then set% = 1
  If z% Then
    v% = v% Or (1 << bit%)
  Else
    v% = v% And INV (1 << bit%)
  EndIf
  Poke Var collision%(), byte%, v%
End Function

Sub check_direction()
  If radius% <= 20 Then Exit Sub
  If game_type% = 1 Then
    Select Case cmd%
      Case LEFT%
        Inc direction%, -1
        If direction% < 0 Then direction% = 3
      Case RIGHT%
        Inc direction%, 1
        If direction% > 3 Then direction% = 0
    End Select
  Else
    Select Case cmd%
      Case UP%
        direction% = OUTWARD%
      Case DOWN%
        direction% = INWARD%
      Case LEFT%
        direction% = ANTICLOCKWISE%
      Case RIGHT%
        direction% = CLOCKWISE%
    End Select
  EndIf
  cmd% = 0
End Sub

Sub new_apple()
  If Rnd() < 0.8 Then Exit Sub
  Local r% = Rnd() * MAX_RADIUS%
  If r% < 40 Or Abs(radius% - r%) < 10 Then Exit Sub
  Local t% = Rnd() * 360
  If Abs(theta% - t%) < 10 Then Exit Sub
  to_cartesian(r%, t%, apple_x%, apple_y%)
  draw_apple(1)
End Sub

Sub draw_apple(show%)
  If apple_x% = -999 Then Exit Sub
  Local x% = apple_x% + X_OFFSET%
  Local y% = apple_y% + Y_OFFSET%
  Local stalk% = Choice(show%, RGB(Brown), RGB(Black))
  Local apple% = Choice(show%, RGB(Green), RGB(Black))
  Line x% - 4, y% - 5, x%, y%, 1, stalk%
  Line x% - 3, y% - 5, x% + 1, y%, 1, stalk%
  Circle x%, y%, 4, 1, 1, apple%, apple%
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
