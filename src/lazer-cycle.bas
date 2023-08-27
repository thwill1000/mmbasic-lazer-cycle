' Copyright (c) 2022-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

Option Base 0
Option Default None
Option Explicit On
' Option LcdPanel NoConsole

Const VERSION = 10002 ' 1.0.2

'!define CTRL_NO_SNES

'!ifdef PGLCD1
  '!define SOUND_USE_PWM
'!endif

'!info defined NARROW_TRACES

#Include "splib/system.inc"

'!if defined(PICOMITEVGA) || defined(PICOMITE)
  '!replace { Page Copy 0 To 1 , B } { NOP }
  '!replace { Page Copy 1 To 0 , B } { NOP }
  '!replace { Page Copy 1 To 0 , I } { NOP }
  '!replace { Page Write 1 } { NOP }
  '!replace { Page Write 0 } { NOP }
  '!replace { NOP } { ? ; }
'!endif
'!if defined(PICOMITEVGA)
  '!replace { Mode 7 } { Mode 2 }
'!elif defined(PICOMITE)
  '!replace { Mode 7 } { }
'!endif

#Include "splib/ctrl.inc"
#Include "splib/sound.inc"
#Include "splib/string.inc"
#Include "splib/msgbox.inc"
#Include "highscr.inc"
#Include "menu.inc"
'!if defined(PGLCD) || defined(PGLCD2)
#Include "splib/pglcd.inc"
'!endif

If sys.is_device%("pm*") Then
  If Val(Mm.Info(CpuSpeed)) < 252000000 Then
    Error "Requires OPTION CPUSPEED 252000 or 378000"
  EndIf
EndIf

If sys.is_device%("cmm2*") Then
  Const USE_CONTROLLERS$ = "controller_data_cmm2"
  Const START_TEXT$ = str.centre$("Press START, FIRE or SPACE", 40)
ElseIf sys.is_device%("mmb4w") Then
  Const USE_CONTROLLERS$ = "controller_data_mmb4w"
  Const START_TEXT$ = str.centre$("Press SPACE to play", 40)
ElseIf sys.is_device%("pglcd") Then
  Const USE_CONTROLLERS$ = "controller_data_pglcd"
  Const START_TEXT$ = str.centre$("Press START to play", 40)
ElseIf sys.is_device%("pmvga") Then
  Const USE_CONTROLLERS$ = "controller_data_pmvga"
  Const START_TEXT$ = str.centre$("Press START, FIRE or SPACE", 40)
Else
  Error "Unsupported device: " + Mm.Device$
EndIf

Const HIGHSCORE_FILENAME$ = highscr.get_directory$() + "/lazer-cycle.csv"
Const A_START_SELECT = ctrl.A Or ctrl.START Or ctrl.SELECT

Mode 7
Page Write 1
'!uncomment_if NARROW_TRACES
' Const WIDTH% = Mm.HRes \ 2
' Const HEIGHT% = (Mm.VRes - 20) \ 2
'!endif
'!ifndef NARROW_TRACES
Const WIDTH% = Mm.HRes \ 3
Const HEIGHT% = (Mm.VRes - 20) \ 3
'!endif
Const X_OFFSET% = MM.HRes \ 2
Const Y_OFFSET% = MM.VRes \ 2
Const NORTH% = 0, EAST% = 1, SOUTH% = 2, WEST% = 3
Const MAX_CYCLE_IDX% = 3
Const SCORE_Y% = Mm.VRes - 16
Const STATE_OK%    = &b000 ' 0; values 1-3 are "imminent death"
Const STATE_DYING% = &b100 ' 4
Const STATE_DEAD%  = &b101 ' 5
Const HORIZONTAL_MASK% = ctrl.LEFT Or ctrl.RIGHT
Const VERTICAL_MASK%   = ctrl.UP Or ctrl.DOWN
Const DIRECTION_MASK%  = HORIZONTAL_MASK% Or VERTICAL_MASK%

' These would be constants but MMBasic does not support constant arrays
Dim NEXT_DIR%(7)        = (EAST%, NORTH%, WEST%, SOUTH%, EAST%, NORTH%, WEST%, SOUTH%)
Dim SCORE_X%(3)         = (35, 105, 175, 245)
Dim DIRECTIONS%(3)      = (-WIDTH%, 1, WIDTH%, -1)
Dim COMPASS_TO_CTRL%(3) = (ctrl.UP, ctrl.RIGHT, ctrl.DOWN, ctrl.LEFT)
'!uncomment_if NARROW_TRACES
' Dim FRAME_DURATIONS%(5) = (33, 30, 27, 24, 21, 18)
'!endif
'!ifndef NARROW_TRACES
Dim FRAME_DURATIONS%(5) = (42, 38, 34, 30, 26, 22)
'!endif
Dim MUSIC_ENTERTAINER%(792 \ 8)
Dim MUSIC_BLACK_WHITE_RAG%(888 \ 8)

Dim ui_ctrl$ ' Controller driver for controlling the UI.
Dim attract_mode% = 1
Dim score%
Dim difficulty% = Not sys.is_device%("pglcd")
Dim frame_duration%
Dim next_frame%
Dim music_track% = 1 ' 1 = The Entertainer, 2 = The Black & White Rag.

' Each cell of the arena takes up 1 byte:
'   bit  0   - occupied by cycle
'   bits 1-2 - index of cycle
'   bits 3-4 - direction cycle was going in when entered cell
'   bits 5-6 - unused
'   bit  7   - arena wall (other bits will be 0)
Dim arena%(HEIGHT% * WIDTH% \ 8)

Dim cycle.current% ' Current cycle index, set before calling controller subroutines.
Dim cycle.score%(MAX_CYCLE_IDX%)
Dim cycle.nxt%(MAX_CYCLE_IDX%)
Dim cycle.pos%(MAX_CYCLE_IDX%)
Dim cycle.dir%(MAX_CYCLE_IDX%)
Dim cycle.colour%(MAX_CYCLE_IDX%) = (Rgb(Red), Rgb(Yellow), Rgb(Cyan), Rgb(Green))
Dim cycle.ctrl$(MAX_CYCLE_IDX%) Length 32
Dim cycle.ctrl_setting$(MAX_CYCLE_IDX%) Length 32
Dim cycle.state%(MAX_CYCLE_IDX%)
Dim cycle.last_key%(MAX_CYCLE_IDX%)

Dim num_alive%
Dim num_humans%

sys.override_break("on_break")

init_globals()
clear_display()
sound.init()
sound.play_music(MUSIC_ENTERTAINER%(), "on_music_done")
outer_loop()
End

Sub outer_loop()
  Local attract_mode% = 1, i%

  Do
    If attract_mode% Then wipe() : attract_mode% = Not show_title%(5000)
    If attract_mode% Then wipe() : attract_mode% = Not show_instructions%(15000)
    If attract_mode% Then wipe() : attract_mode% = Not show_highscore%(5000)
    If Not attract_mode% Then
      wipe()
      If Not menu.show%(ui_ctrl$, cycle.ctrl_setting$(), cycle.colour%()) Then end_program()
    EndIf

    wipe()
    init_game(attract_mode%)
    draw_arena()
    draw_score()
    If Not attract_mode% Then ready_steady_go()

    If game_loop%() Then
      ' Game loop interrupted after all human players dead.
      attract_mode% = 0
    ElseIf Not attract_mode% Then
      ' Game ended normally whilst not in attract mode.
      show_game_over()
      attract_mode% = Not show_highscore%(5000)
    EndIf

    ctrl.term()
  Loop
End Sub

Sub on_break()
  end_program(1)
End Sub

Sub end_program(break%)
  If sys.is_device%("pglcd") Then
    pglcd.end(break%)
  Else
    If sys.is_device%("pmvga") Then Mode 1
    Page Write 0
    Colour Rgb(White), Rgb(Black)
    Cls
    sys.restore_break()
    sound.term()
    ctrl.term()
    End
  EndIf
End Sub

Sub on_music_done()
  If music_track% = 1 Then
    sound.play_music(MUSIC_BLACK_WHITE_RAG%(), "on_music_done")
    music_track% = 2
  Else
    sound.play_music(MUSIC_ENTERTAINER%(), "on_music_done")
    music_track% = 1
  EndIf
End Sub

' Initialises global variables.
Sub init_globals()
  Local a%, i%, j%

  ' Initialise list of controllers.
  Restore USE_CONTROLLERS$
  Local num_ctrl%, num_poll%
  Read num_ctrl%, num_poll%
  Dim CTRL_DESCRIPTION$(num_ctrl% - 1)
  Dim CTRL_DRIVER$(num_ctrl% - 1)
  Dim CTRLS_TO_POLL$(Max(1, num_poll% - 1))
  j% = 0
  For i% = 0 To num_ctrl% - 1
    Read CTRL_DESCRIPTION$(i%), CTRL_DRIVER$(i%), a%
    If a% Then
      CTRLS_TO_POLL$(j%) = CTRL_DRIVER$(i%)
      Inc j%
    EndIf
  Next
  If num_poll% = 1 Then CTRLS_TO_POLL$(1) = CTRLS_TO_POLL$(0)

  ' Initialise controller settings.
  cycle.ctrl_setting$(0) = "ai_control"
  cycle.ctrl_setting$(1) = "ai_control"
  For i% = 2 To MAX_CYCLE_IDX% : cycle.ctrl_setting$(i%) = "no_control" : Next

  ' Initialise high-scores.
  highscr.init(HIGHSCORE_FILENAME$, "highscore_data")

  ' Initialise music data.
  sound.load_data("entertainer_music_data", MUSIC_ENTERTAINER%())
  sound.load_data("black_white_rag_music_data", MUSIC_BLACK_WHITE_RAG%())
End Sub

' Displays the title screen for a specified duration or until the user presses
' START/FIRE/SPACE.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function show_title%(duration%)
  If sys.is_device%("pglcd") Then
    Const platform$ = "GameMite"
  ElseIf sys.is_device%("pm") Then
    Const platform$ = "PicoMite"
  ElseIf sys.is_device%("pmvga") Then
    Const platform$ = "PicoGAME VGA"
  Else
    Const platform$ = "Colour Maximite 2"
  EndIf

  Text X_OFFSET%, Y_OFFSET% - 27, "LAZER CYCLE", "CM", 1, 2, Rgb(White) 
  Text X_OFFSET%, Y_OFFSET% - 10, platform$ + " Version", "CM", 7, 1, Rgb(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 8, "(c) 2022-2023 Thomas Hugo Williams", "CM", 7, 1, Rgb(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 20, "www.sockpuppetstudios.com", "CM", 7, 1, Rgb(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 40, START_TEXT$, "CM", 1, 1, Rgb(White)
  Page Copy 1 To 0, B
  show_title% = wait%(duration%)
End Function

' Displays the instructions screen for a specified duration or until the user presses
' START/FIRE/SPACE.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function show_instructions%(duration%)
  Const ch$ = Chr$(205)
  Local y% = Y_OFFSET% - 95
  Text X_OFFSET%, y%, ch$ + ch$ + " LAZER CYCLE " + ch$ + ch$, "CT", 1, 1, Rgb(White) : Inc y%, 20
  Text X_OFFSET%, y%, "An arcade game for 1-4 players.", "CT", 1, 1, Rgb(Red) : Inc y%, 20
  Text X_OFFSET%, y%, "Pilot your cycle around the", "CT", 1, 1, Rgb(Yellow) : Inc y%, 12
  Text X_OFFSET%, y%, "arena leaving a trail of light", "CT", 1, 1, Rgb(Yellow) : Inc y%, 12
  Text X_OFFSET%, y%, "behind, longest trail wins.", "CT", 1, 1, Rgb(Yellow) : Inc y%, 20
  Text X_OFFSET%, y%, "You are eliminated if you hit the", "CT", 1, 1, Rgb(Cyan) : Inc y%, 12
  Text X_OFFSET%, y%, "arena wall, or one of the trails.", "CT", 1, 1, Rgb(Cyan) : Inc y%, 20
  Text X_OFFSET%, y%, "Use keyboard, joystick or gamepad", "CT", 1, 1, Rgb(Green) : Inc y%, 12
  Text X_OFFSET%, y%, "UP, DOWN, LEFT and RIGHT to steer.", "CT", 1, 1, Rgb(Green) : Inc y%, 20
  Text X_OFFSET%, y%, "Good Luck!", "CT", 1, 1, Rgb(White)
  Page Copy 1 To 0, B
  show_instructions% = wait%(duration%)
End Function

' Displays the highscore table for a specified duration or until the user presses
' START/FIRE/SPACE.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function show_highscore%(duration%)
  Local ctrl$ = highscr.show_table$(CTRLS_TO_POLL$(), 5000)
  If ctrl$ <> "" Then
    If ui_ctrl$ = "" Then ui_ctrl$ = ctrl$
    show_highscore% = 1
  EndIf
End Function

Sub clear_display()
   Box 0, 0, Mm.HRes, Mm.VRes, 1, Rgb(Black), Rgb(Black)
   Page Copy 1 To 0, B
End Sub

' Waits a specified duration for the user to press START, SELECT, A or SPACE.
'
' @param duration%  duration in milliseconds; if 0 then indefinite.
' @return           1 if the user pressed button/key,
'                   0 if the duration expired.
Function wait%(duration%)
  ctrl.init_keys()
  Local ctrl$ = ctrl.poll_multiple$(CTRLS_TO_POLL$(), A_START_SELECT, duration%)
  If Len(ctrl$) Then ui_ctrl$ = Choice(ui_ctrl$ = "", ctrl$, ui_ctrl$)
  wait% = Len(ctrl$)
End Function

' Handler for the SELECT button that shows the Quit dialog.
'
' @param   ctrl$  controller driver to query.
' @return  1      if the user selected the 'Quit' option, otherwise 0.
Function on_select%(ctrl$)
  msgbox.beep(1)
  Local buttons$(1) Length 3 = ("Yes", "No")
  Const msg$ = Choice(num_alive%, "Return to game menu?", "    Quit game?")
  Const x% = 9, y% = 5, fg% = Rgb(White), bg% = Rgb(Black), frame% = Rgb(Cyan)

  Page Copy 0 To 1, B ' Store screen
  If sys.is_device%("pm*") Then
    FrameBuffer Create
    If sys.is_device%("pmvga") Then FrameBuffer Copy N, F, B Else FrameBuffer Copy N, F
  EndIf

  Const a% = msgbox.show%(x%, y%, 22, 9, msg$, buttons$(), 1, ctrl$, fg%, bg%, frame%, msgbox.NO_PAGES)
  If buttons$(a%) = "Yes" Then
    If num_alive% > 0 Then
      on_select% = 1
      num_alive% = 0
    Else
      end_program()
    EndIf
  EndIf

  Page Copy 1 To 0, B ' Restore screen.
  If sys.is_device%("pm*") Then
    If sys.is_device%("pmvga") Then FrameBuffer Copy F, N, B Else FrameBuffer Copy F, N
    FrameBuffer Close F
  EndIf

  ctrl.wait_until_idle(ctrl$)
End Function

Sub init_game(attract_mode%)
  frame_duration% = FRAME_DURATIONS%(difficulty%)
  num_alive% = 0 ' Incremented later.
  num_humans% = 0
  score% = 0

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

  ' Initialise game ports and keyboard routines.
  Local i%
  For i% = 0 To MAX_CYCLE_IDX%
    cycle.ctrl$(i%) = Choice(attract_mode%, "ai_control", cycle.ctrl_setting$(i%))
    On Error Ignore
    Call cycle.ctrl$(i%), ctrl.OPEN
    If Mm.ErrNo <> 0 Then cycle.ctrl$(i%) = "no_control"
    On Error Abort
  Next
  ctrl.init_keys()

  ' Initialise cycle state.
  cycle.dir%(0) = EAST%
  cycle.dir%(1) = WEST%
  cycle.dir%(2) = SOUTH%
  cycle.dir%(3) = NORTH%

  cycle.pos%(0) = WIDTH * (HEIGHT% \ 2) + 5
  cycle.pos%(1) = WIDTH% * (HEIGHT% \ 2) + WIDTH% - 6
  cycle.pos%(2) = 5.5 * WIDTH%
  cycle.pos%(3) = WIDTH% * (HEIGHT% - 6) + WIDTH% \ 2

  For i% = 0 To MAX_CYCLE_IDX%
    cycle.score%(i%) = 0
    cycle.last_key%(i%) = 0
    If cycle.ctrl$(i%) = "no_control" Then
      cycle.pos%(i%) = -1
      cycle.nxt%(i%) = -1
      cycle.state%(i%) = STATE_DEAD%
    Else
      Inc num_alive%
      Inc num_humans%, cycle.ctrl$(i%) <> "ai_control"
      cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(cycle.dir%(i%))
      Poke Byte p_arena% + cycle.pos%(i%), (cycle.dir%(i%) << 3) + (i% << 1) + 1
      Poke Byte p_arena% + cycle.nxt%(i%), (cycle.dir%(i%) << 3) + (i% << 1) + 1
      cycle.state%(i%) = STATE_OK%
    EndIf
  Next
End Sub

Sub draw_arena()
'!uncomment_if NARROW_TRACES
  ' Local a%, i%, j%
  ' For i% = 0 To Bound(arena%(), 1) - 1
  '   a% = arena%(i%)
  '   If a% = 0 Then Continue For
  '   For j% = 0 To 7
  '     If Peek(Var a%, j%) <> 128 Then Continue For
  '     Pixel 2 * (((i% * 8) Mod WIDTH%) + j%), 2 * ((i% * 8) \ WIDTH%), Rgb(Lilac)
  '   Next
  ' Next
'!endif
'!ifndef NARROW_TRACES
  Local x%
  For x% = 0 To (HEIGHT% * WIDTH%) - 1
    If Peek(Var arena%(), x%) <> 128 Then Continue For
    Box 3 * (x% Mod WIDTH%), 3 * (x% \ WIDTH%), 2, 2, , Rgb(Lilac)
  Next
'!endif
End Sub

Sub ready_steady_go()
  Local music_ptr_bak% = sound.music_ptr%, enabled_bak% = sound.enabled%  
  sound.enable(sound.enabled% Xor sound.MUSIC_FLAG%)
  sound.play_fx(sound.FX_READY_STEADY_GO%())

  ' Draw cycle starting positions.
  Local i%
  For i% = 0 To MAX_CYCLE_IDX%
    If Not (cycle.state%(i%) And &b11) Then cycle.draw(i%)
  Next

  Local msg$(2) = ("READY", "STEADY", "GO!")
  For i% = 0 To 2
    Text X_OFFSET%, Y_OFFSET% - 10, msg$(i%), "CM", 1, 2, Rgb(White)
    Page Copy 1 To 0, I
    Pause 1280
    If i% = 2 Then Pause 240
    Text X_OFFSET%, Y_OFFSET% - 10, msg$(i%), "CM", 1, 2, Rgb(Black)
  Next

  sound.enable(enabled_bak%)
  sound.music_ptr% = music_ptr_bak%
End Sub

' @return  0 - normal game over
'          1 - game interrupted after all human players died
Function game_loop%()
  Local d%, i%, key%, next_frame% = Timer + frame_duration%, tf%, tmp$

  ' Change 0 => 1 to easily test high-score code.
  If 0 Then
    num_alive% = 0
    cycle.score%(0) = 3175
    cycle.score%(1) = 2175
    cycle.score%(2) = 1175
    cycle.score%(3) = 975
    score% = 3175
  EndIf

  Do While num_alive% > 0
    tf% = Timer
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

    Page Copy 1 To 0, I

    ' Move cycles.
    For i% = 0 To MAX_CYCLE_IDX%
      If Not (cycle.state%(i%) And &b11) Then cycle.pos%(i%) = cycle.nxt%(i%)
    Next

    ' Determine changes of direction and check for collisions.
    For i% = 0 To MAX_CYCLE_IDX%
      cycle.current% = i%
      Call cycle.ctrl$(i%), key%
      d% = cycle.dir%(i%)
      If key% And ctrl.SELECT Then game_loop% = on_select%(cycle.ctrl$(i%))
      key% = key% And DIRECTION_MASK%
      If key% <> cycle.last_key%(i%) Then
        Select Case d%
          Case NORTH%, SOUTH%
            Select Case key% And HORIZONTAL_MASK%
              Case ctrl.LEFT : d% = WEST%
              Case ctrl.RIGHT : d% = EAST%
            End Select
          Case EAST%, WEST%
            Select Case key% And VERTICAL_MASK%
              Case ctrl.UP : d% = NORTH%
              Case ctrl.DOWN : d% = SOUTH%
            End Select
        End Select
        cycle.dir%(i%) = d%
        cycle.last_key%(i%) = key%
      EndIf
      cycle.nxt%(i%) = cycle.pos%(i%) + DIRECTIONS%(d%)
      If cycle.state%(i%) <> STATE_DEAD% Then cycle.check_collision(i%)
'      If i% = 0 Then draw_controller(i%, key%)
    Next

    ' Wait for next frame.
    Do While Timer < next_frame% : Loop
    next_frame% = Timer + frame_duration%

'    If score% Mod 5 Then draw_framerate(1000 / (Timer - tf%))

    If num_humans% > 0 Then Continue Do

    ' Check for "attract mode" being interrupted.
    tmp$ = CTRLS_TO_POLL$(score% Mod (Bound(CTRLS_TO_POLL$(), 1) + 1))
    If ctrl.poll_single%(tmp$, A_START_SELECT) Then
      ui_ctrl$ = Choice(ui_ctrl$ = "", tmp$, ui_ctrl$)
      num_alive% = 0
      game_loop% = 1
    EndIf

  Loop

  ' Ensure display updated at end of loop.
  Page Copy 1 To 0, B

  ' Wait for current sound effect (if any) to complete.
  Do While sound.is_playing%(sound.FX_FLAG%) : Loop
End Function

Sub draw_score()
  If num_humans% > 0 Or ((score% \ 100) And &b1) Then
    Local i%, s$ = Str$(score%, 5, 0, "0")
    For i% = 0 To MAX_CYCLE_IDX%
      If cycle.state%(i%) < STATE_DYING% Then
        Text SCORE_X%(i%), SCORE_Y%, s$, , 1, 1, cycle.colour%(i%)
      EndIf
    Next
    Exit Sub
  EndIf

  ' If there are no human players we toggle between the score and the "Press ..." text.
  If score% Mod 100 = 95 Then
    Local i%, sc%
    Text Mm.HRes \ 2, SCORE_Y%, Space$(40), "C", 1, 1, Rgb(White)
    For i% = 0 To MAX_CYCLE_IDX%
      sc% = Choice(cycle.state%(i%) < STATE_DYING%, score%, (cycle.score%(i%) \ 5) * 5)
      Text SCORE_X%(i%), SCORE_Y%, Str$(sc%, 5, 0, "0"), , 1, 1, cycle.colour%(i%)
    Next
  Else
    Text Mm.HRes \ 2, SCORE_Y%, START_TEXT$, "C", 1, 1, Rgb(White)
  EndIf
End Sub

' Shows controller input in the score area.
'
' @param  idx%  player number, 1-4
' @param  x%    input bitmap from controller driver.
Sub draw_controller(idx%, x%)
  Local s$ = cycle.ctrl$(idx%) + ": " + ctrl.bits_to_string$(x%)
  Text 0, SCORE_Y%, str.rpad$(s$, 40), , 1, 1, Rgb(White)
End Sub

' Shows framerate in the score area.
Sub draw_framerate(rate%)
  Text 0, SCORE_Y%, str.rpad$("Framerate: " + Str$(rate%) + " fps", 40), , 1, 1, Rgb(White)
End Sub

Sub wipe()
  Local y%
  sound.play_fx(sound.FX_WIPE%())
  For y% = 0 To Mm.VRes \ 2 Step 5
     Box Mm.HRes \ 2 - y% * 1.2, Mm.VRes \ 2 - y%, 2.4 * y%, 2 * y%, 5, Rgb(Cyan), Rgb(Black)
     Page Copy 1 To 0, B
     Pause 30
  Next
  clear_display()
End Sub

' Draw cycle if STATE_OK% or STATE_DYING%.
Sub cycle.draw(idx%)
'!uncomment_if NARROW_TRACES
  ' Local p% = cycle.pos%(idx%), n% = cycle.nxt%(idx%)
  ' Line 2*(p% Mod WIDTH%), 2*(p%\WIDTH%), 2*(n% Mod WIDTH%), 2*(n%\WIDTH%), 1, cycle.colour%(idx%) * (cycle.state%(idx%) <> STATE_DYING%)
'!endif
'!ifndef NARROW_TRACES
  Local p% = cycle.pos%(idx%), n% = cycle.nxt%(idx%), xn% = 3*(n% Mod WIDTH%), yn% = 3*(n%\WIDTH%)
  Local colour_% = cycle.colour%(idx%) * (cycle.state%(idx%) <> STATE_DYING%)
  Line 3*(p% Mod WIDTH%), 3*(p%\WIDTH%), xn%, yn%, 2, colour_%
  Box xn%, yn%, 2, 2, , colour_% ' BOX necessary to avoid missing pixel artefact in trace.
'!endif
End Sub

Sub cycle.dying(idx%)
  cycle.draw(idx%)
  cycle.pos%(idx%) = cycle.nxt%(idx%) ' Move
  cycle.current% = idx%
  Local key%
  die_control(key%)
  Select Case key%
    Case ctrl.UP:    cycle.dir%(idx%) = NORTH%
    Case ctrl.DOWN:  cycle.dir%(idx%) = SOUTH%
    Case ctrl.LEFT:  cycle.dir%(idx%) = WEST%
    Case ctrl.RIGHT: cycle.dir%(idx%) = EAST%
  End Select
  cycle.nxt%(idx%) = cycle.pos%(idx%) + DIRECTIONS%(cycle.dir%(idx%))
  cycle.check_collision(idx%)
End Sub

Sub show_game_over()
  ' Sort scores and then round down to nearest 5.
  Local dummy%, i%, idx%(MAX_CYCLE_IDX%), j%, k%, winner%
  Sort cycle.score%(), idx%(), 1
  For i% = 0 To MAX_CYCLE_IDX%
    cycle.score%(i%) = (cycle.score%(i%) \ 5) * 5 ' Round down to nearest 5.
  Next
  winner% = idx%(0)

  Box X_OFFSET% - 130, Y_OFFSET% - 50, 260, 80, 0, Rgb(Black), Rgb(Black)

  If cycle.ctrl_setting$(winner%) = "ai_control" Then
    Text X_OFFSET%, Y_OFFSET% - 10, " COMPUTER WINS ", "CM", 1, 2, cycle.colour%(winner%)
    Page Copy 1 To 0, B
  Else
    Local txt$ = " PLAYER " + Str$(winner% + 1) + " WINS "
    Text X_OFFSET%, Y_OFFSET% - 25, txt$, "CM", 1, 2, cycle.colour%(winner%)
    Text X_OFFSET%, Y_OFFSET% + 5, " SCORE: " + Str$(cycle.score%(0)) + " ", "CM", 1, 2, cycle.colour%(winner%)
    Page Copy 1 To 0, B

    ' Determine bonus.
    Local multiplier% = -30 + 10 * difficulty%
    For i% = 0 To MAX_CYCLE_IDX%
      If cycle.ctrl_setting$(i%) <> "no_control" Then Inc multiplier%, 10
    Next

    If multiplier% > 1 Then
      Pause 1500
      Local s$ = " BONUS +" + Str$(multiplier%) + "% "
      Text X_OFFSET%, Y_OFFSET% + 5, s$ , "CM", 1, 2, RGB(White)
      Page Copy 1 To 0, B
      Pause 1500
      Local bonus% = cycle.score%(0) * (1 + multiplier% / 100)
      Do While cycle.score%(0) < bonus%
        Select Case bonus% - cycle.score%(0)
          Case > 60 : Inc cycle.score%(0), 50
          Case > 15 : Inc cycle.score%(0), 10
          Case Else : Inc cycle.score%(0), 5
        End Select
        s$ = " SCORE: " + Str$(cycle.score%(0)) + " "
        Text X_OFFSET%, Y_OFFSET% + 5, s$, "CM", 1, 2, cycle.colour%(winner%)
        Page Copy 1 To 0, B
        sound.play_fx(sound.FX_SELECT%())
        Pause 100
      Loop
    EndIf
  EndIf

  dummy% = wait%(5000)

  wipe()

  ' Insert into high-score table.
  ' For the moment only the winner can enter a high-score,
  ' to change that make the upper bound of the FOR statement = MAX_CYCLE_IDX%
  Local new_highscore%, player%
  For i% = 0 To 0
    player% = idx%(i%)
    If cycle.ctrl_setting$(player%) = "ai_control" Then Continue For
    For j% = 0 To Bound(highscr.values$())
      If cycle.score%(i%) > Val(Field$(highscr.values$(j%), 2)) Then
        For k% = Bound(highscr.values$(), 1) To j% Step -1
          If k% <> 0 Then highscr.values$(k%) = highscr.values$(k% - 1)
        Next
        highscr.values$(j%) = ", " + Str$(cycle.score%(i%))
        highscr.edit(player% + 1, j%, cycle.colour%(player%), cycle.ctrl_setting$(player%))
        new_highscore% = 1
        Exit For
      EndIf
    Next
  Next
  If new_highscore% Then highscr.save(HIGHSCORE_FILENAME$)
End Sub

Sub ai_control(x%)
  If x% < 0 Then Exit Sub

  Local idx% = cycle.current%
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

  x% = COMPASS_TO_CTRL%(d%)
End Sub

Sub die_control(x%)
  If x% < 0 Then Exit Sub
  Local bits% = Peek(Var arena%(), cycle.pos%(cycle.current%)) >> 1
  If (bits% And &b11) = cycle.current% Then
    bits% = (bits% >> 2) And &b11
    x% = COMPASS_TO_CTRL%((bits% + 2) Mod 4)
  EndIf
End Sub

Sub no_control(x%)
  x% = 0
End Sub

Sub keys_cegg(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A     ' Space
  Inc x%, ctrl.keydown%(97)  * ctrl.UP    ' A
  Inc x%, ctrl.keydown%(122) * ctrl.DOWN  ' Z
  Inc x%, ctrl.keydown%(44)  * ctrl.LEFT  ' comma
  Inc x%, ctrl.keydown%(46)  * ctrl.RIGHT ' full-stop
End Sub

Sub keys_azxc(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A     ' Space
  Inc x%, ctrl.keydown%(97)  * ctrl.UP    ' A
  Inc x%, ctrl.keydown%(122) * ctrl.DOWN  ' Z
  Inc x%, ctrl.keydown%(120) * ctrl.LEFT  ' X
  Inc x%, ctrl.keydown%(99)  * ctrl.RIGHT ' C
End Sub

Sub keys_punc(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32) * ctrl.A     ' Space
  Inc x%, ctrl.keydown%(39) * ctrl.UP    ' single-quote
  Inc x%, ctrl.keydown%(47) * ctrl.DOWN  ' slash
  Inc x%, ctrl.keydown%(44) * ctrl.LEFT  ' comma
  Inc x%, ctrl.keydown%(46) * ctrl.RIGHT ' full-stop
End Sub

Sub cycle.check_collision(idx%)
  ' Handle dying.
  If cycle.state%(idx%) = STATE_DYING% Then
    Poke Var arena%(), cycle.pos%(idx%), 0
    Local mask% = (idx% << 1) + 1
    If (Peek(Var arena%(), cycle.nxt%(idx%)) And mask%) <> mask% Then
      cycle.ctrl$(idx%) = "no_control"
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
  Inc num_alive%, -1
  If cycle.ctrl$(idx%) <> "ai_control" Then Inc num_humans%, -1
  cycle.ctrl$(idx%) = "die_control"
  cycle.nxt%(idx%) = cycle.pos%(idx%)
  cycle.score%(idx%) = score%
  sound.play_fx(sound.FX_DIE%())
End Sub

controller_data_cmm2:

Data 11, 6
Data "KEYS: CURSOR",   "keys_cursor", 1
Data "KEYS: AZ,.",     "keys_cegg",   0
Data "KEYS: AZXC",     "keys_azxc",   0
Data "KEYS: '/,.",     "keys_punc",   0
Data "JOYSTICK DX",    "atari_dx",    1
Data "NES GAMEPAD DX", "nes_dx",      1
Data "WII CTRL I2C1",  "wii_any_1",   1
Data "WII CTRL I2C2",  "wii_any_2",   1
Data "WII CTRL I2C3",  "wii_any_3",   1
Data "AI",             "ai_control",  0
Data "NONE",           "no_control",  0

controller_data_mmb4w:

Data 6, 1
Data "KEYS: CURSOR",   "keys_cursor", 1
Data "KEYS: AZ,.",     "keys_cegg",   0
Data "KEYS: AZXC",     "keys_azxc",   0
Data "KEYS: '/,.",     "keys_punc",   0
Data "AI",             "ai_control",  0
Data "NONE",           "no_control",  0

controller_data_pglcd:
Data 3, 1
Data "GAMEPAD",      "ctrl.pglcd2", 1
Data "AI",           "ai_control",  0
Data "NONE",         "no_control",  0

controller_data_pmvga:

Data 10, 5
Data "KEYS: CURSOR", "keys_cursor", 1
Data "KEYS: AZ,.",   "keys_cegg",   0
Data "KEYS: AZXC",   "keys_azxc",   0
Data "KEYS: '/,.",   "keys_punc",   0
Data "GAMEPAD A",    "nes_a",       1
Data "GAMEPAD B",    "nes_b",       1
Data "JOYSTICK A",   "atari_a",     1
Data "JOYSTICK B",   "atari_b",     1
Data "AI",           "ai_control",  0
Data "NONE",         "no_control",  0

highscore_data:

Data "MICKEY, 4000"
Data "MIKE, 3000"
Data "PETER, 2000"
Data "DAVEY, 1500"
Data "JOHN, 1250"
Data "PAUL, 1000"
Data "GEORGE, 800"
Data "RINGO, 600"
Data "MIDDLE, 400"
Data "MODAL, 200"

entertainer_music_data:

Data 792 ' Number of bytes of music data.
Data 3   ' Number of channels.
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

black_white_rag_music_data:
Data 888 ' Number of bytes of music data.
Data 3   ' Number of channels.
Data &h3440003A41003A41, &h3E00354100354100, &h00333F00333F0026, &h303C00303C002F3B
Data &h35002C38002A3600, &h0027330027330029, &h333F00330000333F, &h0000273F00330000
Data &h00330000333F0027, &h273F00273F00273F, &h3F00003F00270000, &h00003E00003E0000
Data &h314216273F16273D, &h4211223F27313D27, &h27313F27313D1122, &h283F1B273D1B2742
Data &h411D29421C28421C, &h1E2A3D1E2A3F1D29, &h3041202C3C202C3B, &h4120273C27303B27
Data &h27303C27303B2027, &h2D3C202C3B202C41, &h3F222E41212D4121, &h24303824303C222E
Data &h313C222E3D222E3D, &h3C1B273D27313D27, &h27313D27313D1B27, &h3139222E3A222E3A
Data &h391B273A27313A27, &h27313A27313A1B27, &h2400002000000000, &h0000000000270000
Data &h003300003000002C, &h003C000038000000, &h4400000000003F00, &h00004B0000480000
Data &h31421B273F1B273D, &h4216223F27313D27, &h27313F27313D1622, &h283F1B273D1B2742
Data &h411D29421C28421C, &h1E2A3D1E2A3F1D29, &h3041202C3C202C3B, &h411B273C27303B27
Data &h27303C27303B1B27, &h2D3C202C3B202C41, &h3F222E41212D4121, &h24303C24303D222E
Data &h3339212D35212D41, &h3F1D294129334129, &h29333C29333D1D29, &h313A222E39222E3A
Data &h3F1D294129314129, &h29313A29313D1D29, &h30381B27331B273C, &h381B273C27303C27
Data &h1E2A3A1E2A3A1B27, &h3338303338303338, &h443C3F4430333830, &h0000000000003C3F
Data &h314216273F16273D, &h4211223F27313D27, &h27313F27313D1122, &h283F1B273D1B2742
Data &h411D29421C28421C, &h1E2A3D1E2A3F1D29, &h3041202C3C202C3B, &h4120273C27303B27
Data &h27303C27303B2027, &h2D3C202C3B202C41, &h3F222E41212D4121, &h24303824303C222E
Data &h313C222E3D222E3D, &h3C1B273D27313D27, &h27313D27313D1B27, &h3139222E3A222E3A
Data &h391B273A27313A27, &h27313A27313A1B27, &h2400002000000000, &h0000000000270000
Data &h003300003000002C, &h003C000038000000, &h4400000000003F00, &h00004B0000480000
Data &h31421B273F1B273D, &h4216223F27313D27, &h27313F27313D1622, &h283F1B273D1B2742
Data &h411D29421C28421C, &h1E2A3D1E2A3F1D29, &h3041202C3C202C3B, &h411B273C27303B27
Data &h27303C27303B1B27, &h2D3C202C3B202C41, &h3F222E41212D4121, &h24303C24303D222E
Data &h3339212D35212D41, &h3F1D294129334129, &h29333C29333D1D29, &h313A222E39222E3A
Data &h3F1D294129314129, &h29313A29313D1D29, &h30381B27331B273C, &h381B273C27303C27
Data &h1E2A3A1E2A3A1B27, &h3338303338303338, &h443C3F4430333830, &h0000000000003C3F
Data &hFFFFFFFFFFFFFFFF, &hFFFFFFFFFFFFFFFF, &hFFFFFFFFFFFFFFFF
