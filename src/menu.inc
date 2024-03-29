' Copyright (c) 2022-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

' Shows the game settings menu.
'
' @param[in, out]  ui_ctrl$    on entry the controller driver to use to navigate the menu,
'                              on exit the controller driver for first human player,
'                              unchanged if there are no human players.
' @param[in,out]   ctrls$()    on entry the current controller driver per player,
'                              on exit the new controller driver per player.
' @param[in]       colours%()  the player colours
' @return                      1 to continue, or 0 to QUIT.
Function menu.show%(ui_ctrl$, ctrls$(), colours%())
  Const x% = X_OFFSET% - 100
  Static initialised% = 0
  Local key%, i%, item% = 0, update% = 1
  Local sounds$(3) Length 10 = ("NONE      ", "FX ONLY   ", "MUSIC ONLY", "MUSIC & FX")
  Local sound_setting% = sound.enabled%
  Local music_ptr_bak%
  Local difficulties$(5) Length 6 = ("NOVICE", "1     ", "2     ", "3     ", "4     ", "5     ")

  If Not initialised% Then
    Dim menu.ctrls$(Bound(ctrls$(), 1)) Length 32
    initialised% = 1
  EndIf
  For i% = Bound(ctrls$(), 0) To Bound(ctrls$(), 1)
    menu.ctrls$(i%) = ctrls$(i%)
  Next

  ' Initialise menu.ctrls$(0) if all entries are currently "no" or "ai" control.
  Local count% = 0
  For i% = Bound(menu.ctrls$(), 0) To Bound(menu.ctrls$(), 1)
    Inc count%, Not InStr("ai_control|no_control", menu.ctrls$(i%))
  Next
  If count% = 0 Then menu.ctrls$(0) = ui_ctrl$

  Text X_OFFSET%, Y_OFFSET% + 90, VERSION_STRING$, "CM", 7, 1, Rgb(Cyan)

  ctrl.init_keys()
  Call ui_ctrl$, ctrl.OPEN

  Do

    If update% Then
      Text x%, Y_OFFSET% - 95, "START GAME", , 1, 1, Rgb(White)
      menu.txt_for_controller(x%, Y_OFFSET% - 75, 0, colours%(0))
      menu.txt_for_controller(x%, Y_OFFSET% - 55, 1, colours%(1))
      menu.txt_for_controller(x%, Y_OFFSET% - 35, 2, colours%(2))
      menu.txt_for_controller(x%, Y_OFFSET% - 15, 3, colours%(3))
      Text x%, Y_OFFSET% + 5,  "DIFFICULTY: " + difficulties$(difficulty%), , 1, 1, Rgb(White)
      Text x%, Y_OFFSET% + 25, "SOUND:      " + sounds$(sound_setting%), , 1, 1, Rgb(White)
      Text x%, Y_OFFSET% + 45, "QUIT", , 1, 1, Rgb(White)
      Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Cyan)
      Page Copy 1 To 0, B
      update% = 0
      Pause ctrl.UI_DELAY
    EndIf

    key% = 0
    Do While Not key%
      Call ui_ctrl$, key%
      If Not key% Then keys_cursor(key%) ' Always respond to the cursor keys.
    Loop
    If key% = ctrl.A Then key% = ctrl.SELECT

    Select Case key%
      Case ctrl.START
        menu.show% = 1
        Exit Do

      Case ctrl.UP
        Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Black)
        Inc item%, -1
        If item% < 0 Then item% = 7
        update% = 1

      Case ctrl.DOWN
        Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Black)
        Inc item%
        If item% > 7 Then item% = 0
        update% = 1

      Case ctrl.LEFT, ctrl.RIGHT, ctrl.SELECT
        Select Case item%
          Case 0
            If key% = ctrl.SELECT Then menu.show% = 1 : Exit Do

          Case 1,2,3,4
            menu.update_controller(item% - 1, Choice(key% = ctrl.LEFT, -1, +1), ui_ctrl$)
            update% = 1

          Case 5 ' Difficulty
            Inc difficulty%, Choice(key% = ctrl.LEFT, -1, 1)
            If difficulty% < 0 Then difficulty% = 5
            If difficulty% > 5 Then difficulty% = 0
            update% = 1

          Case 6 ' Sound
            If sound_setting% And sound.MUSIC_FLAG% Then music_ptr_bak% = sound.music_ptr%
            Inc sound_setting%, Choice(key% = ctrl.LEFT, -1, 1)
            If sound_setting% < 0 Then sound_setting% = 3
            If sound_setting% > 3 Then sound_setting% = 0
            sound.enable(sound_setting%)
            If sound_setting% And sound.MUSIC_FLAG% Then sound.music_ptr% = music_ptr_bak%
            update% = 1

          Case 7 ' Quit
            If key% = ctrl.SELECT Then Exit Do

        End Select

      Case Else
        sound.play_fx(sound.FX_BLART%())
        Pause ctrl.UI_DELAY

    End Select

    If update% Then sound.play_fx(sound.FX_SELECT%())

  Loop

  Call ui_ctrl$, ctrl.CLOSE

  ' Copy controller settings back into parameter.
  For i% = Bound(ctrls$(), 0) To Bound(ctrls$(), 1)
    ctrls$(i%) = menu.ctrls$(i%)
  Next

  ' Update ui_ctrl$ parameter to be controller chosen for first human player.
  For i% = Bound(ctrls$(), 0) To Bound(ctrls$(), 1)
    If ctrls$(i%) <> "ai_control" And ctrls$(i%) <> "no_control" Then
      ui_ctrl$ = ctrls$(i%)
      Exit For
    EndIf
  Next
End Function

Sub menu.txt_for_controller(x%, y%, idx%, colour%)
  Local txt$ = "PLAYER " + Str$(idx% + 1) + ":   " + menu.get_controller_name$(idx%)
  Text x%, y%, txt$, , 1, 1, colour%
End Sub

Function menu.get_controller_name$(idx%)
  Local i%
  For i% = Bound(CTRL_DRIVER$(), 0) To Bound(CTRL_DRIVER$(), 1)
    If menu.ctrls$(idx%) = CTRL_DRIVER$(i%) Then
      menu.get_controller_name$ = CTRL_DESCRIPTION$(i%)
      Do While Len(menu.get_controller_name$) < 14 : Cat menu.get_controller_name$, " " : Loop
      Exit Function
    Endif
  Next
  Error "Unknown controller: " + menu.ctrls$(idx%)
End Function

Sub menu.update_controller(idx%, delta%, ui_ctrl$)
  ' Temporarily close UI controller.
  Call ui_ctrl$, ctrl.CLOSE

  ' Find index of currently selected controller.
  Local i%
  For i% = Bound(CTRL_DRIVER$(), 0) To Bound(CTRL_DRIVER$(), 1)
    If menu.ctrls$(idx%) = CTRL_DRIVER$(i%) Then Exit For
  Next
  If i% = Bound(CTRL_DRIVER$(), 1) + 1 Then Error "Unknown controller: " + menu.ctrls$(idx%)

  Local ok%
  Do
    Inc i%, delta%
    If i% < 0 Then i% = Bound(CTRL_DRIVER$(), 1)
    If i% > Bound(CTRL_DRIVER$(), 1) Then i% = 0

    ' Check there is no conflict with other player's controller choice.
    If Not InStr("ai_control|no_control", CTRL_DRIVER$(i%)) Then
      If menu.has_controller%(idx%, CTRL_DRIVER$(i%)) Then Continue Do
    EndIf
    Select Case CTRL_DRIVER$(i%)
      Case "atari_a"   : ok% = Not menu.has_controller%(idx%, "nes_a")
      Case "atari_b"   : ok% = Not menu.has_controller%(idx%, "nes_b")
      Case "atari_dx"  : ok% = Not menu.has_controller%(idx%, "nes_dx")
      Case "nes_a"     : ok% = Not menu.has_controller%(idx%, "atari_a")
      Case "nes_b"     : ok% = Not menu.has_controller%(idx%, "atari_b")
      Case "nes_dx"    : ok% = Not menu.has_controller%(idx%, "atari_dx")
      Case "keys_cegg" : ok% = Not menu.has_controller%(idx%, "keys_azxc", "keys_punc")
      Case "keys_azxc" : ok% = Not menu.has_controller%(idx%, "keys_cegg")
      Case "keys_punc" : ok% = Not menu.has_controller%(idx%, "keys_cegg")
      Case Else        : ok% = 1
    End Select

    ' Check that we can OPEN the controller.
    If ok% Then ok% = menu.can_open_controller%(CTRL_DRIVER$(i%))
  Loop Until ok%

  menu.ctrls$(idx%) = CTRL_DRIVER$(i%)

  ' Restore UI controller.
  Call ui_ctrl$, ctrl.OPEN
End Sub

' Can we OPEN the given controller ?
Function menu.can_open_controller%(ctrl$)
  On Error Ignore
  Call ctrl$, ctrl.OPEN
  On Error Abort
  Local ok% = Mm.ErrNo = 0
  If ok% Then Call ctrl$, ctrl.CLOSE
  menu.can_open_controller% = ok%
End Function

' Is any player other than idx% using the specified controller ?
Function menu.has_controller%(idx%, ctrl1$, ctrl2$)
  Local i%
  For i% = Bound(menu.ctrls$(), 0) To Bound(menu.ctrls$(), 1)
    If i% = idx% Then Continue For
    If menu.ctrls$(i%) = ctrl1$ Then menu.has_controller% = 1
    If menu.ctrls$(i%) = ctrl2$ Then menu.has_controller% = 1
  Next
End Function
