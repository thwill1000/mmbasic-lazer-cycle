' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

' Shows the game settings menu.
'
' @param[in]      ui_ctrl$    controller driver to use to navigate the menu.
' @param[in,out]  ctrls$()    on entry the current controller driver per player,
'                             on exit the new controller driver per player.
' @param[in]      colours%()  the player colours
' @return                     1 to continue, or 0 to QUIT.
Function menu.show%(ui_ctrl$, ctrls$(), colours%())
  Const x% = X_OFFSET% - 100
  Static initialised% = 0
  Local key%, i%, item% = 0, update% = 1
  Local sounds$(3) = ("MUSIC & FX", "MUSIC ONLY", "FX ONLY   ", "NONE      ")
  Local sound_setting% = 3 - sound.get_state%()

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

  ' Text X_OFFSET%, Y_OFFSET% + 75, "Music by Scott Joplin", "CM", 7, 1, Rgb(Cyan)
  Text X_OFFSET%, Y_OFFSET% + 90, "Game Version " + VERSION$, "CM", 7, 1, Rgb(Cyan)

  ctrl.init_keys()
  On Error Ignore
  Call ui_ctrl$, ctrl.OPEN
  On Error Abort

  Do

    If update% Then
      Text x%, Y_OFFSET% - 95, "START GAME", , 1, 1, Rgb(White)
      menu.txt_for_controller(x%, Y_OFFSET% - 75, 0, colours%(0))
      menu.txt_for_controller(x%, Y_OFFSET% - 55, 1, colours%(1))
      menu.txt_for_controller(x%, Y_OFFSET% - 35, 2, colours%(2))
      menu.txt_for_controller(x%, Y_OFFSET% - 15, 3, colours%(3))
      Text x%, Y_OFFSET% + 5,  "DIFFICULTY: " + Str$(difficulty%), , 1, 1, Rgb(White)
      Text x%, Y_OFFSET% + 25, "SOUND:      " + sounds$(sound_setting%), , 1, 1, Rgb(White)
      Text x%, Y_OFFSET% + 45, "QUIT", , 1, 1, Rgb(White)
      Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Cyan)
      If Not InStr(Mm.Device$, "PicoMite") Then Page Copy 1 To 0, B
      Pause 200
      update% = 0
    EndIf

    Call ui_ctrl$, key%
    If key% = 0 Then keys_cursor(key%) ' Always respond to the cursor keys.
    Select Case key%
      Case ctrl.START
        Exit Do

      Case ctrl.UP
        If item% > 0 Then
          Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Black)
          Inc item%, -1
          update% = 1
        EndIf

      Case ctrl.DOWN
        If item% < 7 Then
          Text x% - 15, Y_OFFSET% - 95 + item% * 20, Chr$(137), , 1, 1, Rgb(Black)
          Inc item%
          update% = 1
        EndIf

      Case ctrl.LEFT, ctrl.RIGHT, ctrl.A
        Select Case item%
          Case 0
            If key% = ctrl.A Then menu.show% = 1 : Exit Do

          Case 1,2,3,4
            menu.update_controller(item% - 1, Choice(key% = ctrl.LEFT, -1, +1))
            update% = 1

          Case 5 ' Difficulty
            Inc difficulty%, Choice(key% = ctrl.LEFT, -1, 1)
            If difficulty% < 1 Then difficulty% = 5
            If difficulty% > 5 Then difficulty% = 1
            update% = 1

          Case 6 ' Sound
            Inc sound_setting%, Choice(key% = ctrl.LEFT, -1, 1)
            If sound_setting% < 0 Then sound_setting% = 3
            If sound_setting% > 3 Then sound_setting% = 0
            If sound_setting% And &b10 Then
              sound.start_music(sound.NO_MUSIC%())
            Else
              sound.start_music(sound.MUSIC%())
            EndIf
            sound.enable_fx(Not (sound_setting% And &b01))
            update% = 1

          Case 7 ' Quit
            If key% = ctrl.A Then Exit Do

        End Select

    End Select

    If update% = 1 Then sound.start_fx(sound.FX_SELECT%(), 1)

  Loop

  close_controller_no_error(ui_ctrl$)

  ' Copy controller settings back into parameter.
  For i% = Bound(ctrls$(), 0) To Bound(ctrls$(), 1)
    ctrls$(i%) = menu.ctrls$(i%)
  Next
End Function

Sub menu.txt_for_controller(x%, y%, idx%, colour%)
  Local txt$ = "PLAYER " + Str$(idx% + 1) + ":   " + menu.get_controller_name$(idx%)
  Text x%, y%, txt$, , 1, 1, colour%
End Sub

Function menu.get_controller_name$(idx%)
  Local i%
  For i% = Bound(CTRL_SUBS$(), 0) To Bound(CTRL_SUBS$(), 1)
    If menu.ctrls$(idx%) = CTRL_SUBS$(i%) Then
      menu.get_controller_name$ = CTRL_NAMES$(i%)
      Do While Len(menu.get_controller_name$) < 13 : Cat menu.get_controller_name$, " " : Loop
      Exit Function
    Endif
  Next
  Error "Unknown controller: " + menu.ctrls$(idx%)
End Function

Sub menu.update_controller(idx%, delta%)
  ' Find index of currently selected controller.
  Local i%
  For i% = Bound(CTRL_SUBS$(), 0) To Bound(CTRL_SUBS$(), 1)
    If menu.ctrls$(idx%) = CTRL_SUBS$(i%) Then Exit For
  Next
  If i% = Bound(CTRL_SUBS$(), 1) + 1 Then Error "Unknown controller: " + menu.ctrls$(idx%)

  Local ok%
  Do
    Inc i%, delta%
    If i% < 0 Then i% = Bound(CTRL_SUBS$(), 1)
    If i% > Bound(CTRL_SUBS$(), 1) Then i% = 0

    ' Check there is no conflict with other player's controller choice.
    If Not InStr("ai_control|no_control", CTRL_SUBS$(i%)) Then
      If menu.has_controller%(idx%, CTRL_SUBS$(i%)) Then Continue Do
    EndIf
    Select Case CTRL_SUBS$(i%)
      Case "atari_a"   : ok% = Not menu.has_controller%(idx%, "nes_a")
      Case "atari_b"   : ok% = Not menu.has_controller%(idx%, "nes_b")
      Case "nes_a"     : ok% = Not menu.has_controller%(idx%, "atari_a")
      Case "nes_b"     : ok% = Not menu.has_controller%(idx%, "atari_b")
      Case "keys_cegg" : ok% = Not menu.has_controller%(idx%, "keys_azxc")
      Case "keys_azxc" : ok% = Not menu.has_controller%(idx%, "keys_cegg")
      Case Else        : ok% = 1
    End Select
  Loop Until ok%

  menu.ctrls$(idx%) = CTRL_SUBS$(i%)
End Sub

' Is any player other than idx% using the specified controller ?
Function menu.has_controller%(idx%, ctrl$)
  Local i%
  For i% = Bound(menu.ctrls$(), 0) To Bound(menu.ctrls$(), 1)
    If i% = idx% Then Continue For
    If menu.ctrls$(i%) = ctrl$ Then menu.has_controller% = 1
  Next
End Function