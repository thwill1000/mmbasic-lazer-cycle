' Code Copyright (c) 2022-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.06

Option Base 0
Option Default None
Option Explicit On
' Option LcdPanel NoConsole

'!ifdef PICOGAME_LCD
' Preprocessor flag PICOGAME_LCD defined
'!set PICOMITE
'!set CTRL_USE_INKEY
'!set SOUND_USE_PWM
'!set CTRL_ONE_PLAYER
'!endif

#Include "ctrl.inc"
#Include "utility.inc"
#Include "highscr.inc"
#Include "sound.inc"

Select Case Mm.Device$
  Case "Colour Maximite 2", "Colour Maximite 2 G2"
    Const USE_PATH% = 1
    Const USE_MODE% = 7
    Const USE_PAGE_COPY% = 1
    Dim CTRLS_TO_POLL$(3) = ("atari_dx", "nes_dx", "wii_any_3", "keys_cursor")
  Case "MMBasic for Windows"
    Const USE_PATH% = 1
    Const USE_MODE% = 7
    Const USE_PAGE_COPY% = 1
    Dim CTRLS_TO_POLL$(1) = ("keys_cursor", "")
  Case "PicoMite"
    Const USE_PATH% = 0
    Const USE_MODE% = 0
    Const USE_PAGE_COPY% = 0
    Dim CTRLS_TO_POLL$(1) = ("nes_a", "keys_cursor")
  Case "PicoMiteVGA"
    Const USE_PATH% = 0
    Const USE_MODE% = 2
    Const USE_PAGE_COPY% = 0
    Dim CTRLS_TO_POLL$(2) = ("atari_a", "nes_a", "keys_cursor")
  Case Else
    Error "Unsupported device: " + Mm.Device$
End Select

If USE_PATH% Then
  Const HIGHSCORE_FILENAME$ = Mm.Info(Path) + "high-scores/test.csv"
Else
  Const HIGHSCORE_FILENAME$ = "/high-scores/test.csv"
EndIf

Dim ctrl$
Dim COLOURS%(3) = (Rgb(Red), Rgb(Yellow), Rgb(Cyan), Rgb(Green))
Dim player%

If USE_MODE% Then Mode USE_MODE%
If USE_PAGE_COPY% Then Page Write 1
Cls

sound.init()
highscr.init(HIGHSCORE_FILENAME$, "highscore_data")

Do
  ctrl$ = highscr.show_table$(CTRLS_TO_POLL$(), 0)
  Cls
  player% = Int(4 * Rnd())
  highscr.edit(player% + 1, Int(10 * Rnd()), COLOURS%(player%), ctrl$)
  Cls
Loop

End

highscore_data:

Data "TOM, 2000"
Data "MICKEY, 1500"
Data "MIKE, 1250"
Data "PETER, 1000"
Data "DAVEY, 800"
Data "JOHN, 600"
Data "PAUL, 400"
Data "GEORGE, 200"
Data "RINGO, 100"
Data "MOOSE, 50"
