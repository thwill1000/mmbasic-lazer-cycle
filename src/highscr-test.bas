' Copyright (c) 2022-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.06

Option Base 0
Option Default None
Option Explicit On
' Option LcdPanel NoConsole

'!info defined PICOGAME_LCD
'!ifdef PICOGAME_LCD
'!define PICOMITE
'!define CTRL_USE_INKEY
'!define SOUND_USE_PWM
'!define CTRL_ONE_PLAYER
'!endif

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

#Include "splib/string.inc"
#Include "splib/ctrl.inc"
#Include "splib/sound.inc"
#Include "highscr.inc"

If sys.is_device%("cmm2*") Then
  Dim CTRLS_TO_POLL$(3) = ("atari_dx", "nes_dx", "wii_any_3", "keys_cursor")
ElseIf sys.is_device%("mmb4w") Then
  Dim CTRLS_TO_POLL$(1) = ("keys_cursor", "")
ElseIf sys.is_device%("pglcd") Then
  Dim CTRLS_TO_POLL$(1) = ("ctrl.gamemite", "keys_cursor")
ElseIf sys.is_device%("pmvga") Then
  Dim CTRLS_TO_POLL$(2) = ("atari_a", "nes_a", "keys_cursor")
Else
  Error "Unsupported device: " + Mm.Device$
EndIf

Const HIGHSCORE_FILENAME$ = highscr.get_directory$() + "/lazer-cycle.csv"

Dim ctrl$
Dim COLOURS%(3) = (Rgb(Red), Rgb(Yellow), Rgb(Cyan), Rgb(Green))
Dim player%

Mode 7
Page Write 1
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
