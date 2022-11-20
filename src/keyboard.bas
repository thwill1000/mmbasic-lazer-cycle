' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

' Utility for building scan code => key code map on PicoMiteVGA.

Option Base 0
Option Default None
Option Explicit On

#Include "keycode.inc"
#Include "scancode.inc"

Dim scan_map_uk%(31)
Dim key_map%(31)

read_scan_map_uk()
generate_scan_map_uk()
dump_scan_map_uk()
End
On Ps2 on_ps2()
Print "Press some keys:"
Do : Loop
End

Sub generate_scan_map_uk()
  ' Letters
  put_scan_code(Asc("a"), scancode.A)
  put_scan_code(Asc("b"), scancode.B)
  put_scan_code(Asc("c"), scancode.C)
  put_scan_code(Asc("d"), scancode.D)
  put_scan_code(Asc("e"), scancode.E)
  put_scan_code(Asc("f"), scancode.F)
  put_scan_code(Asc("g"), scancode.G)
  put_scan_code(Asc("h"), scancode.H)
  put_scan_code(Asc("i"), scancode.I)
  put_scan_code(Asc("j"), scancode.J)
  put_scan_code(Asc("k"), scancode.K)
  put_scan_code(Asc("l"), scancode.L)
  put_scan_code(Asc("m"), scancode.M)
  put_scan_code(Asc("n"), scancode.N)
  put_scan_code(Asc("o"), scancode.O)
  put_scan_code(Asc("p"), scancode.P)
  put_scan_code(Asc("q"), scancode.Q)
  put_scan_code(Asc("r"), scancode.R)
  put_scan_code(Asc("s"), scancode.S)
  put_scan_code(Asc("t"), scancode.T)
  put_scan_code(Asc("u"), scancode.U)
  put_scan_code(Asc("v"), scancode.V)
  put_scan_code(Asc("w"), scancode.W)
  put_scan_code(Asc("x"), scancode.X)
  put_scan_code(Asc("y"), scancode.Y)
  put_scan_code(Asc("z"), scancode.Z)

  ' Numbers
  put_scan_code(Asc("1"), scancode.1)
  put_scan_code(Asc("2"), scancode.2)
  put_scan_code(Asc("3"), scancode.3)
  put_scan_code(Asc("4"), scancode.4)
  put_scan_code(Asc("5"), scancode.5)
  put_scan_code(Asc("6"), scancode.6)
  put_scan_code(Asc("7"), scancode.7)
  put_scan_code(Asc("8"), scancode.8)
  put_scan_code(Asc("9"), scancode.9)
  put_scan_code(Asc("0"), scancode.0)

  ' Keypad
  put_scan_code(Asc("1"), scancode.KEYPAD_1)
  put_scan_code(Asc("2"), scancode.KEYPAD_2)
  put_scan_code(Asc("3"), scancode.KEYPAD_3)
  put_scan_code(Asc("4"), scancode.KEYPAD_4)
  put_scan_code(Asc("5"), scancode.KEYPAD_5)
  put_scan_code(Asc("6"), scancode.KEYPAD_6)
  put_scan_code(Asc("7"), scancode.KEYPAD_7)
  put_scan_code(Asc("8"), scancode.KEYPAD_8)
  put_scan_code(Asc("9"), scancode.KEYPAD_9)
  put_scan_code(Asc("0"), scancode.KEYPAD_0)
  put_scan_code(Asc("*"), scancode.KEYPAD_ASTERISK)
  put_scan_code(keycode.ENTER, scancode.KEYPAD_ENTER)
  put_scan_code(Asc("-"), scancode.KEYPAD_HYPHEN)
  put_scan_code(Asc("."), scancode.KEYPAD_PERIOD)
  put_scan_code(Asc("/"), scancode.KEYPAD_SLASH)
  put_scan_code(Asc("+"), scancode.KEYPAD_PLUS)

  ' Punctuation
  put_scan_code(Asc(";"), scancode.SEMICOLON)
  put_scan_code(Asc("'"), scancode.SINGLE_QUOTE)
  put_scan_code(Asc(","), scancode.COMMA)
  put_scan_code(Asc("."), scancode.PERIOD)
  put_scan_code(Asc("/"), scancode.SLASH)
  put_scan_code(Asc("`"), scancode.BACKTICK)
  put_scan_code(Asc(" "), scancode.SPACE)
  put_scan_code(Asc("-"), scancode.HYPHEN)
  put_scan_code(Asc("="), scancode.EQUALS)
  put_scan_code(Asc("["), scancode.SQUARE_BRA)
  put_scan_code(Asc("]"), scancode.SQUARE_KET)
  put_scan_code(Asc("#"), scancode.HASH)
  put_scan_code(Asc("\"), scancode.BACKSLASH)

  ' Function keys
  put_scan_code(keycode.F1,  scancode.F1)
  put_scan_code(keycode.F2,  scancode.F2)
  put_scan_code(keycode.F3,  scancode.F3)
  put_scan_code(keycode.F4,  scancode.F4)
  put_scan_code(keycode.F5,  scancode.F5)
  put_scan_code(keycode.F6,  scancode.F6)
  put_scan_code(keycode.F7,  scancode.F7)
  put_scan_code(keycode.F8,  scancode.F8)
  put_scan_code(keycode.F9,  scancode.F9)
  put_scan_code(keycode.F10, scancode.F10)
  put_scan_code(keycode.F11, scancode.F11)
  put_scan_code(keycode.F12, scancode.F12)

  ' Special keys
  put_scan_code(keycode.ENTER,        scancode.ENTER)
  put_scan_code(keycode.ESCAPE,       scancode.ESCAPE)
  put_scan_code(keycode.ALT,          scancode.ALT_LEFT)
  put_scan_code(keycode.ALT,          scancode.ALT_RIGHT)
  put_scan_code(keycode.PRINT_SCREEN, scancode.PRINT_SCREEN)
  put_scan_code(keycode.INSERT,       scancode.INSERT)
  put_scan_code(keycode.HOME,         scancode.HOME)
  put_scan_code(keycode.PAGE_UP,      scancode.PAGE_UP)
  put_scan_code(keycode.DELETE,       scancode.DELETE)
  put_scan_code(keycode.BACKSPACE,    scancode.BACKSPACE)
  put_scan_code(keycode.END,          scancode.END)
  put_scan_code(keycode.TAB,          scancode.TAB)
  put_scan_code(keycode.PAGE_DOWN,    scancode.PAGE_DOWN)
  put_scan_code(keycode.UP,           scancode.UP)
  put_scan_code(keycode.LEFT,         scancode.LEFT)
  put_scan_code(keycode.DOWN,         scancode.DOWN)
  put_scan_code(keycode.RIGHT,        scancode.RIGHT)
  'put_scan_code(???, scancode.SHIFT_LEFT)
  'put_scan_code(???, scancode.SHIFT_RIGHT)
  'put_scan_code(???, scancode.SCROLL_LOCK)
  'put_scan_code(???, scancode.CTRL_LEFT)
  'put_scan_code(???, scancode.WIN_LEFT)
  'put_scan_code(???, scancode.WIN_RIGHT)
  'put_scan_code(???, scancode.MENUS)
  'put_scan_code(???, scancode.CTRL_RIGHT)
  'put_scan_code(???, scancode.NUM_LOCK)
  'put_scan_code(???, scancode.CAPS_LOCK)
  'put_scan_code(???, scancode.KEYPAD_ENTER)
  'put_scan_code(???, scancode.PAUSE_BREAK)
End Sub

Sub put_scan_code(key_code%, scan_code%)
  If key_code% > 255 Then Error "Invalid key code: " + Hex$(key_code%)

  Local offset%
  Select Case scan_code%
    Case Is < &hE000
      offset% = scan_code% And &hFF
    Case Is < &hF000
      offset% = (scan_code% And &hFF) + 128
    Case Else
      Error "Unexpected scan code: " + Hex$(scan_code%)
  End Select

  If offset% \ 8 > Bound(scan_map_uk%(), 1) Then Error "Out of bounds"
  Poke Var scan_map_uk%(), offset%, key_code%
End Sub

Sub dump_scan_map_uk
  Local i%
  For i% = Bound(scan_map_uk%(), 0) To Bound(scan_map_uk%(), 1)
    If i% Mod 4 = 0 Then
      Print : Print "Data ";
    Else
      Print ", ";
    EndIf
    Print "&h" Hex$(scan_map_uk%(i%), 16);
  Next
  Print
End Sub

Sub read_scan_map_uk()
  Read Save
  Restore scan_map_uk_data
  Local i%
  For i% = Bound(scan_map_uk%(), 0) To Bound(scan_map_uk%(), 1)
    Read scan_map_uk%(i%)
  Next
  Read Restore
End Sub

Sub on_ps2()
  Local ch%, down%, scan_code% = Mm.Info(PS2), offset% = scan_code% And &hFF
  Select Case scan_code%
    Case Is < &hE000
      down% = 1
    Case Is < &hF000
      Inc offset%, 128
      down% = 1
    Case Is < &hE0F000
      down% = 0
    Case Else
      Inc offset%, 128
      down% = 0
  End Select

  ch% = Peek(Var scan_map_uk%(), offset%)
  Poke Var key_map%(), ch%, down%

  Print "<0x" Hex$(scan_code%, 6) "> => <0x" Hex$(ch%, 2) "> => ";
  Print keycode_to_string$(ch%) " " Choice(down%, "[DOWN]", "[UP]")
  dump_keys_down()
End Sub

Function keycode_to_string$(ch%)
  Static initialised% = 0
  Static to_string$(255) Length 12
  Local i%
  If Not initialised% Then
    Read Save
    Restore keycode.string_data
    For i% = 0 To 255
      Read to_string$(i%)
    Next
    Read Restore
    to_string$(34) = Chr$(34) ' Can't put double-quote in DATA statement.
    initialised% = 1
  End If
  keycode_to_string$ = to_string$(ch%)
End Function

Sub dump_keys_down()
  Local ch%, count%
  Print "Keys down: ";
  For ch% = 0 To 255
    If Peek(Var key_map%(), ch%) Then
      If count% > 0 Then Print ", ";
      Print keycode_to_string$(ch%) " ";
      Inc count%
    EndIf
  Next ch%
  Print Choice(count% = 0, "<None>", "")
End Sub

scan_map_uk_data:

Data &h9C92919395009900, &h0060099496989A00, &h0031710000008B00, &h00327761737A0000
Data &h0033346564786300, &h0035727466762000, &h0036796768626E00, &h003837756A6D0000
Data &h0039306F696B2C00, &h002D703B6C2F2E00, &h00003D5B00270000, &h000023005D0A0000
Data &h0008000000005C00, &h0000003734003100, &h001B383635322E30, &h0000392A2D332B9B
Data &h0000000097000000, &h0000000000000000, &h0000000000008B00, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000008682008700, &h0000808300817F84, &h0000889D00890000
