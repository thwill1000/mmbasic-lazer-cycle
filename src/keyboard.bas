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
'generate_scan_map_uk()
'dump_scan_map_uk()
On Ps2 on_ps2()
Print "Press some keys:"
Do : Loop
End

Sub generate_scan_map_uk()
  ' Letters
  put_scan_code(Asc("a"), &h1C)
  put_scan_code(Asc("b"), &h32)
  put_scan_code(Asc("c"), &h21)
  put_scan_code(Asc("d"), &h23)
  put_scan_code(Asc("e"), &h24)
  put_scan_code(Asc("f"), &h2B)
  put_scan_code(Asc("g"), &h34)
  put_scan_code(Asc("h"), &h33)
  put_scan_code(Asc("i"), &h43)
  put_scan_code(Asc("j"), &h3B)
  put_scan_code(Asc("k"), &h42)
  put_scan_code(Asc("l"), &h4B)
  put_scan_code(Asc("m"), &h3A)
  put_scan_code(Asc("n"), &h31)
  put_scan_code(Asc("o"), &h44)
  put_scan_code(Asc("p"), &h4D)
  put_scan_code(Asc("q"), &h15)
  put_scan_code(Asc("r"), &h2D)
  put_scan_code(Asc("s"), &h1B)
  put_scan_code(Asc("t"), &h2C)
  put_scan_code(Asc("u"), &h3C)
  put_scan_code(Asc("v"), &h2A)
  put_scan_code(Asc("w"), &h1D)
  put_scan_code(Asc("x"), &h22)
  put_scan_code(Asc("y"), &h35)
  put_scan_code(Asc("z"), &h1A)

  ' Numbers
  put_scan_code(Asc("1"), &h16)
  put_scan_code(Asc("2"), &h1E)
  put_scan_code(Asc("3"), &h26)
  put_scan_code(Asc("4"), &h25)
  put_scan_code(Asc("5"), &h2E)
  put_scan_code(Asc("6"), &h36)
  put_scan_code(Asc("7"), &h3D)
  put_scan_code(Asc("8"), &h3E)
  put_scan_code(Asc("9"), &h46)
  put_scan_code(Asc("0"), &h45)

  ' Number pad
  put_scan_code(Asc("1"), &h69)
  put_scan_code(Asc("2"), &h72)
  put_scan_code(Asc("3"), &h7A)
  put_scan_code(Asc("4"), &h6B)
  put_scan_code(Asc("5"), &h73)
  put_scan_code(Asc("6"), &h74)
  put_scan_code(Asc("7"), &h6C)
  put_scan_code(Asc("8"), &h75)
  put_scan_code(Asc("9"), &h7D)
  put_scan_code(Asc("0"), &h70)

  ' Punctuation
  put_scan_code(Asc(";"), &h4C)
  put_scan_code(Asc("'"), &h52)
  put_scan_code(Asc(","), &h41)
  put_scan_code(Asc("."), &h49)
  put_scan_code(Asc("/"), &h4A)
  put_scan_code(Asc("`"), &h0E)
  put_scan_code(Asc(" "), &h29)
  put_scan_code(Asc("-"), &h4E)
  put_scan_code(Asc("="), &h55)
  put_scan_code(Asc("*"), &h7C)
  put_scan_code(Asc("-"), &h7B)
  put_scan_code(Asc("["), &h54)
  put_scan_code(Asc("]"), &h5B)
  put_scan_code(Asc("+"), &h79)
  put_scan_code(Asc("#"), &h5D)
  put_scan_code(Asc("."), &h71) ' Number pad
  put_scan_code(Asc("\"), &h61)

  ' Function keys
  put_scan_code(keycode.F1,  &h05)
  put_scan_code(keycode.F2,  &h06)
  put_scan_code(keycode.F3,  &h04)
  put_scan_code(keycode.F4,  &h0C)
  put_scan_code(keycode.F5,  &h03)
  put_scan_code(keycode.F6,  &h0B)
  put_scan_code(keycode.F7,  &h83)
  put_scan_code(keycode.F8,  &h0A)
  put_scan_code(keycode.F9,  &h01)
  put_scan_code(keycode.F10, &h09)
  put_scan_code(keycode.F11, &h78)
  put_scan_code(keycode.F12, &h07)

  ' Special keys
  put_scan_code(keycode.ENTER,        &h5A)   ' Enter - or should it be => &h0D ?
  put_scan_code(keycode.ESCAPE,       &h76)   ' Escape
  put_scan_code(keycode.ALT,          &h11)   ' Alt (Left)
  put_scan_code(keycode.ALT,          &hE011) ' Alt (Right)
  put_scan_code(keycode.PRINT_SCREEN, &hE07C) ' Prt Scr
  put_scan_code(keycode.INSERT,       &hE070) ' Insert
  put_scan_code(keycode.HOME,         &hE06C) ' Home
  put_scan_code(keycode.PAGE_UP,      &hE07D) ' Page Up
  put_scan_code(keycode.DELETE,       &hE071) ' Delete
  put_scan_code(keycode.BACKSPACE,    &h66)   ' Backspace
  put_scan_code(keycode.END,          &hE069) ' End
  put_scan_code(keycode.TAB,          &h0D)   ' Tab
  put_scan_code(keycode.PAGE_DOWN,    &hE07A) ' Page Down
  put_scan_code(keycode.UP,           &hE075) ' Up Arrow
  put_scan_code(keycode.LEFT,         &hE06B) ' Left Arrow
  put_scan_code(keycode.DOWN,         &hE072) ' Down Arrow
  put_scan_code(keycode.RIGHT,        &hE074) ' Right Arrow
  'put_scan_code(???, &h12)   ' Shift (Left)
  'put_scan_code(???, &h59)   ' Shift (Right)
  'put_scan_code(???, &h7E)   ' Scroll Lock
  'put_scan_code(???, &h14)   ' Ctrl (Left)
  'put_scan_code(???, &hE01F) ' Windows (Left)
  'put_scan_code(???, &hE027) ' Windows (Right)
  'put_scan_code(???, &hE02F) ' Menus
  'put_scan_code(???, &hE014) ' Ctrl (Right)
  'put_scan_code(???, &h77)   ' Num lock
  'put_scan_code(???, &h58)   ' Caps Lock
  'put_scan_code(???, &hE05A) ' Enter - Number pad ?
  'put_scan_code(???, &hE11477E1F014E077) ' Pause/Break
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
