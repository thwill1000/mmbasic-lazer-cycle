' Copyright (c) 2022 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

' Utility for building scan code => key code map on PicoMiteVGA.

Option Base 0
Option Default None
Option Explicit On

Dim map_00%(31)
Dim map_e0%(31)
Dim key_map%(31)

read_scan_code_map()
'generate_scan_code_map()
'dump_scan_code_map()
On Ps2 on_ps2()
Print "Press some keys:"
Do : Loop
End

Sub generate_scan_code_map()
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
  put_scan_code(&h91, &h05) ' F1
  put_scan_code(&h92, &h06) ' F2
  put_scan_code(&h93, &h04) ' F3
  put_scan_code(&h94, &h0C) ' F4
  put_scan_code(&h95, &h03) ' F5
  put_scan_code(&h96, &h0B) ' F6
  put_scan_code(&h97, &h83) ' F7
  put_scan_code(&h98, &h0A) ' F8
  put_scan_code(&h99, &h01) ' F9
  put_scan_code(&h9A, &h09) ' F10
  put_scan_code(&h9B, &h78) ' F11
  put_scan_code(&h9C, &h07) ' F12

  ' Special keys
  put_scan_code(&h0A, &h5A)   ' Enter - or should it be => &h0D ?
  put_scan_code(&h1B, &h76)   ' Escape
  put_scan_code(&h8B, &h11)   ' Alt (Left)
  put_scan_code(&h8B, &hE011) ' Alt (Right)
  put_scan_code(&h9D, &hE07C) ' Prt Scr
  put_scan_code(&h84, &hE070) ' Insert
  put_scan_code(&h86, &hE06C) ' Home
  put_scan_code(&h88, &hE07D) ' Page Up
  put_scan_code(&h7F, &hE071) ' Delete
  put_scan_code(&h08, &h66)   ' Backspace
  put_scan_code(&h87, &hE069) ' End
  put_scan_code(&h09, &h0D)   ' Tab
  put_scan_code(&h89, &hE07A) ' Page Down
  put_scan_code(&h80, &hE075) ' Up Arrow
  put_scan_code(&h82, &hE06B) ' Left Arrow
  put_scan_code(&h81, &hE072) ' Down Arrow
  put_scan_code(&h83, &hE074) ' Right Arrow
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
  If (scan_code% And &hE000) = &hE000 Then
'    If Peek(Var map_e0%(), scan_code%) <> &h00 Then Error "Duplicate mapping: " + Hex$(scan_code%)
    Poke Var map_e0%(), scan_code% And &hFF, key_code%
  Else If (scan_code% And &hFF00) = 0 Then
'    If Peek(Var map_00%(), scan_code%) <> &h00 Then Error "Duplicate mapping: " + Hex$(scan_code%)
    Poke Var map_00%(), scan_code% And &hFF, key_code%
  Else
    Error "Unexpected scan code: " + Hex$(scan_code%)
  End If
End Sub

Sub dump_scan_code_map
  Local i%
  For i% = 0 To 31
    If i% Mod 4 = 0 Then
      Print : Print "Data ";
    Else
      Print ", ";
    EndIf
    Print "&h" Hex$(map_00%(i%), 16);
  Next
  Print
  For i% = 0 To 31
    If i% Mod 4 = 0 Then
      Print : Print "Data ";
    Else
      Print ", ";
    EndIf
    Print "&h" Hex$(map_e0%(i%), 16);
  Next
  Print
End Sub

Sub read_scan_code_map()
  Read Save
  Restore scan_uk_data
  Local i%
  For i% = 0 To 31
    Read map_00%(i%)
  Next
  For i% = 0 To 31
    Read map_e0%(i%)
  Next
  Read Restore
End Sub

Sub on_ps2()
  Local ch%, down%, scan_code% = Mm.Info(PS2)
  Select Case scan_code%
    Case Is < &hE000
      ch% = Peek(Var map_00%(), scan_code% And &hFF) : down% = 1
    Case Is < &hF000
      ch% = Peek(Var map_e0%(), scan_code% And &hFF) : down% = 1
    Case Is < &hE0F000
      ch% = Peek(Var map_00%(), scan_code% And &hFF)
    Case Else
      ch% = Peek(Var map_e0%(), scan_code% And &hFF)
  End Select
  Poke Var key_map%(), ch%, down%

  Print "<0x" Hex$(scan_code%, 6) "> => <0x" Hex$(ch%, 2) "> => " nice$(ch%)
  dump_keys_down()
End Sub

Function nice$(ch%)
  Select Case ch%
    Case &h21 To &h7E : nice$ = Chr$(ch%)
    Case &h09 : nice$ = "Backspace"
    Case &h09 : nice$ = "Tab"
    Case &h0A : nice$ = "Enter"
    Case &h1B : nice$ = "Escape"
    Case &h20 : nice$ = "Space"
    Case &h7F : nice$ = "Delete"
    Case &h80 : nice$ = "Up"
    Case &h81 : nice$ = "Down"
    Case &h82 : nice$ = "Left"
    Case &h83 : nice$ = "Right"
    Case &h84 : nice$ = "Insert"
    Case &h86 : nice$ = "Home"
    Case &h87 : nice$ = "End"
    Case &h88 : nice$ = "Page Up"
    Case &h89 : nice$ = "Page Down"
    Case &h8B : nice$ = "Alt"
    Case &h91 : nice$ = "F1"
    Case &h92 : nice$ = "F2"
    Case &h93 : nice$ = "F3"
    Case &h94 : nice$ = "F4"
    Case &h95 : nice$ = "F5"
    Case &h96 : nice$ = "F6"
    Case &h97 : nice$ = "F7"
    Case &h98 : nice$ = "F8"
    Case &h99 : nice$ = "F9"
    Case &h9A : nice$ = "F10"
    Case &h9B : nice$ = "F11"
    Case &h9C : nice$ = "F12"
    Case &h9D : nice$ = "Prt Scr"
    Case Else : nice$ = "<unknown>"
  End Select
End Function

Sub dump_keys_down()
  Local ch%, count%
  Print "Keys down: ";
  For ch% = 0 To 255
    If Peek(Var key_map%(), ch%) Then
      If count% > 0 Then Print ", ";
      Print nice$(ch%) " ";
      Inc count%
    EndIf
  Next ch%
  Print Choice(count% = 0, "<None>", "")
End Sub

scan_uk_data:

Data &h9C92919395009900, &h0060099496989A00, &h0031710000008B00, &h00327761737A0000
Data &h0033346564786300, &h0035727466762000, &h0036796768626E00, &h003837756A6D0000
Data &h0039306F696B2C00, &h002D703B6C2F2E00, &h00003D5B00270000, &h000023005D0A0000
Data &h0008000000005C00, &h0000003734003100, &h001B383635322E30, &h0000392A2D332B9B
Data &h0000000097000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000

Data &h0000000000000000, &h0000000000000000, &h0000000000008B00, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000008682008700, &h0000808300817F84, &h0000889D00890000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000

