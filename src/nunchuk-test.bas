' Copyright (c) 2022-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For CMM2 running MMBasic 5.07.02b6

Option Base 0
Option Default None
Option Explicit On

#Include "ctrl.inc"

Mode 1
Cls

Option Break 4
On Key 3, on_break

Dim bits%
Dim driver$(3) = ("", "wii_any_1", "wii_any_2", "wii_any_3")
'Dim driver$(3) = ("", "wii_classic_1", "wii_classic_2", "wii_classic_3")
'Dim driver$(3) = ("", "wii_nunchuk_1", "wii_nunchuk_2", "wii_nunchuk_3")
Dim err$(3)
Dim i%
Dim out$(3)

Print "Nunchuck Controller Test"

For i% = 1 To 3
  On Error Ignore
  Call driver$(i%), ctrl.OPEN
  If Mm.ErrNo <> 0 Then err$(i%) = Mid$(Mm.ErrMsg$, InStr(Mm.ErrMsg$, ":") + 2)
  out$(i%) = err$(i%)
  On Error Abort
Next

Do
  Print @(0, 20) "I2C1: " + rpad$(out$(1), 40)
  Print "I2C2: " + rpad$(out$(2), 40)
  Print "I2C3: " + rpad$(out$(3), 40)

  For i% = 1 To 3
    If err$(i%) = "" Then
      Call driver$(i%), bits%
      out$(i%) = ctrl.bits_to_string$(bits%)
    EndIf
  Next
Loop

end_program()

Sub on_break()
  Option Break 3
  end_program()
End Sub

Sub end_program()
  For i% = 1 To 3
    Call driver$(i%), ctrl.CLOSE
  Next
  End
End SUb

' Gets a string padded to a given width with spaces to the right.
'
' @param s$  the string.
' @param w%  the width.
' @return    the padded string.
'            If Len(s$) > w% then returns the unpadded string.
Function rpad$(s$, w%)
  rpad$ = s$
  If Len(s$) < w% Then rpad$ = s$ + Space$(w% - Len(s$))
End Function
