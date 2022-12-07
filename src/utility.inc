' Copyright (c) 2020-22 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07.05

' Utility code lifted from 'splib'.

' Pads a string with spaces to the left and right so that it will be centred
' within a fixed length field. If the string is longer than the field then
' this function just returns the string. If an odd number of spaces are
' required then the extra space is added to the left hand side of the string.
'
' @param  s$  the string to centre.
' @param  x   the field length.
Function str.centre$(s$, x%)
  If Len(s$) < x% Then
    str.centre$ = s$ + Space$((x% - Len(s$)) \ 2)
    str.centre$ = Space$(x% - Len(str.centre$)) + str.centre$
  Else
    str.centre$ = s$
  EndIf
End Function

' Gets bit i% of x%.
Function bits.get%(x%, i%)
  If i% < 0 Or i% > 63 Then Error "i% out of 0 .. 63 range"
  bits.get% = (x% And 1 << i%) <> 0
End Function

Function str.lpad$(s$, x%)
  str.lpad$ = s$
  If Len(s$) < x% Then str.lpad$ = Space$(x% - Len(s$)) + s$
End Function

Function str.rpad$(s$, x%)
  str.rpad$ = s$
  If Len(s$) < x% Then str.rpad$ = s$ + Space$(x% - Len(s$))
End Function

' Returns a copy of s$ with leading and trailing spaces removed.
Function str.trim$(s$)
  Local st%, en%
  For st% = 1 To Len(s$)
    If Peek(Var s$, st%) <> 32 Then Exit For
  Next
  For en% = Len(s$) To 1 Step -1
    If Peek(Var s$, en%) <> 32 Then Exit For
  Next
  If en% >= st% Then str.trim$ = Mid$(s$, st%, en% - st% + 1)
End Function
