Sub Getvalues(CurrentWb As Workbook, Source_workbook As Workbook, i As Long)

CurrentWb.Sheets("Лист1").Range("B2").Value = Source_workbook.Sheets("Журнал РНС").Range("U" & i)
 CurrentWb.Sheets("Лист1").Range("B6").Value = Mid((Source_workbook.Sheets("Журнал РНС").Range("U" & i)), Len(Source_workbook.Sheets("Журнал РНС").Range("U" & i)) - 11, 2)
 CurrentWb.Sheets("Лист1").Range("F6").Value = Right((Source_workbook.Sheets("Журнал РНС").Range("U" & i)), 10)
 If IsNumeric(CurrentWb.Sheets("Лист1").Range("B6").Value) Or IsNumeric(CurrentWb.Sheets("Лист1").Range("F6").Value) Then
 Else
 CurrentWb.Sheets("Лист1").Range("B6").Value = "некорр. МСР"
 CurrentWb.Sheets("Лист1").Range("F6").Value = "некорр. МСР"
 End If
 
 CurrentWb.Sheets("Лист1").Range("B3").Value = Source_workbook.Sheets("Журнал РНС").Range("O" & i)
 
 CurrentWb.Sheets("Лист1").Range("B4").Value = Source_workbook.Sheets("Журнал РНС").Range("F" & i)
 'CurrentWb.Sheets("Лист1").Range("B5").Value = Source_workbook.Sheets("Журнал РНС").Range("G" & i)
 
 'CurrentWb.Sheets("Лист1").Range("D3").Value = Source_workbook.Sheets("Журнал РНС").Range("D" & i)
 CurrentWb.Sheets("Лист1").Range("B5").Value = Source_workbook.Sheets("Журнал РНС").Range("K" & i) 'гестация
 CurrentWb.Sheets("Лист1").Range("F4").Value = Source_workbook.Sheets("Журнал РНС").Range("S" & i)
 CurrentWb.Sheets("Лист1").Range("F5").Value = Source_workbook.Sheets("Журнал РНС").Range("T" & i)

If typeofprotocol = "полный_TMS+PCR" Then
CurrentWb.Sheets("Лист1").Range("A8").Value = "Количественное опредение аминокислот, сукценилацетона, свободного карнитина, ацилкарнитинов " & _
"методом тандемной масс-спектрометрии (ТМС) и количественное определение ДНК TREC, КRЕС и качественное выявление гомозиготной делеции 7 экзона гена SMN1 " & _
"методом полимеразной цепной реакции «ТК-SMA»"

ElseIf typeofprotocol = "TMS" Then
CurrentWb.Sheets("Лист1").Range("A8").Value = "Количественное опредение аминокислот, сукценилацетона, свободного карнитина, ацилкарнитинов " & _
"методом тандемной масс-спектрометрии (ТМС)"

ElseIf typeofprotocol = "расшир._TMS" Then
CurrentWb.Sheets("Лист1").Range("A8").Value = "Количественное опредение аминокислот, сукценилацетона, свободного карнитина, ацилкарнитинов " & _
"методом тандемной масс-спектрометрии (ТМС)"

ElseIf typeofprotocol = "TMS+PCR" Then
CurrentWb.Sheets("Лист1").Range("A8").Value = "Количественное опредение аминокислот, сукценилацетона, свободного карнитина, ацилкарнитинов " & _
"методом тандемной масс-спектрометрии (ТМС) и количественное определение ДНК TREC, КRЕС и качественное выявление гомозиготной делеции 7 экзона гена SMN1 " & _
"методом полимеразной цепной реакции «ТК-SMA»"

ElseIf typeofprotocol = "PCR" Then
CurrentWb.Sheets("Лист1").Range("A8").Value = "Количественное определение ДНК TREC, КRЕС и качественное выявление гомозиготной делеции 7 экзона гена SMN1 " & _
"методом полимеразной цепной реакции «ТК-SMA»"
End If
 
' CurrentWb.Sheets("Лист1").Range("B12").Value = Source_workbook.Sheets("Журнал РНС").Range("Y" & i)
' If Source_workbook.Sheets("Журнал РНС").Range("Z" & i).Value = "Не обнаружена делеция 7 экзона гена SMN1 в гомозиготном состоянии" Then
' CurrentWb.Sheets("Лист1").Range("G12").Value = "Не обнаружено"
' Else
' CurrentWb.Sheets("Лист1").Range("G12").Value = "Обнаружено"
' End If
 
CurrentWb.Sheets("Лист1").Range("B12").Value = Source_workbook.Sheets("Журнал РНС").Range("AJ" & i) 'пролин
CurrentWb.Sheets("Лист1").Range("C12").Value = Source_workbook.Sheets("Журнал РНС").Range("AJ2").Value
CurrentWb.Sheets("Лист1").Range("E12").Value = Source_workbook.Sheets("Журнал РНС").Range("AJ4").Value
'If CurrentWb.Sheets("Лист1").Range("B12").Value < Source_workbook.Sheets("Журнал РНС").Range("AJ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B12").Value = CurrentWb.Sheets("Лист1").Range("B12").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B12").Value > Source_workbook.Sheets("Журнал РНС").Range("AJ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B12").Value = CurrentWb.Sheets("Лист1").Range("B12").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B12").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B13").Value = Source_workbook.Sheets("Журнал РНС").Range("AK" & i) 'валин
CurrentWb.Sheets("Лист1").Range("C13").Value = Source_workbook.Sheets("Журнал РНС").Range("AK2").Value
CurrentWb.Sheets("Лист1").Range("E13").Value = Source_workbook.Sheets("Журнал РНС").Range("AK4").Value
'If CurrentWb.Sheets("Лист1").Range("B13").Value < Source_workbook.Sheets("Журнал РНС").Range("AK2").Value Then
'CurrentWb.Sheets("Лист1").Range("B13").Value = CurrentWb.Sheets("Лист1").Range("B13").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B13").Value > Source_workbook.Sheets("Журнал РНС").Range("AK4").Value Then
'CurrentWb.Sheets("Лист1").Range("B13").Value = CurrentWb.Sheets("Лист1").Range("B13").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B13").Value = "норма"
'End If

  


CurrentWb.Sheets("Лист1").Range("B14").Value = Source_workbook.Sheets("Журнал РНС").Range("AL" & i) 'сумм конц лейцина
CurrentWb.Sheets("Лист1").Range("C14").Value = Source_workbook.Sheets("Журнал РНС").Range("AL2").Value
CurrentWb.Sheets("Лист1").Range("E14").Value = Source_workbook.Sheets("Журнал РНС").Range("AL4").Value
'    If CurrentWb.Sheets("Лист1").Range("B14").Value < Source_workbook.Sheets("Журнал РНС").Range("AL2").Value Then
'    CurrentWb.Sheets("Лист1").Range("B14").Value = CurrentWb.Sheets("Лист1").Range("B14").Value
'    ElseIf CurrentWb.Sheets("Лист1").Range("B14").Value > Source_workbook.Sheets("Журнал РНС").Range("AL4").Value Then
'    CurrentWb.Sheets("Лист1").Range("B14").Value = CurrentWb.Sheets("Лист1").Range("B14").Value
'    Else
'    CurrentWb.Sheets("Лист1").Range("B14").Value = "норма"
'    End If


CurrentWb.Sheets("Лист1").Range("B15").Value = Source_workbook.Sheets("Журнал РНС").Range("AP" & i) 'Метионин
CurrentWb.Sheets("Лист1").Range("C15").Value = Source_workbook.Sheets("Журнал РНС").Range("AP2").Value
CurrentWb.Sheets("Лист1").Range("E15").Value = Source_workbook.Sheets("Журнал РНС").Range("AP4").Value
'If CurrentWb.Sheets("Лист1").Range("B15").Value < Source_workbook.Sheets("Журнал РНС").Range("AP2").Value Then
'CurrentWb.Sheets("Лист1").Range("B15").Value = CurrentWb.Sheets("Лист1").Range("B15").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B15").Value > Source_workbook.Sheets("Журнал РНС").Range("AP4").Value Then
'CurrentWb.Sheets("Лист1").Range("B15").Value = CurrentWb.Sheets("Лист1").Range("B15").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B15").Value = "норма"
'End If




CurrentWb.Sheets("Лист1").Range("B16").Value = Source_workbook.Sheets("Журнал РНС").Range("AR" & i) 'Фениланин
CurrentWb.Sheets("Лист1").Range("C16").Value = Source_workbook.Sheets("Журнал РНС").Range("AR2").Value
CurrentWb.Sheets("Лист1").Range("E16").Value = Source_workbook.Sheets("Журнал РНС").Range("AR4").Value
'If CurrentWb.Sheets("Лист1").Range("B16").Value < Source_workbook.Sheets("Журнал РНС").Range("AR2").Value Then
'CurrentWb.Sheets("Лист1").Range("B16").Value = CurrentWb.Sheets("Лист1").Range("B16").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B16").Value > Source_workbook.Sheets("Журнал РНС").Range("AR4").Value Then
'CurrentWb.Sheets("Лист1").Range("B16").Value = CurrentWb.Sheets("Лист1").Range("B16").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B16").Value = "норма"
'End If



CurrentWb.Sheets("Лист1").Range("B17").Value = Source_workbook.Sheets("Журнал РНС").Range("AS" & i) 'Аргинин
CurrentWb.Sheets("Лист1").Range("C17").Value = Source_workbook.Sheets("Журнал РНС").Range("AS2").Value
CurrentWb.Sheets("Лист1").Range("E17").Value = Source_workbook.Sheets("Журнал РНС").Range("AS4").Value
'If CurrentWb.Sheets("Лист1").Range("B17").Value < Source_workbook.Sheets("Журнал РНС").Range("AS2").Value Then
'CurrentWb.Sheets("Лист1").Range("B17").Value = CurrentWb.Sheets("Лист1").Range("B17").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B17").Value > Source_workbook.Sheets("Журнал РНС").Range("AS4").Value Then
'CurrentWb.Sheets("Лист1").Range("B17").Value = CurrentWb.Sheets("Лист1").Range("B17").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B17").Value = "норма"
'End If



CurrentWb.Sheets("Лист1").Range("B18").Value = Source_workbook.Sheets("Журнал РНС").Range("AT" & i) 'Цитрулин
CurrentWb.Sheets("Лист1").Range("C18").Value = Source_workbook.Sheets("Журнал РНС").Range("AT2").Value
CurrentWb.Sheets("Лист1").Range("E18").Value = Source_workbook.Sheets("Журнал РНС").Range("AT4").Value
'If CurrentWb.Sheets("Лист1").Range("B18").Value < Source_workbook.Sheets("Журнал РНС").Range("AT2").Value Then
'CurrentWb.Sheets("Лист1").Range("B18").Value = CurrentWb.Sheets("Лист1").Range("B18").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B18").Value > Source_workbook.Sheets("Журнал РНС").Range("AT4").Value Then
'CurrentWb.Sheets("Лист1").Range("B18").Value = CurrentWb.Sheets("Лист1").Range("B18").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B18").Value = "норма"
'End If



CurrentWb.Sheets("Лист1").Range("B19").Value = Source_workbook.Sheets("Журнал РНС").Range("AU" & i) 'Тирозин
CurrentWb.Sheets("Лист1").Range("C19").Value = Source_workbook.Sheets("Журнал РНС").Range("AU2").Value
CurrentWb.Sheets("Лист1").Range("E19").Value = Source_workbook.Sheets("Журнал РНС").Range("AU4").Value
'If CurrentWb.Sheets("Лист1").Range("B19").Value < Source_workbook.Sheets("Журнал РНС").Range("AU2").Value Then
'CurrentWb.Sheets("Лист1").Range("B19").Value = CurrentWb.Sheets("Лист1").Range("B19").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B19").Value > Source_workbook.Sheets("Журнал РНС").Range("AU4").Value Then
'CurrentWb.Sheets("Лист1").Range("B19").Value = CurrentWb.Sheets("Лист1").Range("B19").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B19").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B20").Value = Source_workbook.Sheets("Журнал РНС").Range("AZ" & i) 'Свободный карнитин
CurrentWb.Sheets("Лист1").Range("C20").Value = Source_workbook.Sheets("Журнал РНС").Range("AZ2").Value
CurrentWb.Sheets("Лист1").Range("E20").Value = Source_workbook.Sheets("Журнал РНС").Range("AZ4").Value
'If CurrentWb.Sheets("Лист1").Range("B20").Value < Source_workbook.Sheets("Журнал РНС").Range("AZ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B20").Value = CurrentWb.Sheets("Лист1").Range("B20").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B20").Value > Source_workbook.Sheets("Журнал РНС").Range("AZ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B20").Value = CurrentWb.Sheets("Лист1").Range("B20").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B20").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B21").Value = Source_workbook.Sheets("Журнал РНС").Range("BA" & i) 'С2
CurrentWb.Sheets("Лист1").Range("C21").Value = Source_workbook.Sheets("Журнал РНС").Range("BA2").Value
CurrentWb.Sheets("Лист1").Range("E21").Value = Source_workbook.Sheets("Журнал РНС").Range("BA4").Value
'If CurrentWb.Sheets("Лист1").Range("B21").Value < Source_workbook.Sheets("Журнал РНС").Range("BA2").Value Then
'CurrentWb.Sheets("Лист1").Range("B21").Value = CurrentWb.Sheets("Лист1").Range("B21").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B21").Value > Source_workbook.Sheets("Журнал РНС").Range("BA4").Value Then
'CurrentWb.Sheets("Лист1").Range("B21").Value = CurrentWb.Sheets("Лист1").Range("B21").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B21").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B22").Value = Source_workbook.Sheets("Журнал РНС").Range("BB" & i) 'Пропионилкарнитин
CurrentWb.Sheets("Лист1").Range("C22").Value = Source_workbook.Sheets("Журнал РНС").Range("BB2").Value
CurrentWb.Sheets("Лист1").Range("E22").Value = Source_workbook.Sheets("Журнал РНС").Range("BB4").Value
'If CurrentWb.Sheets("Лист1").Range("B22").Value < Source_workbook.Sheets("Журнал РНС").Range("BB2").Value Then
'CurrentWb.Sheets("Лист1").Range("B22").Value = CurrentWb.Sheets("Лист1").Range("B22").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B22").Value > Source_workbook.Sheets("Журнал РНС").Range("BB4").Value Then
'CurrentWb.Sheets("Лист1").Range("B22").Value = CurrentWb.Sheets("Лист1").Range("B22").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B22").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B23").Value = Source_workbook.Sheets("Журнал РНС").Range("BC" & i) 'c3dc/c4 oh
CurrentWb.Sheets("Лист1").Range("C23").Value = Source_workbook.Sheets("Журнал РНС").Range("BC2").Value
CurrentWb.Sheets("Лист1").Range("E23").Value = Source_workbook.Sheets("Журнал РНС").Range("BC4").Value
'If CurrentWb.Sheets("Лист1").Range("B23").Value < Source_workbook.Sheets("Журнал РНС").Range("BC2").Value Then
'CurrentWb.Sheets("Лист1").Range("B23").Value = CurrentWb.Sheets("Лист1").Range("B23").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B23").Value > Source_workbook.Sheets("Журнал РНС").Range("BC4").Value Then
'CurrentWb.Sheets("Лист1").Range("B23").Value = CurrentWb.Sheets("Лист1").Range("B23").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B23").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B24").Value = Source_workbook.Sheets("Журнал РНС").Range("BD" & i) 'c4
CurrentWb.Sheets("Лист1").Range("C24").Value = Source_workbook.Sheets("Журнал РНС").Range("BD2").Value
CurrentWb.Sheets("Лист1").Range("E24").Value = Source_workbook.Sheets("Журнал РНС").Range("BD4").Value
'If CurrentWb.Sheets("Лист1").Range("B24").Value < Source_workbook.Sheets("Журнал РНС").Range("BD2").Value Then
'CurrentWb.Sheets("Лист1").Range("B24").Value = CurrentWb.Sheets("Лист1").Range("B24").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B24").Value > Source_workbook.Sheets("Журнал РНС").Range("BD4").Value Then
'CurrentWb.Sheets("Лист1").Range("B24").Value = CurrentWb.Sheets("Лист1").Range("B24").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B24").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B25").Value = Source_workbook.Sheets("Журнал РНС").Range("BF" & i) 'Изовалерилкарнитин
CurrentWb.Sheets("Лист1").Range("C25").Value = Source_workbook.Sheets("Журнал РНС").Range("BF2").Value
CurrentWb.Sheets("Лист1").Range("E25").Value = Source_workbook.Sheets("Журнал РНС").Range("BF4").Value
'If CurrentWb.Sheets("Лист1").Range("B25").Value < Source_workbook.Sheets("Журнал РНС").Range("BF2").Value Then
'CurrentWb.Sheets("Лист1").Range("B25").Value = CurrentWb.Sheets("Лист1").Range("B25").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B25").Value > Source_workbook.Sheets("Журнал РНС").Range("BF4").Value Then
'CurrentWb.Sheets("Лист1").Range("B25").Value = CurrentWb.Sheets("Лист1").Range("B25").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B25").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B26").Value = Source_workbook.Sheets("Журнал РНС").Range("BE" & i) 'с5-1
CurrentWb.Sheets("Лист1").Range("C26").Value = Source_workbook.Sheets("Журнал РНС").Range("BE2").Value
CurrentWb.Sheets("Лист1").Range("E26").Value = Source_workbook.Sheets("Журнал РНС").Range("BE4").Value
'If CurrentWb.Sheets("Лист1").Range("B26").Value < Source_workbook.Sheets("Журнал РНС").Range("BE2").Value Then
'CurrentWb.Sheets("Лист1").Range("B26").Value = CurrentWb.Sheets("Лист1").Range("B26").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B26").Value > Source_workbook.Sheets("Журнал РНС").Range("BE4").Value Then
'CurrentWb.Sheets("Лист1").Range("B26").Value = CurrentWb.Sheets("Лист1").Range("B26").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B26").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B27").Value = Source_workbook.Sheets("Журнал РНС").Range("BG" & i) 'с4dc/c5oh
CurrentWb.Sheets("Лист1").Range("C27").Value = Source_workbook.Sheets("Журнал РНС").Range("BG2").Value
CurrentWb.Sheets("Лист1").Range("E27").Value = Source_workbook.Sheets("Журнал РНС").Range("BG4").Value
'If CurrentWb.Sheets("Лист1").Range("B27").Value < Source_workbook.Sheets("Журнал РНС").Range("BG2").Value Then
'CurrentWb.Sheets("Лист1").Range("B27").Value = CurrentWb.Sheets("Лист1").Range("B27").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B27").Value > Source_workbook.Sheets("Журнал РНС").Range("BG4").Value Then
'CurrentWb.Sheets("Лист1").Range("B27").Value = CurrentWb.Sheets("Лист1").Range("B27").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B27").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B28").Value = Source_workbook.Sheets("Журнал РНС").Range("BH" & i) 'с6
CurrentWb.Sheets("Лист1").Range("C28").Value = Source_workbook.Sheets("Журнал РНС").Range("BH2").Value
CurrentWb.Sheets("Лист1").Range("E28").Value = Source_workbook.Sheets("Журнал РНС").Range("BH4").Value
'If CurrentWb.Sheets("Лист1").Range("B28").Value < Source_workbook.Sheets("Журнал РНС").Range("BH2").Value Then
'CurrentWb.Sheets("Лист1").Range("B28").Value = CurrentWb.Sheets("Лист1").Range("B28").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B28").Value > Source_workbook.Sheets("Журнал РНС").Range("BH4").Value Then
'CurrentWb.Sheets("Лист1").Range("B28").Value = CurrentWb.Sheets("Лист1").Range("B28").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B28").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B29").Value = Source_workbook.Sheets("Журнал РНС").Range("BI" & i) 'c5dc/c6oh
CurrentWb.Sheets("Лист1").Range("C29").Value = Source_workbook.Sheets("Журнал РНС").Range("BI2").Value
CurrentWb.Sheets("Лист1").Range("E29").Value = Source_workbook.Sheets("Журнал РНС").Range("BI4").Value
'If CurrentWb.Sheets("Лист1").Range("B29").Value < Source_workbook.Sheets("Журнал РНС").Range("BI2").Value Then
'CurrentWb.Sheets("Лист1").Range("B29").Value = CurrentWb.Sheets("Лист1").Range("B29").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B29").Value > Source_workbook.Sheets("Журнал РНС").Range("BI4").Value Then
'CurrentWb.Sheets("Лист1").Range("B29").Value = CurrentWb.Sheets("Лист1").Range("B29").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B29").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B30").Value = Source_workbook.Sheets("Журнал РНС").Range("BJ" & i) 'c6dc
CurrentWb.Sheets("Лист1").Range("C30").Value = Source_workbook.Sheets("Журнал РНС").Range("BJ2").Value
CurrentWb.Sheets("Лист1").Range("E30").Value = Source_workbook.Sheets("Журнал РНС").Range("BJ4").Value
'If CurrentWb.Sheets("Лист1").Range("B30").Value < Source_workbook.Sheets("Журнал РНС").Range("BJ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B30").Value = CurrentWb.Sheets("Лист1").Range("B30").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B30").Value > Source_workbook.Sheets("Журнал РНС").Range("BJ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B30").Value = CurrentWb.Sheets("Лист1").Range("B30").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B30").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B31").Value = Source_workbook.Sheets("Журнал РНС").Range("BK" & i) 'c8-1
CurrentWb.Sheets("Лист1").Range("C31").Value = Source_workbook.Sheets("Журнал РНС").Range("BK2").Value
CurrentWb.Sheets("Лист1").Range("E31").Value = Source_workbook.Sheets("Журнал РНС").Range("BK4").Value
'If CurrentWb.Sheets("Лист1").Range("B31").Value < Source_workbook.Sheets("Журнал РНС").Range("BK2").Value Then
'CurrentWb.Sheets("Лист1").Range("B31").Value = CurrentWb.Sheets("Лист1").Range("B31").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B31").Value > Source_workbook.Sheets("Журнал РНС").Range("BK4").Value Then
'CurrentWb.Sheets("Лист1").Range("B31").Value = CurrentWb.Sheets("Лист1").Range("B31").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B31").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B32").Value = Source_workbook.Sheets("Журнал РНС").Range("BL" & i) 'c8
CurrentWb.Sheets("Лист1").Range("C32").Value = Source_workbook.Sheets("Журнал РНС").Range("BL2").Value
CurrentWb.Sheets("Лист1").Range("E32").Value = Source_workbook.Sheets("Журнал РНС").Range("BL4").Value
'If CurrentWb.Sheets("Лист1").Range("B32").Value < Source_workbook.Sheets("Журнал РНС").Range("BL2").Value Then
'CurrentWb.Sheets("Лист1").Range("B32").Value = CurrentWb.Sheets("Лист1").Range("B32").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B32").Value > Source_workbook.Sheets("Журнал РНС").Range("BL4").Value Then
'CurrentWb.Sheets("Лист1").Range("B32").Value = CurrentWb.Sheets("Лист1").Range("B32").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B32").Value = "норма"
'End If
'


CurrentWb.Sheets("Лист1").Range("B33").Value = Source_workbook.Sheets("Журнал РНС").Range("BN" & i) 'c10:1
CurrentWb.Sheets("Лист1").Range("C33").Value = Source_workbook.Sheets("Журнал РНС").Range("BN2").Value
CurrentWb.Sheets("Лист1").Range("E33").Value = Source_workbook.Sheets("Журнал РНС").Range("BN4").Value
'If CurrentWb.Sheets("Лист1").Range("B34").Value < Source_workbook.Sheets("Журнал РНС").Range("BN2").Value Then
'CurrentWb.Sheets("Лист1").Range("B34").Value = CurrentWb.Sheets("Лист1").Range("B34").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B34").Value > Source_workbook.Sheets("Журнал РНС").Range("BN4").Value Then
'CurrentWb.Sheets("Лист1").Range("B34").Value = CurrentWb.Sheets("Лист1").Range("B34").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B34").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B34").Value = Source_workbook.Sheets("Журнал РНС").Range("BO" & i) 'c10
CurrentWb.Sheets("Лист1").Range("C34").Value = Source_workbook.Sheets("Журнал РНС").Range("BO2").Value
CurrentWb.Sheets("Лист1").Range("E34").Value = Source_workbook.Sheets("Журнал РНС").Range("BO4").Value
'If CurrentWb.Sheets("Лист1").Range("B35").Value < Source_workbook.Sheets("Журнал РНС").Range("BO2").Value Then
'CurrentWb.Sheets("Лист1").Range("B35").Value = CurrentWb.Sheets("Лист1").Range("B35").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B35").Value > Source_workbook.Sheets("Журнал РНС").Range("BO4").Value Then
'CurrentWb.Sheets("Лист1").Range("B35").Value = CurrentWb.Sheets("Лист1").Range("B35").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B35").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B35").Value = Source_workbook.Sheets("Журнал РНС").Range("BP" & i) 'c12-1
CurrentWb.Sheets("Лист1").Range("C35").Value = Source_workbook.Sheets("Журнал РНС").Range("BP2").Value
CurrentWb.Sheets("Лист1").Range("E35").Value = Source_workbook.Sheets("Журнал РНС").Range("BP4").Value
'If CurrentWb.Sheets("Лист1").Range("B36").Value < Source_workbook.Sheets("Журнал РНС").Range("BP2").Value Then
'CurrentWb.Sheets("Лист1").Range("B36").Value = CurrentWb.Sheets("Лист1").Range("B36").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B36").Value > Source_workbook.Sheets("Журнал РНС").Range("BP4").Value Then
'CurrentWb.Sheets("Лист1").Range("B36").Value = CurrentWb.Sheets("Лист1").Range("B36").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B36").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B36").Value = Source_workbook.Sheets("Журнал РНС").Range("BQ" & i) 'c12
CurrentWb.Sheets("Лист1").Range("C36").Value = Source_workbook.Sheets("Журнал РНС").Range("BQ2").Value
CurrentWb.Sheets("Лист1").Range("E36").Value = Source_workbook.Sheets("Журнал РНС").Range("BQ4").Value
'If CurrentWb.Sheets("Лист1").Range("B37").Value < Source_workbook.Sheets("Журнал РНС").Range("BQ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B37").Value = CurrentWb.Sheets("Лист1").Range("B37").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B37").Value > Source_workbook.Sheets("Журнал РНС").Range("BQ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B37").Value = CurrentWb.Sheets("Лист1").Range("B37").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B37").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B37").Value = Source_workbook.Sheets("Журнал РНС").Range("BR" & i) 'c14:2
CurrentWb.Sheets("Лист1").Range("C37").Value = Source_workbook.Sheets("Журнал РНС").Range("BR2").Value
CurrentWb.Sheets("Лист1").Range("E37").Value = Source_workbook.Sheets("Журнал РНС").Range("BR4").Value
'If CurrentWb.Sheets("Лист1").Range("B38").Value < Source_workbook.Sheets("Журнал РНС").Range("BR2").Value Then
'CurrentWb.Sheets("Лист1").Range("B38").Value = CurrentWb.Sheets("Лист1").Range("B38").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B38").Value > Source_workbook.Sheets("Журнал РНС").Range("BR4").Value Then
'CurrentWb.Sheets("Лист1").Range("B38").Value = CurrentWb.Sheets("Лист1").Range("B38").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B38").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B38").Value = Source_workbook.Sheets("Журнал РНС").Range("BS" & i) 'c14:1
CurrentWb.Sheets("Лист1").Range("C38").Value = Source_workbook.Sheets("Журнал РНС").Range("BS2").Value
CurrentWb.Sheets("Лист1").Range("E38").Value = Source_workbook.Sheets("Журнал РНС").Range("BS4").Value
'If CurrentWb.Sheets("Лист1").Range("B39").Value < Source_workbook.Sheets("Журнал РНС").Range("BS2").Value Then
'CurrentWb.Sheets("Лист1").Range("B39").Value = CurrentWb.Sheets("Лист1").Range("B39").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B39").Value > Source_workbook.Sheets("Журнал РНС").Range("BS4").Value Then
'CurrentWb.Sheets("Лист1").Range("B39").Value = CurrentWb.Sheets("Лист1").Range("B39").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B39").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B39").Value = Source_workbook.Sheets("Журнал РНС").Range("BT" & i) 'c14
CurrentWb.Sheets("Лист1").Range("C39").Value = Source_workbook.Sheets("Журнал РНС").Range("BT2").Value
CurrentWb.Sheets("Лист1").Range("E39").Value = Source_workbook.Sheets("Журнал РНС").Range("BT4").Value
'If CurrentWb.Sheets("Лист1").Range("B40").Value < Source_workbook.Sheets("Журнал РНС").Range("BT2").Value Then
'CurrentWb.Sheets("Лист1").Range("B40").Value = CurrentWb.Sheets("Лист1").Range("B40").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B40").Value > Source_workbook.Sheets("Журнал РНС").Range("BT4").Value Then
'CurrentWb.Sheets("Лист1").Range("B40").Value = CurrentWb.Sheets("Лист1").Range("B40").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B40").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B40").Value = Source_workbook.Sheets("Журнал РНС").Range("BU" & i) 'c14OH
CurrentWb.Sheets("Лист1").Range("C40").Value = Source_workbook.Sheets("Журнал РНС").Range("BU2").Value
CurrentWb.Sheets("Лист1").Range("E40").Value = Source_workbook.Sheets("Журнал РНС").Range("BU4").Value
'If CurrentWb.Sheets("Лист1").Range("B41").Value < Source_workbook.Sheets("Журнал РНС").Range("BU2").Value Then
'CurrentWb.Sheets("Лист1").Range("B41").Value = CurrentWb.Sheets("Лист1").Range("B41").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B41").Value > Source_workbook.Sheets("Журнал РНС").Range("BU4").Value Then
'CurrentWb.Sheets("Лист1").Range("B41").Value = CurrentWb.Sheets("Лист1").Range("B41").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B41").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B41").Value = Source_workbook.Sheets("Журнал РНС").Range("BV" & i) 'c16-1
CurrentWb.Sheets("Лист1").Range("C41").Value = Source_workbook.Sheets("Журнал РНС").Range("BV2").Value
CurrentWb.Sheets("Лист1").Range("E41").Value = Source_workbook.Sheets("Журнал РНС").Range("BV4").Value
'If CurrentWb.Sheets("Лист1").Range("B42").Value < Source_workbook.Sheets("Журнал РНС").Range("BV2").Value Then
'CurrentWb.Sheets("Лист1").Range("B42").Value = CurrentWb.Sheets("Лист1").Range("B42").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B42").Value > Source_workbook.Sheets("Журнал РНС").Range("BV4").Value Then
'CurrentWb.Sheets("Лист1").Range("B42").Value = CurrentWb.Sheets("Лист1").Range("B42").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B42").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B42").Value = Source_workbook.Sheets("Журнал РНС").Range("BW" & i) 'c16
CurrentWb.Sheets("Лист1").Range("C42").Value = Source_workbook.Sheets("Журнал РНС").Range("BW2").Value
CurrentWb.Sheets("Лист1").Range("E42").Value = Source_workbook.Sheets("Журнал РНС").Range("BW4").Value
'If CurrentWb.Sheets("Лист1").Range("B43").Value < Source_workbook.Sheets("Журнал РНС").Range("BW2").Value Then
'CurrentWb.Sheets("Лист1").Range("B43").Value = CurrentWb.Sheets("Лист1").Range("B43").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B43").Value > Source_workbook.Sheets("Журнал РНС").Range("BW4").Value Then
'CurrentWb.Sheets("Лист1").Range("B43").Value = CurrentWb.Sheets("Лист1").Range("B43").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B43").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B43").Value = Source_workbook.Sheets("Журнал РНС").Range("BX" & i) 'c16\c17
CurrentWb.Sheets("Лист1").Range("C43").Value = Source_workbook.Sheets("Журнал РНС").Range("BX2").Value
CurrentWb.Sheets("Лист1").Range("E43").Value = Source_workbook.Sheets("Журнал РНС").Range("BX4").Value
'If CurrentWb.Sheets("Лист1").Range("B44").Value < Source_workbook.Sheets("Журнал РНС").Range("BX2").Value Then
'CurrentWb.Sheets("Лист1").Range("B44").Value = CurrentWb.Sheets("Лист1").Range("B44").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B44").Value > Source_workbook.Sheets("Журнал РНС").Range("BX4").Value Then
'CurrentWb.Sheets("Лист1").Range("B44").Value = CurrentWb.Sheets("Лист1").Range("B44").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B44").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B44").Value = Source_workbook.Sheets("Журнал РНС").Range("BY" & i) 'c16OH
CurrentWb.Sheets("Лист1").Range("C44").Value = Source_workbook.Sheets("Журнал РНС").Range("BY2").Value
CurrentWb.Sheets("Лист1").Range("E44").Value = Source_workbook.Sheets("Журнал РНС").Range("BY4").Value
'If CurrentWb.Sheets("Лист1").Range("B45").Value < Source_workbook.Sheets("Журнал РНС").Range("BY2").Value Then
'CurrentWb.Sheets("Лист1").Range("B45").Value = CurrentWb.Sheets("Лист1").Range("B45").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B45").Value > Source_workbook.Sheets("Журнал РНС").Range("BY4").Value Then
'CurrentWb.Sheets("Лист1").Range("B45").Value = CurrentWb.Sheets("Лист1").Range("B45").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B45").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B45").Value = Source_workbook.Sheets("Журнал РНС").Range("BZ" & i) 'c18-2
CurrentWb.Sheets("Лист1").Range("C45").Value = Source_workbook.Sheets("Журнал РНС").Range("BZ2").Value
CurrentWb.Sheets("Лист1").Range("E45").Value = Source_workbook.Sheets("Журнал РНС").Range("BZ4").Value
'If CurrentWb.Sheets("Лист1").Range("B46").Value < Source_workbook.Sheets("Журнал РНС").Range("BZ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B46").Value = CurrentWb.Sheets("Лист1").Range("B46").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B46").Value > Source_workbook.Sheets("Журнал РНС").Range("BZ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B46").Value = CurrentWb.Sheets("Лист1").Range("B46").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B46").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B46").Value = Source_workbook.Sheets("Журнал РНС").Range("CA" & i) 'c18-1
CurrentWb.Sheets("Лист1").Range("C46").Value = Source_workbook.Sheets("Журнал РНС").Range("CA2").Value
CurrentWb.Sheets("Лист1").Range("E46").Value = Source_workbook.Sheets("Журнал РНС").Range("CA4").Value
'If CurrentWb.Sheets("Лист1").Range("B47").Value < Source_workbook.Sheets("Журнал РНС").Range("CA2").Value Then
'CurrentWb.Sheets("Лист1").Range("B47").Value = CurrentWb.Sheets("Лист1").Range("B47").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B47").Value > Source_workbook.Sheets("Журнал РНС").Range("CA4").Value Then
'CurrentWb.Sheets("Лист1").Range("B47").Value = CurrentWb.Sheets("Лист1").Range("B47").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B47").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B47").Value = Source_workbook.Sheets("Журнал РНС").Range("CB" & i) 'c18
CurrentWb.Sheets("Лист1").Range("C47").Value = Source_workbook.Sheets("Журнал РНС").Range("CB2").Value
CurrentWb.Sheets("Лист1").Range("E47").Value = Source_workbook.Sheets("Журнал РНС").Range("CB4").Value
'If CurrentWb.Sheets("Лист1").Range("B48").Value < Source_workbook.Sheets("Журнал РНС").Range("CB2").Value Then
'CurrentWb.Sheets("Лист1").Range("B48").Value = CurrentWb.Sheets("Лист1").Range("B48").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B48").Value > Source_workbook.Sheets("Журнал РНС").Range("CB4").Value Then
'CurrentWb.Sheets("Лист1").Range("B48").Value = CurrentWb.Sheets("Лист1").Range("B48").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B48").Value = "норма"
'End If



CurrentWb.Sheets("Лист1").Range("B48").Value = Source_workbook.Sheets("Журнал РНС").Range("CD" & i) 'c18-1oh
CurrentWb.Sheets("Лист1").Range("C48").Value = Source_workbook.Sheets("Журнал РНС").Range("CD2").Value
CurrentWb.Sheets("Лист1").Range("E48").Value = Source_workbook.Sheets("Журнал РНС").Range("CD4").Value
'If CurrentWb.Sheets("Лист1").Range("B49").Value < Source_workbook.Sheets("Журнал РНС").Range("CD2").Value Then
'CurrentWb.Sheets("Лист1").Range("B49").Value = CurrentWb.Sheets("Лист1").Range("B49").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B49").Value > Source_workbook.Sheets("Журнал РНС").Range("CD4").Value Then
'CurrentWb.Sheets("Лист1").Range("B49").Value = CurrentWb.Sheets("Лист1").Range("B49").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B49").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B49").Value = Source_workbook.Sheets("Журнал РНС").Range("CE" & i) 'c18oh
CurrentWb.Sheets("Лист1").Range("C49").Value = Source_workbook.Sheets("Журнал РНС").Range("CE2").Value
CurrentWb.Sheets("Лист1").Range("E49").Value = Source_workbook.Sheets("Журнал РНС").Range("CE4").Value
'If CurrentWb.Sheets("Лист1").Range("B50").Value < Source_workbook.Sheets("Журнал РНС").Range("CE2").Value Then
'CurrentWb.Sheets("Лист1").Range("B50").Value = CurrentWb.Sheets("Лист1").Range("B50").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B50").Value > Source_workbook.Sheets("Журнал РНС").Range("CE4").Value Then
'CurrentWb.Sheets("Лист1").Range("B50").Value = CurrentWb.Sheets("Лист1").Range("B50").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B50").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B50").Value = Source_workbook.Sheets("Журнал РНС").Range("CO" & i) 'с3/с2
CurrentWb.Sheets("Лист1").Range("C50").Value = Source_workbook.Sheets("Журнал РНС").Range("CO2").Value
CurrentWb.Sheets("Лист1").Range("E50").Value = Source_workbook.Sheets("Журнал РНС").Range("CO4").Value
'If CurrentWb.Sheets("Лист1").Range("B53").Value < Source_workbook.Sheets("Журнал РНС").Range("CO2").Value Then
'CurrentWb.Sheets("Лист1").Range("B53").Value = CurrentWb.Sheets("Лист1").Range("B53").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B53").Value > Source_workbook.Sheets("Журнал РНС").Range("CO4").Value Then
'CurrentWb.Sheets("Лист1").Range("B53").Value = CurrentWb.Sheets("Лист1").Range("B53").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B53").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B51").Value = Source_workbook.Sheets("Журнал РНС").Range("CP" & i) 'с0/(с16+с18)
CurrentWb.Sheets("Лист1").Range("C51").Value = Source_workbook.Sheets("Журнал РНС").Range("CP2").Value
CurrentWb.Sheets("Лист1").Range("E51").Value = Source_workbook.Sheets("Журнал РНС").Range("CP4").Value
'If CurrentWb.Sheets("Лист1").Range("B54").Value < Source_workbook.Sheets("Журнал РНС").Range("CP2").Value Then
'CurrentWb.Sheets("Лист1").Range("B54").Value = CurrentWb.Sheets("Лист1").Range("B54").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B54").Value > Source_workbook.Sheets("Журнал РНС").Range("CP4").Value Then
'CurrentWb.Sheets("Лист1").Range("B54").Value = CurrentWb.Sheets("Лист1").Range("B54").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B54").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B52").Value = Source_workbook.Sheets("Журнал РНС").Range("CR" & i)
CurrentWb.Sheets("Лист1").Range("C52").Value = Source_workbook.Sheets("Журнал РНС").Range("CR2").Value
CurrentWb.Sheets("Лист1").Range("E52").Value = Source_workbook.Sheets("Журнал РНС").Range("CR4").Value
'If CurrentWb.Sheets("Лист1").Range("B55").Value < Source_workbook.Sheets("Журнал РНС").Range("CR2").Value Then
'CurrentWb.Sheets("Лист1").Range("B55").Value = CurrentWb.Sheets("Лист1").Range("B55").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B55").Value > Source_workbook.Sheets("Журнал РНС").Range("CR4").Value Then
'CurrentWb.Sheets("Лист1").Range("B55").Value = CurrentWb.Sheets("Лист1").Range("B55").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B55").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B53").Value = Source_workbook.Sheets("Журнал РНС").Range("CS" & i)
CurrentWb.Sheets("Лист1").Range("C53").Value = Source_workbook.Sheets("Журнал РНС").Range("CS2").Value
CurrentWb.Sheets("Лист1").Range("E53").Value = Source_workbook.Sheets("Журнал РНС").Range("CS4").Value
'If CurrentWb.Sheets("Лист1").Range("B56").Value < Source_workbook.Sheets("Журнал РНС").Range("CS2").Value Then
'CurrentWb.Sheets("Лист1").Range("B56").Value = CurrentWb.Sheets("Лист1").Range("B56").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B56").Value > Source_workbook.Sheets("Журнал РНС").Range("CS4").Value Then
'CurrentWb.Sheets("Лист1").Range("B56").Value = CurrentWb.Sheets("Лист1").Range("B56").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B56").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B54").Value = Source_workbook.Sheets("Журнал РНС").Range("CW" & i)
CurrentWb.Sheets("Лист1").Range("C54").Value = Source_workbook.Sheets("Журнал РНС").Range("CW2").Value
CurrentWb.Sheets("Лист1").Range("E54").Value = Source_workbook.Sheets("Журнал РНС").Range("CW4").Value
'If CurrentWb.Sheets("Лист1").Range("B57").Value < Source_workbook.Sheets("Журнал РНС").Range("CW2").Value Then
'CurrentWb.Sheets("Лист1").Range("B57").Value = CurrentWb.Sheets("Лист1").Range("B57").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B57").Value > Source_workbook.Sheets("Журнал РНС").Range("CW4").Value Then
'CurrentWb.Sheets("Лист1").Range("B57").Value = CurrentWb.Sheets("Лист1").Range("B57").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B57").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B55").Value = Source_workbook.Sheets("Журнал РНС").Range("CX" & i)
CurrentWb.Sheets("Лист1").Range("C55").Value = Source_workbook.Sheets("Журнал РНС").Range("CX2").Value
CurrentWb.Sheets("Лист1").Range("E55").Value = Source_workbook.Sheets("Журнал РНС").Range("CX4").Value
'If CurrentWb.Sheets("Лист1").Range("B58").Value < Source_workbook.Sheets("Журнал РНС").Range("CX2").Value Then
'CurrentWb.Sheets("Лист1").Range("B58").Value = CurrentWb.Sheets("Лист1").Range("B58").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B58").Value > Source_workbook.Sheets("Журнал РНС").Range("CX4").Value Then
'CurrentWb.Sheets("Лист1").Range("B58").Value = CurrentWb.Sheets("Лист1").Range("B58").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B58").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B56").Value = Source_workbook.Sheets("Журнал РНС").Range("CY" & i)
CurrentWb.Sheets("Лист1").Range("C56").Value = Source_workbook.Sheets("Журнал РНС").Range("CY2").Value
CurrentWb.Sheets("Лист1").Range("E56").Value = Source_workbook.Sheets("Журнал РНС").Range("CY4").Value
'If CurrentWb.Sheets("Лист1").Range("B59").Value < Source_workbook.Sheets("Журнал РНС").Range("CY2").Value Then
'CurrentWb.Sheets("Лист1").Range("B59").Value = CurrentWb.Sheets("Лист1").Range("B59").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B59").Value > Source_workbook.Sheets("Журнал РНС").Range("CY4").Value Then
'CurrentWb.Sheets("Лист1").Range("B59").Value = CurrentWb.Sheets("Лист1").Range("B59").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B59").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B57").Value = Source_workbook.Sheets("Журнал РНС").Range("CZ" & i)
CurrentWb.Sheets("Лист1").Range("C57").Value = Source_workbook.Sheets("Журнал РНС").Range("CZ2").Value
CurrentWb.Sheets("Лист1").Range("E57").Value = Source_workbook.Sheets("Журнал РНС").Range("CZ4").Value
'If CurrentWb.Sheets("Лист1").Range("B60").Value < "0" Then
'CurrentWb.Sheets("Лист1").Range("B60").Value = CurrentWb.Sheets("Лист1").Range("B60").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B60").Value > Source_workbook.Sheets("Журнал РНС").Range("CZ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B60").Value = CurrentWb.Sheets("Лист1").Range("B60").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B60").Value = "норма"
'End If

 
CurrentWb.Sheets("Лист1").Range("B58").Value = Source_workbook.Sheets("Журнал РНС").Range("AQ" & i) 'Сукцинилацетон
CurrentWb.Sheets("Лист1").Range("C58").Value = Source_workbook.Sheets("Журнал РНС").Range("AQ2").Value
CurrentWb.Sheets("Лист1").Range("E58").Value = Source_workbook.Sheets("Журнал РНС").Range("AQ4").Value
'If CurrentWb.Sheets("Лист1").Range("B61").Value < Source_workbook.Sheets("Журнал РНС").Range("AQ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B61").Value > Source_workbook.Sheets("Журнал РНС").Range("AQ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B61").Value = "норма"
'End If
  
CurrentWb.Sheets("Лист1").Range("B59").Value = Source_workbook.Sheets("Журнал РНС").Range("AH" & i) 'Глицин
CurrentWb.Sheets("Лист1").Range("C59").Value = Source_workbook.Sheets("Журнал РНС").Range("AH2").Value
CurrentWb.Sheets("Лист1").Range("E59").Value = Source_workbook.Sheets("Журнал РНС").Range("AH4").Value
'If CurrentWb.Sheets("Лист1").Range("B61").Value < Source_workbook.Sheets("Журнал РНС").Range("AQ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B61").Value > Source_workbook.Sheets("Журнал РНС").Range("AQ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B61").Value = "норма"
'End If
  
CurrentWb.Sheets("Лист1").Range("B60").Value = Source_workbook.Sheets("Журнал РНС").Range("AI" & i) 'Аланин
CurrentWb.Sheets("Лист1").Range("C60").Value = Source_workbook.Sheets("Журнал РНС").Range("AI2").Value
CurrentWb.Sheets("Лист1").Range("E60").Value = Source_workbook.Sheets("Журнал РНС").Range("AI4").Value
'If CurrentWb.Sheets("Лист1").Range("B61").Value < Source_workbook.Sheets("Журнал РНС").Range("AQ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B61").Value > Source_workbook.Sheets("Журнал РНС").Range("AQ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B61").Value = "норма"
'End If


CurrentWb.Sheets("Лист1").Range("B61").Value = Source_workbook.Sheets("Журнал РНС").Range("AM" & i) 'Орнитин
CurrentWb.Sheets("Лист1").Range("C61").Value = Source_workbook.Sheets("Журнал РНС").Range("AM2").Value
CurrentWb.Sheets("Лист1").Range("E61").Value = Source_workbook.Sheets("Журнал РНС").Range("AM4").Value
'If CurrentWb.Sheets("Лист1").Range("B61").Value < Source_workbook.Sheets("Журнал РНС").Range("AQ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B61").Value > Source_workbook.Sheets("Журнал РНС").Range("AQ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B61").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B62").Value = Source_workbook.Sheets("Журнал РНС").Range("AO" & i) 'Глутамат
CurrentWb.Sheets("Лист1").Range("C62").Value = Source_workbook.Sheets("Журнал РНС").Range("AO2").Value
CurrentWb.Sheets("Лист1").Range("E62").Value = Source_workbook.Sheets("Журнал РНС").Range("AO4").Value
'If CurrentWb.Sheets("Лист1").Range("B61").Value < Source_workbook.Sheets("Журнал РНС").Range("AQ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B61").Value > Source_workbook.Sheets("Журнал РНС").Range("AQ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B61").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B63").Value = Source_workbook.Sheets("Журнал РНС").Range("AN" & i) 'Gln/Lys
CurrentWb.Sheets("Лист1").Range("C63").Value = Source_workbook.Sheets("Журнал РНС").Range("AN2").Value
CurrentWb.Sheets("Лист1").Range("E63").Value = Source_workbook.Sheets("Журнал РНС").Range("AN4").Value
'If CurrentWb.Sheets("Лист1").Range("B61").Value < Source_workbook.Sheets("Журнал РНС").Range("AQ2").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'ElseIf CurrentWb.Sheets("Лист1").Range("B61").Value > Source_workbook.Sheets("Журнал РНС").Range("AQ4").Value Then
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'Else
'CurrentWb.Sheets("Лист1").Range("B61").Value = "норма"
'End If

CurrentWb.Sheets("Лист1").Range("B65").Value = Source_workbook.Sheets("Журнал РНС").Range("AE" & i)
'If Source_workbook.Sheets("Журнал РНС").Range("AD" & i) = "норма" Then
'Else
'CurrentWb.Sheets("Лист1").Range("B60").Value = CurrentWb.Sheets("Лист1").Range("B60").Value
'End If

CurrentWb.Sheets("Лист1").Range("B66").Value = Source_workbook.Sheets("Журнал РНС").Range("AC" & i)
'If Source_workbook.Sheets("Журнал РНС").Range("AF" & i) = "норма" Then
'Else
'CurrentWb.Sheets("Лист1").Range("B61").Value = CurrentWb.Sheets("Лист1").Range("B61").Value
'End If
  
CurrentWb.Sheets("Лист1").Range("B67").Value = Source_workbook.Sheets("Журнал РНС").Range("Y" & i)
'If Source_workbook.Sheets("Журнал РНС").Range("Z" & i) = "Не обнаружена делеция 7 экзона гена SMN1 в гомозиготном состоянии" Then
'CurrentWb.Sheets("Лист1").Range("B62").Value = "норма"
CurrentWb.Sheets("Лист1").Range("C67").Value = Source_workbook.Sheets("Журнал РНС").Range("Z" & i)
'Else
'CurrentWb.Sheets("Лист1").Range("B62").Value = CurrentWb.Sheets("Лист1").Range("B62").Value
'End If
End Sub
