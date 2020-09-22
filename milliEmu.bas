Attribute VB_Name = "milliEmu"
'====================================='
'           Millipede Emulator        '
'               v0.0.2                '
' By: Don Jarrett. To document the    '
' internals of Millipede(like it hasnt'
' been done). I just needed something '
' else to think about.                '
' Full release with source 5/2/99     '
'====================================='

'====================================='
' This emulator uses the structure of '
' PCSloMo to achieve its goal. I was  '
' never able to get PCSloMo to work   '
' with this game, however, so it      '
' should be a challenge.              '
'====================================='
Public CharImage(4096) As Byte
Public WarpCharSet(16384) As Byte
Public aKeyboard(255) As Byte
Public CyclesPerInt As Long
Public VBlanks As Long, VirtualScreen(2, 256, 256) As Byte
Public Pal(0 To 17) As Long
Public FramesToSkip As Long, Frames As Long
Public Sub MilliMain()

frmMilliEmu.MousePointer = vbHourglass

LoadRom "milliped.104", &H4000&, &H5000&
LoadRom "milliped.103", &H5000&, &H6000&
LoadRom "milliped.102", &H6000&, &H7000&
LoadRom "milliped.101", &H7000&, &H8000&

LoadChr "milliped.106", 0, &H800
LoadChr "milliped.107", 2048, &H1000&

frmMilliEmu.MousePointer = 0

WarpChars
init6502

gameImage(&H2000) = &H80
gameImage(&H408) = &H0
gameImage(&H808) = &H2

  Pal(0) = RGB(0, 0, 0)
  Pal(1) = RGB(&H94, 0, &HD8)
  Pal(2) = RGB(&HD8, 0, 0)
  Pal(3) = RGB(&HF8, &H64, &HD8)
  Pal(4) = RGB(0, &HD8, 0)
  Pal(5) = RGB(0, &HF8, &HD8)
  Pal(6) = RGB(&HD8, &HD8, &H94)
  Pal(7) = RGB(&HD8, &HF8, &HD8)
  Pal(8) = RGB(&HF8, &H94, &H44)
  Pal(9) = RGB(&H0, &H0, &HD8)
  Pal(10) = RGB(&HF8, 0, 0)
  Pal(11) = RGB(&HFF, 0, &HFF)
  
End Sub
Public Sub LoadRom(filename As String, addr As Long, endaddr As Long)
Dim i As Long
Open filename For Binary As #1
For i = addr To endaddr
    Get #1, , gameImage(i)
Next i
Close #1
End Sub
Public Sub LoadChr(filename As String, addr As Long, endaddr As Long)
Open filename For Binary As #1
For i = addr To endaddr
    Get #1, , CharImage(i)
Next i
Close #1
End Sub
Public Function WarpChars()
  Dim X As Integer, cx As Integer, cy As Integer, bit As Integer
  Dim result As Byte
  Dim count As Integer
  count = 0
  For cy = 0 To 7
    For cx = 0 To 31
      For bit = 7 To 0 Step -1
        For X1 = 0 To 7
          result = 0
          If GetBit(bit + (X1 * 8) + (cx * 64) + (cy * 2048)) Then result = 1
          If GetBit(bit + (X1 * 8) + (cx * 64) + (cy * 2048) + 16384) Then result = result Xor 2
          WarpCharSet(count) = result
          count = count + 1
        Next X1
      Next bit
    Next cx
  Next cy
  Erase CharImage
End Function
Public Function GetBit(startbit As Integer) As Integer
  Dim nbyte As Integer
  Dim nbit As Integer
  Dim mask As Integer
  
  nbyte = startbit \ 8
  nbit = startbit Mod 8
  mask = &H80
  mask = mask \ 2 ^ nbit
  
  If (CharImage(nbyte) And mask) = mask Then
    GetBit = 1
  Else
    GetBit = 0
  End If
End Function
Public Function DrawSingleChar(CharNo As Variant, X2 As Integer, Y2 As Integer, palette As Integer)
  Dim cx As Integer
  Dim cy As Integer
  Dim Start As Integer
  Dim hDC As Integer
  Dim X1 As Integer
  Dim Y1 As Integer
  X1 = X2 * 8
  Y1 = Y2 * 8
  Start = CharNo * 64
  For cy = Y1 To Y1 + 7
    For cx = X1 To X1 + 7
      VirtualScreen(viewpic, cx, cy) = WarpCharSet(Start)
      Start = Start + 1
    Next cx
  Next cy
End Function
Public Function drawsprite(offset As Integer, X As Integer, Y As Integer)
  Dim cx As Integer, cy As Integer, Start As Integer
  Dim SpOffset As Integer
  Dim Flip As Integer
  Dim ypos As Byte, xpos As Byte
  Dim hDC As Integer
  
  SpOffset = (offset And &H3E)
  Flip = 1
  Start = SpOffset * 64
  
  If (offset And &H1) = &H1 Then Start = Start + 8192
  If (offset And &H80) = &H80 Then Flip = 0
    
  For cy = 7 To 0 Step -1
    For cx = 0 To 7
      ypos = 255 - (Y + cy)
      If (Flip) Then
        If (238 - (X - cx)) < 0 Then
          xpos = (256 + 238) - (X - cx)
        Else
          xpos = 238 - (X - cx)
        End If
        If WarpCharSet(Start) = 0 Then
        Else
          If xpos < &HF0 Then
             VirtualScreen(viewpic, xpos, ypos) = WarpCharSet(Start)
          End If
        End If
        
        If WarpCharSet(Start + 64) = 0 Then
        Else
          If (xpos + 8) < &HF0 Then
            VirtualScreen(viewpic, xpos + 8, ypos) = WarpCharSet(Start + 64)
          End If
        End If
      Else
        If 238 - (X - (8 - cx)) < 0 Then
          xpos = 256 + 238 - (X - (8 - cx))
        Else
          xpos = 238 - (X - (8 - cx))
        End If
        If WarpCharSet(Start) = 0 Then
        Else
          If (xpos + 8) < &HF0 Then
            VirtualScreen(viewpic, xpos + 8, ypos) = WarpCharSet(Start)
          End If
        End If
        If WarpCharSet(Start + 64) = 0 Then
        Else
          If (xpos < &HF0) Then
            VirtualScreen(viewpic, xpos, ypos) = WarpCharSet(Start + 64)
          End If
        End If
      End If
      Start = Start + 1
    Next cx
  Next cy

End Function
Public Sub DisplayCharSet()
Dim X3 As Integer, Y3 As Integer
For X3 = 0 To 31
    For Y3 = 0 To 7
        DrawSingleChar (Y3 * 32) + X3, X3, Y3, 0
    Next Y3
Next X3
For X3 = 0 To 256
    For Y3 = 0 To 256
        frmMilliEmu.picScreen.PSet (X3, Y3), Pal(VirtualScreen(0, X3, Y3))
    Next Y3
Next X3
Erase VirtualScreen
End Sub
Public Function RedrawScreen()
  Dim X As Integer, Y As Integer, count As Integer
  Dim xpos As Integer
  Dim ypos As Integer
  Dim offset As Integer
  Dim tid As Double
  Dim sluttid As Double
  Dim cx As Integer
  Dim cy As Integer
  Dim Start As Integer
  Dim hDC As Integer
  Dim X1 As Integer
  Dim Y1 As Integer
  Dim col As Long
  tid = Timer
  Dim virx As Long, viry As Long
  Dim col1 As Byte
  count = 0
  'Draw chars
  For X1 = 0 To 29
    For Y1 = 31 To 0 Step -1
      Dim Char As Byte, Add As Byte
      Char = gameImage(&H1000& + count) And &H3F
      Add = 64
      If gameImage(&H1000& + count) And &H40 Then
        Add = 192
      End If
      DrawSingleChar (Char + Add), X1, Y1, 0
      count = count + 1
    Next Y1
  Next X1
  'Draw sprites
  For X = 0 To 15
    xpos = gameImage(&H13D0 + X)
    ypos = gameImage(&H13E0 + X)
    offset = gameImage(&H13C0 + X)
  
    If (ypos < &HF8) Then
      drawsprite offset, xpos, ypos
    End If
  Next X

  If viewpic = 0 Then
    For virx = 0 To 256
      For viry = 0 To 256
        If VirtualScreen(0, virx, viry) = VirtualScreen(1, virx, viry) Then
        Else
          col1 = VirtualScreen(0, virx, viry)
          frmMilliEmu.picScreen.PSet (virx, viry), Pal(col1) 'virtualScreen(viewpic, virx, viry))
          VirtualScreen(1, virx, viry) = VirtualScreen(0, virx, viry)
        End If
      Next viry
    Next virx
  ElseIf viewpic = 1 Then
    For virx = 0 To 256
      For viry = 0 To 256
        If VirtualScreen(1, virx, viry) = VirtualScreen(0, virx, viry) Then
        Else
          col1 = VirtualScreen(1, virx, viry)
          frmMilliEmu.picScreen.PSet (virx, viry), Pal(gameImage(&H2480 + col1))
          VirtualScreen(0, virx, viry) = VirtualScreen(1, virx, viry)
        End If
      Next viry
    Next virx
  End If
  frmMilliEmu.picScreen.Refresh
  If viewpic = 0 Then
    viewpic = 1
  Else
    viewpic = 0
  End If
  frmMilliEmu.picScreen.Refresh
  'BitBlt Arcade.ArcadeScreen.hdc, 0, 0, 256, 256, NewHDC, 0, 0, SRCCOPY
End Function

Public Function ReadMillipedeHW(addr As Long) As Byte
Dim tmp As Integer
Dim val1 As Byte
    Select Case addr
        Case &H2000
            val1 = gameImage(&H2000) And &HF
            val1 = val1 Xor &HF0
            If aKeyboard(vbKey1) = 1 Then val1 = val1 Xor &H20
            If aKeyboard(vbKeyControl) = 1 Then val1 = val1 Xor &H10
            tmp = val1
            gameImage(addr) = val1
        Case &H2001
            val1 = &HFF
            If aKeyboard(vbKey2) Then val1 = val1 Xor &H20
            If aKeyboard(vbKeyControl) Then val1 = val1 Xor &H10
            tmp = val1
        Case &H2680: clockticks6502 = 0
    Case Else
        If addr >= &H400 And addr <= &H40F Then
            tmp = CInt((Rnd * 256) - 1)
            tmp = tmp And &HFF
            If addr = &H408 Then
                tmp = &H40
            End If
        ElseIf addr = &H2010 Or addr = &H2011 Then
            val1 = &HFF
            If aKeyboard(vbKeyUp) = 1 Then val1 = val1 Xor &H8
            If aKeyboard(vbKeyDown) = 1 Then val1 = val1 Xor &H4
            If aKeyboard(vbKeyLeft) = 1 Then val1 = val1 Xor &H2
            If aKeyboard(vbKeyRight) = 1 Then val1 = val1 Xor &H1
            If addr = &H2010 And aKeyboard(vbKey3) = 1 Then
                val1 = val1 Xor &H20
            End If
            tmp = val1
        ElseIf addr >= &H800 And addr <= &H808 Then
            tmp = CInt((Rnd * 255) - 1)
            tmp = tmp And &HFF
            If addr = &H808 Then
                tmp = &H0
            End If
        Else
            tmp = gameImage(addr And &H7FFF)
        End If
    End Select
    ReadMillipedeHW = (tmp And &HFF)
End Function

