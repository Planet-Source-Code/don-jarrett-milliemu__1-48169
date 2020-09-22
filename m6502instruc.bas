Attribute VB_Name = "m6502Instruct"
' This is where all 6502 instructions are kept.
Public Function adc6502()
      Dim tmp As Integer
      
      adrmode (opcode)
      value = get6502memory(savepc)
      
      saveflags = (P And &H1)
      
      sum = A
      sum = (sum + value) And &HFF
      sum = (sum + saveflags) And &HFF
      
      If (sum > &H7F) Or (sum < -&H80) Then
        P = P Or &H40
      Else
        P = (P And &HBF)
      End If
      
      sum = A + (value + saveflags)
      If (sum > &HFF) Then
        P = P Or &H1
      Else
        P = (P And &HFE)
      End If
      
      A = sum And &HFF
      If (P And &H8) Then
             P = (P And &HFE)
              If ((A And &HF) > &H9) Then
                      A = (A + &H6)
              End If
              If ((A And &HF0) > &H90) Then
                      A = A + &H60
                      P = P Or &H1
             End If
      Else
        clockticks6502 = clockticks6502 + 1
      End If
      If (A) Then
      P = (P And &HFD)
      Else
       P = P Or H2
      End If
      If (A And &H80) Then
      P = P Or &H80
      Else
      P = (P And &H7F)
      End If
End Function

Public Function adrmode(opcode As Byte)

  Select Case opcode
    Case &H9D: absx6502
    Case &H29: immediate6502
    Case &HB5: zpx6502
    Case &H90: relative6502
    Case &H10: relative6502
    Case &HD0: relative6502
    Case &H85: zp6502
    Case &HF0: relative6502
    Case &HC9: immediate6502
    Case &HCA: implied6502
    Case &H45: zp6502
    Case &HB0: relative6502
    Case &H20: abs6502
    Case &H60: implied6502
    Case &HA5: zp6502
    Case &H46: zp6502
    Case &HA0: immediate6502
    Case &H18: implied6502
    Case &HA9: immediate6502
    Case &H65: zp6502
    Case &HE0: immediate6502
    Case &H95: zpx6502
    Case &HAD: abs6502
    Case &HA4: zp6502
    Case &HE8: implied6502
    Case &HA8: implied6502
    Case &HA6: zp6502
    Case &H91: indy6502
    Case &H98: implied6502
    Case &HB4: zpx6502
    Case &HB1: indy6502
    Case &H5: zp6502
    Case &H8: implied6502
    Case &H9: immediate6502
    Case &HA: implied6502
    Case &H25: zp6502
    Case &H26: zp6502
    Case &H28: implied6502
    Case &H2C: abs6502
    Case &H30: relative6502
    Case &H38: implied6502
    Case &H40: implied6502
    Case &H48: implied6502
    Case &H49: immediate6502
    Case &H4A: implied6502
    Case &H4C: abs6502
    Case &H4D: abs6502
    Case &H58: implied6502
    Case &H59: absy6502
    Case &H5D: absx6502
    Case &H65: zp6502
    Case &H66: zp6502
    Case &H68: implied6502
    Case &H69: immediate6502
    Case &H6A: implied6502
    Case &H70: relative6502
    Case &H75: zpx6502
    Case &H84: zp6502
    Case &H86: zp6502
    Case &H88: implied6502
    Case &H8A: implied6502
    Case &H8C: abs6502
    Case &H8D: abs6502
    Case &H8E: abs6502
    Case &H8F: implied6502
    Case &H92: indzp6502
    Case &H93: implied6502
    Case &H94: zpx6502
    Case &H96: zpy6502
    Case &H97: implied6502
    Case &H99: absy6502
    Case &H9A: implied6502
    Case &H9B: implied6502
    Case &H9C: abs6502
    Case &H9E: absx6502
    Case &H9F: implied6502
    Case &HA1: indx6502
    Case &HA2: immediate6502
    Case &HA3: implied6502
    Case &HA7: implied6502
    Case &HAA: implied6502
    Case &HAB: implied6502
    Case &HAC: abs6502
    Case &HAE: abs6502
    Case &HAF: implied6502
    Case &HB2: indzp6502
    Case &HB3: implied6502
    Case &HB6: zpy6502
    Case &HB7: implied6502
    Case &HB8: implied6502
    Case &HB9: absy6502
    Case &HBA: implied6502
    Case &HBB: implied6502
    Case &HBC: absx6502
    Case &HBD: absx6502
    Case &HBE: absy6502
    Case &HBF: implied6502
    Case &HC0: immediate6502
    Case &HC1: indx6502
    Case &HC2: implied6502
    Case &HC3: implied6502
    Case &HC4: zp6502
    Case &HC5: zp6502
    Case &HC6: zp6502
    Case &HC7: implied6502
    Case &HC8: implied6502
    Case &HCB: implied6502
    Case &HCC: abs6502
    Case &HCD: abs6502
    Case &HCE: abs6502
    Case &HCF: implied6502
    Case &HD1: indy6502
    Case &HD2: indzp6502
    Case &HD5: zpx6502
    Case &HD6: zpx6502
    Case &HD9: absy6502
    Case &HDD: absx6502
    Case &HDE: absx6502
    Case &HE1: indx6502
    Case &HE4: zp6502
    Case &HE5: zp6502
    Case &HE6: zp6502
    Case &HE9: immediate6502
    Case &HEC: abs6502
    Case &HED: abs6502
    Case &HEE: abs6502
    Case &HF1: indy6502
    Case &HF2: indzp6502
    Case &HF5: zpx6502
    Case &HF6: zpx6502
    Case &HF9: absy6502
    Case &HFD: absx6502
    Case &HFE: absx6502
    Case &H1: indx6502
    Case &H4: zp6502
    Case &H6: zp6502
    Case &HC: abs6502
    Case &HD: abs6502
    Case &HE: abs6502
    Case &H11: indy6502
    Case &H12: indzp6502
    Case &H14: zp6502
    Case &H15: zpx6502
    Case &H16: zpx6502
    Case &H19: absy6502
    Case &H1C: abs6502
    Case &H1D: absx6502
    Case &H1E: absx6502
    Case &H21: indx6502
    Case &H24: zp6502
    Case &H2D: abs6502
    Case &H2E: abs6502
    Case &H31: indy6502
    Case &H32: indzp6502
    Case &H34: zpx6502
    Case &H35: zpx6502
    Case &H36: zpx6502
    Case &H39: absy6502
    Case &H3C: absx6502
    Case &H3D: absx6502
    Case &H3E: absx6502
    Case &H41: indx6502
    Case &H4E: abs6502
    Case &H50: relative6502
    Case &H51: indy6502
    Case &H52: indzp6502
    Case &H55: zpx6502
    Case &H56: zpx6502
    Case &H5E: absx6502
    Case &H61: indx6502
    Case &H64: zp6502
    Case &H6C: indirect6502
    Case &H6D: abs6502
    Case &H6E: abs6502
    Case &H71: indy6502
    Case &H72: indzp6502
    Case &H74: zpx6502
    Case &H76: zpx6502
    Case &H79: absy6502
    Case &H7C: indabsx6502
    Case &H7D: absx6502
    Case &H7E: absx6502
    Case &H80: relative6502
    Case &H81: indx6502
    Case &H89: immediate6502
    Case Else
  End Select
End Function

Public Function and6502()
  adrmode (opcode)
  value = get6502memory(savepc)
  A = (A And value)
  If (A) Then
    P = (P And &HFD)
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = (P And &H7F)
  End If
End Function
Public Function asl6502()
  adrmode (opcode)
  value = get6502memory(savepc)
  
  P = (P And &HFE) Or ((value \ 128) And &H1)
  value = value * 2
  Call put6502memory((savepc), value And &HFF)
  If (value) Then
    P = (P And &HFD)
  Else
    P = P Or &H2
  End If
  If (value And &H80) Then
    P = P Or &H80
  Else
    P = (P And &H7F)
  End If
End Function


Public Function asla6502()
  If A > 0 Then
    A = A
  End If
  
  P = (P And &HFE) Or ((A \ 128) And &H1)
  A = (A * 2) And &HFF
  
  If (A) Then
    P = (P And &HFD)
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = (P And &H7F)
  End If
End Function

Public Function bcc6502()
  If ((P And &H1) = 0) Then
    adrmode (opcode)
    PC = PC + savepc
    clockticks6502 = clockticks6502 + 1
  Else
    value = gameImage(PC)
    PC = PC + 1
  End If
End Function

Public Function bcs6502()
  If (P And &H1) Then
    adrmode (opcode)
    PC = PC + savepc
    clockticks6502 = clockticks6502 + 1
  Else
    value = gameImage(PC)
    PC = PC + 1
  End If
End Function

Public Function beq6502()
  If (P And &H2) Then
    adrmode (opcode)
    PC = PC + savepc
    clockticks6502 = clockticks6502 + 1
  Else
    value = gameImage(PC)
    PC = PC + 1
  End If
End Function

Public Function bit6502()
  adrmode (opcode)
  value = get6502memory(savepc)
  
  If (value And A) Then
    P = (P And &HFD)
    Else
    P = P Or &H2
  End If
  P = ((P And &H3F) Or (value And &HC0))
End Function

Public Function bmi6502()
  If (P And &H80) Then
    adrmode (opcode)
    PC = PC + savepc
    clockticks6502 = clockticks6502 + 1
  Else
    value = gameImage(savepc)
    PC = PC + 1
  End If
End Function

Public Function bne6502()
  If ((P And &H2) = 0) Then
    adrmode (opcode)
    PC = PC + savepc
    clockticks6502 = clockticks6502 + 1
  Else
    value = gameImage(PC)
    PC = PC + 1
  End If
End Function

Public Function bpl6502()
  If ((P And &H80) = 0) Then
    adrmode (opcode)
    PC = PC + savepc
    clockticks6502 = clockticks6502 + 1
  Else
    value = gameImage(PC)
    PC = PC + 1
  End If
End Function

Public Function brk6502()
  PC = PC + 1
  
  put6502memory &H100 + S, (PC \ 256) And &HFF
  S = (S - 1) And &HFF
    
  put6502memory &H100 + S, (PC And &HFF)
  S = (S - 1) And &HFF
  
  put6502memory &H100 + S, P
  S = (S - 1) And &HFF
  
  P = P Or &H14
  PC = gameImage(65534 And &H7FFF) Or gameImage(65535 And &H7FFF) * &H100&
End Function

Public Function bvc6502()
  If ((P And &H40) = 0) Then
    adrmode (opcode)
    PC = PC + savepc
    clockticks6502 = clockticks6502 + 1
  Else
    value = gameImage(PC)
    PC = PC + 1
  End If
End Function

Public Function bvs6502()
  If (P And &H40) Then
    adrmode (opcode)
    PC = PC + savepc
    clockticks6502 = clockticks6502 + 1
  Else
    value = gameImage(PC)
    PC = PC + 1
  End If
End Function

Public Function clc6502()
  P = P And &HFE
End Function

Public Function cld6502()
  P = P And &HF7
End Function

Public Function cli6502()
  P = P And &HFB
End Function

Public Function clv6502()
  P = P And &HBF
End Function

Public Function cmp6502()
  adrmode (opcode)
  value = get6502memory(savepc)
  
  If (A + &H100 - value) > &HFF Then
    P = P Or &H1
  Else
    P = (P And &HFE)
  End If
  
  value = (A + &H100 - value) And &HFF
  If (value) Then
      P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (value And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function

Public Function cpx6502()
  adrmode (opcode)
  value = get6502memory(savepc)
      
  If (X + &H100 - value > &HFF) Then
    P = P Or &H1
  Else
    P = (P And &HFE)
  End If
  
  value = (X + &H100 - value) And &HFF
  If (value) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  
  If (value And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function

Public Function cpy6502()
  adrmode (opcode)
  value = get6502memory(savepc)
      
  If (Y + &H100 - value > &HFF) Then
    P = (P Or &H1)
  Else
    P = (P And &HFE)
  End If
    value = (Y + &H100 - value) And &HFF
  If (value) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (value And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function

Public Function dec6502()
  adrmode (opcode)
      
  gameImage(savepc) = (gameImage(savepc) - 1) And &HFF
      
  value = get6502memory(savepc)
  If (value) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (value & &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function
Public Function dex6502()
  X = (X - 1) And &HFF
  If (X) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (X And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function

Public Function dey6502()
  Y = (Y - 1) And &HFF
      
  If (Y) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (Y And &H80) Then
    P = P Or &H80
  Else
    P = (P And &H7F)
  End If
End Function

Public Function eor6502()
  adrmode (opcode)
  A = A Xor get6502memory(savepc)
  
  If (A) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (A And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function

Public Function inc6502()
  adrmode (opcode)
  gameImage(savepc) = (gameImage(savepc) + 1) And &HFF
  
  value = get6502memory(savepc)
  
  If (value) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (value And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function

Public Function inx6502()
  X = (X + 1) And &HFF
      
  If (X) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (X And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function

Public Function iny6502()
  Y = (Y + 1) And &HFF
  If (Y) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (Y And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function
Public Function jmp6502()
  adrmode (opcode)
  PC = savepc
End Function
Public Function jsr6502()
  PC = PC + 1
  put6502memory &H100 + S, PC \ 256
  S = (S - 1) And &HFF
  put6502memory &H100 + S, (PC And &HFF)
  S = (S - 1) And &HFF
  PC = PC - 1
  adrmode (opcode)
  PC = savepc
End Function
Public Function lda6502()
  adrmode (opcode)
  A = get6502memory(savepc)
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function ldx6502()
  adrmode (opcode)
  X = get6502memory(savepc)
      
  If (X) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (X And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function
Public Function ldy6502()
  adrmode (opcode)
  Y = get6502memory(savepc)
  
  If (Y) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (Y And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function
Public Function lsr6502()
  adrmode (opcode)
  value = get6502memory(savepc)
         
  P = ((P And &HFE) Or (value And &H1))
  
  value = value \ 2
  put6502memory (savepc), value And &HFF
  
  If (value = Not 0) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If ((value And &H80) = &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function
Public Function lsra6502()
  P = (P And &HFE) Or (A And &H1)
  A = A \ 2
  If (A) Then
    P = (P And &HFD)
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function nop6502()
'TS: Implemented complex code structure ;)
End Function
Public Function ora6502()
  adrmode (opcode)
  A = A Or get6502memory(savepc)
      
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function pha6502()
  gameImage(&H100 + S) = A
  S = (S - 1) And &HFF
End Function
Public Function php6502()
  gameImage(&H100 + S) = P
  S = (S - 1) And &HFF
End Function
Public Function pla6502()
  S = (S + 1) And &HFF
  A = gameImage(S + &H100)
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function plp6502()
  S = (S + 1) And &HFF
  P = gameImage(S + &H100) Or &H20
End Function
Public Function rol6502()
  saveflags = (P And &H1)
  adrmode (opcode)
  value = get6502memory(savepc)
      
  If value > 0 Then
    value = value
  End If
  P = (P And &HFE) Or ((value \ 128) And &H1)
  If value >= 126.5 Then
  value = 0
  Else
  value = value * 2
  End If
  value = value Or saveflags
  xx& = put6502memory((savepc), value)
  If (value) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (value And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function rola6502()
  saveflags = (P And &H1)
  P = (P And &HFE) Or ((A \ &H80) And &H1)
  A = (A * 2) And &HFF
  A = A Or saveflags
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function ror6502()
  saveflags = (P And &H1)
  adrmode (opcode)
  value = get6502memory(savepc)
      
  If value > 0 Then
    value = value
  End If
  P = (P And &HFE) Or (value And &H1)
  value = value \ 2
  If (saveflags) Then
    value = value Or &H80
  End If
  put6502memory (savepc), (value And &HFF)
  If (value) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (value And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function rora6502()
  saveflags = (P And &H1)
  P = (P And &HFE) Or (A And &H1)
  A = (A \ 2) And &HFF
  
  If (saveflags) Then
    A = A Or &H80
  End If
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function rti6502()
  S = (S + 1) And &HFF
  P = gameImage(S + &H100) Or &H20
  S = (S + 1) And &HFF
  PC = gameImage(S + &H100)
  S = (S + 1) And &HFF
  PC = PC Or (gameImage(S + &H100) * 256)
End Function

Public Function rts6502()
  S = (S + 1) And &HFF
  PC = gameImage(S + &H100)
  S = (S + 1) And &HFF
  PC = PC Or (gameImage(S + &H100) * 256)
  PC = PC + 1
End Function

Public Function sbc6502()
  adrmode (opcode)
  value = get6502memory(savepc) Xor &HFF
  
  saveflags = (P And &H1)
  
  sum = A
  sum = (sum + value) And &HFF
  sum = (sum + (saveflags * &H10)) And &HFF
  
  If ((sum > &H7F) Or (sum <= -&H80)) Then
    P = P Or &H40
  Else
    P = P And &HBF
  End If
  
  sum = A + (value + saveflags)
  
  If (sum > &HFF) Then
    P = P Or &H1
  Else
    P = P And &HFE
  End If
  
  A = (sum And &HFF)
  If (P And &H8) Then
    A = A - &H66
    P = P And &HFE
    If ((A And &HF) > &H9) Then
      A = A + &H6
    End If
    If ((A And &HF0) > &H90) Then
      A = A + &H60
      P = P Or H01
    End If
  Else
    clockticks6502 = clockticks6502 + 1
  End If
  
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function sec6502()
  P = P Or &H1
End Function
Public Function sed6502()
  P = P Or &H8
End Function
Public Function sei6502()
  P = P Or &H4
End Function
Public Function sta6502()
  adrmode (opcode)
  put6502memory (savepc), A
End Function
Public Function stx6502()
  adrmode (opcode)
  put6502memory (savepc), X
End Function
Public Function sty6502()
  adrmode (opcode)
  put6502memory (savepc), Y
End Function
Public Function tax6502()
  X = A
  If (X) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (X And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function

Public Function tay6502()
  Y = A
  If (Y) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (Y And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function tsx6502()
  X = S
  If (X) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (X And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function

Public Function txa6502()
  A = X
  If (A) Then
    P = (P And &HFD)
  Else
    P = (P Or &H2)
  End If
  If (A And &H80) Then
    P = (P Or &H80)
  Else
    P = (P And &H7F)
  End If
End Function
Public Function txs6502()
  S = X
End Function

Public Function tya6502()
  A = Y
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function bra6502()
  adrmode (opcode)
  PC = PC + savepc
  clockticks6502 = clockticks6502 + 1
End Function
Public Function dea6502()
  A = (A - 1) And &HFF
  
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function ina6502()
  A = (A + 1) And &HFF
      
  If (A) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (A And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function phx6502()
  put6502memory &H100 + S, X
  S = (S - 1) And &HFF
End Function

Public Function plx6502()
  S = (S + 1) And &HFF
  X = gameImage(1 + &H100)
  If (X) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (X And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function phy6502()
  xx& = put6502memory(&H100 + S, Y)
  S = (S - 1) And &HFF
End Function
Public Function ply6502()
  S = (S + 1) And &HFF
  
  Y = gameImage(S + &H100)
  If (Y) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
  If (Y And &H80) Then
    P = P Or &H80
  Else
    P = P And &H7F
  End If
End Function
Public Function stz6502()
  adrmode (opcode)
  put6502memory (savepc), 0
End Function

Public Function tsb6502()
  Dim tmp As Byte
      
  adrmode (opcode)
  tmp = get6502memory(savepc) Or A
  put6502memory (savepc), tmp
      
  If (tmp) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
End Function
Public Function trb6502()
  Dim tmp As Byte
      
  adrmode (opcode)
  tmp = get6502memory(savepc) And (A Xor &HFF)
  put6502memory (savepc), tmp
  
  If (tmp) Then
    P = P And &HFD
  Else
    P = P Or &H2
  End If
End Function
