Attribute VB_Name = "M6502AddrModes"
'Greatly modified by Tobias Strömstedt in march 1998.
'Implemented Interrupt support
'Correct address masking
'Random numbers
'Full support of all addressing modes
'Bug corrections

' My 2nd try at a 6502 emulator. This one is based on Neil Bradley's
' 6502 C source. I don't like Marat's because it's slower...
' This emulator features all 6502 addressing modes. Also
' all 157 6502 OpCodes are supported.
' The emulator is *VERY* buggy still and the addressing modes
' are called at the wrong times.
' This was written for all the Visual Basic programmers that
' want to write a CPU emulator but don't know where to start.
' I didn't write most of this, it was written by some guy, and the
' source was used in EMU by Neil Bradley. Thanks for donating this
' Neil =).

Public AddressMask As Integer

'Registers and tempregisters
Public A As Byte
Public X As Byte
Public Y As Byte
Public F As Byte
Public S As Byte
Public P As Byte
Public PC As Long
Public savepc As Long
Public value As Byte
Public sum As Integer, saveflags As Integer
Public help As Integer

Public Const DEBUGGING = 0

Public opcode As Byte
Public clockticks6502 As Long
Public TimerTicks As Long
Public opcodes As Long
' arrays
Public TICKS(0 To 256) As Integer
Public addrmode(0 To 256)
Public instruction(0 To 256)
Public gameImage(0 To 131744) As Byte

Public TrapBadOps As Integer  ' Quit emulation when OpCode is invalid...
Public CPUPaused As Integer  ' Is the CPU Paused
Public CPURunning As Integer ' Is the CPU running?
Public ROMLoaded As Integer  ' Is a Game loaded?
Public Trace As Integer      ' Print Current OpCode?

Public Function implied6502()
  
End Function

Public Function indabsx6502()
  help = gameImage(PC) + (gameImage(PC + 1) * 256) + X
  savepc = gameImage(help And &HFF) + (gameImage(help + 1) * 256)
End Function
Public Function indx6502()
'TS: Changed PC++ and removed '(?)
  value = (gameImage(PC) + X) And &HFF
  PC = PC + 1
  savepc = gameImage(value And &HFF) + (gameImage(value + 1) * &H100&)
End Function
Public Function indy6502()
'TS: Changed PC++ and == to != (If then else)
  value = get6502memory(PC)
  PC = PC + 1
      
  savepc = get6502memory(value And &HFF) + (get6502memory(value + 1) * &H100&)
  If (TICKS(opcode) = 5) And ((savepc \ 256) = ((savepc + Y) \ 256)) Then clockticks6502 = clockticks6502 + 1
  savepc = savepc + Y
End Function

Public Function zpx6502()
'TS: Rewrote everything!
'Overflow stupid check
  savepc = get6502memory(PC)
  savepc = savepc + X
  PC = PC + 1
  savepc = savepc And &HFF
End Function

Public Function exec6502()
  
  opcode = gameImage(PC)  ' Fetch Next Operation
  PC = PC + 1
  
  clockticks6502 = clockticks6502 + TICKS(opcode)
Select Case opcode
    Case &H9D, &H95, &H91, &H85, &H85, &H8D, &H92, &H99: sta6502
    Case &H29, &H21, &H25, &H2D, &H31, &H32, &H35, &H39, &H3D: and6502
    Case &HB5, &HA5, &HA9, &HAD, &HB1, &HA1, &HB2, &HB9, &HBD: lda6502
    Case &HC9, &HC1, &HC5, &HD1, &HD2, &HD5, &HD9, &HDD: cmp6502
    Case &H45, &H41, &H49, &H4D, &H4D, &H51, &H52, &H55, &H59, &H5D: eor6502
    Case &H46, &H4E, &H56, &H5E: lsr6502
    Case &HA0, &HA4, &HB4, &HAC, &HBC: ldy6502
    Case &H65, &H69, &H61, &H6D, &H71, &H72, &H75, &H79, &H7D: adc6502
    Case &H1B, &H1F, &H22, &H23, &H27, &H2B, &H2F, &H33, &H37, &H3B, &H3F, &H42, &H43, &H44, &H47, &H4B, &H4F, &H53, &H54, &H57, &H5B, &H5C, &H5F, &H62, &H63, &H67, &H6B, &H6F, &H73, &H77, &H7B, &H7F, &H82, &H83, &H87, &H8B, &H8F, &H93, &H97, &HA7, &HAB, &HAF, &HB3, &HB3, &HB7, &HBB, &HBF, &HC2, &HC3, &HC7, &HCB, &HCF, &HD3, &HD4, &HD4, &HD7, &HDB, &HDC, &HDF, &HE2, &HE3, &HE7, &HEA, &HEB, &HEF, &HF3, &HF4, &HF7, &HFB, &HFC, &HFF, &H2, &H3, &H7, &HB, &HF, &H13, &H17: nop6502
    Case &HE0, &HE4, &HE4: cpx6502
    Case &H9, &H5, &H19, &H1D, &H1, &HD, &H11, &H12, &H15: ora6502
    Case &H4C, &H6C, &H7C: jmp6502
    Case &HA6, &HA2, &HAE, &HB6, &HBE: ldx6502
    Case &H8C, &H84, &H94: sty6502
    Case &HC0, &HC4, &HCC: cpy6502
    Case &HE5, &HE1, &HE5, &HE9, &HED, &HF1, &HF2, &HF5, &HF9, &HFD: sbc6502
    Case &HE6, &HEE, &HF6, &HFE, &HFE: inc6502
    Case &H1C, &H14: trb6502
    Case &H1E, &H6, &HE, &H16: asl6502
    Case &H24, &H2C, &H3C, &H34, &H89: bit6502
    Case &H26, &H2E, &H36, &H3E: rol6502
    Case &H66, &H6E, &H76, &H7E: ror6502
    Case &H64, &H74, &H9C, &H9E: stz6502
    Case &H4, &HC: tsb6502
    Case &H86, &H8E, &H96: stx6502
    Case &HD6, &HDE, &HCE, &HC6: dec6502
    Case &HB0: bcs6502
    Case &H20: jsr6502
    Case &H60: rts6502
    Case &H90: bcc6502
    Case &H10: bpl6502
    Case &HD0: bne6502
    Case &HF0: beq6502
    Case &HCA: dex6502
    Case &H18: clc6502
    Case &H9A: txs6502
    Case &HE8: inx6502
    Case &HA8: tay6502
    Case &H38: sec6502
    Case &H98: tya6502
    Case &H88: dey6502
    Case &H8: php6502
    Case &HA: asla6502
    Case &H1A: ina6502
    Case &H2A: rola6502
    Case &H30: bmi6502
    Case &H28: plp6502
    Case &H3A: dea6502
    Case &H40: rti6502
    Case &H48: pha6502
    Case &H4A: lsra6502
    Case &H50: bvc6502
    Case &H58: cli6502
    Case &H5A: phy6502
    Case &H0: brk6502
    Case &H68: pla6502
    Case &H6A: rora6502
    Case &H7A: ply6502
    Case &H78: sei6502
    Case &H70: bvs6502
    Case &HFA: plx6502
    Case &H80: bra6502
    Case &HF8: sed6502
    Case &HC8: iny6502
    Case &HD8: cld6502
    Case &HDA: phx6502
    Case &HB8: clv6502
    Case &H8A: txa6502
    Case &HAA: tax6502
    Case &HBA: tsx6502
  End Select
  If clockticks6502 >= 8000 Then
    clockticks6502 = 0
    If (P And &H4) = 0 Then
        irq6502
    End If
  End If
End Function
Public Function indzp6502()
'Added pc=pc+1, and (value+1) (Why Don?)
  value = gameImage(PC)
  PC = PC + 1
  savepc = gameImage(value And &HFF) + (get6502memory(value + 1) * &H100&)
End Function

Public Function zpy6502()
'TS: Added PC=PC+1
      savepc = gameImage(PC) + Y
      PC = PC + 1
      savepc = savepc And &HFF
End Function

Public Function absy6502()
'TS: Changed to != instead of == (Look at absx for more details)
  savepc = gameImage(PC) + (gameImage(PC + 1) * &H100&)
  PC = PC + 2
  savepc = savepc + Y
End Function

Public Function get6502memory(addr As Long) As Byte
    get6502memory = ReadMillipedeHW(addr)
End Function
Public Function immediate6502()
  savepc = PC
  PC = PC + 1
End Function
Public Function indirect6502()
  help = gameImage(PC) + (gameImage(PC + 1) * &H100&)
  savepc = gameImage(help And &HFF) + (gameImage(help + 1) * &H100&)
  PC = PC + 2
End Function

Public Function absx6502()
'TS: Changed to if then else instead of if then (!= instead of ==)
  savepc = gameImage(PC) + (gameImage(PC + 1) * &H100&)
  PC = PC + 2
  savepc = savepc + X
End Function
Public Function put6502memory(addr As Long, value As Byte)
    If addr = &H2600 Then
        clockticks6502 = 0
    End If
    If addr >= &H4000& And addr <= &H7FFF& Then
        ' No write here, Protect PrgRom
    Else
       gameImage(addr) = value
    End If
End Function
Public Function abs6502()
  savepc = gameImage(PC) + (gameImage(PC + 1) * &H100&)
  PC = PC + 2
End Function
Public Function relative6502()
'Changed to PC++ and == to != (If then else)
  savepc = gameImage(PC)
  PC = PC + 1

  If (savepc And &H80) Then savepc = savepc - &H100&
End Function

Public Function reset6502()
  A = 0: X = 0: Y = 0: P = 0
  P = P Or &H34
  S = &HFF
  PC = gameImage(65532 And &H7FFF) Or (gameImage(65533 And &H7FFF) * &H100&)
  Debug.Print PC
End Function

Public Function zp6502()
  savepc = gameImage(PC)
  PC = PC + 1
End Function

Public Function irq6502()
   ' Maskable interrupt
   put6502memory &H100 + S, (PC \ &H100&)
   S = (S - 1) And &HFF
   put6502memory &H100 + S, (PC And &HFF)
   S = (S - 1) And &HFF
   put6502memory &H100 + S, P
   S = (S - 1) And &HFF
   P = P Or &H4
   PC = gameImage(65534 And &H7FFF) Or gameImage(65535 And &H7FFF) * 2 ^ 8
End Function

Public Function nmi6502()
'TS: Changed PC>>8 to / not *
  put6502memory &H100 + S, (PC \ &H100&)
  S = (S - 1) And &HFF
  put6502memory &H100 + S, (PC And &HFF)
  S = (S - 1) And &HFF
  put6502memory &H100 + S, P
  P = P Or &H4
  S = (S - 1) And &HFF
  PC = gameImage(65530 And &H7FFF) Or (gameImage(65531 And &H7FFF) * &H100&)
End Function
