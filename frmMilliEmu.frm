VERSION 5.00
Begin VB.Form frmMilliEmu 
   BackColor       =   &H8000000A&
   Caption         =   "Form1"
   ClientHeight    =   3915
   ClientLeft      =   60
   ClientTop       =   630
   ClientWidth     =   3630
   Icon            =   "frmMilliEmu.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   261
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   242
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picScreen 
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000008&
      Height          =   4215
      Left            =   0
      ScaleHeight     =   277
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   229
      TabIndex        =   0
      Top             =   0
      Width           =   3495
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuFileExit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuFrameSkip 
      Caption         =   "&FrameSkip"
      Begin VB.Menu mnuFrameSkip1 
         Caption         =   "1"
      End
      Begin VB.Menu mnuFrameSkip2 
         Caption         =   "2"
      End
      Begin VB.Menu mnuFrameSkip3 
         Caption         =   "3"
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "&Help"
      Begin VB.Menu mnuHelpAbout 
         Caption         =   "About"
      End
   End
End
Attribute VB_Name = "frmMilliEmu"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
aKeyboard(KeyCode) = 1
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
aKeyboard(KeyCode) = 0
End Sub

Private Sub Form_Load()
  Dim FrameTime As Double
  Dim TotalTime As Double
  Dim NumberOfFrames As Integer
Caption = "milli·Emu"
Show
Form_Resize
mnuFrameSkip3_Click
DoEvents
MilliMain
'DisplayCharSet
reset6502
NumberOfFrames = 0
Do While aKeyboard(vbKeyEscape) = 0
    FrameTime = Timer
    For test = 1 To 4000
        exec6502
    Next test
    gameImage(&H2000) = &H80
    If Frames Mod FramesToSkip = 0 Then
        RedrawScreen
        Frames = Frames + 1
    Else
        Frames = Frames + 1
    End If
    TotalTime = TotalTime + (Timer - FrameTime)
    NumberOfFrames = NumberOfFrames + 1
    Caption = "milli·Emu - " & Format(1 / (TotalTime / NumberOfFrames), "0.0") & " FPS"
    DoEvents
Loop
End Sub

Private Sub Form_Resize()
If WindowState = vbMinimized Then Exit Sub
picScreen.Width = frmMilliEmu.ScaleWidth
picScreen.Height = frmMilliEmu.ScaleHeight
End Sub

Private Sub Form_Unload(Cancel As Integer)
End
End Sub

Private Sub mnuFileExit_Click()
Form_Unload 0
End Sub

Private Sub mnuFrameSkip1_Click()
mnuFrameSkip2.Checked = False
mnuFrameSkip3.Checked = False
mnuFrameSkip1.Checked = True
FramesToSkip = 1
End Sub

Private Sub mnuFrameSkip2_Click()
mnuFrameSkip1.Checked = False
mnuFrameSkip2.Checked = True
mnuFrameSkip3.Checked = False
FramesToSkip = 2
End Sub

Private Sub mnuFrameSkip3_Click()
mnuFrameSkip1.Checked = False
mnuFrameSkip2.Checked = False
mnuFrameSkip3.Checked = True
FramesToSkip = 3
End Sub

Private Sub mnuHelpAbout_Click()
Dim msg As String
msg = "milliEmu v0.0.2" & vbCrLf
msg = msg & "__________________" & vbCrLf
msg = msg & vbCrLf
msg = msg & "By Don Jarrett, 1999" & vbCrLf
msg = msg & "About 2 hours of work and a lot" & vbCrLf
msg = msg & "of frustration. Hope you enjoy." & vbCrLf & vbCrLf
msg = msg & "© 1999 Don Jarrett"
MsgBox msg, 64, "milliEmu"
End Sub

Private Sub picScreen_KeyDown(KeyCode As Integer, Shift As Integer)
Form_KeyDown KeyCode, 0
End Sub

Private Sub picScreen_KeyUp(KeyCode As Integer, Shift As Integer)
Form_KeyUp KeyCode, 0
End Sub
