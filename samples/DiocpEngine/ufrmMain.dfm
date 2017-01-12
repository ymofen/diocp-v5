object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 360
    Top = 56
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object btnExitAWorker: TButton
    Left = 120
    Top = 24
    Width = 121
    Height = 25
    Caption = 'btnExitAWorker'
    TabOrder = 0
    OnClick = btnExitAWorkerClick
  end
  object btnAddAWorker: TButton
    Left = 120
    Top = 64
    Width = 121
    Height = 25
    Caption = 'btnAddAWorker'
    TabOrder = 1
    OnClick = btnAddAWorkerClick
  end
  object btnStart: TButton
    Left = 120
    Top = 144
    Width = 121
    Height = 25
    Caption = 'btnStart'
    TabOrder = 2
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 296
    Top = 144
    Width = 137
    Height = 25
    Caption = 'btnStop'
    TabOrder = 3
    OnClick = btnStopClick
  end
  object btn1: TButton
    Left = 120
    Top = 200
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 4
    OnClick = btn1Click
  end
  object edtNum: TEdit
    Left = 232
    Top = 202
    Width = 121
    Height = 21
    TabOrder = 5
    Text = '0'
  end
  object tmrIInfo: TTimer
    OnTimer = tmrIInfoTimer
    Left = 72
    Top = 72
  end
end
