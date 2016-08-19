object Form1: TForm1
  Left = 316
  Top = 240
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 880
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mmoLog: TMemo
    Left = 0
    Top = 153
    Width = 880
    Height = 288
    Align = alClient
    Lines.Strings = (
      'mmoLog')
    TabOrder = 0
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 880
    Height = 153
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 368
      Top = 15
      Width = 60
      Height = 13
      Caption = #27979#35797#32447#31243#25968
    end
    object btnClear: TButton
      Left = 464
      Top = 80
      Width = 75
      Height = 25
      Caption = 'btnClear'
      TabOrder = 0
      OnClick = btnClearClick
    end
    object btnFreePool: TButton
      Left = 118
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btnFreePool'
      TabOrder = 1
      OnClick = btnFreePoolClick
    end
    object btnNewPool: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btnNewPool'
      TabOrder = 2
      OnClick = btnNewPoolClick
    end
    object btnPoolInfo: TButton
      Left = 304
      Top = 40
      Width = 75
      Height = 25
      Caption = 'btnPoolInfo'
      TabOrder = 3
    end
    object btnSimpleTester: TButton
      Left = 161
      Top = 40
      Width = 137
      Height = 25
      Caption = 'btnSimpleTester'
      TabOrder = 4
    end
    object btnSpeedTester: TButton
      Left = 592
      Top = 80
      Width = 89
      Height = 25
      Caption = 'btnSpeedTester'
      TabOrder = 5
      OnClick = btnSpeedTesterClick
    end
    object btnThreadDefaultVarsTester: TButton
      Left = 89
      Top = 80
      Width = 209
      Height = 25
      Caption = 'btnThreadDefaultVarsTester'
      TabOrder = 6
      OnClick = btnThreadDefaultVarsTesterClick
    end
    object btnThreadTester2: TButton
      Left = 304
      Top = 80
      Width = 137
      Height = 25
      Caption = 'btnThreadTester2'
      TabOrder = 7
    end
    object edtThread: TEdit
      Left = 434
      Top = 11
      Width = 71
      Height = 21
      TabOrder = 8
      Text = '5'
    end
    object btnDefaultVarsTester: TButton
      Left = 161
      Top = 122
      Width = 145
      Height = 25
      Caption = 'btnDefaultVarsTester'
      TabOrder = 9
      OnClick = btnDefaultVarsTesterClick
    end
    object Button1: TButton
      Left = 768
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 10
      OnClick = Button1Click
    end
  end
end
