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
    object Label2: TLabel
      Left = 22
      Top = 15
      Width = 43
      Height = 13
      Caption = 'BlockSize'
    end
    object btnCheckBounds: TButton
      Left = 592
      Top = 40
      Width = 89
      Height = 25
      Caption = 'btnCheckBounds'
      TabOrder = 0
      OnClick = btnCheckBoundsClick
    end
    object btnClear: TButton
      Left = 464
      Top = 80
      Width = 75
      Height = 25
      Caption = 'btnClear'
      TabOrder = 1
      OnClick = btnClearClick
    end
    object btnFreePool: TButton
      Left = 246
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btnFreePool'
      TabOrder = 2
      OnClick = btnFreePoolClick
    end
    object btnNewPool: TButton
      Left = 144
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btnNewPool'
      TabOrder = 3
      OnClick = btnNewPoolClick
    end
    object btnOutOfBounds: TButton
      Left = 456
      Top = 40
      Width = 118
      Height = 25
      Caption = 'btnOutOfBounds'
      TabOrder = 4
      OnClick = btnOutOfBoundsClick
    end
    object btnPoolInfo: TButton
      Left = 304
      Top = 40
      Width = 75
      Height = 25
      Caption = 'btnPoolInfo'
      TabOrder = 5
      OnClick = btnPoolInfoClick
    end
    object btnSimpleTester: TButton
      Left = 161
      Top = 40
      Width = 137
      Height = 25
      Caption = 'btnSimpleTester'
      TabOrder = 6
      OnClick = btnSimpleTesterClick
    end
    object btnSpeedTester: TButton
      Left = 592
      Top = 80
      Width = 89
      Height = 25
      Caption = 'btnSpeedTester'
      TabOrder = 7
      OnClick = btnSpeedTesterClick
    end
    object btnSpinLocker: TButton
      Left = 687
      Top = 40
      Width = 75
      Height = 25
      Caption = 'btnSpinLocker'
      TabOrder = 8
      OnClick = btnSpinLockerClick
    end
    object btnThreadTester: TButton
      Left = 161
      Top = 80
      Width = 137
      Height = 25
      Caption = 'btnThreadTester'
      TabOrder = 9
      OnClick = btnThreadTesterClick
    end
    object btnThreadTester2: TButton
      Left = 304
      Top = 80
      Width = 137
      Height = 25
      Caption = 'btnThreadTester2'
      TabOrder = 10
      OnClick = btnThreadTester2Click
    end
    object edtThread: TEdit
      Left = 434
      Top = 11
      Width = 71
      Height = 21
      TabOrder = 11
      Text = '5'
    end
    object btnSimpleBlockBuffer: TButton
      Left = 8
      Top = 122
      Width = 137
      Height = 25
      Caption = 'btnSimpleBlockBuffer'
      TabOrder = 12
      OnClick = btnSimpleBlockBufferClick
    end
    object edtBlockSize: TEdit
      Left = 79
      Top = 11
      Width = 59
      Height = 21
      TabOrder = 13
      Text = '4096'
    end
    object btnBlockBufferTester: TButton
      Left = 161
      Top = 122
      Width = 145
      Height = 25
      Caption = 'btnBlockBufferTester'
      TabOrder = 14
      OnClick = btnBlockBufferTesterClick
    end
    object btnAttachObject: TButton
      Left = 312
      Top = 122
      Width = 129
      Height = 25
      Caption = 'btnAttachObject'
      TabOrder = 15
      OnClick = btnAttachObjectClick
    end
    object btnRefBuffer: TButton
      Left = 464
      Top = 122
      Width = 110
      Height = 25
      Caption = 'btnRefBuffer'
      TabOrder = 16
      OnClick = btnRefBufferClick
    end
  end
end
