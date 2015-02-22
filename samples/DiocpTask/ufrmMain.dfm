object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 378
  ClientWidth = 713
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnPostTask: TButton
    Left = 8
    Top = 8
    Width = 163
    Height = 25
    Caption = #25237#36882#19968#20010#20219#21153#22312#20027#32447#31243#25191#34892
    TabOrder = 0
    OnClick = btnPostTaskClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 136
    Width = 625
    Height = 218
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object SpeedTester: TButton
    Left = 8
    Top = 39
    Width = 163
    Height = 25
    Caption = #36895#24230#27979#35797
    TabOrder = 2
    OnClick = SpeedTesterClick
  end
  object btnState: TButton
    Left = 177
    Top = 8
    Width = 144
    Height = 25
    Caption = #33719#21462#24037#20316#32447#31243#29366#24577
    TabOrder = 3
    OnClick = btnStateClick
  end
  object btnSignal: TButton
    Left = 368
    Top = 39
    Width = 129
    Height = 25
    Caption = #35302#21457#19968#20010#20449#21495
    TabOrder = 4
    OnClick = btnSignalClick
  end
  object btnRegister: TButton
    Left = 368
    Top = 8
    Width = 129
    Height = 25
    Caption = #27880#20876#19968#20010#20449#21495
    TabOrder = 5
    OnClick = btnRegisterClick
  end
  object btnUnRegister: TButton
    Left = 512
    Top = 8
    Width = 129
    Height = 25
    Caption = #21462#28040#27880#20876#19968#20010#20449#21495
    TabOrder = 6
    OnClick = btnUnRegisterClick
  end
end
