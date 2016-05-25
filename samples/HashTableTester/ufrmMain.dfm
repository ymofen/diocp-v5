object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 387
  ClientWidth = 655
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnTester: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'btnTester'
    TabOrder = 0
    OnClick = btnTesterClick
  end
  object mmoLog: TMemo
    Left = 24
    Top = 96
    Width = 609
    Height = 265
    Lines.Strings = (
      'mmoLog')
    TabOrder = 1
  end
  object btnTester2: TButton
    Left = 200
    Top = 48
    Width = 75
    Height = 25
    Caption = 'btnTester2'
    TabOrder = 2
    OnClick = btnTester2Click
  end
  object edtFindAddr: TEdit
    Left = 200
    Top = 21
    Width = 153
    Height = 21
    TabOrder = 3
    Text = '3524784'
  end
  object edtKey: TEdit
    Left = 384
    Top = 21
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '1400'
  end
  object dlgOpen: TOpenDialog
    Left = 320
    Top = 200
  end
end
