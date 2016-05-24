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
end
