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
  object btnWriterLog: TButton
    Left = 8
    Top = 65
    Width = 75
    Height = 25
    Caption = 'btnWriterLog'
    TabOrder = 0
    OnClick = btnWriterLogClick
  end
  object btnViewLog: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = 'btnViewLog'
    TabOrder = 1
    OnClick = btnViewLogClick
  end
  object edtAccessToken: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'logtoken-001'
  end
  object edtLogContent: TEdit
    Left = 104
    Top = 67
    Width = 281
    Height = 21
    TabOrder = 3
    Text = #26085#24535#20869#23481
  end
end
