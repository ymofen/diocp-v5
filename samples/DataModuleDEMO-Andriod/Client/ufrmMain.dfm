object frmMain: TfrmMain
  Left = 571
  Top = 391
  Width = 848
  Height = 447
  Caption = 'RawTcpClient'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmoSQL: TMemo
    Left = 8
    Top = 40
    Width = 673
    Height = 105
    Lines.Strings = (
      'select * from YesoulChenYu')
    TabOrder = 0
  end
  object btnConnect: TButton
    Left = 264
    Top = 9
    Width = 75
    Height = 25
    Caption = 'btnConnect'
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object edtHost: TEdit
    Left = 8
    Top = 13
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 156
    Top = 13
    Width = 100
    Height = 21
    TabOrder = 3
    Text = '9983'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 151
    Width = 801
    Height = 234
    DataSource = dsMain
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object btnOpen: TButton
    Left = 703
    Top = 38
    Width = 106
    Height = 25
    Caption = 'btnOpen'
    TabOrder = 5
    OnClick = btnOpenClick
  end
  object cdsMain: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 408
    Top = 208
  end
  object dsMain: TDataSource
    DataSet = cdsMain
    Left = 376
    Top = 208
  end
end
