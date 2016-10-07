object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 373
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmoOutputs: TMemo
    Left = 8
    Top = 276
    Width = 385
    Height = 89
    Lines.Strings = (
      'mmoOutputs')
    TabOrder = 0
  end
  object btnNumToBytes: TButton
    Left = 184
    Top = 8
    Width = 105
    Height = 25
    Caption = 'btnNumToBytes'
    TabOrder = 1
    OnClick = btnNumToBytesClick
  end
  object edtNum: TEdit
    Left = 16
    Top = 8
    Width = 162
    Height = 21
    TabOrder = 2
    Text = '69532323841'
  end
  object mmoBuffer: TMemo
    Left = 8
    Top = 88
    Width = 281
    Height = 89
    Lines.Strings = (
      '24 10 30 73 10 01')
    TabOrder = 3
  end
  object btnGetBitUInt64: TButton
    Left = 304
    Top = 86
    Width = 113
    Height = 25
    Caption = 'btnGetBitUInt64'
    TabOrder = 4
    OnClick = btnGetBitUInt64Click
  end
  object btnByteAsBinary: TButton
    Left = 304
    Top = 136
    Width = 113
    Height = 25
    Caption = 'btnByteAsBinary'
    TabOrder = 5
    OnClick = btnByteAsBinaryClick
  end
end
