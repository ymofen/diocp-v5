object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #26381#21153#31471
  ClientHeight = 255
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 48
    Height = 13
    Caption = #32465#23450#31471#21475
  end
  object Label2: TLabel
    Left = 16
    Top = 208
    Width = 166
    Height = 13
    Caption = #26368#21518#19968#27425#25509#25910#30340#23458#25143#31471#31471#21475#21644'Ip'
  end
  object Label3: TLabel
    Left = 16
    Top = 230
    Width = 10
    Height = 13
    Caption = 'IP'
  end
  object Label4: TLabel
    Left = 168
    Top = 230
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object Edit1: TEdit
    Left = 78
    Top = 13
    Width = 67
    Height = 21
    TabOrder = 0
    Text = '18882'
  end
  object Button1: TButton
    Left = 168
    Top = 10
    Width = 75
    Height = 25
    Caption = #28857#20987#24320#21551
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 48
    Width = 353
    Height = 145
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 32
    Top = 227
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object Edit3: TEdit
    Left = 194
    Top = 227
    Width = 63
    Height = 21
    TabOrder = 4
  end
  object Button2: TButton
    Left = 263
    Top = 225
    Width = 75
    Height = 25
    Caption = #36820#22238#28040#24687
    TabOrder = 5
    OnClick = Button2Click
  end
end
