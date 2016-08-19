object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 364
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 650
    Height = 232
    ActivePage = tsUtils_Strings
    Align = alClient
    TabOrder = 0
    object tsUtils_Strings: TTabSheet
      Caption = 'tsUtils_Strings'
      object btnGetStrValueOfName: TButton
        Left = 16
        Top = 3
        Width = 145
        Height = 25
        Caption = 'btnGetStrValueOfName'
        TabOrder = 0
        OnClick = btnGetStrValueOfNameClick
      end
    end
  end
  object mmoLog: TMemo
    Left = 0
    Top = 232
    Width = 650
    Height = 132
    Align = alBottom
    Lines.Strings = (
      'mmoLog')
    TabOrder = 1
  end
end
