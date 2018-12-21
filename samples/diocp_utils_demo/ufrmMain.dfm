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
      object btnSB: TButton
        Left = 16
        Top = 80
        Width = 75
        Height = 25
        Caption = 'btnSB'
        TabOrder = 1
        OnClick = btnSBClick
      end
      object btnCompare: TButton
        Left = 176
        Top = 80
        Width = 75
        Height = 25
        Caption = 'btnCompare'
        TabOrder = 2
        OnClick = btnCompareClick
      end
    end
    object tsHttpResponseHeader: TTabSheet
      Caption = 'tsHttpResponseHeader'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmoHeader: TMemo
        Left = 0
        Top = 0
        Width = 361
        Height = 204
        Align = alLeft
        Lines.Strings = (
          'HTTP/1.1 404 Not Found'
          'Content-Type: text/plain; charset=utf-8'
          'X-Content-Type-Options: nosniff'
          'Date: Thu, 20 Dec 2018 02:15:38 GMT'
          'Content-Length: 19')
        TabOrder = 0
      end
      object btnDecodeHeader: TButton
        Left = 376
        Top = 3
        Width = 137
        Height = 25
        Caption = 'btnDecodeHeader'
        TabOrder = 1
        OnClick = btnDecodeHeaderClick
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
