object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 460
  ClientWidth = 929
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
    Width = 929
    Height = 460
    ActivePage = tsDValue
    Align = alClient
    TabOrder = 0
    object tsJSON: TTabSheet
      Caption = 'tsJSON'
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 921
        Height = 65
        Align = alTop
        Caption = 'pnlTop'
        TabOrder = 0
        object btnParseJSON: TButton
          Left = 0
          Top = 21
          Width = 75
          Height = 25
          Caption = 'btnParseJSON'
          TabOrder = 0
          OnClick = btnParseJSONClick
        end
        object btnEncodeJSON: TButton
          Left = 120
          Top = 21
          Width = 75
          Height = 25
          Caption = 'btnEncodeJSON'
          TabOrder = 1
          OnClick = btnEncodeJSONClick
        end
        object btnClear: TButton
          Left = 256
          Top = 21
          Width = 75
          Height = 25
          Caption = 'btnClear'
          TabOrder = 2
          OnClick = btnClearClick
        end
        object btnObjectTester: TButton
          Left = 504
          Top = 21
          Width = 145
          Height = 25
          Caption = 'btnObjectTester'
          TabOrder = 3
          OnClick = btnObjectTesterClick
        end
      end
      object mmoData: TMemo
        Left = 0
        Top = 65
        Width = 921
        Height = 367
        Align = alClient
        Lines.Strings = (
          '{'
          '    "oldValue": null,'
          '    "id": "fcontenttype",'
          '    "newValue": null'
          '}')
        TabOrder = 1
      end
    end
    object tsMultiParts: TTabSheet
      Caption = 'tsMultiParts'
      ImageIndex = 1
      object btnSave: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = 'btnSave'
        TabOrder = 0
        OnClick = btnSaveClick
      end
      object btnParse: TButton
        Left = 16
        Top = 64
        Width = 75
        Height = 25
        Caption = 'btnParse'
        TabOrder = 1
        OnClick = btnParseClick
      end
      object btnParseAFile: TButton
        Left = 16
        Top = 152
        Width = 75
        Height = 25
        Caption = 'btnParseAFile'
        TabOrder = 2
        OnClick = btnParseAFileClick
      end
    end
    object tsMsgPack: TTabSheet
      Caption = 'tsMsgPack'
      ImageIndex = 2
      object btnMsgPackTester: TButton
        Left = 24
        Top = 16
        Width = 129
        Height = 25
        Caption = 'btnMsgPackTester'
        TabOrder = 0
        OnClick = btnMsgPackTesterClick
      end
    end
    object tsDValue: TTabSheet
      Caption = 'tsDValue'
      ImageIndex = 3
      object btnDValue: TButton
        Left = 16
        Top = 32
        Width = 75
        Height = 25
        Caption = 'btnDValue'
        TabOrder = 0
        OnClick = btnDValueClick
      end
      object btnBase64: TButton
        Left = 16
        Top = 104
        Width = 75
        Height = 25
        Caption = 'btnBase64'
        TabOrder = 1
        OnClick = btnBase64Click
      end
      object Button1: TButton
        Left = 168
        Top = 32
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 2
        Visible = False
        OnClick = Button1Click
      end
      object btnDValueCloneFrom: TButton
        Left = 168
        Top = 104
        Width = 113
        Height = 25
        Caption = 'btnDValueCloneFrom'
        TabOrder = 3
        OnClick = btnDValueCloneFromClick
      end
      object btnDValueSetLength: TButton
        Left = 16
        Top = 168
        Width = 129
        Height = 25
        Caption = 'btnDValueSetLength'
        TabOrder = 4
        OnClick = btnDValueSetLengthClick
      end
    end
  end
  object dlgOpenFile: TOpenDialog
    Left = 456
    Top = 232
  end
end
