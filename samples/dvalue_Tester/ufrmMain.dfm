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
    Height = 288
    ActivePage = tsLoadFile
    Align = alClient
    TabOrder = 0
    object tsJSON: TTabSheet
      Caption = 'tsJSON'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 921
        Height = 65
        Align = alTop
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
          Left = 352
          Top = 21
          Width = 145
          Height = 25
          Caption = 'btnObjectTester'
          TabOrder = 3
          OnClick = btnObjectTesterClick
        end
        object btnInputJSONBuffer: TButton
          Left = 544
          Top = 21
          Width = 137
          Height = 25
          Caption = 'btnInputJSONBuffer'
          TabOrder = 4
          OnClick = btnInputJSONBufferClick
        end
        object btnSetJSON: TButton
          Left = 728
          Top = 21
          Width = 75
          Height = 25
          Caption = 'btnSetJSON'
          TabOrder = 5
          OnClick = btnSetJSONClick
        end
      end
      object mmoData: TMemo
        Left = 0
        Top = 65
        Width = 921
        Height = 195
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      object btnRemovePath: TButton
        Left = 208
        Top = 168
        Width = 113
        Height = 25
        Caption = 'btnRemovePath'
        TabOrder = 5
        OnClick = btnRemovePathClick
      end
      object btnClearTimeOut: TButton
        Left = 376
        Top = 32
        Width = 121
        Height = 25
        Caption = 'btnClearTimeOut'
        TabOrder = 6
        OnClick = btnClearTimeOutClick
      end
    end
    object tsDataSet: TTabSheet
      Caption = 'tsDataSet'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object btnConvertToDValue: TButton
        Left = 379
        Top = 3
        Width = 153
        Height = 25
        Caption = #25968#25454#38598#36716#25442#21040'DValue->JSON'
        TabOrder = 0
        OnClick = btnConvertToDValueClick
      end
      object btnInitCDSDemo: TButton
        Left = 16
        Top = 8
        Width = 129
        Height = 25
        Caption = 'DEMO'#25968#25454#38598#21021#22987#21270
        TabOrder = 1
        OnClick = btnInitCDSDemoClick
      end
      object dbgrdDemo: TDBGrid
        Left = 16
        Top = 39
        Width = 345
        Height = 210
        DataSource = dsMain
        TabOrder = 2
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object mmoJSONData: TMemo
        Left = 379
        Top = 39
        Width = 502
        Height = 210
        Lines.Strings = (
          'mmoJSONData')
        TabOrder = 3
      end
      object btnDValueToDataSet: TButton
        Left = 592
        Top = 3
        Width = 145
        Height = 25
        Caption = 'JSON-DValue->'#25968#25454#38598
        TabOrder = 4
        OnClick = btnDValueToDataSetClick
      end
      object btnEmptyDemo: TButton
        Left = 184
        Top = 8
        Width = 113
        Height = 25
        Caption = #28165#31354'DEMO'#25968#25454#38598
        TabOrder = 5
        OnClick = btnEmptyDemoClick
      end
    end
    object tsLoadFile: TTabSheet
      Caption = 'tsLoadFile'
      ImageIndex = 5
      object btnLoadTextFrom: TButton
        Left = 40
        Top = 32
        Width = 129
        Height = 25
        Caption = 'btnLoadTextFrom'
        TabOrder = 0
        OnClick = btnLoadTextFromClick
      end
    end
  end
  object mmoLog: TMemo
    Left = 0
    Top = 288
    Width = 929
    Height = 172
    Align = alBottom
    Lines.Strings = (
      'mmoLog')
    TabOrder = 1
  end
  object dlgOpenFile: TOpenDialog
    Left = 296
    Top = 352
  end
  object cdsDemo: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 568
    Top = 240
  end
  object dsMain: TDataSource
    DataSet = cdsDemo
    Left = 608
    Top = 240
  end
end
