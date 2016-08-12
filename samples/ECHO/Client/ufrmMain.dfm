object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'diocpv5 echo client'
  ClientHeight = 616
  ClientWidth = 923
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object spllog: TSplitter
    Left = 0
    Top = 437
    Width = 923
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 105
    ExplicitWidth = 335
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 105
    Width = 923
    Height = 332
    ActivePage = tsMonitor
    Align = alClient
    TabOrder = 0
    object tsMonitor: TTabSheet
      Caption = #30417#25511#38754#26495
      ImageIndex = 1
    end
    object tsOperator: TTabSheet
      Caption = #25805#20316
      ImageIndex = 2
      object pnlOpera_Top: TPanel
        Left = 0
        Top = 0
        Width = 915
        Height = 62
        Align = alTop
        TabOrder = 0
        object btnSendObject: TButton
          Left = 94
          Top = 34
          Width = 142
          Height = 25
          Caption = #25152#26377#36830#25509#21457#36865#19968#27425#25968#25454
          TabOrder = 0
          OnClick = btnSendObjectClick
        end
        object btnFill1K: TButton
          Left = 4
          Top = 34
          Width = 75
          Height = 25
          Caption = #22635#20805'1K'#23383#31526
          TabOrder = 1
          OnClick = btnFill1KClick
        end
        object btnEcho: TButton
          Left = 4
          Top = 3
          Width = 75
          Height = 25
          Caption = 'btnEcho'
          TabOrder = 2
          OnClick = btnEchoClick
        end
        object btnInfo: TButton
          Left = 274
          Top = 31
          Width = 75
          Height = 25
          Caption = 'btnInfo'
          TabOrder = 3
          OnClick = btnInfoClick
        end
      end
      object pnlOpera_Send: TPanel
        Left = 0
        Top = 62
        Width = 915
        Height = 242
        Align = alClient
        Caption = 'pnlOpera_Send'
        TabOrder = 1
        object mmoData: TMemo
          Left = 1
          Top = 1
          Width = 913
          Height = 151
          Align = alClient
          Lines.Strings = (
            '0123456789')
          TabOrder = 0
        end
        object mmoOperaLog: TMemo
          Left = 1
          Top = 152
          Width = 913
          Height = 89
          Align = alBottom
          TabOrder = 1
        end
      end
    end
    object tsEvent: TTabSheet
      Caption = #20107#20214
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grpOnConnected: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 909
        Height = 152
        Align = alTop
        Caption = #25104#21151#24314#31435#36830#25509
        TabOrder = 0
        object chkSendData: TCheckBox
          Left = 88
          Top = 0
          Width = 152
          Height = 17
          Caption = #36830#25509#21518#21457#36865#25968#25454
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = chkSendDataClick
        end
        object mmoOnConnected: TMemo
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 899
          Height = 129
          Align = alClient
          Lines.Strings = (
            '0123456789')
          TabOrder = 1
        end
      end
      object grpInterval: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 161
        Width = 909
        Height = 140
        Align = alClient
        Caption = #38388#38548
        TabOrder = 1
        object pnlIntervalTop: TPanel
          Left = 2
          Top = 15
          Width = 905
          Height = 35
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object edtInterval: TEdit
            Left = 3
            Top = 7
            Width = 66
            Height = 21
            TabOrder = 0
            Text = '10'
          end
          object btnSetInterval: TButton
            Left = 84
            Top = 7
            Width = 89
            Height = 25
            Caption = #35774#23450#38388#38548#26102#38388
            TabOrder = 1
            OnClick = btnSetIntervalClick
          end
        end
        object mmoIntervalData: TMemo
          AlignWithMargins = True
          Left = 5
          Top = 53
          Width = 899
          Height = 82
          Align = alClient
          Lines.Strings = (
            'mmoIntervalData')
          TabOrder = 1
        end
        object chkIntervalSendData: TCheckBox
          Left = 88
          Top = -1
          Width = 109
          Height = 17
          Caption = #38388#38548#21457#36865#25968#25454
          TabOrder = 2
          OnClick = chkIntervalSendDataClick
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 923
    Height = 105
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnConnect: TButton
      Left = 278
      Top = 8
      Width = 99
      Height = 25
      Caption = #21019#24314#19968#20010#36830#25509
      TabOrder = 0
      OnClick = btnConnectClick
    end
    object edtHost: TEdit
      Left = 8
      Top = 10
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '127.0.0.1'
    end
    object edtPort: TEdit
      Left = 156
      Top = 10
      Width = 100
      Height = 21
      TabOrder = 2
      Text = '9983'
    end
    object btnClose: TButton
      Left = 471
      Top = 8
      Width = 75
      Height = 25
      Caption = #20851#38381#25152#26377
      TabOrder = 3
      OnClick = btnCloseClick
    end
    object btnCreate: TButton
      Left = 552
      Top = 8
      Width = 75
      Height = 25
      Caption = #21019#24314'N'#20010#36830#25509
      TabOrder = 4
      OnClick = btnCreateClick
    end
    object edtCount: TEdit
      Left = 633
      Top = 10
      Width = 114
      Height = 21
      TabOrder = 5
      Text = '1000'
    end
    object chkRecvEcho: TCheckBox
      Left = 278
      Top = 37
      Width = 187
      Height = 17
      Caption = #25509#25910#21040#25968#25454#21518#30452#25509#36820#22238#25968#25454
      TabOrder = 6
      OnClick = chkRecvEchoClick
    end
    object chkRecvOnLog: TCheckBox
      Left = 447
      Top = 37
      Width = 156
      Height = 17
      Caption = #25910#21040#21518#26174#31034#25968#25454
      TabOrder = 7
      OnClick = chkRecvOnLogClick
    end
    object btnClear: TButton
      Left = 8
      Top = 74
      Width = 75
      Height = 25
      Caption = #28165#31354#26085#24535
      TabOrder = 8
      OnClick = btnClearClick
    end
    object chkHex: TCheckBox
      Left = 447
      Top = 60
      Width = 97
      Height = 17
      Caption = '16'#36827#21046#21457#36865
      TabOrder = 9
      OnClick = chkHexClick
    end
    object chkCheckHeart: TCheckBox
      Left = 584
      Top = 60
      Width = 169
      Height = 17
      Caption = #24515#36339#26816#27979#20851#38381#27515#38142#25509
      TabOrder = 10
      OnClick = chkCheckHeartClick
    end
    object btnSaveHistory: TButton
      Left = 8
      Top = 37
      Width = 75
      Height = 25
      Caption = #20445#23384#37197#32622
      TabOrder = 11
      OnClick = btnSaveHistoryClick
    end
    object chkLogRecvTime: TCheckBox
      Left = 584
      Top = 37
      Width = 147
      Height = 17
      Caption = #35760#24405#25509#25910#25968#25454#20449#24687
      TabOrder = 12
      OnClick = chkLogRecvTimeClick
    end
    object chkSaveData: TCheckBox
      Left = 278
      Top = 60
      Width = 163
      Height = 17
      Caption = #20445#23384#25968#25454
      TabOrder = 13
      OnClick = chkSaveDataClick
    end
    object chkAutoReconnect: TCheckBox
      Left = 278
      Top = 83
      Width = 97
      Height = 17
      Caption = #33258#21160#37325#36830
      TabOrder = 14
      OnClick = chkAutoReconnectClick
    end
    object chkRandomDisconnect: TCheckBox
      Left = 447
      Top = 83
      Width = 123
      Height = 17
      Caption = #38543#26426#26029#32447
      TabOrder = 15
      OnClick = chkRandomDisconnectClick
    end
    object btnReadConfig: TButton
      Left = 89
      Top = 37
      Width = 75
      Height = 25
      Caption = #25171#24320#37197#32622
      TabOrder = 16
      OnClick = btnReadConfigClick
    end
  end
  object pnlLog: TPanel
    Left = 0
    Top = 440
    Width = 923
    Height = 176
    Align = alBottom
    Caption = 'pnlLog'
    TabOrder = 2
    object mmoRecvMessage: TMemo
      Left = 1
      Top = 1
      Width = 921
      Height = 174
      Align = alClient
      Lines.Strings = (
        'iocp tcp client demo')
      TabOrder = 0
    end
  end
  object tmrCheckHeart: TTimer
    Interval = 10000
    OnTimer = tmrCheckHeartTimer
    Left = 432
    Top = 384
  end
  object dlgOpen: TOpenDialog
    Filter = 'echo'#37197#32622'(*.diocp.config)|*.diocp.config'
    Left = 8
    Top = 232
  end
  object dlgSave: TSaveDialog
    DefaultExt = '*.diocp.config'
    Filter = 'echo'#37197#32622'(*.diocp.config)|*.diocp.config'
    Left = 40
    Top = 232
  end
end
