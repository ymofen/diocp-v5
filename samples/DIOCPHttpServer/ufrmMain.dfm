object frmMain: TfrmMain
  Left = 391
  Top = 275
  BorderIcons = [biSystemMenu]
  Caption = 'diocp-v5 http server'
  ClientHeight = 561
  ClientWidth = 856
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    Left = 0
    Top = 80
    Width = 856
    Height = 368
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #30417#25511#38754#26495
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 848
        Height = 305
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlHTTPInfo: TPanel
        Left = 0
        Top = 305
        Width = 848
        Height = 35
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object lblHttpInfo: TLabel
          Left = 8
          Top = 16
          Width = 51
          Height = 13
          Caption = 'lblHttpInfo'
        end
      end
    end
    object tsTester: TTabSheet
      Caption = #27979#35797#38754#26495
      ImageIndex = 2
      object btnCompress: TButton
        Left = 3
        Top = 3
        Width = 75
        Height = 33
        Caption = 'btnCompress'
        TabOrder = 0
        OnClick = btnCompressClick
      end
      object btnInfo: TButton
        Left = 3
        Top = 64
        Width = 75
        Height = 25
        Caption = #23545#35937#20449#24687
        TabOrder = 1
        OnClick = btnInfoClick
      end
      object btnParseRange: TButton
        Left = 3
        Top = 128
        Width = 99
        Height = 25
        Caption = 'btnParseRange'
        TabOrder = 2
        OnClick = btnParseRangeClick
      end
    end
    object tsURLCode: TTabSheet
      Caption = 'tsURLCode'
      ImageIndex = 3
      object mmoURLInput: TMemo
        Left = 3
        Top = 3
        Width = 358
        Height = 94
        Lines.Strings = (
          'mmoURLInput')
        TabOrder = 0
      end
      object mmoURLOutput: TMemo
        Left = 5
        Top = 160
        Width = 356
        Height = 113
        TabOrder = 1
      end
      object btnURLDecode: TButton
        Left = 3
        Top = 103
        Width = 102
        Height = 25
        Caption = 'btnURLDecode'
        TabOrder = 2
        OnClick = btnURLDecodeClick
      end
      object btnURLEncode: TButton
        Left = 138
        Top = 103
        Width = 94
        Height = 25
        Caption = 'URLEncode'
        TabOrder = 3
        OnClick = btnURLEncodeClick
      end
    end
    object tsWebSocket: TTabSheet
      Caption = 'WebSocket'
      ImageIndex = 3
      object mmoWebSocketData: TMemo
        Left = 3
        Top = 34
        Width = 842
        Height = 239
        Lines.Strings = (
          #25512#36865#20449#24687#21040#23458#25143#31471)
        TabOrder = 0
      end
      object btnWebSocketPush: TButton
        Left = 3
        Top = 3
        Width = 137
        Height = 25
        Caption = 'WebSocket'#25512#36865
        TabOrder = 1
        OnClick = btnWebSocketPushClick
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 856
    Height = 80
    Align = alTop
    TabOrder = 1
    object edtPort: TEdit
      Left = 9
      Top = 7
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '8081'
    end
    object btnOpen: TButton
      Left = 142
      Top = 4
      Width = 75
      Height = 25
      Action = actOpen
      TabOrder = 1
    end
    object btnDisconectAll: TButton
      Left = 232
      Top = 4
      Width = 113
      Height = 25
      Caption = 'btnDisconectAll'
      TabOrder = 2
      OnClick = btnDisconectAllClick
    end
    object btnGetWorkerState: TButton
      Left = 360
      Top = 4
      Width = 121
      Height = 25
      Caption = 'btnGetWorkerState'
      TabOrder = 3
      OnClick = btnGetWorkerStateClick
    end
    object btnFindContext: TButton
      Left = 492
      Top = 4
      Width = 94
      Height = 25
      Caption = 'btnFindContext'
      TabOrder = 4
      OnClick = btnFindContextClick
    end
    object chkUseSession: TCheckBox
      Left = 142
      Top = 52
      Width = 97
      Height = 17
      Caption = #35775#38382'Session'
      TabOrder = 5
      OnClick = chkUseSessionClick
    end
    object chkUsePool: TCheckBox
      Left = 248
      Top = 52
      Width = 97
      Height = 17
      Caption = #20351#29992#23545#35937#27744
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object chkIPV6: TCheckBox
      Left = 9
      Top = 52
      Width = 97
      Height = 17
      Caption = #30417#21548'IPV6'
      TabOrder = 7
    end
    object chkRecord2File: TCheckBox
      Left = 384
      Top = 53
      Width = 97
      Height = 17
      Caption = #35760#24405#35831#27714#21040#25991#20214
      TabOrder = 8
    end
    object edtWorkCount: TEdit
      Left = 10
      Top = 31
      Width = 121
      Height = 21
      TabOrder = 9
      Text = '0'
    end
    object chkWebSocketEcho: TCheckBox
      Left = 552
      Top = 56
      Width = 145
      Height = 17
      Caption = 'webSocket Echo'
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
    object chkAccessControl: TCheckBox
      Left = 680
      Top = 56
      Width = 97
      Height = 17
      Caption = #36328#22495#35775#38382
      TabOrder = 11
    end
  end
  object mmoLog: TMemo
    Left = 0
    Top = 448
    Width = 856
    Height = 113
    Align = alBottom
    TabOrder = 2
  end
  object actlstMain: TActionList
    Left = 560
    Top = 176
    object actOpen: TAction
      Caption = 'start'
      OnExecute = actOpenExecute
    end
    object actStop: TAction
      Caption = 'stop'
      OnExecute = actStopExecute
    end
  end
  object tmrHeart: TTimer
    Interval = 30000
    OnTimer = tmrHeartTimer
    Left = 528
    Top = 144
  end
  object tmrWebSocketPing: TTimer
    Interval = 30000
    OnTimer = tmrWebSocketPingTimer
    Left = 424
    Top = 200
  end
  object tmrInfoRefresh: TTimer
    OnTimer = tmrInfoRefreshTimer
    Left = 424
    Top = 288
  end
end
