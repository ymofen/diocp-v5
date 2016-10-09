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
    Top = 62
    Width = 856
    Height = 330
    ActivePage = tsWebSocket
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #30417#25511#38754#26495
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 848
        Height = 302
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object tsTester: TTabSheet
      Caption = #27979#35797#38754#26495
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
    end
    object tsURLCode: TTabSheet
      Caption = 'tsURLCode'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Top = 3
        Width = 358
        Height = 158
        Lines.Strings = (
          #25512#36865#20449#24687#21040#23458#25143#31471)
        TabOrder = 0
      end
      object btnWebSocketPush: TButton
        Left = 380
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
    Height = 62
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
      Top = 39
      Width = 97
      Height = 17
      Caption = #35775#38382'Session'
      TabOrder = 5
      OnClick = chkUseSessionClick
    end
    object chkUsePool: TCheckBox
      Left = 248
      Top = 39
      Width = 97
      Height = 17
      Caption = #20351#29992#23545#35937#27744
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object chkIPV6: TCheckBox
      Left = 9
      Top = 39
      Width = 97
      Height = 17
      Caption = #30417#21548'IPV6'
      TabOrder = 7
    end
    object chkRecord2File: TCheckBox
      Left = 384
      Top = 40
      Width = 97
      Height = 17
      Caption = #35760#24405#35831#27714#21040#25991#20214
      TabOrder = 8
    end
  end
  object mmoLog: TMemo
    Left = 0
    Top = 392
    Width = 856
    Height = 169
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
    Enabled = False
    Interval = 20000
    OnTimer = tmrHeartTimer
    Left = 528
    Top = 144
  end
  object tmrWebSocketPing: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = tmrWebSocketPingTimer
    Left = 400
    Top = 200
  end
end
