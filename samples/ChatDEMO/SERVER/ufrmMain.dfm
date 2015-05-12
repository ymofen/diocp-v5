object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'diocp-v5 CHAT SERVER'
  ClientHeight = 474
  ClientWidth = 766
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
  object PageControl1: TPageControl
    Left = 0
    Top = 59
    Width = 766
    Height = 415
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #30417#25511#38754#26495
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 758
        Height = 387
        Align = alClient
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = #26085#24535
      ImageIndex = 1
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 758
        Height = 387
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 766
    Height = 59
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object edtPort: TEdit
      Left = 9
      Top = 7
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '9983'
    end
    object btnOpen: TButton
      Left = 136
      Top = 4
      Width = 75
      Height = 25
      Action = actOpen
      TabOrder = 1
    end
    object btnDisconnectAll: TButton
      Left = 238
      Top = 4
      Width = 75
      Height = 25
      Action = actDisconnectAll
      Caption = #26029#24320#25152#26377
      TabOrder = 2
    end
  end
  object actlstMain: TActionList
    Left = 232
    Top = 272
    object actOpen: TAction
      Caption = #24320#21551#26381#21153' '
      OnExecute = actOpenExecute
    end
    object actStop: TAction
      Caption = #20572#27490
      OnExecute = actStopExecute
    end
    object actPushMsg: TAction
      Caption = #24191#25773
    end
    object actDisconnectAll: TAction
      Caption = 'Disconnect all'
      OnExecute = actDisconnectAllExecute
    end
  end
  object tmrHeart: TTimer
    Interval = 10000
    OnTimer = tmrHeartTimer
    Left = 376
    Top = 240
  end
end
