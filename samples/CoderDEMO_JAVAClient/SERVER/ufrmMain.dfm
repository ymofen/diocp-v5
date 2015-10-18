object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'diocp-v5 strObject Coder'
  ClientHeight = 453
  ClientWidth = 776
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
    Width = 776
    Height = 394
    ActivePage = tsMonitor
    Align = alClient
    TabOrder = 0
    object tsMonitor: TTabSheet
      Caption = #30417#25511#38754#26495
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 768
        Height = 366
        Align = alClient
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object tsLog: TTabSheet
      Caption = #26085#24535
      ImageIndex = 1
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 768
        Height = 366
        Align = alClient
        TabOrder = 0
      end
    end
    object tsTest: TTabSheet
      Caption = #27979#35797#39029#38754
      ImageIndex = 2
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 776
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
      Left = 142
      Top = 4
      Width = 75
      Height = 25
      Action = actOpen
      TabOrder = 1
    end
    object edtMsg: TEdit
      Left = 256
      Top = 9
      Width = 232
      Height = 21
      TabOrder = 2
      Text = 'this message will push to all client'
    end
    object btnPushMsg: TButton
      Left = 494
      Top = 8
      Width = 75
      Height = 25
      Action = actPushMsg
      TabOrder = 3
    end
    object btnDisconnectAll: TButton
      Left = 142
      Top = 28
      Width = 75
      Height = 25
      Action = actDisconnectAll
      TabOrder = 4
    end
  end
  object actlstMain: TActionList
    Left = 232
    Top = 272
    object actOpen: TAction
      Caption = #24320#21551#26381#21153
      OnExecute = actOpenExecute
    end
    object actStop: TAction
      Caption = #20572#27490#26381#21153
      OnExecute = actStopExecute
    end
    object actPushMsg: TAction
      Caption = #25512#36865#20449#24687
      OnExecute = actPushMsgExecute
    end
    object actDisconnectAll: TAction
      Caption = #26029#24320#25152#26377
      OnExecute = actDisconnectAllExecute
    end
  end
end
