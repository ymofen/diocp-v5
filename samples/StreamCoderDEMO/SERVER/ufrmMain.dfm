object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'diocp3 server for diocp1'
  ClientHeight = 385
  ClientWidth = 640
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
    Width = 640
    Height = 326
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 632
        Height = 298
        Align = alClient
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'log'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 632
        Height = 298
        Align = alClient
        TabOrder = 0
      end
    end
    object tsTest: TTabSheet
      Caption = 'tsTest'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object btnBufferTester: TButton
        Left = 24
        Top = 24
        Width = 121
        Height = 25
        Caption = 'btnBufferTester'
        TabOrder = 0
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 59
    Align = alTop
    Caption = 'pnlTop'
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
      Caption = 'start'
      OnExecute = actOpenExecute
    end
    object actStop: TAction
      Caption = 'stop'
      OnExecute = actStopExecute
    end
    object actPushMsg: TAction
      Caption = 'PushMsg'
      OnExecute = actPushMsgExecute
    end
    object actDisconnectAll: TAction
      Caption = 'Disconnect all'
      OnExecute = actDisconnectAllExecute
    end
  end
end
