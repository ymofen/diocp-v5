object frmMain: TfrmMain
  Left = 391
  Top = 275
  BorderIcons = [biSystemMenu]
  Caption = 'diocp-v5 http server'
  ClientHeight = 387
  ClientWidth = 661
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
    Top = 41
    Width = 661
    Height = 346
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #30417#25511#38754#26495
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 653
        Height = 318
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = #26085#24535
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 653
        Height = 318
        Align = alClient
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
      object btn1: TButton
        Left = 176
        Top = 88
        Width = 75
        Height = 25
        Caption = 'btn1'
        TabOrder = 0
        OnClick = btn1Click
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 661
    Height = 41
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
    Interval = 30000
    OnTimer = tmrHeartTimer
    Left = 328
    Top = 200
  end
end
