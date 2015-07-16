object frmMain: TfrmMain
  Left = 391
  Top = 275
  BorderIcons = [biSystemMenu]
  Caption = 'diocp-v5 ex string server'
  ClientHeight = 455
  ClientWidth = 812
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
    Top = 59
    Width = 812
    Height = 396
    ActivePage = tsLog
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
        Width = 804
        Height = 368
        Align = alClient
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
        Width = 804
        Height = 368
        Align = alClient
        Lines.Strings = (
          'mmoLog')
        TabOrder = 0
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 812
    Height = 59
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnDisconectAll: TButton
      Left = 216
      Top = 5
      Width = 113
      Height = 25
      Caption = #26029#24320#25152#26377
      TabOrder = 0
      OnClick = btnDisconectAllClick
    end
    object btnFindContext: TButton
      Left = 462
      Top = 5
      Width = 94
      Height = 25
      Caption = #27979#35797#26597#25214'Context'
      TabOrder = 1
      OnClick = btnFindContextClick
    end
    object btnGetWorkerState: TButton
      Left = 335
      Top = 5
      Width = 121
      Height = 25
      Caption = #33719#21462#32447#31243#24037#20316#29366#24577
      TabOrder = 2
      OnClick = btnGetWorkerStateClick
    end
    object btnOpen: TButton
      Left = 142
      Top = 5
      Width = 75
      Height = 25
      Action = actOpen
      TabOrder = 3
    end
    object edtPort: TEdit
      Left = 9
      Top = 7
      Width = 121
      Height = 21
      TabOrder = 4
      Text = '9983'
    end
    object chkLogDetails: TCheckBox
      Left = 142
      Top = 36
      Width = 97
      Height = 17
      Caption = #35760#24405#35814#32454#26085#24535
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = chkLogDetailsClick
    end
  end
  object actlstMain: TActionList
    Left = 248
    Top = 104
    object actOpen: TAction
      Caption = #24320#21551
      OnExecute = actOpenExecute
    end
    object actStop: TAction
      Caption = #20851#38381
      OnExecute = actStopExecute
    end
  end
  object tmrKickOut: TTimer
    Interval = 10000
    OnTimer = tmrKickOutTimer
    Left = 72
    Top = 144
  end
end
