object frmMain: TfrmMain
  Left = 391
  Top = 275
  BorderIcons = [biSystemMenu]
  Caption = 'diocp5 echo server'
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
    Top = 91
    Width = 812
    Height = 364
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #30417#25511#38754#26495
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 804
        Height = 336
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
        Height = 336
        Align = alClient
        Lines.Strings = (
          'mmoLog')
        TabOrder = 0
      end
    end
    object tsOperator: TTabSheet
      Caption = #27979#35797#25805#20316
      ImageIndex = 2
      object mmoPushData: TMemo
        Left = 3
        Top = 16
        Width = 430
        Height = 113
        Lines.Strings = (
          'diocpv5'#25512#36865#27979#35797'....')
        TabOrder = 0
      end
      object btnPushToAll: TButton
        Left = 448
        Top = 14
        Width = 129
        Height = 25
        Action = actPushToAll
        TabOrder = 1
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 812
    Height = 91
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
    object btnPostWSAClose: TButton
      Left = 562
      Top = 5
      Width = 103
      Height = 25
      Caption = #24322#27493#26029#24320
      TabOrder = 5
      OnClick = btnPostWSACloseClick
    end
    object btnReOpenTest: TButton
      Left = 671
      Top = 5
      Width = 106
      Height = 25
      Caption = #24320#20851#27979#35797
      TabOrder = 6
      OnClick = btnReOpenTestClick
    end
    object chkLogDetails: TCheckBox
      Left = 142
      Top = 36
      Width = 97
      Height = 17
      Caption = #35760#24405#35814#32454#26085#24535
      TabOrder = 7
      OnClick = chkLogDetailsClick
    end
    object btnPoolInfo: TButton
      Left = 671
      Top = 33
      Width = 106
      Height = 25
      Caption = #20869#23384#27744#20449#24687
      TabOrder = 8
      OnClick = btnPoolInfoClick
    end
    object edtThread: TEdit
      Left = 9
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 9
      Text = '0'
    end
    object chkEcho: TCheckBox
      Left = 256
      Top = 36
      Width = 97
      Height = 17
      Caption = #22238#23556#25968#25454
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = chkEchoClick
    end
    object chkShowInMemo: TCheckBox
      Left = 359
      Top = 36
      Width = 97
      Height = 17
      Caption = #26174#31034#25968#25454
      TabOrder = 11
      OnClick = chkShowInMemoClick
    end
    object chkSaveToFile: TCheckBox
      Left = 462
      Top = 36
      Width = 97
      Height = 17
      Caption = #20445#23384#21040#25991#20214
      TabOrder = 12
      OnClick = chkSaveToFileClick
    end
    object chkUseContextPool: TCheckBox
      Left = 142
      Top = 59
      Width = 103
      Height = 17
      Caption = #20351#29992'ContextPool'
      Checked = True
      State = cbChecked
      TabOrder = 13
    end
    object chkUseBufferPool: TCheckBox
      Left = 256
      Top = 59
      Width = 97
      Height = 17
      Caption = #20351#29992#20869#23384#27744
      TabOrder = 14
      OnClick = chkUseBufferPoolClick
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
    object actPushToAll: TAction
      Caption = #25512#36865#32473#25152#26377#30340#23458#25143#31471
      OnExecute = actPushToAllExecute
    end
  end
  object tmrKickOut: TTimer
    Interval = 10000
    OnTimer = tmrKickOutTimer
    Left = 72
    Top = 144
  end
  object tmrTest: TTimer
    Enabled = False
    OnTimer = tmrTestTimer
    Left = 672
    Top = 128
  end
  object tmrInfo: TTimer
    OnTimer = tmrInfoTimer
    Left = 144
    Top = 144
  end
end
