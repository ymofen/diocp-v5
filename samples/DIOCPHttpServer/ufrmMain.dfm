object frmMain: TfrmMain
  Left = 391
  Top = 275
  BorderIcons = [biSystemMenu]
  Caption = 'diocp-v5 http server'
  ClientHeight = 453
  ClientWidth = 722
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
    Width = 722
    Height = 412
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #30417#25511#38754#26495
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 714
        Height = 384
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object tsLog: TTabSheet
      Caption = #26085#24535
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 714
        Height = 384
        Align = alClient
        TabOrder = 0
      end
    end
    object tsTester: TTabSheet
      Caption = #27979#35797#38754#26495
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 653
      ExplicitHeight = 318
      object btnCompress: TButton
        Left = 3
        Top = 3
        Width = 75
        Height = 33
        Caption = 'btnCompress'
        TabOrder = 0
        OnClick = btnCompressClick
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
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 722
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
    Left = 528
    Top = 144
  end
end
