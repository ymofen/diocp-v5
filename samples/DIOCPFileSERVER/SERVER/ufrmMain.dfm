object frmMain: TfrmMain
  Left = 383
  Top = 145
  BorderIcons = [biSystemMenu]
  Caption = 'DIOCP File SERVER'
  ClientHeight = 392
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
    Height = 351
    ActivePage = tsMoniter
    Align = alClient
    TabOrder = 0
    object tsMoniter: TTabSheet
      Caption = #30417#25511#38754#26495
      ImageIndex = 1
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 653
        Height = 323
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 661
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnOpen: TButton
      Left = 142
      Top = 4
      Width = 75
      Height = 25
      Action = actOpen
      TabOrder = 0
    end
    object edtPort: TEdit
      Left = 15
      Top = 6
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '9983'
    end
  end
  object actlstMain: TActionList
    Left = 552
    Top = 8
    object actOpen: TAction
      Caption = #24320#21551#26381#21153
      OnExecute = actOpenExecute
    end
    object actStop: TAction
      Caption = #20572#27490#26381#21153
      OnExecute = actStopExecute
    end
    object actPushMsg: TAction
      Caption = 'PushMsg'
    end
  end
end
