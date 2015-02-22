object frmMain: TfrmMain
  Left = 383
  Top = 145
  BorderIcons = [biSystemMenu]
  Caption = 'diocp3 db server'
  ClientHeight = 378
  ClientWidth = 613
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
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
  object pgcMain: TPageControl
    Left = 8
    Top = 39
    Width = 601
    Height = 331
    ActivePage = tsMoniter
    TabOrder = 2
    object tsMoniter: TTabSheet
      Caption = 'Moniter'
      ImageIndex = 1
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 593
        Height = 303
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
  end
  object Button1: TButton
    Left = 320
    Top = 4
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
  object actlstMain: TActionList
    Left = 448
    Top = 248
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
    end
  end
end
