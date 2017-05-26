object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 161
  ClientWidth = 581
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnAction: TButton
    Left = 48
    Top = 56
    Width = 75
    Height = 25
    Action = actStart
    TabOrder = 0
  end
  object edtPort: TEdit
    Left = 48
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '9983'
  end
  object actlstMain: TActionList
    Left = 320
    Top = 56
    object actStart: TAction
      Caption = 'actStart'
      OnExecute = actStartExecute
    end
    object actStop: TAction
      Caption = 'actStop'
      OnExecute = actStopExecute
    end
  end
  object tmrInfo: TTimer
    OnTimer = tmrInfoTimer
    Left = 248
    Top = 40
  end
end
