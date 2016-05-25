object Form5: TForm5
  Left = 0
  Top = 0
  Caption = #25968#25454#24179#21488#26381#21153#31471
  ClientHeight = 457
  ClientWidth = 867
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmain
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object redtLog: TRichEdit
    Left = 0
    Top = 0
    Width = 867
    Height = 457
    Align = alClient
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitTop = 41
    ExplicitHeight = 416
  end
  object mmain: TMainMenu
    Left = 24
    Top = 24
    object mniN1: TMenuItem
      Caption = #25511#21046
      object mniStart: TMenuItem
        Caption = #24320#21551
        OnClick = mniStartClick
      end
      object mniStop: TMenuItem
        Caption = #20572#27490
        OnClick = mniStopClick
      end
      object mniN3: TMenuItem
        Caption = #26029#24320#25152#26377
        OnClick = mniN3Click
      end
    end
    object mniN4: TMenuItem
      Caption = #37197#32622
      object mniN5: TMenuItem
        Caption = #31471#21475
      end
    end
  end
  object tmrKeepAlive: TTimer
    Interval = 10000
    OnTimer = tmrKeepAliveTimer
    Left = 96
    Top = 24
  end
end
