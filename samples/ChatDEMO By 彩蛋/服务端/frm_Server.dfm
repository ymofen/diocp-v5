object Form5: TForm5
  Left = 0
  Top = 0
  Caption = #26381#21153#31471
  ClientHeight = 516
  ClientWidth = 867
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmain
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pgc: TPageControl
    Left = 0
    Top = 0
    Width = 867
    Height = 516
    ActivePage = tsMsg
    Align = alClient
    TabOrder = 0
    object tsState: TTabSheet
      Caption = #36816#34892#29366#24577
    end
    object tsMsg: TTabSheet
      Caption = #36890#35759#30417#25511
      ImageIndex = 1
      object lvUser: TListView
        Left = 0
        Top = 0
        Width = 250
        Height = 427
        Align = alLeft
        Columns = <
          item
            Caption = #36134#21495
          end
          item
            Caption = #26165#31216
          end
          item
            Caption = 'IP'
          end
          item
            Caption = #29366#24577
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object statCtl: TStatusBar
        Left = 0
        Top = 469
        Width = 859
        Height = 19
        Panels = <
          item
            Text = #20849'X'#20154' '#22312#32447'X'#20154
            Width = 200
          end>
      end
      object mmoMsg: TMemo
        Left = 250
        Top = 0
        Width = 609
        Height = 427
        Align = alClient
        BorderStyle = bsNone
        Lines.Strings = (
          #30417#25511#35760#24405#65306)
        TabOrder = 2
      end
      object pnl1: TPanel
        Left = 0
        Top = 427
        Width = 859
        Height = 42
        Align = alBottom
        BevelKind = bkFlat
        BevelOuter = bvNone
        TabOrder = 3
        object edtMsg: TEdit
          Left = 16
          Top = 8
          Width = 297
          Height = 21
          TabOrder = 0
          Text = #20844#21578#65306
        end
        object btnSend: TButton
          Left = 326
          Top = 6
          Width = 75
          Height = 25
          Caption = #21457#36865#20844#21578
          TabOrder = 1
          OnClick = btnSendClick
        end
        object btn1: TButton
          Left = 520
          Top = 8
          Width = 75
          Height = 25
          Caption = 'btn1'
          TabOrder = 2
        end
      end
    end
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
  end
  object tmrKeepAlive: TTimer
    Interval = 10000
    OnTimer = tmrKeepAliveTimer
    Left = 96
    Top = 24
  end
end
