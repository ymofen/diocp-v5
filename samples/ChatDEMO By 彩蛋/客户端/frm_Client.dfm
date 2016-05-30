object frmClient: TfrmClient
  Left = 0
  Top = 0
  Caption = #23458#25143#31471
  ClientHeight = 398
  ClientWidth = 740
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lvUser: TListView
    Left = 0
    Top = 0
    Width = 161
    Height = 398
    Align = alLeft
    Columns = <
      item
        Caption = #36134#21495
      end
      item
        Caption = #26165#31216
      end
      item
        Caption = #29366#24577
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object pnl1: TPanel
    Left = 161
    Top = 0
    Width = 579
    Height = 398
    Align = alClient
    Caption = 'pnl1'
    TabOrder = 1
    object pnl2: TPanel
      Left = 1
      Top = 356
      Width = 577
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'pnl2'
      TabOrder = 0
      object btnSendMsg: TButton
        Left = 480
        Top = 8
        Width = 89
        Height = 25
        Caption = #21457#36865
        TabOrder = 0
        OnClick = btnSendMsgClick
      end
      object edtMsg: TEdit
        Left = 14
        Top = 11
        Width = 460
        Height = 21
        TabOrder = 1
        Text = #20320#22909#65281
      end
    end
    object mmoMsg: TMemo
      Left = 1
      Top = 1
      Width = 577
      Height = 355
      Align = alClient
      BorderStyle = bsNone
      Lines.Strings = (
        #32842#22825#35760#24405#65306)
      TabOrder = 1
    end
  end
  object tmrKeepAlive: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrKeepAliveTimer
    Left = 256
    Top = 128
  end
end
