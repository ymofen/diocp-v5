object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'diocp-v5 coder client'
  ClientHeight = 523
  ClientWidth = 771
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmoRecvMessage: TMemo
    Left = 0
    Top = 49
    Width = 616
    Height = 302
    Align = alClient
    TabOrder = 0
    ExplicitTop = 59
    ExplicitWidth = 702
    ExplicitHeight = 151
  end
  object pnlOperator: TPanel
    Left = 0
    Top = 0
    Width = 771
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnConnect: TButton
      Left = 241
      Top = 9
      Width = 75
      Height = 25
      Caption = #36830#25509
      TabOrder = 0
      OnClick = btnConnectClick
    end
    object edtHost: TEdit
      Left = 8
      Top = 11
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '127.0.0.1'
    end
    object edtPort: TEdit
      Left = 135
      Top = 11
      Width = 100
      Height = 21
      TabOrder = 2
      Text = '9983'
    end
    object btnLogin: TButton
      Left = 552
      Top = 9
      Width = 75
      Height = 25
      Caption = #30331#38470
      TabOrder = 3
      OnClick = btnLoginClick
    end
    object cbbName: TComboBox
      Left = 401
      Top = 11
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 4
      Text = #27611#27901#19996
      Items.Strings = (
        #27611#27901#19996
        #24429#24503#24576
        #23435#32654#40836
        #33931#20171#30707)
    end
  end
  object pnlSendArea: TPanel
    Left = 0
    Top = 351
    Width = 771
    Height = 172
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 216
    ExplicitWidth = 702
    object btnSendObject: TButton
      Left = 620
      Top = 22
      Width = 105
      Height = 25
      Caption = 'btnSendObject'
      TabOrder = 0
      OnClick = btnSendObjectClick
    end
    object mmoData: TMemo
      Left = 1
      Top = 3
      Width = 613
      Height = 166
      TabOrder = 1
    end
  end
  object lstUsers: TListBox
    Left = 616
    Top = 49
    Width = 155
    Height = 302
    Align = alRight
    ItemHeight = 13
    TabOrder = 3
    ExplicitTop = 65
    ExplicitHeight = 286
  end
  object tmrHeart: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrHeartTimer
    Left = 136
    Top = 120
  end
end
