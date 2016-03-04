object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'diocpv5 echo client'
  ClientHeight = 356
  ClientWidth = 811
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 79
    Width = 811
    Height = 277
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #26085#24535
      object mmoRecvMessage: TMemo
        Left = 0
        Top = 0
        Width = 803
        Height = 249
        Align = alClient
        Lines.Strings = (
          'iocp tcp client demo')
        TabOrder = 0
      end
    end
    object tsMonitor: TTabSheet
      Caption = #30417#25511#38754#26495
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 787
      ExplicitHeight = 264
    end
    object tsOperator: TTabSheet
      Caption = #25805#20316
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 787
      ExplicitHeight = 264
      object mmoData: TMemo
        Left = 3
        Top = 3
        Width = 450
        Height = 206
        Lines.Strings = (
          '0123456789')
        TabOrder = 0
      end
      object btnFill1K: TButton
        Left = 459
        Top = 3
        Width = 75
        Height = 25
        Caption = #22635#20805'1K'#23383#31526
        TabOrder = 1
        OnClick = btnFill1KClick
      end
      object btnSendObject: TButton
        Left = 459
        Top = 48
        Width = 142
        Height = 25
        Caption = #25152#26377#36830#25509#21457#36865#19968#27425#25968#25454
        TabOrder = 2
        OnClick = btnSendObjectClick
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 811
    Height = 79
    Align = alTop
    TabOrder = 1
    object btnConnect: TButton
      Left = 278
      Top = 8
      Width = 99
      Height = 25
      Caption = #21019#24314#19968#20010#36830#25509
      TabOrder = 0
      OnClick = btnConnectClick
    end
    object edtHost: TEdit
      Left = 8
      Top = 13
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '127.0.0.1'
    end
    object edtPort: TEdit
      Left = 156
      Top = 13
      Width = 100
      Height = 21
      TabOrder = 2
      Text = '9983'
    end
    object btnClose: TButton
      Left = 471
      Top = 8
      Width = 75
      Height = 25
      Caption = #20851#38381#25152#26377
      TabOrder = 3
      OnClick = btnCloseClick
    end
    object btnCreate: TButton
      Left = 552
      Top = 8
      Width = 75
      Height = 25
      Caption = #21019#24314'N'#20010#36830#25509
      TabOrder = 4
      OnClick = btnCreateClick
    end
    object edtCount: TEdit
      Left = 633
      Top = 10
      Width = 114
      Height = 21
      TabOrder = 5
      Text = '1000'
    end
    object chkSendData: TCheckBox
      Left = 278
      Top = 56
      Width = 152
      Height = 17
      Caption = #36830#25509#21518#21457#36865'(MEMO'#25968#25454')'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = chkSendDataClick
    end
    object chkRecvEcho: TCheckBox
      Left = 278
      Top = 37
      Width = 187
      Height = 17
      Caption = #25509#25910#21040#25968#25454#21518#30452#25509#36820#22238#25968#25454
      TabOrder = 7
      OnClick = chkRecvEchoClick
    end
    object chkRecvOnLog: TCheckBox
      Left = 471
      Top = 39
      Width = 156
      Height = 17
      Caption = #25910#21040#25968#25454#35760#24405#26085#24535
      TabOrder = 8
      OnClick = chkRecvOnLogClick
    end
  end
end
