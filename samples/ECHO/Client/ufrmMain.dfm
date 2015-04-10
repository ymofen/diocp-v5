object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'diocp3 coder client'
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
  object btnConnect: TButton
    Left = 278
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btnConnect'
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
  object btnSendObject: TButton
    Left = 360
    Top = 8
    Width = 105
    Height = 25
    Caption = 'btnSendObject'
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 56
    Width = 795
    Height = 292
    ActivePage = TabSheet1
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object mmoRecvMessage: TMemo
        Left = 0
        Top = 0
        Width = 787
        Height = 264
        Align = alClient
        Lines.Strings = (
          'iocp tcp client demo')
        TabOrder = 0
      end
    end
    object tsMonitor: TTabSheet
      Caption = 'tsMonitor'
      ImageIndex = 1
    end
    object tsOperator: TTabSheet
      Caption = 'tsOperator'
      ImageIndex = 2
      object mmoData: TMemo
        Left = 3
        Top = 3
        Width = 450
        Height = 206
        Lines.Strings = (
          '0123456789')
        TabOrder = 0
      end
    end
  end
  object btnClose: TButton
    Left = 471
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btnClose'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object btnCreate: TButton
    Left = 552
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btnCreate'
    TabOrder = 6
    OnClick = btnCreateClick
  end
  object edtCount: TEdit
    Left = 633
    Top = 8
    Width = 114
    Height = 21
    TabOrder = 7
    Text = '1000'
  end
  object chkSendData: TCheckBox
    Left = 633
    Top = 33
    Width = 152
    Height = 17
    Caption = 'SendData OnConnected'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = chkSendDataClick
  end
end
