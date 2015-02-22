object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'RawTcpClient'
  ClientHeight = 314
  ClientWidth = 622
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
    Left = 8
    Top = 160
    Width = 606
    Height = 137
    Lines.Strings = (
      'iocp tcp client demo')
    TabOrder = 0
  end
  object btnConnect: TButton
    Left = 264
    Top = 9
    Width = 75
    Height = 25
    Caption = 'btnConnect'
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object edtHost: TEdit
    Left = 8
    Top = 13
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 156
    Top = 13
    Width = 100
    Height = 21
    TabOrder = 3
    Text = '9983'
  end
  object btnSendObject: TButton
    Left = 456
    Top = 8
    Width = 105
    Height = 25
    Caption = 'btnSendObject'
    TabOrder = 4
    OnClick = btnSendObjectClick
  end
  object btnReConnect: TButton
    Left = 345
    Top = 8
    Width = 88
    Height = 25
    Caption = 'btnReConnect'
    TabOrder = 5
    OnClick = btnReConnectClick
  end
  object mmoData: TMemo
    Left = 8
    Top = 49
    Width = 606
    Height = 89
    Lines.Strings = (
      'this message will send to server')
    TabOrder = 6
  end
end
