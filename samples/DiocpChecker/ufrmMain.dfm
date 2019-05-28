object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 518
  ClientWidth = 878
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 878
    Height = 185
    Align = alTop
    TabOrder = 0
    object btnRefresh: TButton
      Left = 24
      Top = 48
      Width = 75
      Height = 25
      Caption = 'btnRefresh'
      TabOrder = 0
      OnClick = btnRefreshClick
    end
    object btnStart: TButton
      Left = 24
      Top = 17
      Width = 75
      Height = 25
      Caption = '1.btnStart'
      TabOrder = 1
      OnClick = btnStartClick
    end
    object btnCreateClientSocket: TButton
      Left = 26
      Top = 112
      Width = 169
      Height = 25
      Caption = '2.btnCreateClientSocket'
      TabOrder = 2
      OnClick = btnCreateClientSocketClick
    end
    object edtHost: TEdit
      Left = 26
      Top = 143
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '127.0.0.1'
    end
    object edtPort: TEdit
      Left = 178
      Top = 143
      Width = 121
      Height = 21
      TabOrder = 4
      Text = '9983'
    end
    object btnPostRecvRequest: TButton
      Left = 312
      Top = 112
      Width = 137
      Height = 25
      Caption = '3.btnPostRecvRequest'
      TabOrder = 5
      OnClick = btnPostRecvRequestClick
    end
    object btnCloseSocket: TButton
      Left = 512
      Top = 112
      Width = 129
      Height = 25
      Caption = '4.btnCloseSocket'
      TabOrder = 6
      OnClick = btnCloseSocketClick
    end
    object chkShutDown: TCheckBox
      Left = 664
      Top = 116
      Width = 97
      Height = 17
      Caption = 'ShutDown'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object btnCancelIoEx: TButton
      Left = 312
      Top = 143
      Width = 137
      Height = 25
      Caption = 'btnCancelIoEx'
      TabOrder = 8
      OnClick = btnCancelIoExClick
    end
    object btnShutDown: TButton
      Left = 512
      Top = 17
      Width = 129
      Height = 25
      Caption = 'btnShutDown'
      TabOrder = 9
      OnClick = btnShutDownClick
    end
  end
  object mmoInfo: TMemo
    Left = 0
    Top = 185
    Width = 878
    Height = 333
    Align = alClient
    Lines.Strings = (
      'mmoInfo')
    TabOrder = 1
  end
end
