object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 260
  ClientWidth = 613
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
  object Label1: TLabel
    Left = 16
    Top = 173
    Width = 59
    Height = 13
    Caption = #36828#31243#25991#20214'ID'
  end
  object edtHost: TEdit
    Left = 16
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 168
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '9983'
  end
  object btnConnect: TButton
    Left = 304
    Top = 56
    Width = 75
    Height = 25
    Caption = #21019#24314#36830#25509
    TabOrder = 2
    OnClick = btnConnectClick
  end
  object btnUpload: TButton
    Left = 8
    Top = 131
    Width = 75
    Height = 25
    Caption = 'btnUpload'
    TabOrder = 3
    OnClick = btnUploadClick
  end
  object btnDownload: TButton
    Left = 223
    Top = 190
    Width = 75
    Height = 25
    Caption = 'btnDownload'
    TabOrder = 4
    OnClick = btnDownloadClick
  end
  object edtRFileID: TEdit
    Left = 16
    Top = 192
    Width = 201
    Height = 21
    TabOrder = 5
    Text = 'diocpBean\ludashisetup.zip'
  end
  object btnDel: TButton
    Left = 304
    Top = 190
    Width = 75
    Height = 25
    Caption = 'btnDel'
    TabOrder = 6
    OnClick = btnDelClick
  end
  object btnFileSize: TButton
    Left = 304
    Top = 159
    Width = 75
    Height = 25
    Caption = 'btnFileSize'
    TabOrder = 7
    OnClick = btnFileSizeClick
  end
  object edtUploadFileName: TEdit
    Left = 16
    Top = 104
    Width = 273
    Height = 21
    TabOrder = 8
    Text = 'C:\a.zip'
  end
  object btnUpload2: TButton
    Left = 295
    Top = 102
    Width = 84
    Height = 25
    Caption = 'btnUpload2'
    TabOrder = 9
    OnClick = btnUpload2Click
  end
  object rgFileAccess: TRadioGroup
    Left = 16
    Top = 43
    Width = 282
    Height = 38
    Caption = #36830#25509#32452#20214
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'IdTcpClient'
      'DiocpBlockTcpClient')
    TabOrder = 10
  end
  object dlgOpen: TOpenDialog
    Left = 16
    Top = 80
  end
end
