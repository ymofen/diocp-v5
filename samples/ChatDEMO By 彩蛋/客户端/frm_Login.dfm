object frmLogin: TfrmLogin
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #30331#24405
  ClientHeight = 199
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 117
    Top = 43
    Width = 24
    Height = 13
    Caption = #36134#21495
  end
  object lbl2: TLabel
    Left = 117
    Top = 88
    Width = 24
    Height = 13
    Caption = #23494#30721
  end
  object edtUserID: TEdit
    Left = 152
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '1'
  end
  object edtPaw: TEdit
    Left = 152
    Top = 85
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '123'
  end
  object btnOk: TButton
    Left = 117
    Top = 144
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnClose: TButton
    Left = 198
    Top = 144
    Width = 75
    Height = 25
    Caption = #20851#38381
    TabOrder = 3
    OnClick = btnCloseClick
  end
end
