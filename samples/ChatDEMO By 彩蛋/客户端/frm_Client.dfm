object Form6: TForm6
  Left = 0
  Top = 0
  Caption = #23458#25143#31471
  ClientHeight = 243
  ClientWidth = 740
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 175
    Top = 210
    Width = 75
    Height = 25
    Caption = #36830#25509#24182#19978#32447
    TabOrder = 0
    OnClick = btn1Click
  end
  object lstUsers: TListBox
    Left = 0
    Top = 0
    Width = 161
    Height = 243
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
  end
  object mmoMsg: TMemo
    Left = 167
    Top = 8
    Width = 565
    Height = 153
    TabOrder = 2
  end
  object btn3: TButton
    Left = 504
    Top = 210
    Width = 89
    Height = 25
    Caption = #21457#36865#32473#25152#26377#20154
    TabOrder = 3
    OnClick = btn3Click
  end
  object btn4: TButton
    Left = 599
    Top = 210
    Width = 75
    Height = 25
    Caption = #31169#32842
    TabOrder = 4
    OnClick = btn4Click
  end
  object edtUserID: TEdit
    Left = 175
    Top = 183
    Width = 121
    Height = 21
    TabOrder = 5
    OnChange = edtUserIDChange
  end
  object edtMsg: TEdit
    Left = 368
    Top = 183
    Width = 364
    Height = 21
    TabOrder = 6
    Text = #20320#22909#65281
  end
  object tmrKeepAlive: TTimer
    Interval = 5000
    OnTimer = tmrKeepAliveTimer
    Left = 256
    Top = 128
  end
end
