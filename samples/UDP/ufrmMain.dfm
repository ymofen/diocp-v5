object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 458
  ClientWidth = 826
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    Left = 0
    Top = 41
    Width = 826
    Height = 417
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #25509#25910#25968#25454
      ExplicitWidth = 281
      ExplicitHeight = 165
      object mmoOutput: TMemo
        Left = 0
        Top = 0
        Width = 818
        Height = 389
        Align = alClient
        Lines.Strings = (
          'mmoOutput')
        TabOrder = 0
        ExplicitLeft = -529
        ExplicitTop = -69
        ExplicitWidth = 810
        ExplicitHeight = 234
      end
    end
    object tsSend: TTabSheet
      Caption = #21457#36865
      ImageIndex = 1
      object btnSend: TButton
        Left = 16
        Top = 80
        Width = 75
        Height = 25
        Caption = 'btnSend'
        TabOrder = 0
        OnClick = btnSendClick
      end
      object edtRemoteHost: TEdit
        Left = 16
        Top = 32
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '127.0.0.1'
      end
      object edtRemotePort: TEdit
        Left = 168
        Top = 32
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '9983'
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 826
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 400
    ExplicitTop = 88
    ExplicitWidth = 185
    object btnStart: TButton
      Left = 152
      Top = 9
      Width = 75
      Height = 25
      Caption = 'btnStart'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object edtPort: TEdit
      Left = 8
      Top = 11
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '9983'
    end
  end
end
