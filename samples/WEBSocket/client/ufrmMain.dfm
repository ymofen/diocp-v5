object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 422
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 522
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnConnect: TButton
      Left = 392
      Top = 6
      Width = 75
      Height = 25
      Caption = 'btnConnect'
      TabOrder = 0
      OnClick = btnConnectClick
    end
    object edtWsUrl: TEdit
      Left = 4
      Top = 8
      Width = 360
      Height = 21
      TabOrder = 1
      Text = 'ws://127.0.0.1:8081/'
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 41
    Width = 522
    Height = 381
    Align = alClient
    Caption = 'pnlClient'
    TabOrder = 1
    object mmoRecv: TMemo
      Left = 1
      Top = 1
      Width = 520
      Height = 223
      Align = alClient
      Lines.Strings = (
        'mmoRecv')
      TabOrder = 0
    end
    object pnlSend: TPanel
      Left = 1
      Top = 224
      Width = 520
      Height = 156
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'pnlSend'
      TabOrder = 1
      object mmoSend: TMemo
        Left = 0
        Top = 0
        Width = 520
        Height = 126
        Align = alClient
        Lines.Strings = (
          'mmoSend')
        TabOrder = 0
      end
      object pnlSendRight: TPanel
        Left = 0
        Top = 126
        Width = 520
        Height = 30
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btnSend: TButton
          Left = 436
          Top = 3
          Width = 75
          Height = 25
          Caption = 'btnSend'
          TabOrder = 0
          OnClick = btnSendClick
        end
      end
    end
  end
end
