object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 379
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 636
    Height = 177
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #21387#21147#27979#35797
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Label1: TLabel
        Left = 24
        Top = 5
        Width = 264
        Height = 13
        Caption = #32447#31243#25968'/'#36816#34892#27425#25968'('#27599#20010#32447#31243')/'#20241#24687#38388#38548'('#36127#25968#19981#20241#24687')'
      end
      object btnStart: TButton
        Left = 310
        Top = 22
        Width = 75
        Height = 25
        Caption = 'btnStart'
        TabOrder = 0
        OnClick = btnStartClick
      end
      object edtThreadNum: TEdit
        Left = 24
        Top = 24
        Width = 57
        Height = 21
        TabOrder = 1
        Text = '10'
      end
      object btnThreadInfo: TButton
        Left = 24
        Top = 122
        Width = 75
        Height = 25
        Caption = #32447#31243#20449#24687
        TabOrder = 2
        OnClick = btnThreadInfoClick
      end
      object rgAppender: TRadioGroup
        Left = 8
        Top = 64
        Width = 377
        Height = 46
        Caption = 'rgAppender'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          #31354'Appender'
          'SingleFileLogger')
        TabOrder = 3
      end
      object edtPerNum: TEdit
        Left = 87
        Top = 24
        Width = 66
        Height = 21
        TabOrder = 4
        Text = '100000'
      end
      object edtSleep: TEdit
        Left = 160
        Top = 24
        Width = 81
        Height = 21
        TabOrder = 5
        Text = '-1'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
    end
  end
  object mmoInfo: TMemo
    Left = 0
    Top = 177
    Width = 636
    Height = 202
    Align = alClient
    Lines.Strings = (
      'mmoInfo')
    TabOrder = 1
    ExplicitTop = 242
    ExplicitHeight = 139
  end
end
