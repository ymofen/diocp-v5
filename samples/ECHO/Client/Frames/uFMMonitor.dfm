object FMMonitor: TFMMonitor
  Left = 0
  Top = 0
  Width = 483
  Height = 309
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 63
    Height = 13
    Caption = 'server state:'
  end
  object lblsvrState: TLabel
    Left = 96
    Top = 16
    Width = 51
    Height = 13
    Caption = 'lblsvrState'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 25
    Height = 13
    Caption = 'recv:'
  end
  object lblPostRecvINfo: TLabel
    Left = 96
    Top = 48
    Width = 76
    Height = 13
    Caption = 'lblPostRecvINfo'
  end
  object Label3: TLabel
    Left = 16
    Top = 115
    Width = 27
    Height = 13
    Caption = 'send:'
  end
  object lblSend: TLabel
    Left = 96
    Top = 115
    Width = 34
    Height = 13
    Caption = 'lblSend'
  end
  object Label4: TLabel
    Left = 16
    Top = 192
    Width = 48
    Height = 13
    Caption = 'acceptex:'
  end
  object lblAcceptEx: TLabel
    Left = 96
    Top = 192
    Width = 55
    Height = 13
    Caption = 'lblAcceptEx'
  end
  object lblOnlineCounter: TLabel
    Left = 96
    Top = 212
    Width = 79
    Height = 13
    Caption = 'lblOnlineCounter'
  end
  object Label5: TLabel
    Left = 16
    Top = 212
    Width = 32
    Height = 13
    Caption = 'online:'
  end
  object lblRunTimeINfo: TLabel
    Left = 96
    Top = 252
    Width = 72
    Height = 13
    Caption = 'lblRunTimeINfo'
  end
  object Label6: TLabel
    Left = 16
    Top = 231
    Width = 42
    Height = 13
    Caption = 'workers:'
  end
  object lblWorkerCount: TLabel
    Left = 96
    Top = 231
    Width = 79
    Height = 13
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'lblWorkerCount'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnClick = lblWorkerCountClick
  end
  object Label7: TLabel
    Left = 16
    Top = 252
    Width = 43
    Height = 13
    Caption = 'run time:'
  end
  object lblRecvdSize: TLabel
    Left = 96
    Top = 67
    Width = 59
    Height = 13
    Caption = 'lblRecvdSize'
  end
  object lblSentSize: TLabel
    Left = 96
    Top = 134
    Width = 51
    Height = 13
    Caption = 'lblSentSize'
  end
  object lblSendQueue: TLabel
    Left = 96
    Top = 153
    Width = 66
    Height = 13
    Caption = 'lblSendQueue'
  end
  object Label8: TLabel
    Left = 16
    Top = 153
    Width = 70
    Height = 13
    Caption = 'sending queue'
  end
  object tmrReader: TTimer
    Enabled = False
    OnTimer = tmrReaderTimer
    Left = 416
    Top = 24
  end
end
