object dm: Tdm
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object conn: TADOConnection
    KeepConnection = False
    LoginPrompt = False
    Left = 64
    Top = 56
  end
  object qryTemp: TADOQuery
    Connection = conn
    Parameters = <>
    Left = 136
    Top = 56
  end
end
