object dmMain: TdmMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 291
  Width = 451
  object conMain: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=E:\workspace\d_work' +
      'space\diocp3\samples\socket-Coder\DataModuleDEMO\BIN\cnzzz.mdb;P' +
      'ersist Security Info=False;'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 24
    Top = 16
  end
  object dspMain: TDataSetProvider
    DataSet = qryMain
    Left = 144
    Top = 16
  end
  object qryMain: TADOQuery
    Connection = conMain
    Parameters = <>
    Left = 88
    Top = 16
  end
  object cdsMain: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 216
    Top = 24
  end
end
