unit uIRemoteServer;

interface


type
  IRemoteServer = interface(IInterface)
    ['{20B5F070-461C-41F4-AA0C-E500A36E18E4}']

    /// <summary>
    ///   执行远程动作
    /// </summary>
    function Execute(pvCmdIndex: Integer; var vData: OleVariant): Boolean; stdcall;
  end;

implementation

end.
