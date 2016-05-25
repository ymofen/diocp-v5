unit uICoderSocket;

interface

type
  ICoderSocket = interface
    ['{6C90309D-C0AA-40B9-9DAE-8C801A1DF99B}']

    /// <summary>
    ///   尝试发送指定长度的数据
    /// </summary>
    /// <returns>
    ///   返回成功发送数据的长度
    /// </returns>
    /// <param name="buf"> (Pointer) </param>
    /// <param name="len"> (Cardinal) </param>
    function SendBuf(buf:Pointer; len:Cardinal): Cardinal; stdcall;


    /// <summary>
    ///   尝试接收指定长度的数据
    /// </summary>
    /// <returns> Cardinal
    /// </returns>
    /// <param name="buf"> (Pointer) </param>
    /// <param name="len"> (Cardinal) </param>
    function RecvBuf(buf:Pointer; len:Cardinal): Cardinal; stdcall;

    /// <summary>
    ///   关闭连接
    /// </summary>
    procedure CloseSocket; stdcall;
  end;

implementation

end.
