(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-07-16 18:15:25
 *     TDiocpBlockTcpClient添加RecvBufEnd函数
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *   2015-03-16 13:51:06
 *     添加ConnectTimeOut方法(可以进行超时连接,andriod平台暂时未实现)
 *
 *)
 
unit diocp.tcp.blockClient;

interface

uses
  SysUtils
  , diocp.res
  {$IFDEF POSIX}
  , diocp.core.rawPosixSocket
  {$ELSE}
  , diocp.core.rawWinSocket
  , diocp.winapi.winsock2
  {$ENDIF}

  ,Classes
  , SysConst;

// 25:XE5
{$IF CompilerVersion<=25}
type
     NativeUInt = Cardinal;
     IntPtr = Cardinal;
{$ifend}

// before delphi 2007
{$if CompilerVersion < 18}
type
   ULONG_PTR = Cardinal;
{$ifend}

type

  // 22 :XE  25:XE5
  {$IF CompilerVersion >= 22}  // 大于25有Android和iOS
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 {$IF CompilerVersion >= 25} or pidOSX32 or pidAndroid or pidiOSSimulator{$IFEND})]
  {$IFEND}

  TDiocpBlockTcpClient = class(TComponent)
  private
    FActive: Boolean;
    FHost: String;
    FOnDisconnected: TNotifyEvent;
    FPort: Integer;
    FRawSocket: TRawSocket;
    FReadTimeOut: Integer;
    procedure SetActive(const Value: Boolean);
    
    procedure CheckSocketResult(pvSocketResult:Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    /// <summary>
    ///   超时连接
    /// </summary>
    /// <param name="pvMs"> 超时毫秒数 </param>
    procedure ConnectTimeOut(pvMs:Cardinal);
    procedure Disconnect;

    /// <summary>
    ///  是否可以接收数据,如果返回true， 代表可以执行RecvBuffer接收数据
    /// </summary>
    function CanRecvBuffer(pvTimeOut:Integer): Boolean;

    /// <summary>
    ///  recv buffer
    /// </summary>
    procedure recv(buf: Pointer; len: cardinal);

    function Peek(buf: Pointer; len: Cardinal): Integer;
    function RecvBuffer(buf: Pointer; len: cardinal): Integer;
    function SendBuffer(buf: Pointer; len: cardinal): Integer;
    /// <summary>
    ///   阻塞接收数据直到接收到一个endBuf为止
    ///   如果收到的数据到达len大小，会直接返回
    /// </summary>
    /// <returns>
    ///   返回接收到的数据长度
    /// </returns>
    /// <param name="buf"> 用来存放的起始内存地址 </param>
    /// <param name="len"> 内存大小 </param>
    /// <param name="endBuf"> 判断结束的起始内存地址 </param>
    /// <param name="endBufLen"> 内存大小 </param>
    function RecvBufferEnd(buf: Pointer; len: cardinal; endBuf: Pointer; endBufLen:
        Integer): Integer;
    property RawSocket: TRawSocket read FRawSocket;
  published
    property Active: Boolean read FActive write SetActive;

    property Host: String read FHost write FHost;

    property OnDisconnected: TNotifyEvent read FOnDisconnected write
        FOnDisconnected;

    property Port: Integer read FPort write FPort;


    /// <summary>
    ///   unit ms
    /// </summary>
    property ReadTimeOut: Integer read FReadTimeOut write FReadTimeOut;
  end;

implementation

constructor TDiocpBlockTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRawSocket := TRawSocket.Create;
  FReadTimeOut := 30000;
end;

destructor TDiocpBlockTcpClient.Destroy;
begin
  FRawSocket.Free;
  inherited Destroy;
end;

{$IFDEF POSIX}

{$ELSE}
// <2007版本的Windows平台使用
//   SOSError = 'System Error.  Code: %d.'+sLineBreak+'%s';
procedure RaiseLastOSErrorException(LastError: Integer);
var       // 高版本的 SOSError带3个参数
  Error: EOSError;
begin
  if LastError <> 0 then
    Error := EOSError.CreateResFmt(@SOSError, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := EOSError.CreateRes(@SUnkOSError);
  Error.ErrorCode := LastError;
  raise Error;
end;
{$ENDIF}

function TDiocpBlockTcpClient.CanRecvBuffer(pvTimeOut:Integer): Boolean;
begin
  {$IFDEF POSIX}
  Result := FRawSocket.Readable(pvTimeout);
  {$ELSE}
  Result := FRawSocket.Readable(pvTimeout);
  {$ENDIF}
end;

procedure TDiocpBlockTcpClient.CheckSocketResult(pvSocketResult: Integer);
var
  lvErrorCode:Integer;
begin
  ///  Posix, fail return 0
  ///  ms_windows, fail return -1
  {$IFDEF POSIX}
  if (pvSocketResult = -1) or (pvSocketResult = 0) then
  begin
     try
       RaiseLastOSError;
     except
       Disconnect;
       raise;
     end;
   end;
  {$ELSE}
  if (pvSocketResult = SOCKET_ERROR) then
  begin
    lvErrorCode := GetLastError;
    Disconnect;     // 出现异常后断开连接

    {$if CompilerVersion < 23}
    RaiseLastOSErrorException(lvErrorCode);
    {$ELSE}
    RaiseLastOSError(lvErrorCode);
    {$ifend} 
  end;
  {$ENDIF}
end;

procedure TDiocpBlockTcpClient.Connect;
var
  lvIpAddr:String;
begin
  if FActive then exit;

  FRawSocket.createTcpSocket;
  if FReadTimeOut > 0 then
  begin
    FRawSocket.setReadTimeOut(FReadTimeOut);
  end;

  // 进行域名解析
  lvIpAddr := FRawSocket.GetIpAddrByName(FHost);

  FActive := FRawSocket.connect(lvIpAddr, FPort);
  if not FActive then
  begin
    RaiseLastOSError;
  end;
end;

procedure TDiocpBlockTcpClient.ConnectTimeOut(pvMs:Cardinal);
var
  lvIpAddr:String;
begin
  if FActive then exit;

  FRawSocket.createTcpSocket;
  FRawSocket.setReadTimeOut(FReadTimeOut);

  // 进行域名解析
  lvIpAddr := FRawSocket.GetIpAddrByName(FHost);

  FActive := FRawSocket.ConnectTimeOut(lvIpAddr, FPort, pvMs);
  if not FActive then
  begin
    raise Exception.CreateFmt(strConnectTimeOut, [FHost, FPort]);
  end;

end;

procedure TDiocpBlockTcpClient.Disconnect;
begin
  if not FActive then Exit;

  if Assigned(FOnDisconnected) then FOnDisconnected(Self);
  

  FRawSocket.close;

  FActive := false;
end;

function TDiocpBlockTcpClient.Peek(buf: Pointer; len: Cardinal): Integer;
begin
  Result := FRawSocket.PeekBuf(buf^, len);
end;

procedure TDiocpBlockTcpClient.recv(buf: Pointer; len: cardinal);
var
  lvTempL :Integer;
  lvReadL :Cardinal;
  lvPBuf:Pointer;
begin
  lvReadL := 0;
  lvPBuf := buf;
  while lvReadL < len do
  begin
    lvTempL := FRawSocket.RecvBuf(lvPBuf^, len - lvReadL);

    CheckSocketResult(lvTempL);

    lvPBuf := Pointer(IntPtr(lvPBuf) + Cardinal(lvTempL));
    lvReadL := lvReadL + Cardinal(lvTempL);
  end;
end;

function TDiocpBlockTcpClient.RecvBuffer(buf: Pointer; len: cardinal): Integer;
begin
  Result := FRawSocket.RecvBuf(buf^, len);
  CheckSocketResult(Result);
end;

function TDiocpBlockTcpClient.RecvBufferEnd(buf: Pointer; len: cardinal;
    endBuf: Pointer; endBufLen: Integer): Integer;
begin
  Result := FRawSocket.RecvBufEnd(buf, len, endBuf, endBufLen);
  CheckSocketResult(Result);
end;

function TDiocpBlockTcpClient.sendBuffer(buf: Pointer; len: cardinal): Integer;
begin
  Result := FRawSocket.SendBuf(buf^, len);

  CheckSocketResult(Result);
end;

procedure TDiocpBlockTcpClient.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      Connect;
    end else
    begin
      Disconnect;
    end;
  end;
end;

end.
