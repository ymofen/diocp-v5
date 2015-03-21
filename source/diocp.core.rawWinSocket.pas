(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
 
unit diocp.core.rawWinSocket;

interface

uses
  windows, SysUtils, diocp.winapi.winsock2;

const
  SIO_KEEPALIVE_VALS = IOC_IN or IOC_VENDOR or 4;

{ Other NT-specific options. }

  {$EXTERNALSYM SO_MAXDG}
  SO_MAXDG        = $7009;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_MAXPATHDG    = $700A;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  {$EXTERNALSYM SO_CONNECT_TIME}
  SO_CONNECT_TIME = $700C;

type
  TKeepAlive = record
    OnOff: Integer;
    KeepAliveTime: Integer;
    KeepAliveInterval: Integer;
  end;
  TTCP_KEEPALIVE = TKeepAlive;
  PTCP_KEEPALIVE = ^TKeepAlive;
  
  /// <summary>
  ///   raw socket object
  ///     thanks my friend(ryan)
  /// </summary>
  TRawSocket = class(TObject)
  private
    FSocketHandle: TSocket;
  public
    procedure close;
    procedure createTcpSocket;

    /// <summary>
    ///   create socket handle for overlapped
    /// </summary>
    procedure CreateTcpOverlappedSocket;


    procedure createUdpOverlappedSocket;

    function bind(const pvAddr: string; pvPort: Integer): Boolean;
    function listen(const backlog: Integer = 0): Boolean;

    function GetIpAddrByName(const host:string): String;

    function RecvBuf(var data; const len: Integer): Integer;
    function SendBuf(const data; const len: Integer): Integer;

    function connect(const pvAddr: string; pvPort: Integer): Boolean;

    /// <summary>
    ///   超时连接, 返回失败，表示连接超时
    /// </summary>
    function ConnectTimeOut(const pvAddr: string; pvPort: Integer; pvMs:Cardinal):
        Boolean;

    //zero if the time limit expired, or SOCKET_ERROR if an error occurred.
    function selectSocket(vReadReady, vWriteReady, vExceptFlag: PBoolean;
        pvTimeOut: Integer = 0): Integer;

    function SetReadTimeOut(const pvTimeOut: Cardinal): Integer;

    function CancelIO: Boolean;

    /// <summary>
    ///  The shutdown function disables sends or receives on a socket.
    /// </summary>
    function ShutDown(pvHow: Integer = SD_BOTH): Integer;

    /// <summary>
    ///   default 5000 check alive
    /// </summary>
    function setKeepAliveOption(pvKeepAliveTime: Integer = 5000): Boolean;


    /// <summary>
    ///   call in listen RawSocket instance
    /// </summary>
    function UpdateAcceptContext(pvSocket: TSocket): Boolean;

    function setNoDelayOption(pvOption:Boolean): Boolean;

    property SocketHandle: TSocket read FSocketHandle;
  end;

implementation

{ TRawSocket }

var
  __WSAStartupDone:Boolean;

function TRawSocket.bind(const pvAddr: string; pvPort: Integer): Boolean;
var
  sockaddr: TSockAddrIn;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    if pvAddr = '' then
    begin
      sin_addr.S_addr := inet_addr(PAnsichar(AnsiString('0.0.0.0')));
    end else
    begin
      sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    end;
    sin_port :=  htons(pvPort);
  end;
  Result := diocp.winapi.winsock2.bind(FSocketHandle, TSockAddr(sockaddr), SizeOf(sockaddr)) = 0;
end;

function TRawSocket.CancelIO: Boolean;
begin
  Result := Windows.CancelIo(FSocketHandle);
end;

procedure TRawSocket.close;
var
  lvTempSocket: TSocket;
begin
  lvTempSocket := FSocketHandle;
  if lvTempSocket <> INVALID_SOCKET then
  begin
    FSocketHandle := INVALID_SOCKET;
    
    diocp.winapi.winsock2.shutdown(lvTempSocket, SD_BOTH);
    closesocket(lvTempSocket);
  end;
end;

function TRawSocket.connect(const pvAddr: string; pvPort: Integer): Boolean;
var
  sockaddr: TSockAddrIn;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    sin_port :=  htons(pvPort);
  end;
  Result := diocp.winapi.winsock2.connect(FSocketHandle, TSockAddr(sockaddr), sizeof(TSockAddrIn))  = 0;
end;

function TRawSocket.ConnectTimeOut(const pvAddr: string; pvPort: Integer;
    pvMs:Cardinal): Boolean;
var
  lvFlags: Cardinal;
  sockaddr: TSockAddrIn;
  lvErr, lvRet: Integer;
  fs: TFDset;


  tv: timeval;
  Timeptr: PTimeval;

begin
  lvFlags := 1;  // 非阻塞模式
  ioctlsocket(FSocketHandle, FIONBIO, lvFlags);

  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    sin_port :=  htons(pvPort);
  end;
  lvRet := diocp.winapi.winsock2.connect(FSocketHandle, TSockAddr(sockaddr), sizeof(TSockAddrIn));
  if lvRet = 0 then
  begin  // 连接成功
    lvFlags := 0;  // 非阻塞模式
    ioctlsocket(FSocketHandle, FIONBIO, lvFlags);
    Result := true;
  end else
  begin
    FD_ZERO(fs);
    _FD_SET(FSocketHandle, fs);

    tv.tv_sec := pvMs div 1000;
    tv.tv_usec :=  1000 * (pvMs mod 1000);
    Timeptr := @tv;

    lvRet := diocp.winapi.winsock2.select(FSocketHandle, nil, @fs, nil, Timeptr);

    if lvRet <= 0 then
    begin
      Result := false;  //连接超时
//      lvErr := WSAGetLastError;
      closesocket(FSocketHandle);
//      Result := lvErr = 0;
    end else
    begin
      lvFlags := 0;  // 非阻塞模式
      ioctlsocket(FSocketHandle, FIONBIO, lvFlags);
      Result := true;
    end;



  end;

end;

procedure TRawSocket.CreateTcpOverlappedSocket;
begin
  FSocketHandle := WSASocket(AF_INET,SOCK_STREAM, IPPROTO_TCP, Nil, 0, WSA_FLAG_OVERLAPPED);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

procedure TRawSocket.createTcpSocket;
begin
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

procedure TRawSocket.createUdpOverlappedSocket;
begin
  FSocketHandle := WSASocket(AF_INET,SOCK_DGRAM, IPPROTO_UDP, Nil, 0, WSA_FLAG_OVERLAPPED);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

function TRawSocket.GetIpAddrByName(const host:string): String;
var
  lvhostInfo:PHostEnt;
//  lvErr:Integer;
begin
  lvhostInfo := gethostbyname(PAnsiChar(AnsiString(host)));

  if lvhostInfo = nil then
    RaiseLastOSError;

//  lvErr := WSAGetLastError;
//  if lvErr <> 0 then
//  begin
//    RaiseLastOSError(lvErr);
//  end;

  Result := inet_ntoa(PInAddr(lvhostInfo^.h_addr_list^)^);
end;

function TRawSocket.listen(const backlog: Integer): Boolean;
var
  queueSize: Integer;
begin
  if backlog = 0 then
  begin
    queueSize := SOMAXCONN;
  end
  else begin
    queueSize := backlog;
  end;
  Result := diocp.winapi.winsock2.listen(FSocketHandle, queueSize) = 0;
end;

function TRawSocket.RecvBuf(var data; const len: Integer): Integer;
begin
  Result := diocp.winapi.winsock2.recv(FSocketHandle, data, len, 0);
end;

function TRawSocket.selectSocket(vReadReady, vWriteReady, vExceptFlag:
    PBoolean; pvTimeOut: Integer = 0): Integer;
var
  ReadFds: TFDset;
  ReadFdsptr: PFDset;
  WriteFds: TFDset;
  WriteFdsptr: PFDset;
  ExceptFds: TFDset;
  ExceptFdsptr: PFDset;
  tv: timeval;
  Timeptr: PTimeval;
begin
  if Assigned(vReadReady) then
  begin
    ReadFdsptr := @ReadFds;
    FD_ZERO(ReadFds);
    _FD_SET(FSocketHandle, ReadFds);
  end
  else
    ReadFdsptr := nil;
  if Assigned(vWriteReady) then
  begin
    WriteFdsptr := @WriteFds;
    FD_ZERO(WriteFds);
    _FD_SET(FSocketHandle, WriteFds);
  end
  else
    WriteFdsptr := nil;
  if Assigned(vExceptFlag) then
  begin
    ExceptFdsptr := @ExceptFds;
    FD_ZERO(ExceptFds);
    _FD_SET(FSocketHandle, ExceptFds);
  end
  else
    ExceptFdsptr := nil;
  if pvTimeOut >= 0 then
  begin
    tv.tv_sec := pvTimeOut div 1000;
    tv.tv_usec :=  1000 * (pvTimeOut mod 1000);
    Timeptr := @tv;
  end
  else
    Timeptr := nil;

  //The select function determines the status of one or more sockets, waiting if necessary,
  //to perform synchronous I/O.
  //  The select function returns the total number of socket handles that are ready
  //  and contained in the fd_set structures,
  //  zero if the time limit expired, or SOCKET_ERROR if an error occurred.
  //  If the return value is SOCKET_ERROR,
  //  WSAGetLastError can be used to retrieve a specific error code.

  Result := diocp.winapi.winsock2.select(FSocketHandle + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr);

  if Assigned(vReadReady) then
    vReadReady^ := FD_ISSET(FSocketHandle, ReadFds);
  if Assigned(vWriteReady) then
    vWriteReady^ := FD_ISSET(FSocketHandle, WriteFds);
  if Assigned(vExceptFlag) then
    vExceptFlag^ := FD_ISSET(FSocketHandle, ExceptFds);
end;

function TRawSocket.SendBuf(const data; const len: Integer): Integer;
begin
  Result := diocp.winapi.winsock2.Send(FSocketHandle, data, len, 0);
end;

function TRawSocket.setKeepAliveOption(pvKeepAliveTime: Integer = 5000):
    Boolean;
var
  Opt, insize, outsize: integer;
  outByte: DWORD;
  inKeepAlive, outKeepAlive: TTCP_KEEPALIVE;
begin
  Result := false;
  Opt := 1;
  if SetSockopt(FSocketHandle, SOL_SOCKET, SO_KEEPALIVE,
     @Opt, sizeof(Opt)) = SOCKET_ERROR then exit;

  inKeepAlive.OnOff := 1;
  
  inKeepAlive.KeepAliveTime := pvKeepAliveTime;

  inKeepAlive.KeepAliveInterval := 1;
  insize := sizeof(TTCP_KEEPALIVE);
  outsize := sizeof(TTCP_KEEPALIVE);

  if WSAIoctl(FSocketHandle,
     SIO_KEEPALIVE_VALS,
     @inKeepAlive, insize,
     @outKeepAlive,
    outsize, outByte, nil, nil) <> SOCKET_ERROR then
  begin
    Result := true;
  end;
end;

function TRawSocket.setNoDelayOption(pvOption:Boolean): Boolean;
var
  bNoDelay: BOOL;
begin
  bNoDelay := pvOption;
  Result := setsockopt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, @bNoDelay, SizeOf(bNoDelay)) <> SOCKET_ERROR;
end;

function TRawSocket.SetReadTimeOut(const pvTimeOut: Cardinal): Integer;
begin
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@pvTimeOut), SizeOf(Cardinal));
end;

function TRawSocket.ShutDown(pvHow: Integer = SD_BOTH): Integer;
begin
  Result := diocp.winapi.winsock2.shutdown(FSocketHandle, pvHow);
end;

function TRawSocket.UpdateAcceptContext(pvSocket: TSocket): Boolean;
begin
  result := setsockopt(pvSocket, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT,
    PAnsiChar(@FSocketHandle),
   SizeOf(TSocket)) <> SOCKET_ERROR
end;

function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean): Boolean;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

procedure __CheckWSAStartup;
var
  AData: WSAData;
begin
  if lock_cmp_exchange(False, True, __WSAStartupDone) = False then
  begin
    if WSAStartup(MakeWord(1, 1), AData) <> 0 then
    begin
      __WSAStartupDone := false;
      RaiseLastOSError();
    end;
  end;
end;

initialization
   __CheckWSAStartup();


end.
