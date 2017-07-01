(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
(*$HPPEMIT '#ifdef s6_addr'*)
(*$HPPEMIT '  #undef s6_addr'*)
(*$HPPEMIT '#endif'*)
(*$HPPEMIT '#ifdef s6_addr16'*)
(*$HPPEMIT '  #undef s6_addr16'*)
(*$HPPEMIT '#endif'*)

unit diocp_core_rawWinSocket;

interface

uses
  windows, SysUtils, diocp_winapi_winsock2, diocp_sockets_utils;



/// IPV6 DEFINE
type
  {$EXTERNALSYM IN6_ADDR}
  IN6_ADDR = record
    case Integer of
      0: (s6_bytes: array[0..15] of u_char);
      1: (s6_words: array[0..7] of u_short);
  end;
  {$EXTERNALSYM ADDRESS_FAMILY}
  ADDRESS_FAMILY = u_short;
  {$EXTERNALSYM SCOPE_ID}
  SCOPE_ID = record
//    union {
//        struct {
//            ULONG Zone : 28;
//            ULONG Level : 4;
//        };
//        ULONG Value;
//    };
    Value : ULONG;
  end;
  {$EXTERNALSYM PSCOPE_ID}
  PSCOPE_ID = ^SCOPE_ID;

  {$EXTERNALSYM sockaddr_in6_union}
  sockaddr_in6_union = record
    case Integer of
      0 : (sin6_scope_id : ULONG);     // Set of interfaces for a scope.
      1 : (sin6_scope_struct : SCOPE_ID);
  end;
  {$EXTERNALSYM SOCKADDR_IN6_LH}
  SOCKADDR_IN6_LH = record
    sin6_family : ADDRESS_FAMILY; // AF_INET6.
    sin6_port : USHORT;           // Transport level port number.
    sin6_flowinfo : ULONG;       // IPv6 flow information.
    sin6_addr : IN6_ADDR;         // IPv6 address.
    a : sockaddr_in6_union;
  end;

  {$EXTERNALSYM PSOCKADDR_IN6_LH}
  PSOCKADDR_IN6_LH = ^SOCKADDR_IN6_LH;
  {$EXTERNALSYM SOCKADDR_IN6}
  SOCKADDR_IN6 = SOCKADDR_IN6_LH;
  {$NODEFINE TSockAddrIn6}
  TSockAddrIn6   = SOCKADDR_IN6;
  {$NODEFINE PSockAddrIn6}
  PSockAddrIn6   = ^TSockAddrIn6;

  TAddrSunB = packed record
    s_b1, s_b2, s_b3, s_b4: UInt8;
  end;

  TAddrSunW = packed record
    s_w1, s_w2: UInt16;
  end;

  PSockIn4Addr = ^TSockIn4Addr;
  TSockIn4Addr = packed record
    case integer of
        0: (S_un_b: TAddrSunB);
        1: (S_un_w: TAddrSunW);
        2: (S_addr: Cardinal);
  end;

  PSockIn6Addr = ^TSockIn6Addr;
  TSockIn6Addr = packed record
    case Integer of
    0: (s6_addr: packed array [0..16-1] of UInt8);
    1: (s6_addr16: packed array [0..8-1] of UInt16);
  end;


type
  TSocketState = (ssDisconnecting, ssDisconnected, ssConnected, ssConnecting, ssListening, ssAccepting);

const
  TSocketStateCaption: array[TSocketState] of String = ('正在断开', '已经断开', '已经连接', '正在连接', '正在侦听', '正在接入');

const
  {$IFNDEF DOTNET}
    {$IFDEF USE_VCL_POSIX}
  DIOCP_PF_INET4 = AF_INET;
  DIOCP_PF_INET6 = AF_INET6;
    {$ELSE}
  DIOCP_PF_INET4 = PF_INET;
  DIOCP_PF_INET6 = PF_INET6;
     {$ENDIF}
  {$ELSE}
  DIOCP_PF_INET4 = ProtocolFamily.InterNetwork;
  DIOCP_PF_INET6 = ProtocolFamily.InterNetworkV6;
  {$ENDIF}

  IPV6_SOCKADDR_SIZE = SizeOf(TSockAddrIn6);

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

  IP_V4 = 0;
  IP_V6 = 1;

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
    FIPVersion: Integer;
    FSocketHandle: TSocket;
    procedure CheckDestroyHandle;
  public
    function SocketValid: Boolean;
    procedure Close(pvShutdown: Boolean = true);
    procedure CreateTcpSocket;

    procedure DoInitialize;

    destructor Destroy; override;

    /// <summary>
    ///   create socket handle for overlapped
    /// </summary>
    procedure CreateTcpOverlappedSocket;


    procedure CreateUdpOverlappedSocket;

    function Bind(const pvAddr: string; pvPort: Integer): Boolean;
    function Listen(const backlog: Integer = 0): Boolean;

    function GetIpAddrByName(const host:string): String;

    function GetLocalPort: Word;

    function GetLocalAddress: String;

    /// <summary>
    ///   -2:  超时
    /// </summary>
    function RecvBuf(var data; const len: Cardinal; pvTimeOut: Cardinal): Integer;
        overload;
    function RecvBuf(var data; const len: Cardinal): Integer; overload;
    function SendBuf(const data; const len: Integer): Integer;
    function PeekBuf(var data; const len: Integer): Integer;

    /// <summary>
    ///   接收字符
    /// </summary>
    /// <returns>
    ///   0: 代表连接被优雅的关闭
    ///   > 0：收到的数据长度
    ///  -1: 接收错误
    ///  -2: 接收超时
    /// </returns>
    /// <param name="buf"> (PAnsiChar) </param>
    /// <param name="len"> (Integer) </param>
    /// <param name="endBuf"> (PAnsiChar) </param>
    /// <param name="endBufLen"> (Integer) </param>
    function RecvBufEnd(buf: PAnsiChar; len: Integer; endBuf: PAnsiChar; endBufLen:
        Integer; pvTimeOut: Cardinal): Integer; overload;
    function RecvBufEnd(buf: PAnsiChar; len: Integer; endBuf: PAnsiChar; endBufLen:
        Integer): Integer; overload;

    function Connect(const pvAddr: string; pvPort: Integer): Boolean;

    /// <summary>
    ///   超时连接, 返回失败，表示连接超时
    /// </summary>
    function ConnectTimeOut(const pvAddr: string; pvPort: Integer; pvMs:Cardinal):
        Boolean;

    //zero if the time limit expired, or SOCKET_ERROR if an error occurred.
    function SelectSocket(vReadReady, vWriteReady, vExceptFlag: PBoolean;
        pvTimeOut: Integer = 0): Integer;

    function SetSendBufferLength(const len:Integer): Integer;
    function SetRecvBufferLength(const len:Integer): Integer;
    function Readable(pvTimeOut:Integer): Boolean;

    /// <summary>
    ///  -1:出现了异常
    /// </summary>
    function ReceiveLength: Integer;

    function SetReadTimeOut(const pvTimeOut: Cardinal): Integer;

    function SetSendTimeOut(const pvTimeOut:Cardinal): Integer;

    function CancelIO: Boolean;

    function CancelIOEx: Boolean;

    /// <summary>
    ///  The shutdown function disables sends or receives on a socket.
    /// </summary>
    function ShutDown(pvHow: Integer = SD_BOTH): Integer;

    /// <summary>
    ///   default 5000 check alive
    /// </summary>
    function SetKeepAliveOption(pvKeepAliveTime: Integer = 5000): Boolean;


    /// <summary>
    ///   call in Listen RawSocket instance
    /// </summary>
    function UpdateAcceptContext(pvSocket: TSocket): Boolean;

    /// <summary>
    ///   是否使用延迟
    ///   默认是开启延迟的
    ///   nagle算法
    /// </summary>
    /// <param name="pvOption">true 为禁用</param>
    function SetNoDelayOption(pvOption:Boolean): Boolean;

    property IPVersion: Integer read FIPVersion write FIPVersion;

    property SocketHandle: TSocket read FSocketHandle;


  end;

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;

function TranslateTInAddrToString(const sockaddr; const AIPVersion:
    Integer): string;

var
  __DebugWSACreateCounter:Integer;

  __DebugWSACloseCounter:Integer;

implementation

{ TRawSocket }

var
  __WSAStartupDone:Boolean;

//function CancelIoEx(hFile: THandle; lpOverlapped:LPOVERLAPPED): BOOL; stdcall; external kernel32 name 'CancelIoEx';

function TranslateTInAddrToString(const sockaddr; const AIPVersion:
    Integer): string;
var
  i: Integer;
begin
  case AIPVersion of
    IP_V6:
      begin
        Result := '';
        for i := 0 to 7 do begin
          Result := Result + IntToHex(ntohs(PSockIn6Addr(@sockaddr).s6_addr16[i]), 1) + ':';
        end;
        SetLength(Result, Length(Result)-1);
      end;
    else
      begin
        Result := IntToStr(PSockIn4Addr(@sockaddr).S_un_b.s_b1) + '.'   {Do not Localize}
                  + IntToStr(PSockIn4Addr(@sockaddr).S_un_b.s_b2) + '.' {Do not Localize}
                  + IntToStr(PSockIn4Addr(@sockaddr).S_un_b.s_b3) + '.' {Do not Localize}
                  + IntToStr(PSockIn4Addr(@sockaddr).S_un_b.s_b4);
      end;

  end;
end;

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
  begin
    result := High(Cardinal) - tick_start + tick_end;
  end;

end;

function TRawSocket.Bind(const pvAddr: string; pvPort: Integer): Boolean;
var
  sockaddr: array[0..127] of byte;
  lvSize:Integer;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);

  if FIPVersion = IP_V4 then
  begin
    with PSockAddrIn(@sockaddr[0])^ do
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
    lvSize := SizeOf(TSockAddrIn);
  end else if FIPVersion = IP_V6 then
  begin
    PSockAddrIn6(@sockaddr[0])^.sin6_family := DIOCP_PF_INET6;
    PSockAddrIn6(@sockaddr[0])^.sin6_port := htons(pvPort);
    lvSize := SizeOf(TSockAddrIn6);
  end else
  begin
    lvSize := 0;
    Assert(false,  'unkonw ip protecol');
  end;


  Result := diocp_winapi_winsock2.Bind(FSocketHandle, PSockAddr(@sockaddr[0]), lvSize) = 0;
end;

function TRawSocket.CancelIO: Boolean;
begin
  Result := Windows.CancelIo(FSocketHandle);
end;

procedure TRawSocket.Close(pvShutdown: Boolean = true);
var
  lvTempSocket: TSocket;
begin
  lvTempSocket := FSocketHandle;
  if lvTempSocket <> INVALID_SOCKET then
  begin
    FSocketHandle := INVALID_SOCKET;
    
    if pvShutdown then
    begin
      // To assure that all data is sent and received on a connected socket before it is closed,
      // an application should use shutdown to close connection before calling closesocket.
      diocp_winapi_winsock2.shutdown(lvTempSocket, SD_BOTH);
    end;

    Closesocket(lvTempSocket);

    InterlockedIncrement(__DebugWSACloseCounter);
  end else
  begin
    //Assert(false, 'xxx');
  end;
end;

function TRawSocket.Connect(const pvAddr: string; pvPort: Integer): Boolean;
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
  Result := diocp_winapi_winsock2.Connect(FSocketHandle, TSockAddr(sockaddr), sizeof(TSockAddrIn))  = 0;
end;

function TRawSocket.ConnectTimeOut(const pvAddr: string; pvPort: Integer;
    pvMs:Cardinal): Boolean;
var
  lvFlags: Cardinal;
  sockaddr: TSockAddrIn;
  lvRet: Integer;
  fs: TFDset;


  tv: timeval;
  Timeptr: PTimeval;

begin
  lvFlags := 1;  // 非阻塞模式
  ioctlsocket(FSocketHandle, LongInt(FIONBIO), lvFlags);

  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    sin_port :=  htons(pvPort);
  end;
  lvRet := diocp_winapi_winsock2.Connect(FSocketHandle, TSockAddr(sockaddr), sizeof(TSockAddrIn));
  if lvRet = 0 then
  begin  // 连接成功
    lvFlags := 0;  // 非阻塞模式
    ioctlsocket(FSocketHandle, Longint(FIONBIO), lvFlags);
    Result := true;
  end else
  begin
    FD_ZERO(fs);
    _FD_SET(FSocketHandle, fs);

    tv.tv_sec := pvMs div 1000;
    tv.tv_usec :=  1000 * (pvMs mod 1000);
    Timeptr := @tv;

    lvRet := diocp_winapi_winsock2.select(FSocketHandle, nil, @fs, nil, Timeptr);

    if lvRet <= 0 then
    begin
      Result := false;  //连接超时
//      lvErr := WSAGetLastError;
      closesocket(FSocketHandle);
//      Result := lvErr = 0;
    end else
    begin
      lvFlags := 0;  // 非阻塞模式
      ioctlsocket(FSocketHandle, Longint(FIONBIO), lvFlags);
      Result := true;
    end;
  end;

end;

procedure TRawSocket.CreateTcpOverlappedSocket;
begin
  CheckDestroyHandle;
  if FIPVersion = IP_V6 then
  begin
    {$IFDEF UNICODE}
    FSocketHandle := WSASocketW(AF_INET6, SOCK_STREAM, IPPROTO_IP, Nil, 0, WSA_FLAG_OVERLAPPED);
    {$ELSE}
    FSocketHandle := WSASocketA(AF_INET6, SOCK_STREAM, IPPROTO_IP, Nil, 0, WSA_FLAG_OVERLAPPED);
    {$ENDIF}
  end else
  begin
//    {$IFDEF UNICODE}
//    FSocketHandle := WSASocketW(AF_INET, SOCK_STREAM, IPPROTO_IP, Nil, 0, WSA_FLAG_OVERLAPPED);
//    {$ELSE}
//    FSocketHandle := WSASocketA(AF_INET, SOCK_STREAM, IPPROTO_IP, Nil, 0, WSA_FLAG_OVERLAPPED);
//    {$ENDIF}
    FSocketHandle := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_IP, Nil, 0, WSA_FLAG_OVERLAPPED)
  end;

 // FSocketHandle := WSASocket(AF_INET6,SOCK_STREAM, IPPROTO_IPV6, Nil, 0, WSA_FLAG_OVERLAPPED);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
  InterlockedIncrement(__DebugWSACreateCounter);
end;

procedure TRawSocket.CreateTcpSocket;
begin
  CheckDestroyHandle;
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
  InterlockedIncrement(__DebugWSACreateCounter);
end;

procedure TRawSocket.CreateUdpOverlappedSocket;
begin
  CheckDestroyHandle;
  FSocketHandle := WSASocket(AF_INET,SOCK_DGRAM, IPPROTO_UDP, Nil, 0, WSA_FLAG_OVERLAPPED);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
  InterlockedIncrement(__DebugWSACreateCounter);
end;

destructor TRawSocket.Destroy;
begin
  CheckDestroyHandle;
  inherited;
end;

function TRawSocket.CancelIOEx: Boolean;
begin
  if Assigned(DiocpCancelIoEx) then
  begin
    Result := DiocpCancelIoEx(self.FSocketHandle, nil);
  end else
  begin
    Result := False;
  end;
end;

procedure TRawSocket.DoInitialize;
begin
  SetNoDelayOption(True);
end;

procedure TRawSocket.CheckDestroyHandle;
var
  lvTempSocket: TSocket;
begin
  lvTempSocket := FSocketHandle;
  if (lvTempSocket <> 0) and (lvTempSocket <> INVALID_SOCKET) then
  begin
    FSocketHandle := INVALID_SOCKET;

    Closesocket(lvTempSocket);

    InterlockedIncrement(__DebugWSACloseCounter);
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

  Result :=string(inet_ntoa(PInAddr(lvhostInfo^.h_addr_list^)^));
end;

function TRawSocket.GetLocalAddress: String;
var
  lvSockAddr: TSockAddr;
  Size: Integer;
begin
  Size := SizeOf(TSockAddr);
  getsockname(SocketHandle, lvSockAddr, Size);
  Result := string(inet_ntoa(lvSockAddr.sin_addr));
end;

function TRawSocket.GetLocalPort: Word;
var
  lvSockAddr: TSockAddr;
  Size: Integer;
begin
  Size := SizeOf(TSockAddr);
  getsockname(SocketHandle, lvSockAddr, Size);
  Result := ntohs(lvSockAddr.sin_port);
end;

function TRawSocket.Listen(const backlog: Integer = 0): Boolean;
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
  Result := diocp_winapi_winsock2.Listen(FSocketHandle, queueSize) = 0;
end;

function TRawSocket.PeekBuf(var data; const len: Integer): Integer;
begin
  Result := diocp_winapi_winsock2.recv(FSocketHandle, data, len, MSG_PEEK);
end;

function TRawSocket.Readable(pvTimeOut:Integer): Boolean;
var
  lvFDSet:TFDSet;
  lvTime_val: TTimeval;
  r:Integer;
begin
  Result := False;
  FD_ZERO(lvFDSet);
  _FD_SET(FSocketHandle, lvFDSet);

  lvTime_val.tv_sec := pvTimeOut div 1000;
  lvTime_val.tv_usec :=  1000 * (pvTimeOut mod 1000);


  //The select function determines the status of one or more sockets, waiting if necessary,
  //to perform synchronous I/O.
  //  The select function returns the total number of socket handles that are ready
  //  and contained in the fd_set structures,
  //  zero if the time limit expired, or SOCKET_ERROR if an error occurred.
  //  If the return value is SOCKET_ERROR,
  //  WSAGetLastError can be used to retrieve a specific error code.

  r := select(0, @lvFDSet, nil, nil, @lvTime_val);
  if r = 0 then
  begin
    Result := False;
  end else if r = -1 then
  begin
    RaiseLastOSError;
  end else if lvFDSet.fd_count > 0 then
  begin
    Result := true;
  end else if r > 0 then
  begin
    Result := True;
  end else
  begin
    Result := False;
  end;
end;

function TRawSocket.ReceiveLength: Integer;
var
  r :Integer;
begin
  r := ioctlsocket(FSocketHandle, FIONREAD, Cardinal(Result));
  if r = -1 then
  begin
    Result := -1;
  end;
end;            


function TRawSocket.RecvBuf(var data; const len: Cardinal; pvTimeOut:
    Cardinal): Integer;
var
  lvTick : Cardinal;
  r:Integer;
begin
  lvTick := GetTickCount;
  while True do
  begin
    if (tick_diff(lvTick, GetTickCount) > pvTimeOut) then
    begin
      Result := -2;
      Exit;
    end else if Readable(1000) then
    begin
      Result := recv(FSocketHandle, data, len, 0);
      Exit;
    end;

    r := ReceiveLength;
    if r = -1 then
    begin
      RaiseLastOSError;
    end else if r > 0 then
    begin
      Result := recv(FSocketHandle, data, len, 0);
      Exit;
    end;

    Sleep(10);
  end;
end;

function TRawSocket.RecvBuf(var data; const len: Cardinal): Integer;
begin
  Result := recv(FSocketHandle, data, len, 0);
end;

function TRawSocket.RecvBufEnd(buf: PAnsiChar; len: Integer; endBuf: PAnsiChar;
    endBufLen: Integer; pvTimeOut: Cardinal): Integer;
var
  lvRet, j:Integer;
  lvTempEndBuf:PAnsiChar;
  lvMatchCounter:Integer;
  lvTick:Cardinal;
begin
  lvTempEndBuf := endBuf;
  lvMatchCounter := 0;
  j:=0;
  lvTick := GetTickCount;
  while (j < len) do
  begin
    if (tick_diff(lvTick, GetTickCount) > pvTimeOut) then
    begin
      Result := -2;
      Exit;
    end else  if ReceiveLength > 0 then
    begin
      lvRet := recv(FSocketHandle, buf^, 1, 0);
      if lvRet = -1 then
      begin
        Result := lvRet;
        exit;
      end;
      if lvRet = 0 then
      begin  // 被关闭
        Result := 0;
        exit;
      end;
      inc(j);
      if buf^ = lvTempEndBuf^ then
      begin
        Inc(lvMatchCounter);
        Inc(lvTempEndBuf);
        if lvMatchCounter = endBufLen then
        begin    // 读取成功
          Break;
        end;
      end else
      begin
        lvTempEndBuf := endBuf;
        lvMatchCounter := 0;
      end;
      inc(buf);
    end else
    begin
      Sleep(10);
    end;
  end;
  Result := j;
end;

function TRawSocket.RecvBufEnd(buf: PAnsiChar; len: Integer; endBuf: PAnsiChar;
    endBufLen: Integer): Integer;
var
  lvRet, j:Integer;
  lvTempEndBuf:PAnsiChar;
  lvMatchCounter:Integer;
begin
  lvTempEndBuf := endBuf;
  lvMatchCounter := 0;
  j:=0;
  while j < len do
  begin
    lvRet := recv(FSocketHandle, buf^, 1, 0);   // 阻塞读取一个字节
    if lvRet = -1 then
    begin
      Result := lvRet;
      exit;
    end;
    if lvRet = 0 then
    begin  // 被关闭
      Result := 0;
      exit;
    end;
    inc(j);
    if buf^ = lvTempEndBuf^ then
    begin
      Inc(lvMatchCounter);
      Inc(lvTempEndBuf);
      if lvMatchCounter = endBufLen then
      begin    // 读取成功
        Break;
      end;
    end else
    begin
      lvTempEndBuf := endBuf;
      lvMatchCounter := 0;
    end;
    inc(buf);
  end;
  Result := j;
end;

function TRawSocket.SelectSocket(vReadReady, vWriteReady, vExceptFlag:
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

  Result := diocp_winapi_winsock2.select(FSocketHandle + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr);

  if Assigned(vReadReady) then
    vReadReady^ := FD_ISSET(FSocketHandle, ReadFds);
  if Assigned(vWriteReady) then
    vWriteReady^ := FD_ISSET(FSocketHandle, WriteFds);
  if Assigned(vExceptFlag) then
    vExceptFlag^ := FD_ISSET(FSocketHandle, ExceptFds);
end;

function TRawSocket.SendBuf(const data; const len: Integer): Integer;
begin
  Result := diocp_winapi_winsock2.Send(FSocketHandle, data, len, 0);
end;

function TRawSocket.SetKeepAliveOption(pvKeepAliveTime: Integer = 5000):
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

function TRawSocket.SetNoDelayOption(pvOption:Boolean): Boolean;
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

function TRawSocket.SetRecvBufferLength(const len:Integer): Integer;
begin
  Result := setsockopt(FSocketHandle,SOL_SOCKET,SO_RCVBUF,PAnsiChar(@len),sizeof(Integer));
end;

function TRawSocket.SetSendBufferLength(const len:Integer): Integer;
begin
  Result := setsockopt(FSocketHandle,SOL_SOCKET,SO_SNDBUF,PAnsiChar(@len),sizeof(Integer));
end;

function TRawSocket.SetSendTimeOut(const pvTimeOut:Cardinal): Integer;
var
  nNetTimeout :Cardinal;
begin
  nNetTimeout := pvTimeOut;
  Result := Setsockopt( FSocketHandle,
             SOL_SOCKET,SO_SNDTIMEO,PAnsiChar(@nNetTimeout), Sizeof(Cardinal));
end;

function TRawSocket.ShutDown(pvHow: Integer = SD_BOTH): Integer;
begin
  Result := diocp_winapi_winsock2.shutdown(FSocketHandle, pvHow);
end;

function TRawSocket.SocketValid: Boolean;
begin
  Result := (FSocketHandle<> 0) and (FSocketHandle <> INVALID_HANDLE_VALUE);
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
   __DebugWSACreateCounter := 0;
   __DebugWSACloseCounter := 0;


end.
