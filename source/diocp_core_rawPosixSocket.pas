(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
 
{$IFDEF POSIX}

{$ELSE}
  警告：该文件是跨平台的RawSocket，不要在Windows工程下加入该文件
{$ENDIF}
 
unit diocp_core_rawPosixSocket;

interface

uses
  SysUtils
  , Classes
  , Posix.Base, Posix.SysSocket, Posix.arpainet, Posix.NetinetIn, Posix.UniStd
  , Posix.NetDB
  , Posix.Fcntl
  , Posix.SysSelect
  , Posix.SysTime
  , Posix.StrOpts
  , Posix.Errno;



const
  SOCKET_ERROR   = -1;
  {$EXTERNALSYM SOCKET_ERROR}

{$IFDEF MSWINDOWS}
  SD_RECEIVE = $00;
  {$EXTERNALSYM SD_RECEIVE}
  SD_SEND    = $01;
  {$EXTERNALSYM SD_SEND}
  SD_BOTH    = $02;
  {$EXTERNALSYM SD_BOTH}
{$ENDIF}

type
  TSocketState = (ssDisconnected, ssConnected, ssConnecting, ssListening, ssAccepting);

  TRawSocket = class(TObject)
  private
    FSockaddr: sockaddr_in;
    FSocketHandle:THandle;
    procedure CheckDestroyHandle;
  public
    function Bind(const pvAddr: string; pvPort: Integer): Boolean;
    procedure CreateTcpSocket;
    procedure CreateUdpSocket;

    destructor Destroy; override;

    /// <summary>
    ///   -2:  超时
    /// </summary>
    function RecvBuf(var data; const len: Cardinal; pvTimeOut: Cardinal): Integer;
        overload;
    function RecvBuf(var data; const len: Cardinal): Integer; overload;
    function PeekBuf(var data; const len: Cardinal): Integer;

    /// <summary>
    ///   -2:  超时
    /// </summary>
    function RecvBufEnd(buf: PByte; len: Integer; endBuf: PByte; endBufLen:
        Integer; pvTimeOut: Cardinal): Integer; overload;
    function RecvBufEnd(buf: PByte; len: Integer; endBuf: PByte; endBufLen:
        Integer): Integer; overload;

    function SendBuf(const data; const len: Cardinal): Integer;
    function SendBufTo(const data; const len: Integer): Integer;
    function Connect(const pvAddr: string; pvPort: Integer): Boolean;


    function ConnectTimeOut(const pvAddr: string; pvPort: Integer; pvMs:Cardinal):
        Boolean;

    function ReceiveLength: Integer;


    procedure SetConnectInfo(const pvAddr: string; pvPort: Integer);

    /// <summary>
    ///   can send?
    ///  unit 's
    /// </summary>
    function Writeable(pvTimeOut:Integer): Integer;

    /// <summary>
    ///   check can recv
    ///    unit usec
    /// </summary>
    function Readable(pvTimeOut:Integer): Boolean;

    /// <summary>
    ///   Peer Info
    ///    未测试
    /// </summary>
    function GetPeerInfo(var vIp: longword; var vPort: Integer): Integer;

    /// <summary>
    ///   set NonBlock mode
    /// </summary>
    function SetNonBlock(pvBlock:Boolean): Integer;

    /// <summary>
    ///  resove host
    /// </summary>
    function GetIpAddrByName(const pvHost: string): string;

    /// <summary>
    ///   set recv time out
    ///    unit is ms
    /// </summary>
    function SetReadTimeOut(const pvTimeOut: Cardinal): Integer;


    /// <summary>
    ///   set send time out
    ///    unit is ms
    /// </summary>
    function SetSendTimeOut(const pvTimeOut: Cardinal): Integer;


    /// <summary>
    ///   手动设置套件字句柄
    /// </summary>
    /// <param name="pvSocketHandle"> (THandle) </param>
    procedure SetSocketHandle(pvSocketHandle:THandle);

    function Listen(const backlog: Integer = 0): Boolean;

    procedure Close(pvShutdown: Boolean = true);

    function IsValidSocketHandle: Boolean;

  public
    property SocketHandle: THandle read FSocketHandle;
  end;


implementation

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;

function ioctlsocket(Socket: THandle; Request: Integer; var Data): Integer; inline;
begin
  Result := ioctl(Socket, Request, @Data);
end;

{$IFDEF POSIX}
function TranslateTInAddrToString(var AInAddr): string;
type
  TIdSunB = packed record
    s_b1, s_b2, s_b3, s_b4: Byte;
  end;

  TIdSunW = packed record
    s_w1, s_w2: Word;
  end;
  PIdIn4Addr = ^TIdIn4Addr;
  TIdIn4Addr = packed record
    case integer of
        0: (S_un_b: TIdSunB);
        1: (S_un_w: TIdSunW);
        2: (S_addr: LongWord);
  end;
begin
  Result := IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b1) + '.'   {Do not Localize}
            + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b2) + '.' {Do not Localize}
            + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b3) + '.' {Do not Localize}
            + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b4);
end;


function ResolvingHostName(const pvHost: string): string;
var
  LAddrInfo: pAddrInfo;
  LHints: AddrInfo;
  LRetVal: Integer;
  M: TMarshaller;
begin
  //IMPORTANT!!!
  //
  //The Hints structure must be zeroed out or you might get an AV.
  //I've seen this in Mac OS X
  FillChar(LHints, SizeOf(LHints), 0);
  LHints.ai_family := AF_INET;
  LHints.ai_socktype := SOCK_STREAM;
  LAddrInfo := nil;

  LRetVal := getaddrinfo(M.AsAnsi(pvHost).ToPointer, nil, LHints, LAddrInfo);
  if LRetVal <> 0 then
  begin
    if LRetVal = EAI_SYSTEM then
    begin
      RaiseLastOSError;
    end
    else
    begin
      raise Exception.CreateFmt('Error resolving Address %s: %s (%d)',
        [pvHost, gai_strerror(LRetVal), LRetVal]);
    end;
  end;
  try
    Result := TranslateTInAddrToString(PSockAddr_In( LAddrInfo^.ai_addr)^.sin_addr);
  finally
    freeaddrinfo(LAddrInfo^);
  end;
end;

{$ENDIF}

destructor TRawSocket.Destroy;
begin
  CheckDestroyHandle;
  inherited;
end;

{ TRawSocket }

function TRawSocket.Bind(const pvAddr: string; pvPort: Integer): Boolean;
var
  s :String;
begin
  FillChar(FSockaddr, SizeOf(sockaddr_in), 0);
  FSockaddr.sin_family := AF_INET;
  FSockaddr.sin_port := htons(pvPort);
  s := pvAddr;
  if s = '' then
  begin
    s := '0.0.0.0';
  end;
  // 未测试
  FSockaddr.sin_addr.s_addr :=inet_addr(MarshaledAString(UTF8Encode(s)));
  Result := Posix.SysSocket.Bind(FSocketHandle, sockaddr(FSockaddr), sizeof(sockaddr_in))  = 0;
end;

procedure TRawSocket.CheckDestroyHandle;
var
  lvTempSocket: THandle;
begin
  lvTempSocket := FSocketHandle;
  if (lvTempSocket <> 0) and (lvTempSocket <> INVALID_HANDLE_VALUE) then
  begin
    FSocketHandle := INVALID_HANDLE_VALUE;

    __close(lvTempSocket);
  end;
end;

procedure TRawSocket.Close(pvShutdown: Boolean = true);
var
  lvTempSocket: THandle;
begin
  lvTempSocket := FSocketHandle;
  ///INVALID_SOCKET
  if lvTempSocket <> INVALID_HANDLE_VALUE then
  begin
    FSocketHandle := INVALID_HANDLE_VALUE;
      __close(lvTempSocket);
  end;
end;

function TRawSocket.Connect(const pvAddr: string; pvPort: Integer): Boolean;
begin
  FillChar(FSockaddr, SizeOf(sockaddr_in), 0);
  FSockaddr.sin_family := AF_INET;
  FSockaddr.sin_port := htons(pvPort);
  FSockaddr.sin_addr.s_addr :=inet_addr(MarshaledAString(UTF8Encode(pvAddr)));
  Result := Posix.SysSocket.Connect(FSocketHandle, sockaddr(FSockaddr), sizeof(sockaddr_in))  = 0;

end;

function TRawSocket.ConnectTimeOut(const pvAddr: string; pvPort: Integer;
    pvMs:Cardinal): Boolean;
var
  lvFlags: Cardinal;
  lvErr, lvRet: Integer;
  fs: fd_set;


  tv: timeval;
  Timeptr: PTimeval;
  valopt,vallen: Cardinal;
begin
  // 获取原标志
  lvFlags := fcntl(FSocketHandle, F_GETFL, 0);
  lvFlags := lvFlags OR (O_NONBLOCK);  // 非阻塞模式
  fcntl(FSocketHandle, F_SETFL, lvFlags);

  FillChar(FSockaddr, SizeOf(sockaddr_in), 0);
  FSockaddr.sin_family := AF_INET;
  FSockaddr.sin_port := htons(pvPort);

  FSockaddr.sin_addr.s_addr :=inet_addr(MarshaledAString(UTF8Encode(pvAddr)));
  lvRet := Posix.SysSocket.Connect(FSocketHandle, sockaddr(FSockaddr), sizeof(sockaddr_in));
  lvErr := GetLastError;
  if lvRet = 0 then
  begin  // 连接成功
    lvFlags := lvFlags AND (NOT O_NONBLOCK);  // 阻塞模式
    fcntl(FSocketHandle, F_SETFL, lvFlags);
    Result := true;
  end else if lvErr = 36 then
  begin
    //RaiseLastOSError;

    FD_ZERO(fs);
    _FD_SET(FSocketHandle, fs);

    tv.tv_sec := pvMs div 1000;
    tv.tv_usec :=  1000 * (pvMs mod 1000);
    Timeptr := @tv;

    lvRet := select(FSocketHandle + 1, nil, @fs, nil, Timeptr);

    if lvRet <= 0 then
    begin
      Result := false;  //连接超时
//      lvErr := WSAGetLastError;
      __close(FSocketHandle);

//      Result := lvErr = 0;
    end else
    begin
      // add by [沈阳]u  83055474
      // 2015-05-11 11:41:48
      getsockopt(FSocketHandle, SOL_SOCKET, SO_ERROR,valopt,  vallen);
      if(valopt>0)then
      begin
        Result := false;
        exit;
      end;
      Result := true;
      lvFlags := lvFlags AND (NOT O_NONBLOCK);  // 阻塞模式
      fcntl(FSocketHandle, F_SETFL, lvFlags);
    end;
  end; 
end;


procedure TRawSocket.CreateTcpSocket;
begin
  CheckDestroyHandle;
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocketHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
end;

procedure TRawSocket.CreateUdpSocket;
begin
  CheckDestroyHandle;
  FSocketHandle := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
end;

function TRawSocket.Readable(pvTimeOut:Integer): Boolean;
var
  lvFDSet:fd_set;
  lvTime_val: timeval;
begin
  // 经过测试
  // 在 android下面 该函数无效, 可以用receiveLength > 0判断
  FD_ZERO(lvFDSet);
  _FD_SET(FSocketHandle, lvFDSet);

  lvTime_val.tv_sec := pvTimeOut div 1000;
  lvTime_val.tv_usec :=  1000 * (pvTimeOut mod 1000);
  Result := select(0, @lvFDSet, nil, nil, @lvTime_val) > 0;
end;



function TRawSocket.SendBuf(const data; const len: Cardinal): Integer;
begin
  Result := Send(FSocketHandle, data, len, 0);
end;

function TRawSocket.SendBufTo(const data; const len: Integer): Integer;
begin
{$IFDEF POSIX}
  Result := sendto(FSocketHandle, data, len, 0, sockaddr(FSockaddr), sizeof(sockaddr_in));
{$ELSE}
  Result := sendto(FSocketHandle, data, len, 0, FSockaddr, sizeof(sockaddr_in));
{$ENDIF}
end;

function TRawSocket.SetReadTimeOut(const pvTimeOut: Cardinal): Integer;
begin
{$IFDEF POSIX}
  // 未测试
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_RCVTIMEO, pvTimeOut, SizeOf(Cardinal));
{$ELSE} 
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@pvTimeOut), SizeOf(Cardinal));
{$ENDIF}
end;


function TRawSocket.SetSendTimeOut(const pvTimeOut: Cardinal): Integer;
begin
{$IFDEF POSIX}
  // 未测试
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_SNDTIMEO, pvTimeOut, SizeOf(Cardinal));
{$ELSE}
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@pvTimeOut), SizeOf(Cardinal));
{$ENDIF}
end;

function TRawSocket.Writeable(pvTimeOut:Integer): Integer;
var
  lvFDSet:fd_set;
  lvTime_val: timeval;
begin
  // 未测试
  FD_ZERO(lvFDSet);
  _FD_SET(FSocketHandle, lvFDSet);

  lvTime_val.tv_sec := pvTimeOut;
  lvTime_val.tv_usec := 0;
  Result := select(0, nil, @lvFDSet, nil, @lvTime_val); 
end;

function TRawSocket.GetIpAddrByName(const pvHost: string): string;
begin
  Result := ResolvingHostName(pvHost);
end;

function TRawSocket.GetPeerInfo(var vIp: longword; var vPort: Integer):
    Integer;
{$IFDEF POSIX}
{$ELSE}
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
{$ENDIF}
begin
{$IFDEF POSIX}
{$ELSE}  
  Size := SizeOf(SockAddrIn);
  result := getpeername(FSocketHandle, TSockAddr(SockAddrIn), Size);
  vIp := SockAddrIn.sin_addr.S_addr;
  vPort := ntohs(SockAddrIn.sin_port);
{$ENDIF}
end;

function TRawSocket.IsValidSocketHandle: Boolean;
begin
   Result := FSocketHandle <> INVALID_HANDLE_VALUE;
end;

function TRawSocket.Listen(const backlog: Integer = 0): Boolean;
var
  queueSize: Integer;
begin
  // 未测试
  if backlog = 0 then
  begin
    queueSize := SOMAXCONN;
  end
  else begin
    queueSize := backlog;
  end;
  Result := Posix.SysSocket.Listen(FSocketHandle, queueSize) = 0;
end;

function TRawSocket.PeekBuf(var data; const len:Cardinal): Integer;
begin
  Result := recv(FSocketHandle, data, len, MSG_PEEK);
end;

function TRawSocket.RecvBuf(var data; const len: Cardinal; pvTimeOut:
    Cardinal): Integer;
var
  lvTick : Cardinal;
begin
  lvTick := TThread.GetTickCount;
  while True do
  begin
    if (tick_diff(lvTick, TThread.GetTickCount) > pvTimeOut) then
    begin
      Result := -2;
      Exit;
    end else  if ReceiveLength > 0 then
    begin
      Result := recv(FSocketHandle, data, len, 0);
      Exit;
    end else
    begin
      Sleep(10);
    end;
  end;
end;

function TRawSocket.RecvBufEnd(buf: PByte; len: Integer; endBuf: PByte;
    endBufLen: Integer; pvTimeOut: Cardinal): Integer;
var
  lvRecvByte:byte;
  lvRet, j:Integer;
  lvTempEndBuf:PByte;
  lvMatchCounter:Integer;
  lvTick:Cardinal;
begin
  lvTempEndBuf := endBuf;
  lvMatchCounter := 0;
  j:=0;
  lvTick := TThread.GetTickCount;
  while j < len do
  begin
    if (tick_diff(lvTick, TThread.GetTickCount) > pvTimeOut) then
    begin
      Result := -2;
      Exit;
    end else  if ReceiveLength > 0 then
    begin
      lvRet := recv(FSocketHandle, buf^, 1, 0);
      //lvRet := RecvBuf(buf^, 1);   // 阻塞读取一个字节
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
      if Byte(buf^) = Byte(lvTempEndBuf^) then
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

function TRawSocket.ReceiveLength: Integer;
var
  r :Integer;
begin
  r := ioctlsocket(FSocketHandle, FIONREAD, Cardinal(Result));
  if r = -1 then
  begin
    Result := 0;
  end;
end;

function TRawSocket.RecvBuf(var data; const len: Cardinal): Integer;
begin
  Result := recv(FSocketHandle, data, len, 0);
end;

function TRawSocket.RecvBufEnd(buf: PByte; len: Integer; endBuf: PByte;
    endBufLen: Integer): Integer;
var
  lvRecvByte:byte;
  lvRet, j:Integer;
  lvTempEndBuf:PByte;
  lvMatchCounter:Integer;
begin
  lvTempEndBuf := endBuf;
  lvMatchCounter := 0;
  j:=0;
  while j < len do
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
    if Byte(buf^) = Byte(lvTempEndBuf^) then
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

procedure TRawSocket.SetConnectInfo(const pvAddr: string; pvPort: Integer);
begin
  FillChar(FSockaddr, SizeOf(sockaddr_in), 0);
  FSockaddr.sin_family := AF_INET;
  FSockaddr.sin_port := htons(pvPort);
  FSockaddr.sin_addr.s_addr :=inet_addr(MarshaledAString(UTF8Encode(pvAddr)));
end;

function TRawSocket.SetNonBlock(pvBlock:Boolean): Integer;
begin
  // not test
  if pvBlock then
  begin
    Result := fcntl(FSocketHandle, F_SETFL, O_SYNC);
  end else
  begin
    Result := fcntl(FSocketHandle, F_SETFL, O_NONBLOCK);
  end;
end;

procedure TRawSocket.SetSocketHandle(pvSocketHandle:THandle);
begin
  FSocketHandle := pvSocketHandle;
end;

initialization

end.
