(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)

unit diocp_sockets_utils;

interface

uses
  Windows, SysUtils, diocp_winapi_winsock2, diocp_res;

type
// 25:XE5
{$IF CompilerVersion<=25}
  NativeUInt = Cardinal;

  IntPtr = Cardinal;
{$ifend}

// XE2 = 22
{$if CompilerVersion < 23}

  ULONG_PTR = Cardinal;
{$else}

  ULONG_PTR = NativeUInt;
{$ifend}

type


  TIocpAcceptEx = function(sListenSocket, sAcceptSocket: TSocket; lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD; var lpdwBytesReceived: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;

  TIocpConnectEx = function(const s: TSocket; const name: PSOCKADDR; const namelen: Integer; lpSendBuffer: Pointer; dwSendDataLength: DWORD; var lpdwBytesSent: DWORD; lpOverlapped: LPWSAOVERLAPPED): BOOL; stdcall;


  //  Extention function "GetAcceptExSockAddrs"
  TIocpGetAcceptExSockAddrs = procedure(lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD; var LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer; var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer); stdcall;

  TIocpDisconnectEx = function(const hSocket: TSocket; lpOverlapped: LPWSAOVERLAPPED; const dwFlags: DWORD; const dwReserved: DWORD): BOOL; stdcall;

  TDiocpCancelIoEx =  function(hFile: THandle; lpOverlapped: LPWSAOVERLAPPED): BOOL; stdcall;



const
  IOCP_RESULT_OK = 0;
  IOCP_RESULT_QUIT = 1;



// copy from dx10

function CreateIoCompletionPort(FileHandle, ExistingCompletionPort: THandle; CompletionKey: ULONG_PTR; NumberOfConcurrentThreads: DWORD): THandle; stdcall;
{$EXTERNALSYM CreateIoCompletionPort}

function GetQueuedCompletionStatus(CompletionPort: THandle; var lpNumberOfBytesTransferred: DWORD; var lpCompletionKey: ULONG_PTR; var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetQueuedCompletionStatus}

var
  IocpAcceptEx: TIocpAcceptEx;
  IocpConnectEx: TIocpConnectEx;
  IocpDisconnectEx: TIocpDisconnectEx;
  IocpGetAcceptExSockaddrs: TIocpGetAcceptExSockAddrs;
  DiocpCancelIoEx:TDiocpCancelIoEx;

function GetSocketAddr(pvAddr: string; pvPort: Integer): TSockAddrIn;

function socketBind(s: TSocket; const pvAddr: string; pvPort: Integer): Boolean;


/// <summary>
/// 设置KeepAlive 检测选项
/// </summary>
/// <returns> Boolean
/// </returns>
/// <param name="pvSocket"> (TSocket) </param>
/// <param name="pvKeepAliveTime"> (Integer) </param>
function SetKeepAlive(pvSocket: TSocket; pvKeepAliveTime: Integer = 5000): Boolean;

function CreateTcpOverlappedSocket: THandle;

procedure CheckWinSocketStart;

function TransByteSize(pvByte: Int64): string;

function GetRunTimeINfo: string;

function NowString: String;

/// <summary>
///   生成一个Diocp的句柄（注意不能跨DLL),否则会重复
///   线程安全
///   由于Socket的Handle在关闭后，会重用
/// </summary>
function MakeDiocpHandle: THandle;

function WSocket_Synchronized_ResolveHost(InAddr: AnsiString): TInAddr;

function WSocketIsDottedIP(const S: AnsiString): Boolean;

implementation

uses
  DateUtils;

var
  hHandle: HMODULE;

const
  SIO_KEEPALIVE_VALS = IOC_IN or IOC_VENDOR or 4;

{ Other NT-specific options. }

  {$EXTERNALSYM SO_MAXDG}
  SO_MAXDG = $7009;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_MAXPATHDG = $700A;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_UPDATE_ACCEPT_CONTEXT = $700B;
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

const
  WINSOCK_LIB_VERSION: Word = $0202;

const
  WSAID_GETACCEPTEXSOCKADDRS: TGuid = (
    D1: $b5367df2;
    D2: $cbac;
    D3: $11cf;
    D4: ($95, $ca, $00, $80, $5f, $48, $a1, $92)
  );
  WSAID_ACCEPTEX: TGuid = (
    D1: $b5367df1;
    D2: $cbac;
    D3: $11cf;
    D4: ($95, $ca, $00, $80, $5f, $48, $a1, $92)
  );
  WSAID_CONNECTEX: TGuid = (
    D1: $25a207b9;
    D2: $ddf3;
    D3: $4660;
    D4: ($8e, $e9, $76, $e5, $8c, $74, $06, $3e)
  );
  {$EXTERNALSYM WSAID_DISCONNECTEX}
  WSAID_DISCONNECTEX: TGuid = (
    D1: $7fda2e11;
    D2: $8630;
    D3: $436f;
    D4: ($a0, $31, $f5, $36, $a6, $ee, $c1, $57)
  );

var
  __CheckWinSocketStart: Boolean;
  __startTime:TDateTime;

  __DiocpHandle:Integer;

{$if CompilerVersion < 15}  // <D7
const
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;
{$ifend}

function GetRunTimeINfo: string;
var
  lvMSec, lvRemain: Int64;
  lvDay, lvHour, lvMin, lvSec: Integer;
begin
  lvMSec := MilliSecondsBetween(Now(), __startTime);
  lvDay := Trunc(lvMSec / MSecsPerDay);
  lvRemain := lvMSec mod MSecsPerDay;

  lvHour := Trunc(lvRemain / (MSecsPerSec * 60 * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60 * 60);

  lvMin := Trunc(lvRemain / (MSecsPerSec * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60);

  lvSec := Trunc(lvRemain / (MSecsPerSec));

  if lvDay > 0 then
    Result := Result + IntToStr(lvDay) + ' d ';

  if lvHour > 0 then
    Result := Result + IntToStr(lvHour) + ' h ';

  if lvMin > 0 then
    Result := Result + IntToStr(lvMin) + ' m ';

  if lvSec > 0 then
    Result := Result + IntToStr(lvSec) + ' s ';
end;

function TransByteSize(pvByte: Int64): string;
var
  lvTB, lvGB, lvMB, lvKB: Word;
  lvRemain: Int64;
begin
  lvRemain := pvByte;

  lvTB := Trunc(lvRemain / BytePerGB / 1024);
  //lvRemain := pvByte - (lvTB * BytePerGB * 1024);
  lvGB := Trunc(lvRemain / BytePerGB);

  lvGB := lvGB mod 1024;      // trunc TB
  lvRemain := lvRemain mod BytePerGB;

  lvMB := Trunc(lvRemain / BytePerMB);
  lvRemain := lvRemain mod BytePerMB;

  lvKB := Trunc(lvRemain / BytePerKB);
  lvRemain := lvRemain mod BytePerKB;
  Result := Format('%d TB, %d GB, %d MB, %d KB, %d B', [lvTB, lvGB, lvMB, lvKB, lvRemain]);
end;


/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean): Boolean; overload;
asm
{$ifdef win32}
        lock    cmpxchg[ecx], dl
{$else}
        .       noframe
        mov     rax, rcx
        lock    cmpxchg[r8], dl
{$endif}
end;



function GetQueuedCompletionStatus; external kernel32 name 'GetQueuedCompletionStatus';

function CreateIoCompletionPort; external kernel32 name 'CreateIoCompletionPort';

function creatTcpSocketHandle: THandle;
begin
  Result := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_IP, nil, 0, WSA_FLAG_OVERLAPPED);
  if (Result = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadAcceptEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER, @WSAID_ACCEPTEX, SizeOf(WSAID_ACCEPTEX), @@IocpAcceptEx, SizeOf(Pointer), bytesReturned, nil, nil);

  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadDisconnectEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER, @WSAID_DISCONNECTEX, SizeOf(WSAID_DISCONNECTEX), @@IocpDisconnectEx, SizeOf(Pointer), bytesReturned, nil, nil);

  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadAcceptExSockaddrs(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER, @WSAID_GETACCEPTEXSOCKADDRS, SizeOf(WSAID_GETACCEPTEXSOCKADDRS), @@IocpGetAcceptExSockaddrs, SizeOf(Pointer), bytesReturned, nil, nil);

  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadConnecteEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER, @WSAID_CONNECTEX, SizeOf(WSAID_CONNECTEX), @@IocpConnectEx, SizeOf(Pointer), bytesReturned, nil, nil);
  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure WSAStart;
var
  lvRET: Integer;
  WSData: TWSAData;
begin
  lvRET := WSAStartup($0202, WSData);
  if lvRET <> 0 then
    RaiseLastOSError;
end;

procedure loadExFunctions;
var
  skt: TSocket;
begin
  skt := creatTcpSocketHandle;
  LoadAcceptEx(skt);
  LoadConnecteEx(skt);
  LoadAcceptExSockaddrs(skt);
  LoadDisconnectEx(skt);
  closesocket(skt);
end;

function GetSocketAddr(pvAddr: string; pvPort: Integer): TSockAddrIn;
begin
  Result.sin_family := AF_INET;
  Result.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(pvAddr)));
  Result.sin_port := htons(pvPort);
end;

function socketBind(s: TSocket; const pvAddr: string; pvPort: Integer): Boolean;
var
  sockaddr: TSockAddrIn;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    sin_port := htons(pvPort);
  end;
  Result := diocp_winapi_winsock2.bind(s, PSockAddr(@sockaddr), SizeOf(sockaddr)) = 0;
end;

function SetKeepAlive(pvSocket: TSocket; pvKeepAliveTime: Integer = 5000): Boolean;
var
  Opt, insize, outsize: integer;
  outByte: DWORD;
  inKeepAlive, outKeepAlive: TTCP_KEEPALIVE;
begin
  Result := false;
  Opt := 1;
  if SetSockopt(pvSocket, SOL_SOCKET, SO_KEEPALIVE, @Opt, sizeof(Opt)) = SOCKET_ERROR then
    exit;

  inKeepAlive.OnOff := 1;

  inKeepAlive.KeepAliveTime := pvKeepAliveTime;

  inKeepAlive.KeepAliveInterval := 1;
  insize := sizeof(TTCP_KEEPALIVE);
  outsize := sizeof(TTCP_KEEPALIVE);

  if WSAIoctl(pvSocket, SIO_KEEPALIVE_VALS, @inKeepAlive, insize, @outKeepAlive, outsize, outByte, nil, nil) <> SOCKET_ERROR then
  begin
    Result := true;
  end;
end;

function CreateTcpOverlappedSocket: THandle;
begin
  Result := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, Nil, 0, WSA_FLAG_OVERLAPPED);
  if (Result = 0) or (Result = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

procedure CheckWinSocketStart;
begin
  if lock_cmp_exchange(False, True, __CheckWinSocketStart) = False then
  begin
    WSAStart;
    loadExFunctions;
  end;
end;

function NowString: String;
begin
  Result := FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', Now);
end;

function MakeDiocpHandle: THandle;
var
  v:Integer;
begin
  v := InterlockedIncrement(__DiocpHandle);
  Result := THandle(v);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check for a valid numeric dotted IP address such as 192.161.65.25         }
{ Accept leading and trailing spaces.                                       }

function WSocketIsDottedIP(const S: AnsiString): Boolean;
var
  I: Integer;
  DotCount: Integer;
  NumVal: Integer;
begin
  Result := False;
  DotCount := 0;
  NumVal := 0;
  I := 1;

  { Skip leading spaces }
  while (I <= Length(S)) and (S[I] = ' ') do
    Inc(I);
  { Can't begin with a dot }
  if (I <= Length(S)) and (S[I] = '.') then
    Exit;
  { Scan full AnsiString }
  while I <= Length(S) do
  begin
    if S[I] = '.' then
    begin
      Inc(DotCount);
      if (DotCount > 3) or (NumVal > 255) then
        Exit;
      NumVal := 0;
      { A dot must be followed by a digit }
      if (I >= Length(S)) or (not (AnsiChar(S[I + 1]) in ['0'..'9'])) then
        Exit;
    end
    else if AnsiChar(S[I]) in ['0'..'9'] then
      NumVal := NumVal * 10 + Ord(S[I]) - Ord('0')
    else
    begin
      { Not a digit nor a dot. Accept spaces until end of AnsiString }
      while (I <= Length(S)) and (S[I] = ' ') do
        Inc(I);
      if I <= Length(S) then
        Exit; { Not a space, do not accept }
      break; { Only spaces, accept        }
    end;
    Inc(I);
  end;
  { We must have exactly 3 dots }
  if (DotCount <> 3) or (NumVal > 255) then
    Exit;
  Result := True;
end;

function WSocket_Synchronized_ResolveHost(InAddr: AnsiString): TInAddr;
var
  Phe: PHostEnt;
  IPAddr: u_long;
begin
  if InAddr = '' then
    {  raise ESocketException.Create('WSocketResolveHost: ''' + InAddr + ''' Invalid Hostname.'); }
    raise Exception.Create('Winsock Resolve Host: ''' + string(InAddr) + ''' Invalid Hostname.'); { V5.26 }

  if WSocketIsDottedIP(InAddr) then
  begin
    { Address is a dotted numeric address like 192.161.124.32 }
    IPAddr := inet_addr(PAnsiChar(InAddr));

    if IPAddr = u_long(INADDR_NONE) then
    begin
      if InAddr = '255.255.255.255' then
      begin
        Result.s_addr := u_long(INADDR_BROADCAST);
        Exit;
      end;
      {     raise ESocketException.Create('WSocketResolveHost: ''' + InAddr + ''' Invalid IP address.');  }
      raise Exception.Create('Winsock Resolve Host: ''' + string(InAddr) + ''' Invalid IP address.'); { V5.26 }
    end;
    Result.s_addr := IPAddr;
    Exit;
  end;

  { Address is a hostname }
  Phe := GetHostByName(PAnsiChar(InAddr));
  if Phe = nil then
    //raise Exception.Create('Winsock Resolve Host: Cannot convert host address ''' + InAddr + '''');
    Result.S_addr := u_long(INADDR_NONE)
  else
    Result.s_addr := PInAddr(Phe^.h_addr_list^)^.s_addr;

  //Result :=string(inet_ntoa(PInAddr(lvhostInfo^.h_addr_list^)^));
end;

initialization
  __CheckWinSocketStart := False;
  __startTime :=  Now();
  __DiocpHandle := 1;

  hHandle := LoadLibrary(kernel32);
  if hHandle <> 0 then
  begin
    DiocpCancelIoEx := TDiocpCancelIoEx(GetProcAddress(hHandle, 'CancelIoEx'));
  end else
  begin
    DiocpCancelIoEx := nil;
  end;

finalization
  if hHandle <> 0 then
    FreeLibrary(hHandle);

end.

