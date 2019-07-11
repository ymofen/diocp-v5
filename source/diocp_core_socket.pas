unit diocp_core_socket;

interface

uses
  SysUtils
{$IFDEF POSIX}
    , Posix.Base, Posix.Stdio, Posix.Pthread, Posix.UniStd, IOUtils,
  Posix.NetDB, Posix.SysSocket, Posix.Fcntl, Posix.StrOpts, Posix.Errno,
  Posix.NetinetIn, Posix.arpainet, Posix.SysSelect, Posix.Systime
{$ELSE}
    , windows, messages, winsock, TlHelp32
{$ENDIF}
  ;

{$IFDEF MSWINDOWS}
{$ELSE}
const
  INVALID_SOCKET = -1;
{$ENDIF}

type
  TDiocpSocket = class(TObject)
  private
    FSocketHandle: THandle;
  public
    procedure RaiseLastError;
    function CreateTcpSocket: Boolean;
    function SetOptionNoneBlock(pvVal:Boolean): Boolean;
    /// <summary>
    ///   连接（使用超时设定）
    /// </summary>
    /// <returns>
    ///    0:连接成功
    ///   -1:连接出现错误
    ///   -2:连接超时
    /// </returns>
    function ConnectTimeOut(const pvAddr: string; pvPort: Integer;
        pvTimeOutSecs:Integer): Integer;
    function RecvBufTimeOut(pvBuf: Pointer; pvLen, pvTimeOutSecs: Integer): Integer;

    property SocketHandle: THandle read FSocketHandle;
  end;

implementation

{$IFDEF MSWINDOWS}
var
  __WSAStartupDone:Boolean;
{$ENDIF}

function TDiocpSocket.ConnectTimeOut(const pvAddr: string; pvPort: Integer;
    pvTimeOutSecs:Integer): Integer;
var
  Addr: sockaddr_in;
  tm: TIMEVAL;
  lvFlag, r: Integer;
  fdWrite, fdError: {$IFDEF MSWINDOWS}TFdSet{$ELSE}FD_SET{$ENDIF};
begin
  FD_ZERO(fdWrite);
  FD_ZERO(fdError);
{$IFDEF MSWINDOWS}
  FD_SET(FSocketHandle, fdWrite);
  FD_SET(FSocketHandle, fdError);
{$ELSE}
  _FD_SET(FSocketHandle, fdWrite);
  _FD_SET(FSocketHandle, fdError);
{$ENDIF}

  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(pvPort);
  {$IFDEF MSWINDOWS}
  Addr.sin_addr.S_addr := inet_addr(PAnsichar(UTF8Encode(pvAddr)));
  {$ELSE}
  Addr.sin_addr.S_addr := inet_addr(MarshaledAString(UTF8Encode(pvAddr)));
  {$ENDIF}
  tm.tv_sec := pvTimeOutSecs;
  tm.tv_usec := 0;
  if not SetOptionNoneBlock(True) then
  begin
    Result := -1;
    exit;
  end;

  CONNECT(FSocketHandle, {$IFDEF MSWINDOWS}sockaddr_in{$ELSE}sockaddr{$ENDIF}(Addr), SizeOf(Addr));

  r := select(FSocketHandle + 1, nil, @fdWrite, @fdError, @tm);
  if r >=0 then
  begin
    if FD_ISSET(FSocketHandle, fdError) then
    begin
      Result := -1;
      Exit;
    end;

    if not FD_ISSET(FSocketHandle, fdWrite) then
    begin
      // 超时
      Result := -2;
      exit;
    end;

  end else
  begin
    Result := -1;
    exit;
  end;

  if not SetOptionNoneBlock(False) then
  begin
    Result := -1;
    exit;
  end;
end;

{ TDiocpSocket }

function TDiocpSocket.CreateTcpSocket: Boolean;
begin
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    Result := false;
  end else
  begin
    Result := True;
  end;
end;

procedure TDiocpSocket.RaiseLastError;
begin
  RaiseLastOSError();
end;

function TDiocpSocket.RecvBufTimeOut(pvBuf: Pointer; pvLen, pvTimeOutSecs:
    Integer): Integer;
var
  rc:Integer;
  tm: TIMEVAL;
  fdRead, fdError: {$IFDEF MSWINDOWS}TFdSet{$ELSE}FD_SET{$ENDIF};
begin
  tm.tv_sec := pvTimeOutSecs;
  tm.tv_usec := 0;

  FD_ZERO(fdRead);
  FD_ZERO(fdError);
{$IFDEF MSWINDOWS}
  FD_SET(FSocketHandle, fdRead);
  FD_SET(FSocketHandle, fdError);
{$ELSE}
  __FD_SET(FSocketHandle, fdRead);
  __FD_SET(FSocketHandle, fdError);
{$ENDIF}
  rc := select(FSocketHandle + 1, @fdRead, nil, @fdError, @tm);
  if (rc > 0) then
  begin
    if FD_ISSET(FSocketHandle, fdRead) and (not FD_ISSET(FSocketHandle, fdError)) then
    begin
       Result := recv(FSocketHandle, pvBuf^, pvLen, 0);
    end else
    begin
      Result := 0;
    end;
  end else
  begin
    Result := 0;
  end;
end;

function TDiocpSocket.SetOptionNoneBlock(pvVal:Boolean): Boolean;
var
  lvFlag:Integer;
begin
  {$IFDEF MSWINDOWS}
  if pvVal then
  begin
    lvFlag := 1;
  end else
  begin
    lvFlag := 0;
  end;
  Result := ioctlsocket(FSocketHandle, FIONBIO, lvFlag) = NO_ERROR;
  {$ELSE}
  lvFlag := Fcntl(FSocketHandle, F_GETFL, 0);
  if lvFlag = -1 then
  begin
    Result := false;
    Exit;
  end;

  if pvVal then
  begin
    lvFlag := lvFlag or O_NONBLOCK;
  end else
  begin
    lvFlag := lvFlag AND (NOT O_NONBLOCK)
  end;
  Result := Fcntl(FSocketHandle, F_SETFL, lvFlag) <> -1;
  {$ENDIF}

end;

{$IFDEF MSWINDOWS}
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
{$ENDIF}

initialization
{$IFDEF MSWINDOWS}
   __CheckWSAStartup();
{$ENDIF}


end.
