unit diocp_cross_tcpclient;

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
  {$IFDEF MSWINDOWS}
  , diocp_core_rawWinSocket
  {$ELSE}
  , diocp_core_rawPosixSocket
  {$ENDIF} ;

type
  TDiocpCrossTcpClient = class(TObject)
  private
    FConnectTimeOut: Integer;
    FHost: String;
    FPort: Integer;
    FRawSocket: TRawSocket;
    FReadTimeOut: Integer;
    procedure SetReadTimeOut(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    function RecvBuf(pvBuf:Pointer; pvLen:Integer): Integer;
    property ConnectTimeOut: Integer read FConnectTimeOut write FConnectTimeOut;
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property ReadTimeOut: Integer read FReadTimeOut write SetReadTimeOut;




  end;

implementation

procedure TDiocpCrossTcpClient.Connect;
begin
  FRawSocket.CreateTcpSocket;
  if not FRawSocket.ConnectTimeOut(self.Host, self.Port, FConnectTimeOut * 1000) then
  begin
    RaiseLastOSError;
  end;
end;

constructor TDiocpCrossTcpClient.Create;
begin
  inherited Create;
  FRawSocket := TRawSocket.Create();
  FConnectTimeOut := 10;
  FReadTimeOut := 5;
end;

destructor TDiocpCrossTcpClient.Destroy;
begin
  FreeAndNil(FRawSocket);
  inherited Destroy;
end;

function TDiocpCrossTcpClient.RecvBuf(pvBuf:Pointer; pvLen:Integer): Integer;
var
  rc:Integer;
  tm: TIMEVAL;
  fdRead, fdError: {$IFDEF MSWINDOWS}TFdSet{$ELSE}FD_SET{$ENDIF};
begin
  tm.tv_sec := FReadTimeOut;
  tm.tv_usec := 0;

  FD_ZERO(fdRead);
  FD_ZERO(fdError);
{$IFDEF MSWINDOWS}
  FD_SET(FRawSocket.SocketHandle, fdRead);
  FD_SET(FRawSocket.SocketHandle, fdError);
{$ELSE}
  __FD_SET(FRawSocket.SocketHandle, fdRead);
  __FD_SET(FRawSocket.SocketHandle, fdError);
{$ENDIF}
   rc := select(FRawSocket.SocketHandle + 1, @fdRead, nil, @fdError, @tm);
  if (rc > 0) then
  begin
    if FD_ISSET(FRawSocket.SocketHandle, fdRead) and (not FD_ISSET(FRawSocket.SocketHandle, fdError)) then
    begin
       Result := FRawSocket.RecvBuf(pvBuf^, pvLen);
    end;
  end;
end;

procedure TDiocpCrossTcpClient.SetReadTimeOut(const Value: Integer);
begin
  FReadTimeOut := Value;
end;

end.
