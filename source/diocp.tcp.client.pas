(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
unit diocp.tcp.client;

{$I 'diocp.inc'}

interface


uses
  diocp.sockets, SysUtils, diocp.sockets.utils
  {$IFDEF UNICODE}, Generics.Collections{$ELSE}, Contnrs {$ENDIF}
  , Classes;

type
  TIocpRemoteContext = class(TDiocpCustomContext)
  private
    FIsConnecting: Boolean;

    FAutoReConnect: Boolean;
    FConnectExRequest: TIocpConnectExRequest;

    FHost: String;
    FPort: Integer;
    procedure PostConnectRequest;
    procedure ReCreateSocket;
    function CanAutoReConnect:Boolean;
  protected
    procedure OnConnecteExResponse(pvObject:TObject);

    procedure OnDisconnected; override;

    procedure SetSocketState(pvState:TSocketState); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///  阻塞方式建立连接
    ///    连接状态变化: ssDisconnected -> ssConnected/ssDisconnected
    /// </summary>
    procedure Connect;

    /// <summary>
    ///  请求异步连接
    ///   连接状态变化: ssDisconnected -> ssConnecting -> ssConnected/ssDisconnected
    /// </summary>
    procedure ConnectASync;

    /// <summary>
    ///   设置该连接对象的自动重连属性
    ///    true：允许自动重连
    /// </summary>
    property AutoReConnect: Boolean read FAutoReConnect write FAutoReConnect;

    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
  end;

  TDiocpTcpClient = class(TDiocpCustom)
  private
    function GetCount: Integer;
    function GetItems(pvIndex: Integer): TIocpRemoteContext;
  private
    FDisableAutoConnect: Boolean;
  private
  {$IFDEF UNICODE}
    FList: TObjectList<TIocpRemoteContext>;
  {$ELSE}
    FList: TObjectList;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    /// <summary>
    ///   添加一个连对象
    /// </summary>
    function Add: TIocpRemoteContext;

    /// <summary>
    ///   总的连接对象数量
    /// </summary>
    property Count: Integer read GetCount;

    /// <summary>
    ///   禁止所有连接对象自动重连
    /// </summary>
    property DisableAutoConnect: Boolean read FDisableAutoConnect write FDisableAutoConnect;

    /// <summary>
    ///   通过位置索引获取其中的一个连接
    /// </summary>
    property Items[pvIndex: Integer]: TIocpRemoteContext read GetItems; default;

  end;

implementation

uses
  utils.safeLogger, diocp.winapi.winsock2;

resourcestring
  strCannotConnect = '当前状态下不能进行连接...';
  strConnectError  = '建立连接失败, 错误代码:%d';



constructor TIocpRemoteContext.Create;
begin
  inherited Create;
  FAutoReConnect := False;
  FConnectExRequest := TIocpConnectExRequest.Create(Self);
  FConnectExRequest.OnResponse := OnConnecteExResponse;
  FIsConnecting := false;  
end;

destructor TIocpRemoteContext.Destroy;
begin
  FreeAndNil(FConnectExRequest);
  inherited Destroy;
end;

function TIocpRemoteContext.CanAutoReConnect: Boolean;
begin
  Result := FAutoReConnect and (Owner.Active) and (not TDiocpTcpClient(Owner).DisableAutoConnect);
end;

procedure TIocpRemoteContext.Connect;
var
  lvRemoteIP:String;
begin
  if SocketState <> ssDisconnected then raise Exception.Create(strCannotConnect);

  ReCreateSocket;

  try
    lvRemoteIP := RawSocket.GetIpAddrByName(FHost);
  except
    lvRemoteIP := FHost;
  end;

  if not RawSocket.connect(lvRemoteIP, FPort) then
    RaiseLastOSError;

  DoConnected;
end;

procedure TIocpRemoteContext.ConnectASync;
begin
  if SocketState <> ssDisconnected then raise Exception.Create(strCannotConnect);

  ReCreateSocket;

  PostConnectRequest;

end;

procedure TIocpRemoteContext.OnConnecteExResponse(pvObject: TObject);
begin
  FIsConnecting := false;
  if TIocpConnectExRequest(pvObject).ErrorCode = 0 then
  begin
    DoConnected;
  end else
  begin
    {$IFDEF DEBUG_ON}
    Owner.logMessage(strConnectError,  [TIocpConnectExRequest(pvObject).ErrorCode]);
    {$ENDIF}

    DoError(TIocpConnectExRequest(pvObject).ErrorCode);

    if (CanAutoReConnect) then
    begin
      Sleep(100);
      PostConnectRequest;
    end else
    begin
      SetSocketState(ssDisconnected);
    end;
  end;
end;

procedure TIocpRemoteContext.OnDisconnected;
begin
  inherited;
end;

procedure TIocpRemoteContext.PostConnectRequest;
begin
  if lock_cmp_exchange(False, True, FIsConnecting) = False then
  begin
    if RawSocket.SocketHandle = INVALID_SOCKET then
    begin
      ReCreateSocket;
    end;

    if not FConnectExRequest.PostRequest(FHost, FPort) then
    begin
      FIsConnecting := false;

      Sleep(1000);

      if CanAutoReConnect then
        PostConnectRequest;
    end;
  end;
end;

procedure TIocpRemoteContext.ReCreateSocket;
begin
  RawSocket.createTcpOverlappedSocket;
  if not RawSocket.bind('0.0.0.0', 0) then
  begin
    RaiseLastOSError;
  end;

  Owner.IocpEngine.IocpCore.bind2IOCPHandle(RawSocket.SocketHandle, 0);
end;

procedure TIocpRemoteContext.SetSocketState(pvState: TSocketState);
begin
  inherited;
  if pvState = ssDisconnected then
  begin
    if CanAutoReConnect then
    begin
      PostConnectRequest;
    end;
  end;
end;

constructor TDiocpTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF UNICODE}
  FList := TObjectList<TIocpRemoteContext>.Create();
{$ELSE}
  FList := TObjectList.Create();
{$ENDIF}
  FDisableAutoConnect := false;
end;

destructor TDiocpTcpClient.Destroy;
begin
  Close;
  FList.Clear;
  FList.Free;
  inherited Destroy;
end;

function TDiocpTcpClient.Add: TIocpRemoteContext;
begin
  if FContextClass = nil then
  begin
    Result := TIocpRemoteContext.Create;
  end else
  begin
    Result := TIocpRemoteContext(FContextClass.Create());
  end;
  Result.Owner := Self;
  FList.Add(Result);
end;

function TDiocpTcpClient.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDiocpTcpClient.GetItems(pvIndex: Integer): TIocpRemoteContext;
begin
{$IFDEF UNICODE}
  Result := FList[pvIndex];
{$ELSE}
  Result := TIocpRemoteContext(FList[pvIndex]);
{$ENDIF}

end;

end.
