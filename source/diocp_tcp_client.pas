(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *   1. 修复ex.tcpclient编码问题，发送大数据时，无法解码的bug
 *      2015-08-17 14:25:56
 *)
unit diocp_tcp_client;

{$I 'diocp.inc'}

interface


uses
  diocp_sockets, SysUtils, diocp_sockets_utils
  {$IFDEF UNICODE}, Generics.Collections{$ELSE}, Contnrs {$ENDIF}
  , Classes, Windows, utils_objectPool, diocp_res
  , diocp_core_rawWinSocket
  , utils_async
  , utils_fileWriter
  , utils_threadinfo
  , utils_queues, SyncObjs;

type
  //TCheck
  /// <summary>
  ///   如果是getFromPool不能进Server的List列表
  /// </summary>
  TIocpRemoteContext = class(TDiocpCustomContext)
  private
    FLastDisconnectTime:Cardinal;
    FIsConnecting: Boolean;
    FBindingHandle:THandle;

    FAutoReConnect: Boolean;
    FConnectExRequest: TIocpConnectExRequest;

    FOnConnectFailEvent: TNotifyContextEvent;
    FOnASyncCycle: TNotifyContextEvent;

    FHost: String;
    FPort: Integer;
    /// <summary>TIocpRemoteContext.PostConnectRequest
    /// </summary>
    /// <returns>
    ///   投递成功或者正在连接返回true
    /// </returns>
    function PostConnectRequest: Boolean;
    procedure ReCreateSocket;

    function CanAutoReConnect:Boolean;

    procedure CheckDestroyBindingHandle;

    procedure DoConnectFail;
  protected
    procedure OnConnecteExResponse(pvObject:TObject);

    procedure OnDisconnected; override;

    procedure OnConnected; override;

    procedure OnConnectFail; virtual;

    procedure SetSocketState(pvState:TSocketState); override;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;

    procedure CheckCanConnect;

  public
    /// <summary>
    ///   进行重连连接，
    ///   如果符合要求
    ///     1. 已经断线
    ///   则进行重连
    /// </summary>
    procedure CheckDoReConnect;

    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///  阻塞方式建立连接
    ///    连接状态变化: ssDisconnected -> ssConnected/ssDisconnected
    /// </summary>
    procedure Connect; overload;

    procedure Connect(pvTimeOut:Integer); overload;

    /// <summary>
    ///  请求异步连接
    ///    连接状态变化: ssDisconnected -> ssConnecting -> ssConnected/ssDisconnected
    ///    如果投递失败，或者连接失败，如果Owner.TrigerDisconnectEventAfterNoneConnected为true触发OnDisconnected
    /// </summary>
    procedure ConnectASync;

    /// <summary>
    ///   设置该连接对象的自动重连属性
    ///    true：允许自动重连
    /// </summary>
    property AutoReConnect: Boolean read FAutoReConnect write FAutoReConnect;

    /// <summary>
    ///   由ASync线程, 循环执行
    /// </summary>
    property OnASyncCycle: TNotifyContextEvent read FOnASyncCycle write FOnASyncCycle;

    /// <summary>
    ///   连接失败事件
    /// </summary>
    property OnConnectFailEvent: TNotifyContextEvent read FOnConnectFailEvent write
        FOnConnectFailEvent;

    property Host: String read FHost write FHost;



    property Port: Integer read FPort write FPort;



  end;

  /// <summary>
  ///   注意
  ///   Add, ClearContexts, 对列表进行写入，没有对列表进行线程安全处理
  ///   Find, CheckContext，Items函数也没有对列表进行锁定
  ///   所以，最好在开始之前对列表进行处理列表. 在停止后对列表进行ClearContexts
  /// </summary>
  TDiocpTcpClient = class(TDiocpCustom)
  private
    function GetCount: Integer;
    function GetItems(pvIndex: Integer): TIocpRemoteContext;
  private
    FDisableAutoConnect: Boolean;
    FAutoConnectTick:Cardinal;

  private
    /// <summary>
    ///  检测使用重新连接 ,单线程使用，仅供DoAutoReconnect调用
    ///    间隔最少5秒以上
    /// </summary>
    procedure DoAutoReconnect(pvASyncWorker:TASyncWorker);

    /// <summary>
    ///    检测使用重新连接 ,单线程使用，仅供DoASyncCycle调用
    ///    间隔最少5秒以上
    /// </summary>
    procedure DoASyncCycle(pvASyncWorker:TASyncWorker);
    procedure SetTrigerDisconnectEventAfterNoneConnected(const Value: Boolean);
  protected
    procedure DoASyncWork(pvFileWritter: TSingleFileWriter; pvASyncWorker:
        TASyncWorker); override;
    procedure SetDisableAutoConnect(const Value: Boolean);

    /// <summary>
    ///   occur on create instance
    /// </summary>
    procedure OnCreateContext(const pvContext: TDiocpCustomContext); override;
  private
  {$IFDEF UNICODE}
    FList: TObjectList<TIocpRemoteContext>;
  {$ELSE}
    FList: TObjectList;
  {$ENDIF}

    FTrigerDisconnectEventAfterNoneConnected: Boolean;
    FOnContextConnectFailEvent: TNotifyContextEvent;
  protected
    FListLocker: TCriticalSection;
    procedure DoAfterOpen;override;
    procedure DoAfterClose; override; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    /// <summary>
    ///   清理Add创建的所有连接
    /// </summary>
    procedure ClearContexts;

    /// <summary>
    ///   添加一个连对象
    /// </summary>
    function Add: TIocpRemoteContext;

    /// <summary>
    ///   pvContext是否是当前列表中的对象
    ///   nil:不是
    /// </summary>
    function CheckContext(pvContext:TObject): TIocpRemoteContext;

    
    function GetStateInfo: String;




    /// <summary>
    ///  禁止重连
    ///    请求断开所有
    ///    等待断开所有(30000)
    ///    清理所有
    /// </summary>
    procedure RemoveAllContext;

    /// <summary>
    ///   总的连接对象数量
    /// </summary>
    property Count: Integer read GetCount;

    /// <summary>
    ///   禁止所有连接对象自动重连
    ///   默认是不禁止
    /// </summary>
    property DisableAutoConnect: Boolean read FDisableAutoConnect write
        SetDisableAutoConnect;

    /// <summary>
    ///    通过位置索引获取其中的一个连接
    /// </summary>
    property Items[pvIndex: Integer]: TIocpRemoteContext read GetItems; default;

    /// <summary>
    ///  为true时: 即使连接失败的情况下，触发OnDisconnected事件, 默认为true
    /// </summary>
    property TrigerDisconnectEventAfterNoneConnected: Boolean read
        FTrigerDisconnectEventAfterNoneConnected write
        SetTrigerDisconnectEventAfterNoneConnected;


    /// <summary>
    ///   连接失败
    /// </summary>
    property OnContextConnectFailEvent: TNotifyContextEvent read FOnContextConnectFailEvent
        write FOnContextConnectFailEvent;

  end;

implementation

uses
  utils_safeLogger, diocp_winapi_winsock2, diocp_core_engine;

resourcestring
  strCannotConnect = '当前状态:%s, 不能进行连接...';
  strConnectError  = '建立连接(%s:%d)失败, 错误代码:%d';
  strConnectTimeOut= '建立连接(%s:%d)超时';


const
  // 重连间隔，避免连接过快，导致OnDisconnected还没有处理完成, 1秒
  RECONNECT_INTERVAL = 1000;


/// <summary>
///   计算两个TickCount时间差，避免超出49天后，溢出
///      感谢 [佛山]沧海一笑  7041779 提供
///      copy自 qsl代码 
/// </summary>
function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;

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
  CheckDestroyBindingHandle;
  FreeAndNil(FConnectExRequest);
  inherited Destroy;
end;

function TIocpRemoteContext.CanAutoReConnect: Boolean;
begin
  Result := FAutoReConnect and (Owner.Active) and (not TDiocpTcpClient(Owner).DisableAutoConnect);
end;

procedure TIocpRemoteContext.CheckCanConnect;
begin
   if SocketState <> ssDisconnected then raise Exception.Create(Format(strCannotConnect, [TSocketStateCaption[SocketState]]));
end;

procedure TIocpRemoteContext.CheckDestroyBindingHandle;
begin
// 会出现异常
//  if (FBindingHandle = 0) or (FBindingHandle = INVALID_SOCKET) then Exit;
//  CloseHandle(FBindingHandle);
//  FBindingHandle := 0;
end;

procedure TIocpRemoteContext.CheckDoReConnect;
begin

  if not (SocketState in [ssConnecting, ssConnected]) then
  begin
    if Owner.Active then
    begin
      AddDebugStrings('*(*)执行重连请求!');
      ConnectASync;
    end else
    begin
      AddDebugStrings('*(*)CheckDoReConnect::Check Owner is deactive!');
    end;
  end;
end;

procedure TIocpRemoteContext.Connect;
var
  lvRemoteIP:String;
begin
  if not Owner.Active then raise Exception.CreateFmt(strEngineIsOff, [Owner.Name]);

  if SocketState <> ssDisconnected then raise Exception.Create(Format(strCannotConnect, [TSocketStateCaption[SocketState]]));

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

procedure TIocpRemoteContext.Connect(pvTimeOut: Integer);
var
  lvRemoteIP:String;
begin
  if not Owner.Active then raise Exception.CreateFmt(strEngineIsOff, [Owner.Name]);

  if SocketState <> ssDisconnected then raise Exception.Create(Format(strCannotConnect, [TSocketStateCaption[SocketState]]));

  ReCreateSocket;

  try
    lvRemoteIP := RawSocket.GetIpAddrByName(FHost);
  except
    lvRemoteIP := FHost;
  end;

  

  if not RawSocket.ConnectTimeOut(lvRemoteIP, FPort, pvTimeOut) then
  begin
    raise Exception.Create(Format(strConnectTimeOut, [lvRemoteIP, FPort]));
  end;

  DoConnected;
  
end;

procedure TIocpRemoteContext.ConnectASync;
begin
  if (Owner <> nil) and (not Owner.Active) then raise Exception.CreateFmt(strEngineIsOff, [Owner.Name]);

  if SocketState <> ssDisconnected then raise Exception.Create(Format(strCannotConnect, [TSocketStateCaption[SocketState]]));

  ReCreateSocket;

  if not PostConnectRequest then
  begin
    DoConnectFail;
  end;
end;

procedure TIocpRemoteContext.DoConnectFail;
begin
  OnConnectFail;

  if Assigned(FOnConnectFailEvent) then
  begin
    FOnConnectFailEvent(Self);
  end;

  if (Owner <> nil) then
  begin
    if Assigned(TDiocpTcpClient(Owner).FOnContextConnectFailEvent)  then
    begin
      TDiocpTcpClient(Owner).FOnContextConnectFailEvent(Self);
    end;
  end;


  if (Owner <> nil) and (TDiocpTcpClient(Owner).TrigerDisconnectEventAfterNoneConnected) then
  begin
    DoNotifyDisconnected;
  end;

  // 状态一定要设定
  SetSocketState(ssDisconnected);
end;

procedure TIocpRemoteContext.OnConnected;
begin
  inherited;
  // 重置断开时间
  FLastDisconnectTime := 0;
end;

procedure TIocpRemoteContext.OnConnecteExResponse(pvObject: TObject);
begin
  try
    FIsConnecting := false;
    if TIocpConnectExRequest(pvObject).ErrorCode = 0 then
    begin
      DoConnected;
    end else
    begin
      {$IFDEF DEBUG_ON}
      Owner.logMessage(strConnectError,  [self.Host, self.Port,  TIocpConnectExRequest(pvObject).ErrorCode]);
      {$ENDIF}

      DoError(TIocpConnectExRequest(pvObject).ErrorCode);

      DoConnectFail;


    end;
  finally
    if Owner <> nil then Owner.DecRefCounter;
  end;
end;

procedure TIocpRemoteContext.OnConnectFail;
begin

end;

procedure TIocpRemoteContext.OnDisconnected;
begin
  inherited OnDisconnected;
end;

procedure TIocpRemoteContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: WORD);
begin
  inherited;

end;

function TIocpRemoteContext.PostConnectRequest: Boolean;
var
  lvPosted:Boolean;
begin
  lvPosted := false;
  Result := False;
  if FHost = '' then
  begin
    raise Exception.Create('请指定要建立连接的IP和端口信息！');
  end;

  if Owner <> nil then Owner.IncRefCounter;
  try
    if lock_cmp_exchange(False, True, FIsConnecting) = False then
    begin
      if RawSocket.SocketHandle = INVALID_SOCKET then
      begin
        ReCreateSocket;
      end;

      if not FConnectExRequest.PostRequest(FHost, FPort) then
      begin
        FIsConnecting := false;
      end else
      begin
        lvPosted := True;
        Result := True;
      end;
    end else
    begin
      Result := True;
      sfLogger.logMessage('TIocpRemoteContext.PostConnectRequest:: 正在进行连接...');
    end;
  finally
    if not lvPosted then
    begin
       if Owner <> nil then Owner.DecRefCounter;
    end;
  end;

end;

procedure TIocpRemoteContext.ReCreateSocket;
begin
  RawSocket.CreateTcpOverlappedSocket;
  if not RawSocket.bind('0.0.0.0', 0) then
  begin
    RaiseLastOSError;
  end;
  CheckDestroyBindingHandle;
  FBindingHandle := Owner.IocpEngine.IocpCore.Bind2IOCPHandle(RawSocket.SocketHandle, 0);
end;

procedure TIocpRemoteContext.SetSocketState(pvState: TSocketState);
begin
  inherited SetSocketState(pvState);
  if pvState = ssDisconnected then
  begin
    // 记录最后断开时间
    FLastDisconnectTime := GetTickCount;
  end;
end;

procedure TDiocpTcpClient.ClearContexts;
begin
  FListLocker.Enter;
  try
    FList.Clear;
  finally
    FListLocker.Leave;
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

  FListLocker := TCriticalSection.Create;

  FContextClass := TIocpRemoteContext;

  // 自动重连
  SetDisableAutoConnect(False);
end;

destructor TDiocpTcpClient.Destroy;
begin
  Close;
  FList.Clear;
  FList.Free;
  FListLocker.Free;
  inherited Destroy;
end;

procedure TDiocpTcpClient.DoAfterOpen;
begin
  inherited;

end;

procedure TDiocpTcpClient.DoAfterClose;
begin
  inherited;

end;

procedure TDiocpTcpClient.DoAutoReconnect(pvASyncWorker:TASyncWorker);
var
  i: Integer;
  lvContext:TIocpRemoteContext;
begin
  if not CheckOperaFlag(OPERA_SHUTDOWN_CONNECT) then
  begin
    FListLocker.Enter;
    try
      for i := 0 to FList.Count - 1 do
      begin
        if pvASyncWorker.Terminated then Break;

        lvContext := TIocpRemoteContext(FList[i]);
        if (lvContext.FAutoReConnect)
          and lvContext.CheckActivityTimeOut(10000)
          then
        begin
          lvContext.CheckDoReConnect;
        end;
      end;
    finally
      FListLocker.Leave;
    end;
  end;
end;

function TDiocpTcpClient.Add: TIocpRemoteContext;
begin
  FListLocker.Enter;
  try  
    if FContextClass = nil then
    begin
      RegisterContextClass(TIocpRemoteContext);
    end;
    Result := TIocpRemoteContext(CreateContext);
    FList.Add(Result);
  finally
    FListLocker.Leave;
  end;
end;

function TDiocpTcpClient.CheckContext(pvContext:TObject): TIocpRemoteContext;
begin
  if FList.IndexOf(TIocpRemoteContext(pvContext)) = -1 then
    Result := nil
  else
    Result := TIocpRemoteContext(pvContext);
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

function TDiocpTcpClient.GetStateInfo: String;
var
  lvStrings:TStrings;
begin
  Result := '';
  if DataMoniter = nil then Exit;

  lvStrings := TStringList.Create;
  try
    if Active then
    begin
      lvStrings.Add(strState_Active);
    end else
    begin
      lvStrings.Add(strState_Off);
    end;


    lvStrings.Add(Format(strRecv_PostInfo,
         [
           DataMoniter.PostWSARecvCounter,
           DataMoniter.ResponseWSARecvCounter,
           DataMoniter.PostWSARecvCounter -
           DataMoniter.ResponseWSARecvCounter,
           DataMoniter.Speed_WSARecvResponse
         ]
        ));


    lvStrings.Add(Format(strRecv_SizeInfo, [TransByteSize(DataMoniter.RecvSize)]));


    lvStrings.Add(Format(strSend_Info,
       [
         DataMoniter.PostWSASendCounter,
         DataMoniter.ResponseWSASendCounter,
         DataMoniter.PostWSASendCounter -
         DataMoniter.ResponseWSASendCounter,
         DataMoniter.Speed_WSASendResponse
       ]
      ));

    lvStrings.Add(Format(strSendRequest_Info,
       [
         DataMoniter.SendRequestCreateCounter,
         DataMoniter.SendRequestOutCounter,
         DataMoniter.SendRequestReturnCounter
       ]
      ));

    lvStrings.Add(Format(strSendQueue_Info,
       [
         DataMoniter.PushSendQueueCounter,
         DataMoniter.PostSendObjectCounter,
         DataMoniter.ResponseSendObjectCounter,
         DataMoniter.SendRequestAbortCounter
       ]
      ));



    lvStrings.Add(Format(strSend_SizeInfo, [TransByteSize(DataMoniter.SentSize)]));

     lvStrings.Add(Format(strContext_Info,
      [
        DataMoniter.ContextCreateCounter,
        DataMoniter.ContextOutCounter,
        DataMoniter.ContextReturnCounter
      ]
     ));

    lvStrings.Add(Format(strOnline_Info,   [OnlineContextCount, DataMoniter.MaxOnlineCount]));

    lvStrings.Add(Format(strWorkers_Info,  [WorkerCount]));

    lvStrings.Add(Format(strRunTime_Info,  [GetRunTimeINfo]));

    Result := lvStrings.Text;
  finally
    lvStrings.Free;
  end;
end;

procedure TDiocpTcpClient.OnCreateContext(const pvContext: TDiocpCustomContext);
begin
  inherited;
end;

procedure TDiocpTcpClient.DoASyncWork(pvFileWritter: TSingleFileWriter;
    pvASyncWorker: TASyncWorker);
begin
  if tick_diff(FAutoConnectTick, GetTickCount) > 5000 then
  begin
    if not self.DisableAutoConnect then
    begin
      DoAutoReconnect(pvASyncWorker);
    end;
    DoASyncCycle(pvASyncWorker);
    FAutoConnectTick := GetTickCount;
  end;
end;

procedure TDiocpTcpClient.DoASyncCycle(pvASyncWorker:TASyncWorker);
var
  i: Integer;
  lvContext:TIocpRemoteContext;
begin
  FListLocker.Enter;
  try
    for i := 0 to FList.Count - 1 do
    begin
      if pvASyncWorker.Terminated then Break;

      lvContext := TIocpRemoteContext(FList[i]);
      if Assigned(lvContext.FOnASyncCycle) then
      begin
        lvContext.FOnASyncCycle(lvContext);
      end;
    end;
  finally
    FListLocker.Leave;
  end;
end;

procedure TDiocpTcpClient.RemoveAllContext;
begin
  IncOperaOptions(OPERA_SHUTDOWN_CONNECT);
  try
    DisconnectAll;
    WaitForContext(30000);
    ClearContexts();
  finally
    DecOperaOptions(OPERA_SHUTDOWN_CONNECT);
  end;
end;

procedure TDiocpTcpClient.SetDisableAutoConnect(const Value: Boolean);
begin
  if Value <> FDisableAutoConnect then
  begin
    FDisableAutoConnect := Value;
  end;
end;

procedure TDiocpTcpClient.SetTrigerDisconnectEventAfterNoneConnected(const
    Value: Boolean);
begin
  FTrigerDisconnectEventAfterNoneConnected := Value;
end;


end.
