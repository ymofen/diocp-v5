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
  , utils_async
  , utils_fileWriter
  , utils_threadinfo
  , utils_buffer;

type
  TIocpRemoteContext = class(TDiocpCustomContext)
  private
    FLastDisconnectTime:Cardinal;
    FIsConnecting: Boolean;
    FBindingHandle:THandle;

    FAutoReConnect: Boolean;
    FConnectExRequest: TIocpConnectExRequest;

    FHost: String;
    FPort: Integer;
    function PostConnectRequest: Boolean;
    procedure ReCreateSocket;
    function CanAutoReConnect:Boolean;
    procedure CheckDestroyBindingHandle;
  protected
    procedure OnConnecteExResponse(pvObject:TObject);

    procedure OnDisconnected; override;

    procedure OnConnected; override;

    procedure SetSocketState(pvState:TSocketState); override;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;

  public
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

  TDiocpExRemoteContext = class(TIocpRemoteContext)
  private
    FOnBufferAction: TOnContextBufferNotifyEvent;
  protected
    FCacheBuffer: TBufferLink;
    FEndBuffer: array [0..254] of Byte;
    FEndBufferLen: Byte;
    FStartBuffer: array [0..254] of Byte;
    FStartBufferLen: Byte;

    procedure DoCleanUp; override;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetEnd(pvBuffer:Pointer; pvBufferLen:Byte);
    procedure SetStart(pvBuffer:Pointer; pvBufferLen:Byte);
    property OnBufferAction: TOnContextBufferNotifyEvent read FOnBufferAction write FOnBufferAction;
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
    FASyncInvoker:TASyncInvoker;
    FDisableAutoConnect: Boolean;

  private
    /// <summary>
    ///  检测使用重新连接 ,单线程使用，仅供DoAutoReconnect调用
    /// </summary>
    procedure DoAutoReconnect(pvASyncWorker:TASyncWorker);
    procedure OnASyncWork(pvASyncWorker:TASyncWorker);
    procedure SetDisableAutoConnect(const Value: Boolean);
  private
  {$IFDEF UNICODE}
    FList: TObjectList<TIocpRemoteContext>;
  {$ELSE}
    FList: TObjectList;
  {$ENDIF}
  protected
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
    ///   总的连接对象数量
    /// </summary>
    property Count: Integer read GetCount;

    /// <summary>
    ///   禁止所有连接对象自动重连
    /// </summary>
    property DisableAutoConnect: Boolean read FDisableAutoConnect write
        SetDisableAutoConnect;

    /// <summary>
    ///    通过位置索引获取其中的一个连接
    /// </summary>
    property Items[pvIndex: Integer]: TIocpRemoteContext read GetItems; default;

  end;

implementation

uses
  utils_safeLogger, diocp_winapi_winsock2, diocp_core_engine;

resourcestring
  strCannotConnect = '当前状态下不能进行连接...';
  strConnectError  = '建立连接失败, 错误代码:%d';
  strConnectTimeOut= '建立连接超时';


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

procedure TIocpRemoteContext.CheckDestroyBindingHandle;
begin
// 会出现异常
//  if (FBindingHandle = 0) or (FBindingHandle = INVALID_SOCKET) then Exit;
//  CloseHandle(FBindingHandle);
//  FBindingHandle := 0;
end;

procedure TIocpRemoteContext.Connect;
var
  lvRemoteIP:String;
begin
  if not Owner.Active then raise Exception.CreateFmt(strEngineIsOff, [Owner.Name]);

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

procedure TIocpRemoteContext.Connect(pvTimeOut: Integer);
var
  lvRemoteIP:String;
begin
  if not Owner.Active then raise Exception.CreateFmt(strEngineIsOff, [Owner.Name]);

  if SocketState <> ssDisconnected then raise Exception.Create(strCannotConnect);

  ReCreateSocket;

  try
    lvRemoteIP := RawSocket.GetIpAddrByName(FHost);
  except
    lvRemoteIP := FHost;
  end;

  

  if not RawSocket.ConnectTimeOut(lvRemoteIP, FPort, pvTimeOut) then
  begin
    raise Exception.Create(strConnectTimeOut);
  end;

  DoConnected;
  
end;

procedure TIocpRemoteContext.ConnectASync;
begin
  if not Owner.Active then raise Exception.CreateFmt(strEngineIsOff, [Owner.Name]);

  if SocketState <> ssDisconnected then raise Exception.Create(strCannotConnect);

  ReCreateSocket;

  PostConnectRequest;

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
      Owner.logMessage(strConnectError,  [TIocpConnectExRequest(pvObject).ErrorCode]);
      {$ENDIF}

      DoError(TIocpConnectExRequest(pvObject).ErrorCode);

      SetSocketState(ssDisconnected);
    end;
  finally
    if Owner <> nil then Owner.DecRefCounter;
  end;
end;

procedure TIocpRemoteContext.OnDisconnected;
begin
  inherited;
end;

procedure TIocpRemoteContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: WORD);
begin
  inherited;

end;

function TIocpRemoteContext.PostConnectRequest: Boolean;
begin
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
        Result := True;
      end;
    end else
    begin
      sfLogger.logMessage('TIocpRemoteContext.PostConnectRequest:: 正在进行连接...');
    end;
  finally
    if not Result then
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
  FList.Clear;
end;

constructor TDiocpTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF UNICODE}
  FList := TObjectList<TIocpRemoteContext>.Create();
{$ELSE}
  FList := TObjectList.Create();
{$ENDIF}
  FASyncInvoker := TASyncInvoker.Create;

  DisableAutoConnect := False;
end;

destructor TDiocpTcpClient.Destroy;
begin
  FASyncInvoker.Terminate;
  FASyncInvoker.WaitForStop;
  Close;
  FList.Clear;
  FList.Free;
  FASyncInvoker.Free;
  inherited Destroy;
end;

procedure TDiocpTcpClient.DoAutoReconnect(pvASyncWorker:TASyncWorker);
var
  i: Integer;
  lvContext:TIocpRemoteContext;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if pvASyncWorker.Terminated then Break;

    lvContext := TIocpRemoteContext(FList[i]);
    if not (lvContext.SocketState in [ssConnecting, ssConnected]) then
    begin
      if lvContext.CanAutoReConnect then
      begin
        lvContext.AddDebugStrings('*(*)执行重连请求!');
        lvContext.ConnectASync;
      end else
      begin
        lvContext.AddDebugStrings('*(*)重连时:Check CanAutoReConnect is false!');
      end;
    end else
    begin
      if lvContext.CheckActivityTimeOut(10000) then
      begin
        ;// 应当进行重连
      
      end;
    end;
  end;
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

    lvStrings.Add(Format(strOnline_Info,   [OnlineContextCount, DataMoniter.MaxOnlineCount]));

    lvStrings.Add(Format(strWorkers_Info,  [WorkerCount]));

    lvStrings.Add(Format(strRunTime_Info,  [GetRunTimeINfo]));

    Result := lvStrings.Text;
  finally
    lvStrings.Free;
  end;
end;

procedure TDiocpTcpClient.OnASyncWork(pvASyncWorker: TASyncWorker);
var
  lvFileWriter: TSingleFileWriter;  
begin
  lvFileWriter := TSingleFileWriter.Create;
  try
    lvFileWriter.FilePreFix := self.Name + '_ASync_';
    {$IFDEF DEBUG}
    lvFileWriter.LogMessage('启动时间:' + FormatDateTime('yyyy-mm-dd:HH:nn:ss.zzz', Now));
    {$ENDIF}
    while not pvASyncWorker.Terminated do
    begin
      try
        SetCurrentThreadInfo('开始进行重连');
        DoAutoReconnect(pvASyncWorker);
        SetCurrentThreadInfo('结束重连过程');
        Sleep(2000);
      except
        on e:Exception do
        begin
          lvFileWriter.LogMessage('ERR:' + e.Message);
        end;  
      end;
    end;
  finally
    {$IFDEF DEBUG}
    lvFileWriter.LogMessage('停止时间:' + FormatDateTime('yyyy-mm-dd:HH:nn:ss.zzz', Now));
    {$ENDIF}
    lvFileWriter.Flush;    
    lvFileWriter.Free;
  end;
end;

procedure TDiocpTcpClient.SetDisableAutoConnect(const Value: Boolean);
begin
  if Value <> FDisableAutoConnect then
  begin
    FDisableAutoConnect := Value;
    if FDisableAutoConnect then
    begin
      FASyncInvoker.Terminate;
      FASyncInvoker.WaitForStop;
    end;
  end;

  if not FDisableAutoConnect then
  begin
    if FASyncInvoker.Terminated then
      FASyncInvoker.Start(OnASyncWork);
  end;
end;


constructor TDiocpExRemoteContext.Create;
begin
  inherited Create;
  FCacheBuffer := TBufferLink.Create();
end;

destructor TDiocpExRemoteContext.Destroy;
begin
  FreeAndNil(FCacheBuffer);
  inherited Destroy;
end;

procedure TDiocpExRemoteContext.DoCleanUp;
begin
  inherited;
  FCacheBuffer.clearBuffer;
end;

procedure TDiocpExRemoteContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
    ErrCode: WORD);
var
  j:Integer;
  lvBuffer:array of byte;
begin
  FCacheBuffer.AddBuffer(buf, len);
  while FCacheBuffer.validCount > 0 do
  begin
    // 标记读取的开始位置，如果数据不够，进行恢复，以便下一次解码
    FCacheBuffer.markReaderIndex;
    
    if FStartBufferLen > 0 then
    begin
      // 不够数据，跳出
      if FCacheBuffer.validCount < FStartBufferLen + FEndBufferLen then Break;
      
      j := FCacheBuffer.SearchBuffer(@FStartBuffer[0], FStartBufferLen);
      if j = -1 then
      begin  // 没有搜索到开始标志
        FCacheBuffer.clearBuffer();
        Exit;
      end else
      begin
        FCacheBuffer.restoreReaderIndex;

        // 跳过开头标志
        FCacheBuffer.Skip(j + FStartBufferLen);
      end;
    end;

    // 不够数据，跳出
    if FCacheBuffer.validCount < FEndBufferLen then Break;
    
    j := FCacheBuffer.SearchBuffer(@FEndBuffer[0], FEndBufferLen);
    if j <> -1 then
    begin
      SetLength(lvBuffer, j);
      FCacheBuffer.readBuffer(@lvBuffer[0], j);
      if Assigned(FOnBufferAction) then
      begin
        FOnBufferAction(Self, @lvBuffer[0], j);
      end;
      FCacheBuffer.Skip(FEndBufferLen);
    end else
    begin      // 没有结束符
      FCacheBuffer.restoreReaderIndex;
      Break;
    end;
  end;                               
  FCacheBuffer.clearHaveReadBuffer();
end;

procedure TDiocpExRemoteContext.SetEnd(pvBuffer:Pointer; pvBufferLen:Byte);
begin
  Move(pvBuffer^, FEndBuffer[0], pvBufferLen);
  FEndBufferLen := pvBufferLen;
end;

procedure TDiocpExRemoteContext.SetStart(pvBuffer:Pointer; pvBufferLen:Byte);
begin
  Move(pvBuffer^, FStartBuffer[0], pvBufferLen);
  FStartBufferLen := pvBufferLen;
end;

end.
