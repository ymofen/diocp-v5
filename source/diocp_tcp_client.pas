(*
 *	 Unit owner: d10.�����
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 ����
 *
 *   1. �޸�ex.tcpclient�������⣬���ʹ�����ʱ���޷������bug
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
  ///   �����getFromPool���ܽ�Server��List�б�
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

    // ��ֹ����ʱ��
    FBlockStartTick:Cardinal;
    FBlockTime:Cardinal;

    FHost: String;
    FPort: Integer;
    /// <summary>TIocpRemoteContext.PostConnectRequest
    /// </summary>
    /// <returns>
    ///   Ͷ�ݳɹ������������ӷ���true
    /// </returns>
    function PostConnectRequest: Boolean;
    procedure ReCreateSocket;

    function CanAutoReConnect:Boolean;

    procedure CheckDestroyBindingHandle;

    procedure DoConnectFail;
  protected

    procedure DoBeforeReconnect(var vAllowReconnect: Boolean); virtual;
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
    ///   �����������ӣ�
    ///   �������Ҫ��
    ///     1. �Ѿ�����
    ///   ���������
    /// </summary>
    procedure CheckDoReConnect;

    constructor Create; override;
    destructor Destroy; override;
    procedure BlockReconnectTime(pvMSecs:Cardinal);
    /// <summary>
    ///  ������ʽ��������
    ///    ����״̬�仯: ssDisconnected -> ssConnected/ssDisconnected
    /// </summary>
    procedure Connect; overload;

    procedure Connect(pvTimeOut:Integer); overload;

    /// <summary>
    ///  �����첽����
    ///    ����״̬�仯: ssDisconnected -> ssConnecting -> ssConnected/ssDisconnected
    ///    ���Ͷ��ʧ�ܣ���������ʧ�ܣ����Owner.TrigerDisconnectEventAfterNoneConnectedΪtrue����OnDisconnected
    /// </summary>
    procedure ConnectASync;

    /// <summary>
    ///   ���ø����Ӷ�����Զ���������
    ///    true�������Զ�����
    /// </summary>
    property AutoReConnect: Boolean read FAutoReConnect write FAutoReConnect;

    /// <summary>
    ///   ��ASync�߳�, ѭ��ִ��
    /// </summary>
    property OnASyncCycle: TNotifyContextEvent read FOnASyncCycle write FOnASyncCycle;

    /// <summary>
    ///   ����ʧ���¼�
    /// </summary>
    property OnConnectFailEvent: TNotifyContextEvent read FOnConnectFailEvent write
        FOnConnectFailEvent;

    property Host: String read FHost write FHost;



    property Port: Integer read FPort write FPort;



  end;

  /// <summary>
  ///   ע��
  ///   Add, ClearContexts, ���б����д�룬û�ж��б�����̰߳�ȫ����
  ///   Find, CheckContext��Items����Ҳû�ж��б��������
  ///   ���ԣ�����ڿ�ʼ֮ǰ���б���д����б�. ��ֹͣ����б����ClearContexts
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
    ///  ���ʹ���������� ,���߳�ʹ�ã�����DoAutoReconnect����
    ///    �������5������
    /// </summary>
    procedure DoAutoReconnect(pvFileWritter: TSingleFileWriter;
        pvASyncWorker:TASyncWorker);

    /// <summary>
    ///    ���ʹ���������� ,���߳�ʹ�ã�����DoASyncCycle����
    ///    �������5������
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
    ///   ����Add��������������
    /// </summary>
    procedure ClearContexts;


    /// <summary>
    ///   ����Ͽ��������ӣ������̷��ء�
    /// </summary>
    procedure DisconnectAll;  override;

    /// <summary>
    ///   ���һ��������(��ӵ�һ��List�У�DiocpTcpClient�����ͷ�ʱ���������ͷ�)
    ///   �����ӣ������ظ�ʹ��
    /// </summary>
    function Add: TIocpRemoteContext;

    /// <summary>
    ///   �Ƴ�һ������,�������ͷ�
    /// </summary>
    /// <returns>
    ///  true: �Ƴ��ɹ������ͷ�
    ///  false: ���ǵ�ǰ������ӵ�����
    /// </returns>
    function RemoveAndFree(aCtx:TIocpRemoteContext): Boolean;

    /// <summary>
    ///   pvContext�Ƿ��ǵ�ǰ�б��еĶ���
    ///   nil:����
    /// </summary>
    function CheckContext(pvContext:TObject): TIocpRemoteContext;

    
    function GetStateInfo: String;




    /// <summary>
    ///  ��ֹ����
    ///    ����Ͽ�����
    ///    �ȴ��Ͽ�����(30000)
    ///    ��������
    /// </summary>
    procedure RemoveAllContext;

    /// <summary>
    ///   �ܵ����Ӷ�������
    /// </summary>
    property Count: Integer read GetCount;

    /// <summary>
    ///   ��ֹ�������Ӷ����Զ�����
    ///   Ĭ���ǲ���ֹ
    /// </summary>
    property DisableAutoConnect: Boolean read FDisableAutoConnect write
        SetDisableAutoConnect;

    /// <summary>
    ///    ͨ��λ��������ȡ���е�һ������
    /// </summary>
    property Items[pvIndex: Integer]: TIocpRemoteContext read GetItems; default;

    /// <summary>
    ///  Ϊtrueʱ: ��ʹ����ʧ�ܵ�����£�����OnDisconnected�¼�, Ĭ��Ϊtrue
    /// </summary>
    property TrigerDisconnectEventAfterNoneConnected: Boolean read
        FTrigerDisconnectEventAfterNoneConnected write
        SetTrigerDisconnectEventAfterNoneConnected;


    /// <summary>
    ///   ����ʧ��
    /// </summary>
    property OnContextConnectFailEvent: TNotifyContextEvent read FOnContextConnectFailEvent
        write FOnContextConnectFailEvent;

  end;

implementation

uses
  utils_safeLogger, diocp_winapi_winsock2, diocp_core_engine;

resourcestring
  strCannotConnect = '��ǰ״̬:%s, ���ܽ�������...';
  strConnectError  = '��������(%s:%d)ʧ��, �������:%d';
  strConnectTimeOut= '��������(%s:%d)��ʱ';


const
  // ����������������ӹ��죬����OnDisconnected��û�д������, 1��
  RECONNECT_INTERVAL = 1000;


/// <summary>
///   ��������TickCountʱ�����ⳬ��49������
///      ��л [��ɽ]�׺�һЦ  7041779 �ṩ
///      copy�� qsl���� 
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

procedure TIocpRemoteContext.BlockReconnectTime(pvMSecs:Cardinal);
begin
  FBlockTime := pvMSecs;
  FBlockStartTick := GetTickCount;
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
// ������쳣
//  if (FBindingHandle = 0) or (FBindingHandle = INVALID_SOCKET) then Exit;
//  CloseHandle(FBindingHandle);
//  FBindingHandle := 0;
end;

procedure TIocpRemoteContext.CheckDoReConnect;
begin     
  if self.CtxStateFlag = CTX_STATE_INITIAL then  
  begin
    if Owner.Active then
    begin
      {$IFDEF DIOCP_DEBUG}
      AddDebugStrings('*(*)ִ����������!');
      {$ENDIF}
      ConnectASync;
    end else
    begin
      {$IFDEF DIOCP_DEBUG}
      AddDebugStrings('*(*)CheckDoReConnect::Check Owner is deactive!');
      {$ENDIF}
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
  if (Owner <> nil) then
  begin
    // ��ֹ����
    if Owner.DisableConnectFlag = 1 then Exit;
  end;

  if SocketState <> ssDisconnected then raise Exception.Create(Format(strCannotConnect, [TSocketStateCaption[SocketState]]));

  ReCreateSocket;

  DoSetCtxState(CTX_STATE_CONNECTING);
  if not PostConnectRequest then
  begin
    DoConnectFail;

    // ����ʧ��, ����Ƿ���Ҫ�ع鵽��
    self.CheckReleaseBack;
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

  // ״̬һ��Ҫ�趨
  SetSocketState(ssDisconnected);
  DoSetCtxState(CTX_STATE_INITIAL);
end;

procedure TIocpRemoteContext.DoBeforeReconnect(var vAllowReconnect: Boolean);
begin
  vAllowReconnect := (FAutoReConnect) and CheckActivityTimeOut(10000);
  if vAllowReconnect then
  begin
    if (FBlockStartTick > 0) and (FBlockTime > 0) then
    begin
      if tick_diff(FBlockStartTick, GetTickCount) < FBlockTime then
      begin         // ��ֹ����
        vAllowReconnect := False;
      end else
      begin
        FBlockStartTick := 0;
        FBlockTime := 0;
      end;
    end;

  end;
end;

procedure TIocpRemoteContext.OnConnected;
begin
  inherited;
  // ���öϿ�ʱ��
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
      {$IFDEF DIOCP_DEBUG}
      Owner.logMessage(strConnectError,  [self.Host, self.Port,  TIocpConnectExRequest(pvObject).ErrorCode]);
      {$ENDIF}

      DoError(TIocpConnectExRequest(pvObject).ErrorCode);

      DoConnectFail;
      self.CheckReleaseBack;
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
    raise Exception.Create('��ָ��Ҫ�������ӵ�IP�Ͷ˿���Ϣ��');
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
      sfLogger.logMessage('TIocpRemoteContext.PostConnectRequest:: ���ڽ�������...');
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
    // ��¼���Ͽ�ʱ��
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

  // �Զ�����
  SetDisableAutoConnect(False);
end;

destructor TDiocpTcpClient.Destroy;
begin
  Close;
  FList.Clear;
  FList.Free;
  FListLocker.Free;
  FListLocker := nil;
  inherited Destroy;
end;

procedure TDiocpTcpClient.DisconnectAll;
var
  i: Integer;
  lvContext:TIocpRemoteContext;
  vAllow:Boolean;
begin
  inherited DisconnectAll;
  if FListLocker = nil then Exit;  
  FListLocker.Enter;
  try
    for i := 0 to FList.Count - 1 do
    begin
      lvContext := TIocpRemoteContext(FList[i]);
      lvContext.RequestDisconnect('��������Ͽ���������');
    end;
  finally
    FListLocker.Leave;
  end;
end;

procedure TDiocpTcpClient.DoAfterOpen;
begin
  inherited;

end;

procedure TDiocpTcpClient.DoAfterClose;
begin
  inherited;

end;

procedure TDiocpTcpClient.DoAutoReconnect(pvFileWritter: TSingleFileWriter;
    pvASyncWorker:TASyncWorker);
var
  i: Integer;
  lvContext:TIocpRemoteContext;
  vAllow:Boolean;
begin
  if not CheckOperaFlag(OPERA_SHUTDOWN_CONNECT) then
  begin
    if self.DisableConnectFlag = 1 then Exit;
    
    FListLocker.Enter;
    try
      for i := 0 to FList.Count - 1 do
      begin
        if pvASyncWorker.Terminated then Break;
        try
          lvContext := TIocpRemoteContext(FList[i]);
          lvContext.DoBeforeReconnect(vAllow);
          if vAllow then
          begin
            lvContext.CheckDoReConnect;
          end;
        except
          on e:Exception do
          begin
            {$IFDEF DEBUG}
            pvFileWritter.LogMessage('DoAutoReconnect[%d]�������쳣:%s', [i, e.Message]);
            {$ENDIF}
          end;
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
    FAutoConnectTick := GetTickCount;
    if not self.DisableAutoConnect then
    begin
      DoAutoReconnect(pvFileWritter, pvASyncWorker);
    end;
    DoASyncCycle(pvASyncWorker);
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
    FDisableConnectFlag := 1;
    try
      DisconnectAll;
      WaitForContext(30000);
      ClearContexts();
    finally
      FDisableConnectFlag := 0;
    end;
  finally
    DecOperaOptions(OPERA_SHUTDOWN_CONNECT);
  end;
end;

function TDiocpTcpClient.RemoveAndFree(aCtx:TIocpRemoteContext): Boolean;
begin
  FListLocker.Enter;
  try
    Result := FList.Remove(aCtx) <> -1;
  finally
    FListLocker.Leave;
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
