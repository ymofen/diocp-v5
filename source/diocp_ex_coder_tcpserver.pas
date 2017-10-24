(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
  *   2015-04-08 12:34:33
  *    (感谢 suoler反馈bug和提供bug重现)
  *    异步处理逻辑请求后OnContextAction
  *      当连接已经关闭，但是请求还没有来得及处理，然后连接上下文已经归还到池，这个时候应该放弃处理任务()
 *)
unit diocp_ex_coder_tcpserver;

interface

/// 三个编译开关，只能开启一个
{.$DEFINE INNER_IOCP_PROCESSOR}     // iocp线程触发事件
{.$DEFINE QDAC_QWorker}   // 用qworker进行调度触发事件
{$DEFINE DIOCP_Task}     // 用diocp_task进行调度触发事件

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

uses
  diocp_tcp_server, utils_buffer, SysUtils, Classes,
  diocp_coder_baseObject, utils_queues, utils_locker, utils_BufferPool
  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp_task{$ENDIF}
  ;

const
  BLOCK_BUFFER_TAG = 10000;

type
  TDiocpCoderTcpServer = class;

  TDiocpCoderSendRequest = class(TIocpSendRequest)
  private
    FMemBlock:PMemoryBlock;
    FBlockMem: Boolean;
  protected
    procedure ResponseDone; override;
    procedure CancelRequest;override;
  end;

  /// <summary>
  ///   请求任务对象, 用于处理异步任务时，可以对比任务时的信息，用于可以进行取消任务
  /// </summary>
  TDiocpTaskObject = class(TObject)
  private
    FOwner:TDiocpCoderTcpServer;
    /// <summary>
    ///   投递异步之前记录DNA，用于做异步任务时，是否取消当前任务
    /// </summary>
    FContextDNA:Integer;
    // 解码对象
    FData: TObject;
  public
    /// <summary>
    ///   归还到对象池
    /// </summary>
    procedure Close;
  end;

  TIOCPCoderClientContext = class(diocp_tcp_server.TIOCPClientContext)
  private
    FCoderExchange:TDiocpContextCoderExchange;
    
    // 发送写入单线程写入
    FBlockBuffer: TBlockBuffer;

    /// 是否正在处理请求
    FIsProcessRequesting:Boolean;
    
    /// 请求的任务队列
    FRequestQueue:TSimpleQueue;
    

    FStateINfo: String;
    FSendBlock: PMemoryBlock; //不得闲
    function GetStateINfo: String;

    /// <summary>
    ///  执行一次请求
    /// </summary>
    function DoExecuteRequest(pvTaskObj: TDiocpTaskObject): HRESULT;

    /// <summary>
    ///   清理请求列表中的对象
    /// </summary>
    procedure ClearRequestTaskObject();

    {$IFDEF QDAC_QWorker}
    procedure OnExecuteJob(pvJob:PQJob);
    {$ENDIF}
    {$IFDEF DIOCP_Task}
    procedure OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
    {$ENDIF}
    procedure DoInnerJob(pvTaskObject: TDiocpTaskObject);
    procedure DoSendBufferCompleted(pvBuffer: Pointer; len: Cardinal; pvBufferTag:
        Integer; pvTagData: Pointer; pvErrorCode: Integer); override;
    procedure OnBlockBufferWrite(pvSender: TObject; pvBuffer: Pointer; pvLength:
        Integer);
  protected
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
    
    procedure RecvBuffer(buf:PAnsiChar; len:Cardinal); virtual;

    procedure DoCleanUp;override;
  protected
  public
    constructor Create;override;

    destructor Destroy; override;

    /// <summary>
    ///   接收到一个完整的数据包
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure DoContextAction(const pvDataObject:TObject); virtual;

    /// <summary>
    ///   回写对象(发送对象会客户端, 会调用解码器进行解码)
    /// </summary>
    /// <param name="pvDataObject"> 要回写的对象 </param>
    procedure WriteObject(const pvDataObject:TObject);


    /// <summary>
    ///   一些状态信息
    /// </summary>
    property StateINfo: String read GetStateINfo write FStateINfo;
  end;



  TOnContextAction = procedure(pvClientContext:TIOCPCoderClientContext;
      pvObject:TObject) of object;

  {$IF RTLVersion>22}
  // thanks: 麦子仲肥19183455
  //  vcl for win64
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TDiocpCoderTcpServer = class(TDiocpTcpServer)
  private
    ///异步任务投递对象池
    FTaskObjectPool: TBaseQueue;

    FCoderExchangeClass:TDiocpContextCoderExchangeClass;

    FInnerEncoder: TDiocpEncoder;
    FInnerDecoder: TDiocpDecoder;
    FUseMaxSendBlock: Boolean;
    
    // 内存池
    // 目前用与发送
    FBlockBufferPool: PBufferPool;

  protected
    FEncoder: TDiocpEncoder;
    FDecoder: TDiocpDecoder;
    FLogicWorkerNeedCoInitialize: Boolean;
    FOnContextAction: TOnContextAction;

    procedure DoAfterOpen; override;
    function GetTaskObject:TDiocpTaskObject;
    procedure GiveBackTaskObject(pvObj:TDiocpTaskObject);

    function CreateCoderExchange: TDiocpContextCoderExchange;

    /// <summary>
    ///   当创建新的连接对象时会调用的函数
    ///   可以在这里面做一些初始化
    /// </summary>
    procedure OnCreateClientContext(const context: TIocpClientContext); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   注册编码器和解码器类
    /// </summary>
    procedure RegisterCoderClass(pvDecoderClass:TDiocpDecoderClass;
        pvEncoderClass:TDiocpEncoderClass);

    procedure RegisterCoderExchangeClass(const pvCoderExchangeClass:
        TDiocpContextCoderExchangeClass);

    /// <summary>
    ///   register Decoder instance
    /// </summary>
    /// <param name="pvDecoder"> (TIOCPDecoder) </param>
    procedure RegisterDecoder(pvDecoder: TDiocpDecoder);

    /// <summary>
    ///   register Encoder instance
    /// </summary>
    /// <param name="pvEncoder"> (TIOCPEncoder) </param>
    procedure RegisterEncoder(pvEncoder: TDiocpEncoder);

  published

    /// <summary>
    ///   处理逻辑线程执行逻辑前执行CoInitalize
    /// </summary>
    property LogicWorkerNeedCoInitialize: Boolean read FLogicWorkerNeedCoInitialize write FLogicWorkerNeedCoInitialize;

    /// <summary>
    ///   收到一个完整的数据包的执行事件(在IocpTask/Qworker线程中触发)
    /// </summary>
    property OnContextAction: TOnContextAction read FOnContextAction write FOnContextAction;

    /// <summary>
    ///  不得闲，增加一个是否使用大数据块发送模式，如果设置为True，那么就是在发送的时候，每次发送按照
    ///  TCP的协议包进行组合，一般默认最大发送的是64K
    ///  TCP 包的大小就应该是 1500 - IP头(20) - TCP头(20) = 1460 (BYTES)，也就相当于1460*44的块进行发送一个包
    ///   收到一个完整的数据包的执行事件(在IocpTask/Qworker线程中触发)
    /// </summary>
    property UseMaxSendBlock: Boolean read FUseMaxSendBlock write FUseMaxSendBlock;
  end;



implementation

uses
  utils_safeLogger;

{$IFDEF DIOCP_DEBUG}
var
  __debug_tag:Integer;
{$ENDIF}


constructor TIOCPCoderClientContext.Create;
begin
  inherited Create;
  FRequestQueue := TSimpleQueue.Create();
  
  FBlockBuffer := TBlockBuffer.Create(nil);
  FBlockBuffer.OnBufferWrite := OnBlockBufferWrite;
end;

destructor TIOCPCoderClientContext.Destroy;
begin

  // 清理待处理请求队列
  ClearRequestTaskObject();

  FRequestQueue.Free;

  if FCoderExchange <> nil then
  begin
    FCoderExchange.Free;
  end;
  FBlockBuffer.Free;

  inherited Destroy;
end;

procedure TIOCPCoderClientContext.DoCleanUp;
begin
  // 清理待处理请求队列
  ClearRequestTaskObject;

  // 正在处理
  FIsProcessRequesting := False;                   
  inherited;
end;

procedure TIOCPCoderClientContext.ClearRequestTaskObject;
var
  lvTask:TDiocpTaskObject;
  lvObj:TObject;
begin
  self.Lock;
  try
    while True do
    begin
      lvTask := TDiocpTaskObject(FRequestQueue.DeQueue);
      if lvTask = nil then Break;

      lvObj := lvTask.FData;
      
      // 归还到任务池
      lvTask.Close;
      try
        // 释放解码对象
        if lvObj <> nil then FreeAndNil(lvObj);
      except
      end; 
    end;
  finally
    self.UnLock;
  end;  
end;

procedure TIOCPCoderClientContext.DoContextAction(const pvDataObject:TObject);
begin

end;

function TIOCPCoderClientContext.DoExecuteRequest(pvTaskObj: TDiocpTaskObject):
    HRESULT;
var
  lvObj:TObject;
begin
  Result := S_FALSE;
  lvObj := pvTaskObj.FData;
  // 连接已经断开
  if Owner = nil then Exit;

  // 连接已经释放
  if Self = nil then Exit;

  // 已经不是当初投递的连接
  if self.ContextDNA <> pvTaskObj.FContextDNA then Exit;



  self.CheckThreadIn(STRING_EMPTY);
  try
    try
      // 执行Owner的事件
      if Assigned(TDiocpCoderTcpServer(Owner).FOnContextAction) then
        TDiocpCoderTcpServer(Owner).FOnContextAction(Self, lvObj);
      DoContextAction(lvObj);
    except
     on E:Exception do
      begin
        FOwner.LogMessage('截获处理逻辑异常:' + e.Message);
      end;
    end;
    Result := S_OK;
  finally
    self.CheckThreadOut;
  end;


end;

procedure TIOCPCoderClientContext.DoInnerJob(pvTaskObject: TDiocpTaskObject);
var
  lvObj:TObject;
begin
  lvObj := pvTaskObject.FData;
  try
    try
      // 执行任务
      if DoExecuteRequest(pvTaskObject) <> S_OK then
      begin
        // 执行失败             
      end;
    except
      on E:Exception do
      begin
        Self.Owner.LogMessage('1-DoInnerJob Err:%s', [e.Message], CORE_LOG_FILE);
      end;
    end;
  finally
    try
      // 归还到任务池
      pvTaskObject.Close;
      // 释放解码对象
      if lvObj <> nil then
      begin
        TDiocpCoderTcpServer(Owner).FDecoder.ReleaseData(FCoderExchange, lvObj, True);
      end;
    except
      on E:Exception do
      begin
        Self.Owner.LogMessage('2-DoInnerJob finally:%s', [e.Message], CORE_LOG_FILE);
      end;
    end;
  end;

end;

procedure TIOCPCoderClientContext.DoSendBufferCompleted(pvBuffer: Pointer; len:
    Cardinal; pvBufferTag: Integer; pvTagData: Pointer; pvErrorCode: Integer);
{$IFDEF DIOCP_DEBUG}
var
  r:Integer;
{$ENDIF}
begin
  inherited;
  {$IFDEF DIOCP_DEBUG}
  if pvBufferTag >= BLOCK_BUFFER_TAG then
  begin
    r := ReleaseRef(pvBuffer, Format('- DoSendBufferCompleted(%d)', [pvBufferTag]));
    PrintDebugString(Format('- %x: %d', [IntPtr(pvBuffer), r]));
  end;
  {$ELSE}
  if pvBufferTag >= BLOCK_BUFFER_TAG then
  begin
    ReleaseRef(pvBuffer);
  end;
  {$ENDIF}
end;

function TIOCPCoderClientContext.GetStateINfo: String;
begin
  Result := FStateINfo;
end;

procedure TIOCPCoderClientContext.OnBlockBufferWrite(pvSender: TObject;
    pvBuffer: Pointer; pvLength: Integer);
{$IFDEF DIOCP_DEBUG}
var
  r , n:Integer;
{$ENDIF}
begin
  if not Self.Active then Exit;
  {$IFDEF DIOCP_DEBUG}
  n := BLOCK_BUFFER_TAG + AtomicIncrement(__debug_tag);
  if n < BLOCK_BUFFER_TAG then
  begin
    __debug_tag := 0;
    n := BLOCK_BUFFER_TAG + 1;
  end;
  r := AddRef(pvBuffer, Format('+ OnBlockBufferWrite(%d)', [n]));
  PrintDebugString(Format('+ %2x: %d', [IntPtr(pvBuffer), r]));
  if not Self.PostWSASendRequest(pvBuffer, pvLength, dtNone, n) then
  begin
    r := ReleaseRef(pvBuffer, '- OnBlockBufferWrite PostWSASendRequest false');
    PrintDebugString(Format('- %2x: %d', [IntPtr(pvBuffer), r]));
  end;
  {$ELSE}
  AddRef(pvBuffer);
  if not Self.PostWSASendRequest(pvBuffer, pvLength, dtNone, BLOCK_BUFFER_TAG) then
  begin
     ReleaseRef(pvBuffer);
  end;
  {$ENDIF}

end;



procedure TIOCPCoderClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: WORD);
begin
  RecvBuffer(buf, len);
end;

{$IFDEF QDAC_QWorker}
procedure TIOCPCoderClientContext.OnExecuteJob(pvJob: PQJob);
var
  lvTask:TDiocpTaskObject;
  lvObj:TObject;
begin
  while (Self.Active) do
  begin 
    //取出一个任务
    self.Lock;
    try
      lvTask := TDiocpTaskObject(FRequestQueue.DeQueue);
      if lvTask = nil then
      begin
        FIsProcessRequesting := False;
        Break;
      end;
    finally
      self.UnLock;
    end;


    // 避免断开
    if self.LockContext('OnExecuteJob', Self) then
    try    
      // 如果需要执行
      if TDiocpCoderTcpServer(FOwner).LogicWorkerNeedCoInitialize then
        pvJob.Worker.ComNeeded();
        
      DoInnerJob(lvTask);
    finally 
      self.unLockContext('OnExecuteJob', Self);
    end;
  end;


end;
{$ENDIF}

{$IFDEF DIOCP_Task}
procedure TIOCPCoderClientContext.OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
var
  lvTask:TDiocpTaskObject;
  lvObj:TObject;
begin

  while (Self.Active) do
  begin
    //取出一个任务
    self.Lock;
    try
      lvTask := TDiocpTaskObject(FRequestQueue.DeQueue);
      if lvTask = nil then
      begin
        FIsProcessRequesting := False;
        Break;
      end;
    finally
      self.UnLock;
    end;

    // 避免断开
    if self.LockContext('OnExecuteJob', Self) then
    try
      // 如果需要执行
      if TDiocpCoderTcpServer(Owner).FLogicWorkerNeedCoInitialize then
        pvTaskRequest.iocpWorker.checkCoInitializeEx();
        
      DoInnerJob(lvTask);
    finally 
      self.unLockContext('OnExecuteJob', Self);
    end;
  end; 
end;
{$ENDIF}

procedure TIOCPCoderClientContext.RecvBuffer(buf:PAnsiChar; len:Cardinal);
var
  lvTaskObject:TDiocpTaskObject;
  lvDecodeObj:TObject;
  lvDecoder:TDiocpDecoder;
  r:Integer;
begin
  lvDecoder := TDiocpCoderTcpServer(Owner).FDecoder;

  lvDecoder.SetRecvBuffer(FCoderExchange, buf, len);

  self.StateINfo := '接收到数据,准备进行解码';

  ////避免一次收到多个包时导致只调用了一次逻辑的处理(DoContextAction);
  ///  2013年9月26日 08:57:20
  ///    感谢群内JOE找到bug。
  while True do
  begin

    r := lvDecoder.Decode(FCoderExchange);
    if r = -1 then
    begin
      self.RequestDisconnect('解码失败');
      exit;
    end else if r = 1 then
    begin
      lvDecodeObj := lvDecoder.GetData(FCoderExchange, True);

      // 借一个任务类
      lvTaskObject := TDiocpCoderTcpServer(Owner).GetTaskObject;
      lvTaskObject.FContextDNA := self.ContextDNA;

      // 任务需要处理的解码对象
      lvTaskObject.FData := lvDecodeObj;
      try
        self.StateINfo := '解码成功,准备调用dataReceived进行逻辑处理';

        {$IFDEF INNER_IOCP_PROCESSOR}
        // 如果需要执行
        if TDiocpCoderTcpServer(Owner).FLogicWorkerNeedCoInitialize then
          CurrRecvRequest.checkCoInitializeEx();
          
        self.DoInnerJob(lvTaskObject);
        {$ELSE}
        // 加入到请求处理队列
        self.Lock;
        try
          FRequestQueue.EnQueue(lvTaskObject);
          
          if not FIsProcessRequesting then
          begin
            FIsProcessRequesting := true;
           {$IFDEF QDAC_QWorker}
             Workers.Post(OnExecuteJob, FRequestQueue);
           {$ELSE}
             iocpTaskManager.PostATask(OnExecuteJob, FRequestQueue);
           {$ENDIF}
          end;
        finally
          self.UnLock();
        end;
        {$ENDIF}

      except
        on E:Exception do
        begin
          Owner.LogMessage('截获投递逻辑处理异常:' + e.Message);

          // 投递异常 归还任务对象
          lvTaskObject.Close;

          if lvDecodeObj <> nil then
          begin
            TDiocpCoderTcpServer(Owner).FDecoder.ReleaseData(FCoderExchange, lvDecodeObj, True);
          end;
        end;
      end;
    end else
    begin
      //缓存中没有可以使用的完整数据包,跳出循环
      Break;
    end;
  end;
end;



procedure TIOCPCoderClientContext.WriteObject(const pvDataObject:TObject);
var
  lvOutBuffer:TBufferLink; 
  lvStart:Boolean;
begin
  lvStart := false;
  if not Active then Exit;

  if self.LockContext('WriteObject', Self) then
  try
    lock;
    try
      FBlockBuffer.ClearBuffer;
      TDiocpCoderTcpServer(Owner).FEncoder.Encode(FCoderExchange, pvDataObject, FBlockBuffer);
      FBlockBuffer.FlushBuffer;
    finally
      UnLock;
    end;
  finally
    self.unLockContext('WriteObject', Self);
    //sfLogger.logMessage('离开回写对象[%d]',[Integer(self)], 'BCB_DEBUG'); 
  end;    
end;

constructor TDiocpCoderTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTaskObjectPool := TBaseQueue.Create();
  RegisterContextClass(TIOCPCoderClientContext);
  FIocpSendRequestClass := TDiocpCoderSendRequest;

  // 4K, 每次投递4k
  FBlockBufferPool := newBufferPool(1024 * 4);
end;

destructor TDiocpCoderTcpServer.Destroy;
begin
  if FInnerDecoder <> nil then FInnerDecoder.Free;
  if FInnerEncoder <> nil then FInnerEncoder.Free;
  FTaskObjectPool.FreeDataObject;
  FTaskObjectPool.Free;

  FreeBufferPool(FBlockBufferPool);
  inherited Destroy;
end;

function TDiocpCoderTcpServer.CreateCoderExchange: TDiocpContextCoderExchange;
begin
  Assert(FCoderExchangeClass <> nil);
  Result := FCoderExchangeClass.Create;
end;

procedure TDiocpCoderTcpServer.DoAfterOpen;
begin
  inherited;
  {$IFDEF DEBUG}
  {$IFDEF CONSOLE}
  
    {$IFDEF INNER_IOCP_PROCESSOR}
      Writeln('[#] 由DIOCP线程处理Http请求');
    {$ELSE}
      {$IFDEF DIOCP_Task}
        Writeln('[#] 由DIOCP-Task处理Http请求');
      {$ENDIF}

      {$IFDEF QDAC_QWorker}
        Writeln('[#] 由QDAC-QWorkers处理Http请求');
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function TDiocpCoderTcpServer.GetTaskObject: TDiocpTaskObject;
begin
  Result := TDiocpTaskObject(FTaskObjectPool.DeQueue);
  if Result = nil then
  begin
    Result := TDiocpTaskObject.Create;
  end;
  Result.FContextDNA := 0;
  Result.FData := nil;
  Result.FOwner := Self; 
end;

procedure TDiocpCoderTcpServer.GiveBackTaskObject(pvObj: TDiocpTaskObject);
begin
  pvObj.FContextDNA := 0;
  pvObj.FData := nil;
  pvObj.FOwner := nil;
  FTaskObjectPool.EnQueue(pvObj);
end;

procedure TDiocpCoderTcpServer.OnCreateClientContext(const context:
    TIocpClientContext);
begin
  inherited;
  TIOCPCoderClientContext(context).FCoderExchange := CreateCoderExchange;
  TIOCPCoderClientContext(context).FBlockBuffer.SetBufferPool(FBlockBufferPool);
end;

procedure TDiocpCoderTcpServer.RegisterCoderClass(
    pvDecoderClass:TDiocpDecoderClass; pvEncoderClass:TDiocpEncoderClass);
begin
  if FInnerDecoder <> nil then
  begin
    raise Exception.Create('已经注册了解码器类');
  end;

  FInnerDecoder := pvDecoderClass.Create;
  RegisterDecoder(FInnerDecoder);

  if FInnerEncoder <> nil then
  begin
    raise Exception.Create('已经注册了编码器类');
  end;
  FInnerEncoder := pvEncoderClass.Create;
  RegisterEncoder(FInnerEncoder);
end;

procedure TDiocpCoderTcpServer.RegisterCoderExchangeClass(const
    pvCoderExchangeClass: TDiocpContextCoderExchangeClass);
begin
  FCoderExchangeClass := pvCoderExchangeClass;
end;

{ TDiocpCoderTcpServer }

procedure TDiocpCoderTcpServer.RegisterDecoder(pvDecoder: TDiocpDecoder);
begin
  FDecoder := pvDecoder;
end;

procedure TDiocpCoderTcpServer.RegisterEncoder(pvEncoder: TDiocpEncoder);
begin
  FEncoder := pvEncoder;
end;



{ TDiocpCoderSendRequest }

procedure TDiocpCoderSendRequest.CancelRequest;
begin
  if FBlockMem then
    FMemBlock := nil
  else if FMemBlock <> nil then
  begin
    FreeMemBlock(FMemBlock);
    FMemBlock := nil;
  end;
  inherited;  
end;

procedure TDiocpCoderSendRequest.ResponseDone;
begin
  if FBlockMem then
    FMemBlock := nil
  else if FMemBlock <> nil then
  begin
    FreeMemBlock(FMemBlock);
    FMemBlock := nil;
  end;
  inherited;
end;

{ TDiocpTaskObject }

procedure TDiocpTaskObject.Close;
begin
  Assert(FOwner <> nil, '归还重复!');
  FOwner.GiveBackTaskObject(Self);
end;

end.
