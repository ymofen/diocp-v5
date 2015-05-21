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
unit diocp.coder.tcpServer;

interface

// call DoContextAction procedure with qworker
{.$DEFINE QDAC_QWorker}

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

uses
  diocp.tcp.server, utils.buffer, SysUtils, Classes,
  diocp.coder.baseObject, utils.queues, utils.locker
  {$IFDEF QDAC_QWorker}
    , qworker
  {$ELSE}
    , diocp.task
  {$ENDIF}
  ;

type
  TDiocpCoderTcpServer = class;

  TDiocpCoderSendRequest = class(TIocpSendRequest)
  private
    FMemBlock:PMemoryBlock;
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

  TIOCPCoderClientContext = class(diocp.tcp.server.TIOCPClientContext)
  private
    /// 是否正在处理请求
    FIsProcessRequesting:Boolean;
    
    /// 请求的任务队列
    FRequestQueue:TSimpleQueue;
    
    /// 正在发送的BufferLink
    FCurrentSendBufferLink: TBufferLink;

    //  待发送队列<TBufferLink队列>
    FSendingQueue: TSimpleQueue;

    FRecvBuffers: TBufferLink;
    FStateINfo: String;
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
   {$ELSE}
    procedure OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
   {$ENDIF}
  protected
    procedure Add2Buffer(buf:PAnsiChar; len:Cardinal);
    procedure ClearRecvedBuffer;
    function DecodeObject: TObject;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
    
    procedure RecvBuffer(buf:PAnsiChar; len:Cardinal); virtual;

    procedure DoCleanUp;override;
  protected
    /// <summary>
    ///   从发送队列中取出一个要发送的对象进行发送
    /// </summary>
    procedure CheckStartPostSendBufferLink;

    /// <summary>
    ///   投递完成后，继续投递下一个请求,
    ///     只在HandleResponse中调用
    /// </summary>
    procedure PostNextSendRequest; override;
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
    ///   received buffer
    /// </summary>
    property Buffers: TBufferLink read FRecvBuffers;

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

    FInnerEncoder: TIOCPEncoder;
    FInnerDecoder: TIOCPDecoder;

    FEncoder: TIOCPEncoder;
    FDecoder: TIOCPDecoder;
    FLogicWorkerNeedCoInitialize: Boolean;
    FOnContextAction: TOnContextAction;

    function GetTaskObject:TDiocpTaskObject;
    procedure GiveBackTaskObject(pvObj:TDiocpTaskObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   注册编码器和解码器类
    /// </summary>
    procedure RegisterCoderClass(pvDecoderClass:TIOCPDecoderClass;
        pvEncoderClass:TIOCPEncoderClass);

    /// <summary>
    ///   register Decoder instance
    /// </summary>
    /// <param name="pvDecoder"> (TIOCPDecoder) </param>
    procedure RegisterDecoder(pvDecoder:TIOCPDecoder);

    /// <summary>
    ///   register Encoder instance
    /// </summary>
    /// <param name="pvEncoder"> (TIOCPEncoder) </param>
    procedure RegisterEncoder(pvEncoder:TIOCPEncoder);

  published

    /// <summary>
    ///   处理逻辑线程执行逻辑前执行CoInitalize
    /// </summary>
    property LogicWorkerNeedCoInitialize: Boolean read FLogicWorkerNeedCoInitialize write FLogicWorkerNeedCoInitialize;

    /// <summary>
    ///   收到一个完整的数据包的执行事件(在IocpTask/Qworker线程中触发)
    /// </summary>
    property OnContextAction: TOnContextAction read FOnContextAction write FOnContextAction;
  end;



implementation

uses
  utils.safeLogger;

constructor TIOCPCoderClientContext.Create;
begin
  inherited Create;
  FSendingQueue := TSimpleQueue.Create();
  FRequestQueue := TSimpleQueue.Create();
  FRecvBuffers := TBufferLink.Create();
end;

destructor TIOCPCoderClientContext.Destroy;
begin
  if IsDebugMode then
  begin
    Assert(FSendingQueue.size = 0);
  end;

  FSendingQueue.Free;
  FRecvBuffers.Free;

  // 清理待处理请求队列
  ClearRequestTaskObject();

  FRequestQueue.Free;
  inherited Destroy;
end;

procedure TIOCPCoderClientContext.DoCleanUp;
begin
  /// 清理当前发送队列
  if FCurrentSendBufferLink <> nil then
  begin
    FCurrentSendBufferLink.Free;
  end;

  // 清理释放待发送队列的BufferLink实例 
  FSendingQueue.FreeDataObject;

  // 清理待处理请求队列
  ClearRequestTaskObject;

  // 正在处理
  FIsProcessRequesting := False;                   

  // 清理已经接收缓存数据
  FRecvBuffers.clearBuffer;
  inherited;
end;

procedure TIOCPCoderClientContext.Add2Buffer(buf:PAnsiChar; len:Cardinal);
begin
  //add to context receivedBuffer
  FRecvBuffers.AddBuffer(buf, len);
end;

procedure TIOCPCoderClientContext.CheckStartPostSendBufferLink;
var
  lvMemBlock:PMemoryBlock;
  lvValidCount, lvDataLen: Integer;
  lvSendRequest:TDiocpCoderSendRequest;
begin
  lock();
  try
    // 如果当前发送Buffer为nil 则退出
    if FCurrentSendBufferLink = nil then Exit;

    // 获取第一块
    lvMemBlock := FCurrentSendBufferLink.FirstBlock;

    lvValidCount := FCurrentSendBufferLink.validCount;
    if (lvValidCount = 0) or (lvMemBlock = nil) then
    begin
      // 释放当前发送数据对象
      FCurrentSendBufferLink.Free;
            
      // 如果当前块 没有任何数据, 则获取下一个要发送的BufferLink
      FCurrentSendBufferLink := TBufferLink(FSendingQueue.DeQueue);
      // 如果当前发送Buffer为nil 则退出
      if FCurrentSendBufferLink = nil then Exit;

      // 获取需要发送的一块数据
      lvMemBlock := FCurrentSendBufferLink.FirstBlock;
      
      lvValidCount := FCurrentSendBufferLink.validCount;
      if (lvValidCount = 0) or (lvMemBlock = nil) then
      begin  // 没有需要发送的数据了
        FCurrentSendBufferLink := nil;  // 没有数据了, 下次压入时执行释放
        exit;      
      end; 
    end;
    if lvValidCount > Integer(lvMemBlock.DataLen) then
    begin
      lvDataLen := lvMemBlock.DataLen;
    end else
    begin
      lvDataLen := lvValidCount;
    end;


  finally
    unLock();
  end;

  if lvDataLen > 0 then
  begin
    // 从当前BufferLink中移除内存块
    FCurrentSendBufferLink.RemoveBlock(lvMemBlock);

    lvSendRequest := TDiocpCoderSendRequest(GetSendRequest);
    lvSendRequest.FMemBlock := lvMemBlock;
    lvSendRequest.SetBuffer(lvMemBlock.Memory, lvDataLen, dtNone);
    if InnerPostSendRequestAndCheckStart(lvSendRequest) then
    begin
      // 投递成功 内存块的释放在HandleResponse中
    end else
    begin
      lvSendRequest.UnBindingSendBuffer;
      lvSendRequest.FMemBlock := nil;
      lvSendRequest.CancelRequest;

      /// 释放掉内存块
      FreeMemBlock(lvMemBlock);
      
      TDiocpCoderTcpServer(FOwner).ReleaseSendRequest(lvSendRequest);
    end;
  end;          
end;

procedure TIOCPCoderClientContext.ClearRecvedBuffer;
begin
  if FRecvBuffers.validCount = 0 then
  begin
    FRecvBuffers.clearBuffer;
  end else
  begin
    FRecvBuffers.clearHaveReadBuffer;
  end;
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

  if self.LockContext('处理逻辑', Self) then
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
    self.UnLockContext('处理逻辑', Self);
  end; 
end;

function TIOCPCoderClientContext.DecodeObject: TObject;
begin
  Result := TDiocpCoderTcpServer(Owner).FDecoder.Decode(FRecvBuffers, Self);
end;

function TIOCPCoderClientContext.GetStateINfo: String;
begin
  Result := FStateINfo;
end;



procedure TIOCPCoderClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: WORD);
begin
  RecvBuffer(buf, len);
end;

procedure TIOCPCoderClientContext.PostNextSendRequest;
begin
  inherited PostNextSendRequest;
  CheckStartPostSendBufferLink;
end;

{$IFDEF QDAC_QWorker}
procedure TIOCPCoderClientContext.OnExecuteJob(pvJob: PQJob);
var
  lvTask:TDiocpTaskObject;
  lvObj:TObject;
begin
  try
    while (Self.Active) do
    begin
      //取出一个任务
      //取出一个任务
      self.Lock;
      try
        lvTask := TDiocpTaskObject(FRequestQueue.DeQueue);
      finally
        self.UnLock;
      end;
      if lvTask = nil then Break;
      
      lvObj := lvTask.FData;
      try
        try
          // 执行任务
          if DoExecuteRequest(lvTask) <> S_OK then
          begin
            Break;
          end;
        except          
        end;
      finally
        // 归还到任务池
        lvTask.Close;
        try
          // 释放解码对象
          if lvObj <> nil then FreeAndNil(lvObj);
        except
        end;
      end;
    end;
  finally
    // 置正在处理任务标记
    self.Lock();
    FIsProcessRequesting := False;
    self.Unlock(); 
  end;

end;
{$ELSE}

procedure TIOCPCoderClientContext.OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
var
  lvTask:TDiocpTaskObject;
  lvObj:TObject;
begin
  try
    while (Self.Active) do
    begin
      //取出一个任务
      self.Lock;
      try
        lvTask := TDiocpTaskObject(FRequestQueue.DeQueue);
      finally
        self.UnLock;
      end;
      if lvTask = nil then Break;
      
      lvObj := lvTask.FData;
      try
        try
          // 如果需要执行
          if TDiocpCoderTcpServer(Owner).FLogicWorkerNeedCoInitialize then
            pvTaskRequest.iocpWorker.checkCoInitializeEx();

          // 执行任务
          if DoExecuteRequest(lvTask) <> S_OK then
          begin
            Break;
          end;
        except          
        end;
      finally
        // 归还到任务池
        lvTask.Close;
        try
          // 释放解码对象
          if lvObj <> nil then FreeAndNil(lvObj);
        except
        end;
      end;
    end;
  finally
    // 置正在处理任务标记
    self.Lock();
    FIsProcessRequesting := False;
    self.Unlock(); 
  end;
//
//  lvTask := TDiocpTaskObject(pvTaskRequest.TaskData);
//  lvObj := lvTask.FData;
//  try
//    // 连接已经断开
//    if Owner = nil then Exit;
//
//    // 连接已经释放
//    if Self = nil then Exit;
//
//    // 已经不是当初投递的连接
//    if self.ContextDNA <> lvTask.FContextDNA then Exit;
//
//    if self.LockContext('处理逻辑', Self) then
//    try
//      try
//        if TDiocpCoderTcpServer(Owner).FLogicWorkerNeedCoInitialize then
//          pvTaskRequest.iocpWorker.checkCoInitializeEx();
//
//        // 执行Owner的事件
//        if Assigned(TDiocpCoderTcpServer(Owner).FOnContextAction) then
//          TDiocpCoderTcpServer(Owner).FOnContextAction(Self, lvObj);
//
//        DoContextAction(lvObj);
//      except
//       on E:Exception do
//        begin
//          FOwner.LogMessage('截获处理逻辑异常:' + e.Message);
//        end;
//      end;
//    finally
//      self.UnLockContext('处理逻辑', Self);
//    end;
//  finally
//    // 归还到任务池
//    lvTask.Close;
//    try
//      // 释放解码对象
//      if lvObj <> nil then FreeAndNil(lvObj);
//    except
//    end;
//  end;
end;
{$ENDIF}

procedure TIOCPCoderClientContext.RecvBuffer(buf:PAnsiChar; len:Cardinal);
var
  lvTaskObject:TDiocpTaskObject;
  lvDecodeObj:TObject;
begin
  Add2Buffer(buf, len);

  self.StateINfo := '接收到数据,准备进行解码';

  ////避免一次收到多个包时导致只调用了一次逻辑的处理(DoContextAction);
  ///  2013年9月26日 08:57:20
  ///    感谢群内JOE找到bug。
  while True do
  begin

    //调用注册的解码器<进行解码>
    lvDecodeObj := DecodeObject;
    if Integer(lvDecodeObj) = -1 then
    begin
      /// 错误的包格式, 关闭连接
      DoDisconnect;
      exit;
    end else if lvDecodeObj <> nil then
    begin
      // 借一个任务类
      lvTaskObject := TDiocpCoderTcpServer(Owner).GetTaskObject;
      lvTaskObject.FContextDNA := self.ContextDNA;

      // 任务需要处理的解码对象
      lvTaskObject.FData := lvDecodeObj;
      try
        self.StateINfo := '解码成功,准备调用dataReceived进行逻辑处理';


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

      except
        on E:Exception do
        begin
          Owner.LogMessage('截获投递逻辑处理异常!' + e.Message);

          // 投递异常 归还任务对象
          lvTaskObject.Close;
        end;
      end;
    end else
    begin
      //缓存中没有可以使用的完整数据包,跳出循环
      Break;
    end;
  end;

  //清理缓存<如果没有可用的内存块>清理
  ClearRecvedBuffer;
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
    //sfLogger.logMessage('进入回写对象[%d]',[Integer(self)], 'BCB_DEBUG');
    lvOutBuffer := TBufferLink.Create;
    try
      TDiocpCoderTcpServer(Owner).FEncoder.Encode(pvDataObject, lvOutBuffer);
      lock();
      try
        if FSendingQueue.size >= TDiocpCoderTcpServer(Owner).MaxSendingQueueSize then
        begin
          raise Exception.Create('Out of MaxSendingQueueSize!!!');
        end;
        FSendingQueue.EnQueue(lvOutBuffer);
        if FCurrentSendBufferLink = nil then
        begin
          FCurrentSendBufferLink := TBufferLink(FSendingQueue.DeQueue);
          lvStart := true;
        end;
      finally
        unLock;
      end;
    except
      lvOutBuffer.Free;
      raise;           
    end;
    
    if lvStart then
    begin
      CheckStartPostSendBufferLink;
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
  FClientContextClass := TIOCPCoderClientContext;
  
  FIocpSendRequestClass := TDiocpCoderSendRequest;
end;

destructor TDiocpCoderTcpServer.Destroy;
begin
  if FInnerDecoder <> nil then FInnerDecoder.Free;
  if FInnerEncoder <> nil then FInnerEncoder.Free;
  FTaskObjectPool.FreeDataObject;
  FTaskObjectPool.Free;
  inherited Destroy;
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

procedure TDiocpCoderTcpServer.RegisterCoderClass(
    pvDecoderClass:TIOCPDecoderClass; pvEncoderClass:TIOCPEncoderClass);
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

{ TDiocpCoderTcpServer }

procedure TDiocpCoderTcpServer.RegisterDecoder(pvDecoder:TIOCPDecoder);
begin
  FDecoder := pvDecoder;
end;

procedure TDiocpCoderTcpServer.RegisterEncoder(pvEncoder:TIOCPEncoder);
begin
  FEncoder := pvEncoder;
end;



{ TDiocpCoderSendRequest }

procedure TDiocpCoderSendRequest.CancelRequest;
begin
  if FMemBlock <> nil then
  begin
    FreeMemBlock(FMemBlock);
    FMemBlock := nil;
  end;
  inherited;  
end;

procedure TDiocpCoderSendRequest.ResponseDone;
begin
  if FMemBlock <> nil then
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
