(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 ����
 *
  *   2015-04-08 12:34:33
  *    (��л suoler����bug���ṩbug����)
  *    �첽�����߼������OnContextAction
  *      �������Ѿ��رգ���������û�����ü�����Ȼ�������������Ѿ��黹���أ����ʱ��Ӧ�÷�����������()
 *)
unit diocp_ex_coder_tcpserver;

interface

/// �������뿪�أ�ֻ�ܿ���һ��
{.$DEFINE INNER_IOCP_PROCESSOR}     // iocp�̴߳����¼�
{.$DEFINE QDAC_QWorker}   // ��qworker���е��ȴ����¼�
{$DEFINE DIOCP_Task}     // ��diocp_task���е��ȴ����¼�

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
  ///   �����������, ���ڴ����첽����ʱ�����ԶԱ�����ʱ����Ϣ�����ڿ��Խ���ȡ������
  /// </summary>
  TDiocpTaskObject = class(TObject)
  private
    FOwner:TDiocpCoderTcpServer;
    /// <summary>
    ///   Ͷ���첽֮ǰ��¼DNA���������첽����ʱ���Ƿ�ȡ����ǰ����
    /// </summary>
    FContextDNA:Integer;
    // �������
    FData: TObject;
  public
    /// <summary>
    ///   �黹�������
    /// </summary>
    procedure Close;
  end;

  TIOCPCoderClientContext = class(diocp_tcp_server.TIOCPClientContext)
  private
    FCoderExchange:TDiocpContextCoderExchange;
    
    // ����д�뵥�߳�д��
    FBlockBuffer: TBlockBuffer;

    /// �Ƿ����ڴ�������
    FIsProcessRequesting:Boolean;
    
    /// ������������
    FRequestQueue:TSimpleQueue;
    

    FStateINfo: String;
    FSendBlock: PMemoryBlock; //������
    function GetStateINfo: String;

    /// <summary>
    ///  ִ��һ������
    /// </summary>
    function DoExecuteRequest(pvTaskObj: TDiocpTaskObject): HRESULT;

    /// <summary>
    ///   ���������б��еĶ���
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
    ///   ���յ�һ�����������ݰ�
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure DoContextAction(const pvDataObject:TObject); virtual;

    /// <summary>
    ///   ��д����(���Ͷ����ͻ���, ����ý��������н���)
    /// </summary>
    /// <param name="pvDataObject"> Ҫ��д�Ķ��� </param>
    procedure WriteObject(const pvDataObject:TObject);


    /// <summary>
    ///   һЩ״̬��Ϣ
    /// </summary>
    property StateINfo: String read GetStateINfo write FStateINfo;
  end;



  TOnContextAction = procedure(pvClientContext:TIOCPCoderClientContext;
      pvObject:TObject) of object;

  {$IF RTLVersion>22}
  // thanks: �����ٷ�19183455
  //  vcl for win64
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TDiocpCoderTcpServer = class(TDiocpTcpServer)
  private
    ///�첽����Ͷ�ݶ����
    FTaskObjectPool: TBaseQueue;

    FCoderExchangeClass:TDiocpContextCoderExchangeClass;

    FInnerEncoder: TDiocpEncoder;
    FInnerDecoder: TDiocpDecoder;
    FUseMaxSendBlock: Boolean;
    
    // �ڴ��
    // Ŀǰ���뷢��
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
    ///   �������µ����Ӷ���ʱ����õĺ���
    ///   ��������������һЩ��ʼ��
    /// </summary>
    procedure OnCreateClientContext(const context: TIocpClientContext); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   ע��������ͽ�������
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
    ///   �����߼��߳�ִ���߼�ǰִ��CoInitalize
    /// </summary>
    property LogicWorkerNeedCoInitialize: Boolean read FLogicWorkerNeedCoInitialize write FLogicWorkerNeedCoInitialize;

    /// <summary>
    ///   �յ�һ�����������ݰ���ִ���¼�(��IocpTask/Qworker�߳��д���)
    /// </summary>
    property OnContextAction: TOnContextAction read FOnContextAction write FOnContextAction;

    /// <summary>
    ///  �����У�����һ���Ƿ�ʹ�ô����ݿ鷢��ģʽ���������ΪTrue����ô�����ڷ��͵�ʱ��ÿ�η��Ͱ���
    ///  TCP��Э���������ϣ�һ��Ĭ������͵���64K
    ///  TCP ���Ĵ�С��Ӧ���� 1500 - IPͷ(20) - TCPͷ(20) = 1460 (BYTES)��Ҳ���൱��1460*44�Ŀ���з���һ����
    ///   �յ�һ�����������ݰ���ִ���¼�(��IocpTask/Qworker�߳��д���)
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

  // ����������������
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
  // ����������������
  ClearRequestTaskObject;

  // ���ڴ���
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
      
      // �黹�������
      lvTask.Close;
      try
        // �ͷŽ������
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
  // �����Ѿ��Ͽ�
  if Owner = nil then Exit;

  // �����Ѿ��ͷ�
  if Self = nil then Exit;

  // �Ѿ����ǵ���Ͷ�ݵ�����
  if self.ContextDNA <> pvTaskObj.FContextDNA then Exit;



  self.CheckThreadIn(STRING_EMPTY);
  try
    try
      // ִ��Owner���¼�
      if Assigned(TDiocpCoderTcpServer(Owner).FOnContextAction) then
        TDiocpCoderTcpServer(Owner).FOnContextAction(Self, lvObj);
      DoContextAction(lvObj);
    except
     on E:Exception do
      begin
        FOwner.LogMessage('�ػ����߼��쳣:' + e.Message);
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
      // ִ������
      if DoExecuteRequest(pvTaskObject) <> S_OK then
      begin
        // ִ��ʧ��             
      end;
    except
      on E:Exception do
      begin
        Self.Owner.LogMessage('1-DoInnerJob Err:%s', [e.Message], CORE_LOG_FILE);
      end;
    end;
  finally
    try
      // �黹�������
      pvTaskObject.Close;
      // �ͷŽ������
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
    //ȡ��һ������
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


    // ����Ͽ�
    if self.LockContext('OnExecuteJob', Self) then
    try    
      // �����Ҫִ��
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
    //ȡ��һ������
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

    // ����Ͽ�
    if self.LockContext('OnExecuteJob', Self) then
    try
      // �����Ҫִ��
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

  self.StateINfo := '���յ�����,׼�����н���';

  ////����һ���յ������ʱ����ֻ������һ���߼��Ĵ���(DoContextAction);
  ///  2013��9��26�� 08:57:20
  ///    ��лȺ��JOE�ҵ�bug��
  while True do
  begin

    r := lvDecoder.Decode(FCoderExchange);
    if r = -1 then
    begin
      self.RequestDisconnect('����ʧ��');
      exit;
    end else if r = 1 then
    begin
      lvDecodeObj := lvDecoder.GetData(FCoderExchange, True);

      // ��һ��������
      lvTaskObject := TDiocpCoderTcpServer(Owner).GetTaskObject;
      lvTaskObject.FContextDNA := self.ContextDNA;

      // ������Ҫ����Ľ������
      lvTaskObject.FData := lvDecodeObj;
      try
        self.StateINfo := '����ɹ�,׼������dataReceived�����߼�����';

        {$IFDEF INNER_IOCP_PROCESSOR}
        // �����Ҫִ��
        if TDiocpCoderTcpServer(Owner).FLogicWorkerNeedCoInitialize then
          CurrRecvRequest.checkCoInitializeEx();
          
        self.DoInnerJob(lvTaskObject);
        {$ELSE}
        // ���뵽���������
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
          Owner.LogMessage('�ػ�Ͷ���߼������쳣:' + e.Message);

          // Ͷ���쳣 �黹�������
          lvTaskObject.Close;

          if lvDecodeObj <> nil then
          begin
            TDiocpCoderTcpServer(Owner).FDecoder.ReleaseData(FCoderExchange, lvDecodeObj, True);
          end;
        end;
      end;
    end else
    begin
      //������û�п���ʹ�õ��������ݰ�,����ѭ��
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
    //sfLogger.logMessage('�뿪��д����[%d]',[Integer(self)], 'BCB_DEBUG'); 
  end;    
end;

constructor TDiocpCoderTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTaskObjectPool := TBaseQueue.Create();
  RegisterContextClass(TIOCPCoderClientContext);
  FIocpSendRequestClass := TDiocpCoderSendRequest;

  // 4K, ÿ��Ͷ��4k
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
      Writeln('[#] ��DIOCP�̴߳���Http����');
    {$ELSE}
      {$IFDEF DIOCP_Task}
        Writeln('[#] ��DIOCP-Task����Http����');
      {$ENDIF}

      {$IFDEF QDAC_QWorker}
        Writeln('[#] ��QDAC-QWorkers����Http����');
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
    raise Exception.Create('�Ѿ�ע���˽�������');
  end;

  FInnerDecoder := pvDecoderClass.Create;
  RegisterDecoder(FInnerDecoder);

  if FInnerEncoder <> nil then
  begin
    raise Exception.Create('�Ѿ�ע���˱�������');
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
  Assert(FOwner <> nil, '�黹�ظ�!');
  FOwner.GiveBackTaskObject(Self);
end;

end.
