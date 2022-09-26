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
unit diocp_coder_tcpServer;

interface

/// �������뿪�أ�ֻ�ܿ���һ��
{$DEFINE INNER_IOCP_PROCESSOR}     // iocp�̴߳����¼�
{.$DEFINE QDAC_QWorker}   // ��qworker���е��ȴ����¼�
{.$DEFINE DIOCP_Task}     // ��diocp_task���е��ȴ����¼�

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

uses
  diocp_tcp_server, utils_buffer, SysUtils, Classes,
  diocp_coder_baseObject, utils_queues, utils_locker
  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp_task{$ENDIF}
  ;

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
    /// �Ƿ����ڴ�������
    FIsProcessRequesting:Boolean;
    
    /// ������������
    FRequestQueue:TSimpleQueue;
    
    /// ���ڷ��͵�BufferLink
    FCurrentSendBufferLink: TBufferLink;

    //  �����Ͷ���<TBufferLink����>
    FSendingQueue: TSimpleQueue;

    FRecvBuffers: TBufferLink;
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
  protected
    procedure Add2Buffer(buf:PAnsiChar; len:Cardinal);
    procedure ClearRecvedBuffer;
    function DecodeObject: TObject;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
    
    procedure RecvBuffer(buf:PAnsiChar; len:Cardinal); virtual;

    procedure DoCleanUp;override;
  protected
    /// <summary>
    ///   �ӷ��Ͷ�����ȡ��һ��Ҫ���͵Ķ�����з���
    /// </summary>
    procedure CheckStartPostSendBufferLink;

    /// <summary>
    ///   Ͷ����ɺ󣬼���Ͷ����һ������,
    ///     ֻ��HandleResponse�е���
    /// </summary>
    procedure PostNextSendRequest; override;
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
    ///   received buffer
    /// </summary>
    property Buffers: TBufferLink read FRecvBuffers;

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

    FInnerEncoder: TIOCPEncoder;
    FInnerDecoder: TIOCPDecoder;
    FUseMaxSendBlock: Boolean;

  protected
    FEncoder: TIOCPEncoder;
    FDecoder: TIOCPDecoder;
    FLogicWorkerNeedCoInitialize: Boolean;
    FOnContextAction: TOnContextAction;

    procedure DoAfterOpen; override;
    function GetTaskObject:TDiocpTaskObject;
    procedure GiveBackTaskObject(pvObj:TDiocpTaskObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   ע��������ͽ�������
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

  // ����������������
  ClearRequestTaskObject();

  FRequestQueue.Free;
  inherited Destroy;
end;

procedure TIOCPCoderClientContext.DoCleanUp;
begin
  /// ����ǰ���Ͷ���
  if FCurrentSendBufferLink <> nil then
  begin
    FCurrentSendBufferLink.Free;
    FCurrentSendBufferLink := nil;
  end;
  if FSendBlock <> nil then //������
  begin
    FreeMemBlock(FSendBlock);
    FSendBlock := nil;
  end;

  // �����ͷŴ����Ͷ��е�BufferLinkʵ�� 
  FSendingQueue.FreeDataObject;

  // ����������������
  ClearRequestTaskObject;

  // ���ڴ���
  FIsProcessRequesting := False;                   

  // �����Ѿ����ջ�������
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
label ReDo;
begin
  //�����У�������������
  if TDiocpCoderTcpServer(Owner).FUseMaxSendBlock then
  begin
    Lock;
    try
      if FCurrentSendBufferLink = nil then
      begin
        if FSendBlock <> nil then
        begin
          FreeMemBlock(FSendBlock);
          FSendBlock := nil;
        end;
        Exit;
      end;
      ReDo:
      lvValidCount := FCurrentSendBufferLink.validCount;
      if lvValidCount = 0 then
      begin
        FCurrentSendBufferLink.Free;
        FCurrentSendBufferLink := TBufferLink(FSendingQueue.DeQueue);
        if FCurrentSendBufferLink = nil then
        begin
          if FSendBlock <> nil then
          begin
            FreeMemBlock(FSendBlock);
            FSendBlock := nil;
          end;
          Exit;
        end;
        goto ReDo;
      end;
    finally
      UnLock;
    end;
    if FSendBlock = nil then
      FSendBlock := GetMemBlock(MB_MaxBlock);

    if lvValidCount >= MAX_SEND_BLOCK_SIZE then
    begin
      FCurrentSendBufferLink.readBuffer(FSendBlock^.Memory,MAX_SEND_BLOCK_SIZE);
      FCurrentSendBufferLink.clearHaveReadBuffer;
      lvDataLen := MAX_SEND_BLOCK_SIZE;
    end
    else
    begin
      lvDataLen := lvValidCount;
      FCurrentSendBufferLink.readBuffer(FSendBlock^.Memory,lvDataLen);
      FCurrentSendBufferLink.clearBuffer;
    end;

    lvSendRequest := TDiocpCoderSendRequest(GetSendRequest);
    lvSendRequest.FBlockMem := True;
    lvSendRequest.FMemBlock := FSendBlock;
    lvSendRequest.SetBuffer(FSendBlock.Memory, lvDataLen, dtNone);
    if not InnerPostSendRequestAndCheckStart(lvSendRequest) then
    begin
      lvSendRequest.UnBindingSendBuffer;
      lvSendRequest.FMemBlock := nil;
      lvSendRequest.CancelRequest;
      lvSendRequest.FBlockMem := False;

      /// �ͷŵ��ڴ��
      FreeMemBlock(FSendBlock);
      FSendBlock := nil;
      TDiocpCoderTcpServer(FOwner).ReleaseSendRequest(lvSendRequest);
    end;
    Exit;
  end;

  lock();
  try
    // �����ǰ����BufferΪnil ���˳�
    if FCurrentSendBufferLink = nil then
    begin
      if FSendBlock <> nil then
      begin
        FreeMemBlock(FSendBlock);
        FSendBlock := nil;
      end;
      Exit;
    end;
    // ��ȡ��һ��
    lvMemBlock := FCurrentSendBufferLink.FirstBlock;

    lvValidCount := FCurrentSendBufferLink.validCount;
    if (lvValidCount = 0) or (lvMemBlock = nil) then
    begin
      // �ͷŵ�ǰ�������ݶ���
      FCurrentSendBufferLink.Free;
            
      // �����ǰ�� û���κ�����, ���ȡ��һ��Ҫ���͵�BufferLink
      FCurrentSendBufferLink := TBufferLink(FSendingQueue.DeQueue);
      // �����ǰ����BufferΪnil ���˳�
      if FCurrentSendBufferLink = nil then
      begin
        Exit;
      end;

      // ��ȡ��Ҫ���͵�һ������
      lvMemBlock := FCurrentSendBufferLink.FirstBlock;
      
      lvValidCount := FCurrentSendBufferLink.validCount;
      if (lvValidCount = 0) or (lvMemBlock = nil) then
      begin  // û����Ҫ���͵�������
        FCurrentSendBufferLink := nil;  // û��������, �´�ѹ��ʱִ���ͷ�
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
    // �ӵ�ǰBufferLink���Ƴ��ڴ��
    FCurrentSendBufferLink.RemoveBlock(lvMemBlock);

    lvSendRequest := TDiocpCoderSendRequest(GetSendRequest);
    lvSendRequest.FBlockMem := False;
    lvSendRequest.FMemBlock := lvMemBlock;
    lvSendRequest.SetBuffer(lvMemBlock.Memory, lvDataLen, dtNone);
    if InnerPostSendRequestAndCheckStart(lvSendRequest) then
    begin
      // Ͷ�ݳɹ� �ڴ����ͷ���HandleResponse��
    end else
    begin
      lvSendRequest.UnBindingSendBuffer;
      lvSendRequest.FMemBlock := nil;
      lvSendRequest.CancelRequest;
      lvSendRequest.FBlockMem := False;

      /// �ͷŵ��ڴ��
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

  if self.LockContext('�����߼�', Self) then
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
    self.UnLockContext('�����߼�', Self);
  end; 
end;

procedure TIOCPCoderClientContext.DoInnerJob(pvTaskObject: TDiocpTaskObject);
var
  lvObj:TObject;
begin
  // ����Ķ���(��Ҫ�ͷ�)
  lvObj:= pvTaskObject.FData;
  try
    try
      // �����Ҫִ��
      if TDiocpCoderTcpServer(Owner).FLogicWorkerNeedCoInitialize then
         Self.CurrRecvRequest.IocpWorker.checkCoInitializeEx();

      // ִ������
      if DoExecuteRequest(pvTaskObject) <> S_OK then
      begin

      end;
    except
      on e:Exception do
      begin
        sfLogger.LogMessage(
          Format('DoInnerJob:%s', [e.Message]), CORE_LOG_FILE);
      end;
    end;
  finally
    try
     // �黹�������
     pvTaskObject.Close;
      // �ͷŽ������
     if lvObj <> nil then FreeAndNil(lvObj);
    except
      on e:Exception do
      begin
        self.Owner.LogMessage(
          Format('DoInnerJob::FreeAndNil(pvTaskObject):%s', [e.Message]), CORE_LOG_FILE);
      end;
    end;
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


    lvObj := lvTask.FData;
    try
      try
        // �����Ҫִ��
        if TDiocpCoderTcpServer(FOwner).LogicWorkerNeedCoInitialize then
          pvJob.Worker.ComNeeded();
          
        // ִ������
        if DoExecuteRequest(lvTask) <> S_OK then
        begin
          Break;
        end;
      except
        on E:Exception do
        begin
          Self.LogMessage('1-OnExecuteJob Err:%s', [e.Message], CORE_LOG_FILE);
        end;
      end;
    finally
      // �黹�������
      lvTask.Close;
      try
        // �ͷŽ������
        if lvObj <> nil then FreeAndNil(lvObj);
      except
        on E:Exception do
        begin
          Self.LogMessage('1-OnExecuteJob Err:%s', [e.Message], CORE_LOG_FILE);
        end;
      end;
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

    lvObj := lvTask.FData;
    try
      try
        // �����Ҫִ��
        if TDiocpCoderTcpServer(Owner).FLogicWorkerNeedCoInitialize then
          pvTaskRequest.iocpWorker.checkCoInitializeEx();

        // ִ������
        if DoExecuteRequest(lvTask) <> S_OK then
        begin
          Break;
        end;
      except
        on E:Exception do
        begin
          Self.LogMessage('1-OnExecuteJob Err:%s', [e.Message], CORE_LOG_FILE);
        end;
      end;
    finally
      // �黹�������
      lvTask.Close;
      try
        // �ͷŽ������
        if lvObj <> nil then FreeAndNil(lvObj);
      except
        on E:Exception do
        begin
          Self.LogMessage('2-OnExecuteJob Err:%s', [e.Message], CORE_LOG_FILE);
        end;
      end;
    end;
  end; 
end;
{$ENDIF}

procedure TIOCPCoderClientContext.RecvBuffer(buf:PAnsiChar; len:Cardinal);
var
  lvTaskObject:TDiocpTaskObject;
  lvDecodeObj:TObject;
begin
  Add2Buffer(buf, len);

  self.StateINfo := '���յ�����,׼�����н���';

  ////����һ���յ������ʱ����ֻ������һ���߼��Ĵ���(DoContextAction);
  ///  2013��9��26�� 08:57:20
  ///    ��лȺ��JOE�ҵ�bug��
  while True do
  begin

    //����ע��Ľ�����<���н���>
    lvDecodeObj := DecodeObject;
    if Integer(lvDecodeObj) = -1 then
    begin
      /// ����İ���ʽ, �ر�����
      RequestDisconnect('����İ���ʽ');
      exit;
    end else if lvDecodeObj <> nil then
    begin
      // ��һ��������
      lvTaskObject := TDiocpCoderTcpServer(Owner).GetTaskObject;
      lvTaskObject.FContextDNA := self.ContextDNA;

      // ������Ҫ����Ľ������
      lvTaskObject.FData := lvDecodeObj;
      try
        self.StateINfo := '����ɹ�,׼������dataReceived�����߼�����';

        {$IFDEF INNER_IOCP_PROCESSOR}
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
          Owner.LogMessage('�ػ�Ͷ���߼������쳣!' + e.Message);

          // Ͷ���쳣 �黹�������
          lvTaskObject.Close;
        end;
      end;
    end else
    begin
      //������û�п���ʹ�õ��������ݰ�,����ѭ��
      Break;
    end;
  end;

  //������<���û�п��õ��ڴ��>����
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
    //sfLogger.logMessage('�����д����[%d]',[Integer(self)], 'BCB_DEBUG');
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
    //sfLogger.logMessage('�뿪��д����[%d]',[Integer(self)], 'BCB_DEBUG'); 
  end;    
end;

constructor TDiocpCoderTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTaskObjectPool := TBaseQueue.Create();
  RegisterContextClass(TIOCPCoderClientContext);
  
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

procedure TDiocpCoderTcpServer.RegisterCoderClass(
    pvDecoderClass:TIOCPDecoderClass; pvEncoderClass:TIOCPEncoderClass);
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
