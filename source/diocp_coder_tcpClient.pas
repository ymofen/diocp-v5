(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)


unit diocp_coder_tcpClient;


interface

uses
  diocp_tcp_client, diocp_sockets, diocp_coder_baseObject,
  utils_buffer, SysUtils, Classes, utils_queues, utils_safeLogger;

type
  TIocpCoderRemoteContext = class;
  TDiocpCoderTcpClient = class;
  
  TOnContextAction = procedure(pvObject:TObject) of object;

  TOnChildContextAction = procedure(pvTcpClient: TDiocpCoderTcpClient; pvContext:
      TIocpCoderRemoteContext; pvActionObject: TObject) of object;

  TDiocpCoderSendRequest = class(TIocpSendRequest)
  private
    FMemBlock:PMemoryBlock;
  protected
    procedure ResponseDone; override; 
    procedure CancelRequest;override;
  end;


  TIocpCoderRemoteContext = class(TIocpRemoteContext)
  private
    ///  正在发送的BufferLink
    FCurrentSendBufferLink: TBufferLink;

    // 待发送队列<TBufferLink队列>
    FSendingQueue: TSimpleQueue;

    FRecvBufferLink: TBufferLink;

    FInnerEncoder: TIOCPEncoder;
    FInnerDecoder: TIOCPDecoder;

    FEncoder: TIOCPEncoder;
    FDecoder: TIOCPDecoder;
    FOnContextAction: TOnContextAction;
  protected
    /// <summary>
    ///   从发送队列中取出一个要发送的对象进行发送
    /// </summary>
    procedure CheckStartPostSendBufferLink;

    /// <summary>
    ///   on recved data, run in iocp worker thread
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; errCode: WORD); override;

    /// <summary>
    ///   投递完成后，继续投递下一个请求,
    ///     只在HandleResponse中调用
    /// </summary>
    procedure PostNextSendRequest; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///   注册编码器和解码器类
    /// </summary>
    procedure RegisterCoderClass(pvDecoderClass:TIOCPDecoderClass;
        pvEncoderClass:TIOCPEncoderClass);
    /// <summary>
    ///   注册解码器
    /// </summary>
    /// <param name="pvDecoder"> (TIOCPDecoder) </param>
    procedure RegisterDecoder(pvDecoder:TIOCPDecoder);

    /// <summary>
    ///   注册编码器
    /// </summary>
    /// <param name="pvEncoder"> (TIOCPEncoder) </param>
    procedure RegisterEncoder(pvEncoder:TIOCPEncoder);


    /// <summary>
    ///   发送一个对象到服务端
    /// </summary>
    procedure WriteObject(pvObject:TObject);
  public

    /// <summary>
    ///   接收到一个对象
    /// </summary>
    property OnContextAction: TOnContextAction read FOnContextAction write
        FOnContextAction;
  end;


  TDiocpCoderTcpClient = class(TDiocpTcpClient)
  private
    FOnChildContextAction: TOnChildContextAction;
    procedure DoChildContextAction(pvContext: TIocpCoderRemoteContext;
        pvActionObject: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    property OnChildContextAction: TOnChildContextAction read FOnChildContextAction
        write FOnChildContextAction;


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






implementation



constructor TIocpCoderRemoteContext.Create;
begin
  inherited Create;
  FRecvBufferLink := TBufferLink.Create();

  FSendingQueue := TSimpleQueue.Create();
end;

destructor TIocpCoderRemoteContext.Destroy;
begin
  if IsDebugMode then
  begin
    Assert(FSendingQueue.size = 0);
  end;  
  FSendingQueue.Free;

  FreeAndNil(FRecvBufferLink);
  if FInnerDecoder <> nil then FInnerDecoder.Free;
  if FInnerEncoder <> nil then FInnerEncoder.Free;
  inherited Destroy;
end;

procedure TIocpCoderRemoteContext.CheckStartPostSendBufferLink;
var
  lvMemBlock:PMemoryBlock;
  lvDataLen: Integer;
  lvValidCount:Cardinal;
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
    if lvValidCount > lvMemBlock.DataLen then
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
      
      TDiocpCoderTcpClient(Owner).ReleaseSendRequest(lvSendRequest);
    end;
  end;          
end;

procedure TIocpCoderRemoteContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
    errCode: WORD);
var
  lvObject:TObject;
begin
  //inherited OnRecvBuffer(buf, len, errCode);
  FRecvBufferLink.AddBuffer(buf, len);

  while True do
  begin
    //调用注册的解码器<进行解码>
    lvObject := FDecoder.Decode(FRecvBufferLink, Self);
    if Integer(lvObject) = -1 then
    begin
      self.Close;
      exit;
    end else if lvObject <> nil then
    begin
      try
        try
          if Assigned(FOnContextAction) then
            FOnContextAction(lvObject);
          TDiocpCoderTcpClient(Owner).DoChildContextAction(Self, lvObject);
        except
          on E:Exception do
          begin
            Owner.LogMessage('截获处理逻辑异常!' + e.Message, '', lgvError);
          end;
        end;
      finally
        lvObject.Free;
      end;
    end else
    begin
      //缓存中没有可以使用的完整数据包,跳出循环
      Break;
    end;
  end;

  //清理缓存<如果没有可用的内存块>清理
  if FRecvBufferLink.validCount = 0 then
  begin
    FRecvBufferLink.clearBuffer;
  end else
  begin
    FRecvBufferLink.clearHaveReadBuffer;
  end;
end;

procedure TIocpCoderRemoteContext.PostNextSendRequest;
begin
  inherited PostNextSendRequest;
  CheckStartPostSendBufferLink;
end;

procedure TIocpCoderRemoteContext.RegisterCoderClass(
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

{ TIocpCoderRemoteContext }

procedure TIocpCoderRemoteContext.RegisterDecoder(pvDecoder:TIOCPDecoder);
begin
  FDecoder := pvDecoder;
end;

procedure TIocpCoderRemoteContext.RegisterEncoder(pvEncoder:TIOCPEncoder);
begin
  FEncoder := pvEncoder;
end;


procedure TIocpCoderRemoteContext.WriteObject(pvObject:TObject);
var
  lvOutBuffer:TBufferLink; 
  lvStart:Boolean;
begin
  lvStart := false;
  if not Active then Exit;

  if self.LockContext('WriteObject', Self) then
  try
    lvOutBuffer := TBufferLink.Create;
    try
      FEncoder.Encode(pvObject, lvOutBuffer);
      lock();
      try
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
  end;
end;

{ TDiocpCoderTcpClient }

constructor TDiocpCoderTcpClient.Create(AOwner: TComponent);
begin
  inherited;
  registerContextClass(TIocpCoderRemoteContext);
  FIocpSendRequestClass := TDiocpCoderSendRequest;
end;

procedure TDiocpCoderTcpClient.DoChildContextAction(pvContext:
    TIocpCoderRemoteContext; pvActionObject: TObject);
begin
  if Assigned(FOnChildContextAction) then
  begin
    FOnChildContextAction(Self, pvContext, pvActionObject);

  end;
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
