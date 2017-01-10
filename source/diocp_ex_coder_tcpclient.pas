(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)


unit diocp_ex_coder_tcpclient;


interface

uses
  diocp_tcp_client, diocp_sockets, diocp_coder_baseObject,
  SysUtils, Classes, utils_queues, utils_safeLogger, utils_BufferPool,
  utils_strings;

const
  BLOCK_BUFFER_TAG = 10000;
  
type
  TIocpCoderRemoteContext = class;
  TDiocpCoderTcpClient = class;
  
  TOnContextAction = procedure(const pvObject: Pointer) of object;

  TOnChildContextAction = procedure(const pvTcpClient: TDiocpCoderTcpClient;
      const pvContext: TIocpCoderRemoteContext; const pvActionObject: Pointer) of
      object;


  TIocpCoderRemoteContext = class(TIocpRemoteContext)
  private
    // 发送写入单线程写入
    FBlockBuffer: TBlockBuffer;
    FCoderExchange:TDiocpContextCoderExchange;
    FCoderExchangeClass:TDiocpContextCoderExchangeClass;
    
    FInnerEncoder: TDiocpEncoder;
    FInnerDecoder: TDiocpDecoder;

    FEncoder: TDiocpEncoder;
    FDecoder: TDiocpDecoder;
    FOnContextAction: TOnContextAction;
    procedure DoSendBufferCompleted(pvBuffer: Pointer; len: Cardinal; pvBufferTag,
        pvErrorCode: Integer); override;
    procedure OnBlockBufferWrite(pvSender: TObject; pvBuffer: Pointer; pvLength:
        Integer);
  protected
    function CreateCoderExchange: TDiocpContextCoderExchange;
    /// <summary>
    ///   on recved data, run in iocp worker thread
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; errCode: WORD); override;

  public
    constructor Create; override;
    destructor Destroy; override;
    
    /// <summary>
    ///   注册编码器和解码器类
    /// </summary>
    procedure RegisterCoderClass(pvDecoderClass: TDiocpDecoderClass;
        pvEncoderClass: TDiocpEncoderClass);
    procedure RegisterCoderExchangeClass(const pvCoderExchangeClass:
        TDiocpContextCoderExchangeClass);

    /// <summary>
    ///   注册解码器
    /// </summary>
    /// <param name="pvDecoder"> (TIOCPDecoder) </param>
    procedure RegisterDecoder(pvDecoder: TDiocpDecoder);

    /// <summary>
    ///   注册编码器
    /// </summary>
    /// <param name="pvEncoder"> (TIOCPEncoder) </param>
    procedure RegisterEncoder(pvEncoder: TDiocpEncoder);


    /// <summary>
    ///   发送一个对象到服务端
    /// </summary>
    procedure WriteObject(const pvObject: Pointer);
  public

    /// <summary>
    ///   接收到一个对象
    /// </summary>
    property OnContextAction: TOnContextAction read FOnContextAction write
        FOnContextAction;
  end;


  TDiocpCoderTcpClient = class(TDiocpTcpClient)
  private
    // 内存池
    // 目前用与发送
    FBlockBufferPool: PBufferPool;

    FOnChildContextAction: TOnChildContextAction;
    procedure DoChildContextAction(const pvContext: TIocpCoderRemoteContext; const
        pvActionObject: Pointer);
    procedure OnCreateContext(const pvContext: TDiocpCustomContext); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnChildContextAction: TOnChildContextAction read FOnChildContextAction
        write FOnChildContextAction;


  end;






implementation


{$IFDEF DIOCP_DEBUG}
var
  __debug_tag:Integer;
{$ENDIF}


constructor TIocpCoderRemoteContext.Create;
begin
  inherited Create;
  FBlockBuffer := TBlockBuffer.Create(nil);
  FBlockBuffer.OnBufferWrite := OnBlockBufferWrite;
end;

destructor TIocpCoderRemoteContext.Destroy;
begin
  if FCoderExchange <> nil then
  begin
    FCoderExchange.Free;
  end;

  FBlockBuffer.Free;
  if FInnerDecoder <> nil then FInnerDecoder.Free;
  if FInnerEncoder <> nil then FInnerEncoder.Free;
  inherited Destroy;
end;

function TIocpCoderRemoteContext.CreateCoderExchange:
    TDiocpContextCoderExchange;
begin
  Assert(FCoderExchangeClass <> nil);
  Result := FCoderExchangeClass.Create;
end;

procedure TIocpCoderRemoteContext.DoSendBufferCompleted(pvBuffer: Pointer; len:
    Cardinal; pvBufferTag, pvErrorCode: Integer);
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

procedure TIocpCoderRemoteContext.OnBlockBufferWrite(pvSender: TObject;
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

procedure TIocpCoderRemoteContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
    errCode: WORD);
var
  lvObject:Pointer;
  r:Integer;
begin
  FDecoder.SetRecvBuffer(FCoderExchange, buf, len);
  while True do
  begin
    //调用注册的解码器<进行解码>
    r := FDecoder.Decode(FCoderExchange);
    if r = -1 then
    begin
      self.RequestDisconnect('解码失败');
      exit;
    end else if r = 1 then
    begin
      lvObject := FDecoder.GetData(FCoderExchange, False);
      try
        try
          if Assigned(FOnContextAction) then FOnContextAction(lvObject);
          TDiocpCoderTcpClient(Owner).DoChildContextAction(Self, lvObject);
        except
          on E:Exception do
          begin
            Owner.LogMessage('截获处理逻辑异常!' + e.Message, '', lgvError);
          end;
        end;
      finally
        FDecoder.ReleaseData(FCoderExchange, lvObject, False);
      end;
    end else
    begin
      //缓存中没有可以使用的完整数据包,跳出循环
      Break;
    end;
  end;
end;

procedure TIocpCoderRemoteContext.RegisterCoderClass(pvDecoderClass:
    TDiocpDecoderClass; pvEncoderClass: TDiocpEncoderClass);
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

procedure TIocpCoderRemoteContext.RegisterCoderExchangeClass(const
    pvCoderExchangeClass: TDiocpContextCoderExchangeClass);
begin
  FCoderExchangeClass := pvCoderExchangeClass;
  if FCoderExchange <> nil then
  begin
    FCoderExchange.Free;
    FCoderExchange := nil;
  end;
  FCoderExchange := CreateCoderExchange;
end;

{ TIocpCoderRemoteContext }

procedure TIocpCoderRemoteContext.RegisterDecoder(pvDecoder: TDiocpDecoder);
begin
  FDecoder := pvDecoder;
end;

procedure TIocpCoderRemoteContext.RegisterEncoder(pvEncoder: TDiocpEncoder);
begin
  FEncoder := pvEncoder;
end;


procedure TIocpCoderRemoteContext.WriteObject(const pvObject: Pointer);
begin
  if not Active then Exit;

  if self.LockContext('WriteObject', Self) then
  try
    lock;
    try
      FBlockBuffer.ClearBuffer;
      FEncoder.Encode(FCoderExchange, pvObject, FBlockBuffer);
      FBlockBuffer.FlushBuffer;
    finally
      UnLock;
    end;     
  finally
    self.unLockContext('WriteObject', Self);
  end;
end;

{ TDiocpCoderTcpClient }

constructor TDiocpCoderTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  registerContextClass(TIocpCoderRemoteContext);
  
  // 4K, 每次投递4k
  FBlockBufferPool := newBufferPool(1024 * 4);
end;

destructor TDiocpCoderTcpClient.Destroy;
begin
  FreeBufferPool(FBlockBufferPool);
  inherited Destroy;
end;

procedure TDiocpCoderTcpClient.DoChildContextAction(const pvContext:
    TIocpCoderRemoteContext; const pvActionObject: Pointer);
begin
  if Assigned(FOnChildContextAction) then
  begin
    FOnChildContextAction(Self, pvContext, pvActionObject);
  end;
end;

procedure TDiocpCoderTcpClient.OnCreateContext(const pvContext:
    TDiocpCustomContext);
begin
  inherited;
  TIocpCoderRemoteContext(pvContext).FBlockBuffer.SetBufferPool(FBlockBufferPool);
end;

end.
