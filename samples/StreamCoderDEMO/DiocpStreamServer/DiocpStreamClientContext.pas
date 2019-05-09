unit DiocpStreamClientContext;

interface

uses
  SysUtils, Classes, Windows, Math, diocp_tcp_server, DiocpStreamProtocol,
  utils_strings;


type
  TDiocpStreamClientContext = class(TIocpClientContext)
  private
    FSendLock:Integer;
    FStreamObject: TDiocpStreamObject;
  protected
    procedure DoCleanUp;override;
    procedure OnDisconnected; override;

    procedure OnConnected; override;


    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///   数据处理
    /// </summary>
    /// <param name="pvObject"> (TDiocpStreamObject) </param>
    procedure DoContextAction(const pvObject: TMemoryStream);

    procedure WriteObject(pvObject: TMemoryStream);
  end;

implementation

constructor TDiocpStreamClientContext.Create;
begin
  inherited Create;
  FStreamObject := TDiocpStreamObject.Create();
end;

destructor TDiocpStreamClientContext.Destroy;
begin
  FreeAndNil(FStreamObject);
  inherited Destroy;
end;

procedure TDiocpStreamClientContext.DoCleanUp;
begin
  inherited DoCleanUp;
  FStreamObject.Clear;
end;

procedure TDiocpStreamClientContext.DoContextAction(const pvObject:
    TMemoryStream);
begin
  // 直接返回
  WriteObject(pvObject);
end;

procedure TDiocpStreamClientContext.OnConnected;
begin

end;

procedure TDiocpStreamClientContext.OnDisconnected;
begin
end;

procedure TDiocpStreamClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
    ErrCode: WORD);
var
  i, r:Integer;
  lvPtr:PByte;
begin
  try
    i:= 0;
    lvPtr := PByte(buf);
    while i < len do
    begin
      r := FStreamObject.InputBuffer(lvPtr^);
      if r = -2 then
      begin 
        Self.RequestDisconnect(Format('超过最大尺寸(%d)', [FStreamObject.ContentLength]));
        FStreamObject.Clear;
        Break;
      end else if r = 1 then
      begin
        DoContextAction(FStreamObject.Content);
        FStreamObject.Clear;
      end else if r = -1 then
      begin
        Self.RequestDisconnect('异常数据包(-1)');
        FStreamObject.Clear;
        Break;
      end;
      Inc(i);
      Inc(lvPtr);  
    end;
  except
    on E:Exception do
    begin
      Self.RequestDisconnect('处理逻辑出现异常:' + e.Message);
    end;
  end;  
  
end;

procedure TDiocpStreamClientContext.WriteObject(pvObject: TMemoryStream);
var
  lvBlock:array[0..MAX_BLOCK_SIZE-1] of Byte;
  r : Integer;
  lvStreamObject:TDiocpStreamObject;
begin

  lvStreamObject := TDiocpStreamObject.Create;
  try
    lvStreamObject.WrapContent(pvObject);
    lvStreamObject.ResetReadPosition;
    SpinLock(FSendLock);    // 发送锁
    try
      while True do
      begin
        r := lvStreamObject.ReadBlock(@lvBlock[0], MAX_BLOCK_SIZE);
        if (r = 0) then
        begin
          Break;
        end;
        PostWSASendRequest(@lvBlock[0], r);
      end;
    finally
      SpinUnLock(FSendLock);
    end;
  finally
    lvStreamObject.Free;
  end;

end;

end.
