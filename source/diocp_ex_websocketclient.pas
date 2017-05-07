unit diocp_ex_websocketclient;

interface

uses
  diocp_tcp_client, diocp_ex_http_common, SysUtils, Classes, utils_url,
  utils_websocket, utils_strings;

type
  TDiocpWebSocketContext = class(TIocpRemoteContext)
  private
    FURL: TURL;
    FHeaderBuilder: THttpHeaderBuilder;
    /// <summary>
    ///  WebSocket接收到的整个数据
    /// </summary>
    FWebSocketContentBuffer: TDBufferBuilder;

    FWsFrame: TDiocpWebSocketFrame;
    FHttpBuffer: THttpBuffer;
    FOnRecv: TNotifyEvent;
    FOnShakeHand: TNotifyEvent;

    FRecvShakeHand:Byte;
    procedure PostWebSocketRequest;
  protected
    procedure OnConnected; override;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Open(WsUrl:string);

    procedure SendBuffer(buf: Pointer; len: Cardinal; opcode: Byte);

    property HttpBuffer: THttpBuffer read FHttpBuffer;
    property WebSocketContentBuffer: TDBufferBuilder read FWebSocketContentBuffer;
    property OnRecv: TNotifyEvent read FOnRecv write FOnRecv;
    property OnShakeHand: TNotifyEvent read FOnShakeHand write FOnShakeHand;
  end;

  TDiocpWebSocketTcpClient = class(TDiocpTcpClient)
  public
    constructor Create(AOwner: TComponent); override;
  end;


procedure DoInitializeWebSocketClient;
procedure DoFinalizeWebSocketClient;

function NewWsClient: TDiocpWebSocketContext;

  

implementation

var
  __webtcpClient:TDiocpWebSocketTcpClient;

procedure DoInitializeWebSocketClient;
begin
   __webtcpClient := TDiocpWebSocketTcpClient.Create(nil);
   __webtcpClient.Open;
end;

procedure DoFinalizeWebSocketClient;
begin
  __webtcpClient.Close;
  __webtcpClient.Free;
end;

function NewWsClient: TDiocpWebSocketContext;
begin
  Result := TDiocpWebSocketContext(__webtcpClient.Add);
end;

constructor TDiocpWebSocketContext.Create;
begin
  inherited Create;
  FHeaderBuilder := THttpHeaderBuilder.Create();
  FURL := TURL.Create();
  FWsFrame := TDiocpWebSocketFrame.Create();
  FHttpBuffer := THttpBuffer.Create();
  FWebSocketContentBuffer := TDBufferBuilder.Create();
end;

destructor TDiocpWebSocketContext.Destroy;
begin
  FreeAndNil(FWebSocketContentBuffer);
  FreeAndNil(FHttpBuffer);
  FreeAndNil(FWsFrame);
  FreeAndNil(FURL);
  FreeAndNil(FHeaderBuilder);
  inherited Destroy;
end;

procedure TDiocpWebSocketContext.OnConnected;
begin
  inherited;
  FRecvShakeHand := 0;
  PostWebSocketRequest;
  FHttpBuffer.DoCleanUp;
  FWsFrame.DoCleanUp;
end;

procedure TDiocpWebSocketContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
    ErrCode: WORD);
var
  lvPtr:PByte;
  i, r: Integer;
begin
  inherited;
  lvPtr := PByte(buf);
  for i := 0 to len - 1 do
  begin
    if FRecvShakeHand = 0 then
    begin
      r := FHttpBuffer.InputBuffer(lvPtr^);
      Inc(lvPtr);
      if r = 1 then
      begin
        FRecvShakeHand := 1;
        if Assigned(FOnShakeHand) then
        begin
          FOnShakeHand(Self);
        end;
        FHttpBuffer.DoCleanUp;
      end;
    end else
    begin
      r := FWsFrame.InputBuffer(lvPtr^);
      Inc(lvPtr);
      if r = 1 then
      begin
        FWebSocketContentBuffer.AppendBuffer(FWsFrame.ContentBuffer, FWsFrame.ContentLength);
        if FWsFrame.GetFIN <> FIN_EOF then
        begin
          FWsFrame.DoCleanUp();
        end else
        begin
          if Assigned(FOnRecv) then
          begin
            FOnRecv(Self);
          end;
          FWsFrame.DoCleanUp;
          FWebSocketContentBuffer.Clear;
        end;      
      end;      
    end;
  end;
end;

procedure TDiocpWebSocketContext.Open(WsUrl:string);
begin
  CheckCanConnect;
  FURL.SetURL(WsUrl);
  Host := FURL.Host;
  Port := StrToInt(FURL.Port);
  Connect();
end;

procedure TDiocpWebSocketContext.PostWebSocketRequest;
var
  s:String;
  lvBytes:TBytes;
begin

  //    GET ws://127.0.0.1:8081/ HTTP/1.1
  //    Host: 127.0.0.1:8081
  //    Connection: Upgrade
  //    Pragma: no-cache
  //    Cache-Control: no-cache
  //    Upgrade: websocket
  //    Origin: http://127.0.0.1:8081
  //    Sec-WebSocket-Version: 13
  //    User-Agent: Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/38.0.2125.122 Safari/537.36
  //    Accept-Encoding: gzip,deflate,sdch
  //    Accept-Language: zh-CN,zh;q=0.8
  //    Cookie: diocp_cookie=xxxx
  //    Sec-WebSocket-Key: pAwC+w4+DLzmrLTUuBG4cQ==
  //    Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits

   FHeaderBuilder.URI := FURL.URI;
   FHeaderBuilder.Method := 'GET';
   FHeaderBuilder.SetHeader('Connection', 'Upgrade');
   FHeaderBuilder.SetHeader('Upgrade', 'websocket');
   s := FHeaderBuilder.Build();

   lvBytes := StringToBytes(s);

   Self.PostWSASendRequest(PByte(@lvBytes[0]), Length(lvBytes));    
   
end;

procedure TDiocpWebSocketContext.SendBuffer(buf: Pointer; len: Cardinal;
    opcode: Byte);
var
  lvWSFrame:TDiocpWebSocketFrame;
begin  
  lvWSFrame := TDiocpWebSocketFrame.Create;
  try
    lvWSFrame.EncodeBuffer(buf, len, true, opcode);

    PostWSASendRequest(lvWSFrame.Buffer.Memory, lvWSFrame.Buffer.Length);
  finally
    lvWSFrame.Free;
  end;
  
end;

constructor TDiocpWebSocketTcpClient.Create(AOwner: TComponent);
begin
  inherited;
  RegisterContextClass(TDiocpWebSocketContext);
end;


end.
