unit diocp_ex_websocketclient;

interface

uses
  diocp_tcp_client, diocp_ex_http_common, SysUtils, Classes, utils_url,
  utils_websocket, utils_strings, diocp_core_rawWinSocket, diocp_core_engine,
  utils_base64;

type
  TDiocpWebSocketContext = class(TIocpRemoteContext)
  private
    FSendPingTick: Cardinal;
    FURL: TURL;
    FWsUrl: String;
    FHeaderBuilder: THttpHeaderBuilder;
    /// <summary>
    /// WebSocket接收到的整个数据
    /// </summary>
    FWebSocketContentBuffer: TDBufferBuilder;

    FWsFrame: TDiocpWebSocketFrame;
    FHttpBuffer: THttpBuffer;
    FHttpOrigin: string;
    FOnRecv: TNotifyEvent;
    FOnShakeHand: TNotifyEvent;

    FRecvShakeHand: Byte;
    FOnDisconnectedEvent: TNotifyEvent;
    FMasked: Boolean;
    procedure PostWebSocketRequest;
  protected
    procedure OnConnected; override;
    procedure OnDisconnected; override;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
    procedure DoRecv();
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Open(WsUrl: string);

    procedure SendPing;

    procedure CheckSendPing(pvInterval: Cardinal = 20000);

    function SendBuffer(buf: Pointer; len: Cardinal; opcode: Byte): Boolean;

    procedure SendText(const s: string);

    property HeaderBuilder: THttpHeaderBuilder read FHeaderBuilder;
    property HttpBuffer: THttpBuffer read FHttpBuffer;
    property OnDisconnectedEvent: TNotifyEvent read FOnDisconnectedEvent write FOnDisconnectedEvent;

    property WebSocketContentBuffer: TDBufferBuilder read FWebSocketContentBuffer;
    property OnRecv: TNotifyEvent read FOnRecv write FOnRecv;
    property OnShakeHand: TNotifyEvent read FOnShakeHand write FOnShakeHand;
    property HttpOrigin: string read FHttpOrigin write FHttpOrigin;
    property Masked: Boolean read FMasked write FMasked;


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
  __webtcpClient: TDiocpWebSocketTcpClient;

procedure DoInitializeWebSocketClient;
begin
  if __webtcpClient = nil then
  begin
    __webtcpClient := TDiocpWebSocketTcpClient.Create(nil);
    __webtcpClient.Open;
  end;
end;

procedure DoFinalizeWebSocketClient;
begin
  if __webtcpClient <> nil then
  begin
    Assert(__defaultDiocpEngine <> nil, 'iocp engine is null');

    __webtcpClient.Close;
    __webtcpClient.Free;
    __webtcpClient := nil;
  end;
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

procedure TDiocpWebSocketContext.CheckSendPing(pvInterval: Cardinal = 20000);
begin
  if self.SocketState <> ssConnected then
    Exit;

  if tick_diff(FSendPingTick, GetTickCount) > pvInterval then
  begin
    SendPing;
    FSendPingTick := GetTickCount;
  end;
end;

procedure TDiocpWebSocketContext.DoRecv;
var
  lvOptCode: Integer;
begin
  lvOptCode := FWsFrame.GetOptCode;
  if lvOptCode = OPT_PING then
  begin
    PostWSASendRequest(@WS_MSG_PONG, 2, False);
  end
  else if lvOptCode = OPT_PONG then
  begin
    Assert(lvOptCode = OPT_PONG);; // {noop}
  end
  else if lvOptCode = OPT_CLOSE then
  begin
    RequestDisconnect('收到WebSocket-Close请求');
  end
  else
  begin
    if Assigned(FOnRecv) then
    begin
      FOnRecv(self);
    end;
  end;
end;

procedure TDiocpWebSocketContext.OnConnected;
begin
  inherited;
  FRecvShakeHand := 0;
  PostWebSocketRequest;
  FHttpBuffer.DoCleanUp;
  FWsFrame.DoCleanUp;
  FSendPingTick := GetTickCount;

end;

procedure TDiocpWebSocketContext.OnDisconnected;
begin
  inherited;
  if Assigned(FOnDisconnectedEvent) then
  begin
    FOnDisconnectedEvent(self);
  end;
end;

procedure TDiocpWebSocketContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD);
var
  lvPtr: PByte;
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
          FOnShakeHand(self);
        end;
        FHttpBuffer.DoCleanUp;
      end;
    end
    else
    begin
      r := FWsFrame.InputBuffer(lvPtr^);
      Inc(lvPtr);
      if r = 1 then
      begin
        FWebSocketContentBuffer.AppendBuffer(FWsFrame.ContentBuffer, FWsFrame.ContentLength);
        if FWsFrame.GetFIN <> FIN_EOF then
        begin
          FWsFrame.DoCleanUp();
        end
        else
        begin
          DoRecv();

          FWsFrame.DoCleanUp;
          FWebSocketContentBuffer.Clear;
        end;
      end;
    end;
  end;
end;

procedure TDiocpWebSocketContext.Open(WsUrl: string);
begin
  CheckCanConnect;
  FWsUrl := WsUrl;
  FURL.SetURL(WsUrl);
  Host := FURL.Host;
  Port := StrToInt(FURL.Port);
  Connect();
end;

procedure TDiocpWebSocketContext.PostWebSocketRequest;
var
  s: String;
  lvBytes: TBytes;
begin

  // GET ws://127.0.0.1:8081/ HTTP/1.1
  // Host: 127.0.0.1:8081
  // Connection: Upgrade
  // Pragma: no-cache
  // Cache-Control: no-cache
  // Upgrade: websocket
  // Origin: http://127.0.0.1:8081
  // Sec-WebSocket-Version: 13
  // User-Agent: Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/38.0.2125.122 Safari/537.36
  // Accept-Encoding: gzip,deflate,sdch
  // Accept-Language: zh-CN,zh;q=0.8
  // Cookie: diocp_cookie=xxxx
  // Sec-WebSocket-Key: pAwC+w4+DLzmrLTUuBG4cQ==
  // Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits

  //GET ws://127.0.0.1:16001/ws/service HTTP/1.1
  //Host: 127.0.0.1:16001
  //Connection: Upgrade
  //Pragma: no-cache
  //Cache-Control: no-cache
  //User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36
  //Upgrade: websocket
  //Origin: http://139.199.181.47:8090
  //Sec-WebSocket-Version: 13
  //Accept-Encoding: gzip, deflate, br
  //Accept-Language: zh-CN,zh;q=0.9
  //Sec-WebSocket-Key: w3MBoVLXJCFObHwstYZeRg==
  //Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits


  FHeaderBuilder.URI := FWsUrl; //FURL.URI;
  FHeaderBuilder.URLParams := FURL.ParamStr;
  // FHeaderBuilder.URI := FWsUrl;
  FHeaderBuilder.Method := 'GET';

  FHeaderBuilder.SetHeader('Pragma', 'no-cache');
  FHeaderBuilder.SetHeader('Cache-Control', 'no-cache');

  // 有些服务器会检测跨域问题, 所以需要设定
  if Length(self.FHttpOrigin) = 0 then
  begin
    if FURL.Protocol ='ws' then
    begin
      s := Format('%s://%s', ['http', self.FURL.RawHostStr]);
    end else
    begin
      s := Format('%s://%s', ['https', self.FURL.RawHostStr]);
    end;

    FHeaderBuilder.SetHeader('Origin', s);
  end else
  begin
    FHeaderBuilder.SetHeader('Origin', self.FHttpOrigin);
  end;


  FHeaderBuilder.SetHeader('Accept-Encoding', 'gzip, deflate, br');
  FHeaderBuilder.SetHeader('Accept-Language', 'zh-CN,zh;q=0.9');

  FHeaderBuilder.SetHeader('Connection', 'Upgrade');
  FHeaderBuilder.SetHeader('Upgrade', 'websocket');
  FHeaderBuilder.SetHeader('Host', FURL.RawHostStr);
  FHeaderBuilder.SetHeader('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36');
  // FHeaderBuilder.SetHeader('Origin', 'file://');
  FHeaderBuilder.SetHeader('Sec-WebSocket-Version', '13');
  FHeaderBuilder.SetHeader('Sec-WebSocket-Key', Base64Encode('Diocp' + NowString));
  FHeaderBuilder.SetHeader('Sec-WebSocket-Extensions', 'permessage-deflate; client_max_window_bits');

  s := FHeaderBuilder.Build();

  lvBytes := StringToBytes(s);

  self.PostWSASendRequest(PByte(@lvBytes[0]), Length(lvBytes));

end;

function TDiocpWebSocketContext.SendBuffer(buf: Pointer; len: Cardinal; opcode:
    Byte): Boolean;
var
  lvWSFrame: TDiocpWebSocketFrame;
begin
  lvWSFrame := TDiocpWebSocketFrame.Create;
  try
    lvWSFrame.EncodeBuffer(buf, len, true, opcode, FMasked);
    Result := PostWSASendRequest(lvWSFrame.Buffer.Memory, lvWSFrame.Buffer.Length);
  finally
    lvWSFrame.Free;
  end;

end;

procedure TDiocpWebSocketContext.SendPing;
begin
  // PostWSASendRequest(@WS_MSG_PING, 2, False);
  SendBuffer(nil, 0, OPT_PING);
end;

procedure TDiocpWebSocketContext.SendText(const s: string);
var
  lvBytes: TBytes;
begin
  lvBytes := StringToUtf8Bytes(s);
  SendBuffer(@lvBytes[0], Length(lvBytes), OPT_TEXT);
end;

constructor TDiocpWebSocketTcpClient.Create(AOwner: TComponent);
begin
  inherited;
  RegisterContextClass(TDiocpWebSocketContext);
end;

end.
