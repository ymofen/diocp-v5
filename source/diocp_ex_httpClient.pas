unit diocp_ex_httpClient;

interface

uses
  Classes
  {$IFDEF POSIX}
  , diocp_core_rawPosixSocket
  {$ELSE}
  , diocp_core_rawWinSocket
  , diocp_winapi_winsock2
  , SysConst
  {$ENDIF}
  , SysUtils, utils_URL, utils_strings, diocp_ex_http_common;




const
  HTTP_HEADER_END :array[0..3] of Byte=(13,10,13,10);

type
  //2007以上直接=TBytes
{$if CompilerVersion< 18.5}
  TBytes = array of Byte;
{$IFEND}

  TDiocpHttpClient = class(TComponent)
  private
    FCheckThreadSafe: Boolean;
    FCreatTheadID:THandle;
    FLastActivity:Cardinal;
    FLastHost:String;
    FLastPort:Integer;
    FHttpBuffer:THttpBuffer;
    FStringBuilder:TDStringBuilder;
    FRequestHeaderBuilder:TDStringBuilder;
    FCustomeHeader: TStrings;
    FKeepAlive: Boolean;
    FKeepAliveTimeOut: Cardinal;
    FURL: TURL;
    FRawSocket: TRawSocket;
    FRequestAccept: String;
    FRequestAcceptEncoding: String;
    FRawCookie:String;

    FResponseResultCode:Integer;

    FRequestBody: TMemoryStream;
    FRequestContentType: String;
    FRequestHeader: TStringList;
    FReponseBuilder: TDBufferBuilder;
    FResponseBody: TMemoryStream;
    FResponseContentType: String;
    FResponseContentEncoding:String;
    FResponseHeader: TStringList;
    FResponseSize: Integer;
    FResponseHttpVersionValue: Integer;
    FTimeOut: Integer;
    /// <summary>
    ///  CheckRecv buffer
    /// </summary>
    procedure CheckRecv(buf: Pointer; len: cardinal);
    procedure CheckSocketResult(pvSocketResult:Integer);
    procedure InnerExecuteRecvResponse();
    procedure InnerExecuteRecvResponseTimeOut;
    /// <summary>
    ///   检测是否需要关闭连接
    /// </summary>
    procedure CheckCloseConnection;

    procedure DoAfterResponse;
    function GetResponseResultCode: Integer;


    procedure ResetState;

    procedure DecodeFirstLine;

    function CheckConnect(pvHost: string; pvPort: Integer): Boolean;
  public
    procedure Cleaup;

    constructor Create(AOwner: TComponent); override;
    
    destructor Destroy; override;
    procedure Close;
    procedure Post(pvURL:String);
    procedure Get(pvURL:String);
    procedure DirectPost(pvHost: string; pvPort: Integer; pvBuf: Pointer; len:
        Cardinal);

    procedure SetRequestBodyAsString(pvRequestData: string; pvConvert2Utf8:
        Boolean);

    /// <summary>
    ///   是否检测线程安全，只能在创建线程中使用
    /// </summary>
    property CheckThreadSafe: Boolean read FCheckThreadSafe write FCheckThreadSafe;
    
    property CustomeHeader: TStrings read FCustomeHeader;

    /// <summary>
    ///   是否保持连接状态
    /// </summary>
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;

    /// <summary>
    ///   超时没有数据交互，下次请求时进行重连
    ///   KeepAlive时有效
    /// </summary>
    property KeepAliveTimeOut: Cardinal read FKeepAliveTimeOut write FKeepAliveTimeOut;

    /// <summary>
    ///   请求参数:
    ///    接受数据的编码类型
    ///    Accept-Encoding:gzip,deflate
    /// </summary>
    property RequestAcceptEncoding: String read FRequestAcceptEncoding write
        FRequestAcceptEncoding;

    /// <summary>
    ///   请求参数:
    ///    接受数据类型
    ///    Accept:text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8
    /// </summary>
    property RequestAccept: String read FRequestAccept write FRequestAccept;
        
    /// <summary>
    ///  POST请求时, 内容数据类型
    /// </summary>
    property RequestContentType: String read FRequestContentType write
        FRequestContentType;

    property RequestBody: TMemoryStream read FRequestBody;
    property RequestHeader: TStringList read FRequestHeader;
    property RequestHeaderBuilder: TDStringBuilder read FRequestHeaderBuilder;

    property ResponseBody: TMemoryStream read FResponseBody;
    property ResponseResultCode: Integer read GetResponseResultCode;

    property ResponseHeader: TStringList read FResponseHeader;
    
    
    /// <summary>
    ///   响应得到的头信息
    ///   返回的数据类型
    ///     Content-Type:image/png
    ///     Content-Type:text/html; charset=utf-8
    /// </summary>
    property ResponseContentType: String read FResponseContentType;

    /// <summary>
    ///   响应的整个数据长度
    /// </summary>
    property ResponseSize: Integer read FResponseSize;

    /// <summary>
    ///   响应的Http版本
    ///   10: HTTP/1.0
    ///   11: HTTP/1.1
    /// </summary>
    property ResponseHttpVersionValue: Integer read FResponseHttpVersionValue;




    /// <summary>
    ///   读取和发送超时, 单位ms
    /// </summary>
    property TimeOut: Integer read FTimeOut write FTimeOut;


  end;


procedure WriteStringToStream(pvStream: TStream; pvDataString: string;
    pvConvert2Utf8: Boolean = true);

function ReadStringFromStream(pvStream: TStream; pvIsUtf8Raw:Boolean): String;


implementation

{ TDiocpHttpClient }

resourcestring
  STRING_E_RECV_ZERO = '服务端主动断开关闭';
  STRING_E_TIMEOUT   = '服务端响应超时';

{$IFDEF POSIX}

{$ELSE}
// <2007版本的Windows平台使用
//   SOSError = 'System Error.  Code: %d.'+sLineBreak+'%s';
procedure RaiseLastOSErrorException(LastError: Integer);
var       // 高版本的 SOSError带3个参数
  Error: EOSError;
begin
  if LastError <> 0 then
    Error := EOSError.CreateResFmt(@SOSError, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := EOSError.CreateRes(@SUnkOSError);
  Error.ErrorCode := LastError;
  raise Error;
end;

{$ENDIF}

procedure WriteStringToStream(pvStream: TStream; pvDataString: string;
    pvConvert2Utf8: Boolean = true);
{$IFDEF UNICODE}
var
  lvRawData:TBytes;
{$ELSE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  if pvConvert2Utf8 then
  begin
    lvRawData := TEncoding.UTF8.GetBytes(pvDataString);
  end else
  begin
    lvRawData := TEncoding.Default.GetBytes(pvDataString);
  end;
  pvStream.Write(lvRawData[0], Length(lvRawData));
{$ELSE}
  if pvConvert2Utf8 then
  begin
    lvRawStr := UTF8Encode(pvDataString);
  end else
  begin
    lvRawStr := AnsiString(pvDataString);
  end;
  pvStream.WriteBuffer(PAnsiChar(lvRawStr)^, length(lvRawStr));
{$ENDIF}
end;

function ReadStringFromStream(pvStream: TStream; pvIsUtf8Raw:Boolean): String;
{$IFDEF UNICODE}
var
  lvRawData:TBytes;
{$ELSE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  SetLength(lvRawData, pvStream.Size);
  pvStream.Position := 0;
  pvStream.Read(lvRawData[0], pvStream.Size);

  if pvIsUtf8Raw then
  begin
    Result := TEncoding.UTF8.GetString(lvRawData);
  end else
  begin
    Result := TEncoding.Default.GetString(lvRawData);
  end;
{$ELSE}
  SetLength(lvRawStr, pvStream.Size);
  pvStream.Position := 0;
  pvStream.Read(PAnsiChar(lvRawStr)^, pvStream.Size);
  if pvIsUtf8Raw then
  begin
    Result := UTF8Decode(lvRawStr);
  end else
  begin
    Result := AnsiString(lvRawStr);
  end;
{$ENDIF}
end;

constructor TDiocpHttpClient.Create(AOwner: TComponent);
begin
  inherited;
  FCreatTheadID := GetCurrentThreadID;
  FCheckThreadSafe := False;
  
  FHttpBuffer:= THttpBuffer.Create;
  FReponseBuilder := FHttpBuffer.ContentBuilder;
  
  FStringBuilder := TDStringBuilder.Create;
  FRequestHeaderBuilder := TDStringBuilder.Create;
  FRawSocket := TRawSocket.Create;
  FRequestBody := TMemoryStream.Create;
  FRequestHeader := TStringList.Create;
  FCustomeHeader := TStringList.Create;
  FCustomeHeader.NameValueSeparator := ':';

  FResponseBody := TMemoryStream.Create;
  FResponseHeader := TStringList.Create;

  FTimeOut := 30000;

  FURL := TURL.Create;

{$if CompilerVersion >= 18.5}
  FRequestHeader.LineBreak := #13#10;
  FResponseHeader.LineBreak := #13#10;
{$IFEND}

  FKeepAlive := True;
  
  // 20秒
  FKeepAliveTimeOut := 20000;
end;

destructor TDiocpHttpClient.Destroy;
begin
  FRawSocket.Free;
  FResponseHeader.Free;
  FResponseBody.Free;
  FRequestHeader.Free;
  FCustomeHeader.Free;
  FRequestBody.Free;
  FURL.Free;
  FStringBuilder.Free;
  FRequestHeaderBuilder.Free;
  FHttpBuffer.Free;
  inherited;
end;

procedure TDiocpHttpClient.CheckCloseConnection;
var
  lvTempStr:String;
begin
  lvTempStr := StringsValueOfName(FResponseHeader, 'Connection', [':'], True);
  if (Length(lvTempStr) = 0) then
  begin
    if FResponseHttpVersionValue = 10 then
    begin    // 10默认关闭
      self.Close;
    end;
  end else if SameStr(lvTempStr, 'close') then
  begin
    Self.Close;
  end else if ResponseResultCode <> 200 then
  begin     // 200, OK
    self.Close;
  end else
  begin
    if not FKeepAlive then Close;
  end;
end;

procedure TDiocpHttpClient.CheckSocketResult(pvSocketResult: Integer);
var
  lvErrorCode:Integer;
begin
  if pvSocketResult = -2 then
  begin
    self.Close;
    raise Exception.Create(STRING_E_TIMEOUT);
  end;
  {$IFDEF POSIX}
  if (pvSocketResult = -1) or (pvSocketResult = 0) then
  begin
     try
       RaiseLastOSError;
     except
       FRawSocket.Close;
       raise;
     end;
   end;
  {$ELSE}
  if (pvSocketResult = SOCKET_ERROR) then
  begin
    lvErrorCode := GetLastError;
    FRawSocket.Close;     // 出现异常后断开连接

    {$if CompilerVersion < 23}
    RaiseLastOSErrorException(lvErrorCode);
    {$ELSE}
    RaiseLastOSError(lvErrorCode);
    {$ifend} 
  end;
  {$ENDIF}
end;

procedure TDiocpHttpClient.Cleaup;
begin
  FRequestBody.Clear;
  FResponseBody.Clear;
end;

procedure TDiocpHttpClient.Close;
begin
  FRawSocket.Close();
  FLastHost := '';
  FLastPort := 0;
  FLastActivity := 0;
end;

procedure TDiocpHttpClient.Get(pvURL: String);
var
  r, len:Integer;
  lvIpAddr:string;
{$IFDEF UNICODE}
  lvRawHeader:TBytes;
{$ELSE}
  lvRawHeader:AnsiString;
{$ENDIF}
begin
  ResetState;
  FURL.SetURL(pvURL);
  FRequestHeader.Clear;
  if FURL.ParamStr = '' then
  begin
    FRequestHeader.Add(Format('GET %s HTTP/1.1', [FURL.URI]));
  end else
  begin
    FRequestHeader.Add(Format('GET %s HTTP/1.1', [FURL.URI + '?' + FURL.ParamStr]));
  end;
  FRequestHeader.Add(Format('Host: %s', [FURL.RawHostStr]));
  
  if FKeepAlive then
  begin
    FRequestHeader.Add('Connection: keep-alive');
  end;
  
  if FRawCookie <> '' then
  begin
    FRequestHeader.Add('Cookie:' + FRawCookie);
  end;
  
  FRequestHeader.Add('');                 // 添加一个回车符

  if CheckConnect(FURL.Host, StrToIntDef(FURL.Port, 80)) then
  try   
    FStringBuilder.Clear;
    FStringBuilder.Append(FRequestHeader.Text);
    if FCustomeHeader.Count > 0 then
    begin
      FStringBuilder.Append(FCustomeHeader.Text);
    end;
    FStringBuilder.Append(FStringBuilder.LineBreak);
  {$IFDEF UNICODE}
    lvRawHeader := TEncoding.Default.GetBytes(FStringBuilder.ToString());
    len := Length(lvRawHeader);
    r := FRawSocket.SendBuf(PByte(lvRawHeader)^, len);
    CheckSocketResult(r);
    if r <> len then
    begin
      raise Exception.Create(Format('指定发送的数据长度:%d, 实际发送长度:%d', [len, r]));
    end;
  {$ELSE}
    lvRawHeader := FStringBuilder.ToString();
    len := Length(lvRawHeader);
    r := FRawSocket.SendBuf(PAnsiChar(lvRawHeader)^, len);
    CheckSocketResult(r);
    if r <> len then
    begin
      raise Exception.Create(Format('指定发送的数据长度:%d, 实际发送长度:%d', [len, r]));
    end;
  {$ENDIF}
  //
  //{$IFDEF UNICODE}
  //  lvRawHeader := TEncoding.Default.GetBytes(FRequestHeader.Text);
  //  len := Length(lvRawHeader);
  //  r := FRawSocket.SendBuf(PByte(lvRawHeader)^, len);
  //  CheckSocketResult(r);
  //  if r <> len then
  //  begin
  //    raise Exception.Create(Format('指定发送的数据长度:%d, 实际发送长度:%d', [len, r]));
  //  end;
  //{$ELSE}
  //  lvRawHeader := FRequestHeader.Text;
  //  len := Length(lvRawHeader);
  //  r := FRawSocket.SendBuf(PAnsiChar(lvRawHeader)^, len);
  //  CheckSocketResult(r);
  //  if r <> len then
  //  begin
  //    raise Exception.Create(Format('指定发送的数据长度:%d, 实际发送长度:%d', [len, r]));
  //  end;
  //{$ENDIF}

    InnerExecuteRecvResponse();
  finally
    CheckCloseConnection;
  end;
end;

procedure TDiocpHttpClient.InnerExecuteRecvResponse;
const
  BLOCK_SIZE = 2048;
var
  lvRawHeader, lvBytes:TBytes;
  x, l:Integer;
  lvTempStr, lvRawHeaderStr:String;
  lvBuffer:PByte;

  lvTempBuffer:array[0.. BLOCK_SIZE -1] of Byte;

  function DecodeHttp():Integer;
  var
    r, i:Integer;
  begin
    Result := 0;
    for i := 0 to l - 1 do
    begin
      r := FHttpBuffer.InputBuffer(lvTempBuffer[i]);
      Inc(FResponseSize);
      if r = 1 then
      begin
        lvRawHeaderStr := FHttpBuffer.HeaderBuilder.ToRAWString;

        FResponseHeader.Text := lvRawHeaderStr;
        FResponseContentType := StringsValueOfName(FResponseHeader, 'Content-Type', [':'], True);
        lvTempStr := StringsValueOfName(FResponseHeader, 'Content-Length', [':'], True);
        FResponseContentEncoding :=StringsValueOfName(FResponseHeader, 'Content-Encoding', [':'], True);
        l := StrToIntDef(lvTempStr, 0);
        if l = 0  then
        begin
          Result := 1;
          Break;
        end else
        begin
          FHttpBuffer.ContentLength := l;
        end;
      end else if r = -2 then               
      begin
        Close;
        raise Exception.CreateFmt('%d超过Http设定的头部大小(%d)！', [FHttpBuffer.HeaderBuilder.Size, MAX_HEADER_BUFFER_SIZE]);
      end else if r = 2 then
      begin  // 解码到消息体
        if FResponseContentEncoding = 'zlib' then
        begin
          ZDecompressBufferBuilder(FHttpBuffer.ContentBuilder);
        end
        {$if CompilerVersion>= 18}
        {$IFDEF MSWINDOWS}
        else if FResponseContentEncoding = 'gzip' then
        begin
          GZDecompressBufferBuilder(FHttpBuffer.ContentBuilder);
        end
        {$ENDIF}
        {$ifend}
        ;

        l:= FHttpBuffer.ContentBuilder.Length;


        FResponseBody.Size := l;
        Move(FHttpBuffer.ContentBuilder.Memory^, FResponseBody.Memory^, l);

        Result := 1;
        Break;
      end;
    end;
  end;

begin
  FHttpBuffer.DoCleanUp;
{$IFDEF MSWINDOWS}

  while True do
  begin
    l := FRawSocket.RecvBuf(lvTempBuffer[0], BLOCK_SIZE);
    CheckSocketResult(l);
    if l = 0 then
    begin
      // 对方被关闭
      Close;
      raise Exception.Create('与服务器断开连接！');
    end;
    x := DecodeHttp;
    if x = 1 then
    begin
      Break;
    end;
  end;
{$ELSE}
  while True do
  begin
    l := FRawSocket.RecvBuf(lvTempBuffer[0], BLOCK_SIZE, FTimeOut);
    CheckSocketResult(l);
    if l = 0 then
    begin
      // 对方被关闭
      Close;
      raise Exception.Create('与服务器断开连接！');
    end;
    x := DecodeHttp;
    if x = 1 then
    begin
      Break;
    end;
  end;
{$ENDIF}




  lvTempStr := StringsValueOfName(FResponseHeader, 'Set-Cookie', [':'], True);
  if lvTempStr <> '' then
  begin  
    FRawCookie := lvTempStr;
  end;


  FLastActivity := GetTickCount;
  DoAfterResponse;
end;

procedure TDiocpHttpClient.Post(pvURL: String);
var
  r, len:Integer;
  lvIpAddr:string;
{$IFDEF UNICODE}
  lvRawHeader:TBytes;
{$ELSE}
  lvRawHeader:AnsiString;
{$ENDIF}
begin
  ResetState;

  FURL.SetURL(pvURL);
  FRequestHeader.Clear;
  if FURL.ParamStr = '' then
  begin
    FRequestHeader.Add(Format('POST %s HTTP/1.1', [FURL.URI]));
  end else
  begin
    FRequestHeader.Add(Format('POST %s HTTP/1.1', [FURL.URI + '?' + FURL.ParamStr]));
  end;

  if FKeepAlive then
  begin
    FRequestHeader.Add('Connection: keep-alive');
  end;

  if FRawCookie <> '' then
  begin
    FRequestHeader.Add('Cookie:' + FRawCookie);
  end;

  FRequestHeader.Add(Format('Host: %s', [FURL.RawHostStr]));
  FRequestHeader.Add(Format('Content-Length: %d', [self.FRequestBody.Size]));
  if FRequestContentType = '' then
  begin
    FRequestContentType := 'application/x-www-form-urlencoded';
  end;
  FRequestHeader.Add(Format('Content-Type: %s', [FRequestContentType]));

  if FRequestAcceptEncoding <> '' then
  begin
    FRequestHeader.Add(Format('Accept-Encoding: %s', [FRequestAcceptEncoding]));
  end;

  //FRequestHeader.Add('');                 // 添加一个回车符

  if CheckConnect(FURL.Host, StrToIntDef(FURL.Port, 80)) then
  try

    FRequestHeaderBuilder.Clear;
    FRequestHeaderBuilder.Append(FRequestHeader.Text);
    if FCustomeHeader.Count > 0 then
    begin
      FRequestHeaderBuilder.Append(FCustomeHeader.Text);
    end;
    FRequestHeaderBuilder.Append(FRequestHeaderBuilder.LineBreak);
  {$IFDEF UNICODE}
    lvRawHeader := TEncoding.Default.GetBytes(FRequestHeaderBuilder.ToString());
    len := Length(lvRawHeader);
    r := FRawSocket.SendBuf(PByte(lvRawHeader)^, len);
    CheckSocketResult(r);
    if r <> len then
    begin
      raise Exception.Create(Format('指定发送的数据长度:%d, 实际发送长度:%d', [len, r]));
    end;
  {$ELSE}
    lvRawHeader := FRequestHeaderBuilder.ToString();
    len := Length(lvRawHeader);
    r := FRawSocket.SendBuf(PAnsiChar(lvRawHeader)^, len);
    CheckSocketResult(r);
    if r <> len then
    begin
      raise Exception.Create(Format('指定发送的数据长度:%d, 实际发送长度:%d', [len, r]));
    end;
  {$ENDIF}

    // 发送请求数据体
    if FRequestBody.Size > 0 then
    begin
      len := FRequestBody.Size;
      r := FRawSocket.SendBuf(FRequestBody.Memory^, len);
      CheckSocketResult(r);
      if r <> len then
      begin
        raise Exception.Create(Format('指定发送的数据长度:%d, 实际发送长度:%d', [len, r]));
      end;
    end;

    InnerExecuteRecvResponse();
  finally
    CheckCloseConnection;
  end;
end;

function TDiocpHttpClient.CheckConnect(pvHost: string; pvPort: Integer):
    Boolean;
var
  lvReConnect:Boolean;
  lvIpAddr:string;
begin
  if (GetCurrentThreadID <> FCreatTheadID) and (FCheckThreadSafe) then
  begin
    raise Exception.Create('TDiocpHttpClient::只能在创建线程中发起Http请求(CheckThreadSafe is true)');
  end;
  
  lvReConnect := (pvHost <> FLastHost) or (pvPort <> FLastPort);
  lvReConnect := lvReConnect or (tick_diff(FLastActivity, GetTickCount) > FKeepAliveTimeOut);


  if lvReConnect then
  begin
    FRawSocket.CreateTcpSocket;
    FRawSocket.DoInitialize();

    {$IFDEF MSWINDOWS}
    if FTimeOut > 0 then
    begin
      FRawSocket.SetReadTimeOut(FTimeOut);
      FRawSocket.SetSendTimeOut(FTimeOut);
    end;
    {$ENDIF}

    // 进行域名解析
    lvIpAddr := FRawSocket.GetIpAddrByName(pvHost);
  
    if not FRawSocket.Connect(lvIpAddr, pvPort) then
    begin
      RaiseLastOSError;
    end;
  end;

  FLastHost := pvHost;
  FLastPort := pvPort;
  FLastActivity := GetTickCount;

  Result := True;

end;

procedure TDiocpHttpClient.CheckRecv(buf: Pointer; len: cardinal);
var
  lvTempL :Integer;
  lvReadL :Cardinal;
  lvPBuf:Pointer;
begin
  lvReadL := 0;
  lvPBuf := buf;
  while lvReadL < len do
  begin
    lvTempL := FRawSocket.RecvBuf(lvPBuf^, len - lvReadL);
    if lvTempL = 0 then
    begin
      self.Close;
      raise Exception.Create('与服务器断开连接！');
    end;
    CheckSocketResult(lvTempL);

    lvPBuf := Pointer(IntPtr(lvPBuf) + Cardinal(lvTempL));
    lvReadL := lvReadL + Cardinal(lvTempL);
  end;
end;

procedure TDiocpHttpClient.DecodeFirstLine;
var
  lvLine, lvCode:String;
  lvPtr:PChar;
begin
  if FResponseHeader.Count = 0 then
  begin
    FResponseResultCode := -1;
    exit;
  end;
  // HTTP/1.1 200 OK
  lvLine := FResponseHeader[0];
  lvPtr := PChar(lvLine);

  lvCode := LeftUntil(lvPtr, ['/']);
  if lvCode <> 'HTTP' then
  begin
    FResponseResultCode := -1;
    Exit;
  end;
  SkipChars(lvPtr, [' ', '/']);

  lvCode := LeftUntil(lvPtr, [' ']);
  if lvCode = '1.0' then
  begin
    self.FResponseHttpVersionValue := 10;
  end else if lvCode = '1.1' then
  begin
    self.FResponseHttpVersionValue := 11;
  end else
  begin
    Self.FResponseHttpVersionValue := 11;
  end;



  SkipUntil(lvPtr, [' ']);
  SkipChars(lvPtr, [' ']);
  lvCode := LeftUntil(lvPtr, [' ']);
  FResponseResultCode := StrToIntDef(lvCode, -1);
end;

procedure TDiocpHttpClient.DirectPost(pvHost: string; pvPort: Integer; pvBuf:
    Pointer; len: Cardinal);
var
  r:Integer;
begin
  ResetState;
  if not CheckConnect(pvHost, pvPort) then Exit;
  try
    r := FRawSocket.SendBuf(pvBuf^, len);
    CheckSocketResult(r);
    if r <> len then
    begin
      raise Exception.Create(Format('指定发送的数据长度:%d, 实际发送长度:%d', [len, r]));
    end;

    InnerExecuteRecvResponse();


  finally
    CheckCloseConnection;
  end;

end;

procedure TDiocpHttpClient.DoAfterResponse;
var
  lvCode:Integer;
begin
  lvCode := ResponseResultCode;
  if lvCode = 200 then
  begin
    ; // OK
  end else if lvCode= -1 then
  begin
    raise Exception.Create(Format('错误的ResponseHttpCode[%d]', [lvCode]));
  end else
  begin
    raise Exception.Create(Format('错误的ResponseHttpCode[%d]: %s', [lvCode, GetResponseCodeText(lvCode)]));
  end;
end;

function TDiocpHttpClient.GetResponseResultCode: Integer;
var
  lvLine, lvCode:String;
  lvPtr:PChar;
begin
  if FResponseResultCode = 0 then
  begin
    DecodeFirstLine();

  end;
  Result := FResponseResultCode;
end;

procedure TDiocpHttpClient.InnerExecuteRecvResponseTimeOut;
var
  lvRawHeader, lvBytes:TBytes;
  r, l:Integer;
  lvTempStr, lvRawHeaderStr:String;
  lvBuffer:PByte;
begin
  FHttpBuffer.DoCleanUp;

  FReponseBuilder.Clear;
  // 超过2048以外的长度，认为是错误的
  SetLength(lvRawHeader, 2048);
  FillChar(lvRawHeader[0], 2048, 0);
  r := FRawSocket.RecvBufEnd(@lvRawHeader[0], 2048, @HTTP_HEADER_END[0], 4, FTimeOut);
  if r = 0 then
  begin
    // 对方被关闭
    Close;
    raise Exception.Create('与服务器断开连接！');
  end;
  // 检测是否有错误
  CheckSocketResult(r);

  Inc(FResponseSize, r);
  
  {$IFDEF UNICODE}
  lvRawHeaderStr := TEncoding.Default.GetString(lvRawHeader);
  {$ELSE}
  lvRawHeaderStr := StrPas(@lvRawHeader[0]);
  {$ENDIF}

  FResponseHeader.Text := lvRawHeaderStr;
  FResponseContentType := StringsValueOfName(FResponseHeader, 'Content-Type', [':'], True);
  lvTempStr := StringsValueOfName(FResponseHeader, 'Content-Length', [':'], True);
  FResponseContentEncoding :=StringsValueOfName(FResponseHeader, 'Content-Encoding', [':'], True);
  l := StrToIntDef(lvTempStr, 0);
  if l > 0 then
  begin
    lvBuffer := FReponseBuilder.GetLockBuffer(l);
    try
      CheckRecv(lvBuffer, l);
    finally
      Inc(FResponseSize, l);
      FReponseBuilder.ReleaseLockBuffer(l);
    end;

    if FResponseContentEncoding = 'zlib' then
    begin
      ZDecompressBufferBuilder(FReponseBuilder);
    end
    {$if CompilerVersion>= 18}
    {$IFDEF MSWINDOWS}
    else if FResponseContentEncoding = 'gzip' then
    begin
      GZDecompressBufferBuilder(FReponseBuilder);
    end
    {$ENDIF}
    {$ifend}
    ;

    l:= FReponseBuilder.Length;


    FResponseBody.Size := l;
    Move(FReponseBuilder.Memory^, FResponseBody.Memory^, l); 
  end;

  lvTempStr := StringsValueOfName(FResponseHeader, 'Set-Cookie', [':'], True);

  if lvTempStr <> '' then
  begin  
    FRawCookie := lvTempStr;
  end;


  DoAfterResponse; 
end;

procedure TDiocpHttpClient.ResetState;
begin
  FResponseResultCode := 0;
  FResponseSize := 0;
  FResponseHttpVersionValue := 0;
  FResponseHeader.Clear;
end;

procedure TDiocpHttpClient.SetRequestBodyAsString(pvRequestData: string;
    pvConvert2Utf8: Boolean);
begin
  FRequestBody.Clear;
  WriteStringToStream(FRequestBody, pvRequestData, pvConvert2Utf8);

end;

end.
