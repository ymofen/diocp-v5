unit diocp_ex_httpClient;
{*
 *
 * cleanup()   ������� ������Cookie
 * reset()     �������������cookie
*}

interface

{$I 'diocp.inc'}

{.$DEFINE DIOCP_SSL}

uses
  Classes
  {$IFDEF POSIX}
  , diocp_core_rawPosixSocket
  {$ELSE}
  , diocp_core_rawWinSocket
  , diocp_winapi_winsock2
  , SysConst
  {$IFDEF DIOCP_SSL}
  , utils_openssl
  {$ENDIF}
  {$ENDIF}

  , SysUtils, utils_URL, utils_strings, diocp_ex_http_common, utils_BufferPool;




const
  HTTP_HEADER_END :array[0..3] of Byte=(13,10,13,10);

type
  //2007����ֱ��=TBytes
{$if CompilerVersion< 18.5}
  TBytes = array of Byte;
{$IFEND}

  EDiocpHttpClient = class(Exception);

  TDiocpSocketSendException = class(Exception)
    ErrorCode:Integer;
  end;

  TDiocpSocketRecvException = class(Exception)
    ErrorCode:Integer;
  end;

  TDiocpSocketHttpException = class(Exception)
    ErrorCode:Integer;
  end;

  TDiocpHttpClient = class(TComponent)
  private
    FReConnectCounter:Integer;
    FBufferWriter:TBlockBuffer;
    FCheckThreadSafe: Boolean;
    FCreatTheadID:THandle;
    FLastResponseTick:Cardinal;
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
    FResponseCookie:String;
    FResponseHeader: TStringList;
    FResponseSize: Integer;
    FResponseHttpVersionValue: Integer;
    FPostSize:Integer;
    FTimeOut: Integer;
    
    FConnectTimeOut:Integer;



    /// <summary>
    ///  CheckRecv buffer
    /// </summary>
    procedure CheckRecv(buf: Pointer; len: cardinal);
    procedure CheckSocketRecvResult(pvSocketResult:Integer);
    procedure InnerExecuteRecvResponse();
    procedure InnerExecuteRecvResponseTimeOut;
    /// <summary>
    ///   ����Ƿ���Ҫ�ر�����
    /// </summary>
    procedure CheckCloseConnection;

    procedure DoAfterResponse;
    function GetResponseResultCode: Integer;


    procedure ResetState;

    procedure DecodeFirstLine;

    function CheckConnect(const pvHost: string; pvPort: Integer): Boolean;
    procedure CheckSocketSendResult(pvSocketResult:Integer);
    function GetActive: Boolean;
    procedure OnBufferWrite(pvSender: TObject; pvBuffer: Pointer; pvLength:
        Integer);
  private
    FHostIpV6: Boolean;
    FLastRequestHeader: String;
    FRaiseOnResponseOnExceptCode: Boolean;
    {$IFDEF DIOCP_SSL}
    FSslFlag:Integer;
    FSSLCtx: utils_openssl.PSSL_CTX;
    FSsl: utils_openssl.PSSL;
    procedure DoSSLAfterConnect;
    {$ENDIF}

    function DoRecvBuff(var data; const len: Cardinal): Integer;
  public
    /// <summary>
    ///   ������Cookie
    /// </summary>
    procedure Cleanup;

    /// <summary>
    ///   ��λ
    ///   ����Cookie
    ///   ��λ�趨
    /// </summary>
    procedure Reset();

    {$IFDEF DIOCP_SSL}
    procedure DoTestGetSSL(const url:string);

    {$ENDIF}

    constructor Create(AOwner: TComponent); override;
    
    destructor Destroy; override;
    procedure Close;
    procedure Post(pvURL:String);
    procedure Get(pvURL:String);
    procedure DirectPost(pvHost: string; pvPort: Integer; pvBuf: Pointer; len:
        Cardinal);

    procedure SetRequestBodyAsString(pvRequestData: string; pvConvert2Utf8:
        Boolean);

    function DecodeUtf8AsString: string;
    function GetResponseBodyAsString: string;

    property Active: Boolean read GetActive;
    /// <summary>
    ///   �Ƿ����̰߳�ȫ��ֻ���ڴ����߳���ʹ��
    /// </summary>
    property CheckThreadSafe: Boolean read FCheckThreadSafe write FCheckThreadSafe;
    property ConnectTimeOut: Integer read FConnectTimeOut write FConnectTimeOut;
    
    property CustomeHeader: TStrings read FCustomeHeader;
    property HostIpV6: Boolean read FHostIpV6 write FHostIpV6;



    /// <summary>
    ///   �Ƿ񱣳�����״̬
    /// </summary>
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;

    /// <summary>
    ///   ��ʱû�����ݽ������´�����ʱ��������
    ///   KeepAliveʱ��Ч
    /// </summary>
    property KeepAliveTimeOut: Cardinal read FKeepAliveTimeOut write FKeepAliveTimeOut;
    property LastActivity: Cardinal read FLastActivity;
    property LastRequestHeader: String read FLastRequestHeader write
        FLastRequestHeader;
    property PostSize: Integer read FPostSize;

    property RaiseOnResponseOnExceptCode: Boolean read FRaiseOnResponseOnExceptCode
        write FRaiseOnResponseOnExceptCode;
    property ReConnectCounter: Integer read FReConnectCounter;

    /// <summary>
    ///   �������:
    ///    �������ݵı�������
    ///    Accept-Encoding:gzip,deflate
    /// </summary>
    property RequestAcceptEncoding: String read FRequestAcceptEncoding write
        FRequestAcceptEncoding;

    /// <summary>
    ///   �������:
    ///    ������������
    ///    Accept:text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8
    /// </summary>
    property RequestAccept: String read FRequestAccept write FRequestAccept;

    /// <summary>
    ///  POST����ʱ, ������������
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
    ///   ��Ӧ�õ���ͷ��Ϣ
    ///   ���ص���������
    ///     Content-Type:image/png
    ///     Content-Type:text/html; charset=utf-8
    /// </summary>
    property ResponseContentType: String read FResponseContentType;

    /// <summary>
    ///   ��Ӧ���������ݳ���
    /// </summary>
    property ResponseSize: Integer read FResponseSize;

    /// <summary>
    ///   ��Ӧ��Http�汾
    ///   10: HTTP/1.0
    ///   11: HTTP/1.1
    /// </summary>
    property ResponseHttpVersionValue: Integer read FResponseHttpVersionValue;






    /// <summary>
    ///   ��ȡ�ͷ��ͳ�ʱ, ��λms
    /// </summary>
    property TimeOut: Integer read FTimeOut write FTimeOut;





  end;


procedure WriteStringToStream(pvStream: TStream; pvDataString: string;
    pvConvert2Utf8: Boolean = true);

function ReadStringFromStream(pvStream: TStream; pvIsUtf8Raw:Boolean): String;


implementation

{ TDiocpHttpClient }
const
{$IFDEF DEBUG}
  BLOCK_SIZE = 256;
{$ELSE}
  BLOCK_SIZE = 1024 * 50;
{$ENDIF}

resourcestring
  STRING_E_RECV_ZERO = '����������Ͽ��ر�';
  STRING_E_TIMEOUT   = '�������Ӧ��ʱ';
  STRING_E_CONNECT_TIMEOUT = '�������(%s:%d)�������ӳ�ʱ';
  STRING_E_OSException = 'System Error.  Code: %d.'+sLineBreak+'%s';

  {$IFDEF POSIX}
  SUnkOSError = 'A call to an OS function failed';
  {$ENDIF}

var
  //__trace_id: Integer;
  __root: string;
  __writeBufferPool: PBufferPool;

procedure RaiseLastSendException(LastError: Integer);
var       // �߰汾�� SOSError��3������
  Error: TDiocpSocketSendException;
begin
  if LastError <> 0 then
    Error := TDiocpSocketSendException.CreateResFmt(@STRING_E_OSException, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := TDiocpSocketSendException.CreateRes(@SUnkOSError);
  Error.ErrorCode := LastError;
  raise Error;
end;

procedure RaiseLastRecvException(LastError: Integer);
var       // �߰汾�� SOSError��3������
  Error: TDiocpSocketRecvException;
begin
  if LastError <> 0 then
    Error := TDiocpSocketRecvException.CreateResFmt(@STRING_E_OSException, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := TDiocpSocketRecvException.CreateRes(@SUnkOSError);
  Error.ErrorCode := LastError;
  raise Error;
end;


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
  FRaiseOnResponseOnExceptCode := true;
  FBufferWriter := TBlockBuffer.Create(__writeBufferPool);
  FBufferWriter.OnBufferWrite := OnBufferWrite;
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
  {$if CompilerVersion < 15}  // <D7
  
  {$else}
  FCustomeHeader.NameValueSeparator := ':';
  {$ifend}


  FResponseBody := TMemoryStream.Create;
  FResponseHeader := TStringList.Create;

  FTimeOut := 30000;
  FConnectTimeOut := 10000;

  FURL := TURL.Create;

{$if CompilerVersion >= 18.5}
  FRequestHeader.LineBreak := #13#10;
  FResponseHeader.LineBreak := #13#10;
{$IFEND}

  FKeepAlive := True;
  
  // 20��
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
  FBufferWriter.Free;
  inherited;
end;

procedure TDiocpHttpClient.CheckCloseConnection;
var
  lvTempStr:String;
begin
  if not FKeepAlive then
  begin
    self.Close;
    Exit;
  end;
  
  lvTempStr := StringsValueOfName(FResponseHeader, 'Connection', [':'], True);
  if (Length(lvTempStr) = 0) then
  begin
    if FResponseHttpVersionValue = 10 then
    begin    // 10Ĭ�Ϲر�
      self.Close;
    end;
  end else if SameText(lvTempStr, 'close') then
  begin
    Self.Close;
  end else if ResponseResultCode <> 200 then
  begin     // 200, OK
    self.Close;
  end;
end;

procedure TDiocpHttpClient.CheckSocketRecvResult(pvSocketResult:Integer);
var
  lvErrorCode:Integer;
begin
  if pvSocketResult = -2 then
  begin
    self.Close;
    raise TDiocpSocketRecvException.Create(STRING_E_TIMEOUT);
  end;
  {$IFDEF POSIX}
  if (pvSocketResult = -1) or (pvSocketResult = 0) then
  begin
     lvErrorCode := GetLastError;
     FRawSocket.Close;
     RaiseLastRecvException(lvErrorCode);
   end;
  {$ELSE}
  if (pvSocketResult = SOCKET_ERROR) then
  begin
    lvErrorCode := GetLastError;
    FRawSocket.Close;     // �����쳣��Ͽ�����
    RaiseLastRecvException(lvErrorCode);
  end;
  {$ENDIF}
end;

procedure TDiocpHttpClient.Cleanup;
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


  

  if CheckConnect(FURL.Host, StrToIntDef(FURL.Port, 80)) then
  try
    FBufferWriter.ClearBuffer;
    FStringBuilder.Clear;
    FStringBuilder.Append(FRequestHeader.Text);
    if FCustomeHeader.Count > 0 then
    begin
      FStringBuilder.Append(FCustomeHeader.Text);
    end;
    FStringBuilder.Append(FStringBuilder.LineBreak);   // ������һ���س���
  {$IFDEF UNICODE}
    FLastRequestHeader := FStringBuilder.ToString();
    lvRawHeader := TEncoding.Default.GetBytes(FLastRequestHeader);
    len := Length(lvRawHeader);
    FBufferWriter.Append(@lvRawHeader[0], len);
  {$ELSE}
    FLastRequestHeader := FStringBuilder.ToString();
    lvRawHeader := FLastRequestHeader;
    len := Length(lvRawHeader);
    FBufferWriter.Append(PAnsiChar(lvRawHeader), len);
  {$ENDIF}
    FBufferWriter.FlushBuffer;
    InnerExecuteRecvResponse();
  finally
    CheckCloseConnection;
  end;
end;

procedure TDiocpHttpClient.InnerExecuteRecvResponse;
var
  lvRawHeader, lvBytes:TBytes;
  x, nBufLength:Integer;
  lvTempStr, lvRawHeaderStr, lvCookie, lvTransferEncoding:String;
  lvBuffer:PByte;
  lvIsChunked:Boolean;

  lvTempBuffer:array[0.. BLOCK_SIZE -1] of Byte;

  function DecodeHttp():Integer;
  var
    r, n, l:Integer;
    vByte:byte;
  begin
    Result := 0;
    n := 0;
    for n := 0 to nBufLength - 1 do
    begin
      vByte := lvTempBuffer[n];
//      if n = 0 then
//      begin
//        vByte := Integer(vByte) + 1 -1;
//      end;
      r := FHttpBuffer.InputBuffer(vByte);
      Inc(FResponseSize);
      if r = 1 then
      begin
        lvRawHeaderStr := FHttpBuffer.HeaderBuilder.ToRAWString;

        FResponseHeader.Text := lvRawHeaderStr;
        FResponseContentType := StringsValueOfName(FResponseHeader, 'Content-Type', [':'], True);
        lvCookie := StringsValueOfName(FResponseHeader, 'Set-Cookie', [':'], True);
        if Length(lvCookie) > 0 then
        begin
          lvCookie :=Trim(StringReplace(lvCookie, 'Path=/;', '', [rfReplaceAll, rfIgnoreCase]));
          lvCookie :=Trim(StringReplace(lvCookie, 'HttpOnly', '', [rfReplaceAll, rfIgnoreCase]));
          FResponseCookie := lvCookie;
        end;
        FResponseContentEncoding :=StringsValueOfName(FResponseHeader, 'Content-Encoding', [':'], True);

        lvTempStr := StringsValueOfName(FResponseHeader, 'Content-Length', [':'], True);
        lvTransferEncoding := StringsValueOfName(FResponseHeader, 'Transfer-Encoding', [':'], True);

        FHttpBuffer.IsChunked := lvTransferEncoding = 'chunked';
        if FHttpBuffer.IsChunked then
        begin
          FHttpBuffer.SetChunkedBegin;     // ����Chunked����
        end else
        begin
          if Length(lvTempStr) = 0 then l := 0 else l := StrToIntDef(lvTempStr, 0);
          if l = 0  then
          begin
            Result := 1;
            Break;
          end else
          begin
            FHttpBuffer.ContentLength := l;
          end;
        end;
      end else if r = -2 then               
      begin
        Close;
        raise Exception.CreateFmt('%d����Http�趨��ͷ����С(%d)��', [FHttpBuffer.HeaderBuilder.Size, MAX_HEADER_BUFFER_SIZE]);
      end else if r = 2 then
      begin  // ���뵽��Ϣ��
        if FResponseContentEncoding = 'zlib' then
        begin
          ZDecompressBufferBuilder(FHttpBuffer.ContentBuilder);
        end
        {$IFDEF USE_Z_LZO}
        else if FResponseContentEncoding = 'lzo' then
        begin
          LZODecompressBufferBuilder(FHttpBuffer.ContentBuilder);
        end
        {$ENDIF}
        {$IFDEF USE_ZLIBExGZ}
        else if FResponseContentEncoding = 'gzip' then
        begin
          GZDecompressBufferBuilder(FHttpBuffer.ContentBuilder);
        end
        {$ENDIF}
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

  while True do
  begin
    nBufLength := DoRecvBuff(lvTempBuffer[0], BLOCK_SIZE);
    //nBufLength := FRawSocket.RecvBuf(lvTempBuffer[0], BLOCK_SIZE);
    CheckSocketRecvResult(nBufLength);
    if nBufLength = 0 then
    begin
      // �Է����ر�
      Close;
      raise TDiocpSocketRecvException.Create('��������Ͽ����ӣ�');
    end;
    x := DecodeHttp;
    if x = 1 then
    begin
      Break;
    end;
  end;



  FLastActivity := GetTickCount;
  FLastResponseTick := GetTickCount;
  DoAfterResponse;
end;

procedure TDiocpHttpClient.Post(pvURL: String);
var
  r, len:Integer;
  lvIpAddr, lvCookie:string;
{$IFDEF UNICODE}
  lvRawHeader:TBytes;
{$ELSE}
  lvRawHeader:AnsiString;
{$ENDIF}
begin
  ResetState;

  FURL.SetURL(pvURL);
  FRequestHeader.Clear;
  if Length(FURL.ParamStr)=0 then
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

  lvCookie := self.FResponseCookie;
  if Length(FRawCookie) > 0 then
  begin
    lvCookie := lvCookie + FRawCookie;
  end;
  
  if Length(lvCookie) > 0 then
  begin
    FRequestHeader.Add('Cookie:' + lvCookie);
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

  if FRequestAccept <> '' then
  begin
    FRequestHeader.Add(Format('Accept: %s', [FRequestAccept]));
  end;  

  if CheckConnect(FURL.Host, StrToIntDef(FURL.Port, 80)) then
  try
    FBufferWriter.ClearBuffer;

    FRequestHeaderBuilder.Clear;
    FRequestHeaderBuilder.Append(FRequestHeader.Text);
    if FCustomeHeader.Count > 0 then
    begin
      FRequestHeaderBuilder.Append(FCustomeHeader.Text);
    end;
    FRequestHeaderBuilder.Append(FRequestHeaderBuilder.LineBreak);  // ������һ���س���
  {$IFDEF UNICODE}
    lvRawHeader := TEncoding.Default.GetBytes(FRequestHeaderBuilder.ToString());
    len := Length(lvRawHeader);
    FBufferWriter.Append(PByte(@lvRawHeader[0]), len);

  {$ELSE}
    lvRawHeader := FRequestHeaderBuilder.ToString();
    len := Length(lvRawHeader);
    FBufferWriter.Append(PAnsiChar(lvRawHeader), len);
    {$IFDEF TRACE_LOG}
    WriteStringToUtf8NoBOMFile('request.dat', lvRawHeader);
    {$ENDIF}
  {$ENDIF}



    // ��������������
    if FRequestBody.Size > 0 then
    begin
      len := FRequestBody.Size;
      FBufferWriter.Append(FRequestBody.Memory, len);
      {$IFDEF TRACE_LOG}
      FRequestBody.SaveToFile('request.body.dat');
      {$ENDIF}
    end;

    FBufferWriter.FlushBuffer;

    InnerExecuteRecvResponse();
  finally
    CheckCloseConnection;
  end;
end;

function TDiocpHttpClient.CheckConnect(const pvHost: string; pvPort: Integer):
    Boolean;
var
  lvReConnect:Boolean;
  lvIpAddr:string;
begin
  if (GetCurrentThreadID <> FCreatTheadID) and (FCheckThreadSafe) then
  begin
    raise Exception.Create('TDiocpHttpClient::ֻ���ڴ����߳��з���Http����(CheckThreadSafe is true)');
  end;
  
  lvReConnect := (pvHost <> FLastHost) or (pvPort <> FLastPort);
  lvReConnect := lvReConnect or (tick_diff(FLastActivity, GetTickCount) > FKeepAliveTimeOut);
  lvReConnect := lvReConnect or (not FRawSocket.SocketValid);

  if lvReConnect then
  begin
    if utils_strings.PosStr(':', pvHost) > 0 then
    begin
      lvIpAddr := pvHost;
      FRawSocket.IPVersion := IP_V6;
    end else
    begin
      if FURL.IPVersion = IP_V6 then
      begin
        FRawSocket.IPVersion := IP_V6;
        lvIpAddr := FRawSocket.GetIpAddrByName(pvHost);
      end else
      begin
        // ������������
        lvIpAddr := FRawSocket.GetIpAddrByName(pvHost);
        if utils_strings.PosStr(':', lvIpAddr) > 0 then
        begin
          FRawSocket.IPVersion := IP_V6;
        end else
        begin
          FRawSocket.IPVersion := IP_V4;
        end;
      end;

    end;

    FRawSocket.CreateTcpSocket;
    FRawSocket.DoInitialize();

    {$IFDEF DIOCP_SSL}
    self.FSslFlag := 0;
    {$ENDIF}

    if SameText(FURL.Protocol, 'https') then
    begin
      {$IFDEF DIOCP_SSL}
      if not ssl_isready then
      begin
        raise EDiocpHttpClient.Create('SSL����ʧ��,����SSL�ļ��Ƿ����!');
      end;
      {$ENDIF}
    end;

    {$IFDEF MSWINDOWS}
    if FTimeOut > 0 then
    begin
      FRawSocket.SetReadTimeOut(FTimeOut);
      FRawSocket.SetSendTimeOut(FTimeOut);
    end;
    FRawSocket.SetNoDelayOption(True);
    {$ELSE}

    {$ENDIF}

    try
      {$IFDEF MSWINDOWS}
//      if SameText(FURL.Protocol, 'https') then
//      begin
//        if not FRawSocket.Connect(lvIpAddr, pvPort) then
//        begin
//          raise EDiocpHttpClient.Create(Format(STRING_E_CONNECT_TIMEOUT, [pvHost, pvPort]));
//        end;
//      end else
//      begin
        if FRawSocket.IPVersion = IP_V6 then
        begin
          if not FRawSocket.ConnectV6(lvIpAddr, pvPort) then
          begin
            RaiseLastOSError;
          end;
        end else
        begin
          if not FRawSocket.ConnectTimeOut(lvIpAddr, pvPort, FConnectTimeOut) then
          begin
            raise EDiocpHttpClient.Create(Format(STRING_E_CONNECT_TIMEOUT, [pvHost, pvPort]));
          end;
        end;
      //end;

      {$ELSE}
      if not FRawSocket.Connect(lvIpAddr, pvPort) then
      begin
        RaiseLastOSError;
      end;
      {$ENDIF}

      if SameText(FURL.Protocol, 'https') then
      begin
        {$IFDEF DIOCP_SSL}
        self.FSslFlag := 1;
        Self.DoSSLAfterConnect;
        {$ENDIF}
      end;
    except
      on e:exception do
      begin
        raise Exception.Create(Format('���ӷ�����(%s:%d)�쳣:%s', [lvIpAddr, pvPort, e.Message]));
      end;  
    end;

    Inc(FReConnectCounter);
  end;

  FLastHost := pvHost;
  FLastPort := pvPort;
  FLastActivity := GetTickCount;

  Result := True;

end;

{$IFDEF DIOCP_SSL}
procedure TDiocpHttpClient.DoSSLAfterConnect;
var
  lvMethod:PSSL_METHOD;
  lvRet:cInt;
begin
  if FSSLCtx = nil then
  begin
    lvMethod := utils_openssl.SSLv23_client_method();
    if lvMethod = nil then
    begin
      raise Exception.Create('����SSL Client���õ�methodʧ��!');
    end;

    FSSLCtx := utils_openssl.SSL_CTX_new(lvMethod); //��ʼ���������龰
    if FSSLCtx = nil then
    begin
      raise Exception.Create('�����ͻ���SSL_CTXʧ��!');
    end;

    // ����
    SSL_CTX_set_cipher_list(FSslCtx, SSL_CipherList);
  end;

  if FSsl <> nil then
  begin
    SSL_free(FSsl);
    FSsl := nil;
  end;

  FSsl := utils_openssl.SSL_new(FSSLCtx); //����SSL�Ự�Ļ���,��������ǰ����������� SSLͨѶ��ʽ�����ص�ǰ��SSL ���ӻ�����ָ�롣
  if FSsl = nil then
  begin
    raise Exception.Create('����SSL�Ự�Ļ���ʧ��!');
  end;
 
  utils_openssl.SSL_set_fd(FSsl, FRawSocket.SocketHandle); //�󶨶�д�׽���
  lvRet := utils_openssl.SSL_connect(FSsl);

  if lvRet = -1 then
  begin
    lvRet := SSL_get_error(FSsl, lvRet);
    if SSL_IsFatalErr(lvRet) then
    begin
      raise EDiocpHttpClient.Create(Format('SSL SSL_connect �쳣:%d', [lvRet]));
    end;
  end;

end;


procedure TDiocpHttpClient.DoTestGetSSL(const url: string);
var
  Host: string;
  Port: Word;
  
  ctxClient: utils_openssl.PSSL_CTX; //SSL������
  methClient: utils_openssl.PSSL_METHOD;
  sRemoteIP: string;

  sslClient: utils_openssl.PSSL;
  pStr: Pchar;
  buf: array[0..4095] of Char;
  nRet: integer;
  strSend: string;
begin

  FURL.SetURL(url);


  sRemoteIP := GetIpAddrByName(FURL.Host);
  if sRemoteIP = '' then
  begin
    raise Exception.Create('��ȡԶ��IP��ַʧ��!');

  end;
 

  FRawSocket.CreateTcpSocket;
  FRawSocket.DoInitialize;
  FRawSocket.Connect(sRemoteIP, Port);

  methClient := utils_openssl.SSLv23_client_method();
  if methClient = nil then
  begin
    raise Exception.Create('����SSL Client���õ�methodʧ��!');
  end;
 
  ctxClient := utils_openssl.SSL_CTX_new(methClient); //��ʼ���������龰
  if ctxClient = nil then
  begin
    raise Exception.Create('�����ͻ���SSL_CTXʧ��!');
  end;

      // ����
  SSL_CTX_set_cipher_list(ctxClient, SSL_CipherList);
 
  sslClient := utils_openssl.SSL_new(ctxClient); //����SSL�Ự�Ļ���,��������ǰ����������� SSLͨѶ��ʽ�����ص�ǰ��SSL ���ӻ�����ָ�롣
  if sslClient = nil then
  begin
    FRawSocket.Close(False);
    raise Exception.Create('����SSL�Ự�Ļ���ʧ��!');

  end;

  utils_openssl.SSL_set_fd(sslClient, FRawSocket.SocketHandle); //�󶨶�д�׽���
  if utils_openssl.SSL_connect(sslClient) = -1 then
  begin
    raise Exception.Create('err connect');
  end;

end;

{$ENDIF}

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
      raise Exception.Create('��������Ͽ����ӣ�');
    end;
    CheckSocketRecvResult(lvTempL);

    lvPBuf := Pointer(IntPtr(lvPBuf) + IntPtr(lvTempL));
    lvReadL := lvReadL + Cardinal(lvTempL);
  end;
end;

procedure TDiocpHttpClient.CheckSocketSendResult(pvSocketResult:Integer);
var
  lvErrorCode:Integer;
begin
  if pvSocketResult = -2 then
  begin
    self.Close;
    raise TDiocpSocketSendException.Create(STRING_E_TIMEOUT);
  end;
  {$IFDEF POSIX}
  if (pvSocketResult = -1) or (pvSocketResult = 0) then
  begin
     lvErrorCode := GetLastError;
     FRawSocket.Close;
     RaiseLastSendException(lvErrorCode);
   end;
  {$ELSE}
  if (pvSocketResult = SOCKET_ERROR) then
  begin
    lvErrorCode := GetLastError;
    FRawSocket.Close;     // �����쳣��Ͽ�����
    RaiseLastSendException(lvErrorCode);
  end;
  {$ENDIF}
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
    CheckSocketRecvResult(r);
    if r <> Integer(len) then
    begin
      raise Exception.Create(Format('ָ�����͵����ݳ���:%d, ʵ�ʷ��ͳ���:%d', [len, r]));
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
  {$IFDEF TRACE_LOG}
  FHttpBuffer.HeaderBuilder.SaveToFile(__app_root + 'response.header.dat');
  FHttpBuffer.ContentBuilder.SaveToFile(__app_root + 'response.dat');
  {$ENDIF}
  if FRaiseOnResponseOnExceptCode then
  begin
    lvCode := ResponseResultCode;
    if lvCode = 200 then
    begin
      ; // OK
    end else if lvCode= -1 then
    begin
      raise Exception.Create(Format('�����ResponseHttpCode[%d]', [lvCode]));
    end else
    begin

      raise Exception.Create(Format('�����ResponseHttpCode[%d]: %s', [lvCode, GetResponseCodeText(lvCode)]));
    end;
  end;
end;

function TDiocpHttpClient.DoRecvBuff(var data; const len: Cardinal): Integer;
begin
  {$IFDEF DIOCP_SSL}
  if (FSsl <> nil) and (Self.FSslFlag = 1) then
  begin
    result := utils_openssl.SSL_read(FSsl, @data, len);
  end else
  {$ENDIF}
  begin
    result := FRawSocket.RecvBuf(data, len);
  end;
end;

function TDiocpHttpClient.GetActive: Boolean;
begin
  Result := FRawSocket.SocketValid;
end;

function TDiocpHttpClient.DecodeUtf8AsString: string;
begin
  Result := ReadStringFromStream(FResponseBody, True);
end;

function TDiocpHttpClient.GetResponseBodyAsString: string;
begin
  if StrStrIgnoreCase(PChar(FResponseContentType), 'utf-8') <> nil then
  begin
    Result := ReadStringFromStream(FResponseBody, True);
  end else
  begin
    Result := ReadStringFromStream(FResponseBody, False);
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
  // ����2048����ĳ��ȣ���Ϊ�Ǵ����
  SetLength(lvRawHeader, 2048);
  FillChar(lvRawHeader[0], 2048, 0);
  r := FRawSocket.RecvBufEnd(@lvRawHeader[0], 2048, @HTTP_HEADER_END[0], 4, FTimeOut);
  if r = 0 then
  begin
    // �Է����ر�
    Close;
    raise TDiocpSocketSendException.Create('��������Ͽ����ӣ�');
  end;
  // ����Ƿ��д���
  CheckSocketRecvResult(r);

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
    {$IFDEF USE_Z_LZO}
    else if FResponseContentEncoding = 'lzo' then
    begin
      LZODecompressBufferBuilder(FReponseBuilder);
    end
    {$ENDIF}
    {$IFDEF USE_ZLIBExGZ}
    else if FResponseContentEncoding = 'gzip' then
    begin
      GZDecompressBufferBuilder(FReponseBuilder);
    end
    {$ENDIF}
    ;

    l:= FReponseBuilder.Length;


    FResponseBody.Size := l;
    Move(FReponseBuilder.Memory^, FResponseBody.Memory^, l); 
  end;




  DoAfterResponse; 
end;

procedure TDiocpHttpClient.OnBufferWrite(pvSender: TObject; pvBuffer: Pointer;
    pvLength: Integer);
var
  r:Integer;
begin
{$IFDEF DIOCP_SSL}
  if (FSsl <> nil) and (self.FSslFlag = 1) then
  begin
    r := SSL_write(FSsl, pvBuffer, pvLength);
    CheckSocketSendResult(r);
    if r <> pvLength then
    begin
      FRawSocket.Close();
      raise TDiocpSocketSendException.Create(Format('ָ�����͵����ݳ���:%d, ʵ�ʷ��ͳ���:%d', [pvLength, r]));
    end;
    exit;
  end;
{$ENDIF}
  r := FRawSocket.SendBuf(pvBuffer^, pvLength);
  CheckSocketSendResult(r);
  if r <> pvLength then
  begin
    FRawSocket.Close();
    raise TDiocpSocketSendException.Create(Format('ָ�����͵����ݳ���:%d, ʵ�ʷ��ͳ���:%d', [pvLength, r]));
  end;
  FPostSize := FPostSize + r;
end;

procedure TDiocpHttpClient.Reset;
begin
  self.Cleanup;
  FRaiseOnResponseOnExceptCode := True;
  FResponseCookie := STRING_EMPTY;
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

initialization
  //__trace_id := 0;
  __root := ExtractFilePath(ParamStr(0));
  __writeBufferPool := NewBufferPool(BLOCK_SIZE);



finalization
  FreeBufferPool(__writeBufferPool);


end.
