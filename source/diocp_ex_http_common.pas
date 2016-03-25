unit diocp_ex_http_common;

interface

{$DEFINE USE_ZLIBExGZ}

uses
  utils.strings, SysUtils, utils_dvalue
{$IFDEF USE_ZLIBExGZ}
  , ZLibExGZ, ZLibEx
{$ENDIF}
  , Classes, Zlib;

{$if CompilerVersion>= 21}
  {$define NEWZLib}
{$IFEND}


const
  END_BYTES : array[0..3] of Byte = (13,10,13,10);
  MAX_RAW_BUFFER_SIZE = 0;  

type
  TDHttpCookie = class;
  THttpRequest = class(TObject)
  private
    FBufferBuilder: TDBufferBuilder;
    FContentLength: Int64;
    FEndMatchIndex: Integer;
    FHeaders: TDValue;
    /// <summary>
    ///   0: 需要初始化
    ///   1: 已经初始化
    /// </summary>
    FFlag: Byte;
    FRawHeader: String;
    FDataLength: Integer;
    FHttpVersion: String;
    FMethod: String;
    FPtrBuffer: PByte;
    FRequestRawURL: string;
    FRequestRawCookie: string;
    
    FRequestURI: string;
    /// <summary>
    ///  0: RawHeader;
    ///  1: DataAsString;
    /// </summary>
    FSectionFlag: Byte;
    function DecodeRequestMethod: Integer;

    function DecodeHeader: Integer;
    procedure DecodeHeaderLine(pvLine:string);
    function DecodeFirstLine(pvLine: string): Integer;
    function GetContentLength: Int64;
    function GetDataAsMemory: PByte;

    function GetDataAsString: String;
    function GetRawCookie: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoCleanUp;

    /// <summary>THttpRequest.InputBuffer
    /// </summary>
    /// <returns>
    ///  0: 需要更多的数据来完成解码
    ///  -2: 超过最大长度(MAX_RAW_BUFFER_SIZE)
    ///  1: 解码到头
    ///  2: 解码到请求体
    /// </returns>
    /// <param name="pvByte"> (Byte) </param>
    function InputBuffer(pvByte:Byte): Integer;


    property DataAsMemory: PByte read GetDataAsMemory;

    property DataAsString: String read GetDataAsString;
    
    /// <summary>
    ///   数据长度
    /// </summary>
    property DataLength: Integer read FDataLength;

    property RawHeader: String read FRawHeader;

    property Headers: TDValue read FHeaders;

    property HttpVersion: String read FHttpVersion;

    property Method: String read FMethod write FMethod;
    
    property ContentLength: Int64 read GetContentLength;

    property RawCookie: String read GetRawCookie;



    property RequestRawURL: string read FRequestRawURL write FRequestRawURL;
    property RequestURI: string read FRequestURI;

  end;

  THttpResponse = class(TObject)
  private
    FHeaderBuilder: TDBufferBuilder;
    FContentBuffer: TDBufferBuilder;
    FContentType: RAWString;
    FCookies: TDValue;
    FHeaders: TDValue;
    FResponseCode: Word;
    procedure InnerBuildHeader(pvBuilder: TDBufferBuilder); virtual;
  public
    procedure DoCleanUp;
    constructor Create;
    destructor Destroy; override;
    function AddCookie: TDHttpCookie; overload;
    function AddCookie(pvName:String; pvValue:string): TDHttpCookie; overload;
    property ContentBuffer: TDBufferBuilder read FContentBuffer;
    property ContentType: RAWString read FContentType write FContentType;
    property HeaderBuilder: TDBufferBuilder read FHeaderBuilder;
    property Headers: TDValue read FHeaders;
    property ResponseCode: Word read FResponseCode write FResponseCode;
    /// <summary>
    ///  读取传入的Cookie值
    /// </summary>
    function GetCookie(pvCookieName:string): TDHttpCookie;
    procedure ClearCookies;
    
    procedure EncodeHeader(pvContentLength: Integer);
    procedure ChunkedBuffer(pvBuffer:Pointer; pvLen:Integer);

    procedure ChunkedBufferStart;
    procedure ChunkedBufferEnd;

    /// <summary>
    ///   GZip 压缩
    /// </summary>
    procedure GZipContent;

    procedure ZCompressContent;
  end;

  /// <summary>
  ///   服务端设置客户端Cookie设置类
  /// </summary>
  TDHttpCookie = class(TObject)
  private
    FExpires: TDateTime;
    FName: String;
    FPath: String;
    FValue: String;
  public
    /// <summary>
    ///   编码成一个String
    /// </summary>
    function ToString: String;

    property Expires: TDateTime read FExpires write FExpires;
    property Name: String read FName write FName;
    property Path: String read FPath write FPath;
    property Value: String read FValue write FValue;
  end;



function GetResponseCodeText(pvCode: Word): RAWString;

procedure ZCompressBufferBuilder(pvBuilder:TDBufferBuilder);

procedure ZDecompressBufferBuilder(pvBuilder:TDBufferBuilder);

{$IFDEF USE_ZLIBExGZ}
procedure GZCompressBufferBuilder(pvBuilder:TDBufferBuilder);
procedure GZDecompressBufferBuilder(pvBuilder:TDBufferBuilder);
{$ENDIF}



implementation

function GetResponseCodeText(pvCode: Word): RAWString;
begin
  case pvCode of
    100: Result := '100 Continue';
    101: Result := '101 Switching Protocols';
    102: Result := '102 Processing';
    200: Result := '200 OK';
    201: Result := '201 Created';
    202: Result := '202 Accepted';
    203: Result := '203 Non-Authoriative Information';
    204: Result := '204 No Content';
    205: Result := '205 Reset Content';
    206: Result := '206 Partial Content';
    207: Result := '207 Multi-Status';
    300: Result := '300 Multiple Choices';
    301: Result := '301 Moved Permanently';
    302: Result := '302 Found';
    303: Result := '303 See Other';
    304: Result := '304 Not Modified';
    305: Result := '305 Use Proxy';
    306: Result := '306 (Unused)';
    307: Result := '307 Temporary Redirect';
    400: Result := '400 Bad Request';
    401: Result := '401 Unauthorized';
    403: Result := '403 Forbidden';
    404: Result := '404 Not Found';
    405: Result := '405 Method Not Allowed';
    406: Result := '406 Not Acceptable';
    407: Result := '407 Proxy Authentication Required';
    408: Result := '408 Request Timeout';
    409: Result := '409 Conflict';
    410: Result := '410 Gone';
    411: Result := '411 Length Required';
    412: Result := '412 Precondition Failed';
    413: Result := '413 Request Entity Too Large';
    414: Result := '414 Request URI Too Long';
    415: Result := '415 An Unsupported Media Type';
    416: Result := '416 Requested Range Not Satisfiable';
    417: Result := '417 On Failure';
    422: Result := '422 Unprocessable Entity';
    423: Result := '423 Locked';
    424: Result := '424 Failed Dependency';
    500: Result := '500 Internal Server Error';
    501: Result := '501 Not Implemented';
    502: Result := '502 Bad Gateway';
    503: Result := '503 Service Unavailable';
    504: Result := '504 Gateway Timeout';
    505: Result := '505 Version Not Supported';
    507: Result := '507 Insufficient Storage';
  else
    Result := IntToStr(pvCode) +  ' Unknown Error';
  end;
end;

procedure ZCompressBufferBuilder(pvBuilder:TDBufferBuilder);
{$IFDEF POSIX}
var
  lvBytes, lvOutBytes:TBytes;
{$ELSE}
var
  lvInBuf: TBytes;
  lvOutBuf: Pointer;
  lvOutBytes: Integer;
{$ENDIF}
var
  l: Integer;
begin
{$IFDEF POSIX}
  error
//  SetLength(lvBytes, pvInStream.Size);
//  pvInStream.Position := 0;
//  pvInStream.Read(lvBytes[0], pvInStream.Size);
//  ZLib.ZCompress(lvBytes, lvOutBytes);                 // POSIX下只支持该中方式的压缩
//  pvOutStream.Size := Length(lvOutBytes);
//  pvOutStream.Position := 0;
//  pvOutStream.Write(lvOutBytes[0], Length(lvOutBytes));
{$ELSE}
  try
    {$if defined(NEWZLib)}
    ZLib.ZCompress(pvBuilder.Memory, pvBuilder.Length, lvOutBuf, lvOutBytes);
    {$ELSE}
    ZLib.CompressBuf(pvBuilder.Memory, pvBuilder.Length, lvOutBuf, lvOutBytes);
    {$ifend}
    pvBuilder.Clear;
    pvBuilder.AppendBuffer(lvOutBuf, lvOutBytes);
  finally
    FreeMem(lvOutBuf, lvOutBytes);
  end;
{$ENDIF}
end;

procedure ZDecompressBufferBuilder(pvBuilder:TDBufferBuilder);
var
  l:Integer;
{$IFDEF POSIX}
var
  lvBytes, lvOutBytes:TBytes;
{$ELSE}
var
  lvBytes:TBytes;
  OutBuf: Pointer;
  OutBytes: Integer;
{$ENDIF}
begin
  if pvBuilder.Length = 0 then exit;
{$IFDEF POSIX}
  SetLength(lvBytes, l);
  pvInStream.Position := 0;
  pvInStream.Read(lvBytes[0], pvInStream.Size);
  ZLib.ZDecompress(lvBytes, lvOutBytes);  //POSIX下，只支持该方式
  pvOutStream.Size := Length(lvOutBytes);
  pvOutStream.Position := 0;
  pvOutStream.Write(lvOutBytes[0], Length(lvOutBytes));
{$ELSE}
  {$if defined(NEWZLib)}
  ZLib.ZDecompress(pvBuilder.Memory, pvBuilder.Length, OutBuf, OutBytes);
  {$ELSE}
  Zlib.DecompressBuf(pvBuilder.Memory, pvBuilder.Length, 0, OutBuf, OutBytes);
  {$ifend}
  try
    pvBuilder.Clear;
    pvBuilder.AppendBuffer(OutBuf, OutBytes);
  finally
    FreeMem(OutBuf, OutBytes);
  end;
{$ENDIF}
end;

{$IFDEF USE_ZLIBExGZ}
procedure GZCompressBufferBuilder(pvBuilder:TDBufferBuilder);
var
  lvInStream, lvOutStream:TMemoryStream;
//  lvOutBuf:Pointer;
//  lvOutBytes:Integer;
begin
//  try
//    ZLibEx.ZCompress(pvBuilder.Memory, pvBuilder.Length, lvOutBuf, lvOutBytes);
//    pvBuilder.Clear;
//    pvBuilder.AppendBuffer(lvOutBuf, lvOutBytes);
//  finally
//    FreeMem(lvOutBuf, lvOutBytes);
//  end;
  lvInStream := TMemoryStream.Create;
  lvOutStream := TMemoryStream.Create;
  try
    lvInStream.SetSize(pvBuilder.Length);
    lvInStream.WriteBuffer(pvBuilder.Memory^, pvBuilder.Length);
    lvInStream.Position := 0;
    GZCompressStream(lvInStream, lvOutStream);
    pvBuilder.Clear;
    pvBuilder.AppendBuffer(lvOutStream.Memory, lvOutStream.Size);
  finally
    lvInStream.Free;
    lvOutStream.Free;
  end;

end;

procedure GZDecompressBufferBuilder(pvBuilder:TDBufferBuilder);
var
  lvInStream, lvOutStream:TMemoryStream;
begin
  lvInStream := TMemoryStream.Create;
  lvOutStream := TMemoryStream.Create;
  try
    lvInStream.SetSize(pvBuilder.Length);
    lvInStream.WriteBuffer(pvBuilder.Memory^, pvBuilder.Length);
    lvInStream.Position := 0;
    GZDecompressStream(lvInStream, lvOutStream);
    pvBuilder.Clear;
    pvBuilder.AppendBuffer(lvOutStream.Memory, lvOutStream.Size);
  finally
    lvInStream.Free;
    lvOutStream.Free;
  end; 
end;

{$ENDIF}

constructor THttpRequest.Create;
begin
  inherited Create;
  FBufferBuilder := TDBufferBuilder.Create();
  FHeaders := TDValue.Create();
  FContentLength := -1;
end;

destructor THttpRequest.Destroy;
begin
  FHeaders.Free;
  FreeAndNil(FBufferBuilder);
  inherited Destroy;
end;

function THttpRequest.DecodeHeader: Integer;
var
  lvPtr:PChar;
  lvLine:String;
begin
  lvPtr := PChar(FRawHeader);

  lvLine := LeftUntil(lvPtr, [#13, #10]);
  Result := DecodeFirstLine(lvLine);
  if Result = -1 then
  begin
    Exit;
  end;

  while True do
  begin
    SkipChars(lvPtr,  [#13, #10, ' ', #9]);
    if LeftUntil(lvPtr, [#13, #10], lvLine) = 0 then
    begin
      DecodeHeaderLine(lvLine);
    end else
    begin
      break;
    end;
  end;
  

  Result := 0;
end;

procedure THttpRequest.DecodeHeaderLine(pvLine:string);
var
  lvPtr:PChar;
  lvKey:string;
  r:Integer;
begin
  lvPtr := PChar(pvLine);

  r := LeftUntil(lvPtr, [':'], lvKey);
  if r = -1 then Exit;

  lvKey := LowerCase(Trim(lvKey));
  SkipChars(lvPtr, [':', ' ', #9]);
  FHeaders.ForceByName(lvKey).AsString := lvPtr;
end;

function THttpRequest.DecodeFirstLine(pvLine: string): Integer;
var
  lvPtr, lvTempPtr:PChar;
begin
  // GET /test?v=abc HTTP/1.1
  lvPtr := PChar(pvLine);

  FMethod := UpperCase(LeftUntil(lvPtr, [' ']));
  if FMethod = '' then
  begin
    Result := -1;
    Exit;
  end;

  // 跳过空格
  SkipChars(lvPtr, [' ']);
  Result := 0;
  if (FMethod = 'GET') then
  begin
    ;
  end else if (FMethod = 'POST') then
  begin
    ;
  end else if (FMethod = 'PUT') then
  begin
    ;
  end else if (FMethod = 'HEAD') then
  begin
    ;
  end else if (FMethod = 'OPTIONS') then
  begin
    ;
  end else if (FMethod = 'DELETE') then
  begin
    ;
  end else if (FMethod = 'TRACE') then
  begin
    ;
  end else if (FMethod = 'CONNECT') then
  begin
    ;
  end else
  begin
    Result := -1;
  end;

  if Result = 0 then
  begin
    //if lvPtr^='/' then inc(lvPtr);
    FRequestRawURL := LeftUntil(lvPtr, [' ', #9]);
    lvTempPtr := PChar(FRequestRawURL);
    if LeftUntil(lvTempPtr, ['?'], FRequestURI) = -1 then
    begin     // 截取URI
      FRequestURI := FRequestRawURL;    
    end;   

    SkipChars(lvPtr, [' ', #9]);
    FHttpVersion := lvPtr;
  end;
end;

function THttpRequest.DecodeRequestMethod: Integer;
var
  lvBuf:PChar;
  lvMethod:String;
begin
  lvMethod := ByteBufferToString(FBufferBuilder.Memory, 7);
  lvBuf :=  PChar(lvMethod);
  if (StrLIComp(lvBuf, 'GET', 3) = 0) then
  begin
    FMethod := 'GET';
  end
  else if (StrLIComp(lvBuf, 'POST', 4) = 0) then
  begin
    FMethod := 'POST';
  end
  else if (StrLIComp(lvBuf, 'PUT', 3) = 0) then
  begin
    FMethod := 'PUT';
  end
  else if (StrLIComp(lvBuf, 'HEAD', 3) = 0) then
  begin
    FMethod := 'HEAD';
  end
  else if (StrLIComp(lvBuf, 'OPTIONS', 7) = 0) then
  begin
    FMethod := 'OPTIONS';
  end
  else if (StrLIComp(lvBuf, 'DELETE', 6) = 0) then
  begin
    FMethod := 'DELETE';
  end
  else if (StrLIComp(lvBuf, 'TRACE', 5) = 0) then
  begin
    FMethod := 'TRACE';
  end
  else if (StrLIComp(lvBuf, 'CONNECT', 7) = 0) then
  begin
    FMethod := 'CONNECT';
  end
  else
  begin
    Result := -1;
  end;
end;

procedure THttpRequest.DoCleanUp;
begin
  if FBufferBuilder <> nil then
  begin
    FBufferBuilder.Clear;
  end;
  FDataLength := 0;
  FSectionFlag := 0;
  FFlag := 0;
  FContentLength := -1;
  FRawHeader := '';
  FRequestRawURL := '';
  FRequestRawCookie := '-1';
end;

function THttpRequest.GetContentLength: Int64;
begin
  if FContentLength = -1 then
  begin
    FContentLength := FHeaders.GetValueByName('Content-Length', 0);
  end;
  Result := FContentLength;
end;

function THttpRequest.GetDataAsMemory: PByte;
begin
  Result := FBufferBuilder.Memory;
end;

function THttpRequest.GetDataAsString: String;
begin
  Result := Utf8BufferToString(FBufferBuilder.Memory, FDataLength);
end;

function THttpRequest.GetRawCookie: String;
begin
  if FRequestRawCookie = '-1' then
  begin
    FRequestRawCookie := FHeaders.GetValueByName('Cookie', '');
  end;
  Result := FRequestRawCookie;
end;

function THttpRequest.InputBuffer(pvByte:Byte): Integer;
begin
  Result := 0;
  if FFlag = 0 then
  begin
    FBufferBuilder.Clear;
    FFlag := 1;
    FEndMatchIndex := 0;
    FDataLength := 0;
  end;

  Inc(FDataLength);
  FBufferBuilder.Append(pvByte);
  Inc(FPtrBuffer);

  if FDataLength = 7 then
  begin         // 查看方法是否合法
    if DecodeRequestMethod = -1 then
    begin
      FSectionFlag := 0;
      Result := -1;
      Exit;
    end;
  end;
  

  if (pvByte = END_BYTES[FEndMatchIndex]) then
  begin
    inc(FEndMatchIndex);

    if (FSectionFlag = 0) and (FEndMatchIndex = 4) then
    begin   // 包头
      FRawHeader := Utf8BufferToString(FBufferBuilder.Memory, FDataLength);
      if DecodeHeader = -1 then
      begin
        FSectionFlag := 0;
        Result := -1;
      end else
      begin
        FSectionFlag := 1;
        Result := 1;
      end;

      FFlag := 0;

      Exit;
    end else if (FSectionFlag = 1) and (FEndMatchIndex = 2) then
    begin   // 请求数据<以换行符为一个请求>
      Result := 2;     // DataAsString                          
      FFlag := 0;   // 重新开始请求Buffer进行解码
      Exit;
    end;
  end
  else if (MAX_RAW_BUFFER_SIZE > 0) and (FDataLength = MAX_RAW_BUFFER_SIZE) then
  begin            // 数据过长
    FFlag := 0;
    Result := -2;
  end else
  begin
    FEndMatchIndex := 0;
  end;
  Result := 0;
end;

function TDHttpCookie.ToString: String;
begin
  Result := Format('%s=%s; Path=%s;', [self.FName, self.FValue, Self.FPath]);
end;

constructor THttpResponse.Create;
begin
  inherited Create;
  FHeaderBuilder := TDBufferBuilder.Create();
  FContentBuffer := TDBufferBuilder.Create;
  FHeaders := TDValue.Create();
  FCookies := TDValue.Create();
end;

destructor THttpResponse.Destroy;
begin
  FHeaders.Free;
  FCookies.Free;
  FreeAndNil(FHeaderBuilder);
  FContentBuffer.Free;
  inherited Destroy;
end;

function THttpResponse.AddCookie: TDHttpCookie;
begin
  Result := TDHttpCookie.Create;
  Result.Path := '/';
  FCookies.AddArrayChild.BindObject(Result);
end;

function THttpResponse.AddCookie(pvName:String; pvValue:string):
    TDHttpCookie;
begin
  Result := AddCookie;
  Result.Name := pvName;
  Result.Value := pvValue;
end;

procedure THttpResponse.ClearCookies;
begin
  FCookies.Clear;
end;

procedure THttpResponse.DoCleanUp;
begin
  FCookies.Clear;
  FHeaderBuilder.Clear;
  FContentBuffer.Clear;
  FResponseCode := 0;
  FContentType := '';
end;

procedure THttpResponse.EncodeHeader(pvContentLength: Integer);
begin
  FHeaderBuilder.Clear;
  InnerBuildHeader(FHeaderBuilder);

  FHeaderBuilder.AppendRawStr('Content-Length:').AppendRawStr(IntToStr(pvContentLength)).AppendBreakLineBytes;
  FHeaderBuilder.AppendBreakLineBytes;
end;

function THttpResponse.GetCookie(pvCookieName:string): TDHttpCookie;
var
  i:Integer;
  lvCookie:TDHttpCookie;
begin
  Result := nil;
  for i := 0 to FCookies.Count - 1 do
  begin
    lvCookie := TDHttpCookie(FCookies[i].AsObject);
    if lvCookie.Name = pvCookieName then
    begin
      Result := lvCookie;
      Exit;
    end;
  end;
end;

procedure THttpResponse.InnerBuildHeader(pvBuilder: TDBufferBuilder);
var
  i:Integer;
  lvItem:TDValue;
  lvCode:Word;
begin
  lvCode := FResponseCode;
  if lvCode = 0 then lvCode := 200;
  
  pvBuilder.AppendRawStr('HTTP/1.1 ').AppendRawStr(GetResponseCodeText(lvCode)).AppendBreakLineBytes;
  pvBuilder.AppendRawStr('Server: DIOCP-V5/1.1').AppendBreakLineBytes;
  pvBuilder.AppendRawStr('Content-Type:').AppendRawStr(FContentType).AppendBreakLineBytes;

  for i := 0 to FHeaders.Count - 1 do
  begin
    lvItem := FHeaders.Items[i];
    pvBuilder.AppendRawStr(lvItem.Name.AsString + ':').AppendRawStr(lvItem.Value.AsString).AppendBreakLineBytes;
  end;

  for i := 0 to FCookies.Count - 1 do
  begin
    pvBuilder.AppendRawStr('Set-Cookie:').AppendRawStr(TDHttpCookie(FCookies[i].AsObject).ToString()).AppendBreakLineBytes;
  end;
end;

procedure THttpResponse.ChunkedBuffer(pvBuffer:Pointer; pvLen:Integer);
begin
  FContentBuffer.AppendRawStr(IntToHex(pvLen, 2)).AppendBreakLineBytes;
  FContentBuffer.AppendBuffer(PByte(pvBuffer), pvLen).AppendBreakLineBytes;
end;

procedure THttpResponse.ChunkedBufferEnd;
begin
  FContentBuffer.AppendRawStr('0').AppendBreakLineBytes.AppendBreakLineBytes;
end;

procedure THttpResponse.ChunkedBufferStart;
begin
  FContentBuffer.Clear;
  InnerBuildHeader(FContentBuffer);
  FContentBuffer.AppendRawStr('Transfer-Encoding: chunked').AppendBreakLineBytes;
  FContentBuffer.AppendBreakLineBytes;
   
end;

procedure THttpResponse.GZipContent;
begin
{$IFDEF USE_ZLIBExGZ}
  GZCompressBufferBuilder(FContentBuffer);
{$ELSE}
  Assert(False, '需要引用ZLibxExGZ');
{$ENDIF}

end;

procedure THttpResponse.ZCompressContent;
begin
  ZCompressBufferBuilder(FContentBuffer);
end;

end.
