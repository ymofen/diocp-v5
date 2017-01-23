(*
  *	 Unit owner: D10.Mofen, delphi iocp framework author
  *         homePage: http://www.Diocp.org
  *	       blog: http://www.cnblogs.com/dksoft

  *   2015-02-22 08:29:43
  *     DIOCP-V5 发布

  *    Http协议处理单元
  *    其中大部分思路来自于delphi iocp framework中的iocp.HttpServer
  *

// 验证
GET /1 HTTP/1.1
Host: 127.0.0.1
Ntrip-Version: Ntrip/2.0
User-Agent: NTRIP NtripClientPOSIX/1.49
Connection: close
Authorization: Basic dXNlcjpwYXNzd29yZA==


*)
unit diocp_ex_ntrip;

interface

/// 三个编译开关，只能开启一个
{$DEFINE INNER_IOCP}     // iocp线程触发事件
{.$DEFINE  QDAC_QWorker} // 用qworker进行调度触发事件
{.$DEFINE DIOCP_Task}    // 用diocp_task进行调度触发事件


uses
  Classes, StrUtils, SysUtils, utils_buffer, utils_strings

  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp_task{$ENDIF}
  , diocp_tcp_server, utils_queues, utils_hashs, diocp_ex_http_common;



const
  HTTPLineBreak = #13#10;

type
  TDiocpNtripState = (hsCompleted, hsRecevingBodyData, hsRequest { 接收请求 }, hsRecvingSource { 接收NtripSource数据 } );
  TDiocpNtripContextMode = (ncmNtripNone, ncmNtripSource, ncmNtripClient);
  TDiocpNtripResponse = class;
  TDiocpNtripClientContext = class;
  TDiocpNtripServer = class;
  TDiocpNtripRequest = class;

  TOnRequestAcceptEvent = procedure(pvRequest:TDiocpNtripRequest; var vIsNMEA:Boolean) of object;

  TDiocpNtripRequest = class(TObject)
  private
    /// <summary>
    ///   便于在Close时归还回对象池
    /// </summary>
    FDiocpNtripServer:TDiocpNtripServer;

    FDiocpContext: TDiocpNtripClientContext;

    /// 头信息
    FHttpVersion: Word; // 10, 11

    FRequestVersionStr: String;

    FRequestMethod: String;

    FMountPoint: String;

    /// <summary>
    ///  原始请求中的URL参数数据(没有经过URLDecode，因为在DecodeRequestHeader中要拼接RequestURL时临时进行了URLDecode)
    ///  没有经过URLDecode是考虑到参数值中本身存在&字符，导致DecodeURLParam出现不解码异常
    /// </summary>
    FRequestURLParamData: string;


    FRequestParamsList: TStringList; // TODO:存放http参数的StringList

    FContextType: string;
    FContextLength: Int64;
    FKeepAlive: Boolean;
    FRequestAccept: String;
    FRequestAcceptLanguage: string;
    FRequestAcceptEncoding: string;
    FRequestUserAgent: string;
    FRequestAuth: string;
    FRequestHostName: string;
    FRequestHostPort: string;

    FRawHeaderData: TMemoryStream;

    FRequestHeader: TStringList;

    FResponse: TDiocpNtripResponse;
    FSourceRequestPass: String;

    /// <summary>
    ///   不再使用了，归还回对象池
    /// </summary>
    procedure Close;
    /// <summary>
    /// 解码Http请求参数信息
    /// </summary>
    /// <returns>
    /// 1: 有效的Http参数数据
    /// </returns>
    function DecodeRequestHeader: Integer;

    /// <summary>
    /// 接收到的Buffer,写入数据
    /// </summary>
    procedure WriteRawBuffer(const buffer: Pointer; len: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;


    /// <summary>
    ///   清理
    /// </summary>
    procedure Clear;

    property ContextLength: Int64 read FContextLength;


    /// <summary>
    ///   与客户端建立的连接
    /// </summary>
    property Connection: TDiocpNtripClientContext read FDiocpContext;

    property HttpVersion: Word read FHttpVersion;
    property RequestAccept: String read FRequestAccept;
    property RequestAcceptEncoding: string read FRequestAcceptEncoding;
    property RequestAcceptLanguage: string read FRequestAcceptLanguage;
    /// <summary>
    ///   请求的头信息
    /// </summary>
    property RequestHeader: TStringList read FRequestHeader;

    /// <summary>
    ///   挂节点
    /// </summary>
    property MountPoint: String read FMountPoint;

    /// <summary>
    ///   Source方法请求中的Password
    /// </summary>
    property SourceRequestPass: String read FSourceRequestPass write  FSourceRequestPass;

    /// <summary>
    ///  从头信息中读取的请求服务器请求方式
    /// </summary>
    property RequestMethod: string read FRequestMethod;

    /// <summary>
    ///   从头信息中读取的请求服务器IP地址
    /// </summary>
    property RequestHostName: string read FRequestHostName;

    /// <summary>
    ///   从头信息中读取的请求服务器端口
    /// </summary>
    property RequestHostPort: string read FRequestHostPort;

    /// <summary>
    /// Http响应对象，回写数据
    /// </summary>
    property Response: TDiocpNtripResponse read FResponse;

    /// <summary>
    ///   从Url和Post数据中得到的参数信息: key = value
    /// </summary>
    property RequestParamsList: TStringList read FRequestParamsList;


    /// <summary>
    ///   获取头信息中的用户名和密码信息
    /// </summary>
    /// <returns>
    ///   获取成功返回true
    /// </returns>
    /// <param name="vUser"> (string) </param>
    /// <param name="vPass"> (string) </param>
    function ExtractBasicAuthenticationInfo(var vUser, vPass:string): Boolean;


    /// <summary>
    ///  关闭连接
    /// </summary>
    procedure CloseContext;

    /// <summary>
    /// 得到http请求参数
    /// </summary>
    /// <params>
    /// <param name="ParamsKey">http请求参数的key</param>
    /// </params>
    /// <returns>
    /// 1: http请求参数的值
    /// </returns>
    function GetRequestParam(ParamsKey: string): string;

    /// <summary>
    /// 解析POST和GET参数
    /// </summary>
    /// <pvParamText>
    /// <param name="pvParamText">要解析的全部参数</param>
    /// </pvParamText>
    procedure ParseParams(pvParamText: string);


  end;

  TDiocpNtripResponse = class(TObject)
  private
    FResponseHeader: string;
    FData: TMemoryStream;
    FDiocpContext : TDiocpNtripClientContext;
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure WriteBuf(pvBuf: Pointer; len: Cardinal);
    procedure WriteString(pvString: string; pvUtf8Convert: Boolean = true);

    /// <summary>
    ///  发送ICY200OK信息
    /// </summary>
    procedure ICY200OK();

    /// <summary>
    ///   NtripSource认证时密码错误的回复，回复后，关闭连接
    /// </summary>
    procedure BadPassword();

    /// <summary>
    ///   发送SourceTableOK信息
    /// </summary>
    procedure SourceTableOK();

    /// <summary>
    ///   发送SourceTableOK和SourceTable数据
    /// </summary>
    procedure SourceTableOKAndData(pvSourceTable:AnsiString);

    /// <summary>
    ///   NtripClient验证失败
    /// </summary>
    procedure Unauthorized();

    /// <summary>
    ///   无效的用户认证信息
    /// </summary>
    /// <param name="pvMountpoint"> 挂节点 </param>
    procedure InvalidPasswordMsg(pvMountpoint: string);


    /// <summary>
    ///   与客户端建立的连接
    /// </summary>
    property Connection: TDiocpNtripClientContext read FDiocpContext;

  end;

  /// <summary>
  /// Http 客户端连接
  /// </summary>
  TDiocpNtripClientContext = class(TIocpClientContext)
  private
    // NtripSource，作为转发使用
    FNtripClients:TList;

    // 做分发数据时临时使用
    FTempNtripClients:TList;

    // 缓存数据
    FRecvBuffer: TBufferLink;

    FContextMode: TDiocpNtripContextMode;
    FNtripState: TDiocpNtripState;
    FNtripRequest: TDiocpNtripRequest;
    FMountPoint: String;
    FTag: Integer;
    FTagStr: String;

    // 执行事件
    procedure DoRequest(pvRequest:TDiocpNtripRequest);

  protected

    /// <summary>
    ///   触发本身的Request过程
    /// </summary>
    procedure OnRequest(pvRequest:TDiocpNtripRequest); virtual;

    procedure OnDisconnected; override;

  public
    constructor Create; override;
    destructor Destroy; override;
  protected
    /// <summary>
    /// 归还到对象池，进行清理工作
    /// </summary>
    procedure DoCleanUp; override;

    /// <summary>
    /// 接收到客户端的Http协议数据, 进行解码成TDiocpNtripRequest，响应Http请求
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: Word); override;
  public
    /// <summary>
    ///   添加到NtripSource的分发列表
    /// </summary>
    procedure AddNtripClient(pvContext:TDiocpNtripClientContext);

    /// <summary>
    ///   移除掉NtripSource的分发列表
    ///   在做分发数据时，有执行移除动作(对应的Context不是请求的mountpoint的数据，或者断开)
    /// </summary>
    procedure RemoveNtripClient(pvContext:TDiocpNtripClientContext);

    /// <summary>
    ///   分发GNSSData
    /// </summary>
    procedure DispatchGNSSDATA(buf: Pointer; len: Cardinal);

    property ContextMode: TDiocpNtripContextMode read FContextMode write FContextMode;

    property MountPoint: String read FMountPoint write FMountPoint;



    property Tag: Integer read FTag write FTag;

    property TagStr: String read FTagStr write FTagStr;


  end;

{$IFDEF UNICODE}
  /// <summary>
  /// Request事件类型
  /// </summary>
  TOnDiocpNtripRequestEvent = reference to procedure(pvRequest: TDiocpNtripRequest);

  /// <summary>
  /// 接收到NtripSource数据
  /// </summary>
  TDiocpRecvBufferEvent = reference to procedure(pvContext:TDiocpNtripClientContext; buf: Pointer; len: Cardinal);
{$ELSE}
  /// <summary>
  /// Request事件类型
  /// </summary>
  TOnDiocpNtripRequestEvent = procedure(pvRequest: TDiocpNtripRequest) of object;

  /// <summary>
  /// 接收到NtripSource数据
  /// </summary>
  TDiocpRecvBufferEvent = procedure(pvContext:TDiocpNtripClientContext;buf: Pointer; len: Cardinal) of object;
{$ENDIF}

  /// <summary>
  /// Http 解析服务
  /// </summary>
  TDiocpNtripServer = class(TDiocpTcpServer)
  private
    FDebugState: Integer;
    FNtripSourcePassword: String;
    FRequestPool: TBaseQueue;

    /// <summary>
    ///  存放Source列表
    /// </summary>
    FNtripSources: TDHashTableSafe;

    FOnDiocpNtripRequest: TOnDiocpNtripRequestEvent;
    FOnDiocpRecvNtripSourceBuffer: TDiocpRecvBufferEvent;
    FOnRequestAcceptEvent: TOnRequestAcceptEvent;
    FOnDiocpRecvNtripClientBuffer: TDiocpRecvBufferEvent;

    /// <summary>
    /// 响应Http请求， 执行响应事件
    /// </summary>
    procedure DoRequest(pvRequest: TDiocpNtripRequest);

    /// <summary>
    ///   从池中获取一个对象
    /// </summary>
    function GetRequest: TDiocpNtripRequest;

    /// <summary>
    ///   还回一个对象
    /// </summary>
    procedure GiveBackRequest(pvRequest:TDiocpNtripRequest);

    procedure RemoveNtripSource(pvMountPoint:String; pvContext:TIocpClientContext);

    procedure AddNtripSource(pvMountPoint:String; pvContext:TIocpClientContext);

  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    function GetNtripSourceList(pvStrings:TStrings): Integer; overload;

    function GetNtripSourceList: String; overload;

    



    /// <summary>
    ///   根据mountPoint查找NtripSource
    /// </summary>
    function FindNtripSource(pvMountPoint:string):TDiocpNtripClientContext;


    /// <summary>
    ///   调试状态, 0:正常， 1:调试
    /// </summary>
    property DebugState: Integer read FDebugState write FDebugState;

    /// <summary>
    ///   NtripSourcePassword, 用于NtripSource接入时做认证
    /// </summary>
    property NtripSourcePassword: String read FNtripSourcePassword write FNtripSourcePassword;


    /// <summary>
    ///  请求进入
    /// </summary>
    property OnRequestAcceptEvent: TOnRequestAcceptEvent read FOnRequestAcceptEvent write FOnRequestAcceptEvent;

    /// <summary>
    ///   接收到NtripSource数据
    /// </summary>
    property OnDiocpRecvNtripSourceBuffer: TDiocpRecvBufferEvent read
        FOnDiocpRecvNtripSourceBuffer write FOnDiocpRecvNtripSourceBuffer;

    /// <summary>
    ///   接收到Client Buffer
    /// </summary>
    property OnDiocpRecvNtripClientBuffer: TDiocpRecvBufferEvent read
        FOnDiocpRecvNtripClientBuffer write FOnDiocpRecvNtripClientBuffer;
        
    /// <summary>
    ///   响应Http请求事件
    /// </summary>
    property OnDiocpNtripRequest: TOnDiocpNtripRequestEvent read FOnDiocpNtripRequest
        write FOnDiocpNtripRequest; 
  end;

procedure WriteHttpResponse(pvContent: TIocpClientContext; pvResponse: String;
    pvContextType: String = 'text/html');

var
  // double linebreak;
  __HEAD_END :array[0..3] of Byte;



implementation

uses
  utils_base64;

procedure WriteHttpResponse(pvContent: TIocpClientContext; pvResponse: String;
    pvContextType: String = 'text/html');
var
  lvResponse:THttpResponse;
  s:RAWString;
begin
  s := pvResponse;
  //s := StringReplace(s, sLineBreak, '<BR>', [rfReplaceAll]);
  lvResponse := THttpResponse.Create;
  try
    lvResponse.ResponseCode := 200;
    lvResponse.ContentType := pvContextType;
    lvResponse.ContentBuffer.AppendRawStr(s);
    lvResponse.EncodeHeader(lvResponse.ContentBuffer.Length);
    pvContent.PostWSASendRequest(lvResponse.HeaderBuilder.Memory, lvResponse.HeaderBuilder.Length);
    pvContent.PostWSASendRequest(lvResponse.ContentBuffer.Memory, lvResponse.ContentBuffer.Length);
  finally
    lvResponse.Free;
  end;
  // TODO -cMM: WriteHttpResponse default body inserted
end;

procedure TDiocpNtripRequest.Clear;
begin
  FRawHeaderData.Clear;
  FMountPoint := '';
  FSourceRequestPass := '';
  FRequestVersionStr := '';
  FRequestMethod := '';
  FRequestParamsList.Clear;
  FContextLength := 0;
  FResponse.Clear;  
end;

procedure TDiocpNtripRequest.Close;
begin
  if FDiocpNtripServer = nil then exit;
  FDiocpNtripServer.GiveBackRequest(Self);
end;

procedure TDiocpNtripRequest.CloseContext;
begin
  FDiocpContext.PostWSACloseRequest();
end;

function TDiocpNtripRequest.GetRequestParam(ParamsKey: string): string;
var
  lvTemp: string; // 返回的参数值
  lvParamsCount: Integer; // 参数数量
  I: Integer;
begin
  Result := '';

  lvTemp := ''; // 返回的参数值默认值为空

  // 得到提交过来的参数的数量
  lvParamsCount := self.FRequestParamsList.Count;

  // 判断是否有提交过来的参数数据
  if lvParamsCount = 0 then exit;

  // 循环比较每一组参数的key，是否和当前输入一样
  for I := 0 to lvParamsCount - 1 do
  begin 
    if Trim(self.FRequestParamsList.Names[I]) = Trim(ParamsKey) then
    begin
      lvTemp := Trim(self.FRequestParamsList.ValueFromIndex[I]);
      Break;
    end;
  end; 

  Result := lvTemp;
end;

constructor TDiocpNtripRequest.Create;
begin
  inherited Create;
  FRawHeaderData := TMemoryStream.Create();
  FRequestHeader := TStringList.Create();
  FResponse := TDiocpNtripResponse.Create();

  FRequestParamsList := TStringList.Create; // TODO:创建存放http参数的StringList
end;

destructor TDiocpNtripRequest.Destroy;
begin
  FreeAndNil(FResponse);
  FRawHeaderData.Free;
  FRequestHeader.Free;
  FreeAndNil(FRequestParamsList); // TODO:释放存放http参数的StringList
  inherited Destroy;
end;

function TDiocpNtripRequest.DecodeRequestHeader: Integer;
var
  lvRawString: AnsiString;
  lvMethod: AnsiString;
  lvRequestCmdLine, lvTempStr, lvRemainStr: String;
  p : PChar;
begin
  Result := 1;
  SetLength(lvRawString, FRawHeaderData.Size + 1);
  FillChar(PAnsiChar(lvRawString)^, FRawHeaderData.Size + 1, 0);
  FRawHeaderData.Position := 0;
  FRawHeaderData.Read(PAnsiChar(lvRawString)^, FRawHeaderData.Size);
  FRequestHeader.Text := lvRawString;

  // GET /test?v=abc HTTP/1.1
  // SOURCE letmein /Mountpoint
  lvRequestCmdLine := FRequestHeader[0];
  P := PChar(lvRequestCmdLine);
  FRequestHeader.Delete(0);

  // Method
  lvTempStr := LeftUntil(P, [' ']);
  if lvTempStr = '' then Exit;
  lvTempStr := UpperCase(lvTempStr);

  // 跳过空格
  SkipChars(P, [' ']);
  if lvTempStr = 'GET' then
  begin
    FRequestMethod := 'GET';
    
    FMountPoint := LeftUntil(P, [' ']);

    if Length(FMountPoint) > 0 then
    begin
      if PChar(FMountPoint)^ in ['/', '\'] then
      begin
        Delete(FMountPoint, 1, 1);
      end;
    end;


    // 跳过空格
    SkipChars(P, [' ']);

    // 请求的HTTP版本
    lvTempStr := P;
    FRequestVersionStr := UpperCase(lvTempStr);
  end else
  begin    // SOURCE
    FRequestMethod := 'SOURCE';
    if P^=' ' then
    begin
      FSourceRequestPass := '';
    end else
    begin
      FSourceRequestPass := LeftUntil(P, [' ']);
    end;
    // 跳过空格
    SkipChars(P, [' ']);
    if P^='/' then inc(p);
    
    FMountPoint := P;

  end;
end;

function TDiocpNtripRequest.ExtractBasicAuthenticationInfo(var vUser,
    vPass:string): Boolean;
var
  lvAuth, lvValue:string;
  p:PChar;
begin
  Result := False;
  // Authorization: Basic aHVnb2JlbjpodWdvYmVuMTIz
  lvAuth := Trim(StringsValueOfName(FRequestHeader, 'Authorization', [':'], true));
  if lvAuth <> '' then
  begin  // 认证信息
    p := PChar(lvAuth);    //Basic aHVnb2JlbjpodWdvYmVuMTIz

    // 跳过Basic
    SkipUntil(P, [' ']);
    SkipChars(P, [' ']);


    // Base64
    lvValue := P;
    lvValue := Base64Decode(lvValue);

    /// userid:pasword
    P := PChar(lvValue);

    // 取用户ID
    vUser := LeftUntil(P, [':']);
    SkipChars(P, [':']);
    // 取密码
    vPass := P;

    Result := true;
  end;

end;


/// <summary>
///  解析POST和GET参数
/// </summary>
/// <pvParamText>
/// <param name="pvParamText">要解析的全部参数</param>
/// </pvParamText>
procedure TDiocpNtripRequest.ParseParams(pvParamText: string);
begin
  SplitStrings(pvParamText, FRequestParamsList, ['&']);
end;

procedure TDiocpNtripRequest.WriteRawBuffer(const buffer: Pointer; len: Integer);
begin
  FRawHeaderData.WriteBuffer(buffer^, len);
end;

procedure TDiocpNtripResponse.BadPassword;
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'ERROR - Bad Password' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);

end;

procedure TDiocpNtripResponse.Clear;
begin
  FData.Clear;
  FResponseHeader := '';
end;

constructor TDiocpNtripResponse.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create();
end;

destructor TDiocpNtripResponse.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TDiocpNtripResponse.ICY200OK;
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'ICY 200 OK' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.InvalidPasswordMsg(pvMountpoint: string);
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'Server: NtripCaster/1.0' + sLineBreak
          + 'WWW-Authenticate: Basic realm="/' +pvMountpoint + '"' + sLineBreak
          + 'Content-Type: text/html' + sLineBreak
          + 'Connection: close' + sLineBreak
          + '<html><head><title>401 Unauthorized</title></head><body bgcolor=black text=white link=blue alink=red>' + sLineBreak
          + '<h1><center>The server does not recognize your privileges to the requested entity stream</center></h1>' + sLineBreak
          + '</body></html>' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.SourceTableOKAndData(pvSourceTable:AnsiString);
var
  lvData:AnsiString;
  len: Integer;
begin
//SOURCETABLE 200 OK
//Content-Type: text/plain
//Content-Length: n
//CAS;129.217.182.51;80;EUREF;BKG;0;DEU;51.5;7.5;http://igs.ifag.de/index_ntrip_cast.htm
//CAS;62.159.109.248;8080;Trimble GPSNet;Trimble Terrasat;1;DEU;48.03;11.72;http://www.virtualrtk.com
//NET;EUREF;EUREF;B;N;http://www.epncb.oma.be/euref_IP;http://www.epncb.oma.be/euref_IP;http
//ENDSOURCETABLE

  lvData := 'SOURCETABLE 200 OK' + sLineBreak +
            'Content-Type: text/plain' + sLineBreak +
            'Content-Length: ' + IntToStr(length(pvSourceTable)) + sLineBreak + sLineBreak +
            pvSourceTable + sLineBreak +
           'ENDSOURCETABLE' + sLineBreak + sLineBreak;


  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.SourceTableOK;
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'SOURCETABLE 200 OK' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.Unauthorized;
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'HTTP/1.0 401 Unauthorized' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.WriteBuf(pvBuf: Pointer; len: Cardinal);
begin
  FData.Write(pvBuf^, len);
end;

procedure TDiocpNtripResponse.WriteString(pvString: string; pvUtf8Convert:
    Boolean = true);
var
  lvRawString: AnsiString;
begin
  if pvUtf8Convert then
  begin     // 进行Utf8转换
    lvRawString := UTF8Encode(pvString);
  end else
  begin
    lvRawString := AnsiString(pvString);
  end;
  FData.WriteBuffer(PAnsiChar(lvRawString)^, Length(lvRawString));
end;

procedure TDiocpNtripClientContext.AddNtripClient(
  pvContext: TDiocpNtripClientContext);
begin
  // 当前是否NtripSource
  if FContextMode <> ncmNtripSource then Exit;

  self.Lock;
  try
    FNtripClients.Add(pvContext);
  finally
    self.UnLock;
  end;
end;

constructor TDiocpNtripClientContext.Create;
begin
  inherited Create;
  FRecvBuffer := TBufferLink.Create;
  FNtripClients := TList.Create;
  FTempNtripClients := TList.Create;
end;

destructor TDiocpNtripClientContext.Destroy;
begin
  FNtripClients.Free;
  FTempNtripClients.Free;
  FRecvBuffer.Free;
  inherited Destroy;
end;

procedure TDiocpNtripClientContext.DispatchGNSSDATA(buf: Pointer;
  len: Cardinal);
var
  i:Integer;
  lvContext:TDiocpNtripClientContext;
begin
  FTempNtripClients.Clear;
  // copy到临时列表中
  Self.Lock;
  FTempNtripClients.Assign(FNtripClients);
  Self.UnLock;

  for i := 0 to FTempNtripClients.Count -1 do
  begin
    lvContext :=TDiocpNtripClientContext(FTempNtripClients[i]);
    if lvContext.LockContext('分发GNSS数据', Self) then
    begin
      try
        if not SameText(lvContext.FMountPoint, self.FMountPoint) then  // 不是请求的挂节点
        begin
          RemoveNtripClient(lvContext);
        end else
        begin
          // 分发数据
          lvContext.PostWSASendRequest(buf, len);
        end;
      finally
        lvContext.UnLockContext('分发GNSS数据', Self);
      end;
    end else
    begin
      RemoveNtripClient(lvContext);
    end;
  end;
end;

procedure TDiocpNtripClientContext.DoCleanUp;
begin
  inherited;
  FTag := 0;
  FTagStr := '';
  FNtripState := hsCompleted;
  FContextMode := ncmNtripNone;
  FMountPoint := '';

  // 清空列表
  FNtripClients.Clear;
  if FNtripRequest <> nil then
  begin
    FNtripRequest.Close;
    FNtripRequest := nil;
  end;
end;

procedure TDiocpNtripClientContext.DoRequest(pvRequest: TDiocpNtripRequest);
begin
   OnRequest(pvRequest);
   TDiocpNtripServer(FOwner).DoRequest(pvRequest);
end;


procedure TDiocpNtripClientContext.OnDisconnected;
begin
  if ContextMode = ncmNtripSource then
  begin

    // 移除
    TDiocpNtripServer(FOwner).RemoveNtripSource(FMountPoint, Self);

//    .FNtripSources.Lock;
//    TDiocpNtripServer(FOwner).FNtripSources.ValueMap[FMountPoint] := nil;
//    TDiocpNtripServer(FOwner).FNtripSources.unLock;
  end;

  inherited;
end;

procedure TDiocpNtripClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: Word);
var
  lvTmpBuf: PAnsiChar;
  j: Integer;
  lvRemain: Cardinal;
  lvTempRequest: TDiocpNtripRequest;
  lvIsNMEA:Boolean;
  lvBuffer:TBytes;
  
begin
  RecordWorkerStartTick;
  try
    if FNtripRequest = nil then
    begin // 完成后重置，重新处理下一个包
      FNtripRequest := TDiocpNtripServer(Owner).GetRequest;
      FNtripRequest.FDiocpContext := self;
      FNtripRequest.Response.FDiocpContext := self;
      FNtripRequest.Clear;
      FNtripState := hsRequest;
    end;


    if self.FNtripState = hsRecvingSource then
    begin   // 直接接收NtripSource数据
      if Assigned(TDiocpNtripServer(FOwner).FOnDiocpRecvNtripSourceBuffer) then
      begin
        TDiocpNtripServer(FOwner).FOnDiocpRecvNtripSourceBuffer(Self, buf, len);
      end;
    end else if FNtripState = hsRecevingBodyData then
    begin
      if Assigned(TDiocpNtripServer(FOwner).FOnDiocpRecvNtripClientBuffer) then
      begin
        TDiocpNtripServer(FOwner).FOnDiocpRecvNtripClientBuffer(Self, buf, len);
      end;
    end else
    begin
      FRecvBuffer.AddBuffer(buf, len);

      j := FRecvBuffer.SearchBuffer(@__HEAD_END[0], 4);
      if j = -1 then
      begin      // 没有接收到头
        Exit;
      end;

      FNtripRequest.FRawHeaderData.Clear;
      FNtripRequest.FRawHeaderData.SetSize(j + 4);
      /// 写入到原始头，便于解码
      FRecvBuffer.readBuffer(FNtripRequest.FRawHeaderData.Memory, j + 4);

      if FNtripRequest.DecodeRequestHeader = 0 then
      begin
        self.RequestDisconnect('无效的Http协议数据', self);
        Exit;
      end;



      // 设置Context的挂机点
      Self.FMountPoint := FNtripRequest.FMountPoint;

      if SameText(FNtripRequest.FRequestMethod, 'SOURCE') then
      begin    // NtripSource进行认证
        if FNtripRequest.SourceRequestPass <> TDiocpNtripServer(FOwner).FNtripSourcePassword then
        begin
          FNtripRequest.Response.BadPassword;
          self.RequestDisconnect('NtripSource 接入密钥 验证失败', self);
          Exit;
        end else
        begin
          Self.FContextMode := ncmNtripSource;

          // 改变装入进入接收数据模式
          FNtripState := hsRecvingSource;


          TDiocpNtripServer(FOwner).AddNtripSource(FMountPoint, Self);

//          // 添加到NtripSource对应表中
//          TDiocpNtripServer(FOwner).FNtripSources.Lock;
//          TDiocpNtripServer(FOwner).FNtripSources.ValueMap[FMountPoint] := Self;
//          TDiocpNtripServer(FOwner).FNtripSources.unLock;

          // 回应请求
          FNtripRequest.Response.ICY200OK;


          j := FRecvBuffer.validCount;
          if j > 0 then
          begin

            SetLength(lvBuffer, j);
            FRecvBuffer.readBuffer(@lvBuffer[0], j);


            if Assigned(TDiocpNtripServer(FOwner).FOnDiocpRecvNtripSourceBuffer) then
            begin
              TDiocpNtripServer(FOwner).FOnDiocpRecvNtripSourceBuffer(Self, buf, len);
            end;
          end;
        end;
        // 清空所有
        FRecvBuffer.clearBuffer;
      end else
      begin   // client请求模式

        FContextMode := ncmNtripClient;

        lvIsNMEA := false;
        if Assigned(TDiocpNtripServer(FOwner).OnRequestAcceptEvent) then
        begin
          TDiocpNtripServer(FOwner).OnRequestAcceptEvent(FNtripRequest, lvIsNMEA);
        end;
        // 触发事件
        DoRequest(FNtripRequest);

        FNtripState := hsRecevingBodyData;

        j := FRecvBuffer.validCount;
        if j > 0 then
        begin

          SetLength(lvBuffer, j);
          FRecvBuffer.readBuffer(@lvBuffer[0], j);

          if Assigned(TDiocpNtripServer(FOwner).FOnDiocpRecvNtripClientBuffer) then
          begin
            TDiocpNtripServer(FOwner).FOnDiocpRecvNtripClientBuffer(Self, @lvBuffer[0], j);
          end;
        end;

        // 清空所有
        FRecvBuffer.clearBuffer;
      end;
    end;
  finally
    RecordWorkerEndTick;
  end;
end;

procedure TDiocpNtripClientContext.OnRequest(pvRequest:TDiocpNtripRequest);
begin

end;

procedure TDiocpNtripClientContext.RemoveNtripClient(
  pvContext: TDiocpNtripClientContext);
begin
  self.Lock;
  try
    FNtripClients.Remove(pvContext);
  finally
    self.UnLock;
  end;
end;

{ TDiocpNtripServer }

constructor TDiocpNtripServer.Create(AOwner: TComponent);
begin
  inherited;
  FRequestPool := TBaseQueue.Create;
  FNtripSources := TDHashTableSafe.Create();

  KeepAlive := false;
  RegisterContextClass(TDiocpNtripClientContext);
end;

destructor TDiocpNtripServer.Destroy;
begin
  SafeStop;
  
  FRequestPool.FreeDataObject;
  FRequestPool.Free;
  
  FNtripSources.Free;
  inherited Destroy;
end;



procedure TDiocpNtripServer.DoRequest(pvRequest: TDiocpNtripRequest);
begin
  if Assigned(FOnDiocpNtripRequest) then
  begin
    FOnDiocpNtripRequest(pvRequest);
  end;
end;

function TDiocpNtripServer.FindNtripSource(
  pvMountPoint: string): TDiocpNtripClientContext;
begin
  FNtripSources.Lock;
  Result := TDiocpNtripClientContext(FNtripSources.ValueMap[pvMountPoint]);
  FNtripSources.unLock();
end;

function TDiocpNtripServer.GetNtripSourceList(pvStrings:TStrings): Integer;
var
  lvList:TList;
  i: Integer;
begin
  lvList := TList.Create;
  try
    FNtripSources.Lock;
    try
      FNtripSources.GetDatas(lvList);

      Result := lvList.Count;
      for i := 0 to lvList.Count - 1 do
      begin
        pvStrings.Add(TDiocpNtripClientContext(lvList[i]).FMountPoint + ',' + IntToStr(IntPtr(lvList[i])));
      end;
    finally
      FNtripSources.unLock();
    end; 
  finally
    lvList.Free;
  end;
end;

function TDiocpNtripServer.GetNtripSourceList: String;
var
  lvStrs:TStrings;
begin
  lvStrs := TStringList.Create;
  try
    GetNtripSourceList(lvStrs);
    Result := lvStrs.Text;
  finally
    lvStrs.Free;
  end;
end;

function TDiocpNtripServer.GetRequest: TDiocpNtripRequest;
begin
  Result := TDiocpNtripRequest(FRequestPool.DeQueue);
  if Result = nil then
  begin
    Result := TDiocpNtripRequest.Create;
  end;
  Result.FDiocpNtripServer := Self;
end;

procedure TDiocpNtripServer.GiveBackRequest(pvRequest: TDiocpNtripRequest);
begin
  FRequestPool.EnQueue(pvRequest);
end;

procedure TDiocpNtripServer.AddNtripSource(pvMountPoint:String;
    pvContext:TIocpClientContext);
var
  lvContext:TIocpClientContext;
  s:string;
begin
  // 添加到NtripSource对应表中
  FNtripSources.Lock;
  try
    lvContext := TIocpClientContext(FNtripSources.ValueMap[pvMountPoint]);
    FNtripSources.ValueMap[pvMountPoint] := pvContext;
  finally
    FNtripSources.unLock;
  end;

  s := Format('+ NtripSource Bind, %s, %d, %s:%d',
    [pvMountPoint, IntPtr(pvContext), pvContext.RemoteAddr, pvContext.RemotePort]);
  AddDebugStrings(s);

  try
    if (lvContext <> nil) and (lvContext <> pvContext) then
    begin
      s := Format('= NtripSource kick out, %s, %d, now:%d',
        [pvMountPoint, IntPtr(lvContext), IntPtr(pvContext)]);
      AddDebugStrings(s);
      lvContext.RequestDisconnect(s);
    end;
  except
  end;
end;

procedure TDiocpNtripServer.RemoveNtripSource(pvMountPoint:String;
    pvContext:TIocpClientContext);
var
  lvRemove:Boolean;
  lvContext:TIocpClientContext;
begin
  FNtripSources.Lock;
  try
    lvContext :=TIocpClientContext(FNtripSources.ValueMap[pvMountPoint]);
    lvRemove := (lvContext = pvContext);
    if lvRemove then
       FNtripSources.ValueMap[pvMountPoint] := nil;
  finally
    FNtripSources.unLock;
  end;

  if FDebugState = 1 then
  begin
    if lvRemove then
    begin
      AddDebugStrings(Format('- NtripSource, %s, %d, %s:%d',
      [pvMountPoint, IntPtr(pvContext), pvContext.RemoteAddr, pvContext.RemotePort]));
    end else
    begin
      AddDebugStrings(Format('= NtripSource remove fail, maybe is kick out, %s, %d, %d',
        [pvMountPoint, IntPtr(pvContext), IntPtr(lvContext)]));
    end;
  end;
end;


initialization
  // double linebreak;
  __HEAD_END[0] := 13;
  __HEAD_END[1] := 10;
  __HEAD_END[2] := 13;
  __HEAD_END[3] := 10;

end.
