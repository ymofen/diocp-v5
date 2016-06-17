(*
  *	 Unit owner: D10.Mofen, delphi iocp framework author
  *         homePage: http://www.Diocp.org
  *	       blog: http://www.cnblogs.com/dksoft

  *   2015-02-22 08:29:43
  *     DIOCP-V5 发布

  *
  *   2015-04-08 12:34:33
  *    (感谢 (Xjumping  990669769)/(suoler)反馈bug和提供bug重现)
  *    改为异步处理Http请求后
  *      当连接已经关闭，但是请求还没有来得及处理，然后连接上下文已经归还到池，这个时候应该放弃处理任务()
  *
  *   2015-07-29 12:06:08
  *   diocp_ex_httpServer初步完成Cookie和Session
  *
  *
  *   2015-08-25 09:56:05
  *   修正TDiocpHttpRequest回归对象池时，进行清理对象。避免残留多余的Cookie对象。(感谢阿木反馈bug)
  *
  *
*)
unit diocp_ex_httpServer;

interface

/// 三个编译开关，只能开启一个
{.$DEFINE INNER_IOCP}     // iocp线程触发事件
{.$DEFINE  QDAC_QWorker} // 用qworker进行调度触发事件
{.$DEFINE DIOCP_Task}    // 用diocp_task进行调度触发事件


uses
  Classes, StrUtils, SysUtils, utils_buffer, utils_strings


  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp_task{$ENDIF}
  , diocp_tcp_server, utils_queues, utils_hashs, utils_dvalue
  , diocp_ex_http_common
  , utils_objectPool, utils_safeLogger, Windows, utils_threadinfo, SyncObjs;



const
  HTTPLineBreak = #13#10;
  SESSIONID = 'diocp_sid';

type
  TDiocpHttpState = (hsCompleted, hsRequest { 接收请求 } , hsRecvingPost { 接收数据 } );
  TDiocpHttpResponse = class;
  TDiocpHttpClientContext = class;
  TDiocpHttpServer = class;
  TDiocpHttpRequest = class;
  TDiocpHttpSession = class;

  TDiocpHttpCookie = diocp_ex_http_common.TDHttpCookie;

  TDiocpHttpSessionClass = class of TDiocpHttpSession;

{$IFDEF UNICODE}

  /// <summary>
  /// Request事件类型
  /// </summary>
  TOnDiocpHttpRequestEvent = reference to procedure(pvRequest: TDiocpHttpRequest);
{$ELSE}
  /// <summary>
  /// Request事件类型
  /// </summary>
  TOnDiocpHttpRequestEvent = procedure(pvRequest: TDiocpHttpRequest) of object;
{$ENDIF}

  /// <summary>
  ///  基础的Session类，用户可以自己扩展该类，然后注册
  /// </summary>
  TDiocpHttpSession = class(TObject)
  private
    FLastActivity: Integer; 
    procedure SetSessionTimeOut(const Value: Integer);
  protected
    FSessionTimeOut: Integer;
    procedure DoCleanup; virtual;
  public
    constructor Create; virtual;
    property LastActivity: Integer read FLastActivity;
    property SessionTimeOut: Integer read FSessionTimeOut write SetSessionTimeOut;

    /// <summary>
    ///  立即失效
    /// </summary>
    procedure Invalidate;
  end;

  /// <summary>
  ///   使用DValue存储数据的Session
  /// </summary>
  TDiocpHttpDValueSession = class(TDiocpHttpSession)
  private
    FDValues: TDValue;
  protected
    procedure DoCleanup; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property DValues: TDValue read FDValues;
  end;


  TDiocpHttpRequest = class(TObject)
  private
    FOwnerPool:TSafeQueue;
    
    FLocker:TCriticalSection;
    FThreadID:THandle;
    FThreadDebugInfo:String;
    
    FReleaseLater:Boolean;
    FReleaseLaterMsg:String;
    
    FSessionID : String;

    FInnerRequest:THttpRequest;

    /// <summary>
    ///   投递之前记录DNA，用于做异步任务时，是否取消当前任务
    /// </summary>
    FContextDNA : Integer;
    FDebugStrings: TStrings;

    /// <summary>
    ///   便于在Close时归还回对象池
    /// </summary>
    FDiocpHttpServer:TDiocpHttpServer;

    FDiocpContext: TDiocpHttpClientContext;



    FLastResponseContentLength: Integer;


    FResponse: TDiocpHttpResponse;

    /// <summary>
    ///   不再使用了，归还回对象池
    /// </summary>
    procedure Close;

    /// <summary>
    ///   检测Cookie中的SessionID信息
    ///   不创建Session对象
    /// </summary>
    procedure CheckCookieSession;
    function GetContentLength: Int64;
    function GetContentType: String;
    function GetContentAsMemory: PByte;
    function GetContentBody: TDBufferBuilder;
    function GetDataAsRawString: RAWString;
    function GetHeader: TDValue;
    function GetHttpVersion: Word;
    function GetContentDataLength: Integer;
    function GetHeaderAsMemory: PByte;
    function GetHeaderDataLength: Integer;
    function GetRequestAccept: String;
    function GetRequestAcceptEncoding: string;
    function GetRequestCookies: string;
    function GetRequestHost: string;
    function GetRequestMethod: string;
    function GetRequestParamsList: TDValue;
    function GetRequestRawHeaderString: string;
    function GetRequestRawURL: String;
    function GetRequestURI: String;
    function GetRequestURL: String;
    function GetRequestURLParamData: string;
    function GetURLParams: TDValue;
    procedure InnerAddToDebugStrings(pvMsg:String);
  protected
  public
    constructor Create;
    destructor Destroy; override;







    /// <summary>
    ///   设置Request暂时不进行释放
    /// </summary>
    /// <param name="pvMsg"> 信息(便于状态观察) </param>
    procedure SetReleaseLater(pvMsg:String);

    /// <summary>
    ///   获取当前Session
    ///    如果没有会进行创建    
    /// </summary>
    function GetSession: TDiocpHttpSession;

    /// <summary>
    ///   手动清理当前Session
    /// </summary>
    procedure RemoveSession;

    /// <summary>
    ///  获取当前会话ID, 如果没有会设置Response的Cookie信息
    /// </summary>
    function GetSessionID: String;


    /// <summary>
    ///   将Post的原始数据解码，放到参数列表中
    ///   在OnDiocpHttpRequest中调用
    /// </summary>
    procedure DecodePostDataParam(
      {$IFDEF UNICODE} pvEncoding:TEncoding {$ELSE}pvUseUtf8Decode:Boolean{$ENDIF});

    /// <summary>
    ///   解码URL中的参数，放到参数列表中
    ///   在OnDiocpHttpRequest中调用
    /// </summary>
    procedure DecodeURLParam(pvUseUtf8Decode:Boolean); overload;

    {$IFDEF UNICODE}
    /// <summary>
    ///   解码URL中的参数，放到参数列表中
    ///   在OnDiocpHttpRequest中调用
    /// </summary>
    procedure DecodeURLParam(pvEncoding:TEncoding); overload;
    {$ENDIF}

    /// <summary>
    ///   清理
    /// </summary>
    procedure Clear;

    /// <summary>
    ///  读取传入的Cookie值
    /// </summary>
    function GetCookie(pvCookieName:string):String;

    /// <summary>
    ///   将请求保存到流
    /// </summary>
    procedure SaveToStream(pvStream:TStream);

    /// <summary>
    ///   将请求保存到文件
    /// </summary>
    procedure SaveToFile(pvFile:string);

    procedure ContentSaveToFile(pvFile:String);

    property ContentType: String read GetContentType;

    property ContentLength: Int64 read GetContentLength;


    /// <summary>
    ///   与客户端建立的连接
    /// </summary>
    property Connection: TDiocpHttpClientContext read FDiocpContext;
    property ContentAsMemory: PByte read GetContentAsMemory;
    property ContentAsString: RAWString read GetDataAsRawString;

    property ContentBody: TDBufferBuilder read GetContentBody;
    /// <summary>
    ///   请求数据(ConentAsMemory)长度应该与Content-Length一致
    /// </summary>
    property ContentDataLength: Integer read GetContentDataLength;




    /// <summary>
    ///   请求头
    /// </summary>
    property Header: TDValue read GetHeader;

    property HttpVersion: Word read GetHttpVersion;

    property HeaderAsMemory: PByte read GetHeaderAsMemory;
    property HeaderDataLength: Integer read GetHeaderDataLength;

    property LastResponseContentLength: Integer read FLastResponseContentLength;



    property RequestAccept: String read GetRequestAccept;
    property RequestAcceptEncoding: string read GetRequestAcceptEncoding;
    property RequestCookies: string read GetRequestCookies;


    /// <summary>
    ///   从头信息解码器出来的Url,包含参数
    ///   URI + 参数
    /// </summary>
    property RequestURL: String read GetRequestURL;

    /// <summary>
    ///   从头信息提取出来的URL，未经过任何加工,包含参数
    /// </summary>
    property RequestRawURL: String read GetRequestRawURL;

    /// <summary>
    ///   不带URL参数
    /// </summary>
    property RequestURI: String read GetRequestURI;

    /// <summary>
    ///  从头信息中读取的请求服务器请求方式
    /// </summary>
    property RequestMethod: string read GetRequestMethod;

    /// <summary>
    ///   从头信息中读取的请求服务器IP地址
    /// </summary>
    property RequestHost: string read GetRequestHost;

    /// <summary>
    /// Http响应对象，回写数据
    /// </summary>
    property Response: TDiocpHttpResponse read FResponse;


    property RequestRawHeaderString: string read GetRequestRawHeaderString;

    /// <summary>
    ///  原始请求中的URL参数数据(没有经过URLDecode，因为在DecodeRequestHeader中要拼接RequestURL时临时进行了URLDecode)
    ///  没有经过URLDecode是考虑到参数值中本身存在&字符，导致DecodeURLParam出现不解码异常
    /// </summary>
    property RequestURLParamData: string read GetRequestURLParamData;

    /// <summary>
    ///   所有的请求参数， 注意调用前先调用DecodeURL和DecodePostParams
    /// </summary>
    property RequestParamsList: TDValue read GetRequestParamsList;
    property URLParams: TDValue read GetURLParams;

    

    procedure AddDebugStrings(pvDebugInfo: String; pvAddTimePre: Boolean = true);
    procedure CheckThreadIn;

    procedure CheckThreadSetInfo(pvDebugInfo:string);

    procedure CheckThreadOut;

    /// <summary>
    ///   应答完毕，发送会客户端
    ///   请尽量使用SendResponse和DoResponseEnd来代替
    /// </summary>
    procedure ResponseEnd;


    /// <summary>
    ///   本次应答完成, 检测是否需要关闭连接等相应工作
    /// </summary>
    procedure DoResponseEnd;


    /// <summary>
    ///   直接发送Response.Header和Data数据
    /// </summary>
    procedure SendResponse(pvContentLength: Integer = 0);

    /// <summary>
    ///   直接发送数据
    /// </summary>
    procedure SendResponseBuffer(pvBuffer:PByte; pvLen:Cardinal);



    /// <summary>
    ///  关闭连接
    /// </summary>
    procedure CloseContext;
    function GetDebugString: String;

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
    ///   获取响应的数据长度(不包含头信息)
    /// </summary>
    function GetResponseLength: Integer;

    








  end;

  TDiocpHttpResponse = class(TObject)
  private
    FCookieData : String;
    FDiocpContext : TDiocpHttpClientContext;
    
    FInnerResponse:THttpResponse;
    procedure ClearAllCookieObjects;
    function GetContentBody: TDBufferBuilder;
    function GetContentType: String;
    function GetHeader: TDValue;
    function GetHttpCodeStr: String;
    procedure SetContentType(const Value: String);
    procedure SetHttpCodeStr(const Value: String);
  public
    procedure Clear;
    procedure ClearContent;
    constructor Create;
    destructor Destroy; override;
    procedure WriteBuf(pvBuf: Pointer; len: Cardinal);
    procedure WriteString(pvString: string; pvUtf8Convert: Boolean = true);

    function GetResponseHeaderAsString: RAWString;

    function AddCookie: TDiocpHttpCookie; overload;

    procedure LoadFromFile(pvFile:string);

    function LoadFromStream(pvStream: TStream; pvSize: Integer): Integer;

    function AddCookie(pvName:String; pvValue:string): TDiocpHttpCookie; overload;

    function EncodeHeader: String;

    procedure EncodeResponseHeader(pvContentLength: Integer);

    procedure SaveToFile(pvFile:string);


    /// <summary>
    ///   与客户端建立的连接
    /// </summary>
    property Connection: TDiocpHttpClientContext read FDiocpContext;


    /// <summary>
    ///   不建议直接使用
    /// </summary>
    property ContentBody: TDBufferBuilder read GetContentBody;

    property ContentType: String read GetContentType write SetContentType;

    property Header: TDValue read GetHeader;

    property HttpCodeStr: String read GetHttpCodeStr write SetHttpCodeStr;

    procedure RedirectURL(pvURL:String);

    procedure GZipContent;

    procedure DeflateCompressContent;

    procedure ZLibContent;

    procedure SetChunkedStart;

    procedure SetChunkedEnd;

    procedure ChunkedFlush;

    procedure SetChunkedBuffer(pvBuffer:Pointer; pvLen:Integer);

    procedure SetChunkedUtf8(pvStr:string);
  end;

  /// <summary>
  /// Http 客户端连接
  /// </summary>
  TDiocpHttpClientContext = class(TIocpClientContext)
  private
    FHttpState: TDiocpHttpState;
    FCurrentRequest: TDiocpHttpRequest;
    {$IFDEF QDAC_QWorker}
    procedure OnExecuteJob(pvJob:PQJob);
    {$ENDIF}
    {$IFDEF DIOCP_Task}
    procedure OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
    {$ENDIF}

    // 执行事件
    procedure DoRequest(pvRequest:TDiocpHttpRequest);

  public
    constructor Create; override;
    destructor Destroy; override;
  protected
    /// <summary>
    /// 归还到对象池，进行清理工作
    /// </summary>
    procedure DoCleanUp; override;

    /// <summary>
    /// 接收到客户端的Http协议数据, 进行解码成TDiocpHttpRequest，响应Http请求
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: Word);
      override;
  end;



  /// <summary>
  /// Http 解析服务
  /// </summary>
  TDiocpHttpServer = class(TDiocpTcpServer)
  private
    FAccessXRequest: Boolean;

    FRequestPool: TSafeQueue;
    FSessionObjectPool: TObjectPool;
    FSessionList: TDHashTableSafe;
    FSessionClass : TDiocpHttpSessionClass;

    FOnDiocpHttpRequest: TOnDiocpHttpRequestEvent;
    FOnDiocpHttpRequestPostDone: TOnDiocpHttpRequestEvent;

    FLogicWorkerNeedCoInitialize: Boolean;
    FSessionTimeOut: Integer;

    /// <summary>
    /// 响应Http请求， 执行响应事件
    /// </summary>
    procedure DoRequest(pvRequest: TDiocpHttpRequest);

    /// <summary>
    ///   响应Post数据事件
    /// </summary>
    procedure DoRequestPostDataDone(pvRequest: TDiocpHttpRequest);

    /// <summary>
    ///   从池中获取一个对象
    /// </summary>
    function GetRequest: TDiocpHttpRequest;

    /// <summary>
    ///   还回一个对象
    /// </summary>
    procedure GiveBackRequest(pvRequest:TDiocpHttpRequest);

    /// <summary>
    ///   获取一个Session对象
    /// </summary>
    function GetSession(pvSessionID:string): TDiocpHttpSession;

    /// <summary>
    ///   移除掉一个Session，释放
    /// </summary>
    function RemoveSession(pvSessionID:String): Boolean;

    
    function GetSessionCount: Integer;

    function OnCreateSessionObject: TObject;

    /// <summary>
    ///   SessionMap删除的时候事件，归还到Session池
    /// </summary>
    procedure OnSessionRemove(pvData:Pointer);

  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    procedure RegisterSessionClass(pvClass:TDiocpHttpSessionClass);

    /// <summary>
    ///   允许跨域访问, 设置后，SendRespnose加入响应的响应头
    /// </summary>
    property AccessXRequest: Boolean read FAccessXRequest write FAccessXRequest;
    
    /// <summary>
    ///   获取Session总数
    /// </summary>
    property SessionCount: Integer read GetSessionCount;


    /// <summary>
    ///   当Http请求的Post数据完成后触发的事件
    ///   用来处理解码一些数据,比如Post的参数
    /// </summary>
    property OnDiocpHttpRequestPostDone: TOnDiocpHttpRequestEvent read
        FOnDiocpHttpRequestPostDone write FOnDiocpHttpRequestPostDone;

    /// <summary>
    /// 响应Http请求事件
    /// </summary>
    property OnDiocpHttpRequest: TOnDiocpHttpRequestEvent read FOnDiocpHttpRequest
        write FOnDiocpHttpRequest;

    /// <summary>
    ///   检查Session超时, 剔除超时的Session
    /// </summary>
    procedure CheckSessionTimeOut;

  published
    /// <summary>
    ///   处理逻辑线程执行逻辑前执行CoInitalize
    /// </summary>
    property LogicWorkerNeedCoInitialize: Boolean read FLogicWorkerNeedCoInitialize
        write FLogicWorkerNeedCoInitialize;
        
    property SessionTimeOut: Integer read FSessionTimeOut write FSessionTimeOut;


  end;



implementation

uses
  ComObj;

function FixHeader(const Header: string): string;
begin
  Result := Header;
  if (RightStr(Header, 4) <> #13#10#13#10) then
  begin
    if (RightStr(Header, 2) = #13#10) then
      Result := Result + #13#10
    else
      Result := Result + #13#10#13#10;
  end;
end;

function MakeHeader(const Status, pvRequestVersionStr: string; pvKeepAlive:
    Boolean; const ContType, Header: string; pvContextLength: Integer): string;
var
  lvVersionStr:string;
begin
  Result := '';

  lvVersionStr := pvRequestVersionStr;
  if lvVersionStr = '' then lvVersionStr := 'HTTP/1.0';

  if (Status = '') then
    Result := Result + lvVersionStr + ' 200 OK' + #13#10
  else
    Result := Result + lvVersionStr + ' ' + Status + #13#10;

  if (ContType = '') then
    Result := Result + 'Content-Type: text/html' + #13#10
  else
    Result := Result + 'Content-Type: ' + ContType + #13#10;

  if (pvContextLength > 0) then
    Result := Result + 'Content-Length: ' + IntToStr(pvContextLength) + #13#10;
  // Result := Result + 'Cache-Control: no-cache'#13#10;

  if pvKeepAlive then
    Result := Result + 'Connection: keep-alive'#13#10
  else
    Result := Result + 'Connection: close'#13#10;

  Result := Result + 'Server: DIOCP-V5/1.0'#13#10;

end;

procedure TDiocpHttpRequest.Clear;
begin
  FResponse.Clear;
  FReleaseLater := false;
  FInnerRequest.DoCleanUp;
end;

procedure TDiocpHttpRequest.Close;
begin

  if FDiocpHttpServer = nil then
  begin
    if IsDebugMode then
    begin
      Assert(False, 'FDiocpHttpServer is nil');
    end;
    exit;
  end;

  FDiocpHttpServer.GiveBackRequest(Self);
end;

procedure TDiocpHttpRequest.CloseContext;
begin
  FDiocpContext.PostWSACloseRequest();
end;

function TDiocpHttpRequest.GetCookie(pvCookieName: string): String;
var
  lvCookie:TDiocpHttpCookie;
begin
  lvCookie := FResponse.FInnerResponse.GetCookie(pvCookieName);
  if lvCookie <> nil then
  begin
    Result := lvCookie.Value;
  end else
  begin
    Result := FInnerRequest.GetCookie(pvCookieName);
  end;
end;

function TDiocpHttpRequest.GetRequestParam(ParamsKey: string): string;
begin
  Result := FInnerRequest.RequestParams.GetValueByName(ParamsKey, '');
end;

constructor TDiocpHttpRequest.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create;
  FDebugStrings := TStringList.Create;
  FInnerRequest := THttpRequest.Create;
  FResponse := TDiocpHttpResponse.Create();

  //FRequestParamsList := TStringList.Create; // TODO:创建存放http参数的StringList
end;

destructor TDiocpHttpRequest.Destroy;
begin
  FreeAndNil(FResponse);
  FDebugStrings.Free;

  //FreeAndNil(FRequestParamsList); // TODO:释放存放http参数的StringList

  FInnerRequest.Free;

  FLocker.Free;

  inherited Destroy;
end;

procedure TDiocpHttpRequest.AddDebugStrings(pvDebugInfo: String; pvAddTimePre:
    Boolean = true);
var
  s:string;
begin
  if pvAddTimePre then s := Format('[%s]:%s', [NowString, pvDebugInfo])
  else s := pvDebugInfo;
  FLocker.Enter();
  try
    InnerAddToDebugStrings(s);
  finally
    FLocker.Leave;
  end;
end;

procedure TDiocpHttpRequest.DoResponseEnd;
begin
  if not FInnerRequest.CheckKeepAlive then
  begin
    FDiocpContext.PostWSACloseRequest;
  end;    
end;

procedure TDiocpHttpRequest.CheckCookieSession;
begin
  // 对session的处理
  FSessionID := GetCookie(SESSIONID);
  if FSessionID = '' then
  begin
    FSessionID := SESSIONID + '_' + DeleteChars(CreateClassID, ['-', '{', '}']);
    Response.AddCookie(SESSIONID, FSessionID);
  end;
end;

procedure TDiocpHttpRequest.CheckThreadIn;
var
  s:string;
begin
  if FThreadID <> 0 then
  begin
    s := GetDebugString;
    raise Exception.CreateFmt('(%d,%d)当前对象已经被其他线程正在使用::%s',
       [utils_strings.GetCurrentThreadID, FThreadID, s]);
  end;
  FThreadID := utils_strings.GetCurrentThreadID;
end;

procedure TDiocpHttpRequest.CheckThreadOut;
begin
  FThreadID := 0;  
end;

procedure TDiocpHttpRequest.CheckThreadSetInfo(pvDebugInfo:string);
var
  lvThreadID:THandle;
  s:string;
begin
  lvThreadID := utils_strings.GetCurrentThreadID;
  if lvThreadID <> FThreadID then
  begin
    s := GetDebugString;
    raise Exception.CreateFmt('(%d,%d)当前对象已经被其他线程正在使用::%s',
      [utils_strings.GetCurrentThreadID, FThreadID, s]);
  end;
  FThreadDebugInfo := pvDebugInfo;
end;

procedure TDiocpHttpRequest.ContentSaveToFile(pvFile:String);
begin
  FInnerRequest.ContentSaveToFile(pvFile);
end;

procedure TDiocpHttpRequest.DecodePostDataParam({$IFDEF UNICODE} pvEncoding:TEncoding {$ELSE}pvUseUtf8Decode:Boolean{$ENDIF});
begin
  {$IFDEF UNICODE}
  FInnerRequest.DecodeContentAsFormUrlencoded(pvEncoding);
  {$ELSE}
  FInnerRequest.DecodeContentAsFormUrlencoded(pvUseUtf8Decode);
  {$ENDIF}
end;

{$IFDEF UNICODE}
procedure TDiocpHttpRequest.DecodeURLParam(pvEncoding:TEncoding);
begin
  FInnerRequest.DecodeURLParam(pvEncoding);
end;
{$ENDIF}

procedure TDiocpHttpRequest.DecodeURLParam(pvUseUtf8Decode:Boolean);
begin
  FInnerRequest.DecodeURLParam(pvUseUtf8Decode);
end;

function TDiocpHttpRequest.GetContentLength: Int64;
begin
  Result := FInnerRequest.ContentLength;
end;

function TDiocpHttpRequest.GetContentType: String;
begin
  Result := FInnerRequest.ContentType;
end;

function TDiocpHttpRequest.GetContentAsMemory: PByte;
begin
  Result := FInnerRequest.ContentAsMemory;
end;

function TDiocpHttpRequest.GetContentBody: TDBufferBuilder;
begin
  Result := FInnerRequest.ContentBody;
end;

function TDiocpHttpRequest.GetDataAsRawString: RAWString;
begin
  Result := FInnerRequest.ContentAsRAWString;
end;

function TDiocpHttpRequest.GetHeader: TDValue;
begin
  Result := FInnerRequest.Headers;
end;

function TDiocpHttpRequest.GetHttpVersion: Word;
begin
  Result := FInnerRequest.HttpVersionValue;
end;

function TDiocpHttpRequest.GetContentDataLength: Integer;
begin
  Result := FInnerRequest.ContentBody.Size;
end;

function TDiocpHttpRequest.GetDebugString: String;
begin
  FLocker.Enter();
  try
    Result := FDebugStrings.Text;
  finally
    FLocker.Leave;
  end;
end;

function TDiocpHttpRequest.GetHeaderAsMemory: PByte;
begin
  Result := FInnerRequest.HeaderAsMermory;
end;

function TDiocpHttpRequest.GetHeaderDataLength: Integer;
begin
  Result := FInnerRequest.HeaderDataLength;
end;

function TDiocpHttpRequest.GetRequestAccept: String;
begin
  Result := FInnerRequest.Headers.GetValueByName('Accept', '');
end;

function TDiocpHttpRequest.GetRequestAcceptEncoding: string;
begin
  Result := FInnerRequest.Headers.GetValueByName('Accept-Encoding', '');
end;

function TDiocpHttpRequest.GetRequestCookies: string;
begin  
  Result := FInnerRequest.RawCookie;
end;

function TDiocpHttpRequest.GetRequestHost: string;
begin
  Result := FInnerRequest.Headers.GetValueByName('Host', '');
end;

function TDiocpHttpRequest.GetRequestMethod: string;
begin
  Result := FInnerRequest.Method;
end;

function TDiocpHttpRequest.GetRequestParamsList: TDValue;
begin
  Result := FInnerRequest.RequestParams;
end;

function TDiocpHttpRequest.GetRequestRawHeaderString: string;
begin
  Result := FInnerRequest.RawHeader;
end;

function TDiocpHttpRequest.GetRequestRawURL: String;
begin
  Result := FInnerRequest.RequestRawURL;
end;

function TDiocpHttpRequest.GetRequestURI: String;
begin
  Result := FInnerRequest.RequestURI;
end;

function TDiocpHttpRequest.GetRequestURL: String;
begin
  Result := FInnerRequest.RequestURL;
end;

function TDiocpHttpRequest.GetRequestURLParamData: string;
begin
  Result := FInnerRequest.RequestRawURLParamStr;
end;

function TDiocpHttpRequest.GetResponseLength: Integer;
begin
  Result := FResponse.FInnerResponse.ContentBuffer.Length;
end;

procedure TDiocpHttpRequest.RemoveSession;
begin
  CheckCookieSession;
  TDiocpHttpServer(Connection.Owner).RemoveSession(FSessionID);
end;

function TDiocpHttpRequest.GetSession: TDiocpHttpSession;
begin
  CheckCookieSession;
  Result := TDiocpHttpServer(Connection.Owner).GetSession(FSessionID);
end;

function TDiocpHttpRequest.GetSessionID: String;
begin
  CheckCookieSession;
  Result := FSessionID;
end;

function TDiocpHttpRequest.GetURLParams: TDValue;
begin
  Result := FInnerRequest.URLParams;
end;

procedure TDiocpHttpRequest.InnerAddToDebugStrings(pvMsg:String);
begin
  FDebugStrings.Add(pvMsg);
  if FDebugStrings.Count > 500 then FDebugStrings.Delete(0);
end;

procedure TDiocpHttpRequest.ResponseEnd;
var
  lvFixedHeader: AnsiString;
  len: Integer;
begin       
  lvFixedHeader := FResponse.EncodeHeader;

  if (lvFixedHeader <> '') then
    lvFixedHeader := FixHeader(lvFixedHeader)
  else
    lvFixedHeader := lvFixedHeader + HTTPLineBreak;

  // FResponseSize必须准确指定发送的数据包大小
  // 用于在发送完之后(Owner.TriggerClientSentData)断开客户端连接
  if lvFixedHeader <> '' then
  begin
    len := Length(lvFixedHeader);
    FDiocpContext.PostWSASendRequest(PAnsiChar(lvFixedHeader), len);
  end;

  if FResponse.FInnerResponse.ContentBuffer.Length > 0 then
  begin
    FDiocpContext.PostWSASendRequest(FResponse.FInnerResponse.ContentBuffer.Memory,
      FResponse.FInnerResponse.ContentBuffer.Length);
  end;

  if not FInnerRequest.CheckKeepAlive then
  begin
    FDiocpContext.PostWSACloseRequest;
  end;
end;

procedure TDiocpHttpRequest.SaveToFile(pvFile:string);
var
  lvFileStream:TFileStream;
begin
  lvFileStream := TFileStream.Create(pvFile, fmCreate);
  try
    SaveToStream(lvFileStream);  
  finally
    lvFileStream.Free;
  end;

end;

procedure TDiocpHttpRequest.SaveToStream(pvStream:TStream);
begin
  pvStream.WriteBuffer(FInnerRequest.HeaderAsMermory^, FInnerRequest.HeaderDataLength);
  if FInnerRequest.ContentLength > 0 then
  begin
    pvStream.WriteBuffer(FInnerRequest.ContentAsMemory^, FInnerRequest.ContentLength);
  end;

end;

procedure TDiocpHttpRequest.SendResponse(pvContentLength: Integer = 0);
var
  len: Integer;
  s:RAWString;
begin
  if FResponse.Header.FindByName('Connection') = nil then
  begin
    if FInnerRequest.CheckKeepAlive then
    begin
      FResponse.Header.ForceByName('Connection').AsString := 'keep-alive';
    end else
    begin
      FResponse.Header.ForceByName('Connection').AsString := 'close';
    end;
  end;

  if FDiocpHttpServer.FAccessXRequest then
  begin  // 跨域访问支持
    FResponse.Header.ForceByName('Access-Control-Allow-Origin').AsString := '*';
    FResponse.Header.ForceByName('Access-Control-Allow-Methods').AsString := 'POST, GET, OPTIONS, DELETE';
    FResponse.Header.ForceByName('Access-Control-Allow-Headers').AsString := 'x-requested-with,content-type';
  end;
  
  if pvContentLength = 0 then
  begin
    FLastResponseContentLength := FResponse.FInnerResponse.ContentBuffer.Length;
    FResponse.EncodeResponseHeader(FLastResponseContentLength);
  end else
  begin
    FLastResponseContentLength := pvContentLength;
    FResponse.EncodeResponseHeader(pvContentLength);
  end;

  if FResponse.FInnerResponse.HeaderBuilder.Size = 0 then
  begin
    Assert(False, '响应数据为空');
  end;
  
  FDiocpContext.PostWSASendRequest(FResponse.FInnerResponse.HeaderBuilder.Memory,
    FResponse.FInnerResponse.HeaderBuilder.Size);


  if FResponse.FInnerResponse.ContentBuffer.Length > 0 then
  begin
    FDiocpContext.PostWSASendRequest(FResponse.FInnerResponse.ContentBuffer.Memory,
      FLastResponseContentLength);
  end;

end;

procedure TDiocpHttpRequest.SendResponseBuffer(pvBuffer:PByte; pvLen:Cardinal);
begin
  FDiocpContext.PostWSASendRequest(pvBuffer, pvLen);
end;

procedure TDiocpHttpRequest.SetReleaseLater(pvMsg:String);
begin
  FReleaseLater := True;
  FReleaseLaterMsg := pvMsg;
end;

procedure TDiocpHttpResponse.ChunkedFlush;
begin
  FDiocpContext.PostWSASendRequest(FInnerResponse.ContentBuffer.Memory, FInnerResponse.ContentBuffer.Length);
  FInnerResponse.ContentBuffer.Clear;
end;

procedure TDiocpHttpResponse.Clear;
begin
  FInnerResponse.DoCleanUp;
end;

constructor TDiocpHttpResponse.Create;
begin
  inherited Create;
  FInnerResponse := THttpResponse.Create;
end;

destructor TDiocpHttpResponse.Destroy;
begin
  Clear;
  FInnerResponse.Free;
  inherited Destroy;
end;

function TDiocpHttpResponse.AddCookie: TDiocpHttpCookie;
begin
  Result := FInnerResponse.AddCookie;
end;

function TDiocpHttpResponse.AddCookie(pvName:String; pvValue:string):
    TDiocpHttpCookie;
begin
  Result := FInnerResponse.AddCookie(pvName, pvValue);
end;

function TDiocpHttpResponse.EncodeHeader: String;
begin    
  FInnerResponse.EncodeHeader(FInnerResponse.ContentBuffer.Length);
  Result := FInnerResponse.HeaderBuilder.ToRAWString;
end;

procedure TDiocpHttpResponse.ClearAllCookieObjects;
begin
  FInnerResponse.ClearCookies;
end;

procedure TDiocpHttpResponse.ClearContent;
begin
  FInnerResponse.ContentBuffer.Clear;
end;

procedure TDiocpHttpResponse.EncodeResponseHeader(pvContentLength: Integer);
begin
  FInnerResponse.EncodeHeader(pvContentLength);
end;

function TDiocpHttpResponse.GetContentType: String;
begin
  Result := FInnerResponse.ContentType;
end;

function TDiocpHttpResponse.GetHeader: TDValue;
begin
  Result := FInnerResponse.Headers;
end;

function TDiocpHttpResponse.GetHttpCodeStr: String;
begin
  Result := FInnerResponse.ResponseCodeStr;
end;

procedure TDiocpHttpResponse.GZipContent;
begin
  FInnerResponse.GZipContent;
end;

procedure TDiocpHttpResponse.RedirectURL(pvURL: String);
var
  lvFixedHeader: AnsiString;
  len: Integer;
begin
  //lvFixedHeader := MakeHeader('302 Temporarily Moved', 'HTTP/1.0', false, '', '', 0);
  lvFixedHeader := MakeHeader('307 Temporary Redirect', 'HTTP/1.0', false, '', '', 0);

  lvFixedHeader := lvFixedHeader + 'Location: ' + pvURL + HTTPLineBreak;

  lvFixedHeader := FixHeader(lvFixedHeader);

  len := Length(lvFixedHeader);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvFixedHeader), len);
end;

procedure TDiocpHttpResponse.SetChunkedBuffer(pvBuffer:Pointer; pvLen:Integer);
begin
  FInnerResponse.ChunkedBuffer(pvBuffer, pvLen);
end;

procedure TDiocpHttpResponse.SetChunkedEnd;
begin
  FInnerResponse.ChunkedBufferEnd;
end;

procedure TDiocpHttpResponse.SetChunkedStart;
begin
  FInnerResponse.ChunkedBufferStart;
end;

procedure TDiocpHttpResponse.SetChunkedUtf8(pvStr:string);
var
  lvBytes:TBytes;
begin
  lvBytes := StringToUtf8Bytes(pvStr);
  FInnerResponse.ChunkedBuffer(@lvBytes[0], length(lvBytes));
end;

procedure TDiocpHttpResponse.SetContentType(const Value: String);
begin
  FInnerResponse.ContentType := Value;
end;

procedure TDiocpHttpResponse.SetHttpCodeStr(const Value: String);
begin
  FInnerResponse.ResponseCodeStr := Value;
end;

procedure TDiocpHttpResponse.WriteBuf(pvBuf: Pointer; len: Cardinal);
begin
  FInnerResponse.ContentBuffer.AppendBuffer(PByte(pvBuf), len);
end;

procedure TDiocpHttpResponse.WriteString(pvString: string; pvUtf8Convert:
    Boolean = true);
var
  lvRawString: AnsiString;
begin
  if pvUtf8Convert then
  begin     // 进行Utf8转换
    FInnerResponse.ContentBuffer.AppendUtf8(pvString);
  end else
  begin
    lvRawString := pvString;
    FInnerResponse.ContentBuffer.AppendBuffer(PByte(lvRawString), Length(lvRawString));
  end;
end;

procedure TDiocpHttpResponse.DeflateCompressContent;
begin
  FInnerResponse.DeflateCompressContent
end;

function TDiocpHttpResponse.GetContentBody: TDBufferBuilder;
begin
  Result := FInnerResponse.ContentBuffer;
end;

function TDiocpHttpResponse.GetResponseHeaderAsString: RAWString;
begin
  Result := FInnerResponse.HeaderBuilder.ToRAWString;
end;

procedure TDiocpHttpResponse.LoadFromFile(pvFile:string);
begin
  FInnerResponse.ContentBuffer.LoadFromFile(pvFile);
end;

function TDiocpHttpResponse.LoadFromStream(pvStream: TStream; pvSize: Integer):
    Integer;
begin
  Result := FInnerResponse.ContentBuffer.CopyFrom(pvStream, pvSize);
end;

procedure TDiocpHttpResponse.SaveToFile(pvFile:string);
var
  lvStream:TFileStream;
begin
  lvStream := TFileStream.Create(pvFile, fmCreate);
  try
    lvStream.WriteBuffer(FInnerResponse.HeaderBuilder.Memory^, FInnerResponse.HeaderBuilder.Size);
    lvStream.WriteBuffer(FInnerResponse.ContentBuffer.Memory^, FInnerResponse.ContentBuffer.Size);
  finally
    lvStream.Free;
  end;

end;

procedure TDiocpHttpResponse.ZLibContent;
begin
  FInnerResponse.ZCompressContent;
end;

constructor TDiocpHttpClientContext.Create;
begin
  inherited Create;
end;

destructor TDiocpHttpClientContext.Destroy;
begin
  inherited Destroy;
end;

procedure TDiocpHttpClientContext.DoCleanUp;
begin                                              
  inherited;
  FHttpState := hsCompleted;
  if FCurrentRequest <> nil then
  begin
    FCurrentRequest.Close;
    FCurrentRequest := nil;
  end;
end;

procedure TDiocpHttpClientContext.DoRequest(pvRequest: TDiocpHttpRequest);
begin
   {$IFDEF QDAC_QWorker}
   Workers.Post(OnExecuteJob, pvRequest);
   {$ELSE}
     {$IFDEF DIOCP_TASK}
     iocpTaskManager.PostATask(OnExecuteJob, pvRequest);
     {$ELSE}
      try
        pvRequest.AddDebugStrings('DoRequest::' + pvRequest.RequestURI);

        pvRequest.CheckThreadIn;

        // 如果需要执行
        if TDiocpHttpServer(FOwner).LogicWorkerNeedCoInitialize then
          FRecvRequest.IocpWorker.checkCoInitializeEx();

        // 直接触发事件
        TDiocpHttpServer(FOwner).DoRequest(pvRequest);
      finally
        // 归还HttpRequest到池
        if not pvRequest.FReleaseLater then
        begin
          pvRequest.CheckThreadOut;
          pvRequest.Close;
        end;
      end;
     {$ENDIF}
   {$ENDIF}
end;

{$IFDEF QDAC_QWorker}
procedure TDiocpHttpClientContext.OnExecuteJob(pvJob:PQJob);
var
  lvObj:TDiocpHttpRequest;
begin
  // 无论如何都要归还HttpRequest到池
  lvObj := TDiocpHttpRequest(pvJob.Data);
  try
     // 连接已经断开, 放弃处理逻辑
     if (Self = nil) then Exit;

     // 连接已经断开, 放弃处理逻辑
     if (FOwner = nil) then Exit;

     lvObj.CheckThreadIn;

     // 已经不是当时请求的连接， 放弃处理逻辑
     if lvObj.FContextDNA <> self.ContextDNA then
     begin
       Exit;
     end;

     if Self.LockContext('HTTP逻辑处理...', Self) then
     try
       // 触发事件
       TDiocpHttpServer(FOwner).DoRequest(lvObj);
     finally
       self.UnLockContext('HTTP逻辑处理...', Self);
     end;
  finally
    // 归还HttpRequest到池
    if not lvObj.FReleaseLater then
    begin
      lvObj.CheckThreadOut;
      lvObj.Close;
    end;
  end;
end;

{$ENDIF}

{$IFDEF DIOCP_Task}
procedure TDiocpHttpClientContext.OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
var
  lvObj:TDiocpHttpRequest;
begin
  lvObj := TDiocpHttpRequest(pvTaskRequest.TaskData);
  try
    // 连接已经断开, 放弃处理逻辑
    if (Self = nil) then Exit;

    // 连接已经断开, 放弃处理逻辑
    if (FOwner = nil) then Exit;

    lvObj.CheckThreadIn;

    // 如果需要执行
    if TDiocpHttpServer(FOwner).LogicWorkerNeedCoInitialize then
      pvTaskRequest.iocpWorker.checkCoInitializeEx();

     // 已经不是当时请求的连接， 放弃处理逻辑
     if lvObj.FContextDNA <> self.ContextDNA then
     begin
       Exit;
     end;

     if Self.LockContext('HTTP逻辑处理...', Self) then
     try
       // 触发事件
       TDiocpHttpServer(FOwner).DoRequest(lvObj);
     finally
       self.UnLockContext('HTTP逻辑处理...', Self);
     end;
  finally
    // 归还HttpRequest到池
    if not lvObj.FReleaseLater then
    begin
      lvObj.CheckThreadOut;
      lvObj.Close;
    end;
  end;

end;
{$ENDIF}



procedure TDiocpHttpClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: Word);
var
  lvTmpBuf: PByte;
  lvRemain: Cardinal;
  r:Integer;
  lvTempRequest: TDiocpHttpRequest;
begin
  inherited;
  lvTmpBuf := PByte(buf);
  lvRemain := len;
  while (lvRemain > 0) do
  begin
    if FCurrentRequest = nil then
    begin
      FCurrentRequest := TDiocpHttpServer(Owner).GetRequest;
      FCurrentRequest.FDiocpContext := self;
      FCurrentRequest.Response.FDiocpContext := self;
      FCurrentRequest.Clear;

      // 记录当前contextDNA，异步任务时组做检测
      FCurrentRequest.FContextDNA := self.ContextDNA;
    end;
    
    r := FCurrentRequest.FInnerRequest.InputBuffer(lvTmpBuf^);
    if r = -1 then
    begin

      lvTempRequest := FCurrentRequest;
      try
        FCurrentRequest := nil;
        lvTempRequest.Response.FInnerResponse.ResponseCode := 400;
        lvTempRequest.Response.WriteString(PAnsiChar(lvTmpBuf) + '<BR>******<BR>******<BR>' + PAnsiChar(buf));
        lvTempRequest.ResponseEnd;
      finally
        lvTempRequest.Close;
      end;


      //self.RequestDisconnect('无效的Http请求', self);
      Exit;
    end;

    if r = -2 then
    begin
      self.RequestDisconnect('HTTP请求头数据过大', self);
      Exit;
    end;

    if r = 0 then
    begin
      ; //需要更多的数据解码
    end else
    if r = 1 then
    begin
      if SameText(FCurrentRequest.FInnerRequest.Method, 'POST') or
          SameText(FCurrentRequest.FInnerRequest.Method, 'PUT') then
      begin
        if FCurrentRequest.FInnerRequest.ContentLength = 0 then
        begin
          self.RequestDisconnect('无效的POST/PUT请求数据', self);
          Exit;
        end;
      end else
      begin
        lvTempRequest := FCurrentRequest;

        // 避免断开后还回对象池，造成重复还回
        FCurrentRequest := nil;

        DoRequest(lvTempRequest);
      end;
    end else
    if r = 2 then
    begin
      lvTempRequest := FCurrentRequest;

      // 避免断开后还回对象池，造成重复还回
      FCurrentRequest := nil;

      // 触发事件
      DoRequest(lvTempRequest);
    end;   

    Dec(lvRemain);
    Inc(lvTmpBuf);
  end;
end;

{ TDiocpHttpServer }

constructor TDiocpHttpServer.Create(AOwner: TComponent);
begin
  inherited;
  FRequestPool := TSafeQueue.Create;
  FSessionObjectPool := TObjectPool.Create(OnCreateSessionObject);
  FSessionList := TDHashTableSafe.Create;
  FSessionList.OnDelete := OnSessionRemove;
  FSessionTimeOut := 300;  // five miniutes
  KeepAlive := false;
  RegisterContextClass(TDiocpHttpClientContext);
  RegisterSessionClass(TDiocpHttpDValueSession);
end;

destructor TDiocpHttpServer.Destroy;
begin
  FRequestPool.FreeDataObject;
  FRequestPool.Free;

  /// 只需要清理，清理时会归还到Session对象池
  FSessionList.Clear;
  FSessionList.Free;

  FSessionObjectPool.WaitFor(10000);
  FSessionObjectPool.Free;
  inherited;
end;

procedure TDiocpHttpServer.CheckSessionTimeOut;
begin
  ;
end;

procedure TDiocpHttpServer.DoRequest(pvRequest: TDiocpHttpRequest);
var
  lvMsg:String;
begin
  try
    pvRequest.Connection.SetRecvWorkerHint('进入Http::DoRequest');
    SetCurrentThreadInfo('进入Http::DoRequest');
    try
      try
        pvRequest.CheckCookieSession;

        if Assigned(FOnDiocpHttpRequest) then
        begin
          pvRequest.Connection.SetRecvWorkerHint('DoRequest::FOnDiocpHttpRequest - 1');
          FOnDiocpHttpRequest(pvRequest);
          pvRequest.Connection.SetRecvWorkerHint('DoRequest::FOnDiocpHttpRequest - 1');
        end;
      except
        on E:Exception do
        begin
          pvRequest.Connection.SetRecvWorkerHint('DoRequest::Exception - 1');
          self.LogMessage('Http逻辑处理异常:%s', [e.Message], '', lgvError);
          pvRequest.FReleaseLater := False;
          pvRequest.Response.FInnerResponse.ResponseCode := 500;
          pvRequest.Response.Clear;
          pvRequest.Response.ContentType := 'text/html; charset=utf-8';
          lvMsg := e.Message;
          lvMsg := StringReplace(lvMsg, sLineBreak, '<BR>', [rfReplaceAll]);
          pvRequest.Response.WriteString(lvMsg);
          pvRequest.Connection.SetRecvWorkerHint('DoRequest::Exception - 2');
        end;
      end;
    except
      on E:Exception do
      begin
        pvRequest.Connection.SetRecvWorkerHint('DoRequest::*Exception - 1');
        self.LogMessage('*Http逻辑处理异常:%s', [e.Message], CORE_LOG_FILE, lgvError);
        pvRequest.Connection.SetRecvWorkerHint('DoRequest::*Exception - 2');
      end;
    end;
  finally
    SetCurrentThreadInfo('结束Http::DoRequest');
    pvRequest.Connection.SetRecvWorkerHint('DoRequest:: end');
  end;
end;

procedure TDiocpHttpServer.DoRequestPostDataDone(pvRequest: TDiocpHttpRequest);
begin 
  if Assigned(FOnDiocpHttpRequestPostDone) then
  begin
    FOnDiocpHttpRequestPostDone(pvRequest);
  end;
end;

function TDiocpHttpServer.GetRequest: TDiocpHttpRequest;
begin
  Result := TDiocpHttpRequest(FRequestPool.DeQueue);
  if Result = nil then
  begin
    Result := TDiocpHttpRequest.Create;
  end;
  Result.AddDebugStrings('+ GetRequest');
  Result.FDiocpHttpServer := Self;
  Result.FOwnerPool := FRequestPool;
  Result.Clear;
end;

function TDiocpHttpServer.GetSession(pvSessionID:string): TDiocpHttpSession;
begin
  FSessionList.Lock;
  try
    Result := TDiocpHttpSession(FSessionList.ValueMap[pvSessionID]);
    if Result = nil then
    begin
      Result := TDiocpHttpSession(FSessionObjectPool.GetObject);
      Result.DoCleanup;
      Result.SessionTimeOut := self.FSessionTimeOut;
      FSessionList.ValueMap[pvSessionID] := Result;
    end;
    Result.FLastActivity := GetTickCount;
  finally
    FSessionList.unLock;
  end;
end;

function TDiocpHttpServer.GetSessionCount: Integer;
begin
  Result := FSessionList.Count;
end;

procedure TDiocpHttpServer.GiveBackRequest(pvRequest: TDiocpHttpRequest);
var
  s:String;
begin
  if pvRequest.FDiocpHttpServer = nil then
  begin
    s := GetDebugString;
    Assert(pvRequest.FDiocpHttpServer <> nil,
      'TDiocpHttpServer.GiveBackRequest::对象重复关闭:' + s);
  end;
  pvRequest.Clear;
  pvRequest.AddDebugStrings('- GiveBackRequest');
  pvRequest.FOwnerPool := nil;
  pvRequest.FDiocpHttpServer := nil;
  FRequestPool.EnQueue(pvRequest);
end;

function TDiocpHttpServer.OnCreateSessionObject: TObject;
begin
  if FSessionClass = nil then raise Exception.Create('尚未注册SessionClass, 不能获取Session');
  Result := FSessionClass.Create();
end;

procedure TDiocpHttpServer.OnSessionRemove(pvData: Pointer);
begin
  try
    // 清理Session
    TDiocpHttpSession(pvData).DoCleanup();
  except
    on E:Exception do
    begin
      LogMessage('Session DoCleanUp Error:' + e.Message, 'rpc_exception', lgvError);
    end;
  end;
  FSessionObjectPool.ReleaseObject(TObject(pvData));
end;

procedure TDiocpHttpServer.RegisterSessionClass(pvClass:TDiocpHttpSessionClass);
begin
  FSessionClass := pvClass;
end;

function TDiocpHttpServer.RemoveSession(pvSessionID:String): Boolean;
var
  lvSession:TDiocpHttpSession;
begin
  FSessionList.Lock;
  try
    lvSession := TDiocpHttpSession(FSessionList.ValueMap[pvSessionID]);
    if lvSession <> nil then
    begin
      // 会触发OnSessionRemove, 归还对应的对象到池
      Result := FSessionList.Remove(pvSessionID);

    end else
    begin
      Result := false;
    end;
  finally
    FSessionList.unLock;
  end;  
end;

constructor TDiocpHttpSession.Create;
begin
  FSessionTimeOut := 300;
end;

constructor TDiocpHttpDValueSession.Create;
begin
  inherited Create;
  FDValues := TDValue.Create();
end;

destructor TDiocpHttpDValueSession.Destroy;
begin
  FDValues.Free;
  inherited Destroy;
end;



procedure TDiocpHttpDValueSession.DoCleanup;
begin
  inherited;
  FDValues.Clear;
end;

procedure TDiocpHttpSession.DoCleanup;
begin

end;

procedure TDiocpHttpSession.Invalidate;
begin
  DoCleanup;
end;

procedure TDiocpHttpSession.SetSessionTimeOut(const Value: Integer);
begin
  FSessionTimeOut := Value;
end;

end.
