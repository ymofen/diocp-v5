(*
  *	 Unit owner: D10.Mofen, delphi iocp framework author
  *         homePage: http://www.Diocp.org
  *	       blog: http://www.cnblogs.com/dksoft

  *   2015-02-22 08:29:43
  *     DIOCP-V5 发布

  *    Http协议处理单元
  *    其中大部分思路来自于delphi iocp framework中的iocp.HttpServer
  *
  *   2015-04-08 12:34:33
  *    (感谢 (Xjumping  990669769)/(suoler)反馈bug和提供bug重现)
  *    改为异步处理Http请求后
  *      当连接已经关闭，但是请求还没有来得及处理，然后连接上下文已经归还到池，这个时候应该放弃处理任务()
  *
  *   2015-07-29 12:06:08
  *   diocp.ex.httpServer初步完成Cookie和Session
  *
  *
  *   2015-08-25 09:56:05
  *   修正TDiocpHttpRequest回归对象池时，进行清理对象。避免残留多余的Cookie对象。(感谢阿木反馈bug)
  *
  *
*)
unit diocp.ex.httpServer;

interface

/// 三个编译开关，只能开启一个
{.$DEFINE INNER_IOCP}     // iocp线程触发事件
{.$DEFINE  QDAC_QWorker} // 用qworker进行调度触发事件
{$DEFINE DIOCP_Task}    // 用diocp.task进行调度触发事件


uses
  Classes, StrUtils, SysUtils, utilsBuffer, utilsStrings


  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp.task{$ENDIF}
  , diocp.tcp.server, utilsQueues, utilsHashs;



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
  public
    constructor Create; virtual;
  end;

  /// <summary>
  ///   设置客户端时进行的Cookie设置类
  /// </summary>
  TDiocpHttpCookie = class(TObject)
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


  TDiocpHttpRequest = class(TObject)
  private
    FSessionID : String;

    /// <summary>
    ///   投递之前记录DNA，用于做异步任务时，是否取消当前任务
    /// </summary>
    FContextDNA : Integer;

    /// <summary>
    ///   便于在Close时归还回对象池
    /// </summary>
    FDiocpHttpServer:TDiocpHttpServer;

    FDiocpContext: TDiocpHttpClientContext;

    /// 头信息
    FHttpVersion: Word; // 10, 11

    FRequestVersionStr: String;

    FRequestMethod: String;
    FRequestRawURL: String;       // 原始的请求URL，不进行任何解码

    /// <summary>
    ///  原始请求中的URL参数数据(没有经过URLDecode，因为在DecodeRequestHeader中要拼接RequestURL时临时进行了URLDecode)
    ///  没有经过URLDecode是考虑到参数值中本身存在&字符，导致DecodeURLParam出现不解码异常
    /// </summary>
    FRequestURLParamData: string;

    FRequestURL: String;          // URI + 参数
    FRequestURI: String;          // URI 不带参数


    FRequestParamsList: TStringList; // TODO:存放http参数的StringList

    // 存放客户端请求的Cookie信息
    FRequestCookieList: TStrings;
    FRequestCookies: string;
    
    FContextType: string;
    FContextLength: Int64;
    FKeepAlive: Boolean;
    FRequestAccept: String;
    FRequestReferer: String;
    FRequestAcceptLanguage: string;
    FRequestAcceptEncoding: string;
    FRequestUserAgent: string;
    FRequestAuth: string;

    FRequestHostName: string;
    FRequestHostPort: string;



    FXForwardedFor: string;

    FRawHeaderData: TMemoryStream;

    /// <summary>
    ///   原始的POST数据
    /// </summary>
    FRawPostData: TMemoryStream;

    FPostDataLen: Integer;

    FRequestHeader: TStringList;

    FResponse: TDiocpHttpResponse;

    /// <summary>
    ///   不再使用了，归还回对象池
    /// </summary>
    procedure Close;
    /// <summary>
    /// 是否有效的Http 请求方法
    /// </summary>
    /// <returns>
    /// 0: 数据不足够进行解码
    /// 1: 有效的数据头
    /// 2: 无效的请求数据头
    /// </returns>
    function DecodeHttpRequestMethod: Integer;

    /// <summary>
    /// 解码Http请求参数信息
    /// </summary>
    /// <returns>
    /// 1: 有效的Http参数数据
    /// </returns>
    function DecodeHttpRequestHeader: Integer;
    
    /// <summary>
    /// 接收到的Buffer,写入数据
    /// </summary>
    procedure WriteRawBuffer(const buffer: Pointer; len: Integer);

    procedure CheckCookieSession;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function GetSession: TDiocpHttpSession;


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
    procedure DecodeURLParam(
      {$IFDEF UNICODE} pvEncoding:TEncoding {$ELSE}pvUseUtf8Decode:Boolean{$ENDIF});

    /// <summary>
    ///   清理
    /// </summary>
    procedure Clear;

    /// <summary>
    ///  读取传入的Cookie值
    /// </summary>
    function GetCookie(pvCookieName:string):String;

    property ContextLength: Int64 read FContextLength;


    /// <summary>
    ///   与客户端建立的连接
    /// </summary>
    property Connection: TDiocpHttpClientContext read FDiocpContext;
    property HttpVersion: Word read FHttpVersion;
    /// <summary>
    ///   原始的Post过来的数据
    /// </summary>
    property RawPostData: TMemoryStream read FRawPostData;
    property RequestAccept: String read FRequestAccept;
    property RequestAcceptEncoding: string read FRequestAcceptEncoding;
    property RequestAcceptLanguage: string read FRequestAcceptLanguage;
    property RequestCookieList: TStrings read FRequestCookieList;
    property RequestCookies: string read FRequestCookies;

    /// <summary>
    ///   请求的头信息
    /// </summary>
    property RequestHeader: TStringList read FRequestHeader;

    /// <summary>
    ///   从头信息解码器出来的Url,包含参数
    /// </summary>
    property RequestURL: String read FRequestURL;

    /// <summary>
    ///   从头信息提取出来的URL，未经过任何加工,包含参数
    /// </summary>
    property RequestRawURL: String read FRequestRawURL;

    /// <summary>
    ///   不带URL参数
    /// </summary>
    property RequestURI: String read FRequestURI;

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
    property Response: TDiocpHttpResponse read FResponse;

    /// <summary>
    ///   从Url和Post数据中得到的参数信息: key = value
    /// </summary>
    property RequestParamsList: TStringList read FRequestParamsList;

    property RequestReferer: String read FRequestReferer;

    /// <summary>
    /// 应答完毕，发送会客户端
    /// </summary>
    procedure ResponseEnd;

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

  TDiocpHttpResponse = class(TObject)
  private
    FResponseHeader: string;
    FCookies: TList;
    FContentType: String;
    FCookieData : String;
    FData: TMemoryStream;
    FDiocpContext : TDiocpHttpClientContext;
    FHttpCodeStr: String;
    procedure ClearAllCookieObjects;
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure WriteBuf(pvBuf: Pointer; len: Cardinal);
    procedure WriteString(pvString: string; pvUtf8Convert: Boolean = true);

    function AddCookie: TDiocpHttpCookie; overload;

    function AddCookie(pvName:String; pvValue:string): TDiocpHttpCookie; overload;

    function EncodeHeader: String;

    /// <summary>
    ///   与客户端建立的连接
    /// </summary>
    property Connection: TDiocpHttpClientContext read FDiocpContext;

    property ContentType: String read FContentType write FContentType;

    property HttpCodeStr: String read FHttpCodeStr write FHttpCodeStr;





    procedure RedirectURL(pvURL:String);
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
    FRequestPool: TSafeQueue;
    FSessionList: TDHashTableSafe;
    FSessionClass : TDiocpHttpSessionClass;

    FOnDiocpHttpRequest: TOnDiocpHttpRequestEvent;
    FOnDiocpHttpRequestPostDone: TOnDiocpHttpRequestEvent;

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

  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    procedure RegisterSessionClass(pvClass:TDiocpHttpSessionClass);



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
  FRawHeaderData.Clear;
  FRawPostData.Clear;
  FRequestURL := '';
  FRequestURI := '';
  FRequestRawURL := '';
  FRequestVersionStr := '';
  FRequestMethod := '';
  FRequestCookies := '';
  FRequestParamsList.Clear;
  FRequestCookieList.Clear;
  FContextLength := 0;
  FPostDataLen := 0;
  FResponse.Clear;  
end;

procedure TDiocpHttpRequest.Close;
begin
  if FDiocpHttpServer = nil then exit;
  FDiocpHttpServer.GiveBackRequest(Self);
end;

procedure TDiocpHttpRequest.CloseContext;
begin
  FDiocpContext.PostWSACloseRequest();
end;

function TDiocpHttpRequest.GetCookie(pvCookieName: string): String;
begin
  Result := StringsValueOfName(FRequestCookieList, pvCookieName, ['='], true);
end;

function TDiocpHttpRequest.GetRequestParam(ParamsKey: string): string;
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

constructor TDiocpHttpRequest.Create;
begin
  inherited Create;
  FRawHeaderData := TMemoryStream.Create();
  FRawPostData := TMemoryStream.Create();
  FRequestHeader := TStringList.Create();
  FResponse := TDiocpHttpResponse.Create();

  FRequestParamsList := TStringList.Create; // TODO:创建存放http参数的StringList
  FRequestCookieList := TStringList.Create;
end;

destructor TDiocpHttpRequest.Destroy;
begin
  FreeAndNil(FResponse);
  FRawPostData.Free;
  FRawHeaderData.Free;
  FRequestHeader.Free;

  FreeAndNil(FRequestParamsList); // TODO:释放存放http参数的StringList
  FRequestCookieList.Free;

  inherited Destroy;
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

function TDiocpHttpRequest.DecodeHttpRequestMethod: Integer;
var
  lvBuf: PAnsiChar;
begin
  Result := 0;
  if FRawHeaderData.Size <= 7 then
    Exit;

  lvBuf := FRawHeaderData.Memory;

  if FRequestMethod <> '' then
  begin
    Result := 1; // 已经解码
    Exit;
  end;

  // 请求方法（所有方法全为大写）有多种，各个方法的解释如下：
  // GET     请求获取Request-URI所标识的资源
  // POST    在Request-URI所标识的资源后附加新的数据
  // HEAD    请求获取由Request-URI所标识的资源的响应消息报头
  // PUT     请求服务器存储一个资源，并用Request-URI作为其标识
  // DELETE  请求服务器删除Request-URI所标识的资源
  // TRACE   请求服务器回送收到的请求信息，主要用于测试或诊断
  // CONNECT 保留将来使用
  // OPTIONS 请求查询服务器的性能，或者查询与资源相关的选项和需求
  // 应用举例：
  // GET方法：在浏览器的地址栏中输入网址的方式访问网页时，浏览器采用GET方法向服务器获取资源，eg:GET /form.html HTTP/1.1 (CRLF)
  //
  // POST方法要求被请求服务器接受附在请求后面的数据，常用于提交表单。

  Result := 1;
  // HTTP 1.1 支持8种请求
  if (StrLIComp(lvBuf, 'GET', 3) = 0) then
  begin
    FRequestMethod := 'GET';
  end
  else if (StrLIComp(lvBuf, 'POST', 4) = 0) then
  begin
    FRequestMethod := 'POST';
  end
  else if (StrLIComp(lvBuf, 'PUT', 3) = 0) then
  begin
    FRequestMethod := 'PUT';
  end
  else if (StrLIComp(lvBuf, 'HEAD', 3) = 0) then
  begin
    FRequestMethod := 'HEAD';
  end
  else if (StrLIComp(lvBuf, 'OPTIONS', 7) = 0) then
  begin
    FRequestMethod := 'OPTIONS';
  end
  else if (StrLIComp(lvBuf, 'DELETE', 6) = 0) then
  begin
    FRequestMethod := 'DELETE';
  end
  else if (StrLIComp(lvBuf, 'TRACE', 5) = 0) then
  begin
    FRequestMethod := 'TRACE';
  end
  else if (StrLIComp(lvBuf, 'CONNECT', 7) = 0) then
  begin
    FRequestMethod := 'CONNECT';
  end
  else
  begin
    Result := 2;
  end;
end;

function TDiocpHttpRequest.DecodeHttpRequestHeader: Integer;
var
  lvRawString: AnsiString;
  lvMethod, lvRawTemp: AnsiString;
  lvRequestCmdLine, lvTempStr, lvRemainStr: String;
  I, J: Integer;
  p : PChar;
begin
  Result := 1;
  SetLength(lvRawString, FRawHeaderData.Size);
  FRawHeaderData.Position := 0;
  FRawHeaderData.Read(lvRawString[1], FRawHeaderData.Size);
  FRequestHeader.Text := lvRawString;

  // GET /test?v=abc HTTP/1.1
  lvRequestCmdLine := FRequestHeader[0];
  P := PChar(lvRequestCmdLine);
  FRequestHeader.Delete(0);

  I := 1;
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求方法(GET, POST, PUT, HEAD...)
  lvMethod := UpperCase(Copy(lvRequestCmdLine, 1, I - 1));
  Inc(I);
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] <> ' ') do
    Inc(I);

  // 请求参数及路径
  lvTempStr := Copy(lvRequestCmdLine, J, I - J);
  FRequestRawURL := lvTempStr;
  FRequestURLParamData := '';

  // 解析参数
  J := Pos('?', lvTempStr);

  if (J <= 0) then
  begin
    lvRawTemp := '';
    FRequestURL := URLDecode(lvTempStr);
    FRequestURL := UTF8Decode(FRequestURL);  // Url经过了Utf8编码
    FRequestURI := FRequestURL;   //无参数和url一致
  end
  else
  begin
    // IE原始URL  : /中国.asp?topicid=a汉字a
    // 后台接收到 : /%E4%B8%AD%E5%9B%BD.asp?topicid=a汉字a

    // FireFox/360极速浏览器原始URL : /%E4%B8%AD%E5%9B%BD.asp?topicid=a%E6%B1%89%E5%AD%97a
    // 后台接收到 : /%E4%B8%AD%E5%9B%BD.asp?topicid=a%E6%B1%89%E5%AD%97a


    // URI需要进行URLDecode和Utf8解码
    FRequestURI := Copy(lvTempStr, 1, J - 1);
    FRequestURI := URLDecode(FRequestURI, False);
    FRequestURI := UTF8Decode(FRequestURI);

    // URL中的参数需要进行URLDecode，IE提交过来的为前台的系统默认编码
    lvRawTemp := Copy(lvTempStr, J + 1, MaxInt);
    FRequestURLParamData := lvRawTemp;
    lvRawTemp := URLDecode(lvRawTemp, False);      // 其他浏览器需要进行URLDecode(IE不需要)

    // 拼接
    FRequestURL := FRequestURI + lvRawTemp;
  end;

  Inc(I);
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(lvRequestCmdLine)) and (lvRequestCmdLine[I] <> ' ') do
    Inc(I);

  // 请求的HTTP版本
  FRequestVersionStr := Trim(UpperCase(Copy(lvRequestCmdLine, J, I - J)));

  if (FRequestVersionStr = '') then
    FRequestVersionStr := 'HTTP/1.0';
  if (lvTempStr = 'HTTP/1.0') then
  begin
    FHttpVersion := 10;
    FKeepAlive := false; // 默认为false
  end
  else
  begin
    FHttpVersion := 11;
    FKeepAlive := true; // 默认为true
  end;

  FContextLength := 0;


  // eg：POST /reg.jsp HTTP/ (CRLF)
  // Accept:image/gif,image/x-xbit,... (CRLF)
  // ...
  // HOST:www.guet.edu.cn (CRLF)
  // Content-Length:22 (CRLF)
  // Connection:Keep-Alive (CRLF)
  // Cache-Control:no-cache (CRLF)
  // (CRLF)         //该CRLF表示消息报头已经结束，在此之前为消息报头
  // user=jeffrey&pwd=1234  //此行以下为提交的数据
  //
  // HEAD方法与GET方法几乎是一样的，对于HEAD请求的回应部分来说，它的HTTP头部中包含的信息与通过GET请求所得到的信息是相同的。利用这个方法，不必传输整个资源内容，就可以得到Request-URI所标识的资源的信息。该方法常用于测试超链接的有效性，是否可以访问，以及最近是否更新。
  // 2、请求报头后述
  // 3、请求正文(略)

  for I := 0 to FRequestHeader.Count - 1 do
  begin
    lvRequestCmdLine := FRequestHeader[I];
    P := PChar(lvRequestCmdLine);

    // 获取右边的字符
    lvTempStr := LeftUntil(P, [':']);
    SkipChars(P, [':', ' ']);

    // 获取剩余的字符
    lvRemainStr := P;

    if (lvRequestCmdLine = '') then
      Continue;

    if SameText(lvTempStr, 'Content-Type') then
    begin
      FContextType := lvRemainStr;
    end else if SameText(lvTempStr, 'Content-Length') then
    begin
      FContextLength := StrToInt64Def(lvRemainStr, -1);
    end else if SameText(lvTempStr, 'Accept') then
    begin
      FRequestAccept := lvRemainStr;
    end else if SameText(lvTempStr, 'Referer') then
    begin
      FRequestReferer := lvRemainStr;
    end else if SameText(lvTempStr, 'Accept-Language') then
    begin
      FRequestAcceptLanguage := lvRemainStr;
    end else if SameText(lvTempStr, 'Accept-Encoding') then
    begin
      FRequestAcceptEncoding := lvRemainStr;
    end else if SameText(lvTempStr, 'User-Agent')then
    begin
      FRequestUserAgent := lvRemainStr;
    end else if SameText(lvTempStr, 'Authorization') then
    begin
      FRequestAuth := lvRemainStr;
    end else if SameText(lvTempStr, 'Cookie') then
    begin
      // Cookie:__gads=ID=6ff3a79a032e04d0:T=1425100914:S=ALNI_MZWDCQuaEqZV3ZYri0E4GU8osX7rw; pgv_pvi=5995954176; lzstat_uv=25556556142595371638|754770@2240623; Hm_lvt_674430fbddd66a488580ec86aba288f7=1433747304,1435200001; Hm_lvt_95eb98507622b340bc1da73ed59cfe34=1435906572; AJSTAT_ok_times=2; __utma=226521935.635858515.1425100841.1436162631.1437634125.12; __utmz=226521935.1437634125.12.12.utmcsr=baidu|utmccn=(organic)|utmcmd=organic; _gat=1; _ga=GA1.2.635858515.1425100841; .CNBlogsCookie=B70AF05C246EE95507A6B4E1206C55C394843B6EB1376064B7CE2199A3441791D09AB86E934DF47E48B2E409BC57F7F4C43950430B29D3B23CAC82C7E58212D912F3ECB144B6971C4D9A7EB4E609A900A50016DA
      FRequestCookies := lvRemainStr;
      SplitStrings(FRequestCookies, FRequestCookieList, [';']);
    end else if SameText(lvTempStr, 'Host') then
    begin
      lvTempStr := lvRemainStr;
      J := Pos(':', lvTempStr);
      if J > 0 then
      begin
        FRequestHostName := Copy(lvTempStr, 1, J - 1);
        FRequestHostPort := Copy(lvTempStr, J + 1, 100);
      end
      else
      begin
        FRequestHostName := lvTempStr;
        FRequestHostPort := IntToStr((FDiocpContext).Owner.Port);
      end;
    end
    else if SameText(lvTempStr, 'Connection') then
    begin
      // HTTP/1.0 默认KeepAlive=False，只有显示指定了Connection: keep-alive才认为KeepAlive=True
      // HTTP/1.1 默认KeepAlive=True，只有显示指定了Connection: close才认为KeepAlive=False
      if FHttpVersion = 10 then
        FKeepAlive := SameText(lvRemainStr, 'keep-alive')
      else if SameText(lvRemainStr, 'close') then
        FKeepAlive := false;
    end
    else if SameText(lvTempStr, 'X-Forwarded-For') then
      FXForwardedFor := lvRemainStr;
  end;
end;

procedure TDiocpHttpRequest.DecodePostDataParam({$IFDEF UNICODE} pvEncoding:TEncoding {$ELSE}pvUseUtf8Decode:Boolean{$ENDIF});
var
  lvRawData : AnsiString;
  lvRawParams, s:String;
  i:Integer;
  lvStrings:TStrings;
{$IFDEF UNICODE}
var
  lvBytes:TBytes;
{$ELSE}
{$ENDIF}
begin
  if FRawPostData.Size = 0 then exit;
  
  // 读取原始数据
  SetLength(lvRawData, FRawPostData.Size);
  FRawPostData.Position := 0;
  FRawPostData.Read(lvRawData[1], FRawPostData.Size);

  lvStrings := TStringList.Create;
  try
    // 先放入到Strings
    SplitStrings(lvRawData, lvStrings, ['&']);

    for i := 0 to lvStrings.Count - 1 do
    begin
      lvRawData := URLDecode(lvStrings.ValueFromIndex[i]);
      if lvRawData <> '' then   // 不合法的Key-Value会导致空字符串
      begin
        {$IFDEF UNICODE}
        if pvEncoding <> nil then
        begin
          // 字符编码转换
          SetLength(lvBytes, length(lvRawData));
          Move(PByte(lvRawData)^, lvBytes[0], Length(lvRawData));
          s := pvEncoding.GetString(lvBytes);
        end else
        begin
          s := lvRawData;
        end;
        {$ELSE}
        if pvUseUtf8Decode then
        begin
          s := UTF8Decode(lvRawData);
        end else
        begin
          s := lvRawData;
        end;
        {$ENDIF}

        // 解码参数
        lvStrings.ValueFromIndex[i] := s;
      end;
    end;
    FRequestParamsList.AddStrings(lvStrings);
  finally
    lvStrings.Free;
  end;
end;


procedure TDiocpHttpRequest.DecodeURLParam(
  {$IFDEF UNICODE} pvEncoding:TEncoding {$ELSE}pvUseUtf8Decode:Boolean{$ENDIF});
var
  lvRawData : AnsiString;
  lvRawParams, s:String;
  i:Integer;
  lvStrings:TStrings;
{$IFDEF UNICODE}
var
  lvBytes:TBytes;
{$ELSE}
{$ENDIF}
begin
  // 解析URL参数
  if FRequestURLParamData = '' then exit;

  lvStrings := TStringList.Create;
  try
    // 先放入到Strings
    SplitStrings(FRequestURLParamData, lvStrings, ['&']);

    for i := 0 to lvStrings.Count - 1 do
    begin
      lvRawData := URLDecode(lvStrings.ValueFromIndex[i]);
      if lvRawData<> '' then
      begin
        {$IFDEF UNICODE}
        if pvEncoding <> nil then
        begin
          // 字符编码转换
          SetLength(lvBytes, length(lvRawData));
          Move(PByte(lvRawData)^, lvBytes[0], Length(lvRawData));
          s := pvEncoding.GetString(lvBytes);
        end else
        begin
          s := lvRawData;
        end;
        {$ELSE}
        if pvUseUtf8Decode then
        begin
          s := UTF8Decode(lvRawData);
        end else
        begin
          s := lvRawData;
        end;
        {$ENDIF}

        // 解码参数
        lvStrings.ValueFromIndex[i] := s;
      end;
    end;
    FRequestParamsList.AddStrings(lvStrings);
  finally
    lvStrings.Free;
  end;

end;

function TDiocpHttpRequest.GetSession: TDiocpHttpSession;
begin
  CheckCookieSession;
  Result := TDiocpHttpServer(Connection.Owner).GetSession(FSessionID);
end;

/// <summary>
///  解析POST和GET参数
/// </summary>
/// <pvParamText>
/// <param name="pvParamText">要解析的全部参数</param>
/// </pvParamText>
procedure TDiocpHttpRequest.ParseParams(pvParamText: string);
begin
  SplitStrings(pvParamText, FRequestParamsList, ['&']);
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

  if FResponse.FData.Size > 0 then
  begin
    FDiocpContext.PostWSASendRequest(FResponse.FData.Memory,
      FResponse.FData.Size);
  end;

  if not FKeepAlive then
  begin
    FDiocpContext.PostWSACloseRequest;
  end;
end;

procedure TDiocpHttpRequest.WriteRawBuffer(const buffer: Pointer; len: Integer);
begin
  FRawHeaderData.WriteBuffer(buffer^, len);
end;

procedure TDiocpHttpResponse.Clear;
begin
  FContentType := '';
  FData.Clear;
  FResponseHeader := '';
  ClearAllCookieObjects;
end;

constructor TDiocpHttpResponse.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create();
  FCookies := TList.Create();
end;

destructor TDiocpHttpResponse.Destroy;
begin
  Clear;
  FreeAndNil(FData);
  FCookies.Free;
  inherited Destroy;
end;

function TDiocpHttpResponse.AddCookie: TDiocpHttpCookie;
begin
  Result := TDiocpHttpCookie.Create;
  Result.Path := '/';
  FCookies.Add(Result);
end;

function TDiocpHttpResponse.AddCookie(pvName:String; pvValue:string):
    TDiocpHttpCookie;
begin
  Result := AddCookie;
  Result.Name := pvName;
  Result.Value := pvValue;
end;

function TDiocpHttpResponse.EncodeHeader: String;
var
  lvVersionStr:string;
  i: Integer;
begin
  Result := '';

  lvVersionStr := 'HTTP/1.0';

  if (FHttpCodeStr = '') then FHttpCodeStr := '200 OK';
  Result := Result + lvVersionStr + ' ' + FHttpCodeStr + #13#10;

  if (FContentType = '') then FContentType := 'text/html';
  Result := Result + 'Content-Type: ' + FContentType + #13#10;

  if (FData.Size > 0) then
    Result := Result + 'Content-Length: ' + IntToStr(FData.Size) + #13#10;

  Result := Result + 'Connection: close'#13#10;


  for i := 0 to FCookies.Count - 1 do
  begin
    Result := Result + 'Set-Cookie:' + TDiocpHttpCookie(FCookies[i]).ToString() + sLineBreak;
  end;

//  if FCookieData <> '' then
//  begin             // Set-Cookie: JSESSIONID=4918D6ED22B81B587E7AF7517CE24E25.server1; Path=/cluster
//    Result := Result + 'Set-Cookie:' + FCookieData + sLineBreak;
//  end;

  Result := Result + 'Server: DIOCP-V5/1.1'#13#10;
end;

procedure TDiocpHttpResponse.ClearAllCookieObjects;
var
  i: Integer;
begin
  for i := 0 to FCookies.Count-1 do
  begin
    TObject(FCookies[i]).Free;
  end;
  FCookies.Clear();
end;

procedure TDiocpHttpResponse.RedirectURL(pvURL: String);
var
  lvFixedHeader: AnsiString;
  len: Integer;
begin
  lvFixedHeader := MakeHeader('302 Temporarily Moved', 'HTTP/1.0', false, '',
    '', 0);

  lvFixedHeader := lvFixedHeader + 'Location: ' + pvURL + HTTPLineBreak;

  lvFixedHeader := FixHeader(lvFixedHeader);

  // FResponseSize必须准确指定发送的数据包大小
  // 用于在发送完之后(Owner.TriggerClientSentData)断开客户端连接
  if lvFixedHeader <> '' then
  begin
    len := Length(lvFixedHeader);
    FDiocpContext.PostWSASendRequest(PAnsiChar(lvFixedHeader), len);
  end;

end;

procedure TDiocpHttpResponse.WriteBuf(pvBuf: Pointer; len: Cardinal);
begin
  FData.Write(pvBuf^, len);
end;

procedure TDiocpHttpResponse.WriteString(pvString: string; pvUtf8Convert:
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
       // 直接触发事件
       TDiocpHttpServer(FOwner).DoRequest(pvRequest);
     finally
       pvRequest.Close;
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
    lvObj.Close;
  end;
end;

{$ENDIF}

{$IFDEF DIOCP_Task}
procedure TDiocpHttpClientContext.OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
var
  lvObj:TDiocpHttpRequest;
begin
  // 无论如何都要归还HttpRequest到池
  lvObj := TDiocpHttpRequest(pvTaskRequest.TaskData);
  try
    // 连接已经断开, 放弃处理逻辑
    if (Self = nil) then Exit;

    // 连接已经断开, 放弃处理逻辑
    if (FOwner = nil) then Exit;

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
    lvObj.Close;
  end;

end;
{$ENDIF}



procedure TDiocpHttpClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: Word);
var
  lvTmpBuf: PAnsiChar;
  CR, LF: Integer;
  lvRemain: Cardinal;
  lvTempRequest: TDiocpHttpRequest;
begin
  inherited;
  lvTmpBuf := buf;
  CR := 0;
  LF := 0;
  lvRemain := len;
  while (lvRemain > 0) do
  begin
    if FHttpState = hsCompleted then
    begin // 完成后重置，重新处理下一个包
      FCurrentRequest := TDiocpHttpServer(Owner).GetRequest;
      FCurrentRequest.FDiocpContext := self;
      FCurrentRequest.Response.FDiocpContext := self;
      FCurrentRequest.Clear;

      // 记录当前contextDNA，异步任务时组做检测
      FCurrentRequest.FContextDNA := self.ContextDNA;

      FHttpState := hsRequest;
    end;

    if (FHttpState = hsRequest) then
    begin
      case lvTmpBuf^ of
        #13:
          Inc(CR);
        #10:
          Inc(LF);
      else
        CR := 0;
        LF := 0;
      end;

      // 写入请求数据
      FCurrentRequest.WriteRawBuffer(lvTmpBuf, 1);

      if FCurrentRequest.DecodeHttpRequestMethod = 2 then
      begin // 无效的Http请求
        // 还回对象池
        self.RequestDisconnect('无效的Http请求', self);
        Exit;
      end;

      // 请求数据已接收完毕(#13#10#13#10是HTTP请求结束的标志)
      if (CR = 2) and (LF = 2) then
      begin
        if FCurrentRequest.DecodeHttpRequestHeader = 0 then
        begin
          self.RequestDisconnect('无效的Http协议数据', self);
          Exit;
        end;

        if SameText(FCurrentRequest.FRequestMethod, 'POST') or
          SameText(FCurrentRequest.FRequestMethod, 'PUT') then
        begin
          // 无效的Post请求直接断开
          if (FCurrentRequest.FContextLength <= 0) then
          begin
            self.RequestDisconnect('无效的POST/PUT请求数据', self);
            Exit;
          end;
          // 改变Http状态, 进入接受数据状态
          FHttpState := hsRecvingPost;
        end
        else
        begin
          FHttpState := hsCompleted;

          lvTempRequest := FCurrentRequest;

          // 避免断开后还回对象池，造成重复还回
          FCurrentRequest := nil;

          // 触发事件
          DoRequest(lvTempRequest);

          FCurrentRequest := nil;
          Break;
        end;
      end;
    end
    else if (FHttpState = hsRecvingPost) then
    begin
      FCurrentRequest.FRawPostData.Write(lvTmpBuf^, 1);
      Inc(FCurrentRequest.FPostDataLen);

      if FCurrentRequest.FPostDataLen >= FCurrentRequest.FContextLength then
      begin
        FHttpState := hsCompleted;

        // 触发事件
        TDiocpHttpServer(FOwner).DoRequestPostDataDone(FCurrentRequest);

        lvTempRequest := FCurrentRequest;

        // 避免断开后还回对象池，造成重复还回
        FCurrentRequest := nil;

        // 触发事件
        DoRequest(lvTempRequest);
      end;
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
  FSessionList := TDHashTableSafe.Create;
  KeepAlive := false;
  RegisterContextClass(TDiocpHttpClientContext);
  RegisterSessionClass(TDiocpHttpSession);
end;

destructor TDiocpHttpServer.Destroy;
begin
  FRequestPool.FreeDataObject;
  FRequestPool.Free;
  FSessionList.FreeAllDataAsObject;
  FSessionList.Free;
  inherited;
end;

procedure TDiocpHttpServer.DoRequest(pvRequest: TDiocpHttpRequest);
begin
  pvRequest.CheckCookieSession;

  if Assigned(FOnDiocpHttpRequest) then
  begin
    FOnDiocpHttpRequest(pvRequest);
  end;
end;

procedure TDiocpHttpServer.DoRequestPostDataDone(pvRequest: TDiocpHttpRequest);
var
  lvRawData:AnsiString;
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
  Result.FDiocpHttpServer := Self;
end;

function TDiocpHttpServer.GetSession(pvSessionID:string): TDiocpHttpSession;
begin
  FSessionList.Lock;
  try
    Result := TDiocpHttpSession(FSessionList.ValueMap[pvSessionID]);
    if Result = nil then
    begin
      if FSessionClass = nil then raise Exception.Create('尚未注册SessionClass, 不能获取Session');
      Result := FSessionClass.Create();
      FSessionList.ValueMap[pvSessionID] := Result;      
    end;
  finally
    FSessionList.unLock;
  end;
end;

procedure TDiocpHttpServer.GiveBackRequest(pvRequest: TDiocpHttpRequest);
begin
  pvRequest.Clear;
  FRequestPool.EnQueue(pvRequest);
end;

procedure TDiocpHttpServer.RegisterSessionClass(pvClass:TDiocpHttpSessionClass);
begin
  FSessionClass := pvClass;
end;

function TDiocpHttpCookie.ToString: String;
begin
  Result := Format('%s=%s; Path=%s;', [self.FName, self.FValue, Self.FPath]);
end;

constructor TDiocpHttpSession.Create;
begin

end;



end.
