(*
  *	 Unit owner: D10.Mofen, delphi iocp framework author
  *         homePage: http://www.Diocp.org
  *	       blog: http://www.cnblogs.com/dksoft

  *   2015-02-22 08:29:43
  *     DIOCP-V5 发布

  *    Http协议处理单元
  *    其中部分思路来自于delphi iocp framework中的iocp.HttpServer
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
  Classes, StrUtils, SysUtils, utils.buffer, utils.strings


  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp.task{$ENDIF}
  , diocp.tcp.server, utils.queues, utils.hashs, utils_dvalue
  , diocp_ex_http_common
  , utils.objectPool, utils.safeLogger, Windows;



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
    FReleaseLater:Boolean;
    FReleaseLaterMsg:String;
    
    FSessionID : String;

    FInnerRequest:THttpRequest;

    /// <summary>
    ///   投递之前记录DNA，用于做异步任务时，是否取消当前任务
    /// </summary>
    FContextDNA : Integer;

    /// <summary>
    ///   便于在Close时归还回对象池
    /// </summary>
    FDiocpHttpServer:TDiocpHttpServer;

    /// <summary>
    ///  请求头
    /// </summary>
    FHeader : TDValue;

    /// <summary>
    ///   URL中的参数
    /// </summary>
    FURLParams: TDValue;

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

    FRequestParamsList: TStringList; // TODO:存放http参数的StringList

    // 存放客户端请求的Cookie信息
    FRequestCookieList: TStrings;

    FContextType: string;
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

    FRawHeader: TMemoryStream;

    /// <summary>
    ///   原始的POST数据
    /// </summary>
    FRawPostData: TMemoryStream;

    FPostDataLen: Integer;

    FRequestHeader: TStringList;
    FRequestRawHeaderString: string;

    FResponse: TDiocpHttpResponse;

    /// <summary>
    ///   不再使用了，归还回对象池
    /// </summary>
    procedure Close;

    /// <summary>
    /// 接收到的Buffer,写入数据
    /// </summary>
    procedure WriteRawHeaderBuffer(const buffer: Pointer; len: Integer);

    /// <summary>
    ///   检测Cookie中的SessionID信息
    ///   不创建Session对象
    /// </summary>
    procedure CheckCookieSession;
    function GetContextLength: Int64;
    function GetRequestCookieList: TStrings;
    function GetRequestCookies: string;
    function GetRequestURI: String;
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

    property ContextType: String read FContextType;

    property ContextLength: Int64 read GetContextLength;


    /// <summary>
    ///   与客户端建立的连接
    /// </summary>
    property Connection: TDiocpHttpClientContext read FDiocpContext;

    /// <summary>
    ///   请求头
    /// </summary>
    property Header: TDValue read FHeader;

    property HttpVersion: Word read FHttpVersion;
    
    /// <summary>
    ///   原始的Post过来的数据
    /// </summary>
    property RawPostData: TMemoryStream read FRawPostData;

    property RawHeader: TMemoryStream read FRawHeader;


    property RequestAccept: String read FRequestAccept;
    property RequestAcceptEncoding: string read FRequestAcceptEncoding;
    property RequestAcceptLanguage: string read FRequestAcceptLanguage;
    property RequestCookieList: TStrings read GetRequestCookieList;
    property RequestCookies: string read GetRequestCookies;

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
    property RequestURI: String read GetRequestURI;

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

    property RequestRawHeaderString: string read FRequestRawHeaderString;

    property RequestReferer: String read FRequestReferer;

    /// <summary>
    ///  原始请求中的URL参数数据(没有经过URLDecode，因为在DecodeRequestHeader中要拼接RequestURL时临时进行了URLDecode)
    ///  没有经过URLDecode是考虑到参数值中本身存在&字符，导致DecodeURLParam出现不解码异常
    /// </summary>
    property RequestURLParamData: string read FRequestURLParamData;
    property URLParams: TDValue read FURLParams;

    /// <summary>
    /// 应答完毕，发送会客户端
    /// </summary>
    procedure ResponseEnd;


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
    FCookieData : String;
    FDiocpContext : TDiocpHttpClientContext;
    
    FInnerResponse:THttpResponse;
    procedure ClearAllCookieObjects;
    function GetContentType: String;
    function GetHeader: TDValue;
    function GetHttpCodeStr: String;
    procedure SetContentType(const Value: String);
    procedure SetHttpCodeStr(const Value: String);
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure WriteBuf(pvBuf: Pointer; len: Cardinal);
    procedure WriteString(pvString: string; pvUtf8Convert: Boolean = true);

    function AddCookie: TDiocpHttpCookie; overload;

    function AddCookie(pvName:String; pvValue:string): TDiocpHttpCookie; overload;

    function EncodeHeader: String;

    function EncodeResponseHeader(pvContentLength: Integer): string;

    /// <summary>
    ///   与客户端建立的连接
    /// </summary>
    property Connection: TDiocpHttpClientContext read FDiocpContext;

    property ContentType: String read GetContentType write SetContentType;

    property Header: TDValue read GetHeader;

    property HttpCodeStr: String read GetHttpCodeStr write SetHttpCodeStr;

    procedure RedirectURL(pvURL:String);

    procedure GZipContent;

    procedure ZLibCompressContent;

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
  FRawHeader.Clear;
  FRawPostData.Clear;
  FURLParams.Clear;
  FRequestURL := '';
  FRequestRawURL := '';
  FRequestVersionStr := '';
  FRequestMethod := '';
  FRequestParamsList.Clear;
  FRequestCookieList.Clear;
  FContextType := '';
  FPostDataLen := 0;
  FResponse.Clear;
  FReleaseLater := false;
  FRequestRawHeaderString := '';
  FInnerRequest.DoCleanUp;
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
var
  i:Integer;
  lvCookie:TDiocpHttpCookie;
begin
  lvCookie := FResponse.FInnerResponse.GetCookie(pvCookieName);

  Result := StringsValueOfName(RequestCookieList, pvCookieName, ['='], true);
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
  FInnerRequest := THttpRequest.Create;
  
  FHeader := TDValue.Create(vntObject);

  FURLParams := TDValue.Create(vntObject);
  
  FRawHeader := TMemoryStream.Create();
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
  FRawHeader.Free;
  FRequestHeader.Free;

  FreeAndNil(FRequestParamsList); // TODO:释放存放http参数的StringList
  FRequestCookieList.Free;

  FHeader.Free;

  FURLParams.Free;

  FInnerRequest.Free;

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

procedure TDiocpHttpRequest.DecodePostDataParam({$IFDEF UNICODE} pvEncoding:TEncoding {$ELSE}pvUseUtf8Decode:Boolean{$ENDIF});
var
  lvRawData : AnsiString;
  lvRawParams, s, lvName, lvValue:String;
  i:Integer;
  lvStrings:TStrings;
  lvSpliteStrings: TArrayStrings;
{$IFDEF UNICODE}
var
  lvBytes:TBytes;
{$ELSE}
{$ENDIF}
begin
  if FRawPostData.Size = 0 then exit;

  SetLength(lvSpliteStrings, 2);
  
  // 读取原始数据
  SetLength(lvRawData, FRawPostData.Size);
  FRawPostData.Position := 0;
  FRawPostData.Read(PAnsiChar(lvRawData)^, FRawPostData.Size);

  lvStrings := TStringList.Create;
  try
    // 先放入到Strings
    SplitStrings(lvRawData, lvStrings, ['&']);

    for i := 0 to lvStrings.Count - 1 do
    begin
      s := Trim(lvStrings[i]);
      if length(s) > 0 then
      begin
        if SplitStr(s, '=', lvName, lvValue) then
        begin
          lvRawData := URLDecode(lvValue);
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
            if s = '' then s := lvRawData;
            

            // 解码参数
            lvStrings.ValueFromIndex[i] := s;
          end;
        end;
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
  s:String;
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
        
        FURLParams.ForceByName(lvStrings.Names[i]).AsString := s;
      end;
    end;
    FRequestParamsList.AddStrings(lvStrings);
  finally
    lvStrings.Free;
  end;

end;

function TDiocpHttpRequest.GetContextLength: Int64;
begin
  Result := FInnerRequest.ContentLength;
end;

function TDiocpHttpRequest.GetRequestCookieList: TStrings;
var
  lvCookies:String;
begin
  if FRequestCookieList.Count = 0 then
  begin
    // Cookie:__gads=ID=6ff3a79a032e04d0:T=1425100914:S=ALNI_MZWDCQuaEqZV3ZYri0E4GU8osX7rw; pgv_pvi=5995954176; lzstat_uv=25556556142595371638|754770@2240623; Hm_lvt_674430fbddd66a488580ec86aba288f7=1433747304,1435200001; Hm_lvt_95eb98507622b340bc1da73ed59cfe34=1435906572; AJSTAT_ok_times=2; __utma=226521935.635858515.1425100841.1436162631.1437634125.12; __utmz=226521935.1437634125.12.12.utmcsr=baidu|utmccn=(organic)|utmcmd=organic; _gat=1; _ga=GA1.2.635858515.1425100841; .CNBlogsCookie=B70AF05C246EE95507A6B4E1206C55C394843B6EB1376064B7CE2199A3441791D09AB86E934DF47E48B2E409BC57F7F4C43950430B29D3B23CAC82C7E58212D912F3ECB144B6971C4D9A7EB4E609A900A50016DA
    lvCookies := GetRequestCookies;
    SplitStrings(lvCookies, FRequestCookieList, [';']);
  end;
  Result := FRequestCookieList;
end;

function TDiocpHttpRequest.GetRequestCookies: string;
begin

  Result := FInnerRequest.RawCookie;
end;

function TDiocpHttpRequest.GetRequestURI: String;
begin
  Result := FInnerRequest.RequestURI;
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

  if FResponse.FInnerResponse.ContentBuffer.Length > 0 then
  begin
    FDiocpContext.PostWSASendRequest(FResponse.FInnerResponse.ContentBuffer.Memory,
      FResponse.FInnerResponse.ContentBuffer.Length);
  end;

  if not FKeepAlive then
  begin
    FDiocpContext.PostWSACloseRequest;
  end;
end;

procedure TDiocpHttpRequest.SendResponse(pvContentLength: Integer = 0);
var
  lvFixedHeader: AnsiString;
  len: Integer;
begin
  lvFixedHeader := FResponse.EncodeResponseHeader(pvContentLength);

  if (lvFixedHeader <> '') then
    lvFixedHeader := FixHeader(lvFixedHeader)
  else
    lvFixedHeader := lvFixedHeader + HTTPLineBreak;

  // FResponseSize必须准确指定发送的数据包大小
  // 用于在发送完之后(Owner.TriggerClientSentData)断开客户端连接
  if lvFixedHeader <> '' then
  begin
    len := Length(lvFixedHeader);
    sfLogger.logMessage('response===' + sLineBreak + lvFixedHeader);
    FDiocpContext.PostWSASendRequest(PAnsiChar(lvFixedHeader), len);
  end;

  if FResponse.FInnerResponse.ContentBuffer.Length > 0 then
  begin
    FDiocpContext.PostWSASendRequest(FResponse.FInnerResponse.ContentBuffer.Memory,
      FResponse.FInnerResponse.ContentBuffer.Length);
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

procedure TDiocpHttpRequest.WriteRawHeaderBuffer(const buffer: Pointer; len:
    Integer);
begin
  FRawHeader.WriteBuffer(buffer^, len);
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

function TDiocpHttpResponse.EncodeResponseHeader(pvContentLength: Integer):
    string;
begin
  FInnerResponse.EncodeHeader(pvContentLength);
  Result := FInnerResponse.HeaderBuilder.ToRAWString;
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

procedure TDiocpHttpResponse.ZLibCompressContent;
begin
  FInnerResponse.ZCompressContent
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
      // 如果需要执行
      if TDiocpHttpServer(FOwner).LogicWorkerNeedCoInitialize then
        FRecvRequest.IocpWorker.checkCoInitializeEx();
        
       // 直接触发事件
       TDiocpHttpServer(FOwner).DoRequest(pvRequest);
     finally
       if not pvRequest.FReleaseLater then pvRequest.Close;
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
    if not lvObj.FReleaseLater then lvObj.Close;
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
    if not lvObj.FReleaseLater then lvObj.Close;
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
      self.RequestDisconnect('无效的Http请求', self);
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
    pvRequest.CheckCookieSession;

    if Assigned(FOnDiocpHttpRequest) then
    begin
      FOnDiocpHttpRequest(pvRequest);
    end;
  except
    on E:Exception do
    begin
      pvRequest.Response.FInnerResponse.ResponseCode := 500;
      pvRequest.Response.Clear;
      pvRequest.Response.ContentType := 'text/html; charset=utf-8';
      lvMsg := e.Message;
      lvMsg := StringReplace(lvMsg, sLineBreak, '<BR>', [rfReplaceAll]);
      pvRequest.Response.WriteString(lvMsg);
    end;

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
  Result.FDiocpHttpServer := Self;
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
begin
  pvRequest.Clear;
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
      FSessionList.Remove(pvSessionID);
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
