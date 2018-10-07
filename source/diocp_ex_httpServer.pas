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
  *   2016-10-07 20:56:41
  *   加入WebSocket支持(感谢音儿)
*)
unit diocp_ex_httpServer;

interface

{$I 'diocp.inc'}

/// 三个编译开关，只能开启一个
{.$DEFINE INNER_IOCP_PROCESSOR}     // iocp线程触发事件
{.$DEFINE QDAC_QWorker}   // 用qworker进行调度触发事件
{$DEFINE DIOCP_Task}     // 用diocp_task进行调度触发事件


uses
  Classes, StrUtils, SysUtils, SHA, utils_strings 

  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp_task{$ENDIF}
  , diocp_tcp_server, utils_queues, utils_hashs, utils_dvalue
  , diocp_ex_http_common
  , diocp_res
  , utils_objectPool, utils_safeLogger, Windows, utils_threadinfo, SyncObjs,
  utils_BufferPool,  utils_websocket,  utils_base64, DateUtils;



const
  HTTPLineBreak = #13#10;
  SESSIONID = 'diocp_sid';
  BLOCK_BUFFER_TAG = 10000;

  BLOCK_STREAM_BUFFER_TAG = 1000;

  Context_Type_WebSocket = 1;

  SEND_BLOCK_SIZE = 1024 * 100;

  Response_state_inital = 0;
  Response_state_stream = 1;
  Response_state_err = 2;
  Response_state_done = 2;

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
    FSessionID: String;
    procedure SetSessionTimeOut(const Value: Integer);
  protected
    FSessionTimeOut: Integer;
    procedure DoCleanup; virtual;
  public
    constructor Create; virtual;
    property LastActivity: Integer read FLastActivity;
    
    property SessionID: String read FSessionID;

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

  TDiocpHttpResponseStream = class(TObject)
  private
    FStream:TStream;
    FRange:THttpRange;
  public
    
    
  end;


  TDiocpHttpRequest = class(TObject)
  private
    __free_flag:Integer;

    FRefCounter:Integer;

    FDecodeState:Integer;
    FDecodeMsg:string;

    FRange:THttpRange;

    FOwnerPool:TSafeQueue;

    FLocker:TCriticalSection;
    FThreadID:THandle;
    FThreadDebugInfo:String;

    /// <summary>
    ///  WebSocket接收到的整个数据
    /// </summary>
    FWebSocketContentBuffer:TDBufferBuilder;

    FReleaseLater:Boolean;
    FReleaseLaterMsg:String;

    FSessionID : String;

    FInnerWebSocketFrame:TDiocpWebSocketFrame;

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

    // 响应
    //FResponseState:Byte;


    FResponse: TDiocpHttpResponse;
    FResponsing: Boolean;

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
    procedure InnerAddToDebugStrings(const pvMsg: String);
    function GetCharset: string;


    function InputBuffer(const buf:Byte):Integer;
  public
    constructor Create;
    destructor Destroy; override;

  public

    function CheckIsRangeRequest: Boolean;

    /// <summary>
    ///   会阻止request释放，和连接投递接收请求
    /// </summary>
    procedure AddRef;

    /// <summary>
    ///   与AddRef配套使用
    /// </summary>
    function DecRef: Boolean;


    /// <summary>
    ///   设置Request暂时不进行释放
    /// </summary>
    /// <param name="pvMsg"> 信息(便于状态观察) </param>
    procedure SetReleaseLater(pvMsg:String);

    /// <summary>
    ///  检测是否是WebSocket请求
    /// </summary>
    function CheckIsWebSocketRequest: Boolean;

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

    /// <summary>
    ///   解码IE URL中的参数，放到参数列表中
    ///   在OnDiocpHttpRequest中调用
    ///   IE参数未进行任何编码
    /// </summary>
    procedure DecodeURLParamAsIE(); overload;

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

    function GetBodyAsString: String;

    property ContentType: String read GetContentType;

    property Charset:string read GetCharset;

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
    property InnerWebSocketFrame: TDiocpWebSocketFrame read FInnerWebSocketFrame;

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

    /// <summary>
    ///   正在响应
    /// </summary>
    property Responsing: Boolean read FResponsing;

    property URLParams: TDValue read GetURLParams;
    property WebSocketContentBuffer: TDBufferBuilder read FWebSocketContentBuffer;

    

    procedure AddDebugStrings(const pvDebugInfo: String; pvAddTimePre: Boolean =
        true);
    procedure CheckThreadIn;

    procedure CheckThreadSetInfo(const pvDebugInfo: string);

    procedure CheckThreadOut;


    /// <summary>
    ///   本次应答完成, 检测是否需要关闭连接等相应工作
    /// </summary>
    procedure DoResponseEnd;


    /// <summary>
    ///   直接发送Response.Header和Data数据
    /// </summary>
    procedure SendResponse(pvContentLength: Integer = 0);

    procedure ErrorResponse(pvCode:Integer; pvMsg:String);

    /// <summary>
    ///   直接发送一个文件
    /// </summary>
    procedure ResponseAFile(pvFileName:string);

    /// <summary>
    ///   处理头
    /// </summary>
    function ResponseAFileETag(const pvFileName: string): Boolean;

    /// <summary>
    ///   直接发送一个文件
    ///    响应类型
    ///    缓存
    /// </summary>
    procedure ResponseAFileEx(const pvFileName: string);

    /// <summary>
    ///   直接发送一个流
    /// </summary>
    procedure ResponseAStream(const pvStream: TStream; pvDoneCallBack:
        TWorkDoneCallBack);

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

    /// <summary>
    ///   请尽量使用SendResponse和DoResponseEnd来代替
    /// </summary>
    procedure ResponseEnd;

    /// <summary>
    ///  响应WEBSocket的握手
    /// </summary>
    procedure ResponseForWebSocketShake;




  end;

  TDiocpHttpResponse = class(TObject)
  private
    FDiocpContext : TDiocpHttpClientContext;
    
    FInnerResponse:THttpResponse;
    procedure ClearAllCookieObjects;
    function GetContentBody: TDBufferBuilder;
    function GetContentType: String;
    function GetHeader: TDValue;
    function GetHttpCodeStr: String;
    function GetResponseCode: Integer;
    function GetResponseID: string;
    procedure SetContentType(const Value: String);
    procedure SetHttpCodeStr(const Value: String);
    procedure SetResponseCode(const Value: Integer);
    procedure SetResponseID(const Value: string);
  public
    procedure Clear;
    procedure ClearContent;
    constructor Create;
    destructor Destroy; override;
    procedure WriteBuf(pvBuf: Pointer; len: Cardinal);
    procedure WriteString(pvString: string; pvUtf8Convert: Boolean = true);
    function GetResponseHeaderAsString: RAWString;

    function AddCookie: TDiocpHttpCookie; overload;

    procedure SetResponseFileName(const pvFile:String);

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
    
    property ResponseCode: Integer read GetResponseCode write SetResponseCode;

    

    property ResponseID: string read GetResponseID write SetResponseID;

    procedure RedirectURL(pvURL:String);

    procedure GZipContent;

    procedure DeflateCompressContent;

    procedure ZLibContent;

    procedure LZOCompressContent;

    procedure SetChunkedStart;

    procedure SetChunkedEnd;

    procedure ChunkedFlush;

    procedure SetChunkedBuffer(pvBuffer:Pointer; pvLen:Integer);

    procedure SetChunkedUtf8(pvStr:string);
  end;

  TDiocpWebSocketRequest = class(TDiocpHttpRequest)
  public

  end;

  /// <summary>
  /// Http 客户端连接
  /// </summary>
  TDiocpHttpClientContext = class(TIocpClientContext)
  private
    __free_flag:Integer;

    /// <summary>
    ///   响应引用计数
    /// </summary>
    FResponseRef:Integer;

    // 响应状态
    FResponseState: Integer;

    FCurrentStream:TStream;
    FCurrentStreamRemainSize:Integer;
    // 是否关闭连接 0是关闭, 1:不关闭
    FCurrentStreamEndAction:Byte;
    
    // 完成回调事件
    FCurrentStreamDoneCallBack: TWorkDoneCallBack;
    
    /// <summary>
    ///   0:普通 Http连接
    ///   1:WebSocket
    /// </summary>
    FContextType:Integer;


    /// 是否正在处理请求
    FIsProcessRequesting:Boolean;
    
    /// 请求的任务队列
    FRequestQueue:TSimpleQueue;

    /// <summary>
    ///   必须单线程操作
    /// </summary>
    FBlockBuffer: TBlockBuffer;
    FBufferPool: PBufferPool;

    FHttpState: TDiocpHttpState;
    FCurrentRequest: TDiocpHttpRequest;
    /// <summary>
    ///   清理请求列表中的对象
    /// </summary>
    procedure ClearTaskListRequest;

    {$IFDEF QDAC_QWorker}
    procedure OnExecuteJob(pvJob:PQJob);
    {$ENDIF}
    {$IFDEF DIOCP_Task}
    procedure OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
    {$ENDIF}

    procedure InnerDoARequest(pvRequest:TDiocpHttpRequest);

    // 执行事件
    procedure DoRequestBACK(pvRequest:TDiocpHttpRequest);

    procedure InnerPushRequest(pvRequest:TDiocpHttpRequest);

    procedure InnerTriggerDoRequest;

    procedure OnBlockBufferWrite(pvSender:TObject; pvBuffer:Pointer;
        pvLength:Integer);

    procedure DoSendBufferCompleted(pvBuffer: Pointer; len: Cardinal; pvBufferTag:
        Integer; pvTagData: Pointer; pvErrorCode: Integer); override;
    procedure SetContextType(const Value: Integer);

    /// <summary>
    ///  发送一块数据
    /// </summary>
    procedure CheckSendStreamBlock();
    function GetResponsing: Boolean;

    procedure InnerDoSendStreamDone(pvCode:Integer);

  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure PostWebSocketSendBuffer(pvBuffer: Pointer; len: Int64; opcode: Byte);
    procedure PostWebSocketData(const s:string; pvConvertToUtf8:Boolean);
    procedure PostWebSocketPing();
  public
    /// <summary>
    ///   准备发送一个流(依次读取发送),如果还有未发送任务，则抛出异常
    /// </summary>
    /// <param name="pvCloseAction">
    ///    0:关闭
    ///    1:不关闭
    /// </param>
    procedure PostWriteAStream(pvStream: TStream; pvSize, pvCloseAction: Integer;
        pvDoneCallBack: TWorkDoneCallBack);

    procedure SetBufferPool(ABufferPool: PBufferPool);

  public
    property ContextType: Integer read FContextType write SetContextType;

    // 响应状态
    //  0:默认, 1:发送流(异步发送), 2: 错误信息响应
    property ResponseState: Integer read FResponseState write FResponseState;

    /// <summary>
    ///  正在响应
    /// </summary>
    property Responsing: Boolean read GetResponsing;



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

    procedure WriteResponseBuffer(buf: Pointer; len: Cardinal);
    procedure FlushResponseBuffer();
  end;



  /// <summary>
  /// Http 解析服务
  /// </summary>
  TDiocpHttpServer = class(TDiocpTcpServer)
  private
    FRequestObjCounter:Integer;
    FRequestObjOutCounter:Integer;
    FRequestObjReturnCounter:Integer;
    FRequestQueueSize:Integer;
    FSessionObjCounter:Integer;
    
    // 内存池
    // 目前用与发送
    FBlockBufferPool:PBufferPool;
    FAccessXRequest: Boolean;
    FDisableSession: Boolean;

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
    function GetHttpRequest: TDiocpHttpRequest;

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
  protected
    procedure DoAfterOpen; override;
    procedure DoAfterClose;override;

    /// <summary>
    ///   当创建新的连接对象时会调用的函数
    ///   可以在这里面做一些初始化
    /// </summary>
    procedure OnCreateClientContext(const context: TIocpClientContext); override;
  public
    /// <summary>
    ///   发送Ping到所有的客户端
    /// </summary>
    procedure WebSocketSendPing();
  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    procedure RegisterSessionClass(pvClass:TDiocpHttpSessionClass);

    /// <summary>
    ///   允许跨域访问, 设置后，SendRespnose加入响应的响应头
    /// </summary>
    property AccessXRequest: Boolean read FAccessXRequest write FAccessXRequest;

    /// <summary>
    ///   内存池
    /// </summary>
    property BlockBufferPool: PBufferPool read FBlockBufferPool;
    property RequestObjCounter: Integer read FRequestObjCounter;
    
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

    function GetPrintDebugInfo: string;

  published

    /// <summary>
    ///   禁用Session
    /// </summary>
    property DisableSession: Boolean read FDisableSession write FDisableSession;

    /// <summary>
    ///   处理逻辑线程执行逻辑前执行CoInitalize
    /// </summary>
    property LogicWorkerNeedCoInitialize: Boolean read FLogicWorkerNeedCoInitialize
        write FLogicWorkerNeedCoInitialize;

    property SessionTimeOut: Integer read FSessionTimeOut write FSessionTimeOut;






  end;

function GetWebSocketAccept(pvWebSocketKey:AnsiString): AnsiString;



implementation

uses
  ComObj;



function GetFileLastModifyTimeEx(pvFileName: AnsiString): TDateTime;
var
  hFile: THandle;
  mCreationTime: TFileTime;
  mLastAccessTime: TFileTime;
  mLastWriteTime: TFileTime;
  dft:DWord;
begin
  hFile := _lopen(PAnsiChar(pvFileName), OF_READ);
  GetFileTime(hFile, @mCreationTime, @mLastAccessTime, @mLastWriteTime);
  _lclose(hFile);
  FileTimeToLocalFileTime(mLastWriteTime, mCreationTime);
  FileTimeToDosDateTime(mCreationTime, LongRec(dft).Hi,LongRec(dft).Lo);
  Result:=FileDateToDateTime(dft);
end;

function GetFileLastModifyTime(const AFileName:string): TDateTime;
var
  lvF,FSize:LongInt;
begin
  lvF:=FileOpen(AFileName, fmOpenRead or fmShareDenyNone);
  if lvF > 0 then
  begin
    Result:=FileDateToDateTime(FileGetDate(lvF));
    FileClose(lvF);
  end else
  begin
    Result := 0;
  end;
end;

function GetETagFromFile(const AFileName:string):String;
var
  lvDateTime:TDateTime;
begin
  lvDateTime := GetFileLastModifyTime(AFileName);
  //Result := Format('W/"%d"',[DateTimeToUnix(lvDateTime)]);
  Result := Format('W/"%s"',[FormatDateTime('yyMMddhhnnsszzz',lvDateTime)]);

end;
  
{$IFDEF DIOCP_DEBUG}
var
  __debug_tag:Integer;
{$ENDIF}

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

function GetWebSocketAccept(pvWebSocketKey:AnsiString): AnsiString;
var
  Key: AnsiString;
  Bin: TBytes;
begin
  Key := pvWebSocketKey + MHSTR;
  Bin := TBytes(SHA1Bin(Key));
  Result := Base64Encode(@Bin[0], Length(Bin));
end;


procedure TDiocpHttpRequest.Clear;
begin
  if FRange <> nil then FRange.Clear;
  FResponse.Clear;
  FReleaseLater := false;
  FInnerRequest.DoCleanUp;
  FInnerWebSocketFrame.DoCleanUp;
  FWebSocketContentBuffer.Clear;
  FDecodeState := 0;
  FDecodeMsg := STRING_EMPTY;
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
  FInnerWebSocketFrame := TDiocpWebSocketFrame.Create;
  FResponse := TDiocpHttpResponse.Create();
  FWebSocketContentBuffer := TDBufferBuilder.Create;
  FRefCounter := 0;

  //FRequestParamsList := TStringList.Create; // TODO:创建存放http参数的StringList
end;

destructor TDiocpHttpRequest.Destroy;
begin
  if FRange <> nil then
  begin
    FreeAndNil(FRange);
    FRange := nil;
  end;
  FreeAndNil(FResponse);
  FDebugStrings.Free;

  //FreeAndNil(FRequestParamsList); // TODO:释放存放http参数的StringList
  FWebSocketContentBuffer.Free;
  FInnerRequest.Free;
  FInnerWebSocketFrame.Free;

  FLocker.Free;

  __free_flag := -1;

  inherited Destroy;
end;

procedure TDiocpHttpRequest.AddDebugStrings(const pvDebugInfo: String;
    pvAddTimePre: Boolean = true);
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

procedure TDiocpHttpRequest.AddRef;
begin
  Self.Connection.IncRecvRef;  
  AtomicIncrement(self.FRefCounter);  
end;

procedure TDiocpHttpRequest.DoResponseEnd;
begin
  if not (FResponse.FInnerResponse.ResponseCode in [0,200]) then
  begin
    FDiocpContext.PostWSACloseRequest;
  end else if not FInnerRequest.CheckKeepAlive then
  begin
    FDiocpContext.PostWSACloseRequest;
  end else if SameText(FResponse.Header.ForceByName('Connection').AsString, 'close') then
  begin
    FDiocpContext.PostWSACloseRequest;
  end;
  
  Self.Clear;
end;

procedure TDiocpHttpRequest.CheckCookieSession;
begin
  if TDiocpHttpServer(Connection.Owner).FDisableSession then
  begin
    raise Exception.Create('Session已经被禁用！');
  end;

  // 对session的处理
  FSessionID := GetCookie(SESSIONID);
  if FSessionID = '' then
  begin
    FSessionID := SESSIONID + '_' + DeleteChars(CreateClassID, ['-', '{', '}']);
    Response.AddCookie(SESSIONID, FSessionID);
  end;
end;

function TDiocpHttpRequest.CheckIsRangeRequest: Boolean;
var
  s:string;
begin
  if (FRange = nil) then
    FRange := THttpRange.Create; 

  if (FRange.Count = -1) then
  begin
    s := Header.GetValueByName('Range', STRING_EMPTY);
    FRange.ParseRange(s);
  end;

  Result := FRange.Count > 0; 
end;

function TDiocpHttpRequest.CheckIsWebSocketRequest: Boolean;
var
  lvUpgrade:string;
begin
  //HTTP/1.1 101 Switching Protocols
  //Upgrade: websocket
  //Connection: Upgrade
  //Sec-WebSocket-Accept: K7DJLdLooIwIG/MOpvWFB3y3FE8=
  Result := False;

  lvUpgrade := Header.GetValueByName('Upgrade', '');

  if SameText(lvUpgrade, 'websocket') then
  begin
    Result := True;

    //pvRequest.Response.
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

procedure TDiocpHttpRequest.CheckThreadSetInfo(const pvDebugInfo: string);
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

procedure TDiocpHttpRequest.DecodeURLParamAsIE;
begin
  FInnerRequest.DecodeURLParamAsIE;
end;

procedure TDiocpHttpRequest.DecodeURLParam(pvUseUtf8Decode:Boolean);
begin
  FInnerRequest.DecodeURLParam(pvUseUtf8Decode);
end;

function TDiocpHttpRequest.DecRef: Boolean;
var
  lvConnection:TDiocpHttpClientContext;
begin
  // 预先存临时变量，避免close后，connection改变
  lvConnection := Self.Connection;
  if AtomicDecrement(Self.FRefCounter) = 0 then
  begin
    Self.Close;
    Result := True;
  end;

  // 后执行，避免先投递了接收请求
  lvConnection.DecRecvRef;


end;

procedure TDiocpHttpRequest.ErrorResponse(pvCode:Integer; pvMsg:String);
begin
  self.Connection.FResponseState := Response_state_err;
  self.FResponse.ResponseCode := pvCode;
  if Length(pvMsg) > 0 then
  begin
    self.FResponse.SetContentType('text/plan;chartset=utf-8;');
    self.FResponse.WriteString(pvMsg, True);
  end else
  begin
    self.FResponse.ContentBody.Clear;
  end;
  SendResponse();
  DoResponseEnd();    
end;

function TDiocpHttpRequest.GetBodyAsString: String;
var
  lvCharset:String;
begin
  lvCharset := Self.GetCharset;
  if SameText(lvCharset, 'utf-8') then
  begin
    Result := self.ContentBody.DecodeUTF8;
  end else
  begin
    Result := self.ContentBody.ToString();
  end;

end;

function TDiocpHttpRequest.GetContentLength: Int64;
begin
  Result := FInnerRequest.ContentLength;
end;

function TDiocpHttpRequest.GetContentType: String;
begin
  Result := FInnerRequest.ContentType;
end;

function TDiocpHttpRequest.GetCharset: string;
begin
  Result := FInnerRequest.Charset;
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

procedure TDiocpHttpRequest.InnerAddToDebugStrings(const pvMsg: String);
begin
  FDebugStrings.Add(pvMsg);
  if FDebugStrings.Count > 500 then FDebugStrings.Delete(0);
end;

function TDiocpHttpRequest.InputBuffer(const buf: Byte): Integer;
begin
  if Connection.ContextType = Context_Type_WebSocket then
  begin
    Result := FInnerWebSocketFrame.InputBuffer(buf);
    if Result = 1 then
    begin
      FWebSocketContentBuffer.AppendBuffer(FInnerWebSocketFrame.ContentBuffer, FInnerWebSocketFrame.ContentLength);

      if FInnerWebSocketFrame.GetFIN <> FIN_EOF then
      begin
        Result := 0;
        FInnerWebSocketFrame.DoCleanUp();
      end;
    end;
  end else
  begin
    Result := FInnerRequest.InputBuffer(buf);
  end;
  FDecodeState := Result;
end;

procedure TDiocpHttpRequest.ResponseAFile(pvFileName: string);
var
  lvFileStream:TFileStream;
begin
  lvFileStream := TFileStream.Create(pvFileName, fmOpenRead or fmShareDenyNone);
  ResponseAStream(lvFileStream, nil);
end;

function TDiocpHttpRequest.ResponseAFileETag(const pvFileName: string): Boolean;
var
  lvFileStream:TFileStream;
  lvDateTime:TDateTime;
  lvETag, lvReqTag:string;
begin
  Result := False;
  if not FileExists(pvFileName) then
  begin
    ErrorResponse(404, STRING_EMPTY);
    Exit;
  end;
  lvReqTag := self.Header.GetValueByName('If-None-Match', STRING_EMPTY);
  lvETag := GetETagFromFile(pvFileName);
  if lvReqTag = lvETag then
  begin
    ErrorResponse(304, STRING_EMPTY);
    exit;
  end;

  Response.Header.ForceByName('ETag').AsString := lvETag;

  Result := true;

end;

procedure TDiocpHttpRequest.ResponseAFileEx(const pvFileName: string);
var
  lvFileStream:TFileStream;
begin
  if ResponseAFileETag(pvFileName) then
  begin
    Response.ContentType := GetContentTypeFromFileExt(ExtractFileExt(pvFileName), 'text/html');
    lvFileStream := TFileStream.Create(pvFileName, fmOpenRead or fmShareDenyNone);
    ResponseAStream(lvFileStream, nil);
  end;
end;

procedure TDiocpHttpRequest.ResponseAStream(const pvStream: TStream;
    pvDoneCallBack: TWorkDoneCallBack);
var
  lvSize:Int64;
  lvRange:PRange;
  lvIsRangeResonse:Boolean;
  lvCloseAction:Integer;
begin
  if not (FResponse.FInnerResponse.ResponseCode in [0,200]) then
  begin
    lvCloseAction := 0;
  end else if not FInnerRequest.CheckKeepAlive then
  begin
    lvCloseAction := 0;
  end else if SameText(FResponse.Header.GetValueByName('Connection', STRING_EMPTY), 'close') then
  begin
    lvCloseAction := 0;
  end else
  begin
    lvCloseAction := 1;
  end;

  lvIsRangeResonse := False;
  if CheckIsRangeRequest then
  begin
    lvRange := FRange.IndexOf(0);
    lvSize := pvStream.Size;
    if lvRange.VEnd = 0 then
    begin
      lvRange.VEnd := lvSize - 1;
    end;

    if (lvRange.VStart < lvSize) then
    begin 
      if lvRange.VEnd > lvSize then
      begin
        lvRange.VEnd := lvSize -1;
      end;
      
      Response.Header.ForceByName('Content-Range').AsString := Format(' bytes %d-%d/%d', [
         lvRange.VStart, lvRange.VEnd, lvSize]);

      Response.SetResponseCode(206);  // 206 Partial Content
      pvStream.Position := lvRange.VStart;
      lvIsRangeResonse := True;
      //SendResponse(pvStream.Size);
      SendResponse(lvRange.VEnd - lvRange.VStart + 1);
      Connection.PostWriteAStream(pvStream, lvRange.VEnd - lvRange.VStart + 1, lvCloseAction, pvDoneCallBack);
      Exit;
    end;
  end;

  if (not lvIsRangeResonse) then
  begin
    SendResponse(pvStream.Size);
    pvStream.Position := 0;
    Connection.PostWriteAStream(pvStream, pvStream.Size, lvCloseAction, pvDoneCallBack);
  end;
end;

procedure TDiocpHttpRequest.ResponseEnd;
begin
  SendResponse();
  DoResponseEnd;
end;




procedure TDiocpHttpRequest.ResponseForWebSocketShake;
var
  lvBuffer:AnsiString;
  lvWebSocketKey:AnsiString;
  lvBlockBuffer:TBlockBuffer;
begin
  lvBlockBuffer :=self.Connection.FBlockBuffer;
  lvBlockBuffer.Lock;
  try
    lvWebSocketKey := Header.GetValueByName('Sec-WebSocket-Key', '');

    lvBuffer := 'HTTP/1.1 101 Switching Protocols'#13#10;
    self.Connection.WriteResponseBuffer(PAnsiChar(lvBuffer), Length(lvBuffer));

    lvBuffer := 'Server: DIOCP/1.1'#13#10;
    self.Connection.WriteResponseBuffer(PAnsiChar(lvBuffer), Length(lvBuffer));

    lvBuffer := 'Upgrade: websocket'#13#10;
    self.Connection.WriteResponseBuffer(PAnsiChar(lvBuffer), Length(lvBuffer));
  
    lvBuffer := 'Connection: Upgrade'#13#10;
    self.Connection.WriteResponseBuffer(PAnsiChar(lvBuffer), Length(lvBuffer));

    lvBuffer := 'Sec-WebSocket-Accept: ' + GetWebSocketAccept(lvWebSocketKey) + #13#10#13#10;
    self.Connection.WriteResponseBuffer(PAnsiChar(lvBuffer), Length(lvBuffer));
    self.Connection.FlushResponseBuffer;
  finally
    lvBlockBuffer.UnLock;
  end;
  //HTTP/1.1 101 Switching Protocols
  //Server: DIOCP/1.1
  //Upgrade: websocket
  //Connection: Upgrade
  //Sec-WebSocket-Accept: Xc/VVMNKizn2QuORua7dU8kW0Og=
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
begin
  if FResponse.Header.FindByName('Connection') = nil then
  begin
    if (FInnerRequest.CheckKeepAlive) and (FResponse.ResponseCode in [0, 200]) then
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

  FDiocpContext.FBlockBuffer.Lock;
  try
    FDiocpContext.WriteResponseBuffer(FResponse.FInnerResponse.HeaderBuilder.Memory,
      FResponse.FInnerResponse.HeaderBuilder.Size);
    if FResponse.FInnerResponse.ContentBuffer.Length > 0 then
    begin
      FDiocpContext.WriteResponseBuffer(FResponse.FInnerResponse.ContentBuffer.Memory,
        FLastResponseContentLength);
    end;

    FDiocpContext.FlushResponseBuffer;
  finally
    FDiocpContext.FBlockBuffer.UnLock;
  end;

//  FDiocpContext.PostWSASendRequest(FResponse.FInnerResponse.HeaderBuilder.Memory,
//    FResponse.FInnerResponse.HeaderBuilder.Size);

//  if FResponse.FInnerResponse.ContentBuffer.Length > 0 then
//  begin
//    FDiocpContext.PostWSASendRequest(FResponse.FInnerResponse.ContentBuffer.Memory,
//      FLastResponseContentLength);
//  end;

end;

procedure TDiocpHttpRequest.SendResponseBuffer(pvBuffer:PByte; pvLen:Cardinal);
begin
  FDiocpContext.WriteResponseBuffer(FResponse.FInnerResponse.HeaderBuilder.Memory,
    FResponse.FInnerResponse.HeaderBuilder.Size);
  //FDiocpContext.PostWSASendRequest(pvBuffer, pvLen);
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

function TDiocpHttpResponse.GetResponseCode: Integer;
begin
  Result := FInnerResponse.ResponseCode;
end;

function TDiocpHttpResponse.GetResponseID: string;
begin
  Result := FInnerResponse.ResponseID;
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

procedure TDiocpHttpResponse.LZOCompressContent;
begin
  FInnerResponse.LZOCompressContent;  
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

procedure TDiocpHttpResponse.SetResponseCode(const Value: Integer);
begin
  FInnerResponse.ResponseCode := Value;
end;

procedure TDiocpHttpResponse.SetResponseFileName(const pvFile:String);
begin
  Header.Add('Content-Disposition','attachment; filename="' + pvFile + '"');
end;

procedure TDiocpHttpResponse.SetResponseID(const Value: string);
begin
  FInnerResponse.ResponseID := Value;
end;

procedure TDiocpHttpResponse.ZLibContent;
begin
  FInnerResponse.ZCompressContent;
end;

constructor TDiocpHttpClientContext.Create;
begin
  inherited Create;
  FBlockBuffer := TBlockBuffer.Create(nil);
  FBlockBuffer.OnBufferWrite := OnBlockBufferWrite;
  FRequestQueue := TSimpleQueue.Create();
  __free_flag := 0;
end;

destructor TDiocpHttpClientContext.Destroy;
begin
  __free_flag := FREE_FLAG;
  
  FBlockBuffer.Free;

  ClearTaskListRequest;
  
  FRequestQueue.Free;
  inherited Destroy;
end;

procedure TDiocpHttpClientContext.CheckSendStreamBlock;
var
  lvBuffer:PByte;
  l, r:Integer;
begin
  self.Lock;
  try
    lvBuffer := GetBuffer(FBufferPool);
    l := FBufferPool.FBlockSize;
    r := self.FCurrentStream.Read(lvBuffer^, l);
    AddRef(lvBuffer);
    Dec(self.FCurrentStreamRemainSize, r);
    if not PostWSASendRequest(lvBuffer, r, False, BLOCK_STREAM_BUFFER_TAG) then
    begin
      ReleaseRef(lvBuffer);
      InnerDoSendStreamDone(-1);
    end;
  finally
    self.UnLock;
  end;
end;

procedure TDiocpHttpClientContext.ClearTaskListRequest;
var
  lvTask:TDiocpHttpRequest;
begin
  self.Lock;
  try
    while True do
    begin
      lvTask := TDiocpHttpRequest(FRequestQueue.DeQueue);
      if lvTask = nil then Break;

      InterlockedDecrement(TDiocpHttpServer(FOwner).FRequestQueueSize);

      // 归还到任务池
      lvTask.Close;
    end;
    FIsProcessRequesting := False;
  finally
    self.UnLock;
  end;  
end;

procedure TDiocpHttpClientContext.DoCleanUp;
begin 
  FHttpState := hsCompleted;
  if FCurrentRequest <> nil then
  begin
    FCurrentRequest.Close;
    FCurrentRequest := nil;
  end;
  FBlockBuffer.CheckThreadNone;
  FBlockBuffer.ClearBuffer;

  FContextType:= 0;
  // 清理待处理请求队列
  ClearTaskListRequest;

  if FCurrentStream <> nil then
  begin           // 中断了发送
    InnerDoSendStreamDone(-1);
  end;
  FCurrentStreamRemainSize := 0;
  FCurrentStreamEndAction := 0;

  FResponseRef := 0;


  inherited DoCleanUp;
end;

procedure TDiocpHttpClientContext.InnerPushRequest(
  pvRequest: TDiocpHttpRequest);
begin
  FRequestQueue.EnQueue(pvRequest);
end;


procedure TDiocpHttpClientContext.DoRequestBACK(pvRequest:TDiocpHttpRequest);
begin
  {$IFDEF INNER_IOCP_PROCESSOR}
  InnerDoARequest(pvRequest);
  {$ELSE}


  if FRequestQueue.Size > 1000 then
  begin
    try
      pvRequest.Connection.RequestDisconnect('未处理的请求队列过大', pvRequest);
    finally
      pvRequest.Close;
    end;
    Exit;
  end;

  self.Lock;
  try
    FRequestQueue.EnQueue(pvRequest);

    InterlockedIncrement(TDiocpHttpServer(FOwner).FRequestQueueSize);

    if not FIsProcessRequesting then
    begin 
      {$IFDEF QDAC_QWorker}
      Workers.Post(OnExecuteJob, FRequestQueue);
      {$ELSE}
      {$IFDEF DIOCP_TASK}
      iocpTaskManager.PostATask(OnExecuteJob, FRequestQueue);
      {$ELSE}
      警告无处理处理逻辑方法，请定义处理宏// {$DEFINE DIOCP_TASK}
      {$ENDIF}
      {$ENDIF}
    end;
  finally
    self.UnLock;
  end;
  {$ENDIF}
end;

procedure TDiocpHttpClientContext.DoSendBufferCompleted(pvBuffer: Pointer; len:
    Cardinal; pvBufferTag: Integer; pvTagData: Pointer; pvErrorCode: Integer);
{$IFDEF DIOCP_DEBUG}
var
  r:Integer;
{$ENDIF}
begin
  inherited;
  {$IFDEF DIOCP_DEBUG}
  if pvBufferTag >= BLOCK_BUFFER_TAG then
  begin
    r := ReleaseRef(pvBuffer, Format('- DoSendBufferCompleted(%d)', [pvBufferTag]));
    PrintDebugString(Format('- %x: %d', [IntPtr(pvBuffer), r]));
  end else if pvBufferTag = BLOCK_STREAM_BUFFER_TAG then
  begin
    r := ReleaseRef(pvBuffer, Format('- DoSendBufferCompleted(%d)', [pvBufferTag]));
    PrintDebugString(Format('- %x: %d', [IntPtr(pvBuffer), r]));
  end;
  {$ELSE}
  if pvBufferTag >= BLOCK_BUFFER_TAG then
  begin
    ReleaseRef(pvBuffer);
  end else if pvBufferTag = BLOCK_STREAM_BUFFER_TAG then
  begin
    ReleaseRef(pvBuffer);
  end;
  {$ENDIF}

  if pvBufferTag = BLOCK_STREAM_BUFFER_TAG then
  begin
    if pvErrorCode = 0 then
    begin
      if FCurrentStreamRemainSize > 0 then
      begin
        CheckSendStreamBlock;
      end else
      begin
        InnerDoSendStreamDone(0);
        FCurrentStreamRemainSize := 0;
        if FCurrentStreamEndAction = 0 then
        begin
          PostWSACloseRequest;
        end else
        begin
        
        end;
      end;
    end else
    begin
      InnerDoSendStreamDone(-1);
      FCurrentStreamRemainSize := 0;
    end;
  end;
end;

procedure TDiocpHttpClientContext.FlushResponseBuffer;
begin
  FBlockBuffer.FlushBuffer;
end;

function TDiocpHttpClientContext.GetResponsing: Boolean;
begin
  Result := FResponseRef > 0;
end;

procedure TDiocpHttpClientContext.InnerDoARequest(pvRequest: TDiocpHttpRequest);
var
  lvObj:TDiocpHttpRequest;
begin
  lvObj := pvRequest;
  try
    // 连接已经断开, 放弃处理逻辑
    if (Self = nil) then Exit;

    // 连接已经断开, 放弃处理逻辑
    if (FOwner = nil) then Exit;

    {$IFDEF DIOCP_DEBUG}
    lvObj.CheckThreadIn;
    lvObj.AddDebugStrings('DoRequest::' + pvRequest.RequestURI);
    {$ENDIF}

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
    {$IFDEF DIOCP_DEBUG}
    lvObj.CheckThreadOut;
    {$ENDIF}
    if (lvObj.FRefCounter > 0) then
    begin

    end else if lvObj.FReleaseLater then             
    begin
    
    end else
    begin
      lvObj.Close;
    end;
  end;
end;

procedure TDiocpHttpClientContext.InnerDoSendStreamDone(pvCode:Integer);
begin
  InterlockedDecrement(FResponseRef);
  if Assigned(FCurrentStreamDoneCallBack) then
  begin
    FCurrentStreamDoneCallBack(FCurrentStream, pvCode);
  end else
  begin
    FCurrentStream.Free;
  end;
  FCurrentStream := nil;

  // 发送完成。可以投递接收请求
  self.DecRecvRef;
end;

procedure TDiocpHttpClientContext.InnerTriggerDoRequest;
{$IFDEF INNER_IOCP_PROCESSOR}
var
  lvObj:TDiocpHttpRequest;
{$ENDIF}
begin
   Self.FResponseState := 0;

{$IFDEF INNER_IOCP_PROCESSOR}
    while (Self.Active) do
    begin
      //取出一个任务
      lvObj := TDiocpHttpRequest(FRequestQueue.DeQueue);
      if lvObj = nil then
      begin
        Break;;
      end;
      InterlockedDecrement(TDiocpHttpServer(FOwner).FRequestQueueSize);
      InnerDoARequest(lvObj);
    end;
{$ELSE}


    if FRequestQueue.Size > 1000 then
    begin
      RequestDisconnect('未处理的请求队列过大', nil);
      Exit;
    end;

    InterlockedIncrement(TDiocpHttpServer(FOwner).FRequestQueueSize);

    {$IFDEF QDAC_QWorker}
    self.IncRecvRef;
    Workers.Post(OnExecuteJob, FRequestQueue);
    {$ELSE}
    {$IFDEF DIOCP_TASK}
    self.IncRecvRef;
    iocpTaskManager.PostATask(OnExecuteJob, FRequestQueue);
    {$ELSE}
    警告无处理处理逻辑方法，请定义处理宏// {$DEFINE DIOCP_TASK}
    {$ENDIF}
    {$ENDIF}

  {$ENDIF}

  
end;


procedure TDiocpHttpClientContext.OnBlockBufferWrite(pvSender:TObject;
    pvBuffer:Pointer; pvLength:Integer);
{$IFDEF DIOCP_DEBUG}
var
  r , n:Integer;
{$ENDIF}
begin
  if not Self.Active then Exit;
  {$IFDEF DIOCP_DEBUG}
  n := BLOCK_BUFFER_TAG + AtomicIncrement(__debug_tag);
  if n < BLOCK_BUFFER_TAG then
  begin
    __debug_tag := 0;
    n := BLOCK_BUFFER_TAG + 1;
  end;
  r := AddRef(pvBuffer, Format('+ OnBlockBufferWrite(%d)', [n]));
  PrintDebugString(Format('+ %2x: %d', [IntPtr(pvBuffer), r]));
  if not Self.PostWSASendRequest(pvBuffer, pvLength, dtNone, n) then
  begin
    r := ReleaseRef(pvBuffer, '- OnBlockBufferWrite PostWSASendRequest false');
    PrintDebugString(Format('- %2x: %d', [IntPtr(pvBuffer), r]));
  end;
  {$ELSE}
  AddRef(pvBuffer);
  if not Self.PostWSASendRequest(pvBuffer, pvLength, dtNone, BLOCK_BUFFER_TAG) then
  begin
     ReleaseRef(pvBuffer);
  end;
  {$ENDIF}
end;

{$IFDEF QDAC_QWorker}
procedure TDiocpHttpClientContext.OnExecuteJob(pvJob:PQJob);
var
  lvObj:TDiocpHttpRequest;
begin
  // 连接已经断开, 放弃处理逻辑
  if (FOwner = nil) then Exit;

  if __free_flag = FREE_FLAG then
  begin
    Exit;
  end;
  if Self.LockContext('qworker httptask', nil) then
  try
    //取出一个任务
    self.Lock;
    try
      if FIsProcessRequesting then Exit;
      FIsProcessRequesting := True;
    finally
      self.UnLock;
    end;

    // 如果需要执行
    if TDiocpHttpServer(FOwner).LogicWorkerNeedCoInitialize then
      pvJob.Worker.ComNeeded();

    while (Self.Active) do
    begin
      //取出一个任务
      lvObj := TDiocpHttpRequest(FRequestQueue.DeQueue);
      if lvObj = nil then
      begin
        Break;;
      end;
      InterlockedDecrement(TDiocpHttpServer(FOwner).FRequestQueueSize);
      InnerDoARequest(lvObj);
    end;
    Self.DecRecvRef;
  finally
    self.UnLockContext('qworker httptask', nil);
  end;

end;

{$ENDIF}

{$IFDEF DIOCP_Task}
procedure TDiocpHttpClientContext.OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
var
  lvObj:TDiocpHttpRequest;
begin
  // 连接已经断开, 放弃处理逻辑
  if (FOwner = nil) then Exit;

  if __free_flag = FREE_FLAG then Exit;

  if Self.LockContext('iocptask httptask', nil) then
  try 
    // 如果需要执行
    if TDiocpHttpServer(FOwner).LogicWorkerNeedCoInitialize then
      pvTaskRequest.iocpWorker.checkCoInitializeEx();

    while (Self.Active) do
    begin
      //取出一个任务
      lvObj := TDiocpHttpRequest(FRequestQueue.DeQueue);
      if lvObj = nil then
      begin
        Break;;
      end;
      InterlockedDecrement(TDiocpHttpServer(FOwner).FRequestQueueSize);
      InnerDoARequest(lvObj);

    end;
    Self.DecRecvRef;
  finally
    self.UnLockContext('iocptask httptask', nil);
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
  try
    while (lvRemain > 0) do
    begin
      if FCurrentRequest = nil then
      begin
        FCurrentRequest := TDiocpHttpServer(Owner).GetHttpRequest;
        FCurrentRequest.FDiocpContext := self;
        FCurrentRequest.Response.FDiocpContext := self;
        FCurrentRequest.Clear;

        // 记录当前contextDNA，异步任务时做检测
        FCurrentRequest.FContextDNA := self.ContextDNA;
      end;


      try
        r := FCurrentRequest.InputBuffer(lvTmpBuf^);
      except
        on e:Exception do
        begin
          r := -1;
          FCurrentRequest.FDecodeState := -2;
          FCurrentRequest.FDecodeMsg := e.Message;
        end;
      end;

      if r = -1 then
      begin                             
      ///  不能在这里处理, responseEnd会访问TBlockWriter，造成多线程访问
  //      lvTempRequest := FCurrentRequest;
  //      try
  //        FCurrentRequest := nil;
  //        lvTempRequest.Response.FInnerResponse.ResponseCode := 400;
  //        //lvTempRequest.Response.WriteString(PAnsiChar(lvTmpBuf) + '<BR>******<BR>******<BR>' + PAnsiChar(buf));
  //        lvTempRequest.ResponseEnd;
  //      finally
  //        lvTempRequest.Close;
  //      end;

        lvTempRequest := FCurrentRequest;

        // 避免断开后还回对象池，造成重复还回
        FCurrentRequest := nil;

        InnerPushRequest(lvTempRequest);

        //self.RequestDisconnect('无效的Http请求', self);
        Exit;
      end;

      if r = -2 then
      begin
        FCurrentRequest.FDecodeMsg := '请求头超过大小限制';

        lvTempRequest := FCurrentRequest;

        // 避免断开后还回对象池，造成重复还回
        FCurrentRequest := nil;

        InnerPushRequest(lvTempRequest);


        Exit;
      end;

      if r = 0 then
      begin
        ; //需要更多的数据解码
      end else
      if r = 1 then
      begin
        if self.FContextType = Context_Type_WebSocket then
        begin
          lvTempRequest := FCurrentRequest;

          // 避免断开后还回对象池，造成重复还回
          FCurrentRequest := nil;

          InnerPushRequest(lvTempRequest);
        end else
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

            InnerPushRequest(lvTempRequest);
          end;
        end;
      end else
      if r = 2 then
      begin     // 解码到请求体
        lvTempRequest := FCurrentRequest;

        // 避免断开后还回对象池，造成重复还回
        FCurrentRequest := nil;

        // 触发事件
        InnerPushRequest(lvTempRequest);
      end;   

      Dec(lvRemain);
      Inc(lvTmpBuf);
    end;
  finally
    // 解码完成触发逻辑处理
    InnerTriggerDoRequest;
  end;
end;

procedure TDiocpHttpClientContext.PostWebSocketData(const s:string;
    pvConvertToUtf8:Boolean);
var
  lvBytes:TBytes;
begin
  if pvConvertToUtf8 then
  begin
    lvBytes := StringToUtf8Bytes(s);  
  end else
  begin
    lvBytes := StringToBytes(s);
  end;
  PostWebSocketSendBuffer(PByte(@lvBytes[0]), Length(lvBytes), OPT_TEXT);
end;

procedure TDiocpHttpClientContext.PostWebSocketPing;
begin
  self.PostWSASendRequest(@WS_MSG_PING, 2, False);
end;

procedure TDiocpHttpClientContext.PostWebSocketSendBuffer(pvBuffer: Pointer;
    len: Int64; opcode: Byte);
var
  lvWSFrame:TDiocpWebSocketFrame;
begin
  if ContextType <> Context_Type_WebSocket then
  begin
    raise Exception.Create('非WebSocket连接');
  end;
  
  lvWSFrame := TDiocpWebSocketFrame.Create;
  try
    lvWSFrame.EncodeBuffer(pvBuffer, len, true, opcode);
    FBlockBuffer.Lock;
    try
      WriteResponseBuffer(lvWSFrame.Buffer.Memory, lvWSFrame.Buffer.Length);
      FlushResponseBuffer;
    finally
      FBlockBuffer.UnLock;
    end;
  finally
    lvWSFrame.Free;
  end;
  
end;

procedure TDiocpHttpClientContext.PostWriteAStream(pvStream: TStream; pvSize,
    pvCloseAction: Integer; pvDoneCallBack: TWorkDoneCallBack);
begin
  FResponseState := Response_state_stream;
  
  self.Lock;
  try
    // 不接收请求
    Self.IncRecvRef;
    if FCurrentStream <> nil then
    begin
      if Assigned(pvDoneCallBack) then
      begin
        pvDoneCallBack(pvStream, -1);
      end else
      begin
        pvStream.Free;
      end;
      raise Exception.Create('存在未发送完成任务！');
    end;
    FCurrentStream := pvStream;
    FCurrentStreamRemainSize := pvSize;
    FCurrentStreamEndAction := pvCloseAction;
    FCurrentStreamDoneCallBack := pvDoneCallBack;
  finally
    self.UnLock;
  end;

  InterlockedIncrement(FResponseRef);
  CheckSendStreamBlock;
end;

procedure TDiocpHttpClientContext.SetBufferPool(ABufferPool: PBufferPool);
begin
  FBufferPool := ABufferPool;
  FBlockBuffer.SetBufferPool(FBufferPool);
end;

procedure TDiocpHttpClientContext.SetContextType(const Value: Integer);
begin
  if FContextType <> Value then
  begin
    if FContextType = 0 then
    begin
      FContextType := Value;
    end else
    begin
      raise Exception.Create('不允许重复设定ContextType');
    end;
  end;
end;

procedure TDiocpHttpClientContext.WriteResponseBuffer(buf: Pointer; len:
    Cardinal);
begin
  FBlockBuffer.CheckIsCurrentThread;
  FBlockBuffer.Append(buf, len);
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

  // 4K, 每次投递4k
  FBlockBufferPool := newBufferPool(SEND_BLOCK_SIZE);
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
  FreeBufferPool(FBlockBufferPool);
  inherited;
end;

procedure TDiocpHttpServer.CheckSessionTimeOut;
begin
  ;
end;

procedure TDiocpHttpServer.DoAfterClose;
begin
  inherited DoAfterClose;
  FRequestPool.FreeDataObject;
  FRequestPool.Clear;
  FRequestObjCounter := 0;

  ClearBufferPool(FBlockBufferPool);

  /// 只需要清理，清理时会归还到Session对象池
  FSessionList.Clear;
  FSessionObjectPool.WaitFor(10000);
  FSessionObjectPool.Clear;
end;

procedure TDiocpHttpServer.DoAfterOpen;
begin
  inherited DoAfterOpen;

  //{$IFDEF DEBUG}
  {$IFDEF CONSOLE}
  
    {$IFDEF INNER_IOCP_PROCESSOR}
      sfLogger.logMessage('[#] 由网络IO工作线程处理Http请求');
    {$ELSE}
      {$IFDEF DIOCP_Task}
        sfLogger.logMessage('[#] 由DIOCP-Task线程池处理Http请求');
      {$ENDIF}

      {$IFDEF QDAC_QWorker}
        sfLogger.logMessage('[#] 由QDAC-QWorkers处理Http请求');
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  //{$ENDIF}
end;

procedure TDiocpHttpServer.DoRequest(pvRequest: TDiocpHttpRequest);
var
  lvMsg:String;
  lvContext:TDiocpHttpClientContext;
  lvOptCode:Byte;
begin
  lvContext := pvRequest.Connection;
  lvContext.CheckThreadIn('DoRequest');
  try
    lvContext.BeginBusy;
    SetCurrentThreadInfo('进入Http::DoRequest');
    try
      try
        if pvRequest.FDecodeState = -1 then
        begin
          pvRequest.Response.FInnerResponse.ResponseCode := 400;
          pvRequest.ResponseEnd;
          Exit;
        end;

        if pvRequest.FDecodeState = -2 then
        begin
          pvRequest.Response.WriteString(pvRequest.FDecodeMsg);
          pvRequest.Response.FInnerResponse.ResponseCode := 503;
          pvRequest.ResponseEnd;
        end;
        
        if lvContext.ContextType = Context_Type_WebSocket then
        begin
          lvOptCode := pvRequest.InnerWebSocketFrame.GetOptCode;
          if lvOptCode = OPT_PING then
          begin
            lvContext.PostWSASendRequest(@WS_MSG_PONG, 2, False);
          end else if lvOptCode = OPT_PONG then
          begin
            ; // {noop}
          end else if lvOptCode = OPT_CLOSE then
          begin
            lvContext.RequestDisconnect('收到WebSocket-Close请求');
          end else if lvOptCode in [OPT_TEXT, OPT_BINARY] then                    
          begin
            if Assigned(FOnDiocpHttpRequest) then
            begin
              FOnDiocpHttpRequest(pvRequest);
            end;
          end;
        end else
        begin
          if not FDisableSession then
            pvRequest.CheckCookieSession;

          if Assigned(FOnDiocpHttpRequest) then
          begin
            FOnDiocpHttpRequest(pvRequest);
          end;
        end;
      except
        on E:Exception do
        begin
          //pvRequest.Connection.SetRecvWorkerHint('DoRequest::Exception - 1');
          self.LogMessage('Http逻辑处理异常:%s', [e.Message], '', lgvError);
          pvRequest.FReleaseLater := False;
          pvRequest.Response.FInnerResponse.ResponseCode := 500;
          pvRequest.Response.Clear;
          pvRequest.Response.ContentType := 'text/html; charset=utf-8';
          lvMsg := e.Message;
          lvMsg := StringReplace(lvMsg, sLineBreak, '<BR>', [rfReplaceAll]);
          pvRequest.Response.WriteString(lvMsg);
          //pvRequest.Connection.SetRecvWorkerHint('DoRequest::Exception - 2');
        end;
      end;
    except
      on E:Exception do
      begin
        //pvRequest.Connection.SetRecvWorkerHint('DoRequest::*Exception - 1');
        self.LogMessage('*Http逻辑处理异常:%s', [e.Message], CORE_LOG_FILE, lgvError);
        //pvRequest.Connection.SetRecvWorkerHint('DoRequest::*Exception - 2');
      end;
    end;
  finally
    lvContext.EndBusy;
    lvContext.CheckThreadOut;
    SetCurrentThreadInfo('结束Http::DoRequest');
    //pvRequest.Connection.SetRecvWorkerHint('DoRequest:: end');
  end;
end;

procedure TDiocpHttpServer.DoRequestPostDataDone(pvRequest: TDiocpHttpRequest);
begin 
  if Assigned(FOnDiocpHttpRequestPostDone) then
  begin
    FOnDiocpHttpRequestPostDone(pvRequest);
  end;
end;

function TDiocpHttpServer.GetPrintDebugInfo: string;
begin
  Result := Format(strHttpServerStateInfo,
     [Self.FSessionObjCounter,
      FRequestQueueSize,
      FRequestObjCounter, FRequestObjOutCounter, FRequestObjReturnCounter,
     self.FBlockBufferPool.FSize, self.FBlockBufferPool.FBlockSize, self.FBlockBufferPool.FPut, self.FBlockBufferPool.FGet]);
end;

function TDiocpHttpServer.GetHttpRequest: TDiocpHttpRequest;
begin
  if UseObjectPool then
  begin
    Result := TDiocpHttpRequest(FRequestPool.DeQueue);
    if Result = nil then
    begin
      Result := TDiocpHttpRequest.Create;
      InterlockedIncrement(FRequestObjCounter);
    end;
    Assert(Result.FThreadID = 0, 'request is using');
    {$IFDEF DIOCP_DEBUG}
    Result.AddDebugStrings('+ GetHttpRequest');
    {$ENDIF}
    Result.FDiocpHttpServer := Self;
    Result.FOwnerPool := FRequestPool;
    Result.Clear;
  end else
  begin
    Result := TDiocpHttpRequest.Create;
    Result.FDiocpHttpServer := Self;
    Result.Clear;
    InterlockedIncrement(FRequestObjCounter);
  end;
  InterlockedIncrement(FRequestObjOutCounter);
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
      Result.FSessionID := pvSessionID;
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

  if UseObjectPool then
  begin
    pvRequest.Clear;
    {$IFDEF DIOCP_DEBUG}
    pvRequest.AddDebugStrings('- GiveBackRequest');
    {$ENDIF}
    pvRequest.FOwnerPool := nil;
    pvRequest.FDiocpHttpServer := nil;

    FRequestPool.EnQueue(pvRequest);
  end else
  begin
    pvRequest.Free;  
  end;
  InterlockedIncrement(FRequestObjReturnCounter);
end;

procedure TDiocpHttpServer.OnCreateClientContext(const context:
    TIocpClientContext);
begin
  inherited;
  TDiocpHttpClientContext(context).SetBufferPool(Self.FBlockBufferPool);
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

procedure TDiocpHttpServer.WebSocketSendPing;
var
  lvList:TList;
  i: Integer;
  lvContext:TDiocpHttpClientContext;
begin
  lvList := TList.Create;
  try
    GetOnlineContextList(lvList);

    for i := 0 to lvList.Count - 1 do
    begin
       lvContext := TDiocpHttpClientContext(lvList[i]);
       if lvContext.LockContext('sendPing', lvContext) then
       try
         if lvContext.ContextType = Context_Type_WebSocket then
         begin
           lvContext.PostWebSocketPing();       
         end;
       finally
         lvContext.UnLockContext('sendPing', lvContext);
       end;
    end;
  finally
    lvList.Free;
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
