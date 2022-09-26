(*
  *	 Unit owner: D10.Mofen, delphi iocp framework author
  *         homePage: http://www.Diocp.org
  *	       blog: http://www.cnblogs.com/dksoft

  *   2015-02-22 08:29:43
  *     DIOCP-V5 ����

  *
  *   2015-04-08 12:34:33
  *    (��л (Xjumping  990669769)/(suoler)����bug���ṩbug����)
  *    ��Ϊ�첽����Http�����
  *      �������Ѿ��رգ���������û�����ü�����Ȼ�������������Ѿ��黹���أ����ʱ��Ӧ�÷�����������()
  *
  *   2015-07-29 12:06:08
  *   diocp_ex_httpServer�������Cookie��Session
  *
  *
  *   2015-08-25 09:56:05
  *   ����TDiocpHttpRequest�ع�����ʱ������������󡣱�����������Cookie����(��л��ľ����bug)
  *
  *   2016-10-07 20:56:41
  *   ����WebSocket֧��(��л����)
*)
unit diocp_ex_httpServer;

interface

{$I 'diocp.inc'}

/// �������뿪�أ�ֻ�ܿ���һ��
{.$DEFINE INNER_IOCP_PROCESSOR}     // iocp�̴߳����¼�
{.$DEFINE QDAC_QWorker}   // ��qworker���е��ȴ����¼�
{$DEFINE DIOCP_Task}     // ��diocp_task���е��ȴ����¼�


uses
  Classes, StrUtils, SysUtils, utils_SHA, utils_strings

  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp_task{$ENDIF}
  , diocp_tcp_server, utils_queues, utils_hashs, utils_dvalue
  , diocp_ex_http_common
  , diocp_res
  , utils_objectPool, utils_safeLogger, Windows, utils_threadinfo, SyncObjs,
  utils_BufferPool,  utils_websocket,  utils_base64, DateUtils,
  utils_fileWriter;



const
  HTTPLineBreak = #13#10;
  SESSIONID = 'diocp_sid';
  BLOCK_BUFFER_TAG = 10000;

  BLOCK_STREAM_BUFFER_TAG = 1000;
  

  Context_Type_WebSocket = 1;

  SEND_BLOCK_SIZE = 1024 * 4;

  Response_state_inital = 0;
  Response_state_stream = 1;
  Response_state_err = 2;
  Response_state_done = 2;

type
  TDiocpHttpState = (hsCompleted, hsRequest { �������� } , hsRecvingPost { �������� } );
  TDiocpHttpResponse = class;
  TDiocpHttpClientContext = class;
  TDiocpHttpServer = class;
  TDiocpHttpRequest = class;
  TDiocpHttpSession = class;

  TDiocpHttpCookie = diocp_ex_http_common.TDHttpCookie;

  TDiocpHttpSessionClass = class of TDiocpHttpSession;

{$IFDEF UNICODE}

  /// <summary>
  /// Request�¼�����
  /// </summary>
  TOnDiocpHttpRequestEvent = reference to procedure(pvRequest: TDiocpHttpRequest);
{$ELSE}
  /// <summary>
  /// Request�¼�����
  /// </summary>
  TOnDiocpHttpRequestEvent = procedure(pvRequest: TDiocpHttpRequest) of object;
{$ENDIF}

  /// <summary>
  ///  ������Session�࣬�û������Լ���չ���࣬Ȼ��ע��
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
    ///  ����ʧЧ
    /// </summary>
    procedure Invalidate;


  end;

  /// <summary>
  ///   ʹ��DValue�洢���ݵ�Session
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
    ///  WebSocket���յ�����������
    /// </summary>
    FWebSocketContentBuffer:TDBufferBuilder;

    FReleaseLater:Boolean;
    FReleaseLaterMsg:String;

    FSessionID : String;

    FInnerWebSocketFrame:TDiocpWebSocketFrame;

    FInnerRequest:THttpRequest;

    /// <summary>
    ///   Ͷ��֮ǰ��¼DNA���������첽����ʱ���Ƿ�ȡ����ǰ����
    /// </summary>
    FContextDNA : Integer;
    FDebugStrings: TStrings;

    /// <summary>
    ///   ������Closeʱ�黹�ض����
    /// </summary>
    FDiocpHttpServer:TDiocpHttpServer;

    FDiocpContext: TDiocpHttpClientContext;



    FLastResponseContentLength: Integer;

    // ��Ӧ
    //FResponseState:Byte;


    FResponse: TDiocpHttpResponse;
    FResponsing: Boolean;

    /// <summary>
    ///   ����ʹ���ˣ��黹�ض����
    /// </summary>
    procedure Close;

    /// <summary>
    ///   ���Cookie�е�SessionID��Ϣ
    ///   ������Session����
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
    ///   ����ֹrequest�ͷţ�������Ͷ�ݽ�������
    /// </summary>
    function AddRef: Boolean;

    /// <summary>
    ///   ��AddRef����ʹ��
    /// </summary>
    function DecRef: Boolean;


    /// <summary>
    ///   ����Request��ʱ�������ͷ�
    /// </summary>
    /// <param name="pvMsg"> ��Ϣ(����״̬�۲�) </param>
    procedure SetReleaseLater(pvMsg:String);

    /// <summary>
    ///  ����Ƿ���WebSocket����
    /// </summary>
    function CheckIsWebSocketRequest: Boolean;

    /// <summary>
    ///   ��ȡ��ǰSession
    ///    ���û�л���д���    
    /// </summary>
    function GetSession: TDiocpHttpSession;

    /// <summary>
    ///   �ֶ�����ǰSession
    /// </summary>
    procedure RemoveSession;

    /// <summary>
    ///  ��ȡ��ǰ�ỰID, ���û�л�����Response��Cookie��Ϣ
    /// </summary>
    function GetSessionID: String;


    /// <summary>
    ///   ��Post��ԭʼ���ݽ��룬�ŵ������б���
    ///   ��OnDiocpHttpRequest�е���
    /// </summary>
    procedure DecodePostDataParam(
      {$IFDEF UNICODE} pvEncoding:TEncoding {$ELSE}pvUseUtf8Decode:Boolean{$ENDIF});

    procedure DecodeBodyAES(const aeskey:string);

    /// <summary>
    ///   ����URL�еĲ������ŵ������б���
    ///   ��OnDiocpHttpRequest�е���
    /// </summary>
    procedure DecodeURLParam(pvUseUtf8Decode:Boolean); overload;

    /// <summary>
    ///   ����IE URL�еĲ������ŵ������б���
    ///   ��OnDiocpHttpRequest�е���
    ///   IE����δ�����κα���
    /// </summary>
    procedure DecodeURLParamAsIE(); overload;

    {$IFDEF UNICODE}
    /// <summary>
    ///   ����URL�еĲ������ŵ������б���
    ///   ��OnDiocpHttpRequest�е���
    /// </summary>
    procedure DecodeURLParam(pvEncoding:TEncoding); overload;
    {$ENDIF}

    /// <summary>
    ///   ����
    /// </summary>
    procedure Clear;

    /// <summary>
    ///  ��ȡ�����Cookieֵ
    /// </summary>
    function GetCookie(pvCookieName:string):String;

    /// <summary>
    ///   �����󱣴浽��
    /// </summary>
    procedure SaveToStream(pvStream:TStream);

    /// <summary>
    ///   �����󱣴浽�ļ�
    /// </summary>
    procedure SaveToFile(pvFile:string);

    procedure ContentSaveToFile(pvFile:String);

    function GetBodyAsString: String;

    property ContentType: String read GetContentType;

    property Charset:string read GetCharset;

    property ContentLength: Int64 read GetContentLength;


    /// <summary>
    ///   ��ͻ��˽���������
    /// </summary>
    property Connection: TDiocpHttpClientContext read FDiocpContext;
    property ContentAsMemory: PByte read GetContentAsMemory;
    property ContentAsString: RAWString read GetDataAsRawString;

    property ContentBody: TDBufferBuilder read GetContentBody;
    /// <summary>
    ///   ��������(ConentAsMemory)����Ӧ����Content-Lengthһ��
    /// </summary>
    property ContentDataLength: Integer read GetContentDataLength;




    /// <summary>
    ///   ����ͷ
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
    ///   ��ͷ��Ϣ������������Url,��������
    ///   URI + ����
    /// </summary>
    property RequestURL: String read GetRequestURL;

    /// <summary>
    ///   ��ͷ��Ϣ��ȡ������URL��δ�����κμӹ�,��������
    /// </summary>
    property RequestRawURL: String read GetRequestRawURL;

    /// <summary>
    ///   ����URL����
    /// </summary>
    property RequestURI: String read GetRequestURI;

    /// <summary>
    ///  ��ͷ��Ϣ�ж�ȡ���������������ʽ
    /// </summary>
    property RequestMethod: string read GetRequestMethod;

    /// <summary>
    ///   ��ͷ��Ϣ�ж�ȡ�����������IP��ַ
    /// </summary>
    property RequestHost: string read GetRequestHost;

    /// <summary>
    /// Http��Ӧ���󣬻�д����
    /// </summary>
    property Response: TDiocpHttpResponse read FResponse;


    property RequestRawHeaderString: string read GetRequestRawHeaderString;

    /// <summary>
    ///  ԭʼ�����е�URL��������(û�о���URLDecode����Ϊ��DecodeRequestHeader��Ҫƴ��RequestURLʱ��ʱ������URLDecode)
    ///  û�о���URLDecode�ǿ��ǵ�����ֵ�б������&�ַ�������DecodeURLParam���ֲ������쳣
    /// </summary>
    property RequestURLParamData: string read GetRequestURLParamData;

    /// <summary>
    ///   ���е���������� ע�����ǰ�ȵ���DecodeURL��DecodePostParams
    /// </summary>
    property RequestParamsList: TDValue read GetRequestParamsList;

    /// <summary>
    ///   ������Ӧ
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
    ///   ����Ӧ�����, ����Ƿ���Ҫ�ر����ӵ���Ӧ����
    /// </summary>
    procedure DoResponseEnd;


    /// <summary>
    ///   ֱ�ӷ���Response.Header��Data����
    /// </summary>
    procedure SendResponse(pvContentLength: Integer = 0);

    procedure ErrorResponse(pvCode:Integer; pvMsg:String);

    /// <summary>
    ///   ֱ�ӷ���һ���ļ�
    /// </summary>
    procedure ResponseAFile(const pvFileName: string);

    /// <summary>
    ///   ����ͷ, ��ӦETag, �������false��ʾ�Ѿ���Ӧ
    /// </summary>
    function ResponseAFileETag(const pvFileName: string): Boolean;

    /// <summary>
    ///   ֱ�ӷ���һ���ļ�
    ///    ��Ӧ����
    ///    ����
    /// </summary>
    procedure ResponseAFileEx(const pvFileName: string);

    /// <summary>
    ///   ֱ�ӷ���һ����
    /// </summary>
    procedure ResponseAStream(const pvStream: TStream; pvDoneCallBack:
        TWorkDoneCallBack);

    /// <summary>
    ///   ֱ�ӷ�������
    /// </summary>
    procedure SendResponseBuffer(pvBuffer:PByte; pvLen:Cardinal);



    /// <summary>
    ///  �ر�����
    /// </summary>
    procedure CloseContext;

    function GetDebugString: String;

    /// <summary>
    /// �õ�http�������
    /// </summary>
    /// <params>
    /// <param name="ParamsKey">http���������key</param>
    /// </params>
    /// <returns>
    /// 1: http���������ֵ
    /// </returns>
    function GetRequestParam(ParamsKey: string): string;


    /// <summary>
    ///   ��ȡ��Ӧ�����ݳ���(������ͷ��Ϣ)
    /// </summary>
    function GetResponseLength: Integer;

    /// <summary>
    ///   �뾡��ʹ��SendResponse��DoResponseEnd������
    /// </summary>
    procedure ResponseEnd;

    /// <summary>
    ///  ��ӦWEBSocket������
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
    ///   ��ͻ��˽���������
    /// </summary>
    property Connection: TDiocpHttpClientContext read FDiocpContext;


    /// <summary>
    ///   ������ֱ��ʹ��
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

    procedure AESEncodeContent(const aes_key:string);

    procedure LZOCompressContent;

    procedure SetChunkedStart;

    procedure SetChunkedEnd;

    procedure ChunkedFlush;

    procedure SetChunkedBuffer(pvBuffer:Pointer; pvLen:Integer);

    procedure SetChunkedUtf8(const pvStr: string);
  end;

  TDiocpWebSocketRequest = class(TDiocpHttpRequest)
  public

  end;

  /// <summary>
  /// Http �ͻ�������
  /// </summary>
  TDiocpHttpClientContext = class(TIocpClientContext)
  private
    __free_flag:Integer;

    {$IFDEF TRACE_HTTP_DETAIL}
    FTraceWriter:TSingleFileWriter;
    {$ENDIF}

    FRequestingFlag:Byte;



    /// <summary>
    ///   ��Ӧ���ü���
    /// </summary>
    FResponseRef:Integer;

    // ��Ӧ״̬
    FResponseState: Integer;

    FCurrentStream:TStream;
    FCurrentStreamRemainSize:Integer;
    // �Ƿ�ر����� 0�ǹر�, 1:���ر�
    FCurrentStreamEndAction:Byte;
    
    // ��ɻص��¼�
    FCurrentStreamDoneCallBack: TWorkDoneCallBack;
    
    /// <summary>
    ///   0:��ͨ Http����
    ///   1:WebSocket
    /// </summary>
    FContextType:Integer;


    
    /// ������������
    FRequestQueue:TSimpleQueue;

    /// <summary>
    ///   ���뵥�̲߳���
    /// </summary>
    FBlockBuffer: TBlockBuffer;
    FBufferPool: PBufferPool;

    FHttpState: TDiocpHttpState;
    FCurrentRequest: TDiocpHttpRequest;
    /// <summary>
    ///   ���������б��еĶ���
    /// </summary>
    procedure ClearTaskListRequest;

    {$IFDEF QDAC_QWorker}
    procedure OnExecuteJob(pvJob:PQJob);
    {$ENDIF}
    {$IFDEF DIOCP_Task}
    procedure OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
    {$ENDIF}

    procedure InnerDoARequest(pvRequest:TDiocpHttpRequest);

    procedure InnerPushRequest(pvRequest:TDiocpHttpRequest);

    procedure InnerTriggerDoRequest;



    procedure DoSendBufferCompleted(pvBuffer: Pointer; len: Cardinal; pvBufferTag:
        Integer; pvTagData: Pointer; pvErrorCode: Integer); override;
    procedure SetContextType(const Value: Integer);

    /// <summary>
    ///  ����һ������
    /// </summary>
    procedure CheckSendStreamBlock();
    function GetResponsing: Boolean;

    procedure InnerDoSendStreamDone(pvCode:Integer);
  protected
     FFlashBufferPostTag:Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure PostWebSocketSendBuffer(pvBuffer: Pointer; len: Int64; opcode: Byte);
    procedure PostWebSocketData(const s:string; pvConvertToUtf8:Boolean);
    procedure PostWebSocketPing();
  public
    /// <summary>
    ///   ׼������һ����(���ζ�ȡ����),�������δ�����������׳��쳣
    /// </summary>
    /// <param name="pvCloseAction">
    ///    0:�ر�
    ///    1:���ر�
    /// </param>
    procedure PostWriteAStream(pvStream: TStream; pvSize, pvCloseAction: Integer;
        pvDoneCallBack: TWorkDoneCallBack);

    procedure SetBufferPool(ABufferPool: PBufferPool);

  public
    property ContextType: Integer read FContextType write SetContextType;

    // ��Ӧ״̬
    //  0:Ĭ��, 1:������(�첽����), 2: ������Ϣ��Ӧ
    property ResponseState: Integer read FResponseState write FResponseState;

    /// <summary>
    ///  ������Ӧ
    /// </summary>
    property Responsing: Boolean read GetResponsing;



  protected
    /// <summary>
    /// �黹������أ�����������
    /// </summary>
    procedure DoCleanUp; override;

    procedure OnBlockBufferWrite(pvSender:TObject; pvBuffer:Pointer;
        pvLength:Integer); virtual;

    /// <summary>
    /// ���յ��ͻ��˵�HttpЭ������, ���н����TDiocpHttpRequest����ӦHttp����
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: Word);
      override;

    procedure WriteResponseBuffer(buf: Pointer; len: Cardinal);
    procedure FlushResponseBuffer();
  end;



  /// <summary>
  /// Http ��������
  /// </summary>
  TDiocpHttpServer = class(TDiocpTcpServer)
  private
    FRequestObjCounter:Integer;
    FRequestObjOutCounter:Integer;
    FRequestObjReturnCounter:Integer;
    FRequestQueueSize:Integer;
    FSessionObjCounter:Integer;
    
    // �ڴ��
    // Ŀǰ���뷢��
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
    /// ��ӦHttp���� ִ����Ӧ�¼�
    /// </summary>
    procedure DoRequest(pvRequest: TDiocpHttpRequest);

    /// <summary>
    ///   ��ӦPost�����¼�
    /// </summary>
    procedure DoRequestPostDataDone(pvRequest: TDiocpHttpRequest);

    /// <summary>
    ///   �ӳ��л�ȡһ������
    /// </summary>
    function GetHttpRequest: TDiocpHttpRequest;

    /// <summary>
    ///   ����һ������
    /// </summary>
    procedure GiveBackRequest(pvRequest:TDiocpHttpRequest);

    /// <summary>
    ///   ��ȡһ��Session����
    /// </summary>
    function GetSession(pvSessionID:string): TDiocpHttpSession;

    /// <summary>
    ///   �Ƴ���һ��Session���ͷ�
    /// </summary>
    function RemoveSession(pvSessionID:String): Boolean;

    
    function GetSessionCount: Integer;

    function OnCreateSessionObject: TObject;

    /// <summary>
    ///   SessionMapɾ����ʱ���¼����黹��Session��
    /// </summary>
    procedure OnSessionRemove(pvData:Pointer);
  protected
    procedure DoAfterOpen; override;
    procedure DoAfterClose; override;

    /// <summary>
    ///   �������µ����Ӷ���ʱ����õĺ���
    ///   ��������������һЩ��ʼ��
    /// </summary>
    procedure OnCreateClientContext(const context: TIocpClientContext); override;
  public
    /// <summary>
    ///   ����Ping�����еĿͻ���
    /// </summary>
    procedure WebSocketSendPing();
  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    procedure RegisterSessionClass(pvClass:TDiocpHttpSessionClass);

    /// <summary>
    ///   ����������, ���ú�SendRespnose������Ӧ����Ӧͷ
    /// </summary>
    property AccessXRequest: Boolean read FAccessXRequest write FAccessXRequest;

    /// <summary>
    ///   �ڴ��
    /// </summary>
    property BlockBufferPool: PBufferPool read FBlockBufferPool;
    property RequestObjCounter: Integer read FRequestObjCounter;
    
    /// <summary>
    ///   ��ȡSession����
    /// </summary>
    property SessionCount: Integer read GetSessionCount;


    /// <summary>
    ///   ��Http�����Post������ɺ󴥷����¼�
    ///   �����������һЩ����,����Post�Ĳ���
    /// </summary>
    property OnDiocpHttpRequestPostDone: TOnDiocpHttpRequestEvent read
        FOnDiocpHttpRequestPostDone write FOnDiocpHttpRequestPostDone;

    /// <summary>
    /// ��ӦHttp�����¼�
    /// </summary>
    property OnDiocpHttpRequest: TOnDiocpHttpRequestEvent read FOnDiocpHttpRequest
        write FOnDiocpHttpRequest;

    /// <summary>
    ///   ���Session��ʱ, �޳���ʱ��Session
    /// </summary>
    procedure CheckSessionTimeOut;

    function GetPrintDebugInfo: string;
    /// <summary>
    ///   ����Ping�����еĿͻ���
    /// </summary>
    procedure WebSocketSendBuffer(pvBuf: Pointer; pvBufLen: Integer; OPTCode: Byte);

  published

    /// <summary>
    ///   ����Session
    /// </summary>
    property DisableSession: Boolean read FDisableSession write FDisableSession;

    /// <summary>
    ///   �����߼��߳�ִ���߼�ǰִ��CoInitalize
    /// </summary>
    property LogicWorkerNeedCoInitialize: Boolean read FLogicWorkerNeedCoInitialize
        write FLogicWorkerNeedCoInitialize;

    property SessionTimeOut: Integer read FSessionTimeOut write FSessionTimeOut;






  end;

function GetWebSocketAccept(pvWebSocketKey:AnsiString): AnsiString;



implementation

uses
  ComObj, utils_byteTools;



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

  //FRequestParamsList := TStringList.Create; // TODO:�������http������StringList
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

  //FreeAndNil(FRequestParamsList); // TODO:�ͷŴ��http������StringList
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

function TDiocpHttpRequest.AddRef: Boolean;
begin
  if self.Connection.LockContext('TDiocpHttpRequest.Ref', nil) then
  begin
    Self.Connection.IncRecvRef;
    AtomicIncrement(self.FRefCounter);
    Result := True;
  end else
  begin
    Result := False;
  end;
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
    raise Exception.Create('Session�Ѿ������ã�');
  end;

  // ��session�Ĵ���
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
    raise Exception.CreateFmt('(%d,%d)��ǰ�����Ѿ��������߳�����ʹ��::%s',
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
    raise Exception.CreateFmt('(%d,%d)��ǰ�����Ѿ��������߳�����ʹ��::%s',
      [utils_strings.GetCurrentThreadID, FThreadID, s]);
  end;
  FThreadDebugInfo := pvDebugInfo;
end;

procedure TDiocpHttpRequest.ContentSaveToFile(pvFile:String);
begin
  FInnerRequest.ContentSaveToFile(pvFile);
end;

procedure TDiocpHttpRequest.DecodeBodyAES(const aeskey: string);
begin
  FInnerRequest.DecodeBodyWithAES(aeskey);
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
  Result := False;
  // Ԥ�ȴ���ʱ����������close��connection�ı�
  lvConnection := Self.Connection;
  Assert(lvConnection <> nil);
  if AtomicDecrement(Self.FRefCounter) = 0 then
  begin
    Self.Close;
    Result := True;
  end;
  // ��ִ�У�������Ͷ���˽�������
  lvConnection.DecRecvRef;
  lvConnection.UnLockContext('TDiocpHttpRequest.Ref', nil);

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
      FWebSocketContentBuffer.AppendBuffer(FInnerWebSocketFrame.ContentBuffer,
        FInnerWebSocketFrame.ContentLength);

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

procedure TDiocpHttpRequest.ResponseAFile(const pvFileName: string);
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
    if Length(Response.ContentType) = 0 then
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
  begin  // �������֧��
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
    Assert(False, '��Ӧ����Ϊ��');
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

procedure TDiocpHttpResponse.AESEncodeContent(const aes_key:string);
begin
  FInnerResponse.AESEncodeContent(aes_key);
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

  // ����ssl
  FDiocpContext.FBlockBuffer.Lock;
  try
    FDiocpContext.WriteResponseBuffer(PAnsiChar(lvFixedHeader),len);
    FDiocpContext.FlushResponseBuffer;
  finally
    FDiocpContext.FBlockBuffer.UnLock;
  end;
//
//  //lvFixedHeader := MakeHeader('302 Temporarily Moved', 'HTTP/1.0', false, '', '', 0);
//  lvFixedHeader := MakeHeader('307 Temporary Redirect', 'HTTP/1.0', false, '', '', 0);
//
//  lvFixedHeader := lvFixedHeader + 'Location: ' + pvURL + HTTPLineBreak;
//
//  lvFixedHeader := FixHeader(lvFixedHeader);
//
//  len := Length(lvFixedHeader);
//  FDiocpContext.PostWSASendRequest(PAnsiChar(lvFixedHeader), len);
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

procedure TDiocpHttpResponse.SetChunkedUtf8(const pvStr: string);
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
  begin     // ����Utf8ת��
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
  {$IFDEF DIOCP_DEBUG}
  if FRequestingFlag = 1 then
  begin
    Assert(FRequestingFlag=0, '���ڴ���Http����....');
  end;
  {$ENDIF}
  
  
  FBlockBuffer.Free;

  ClearTaskListRequest;
  
  FRequestQueue.Free;
  {$IFDEF TRACE_HTTP_DETAIL}
  if FTraceWriter <> nil then
  begin
    FTraceWriter.Free;
  end;
  {$ENDIF}
  inherited Destroy;
end;

procedure TDiocpHttpClientContext.CheckSendStreamBlock;
var
  lvBuffer:PByte;
  l, r:Integer;
begin
  self.Lock;
  try
    {$IFDEF DIOCP_SSL}
    FBlockBuffer.Lock;
    try
      l := FBufferPool.FBlockSize;
      GetMem(lvBuffer, l);
      r := self.FCurrentStream.Read(lvBuffer^, l);
      Dec(self.FCurrentStreamRemainSize, r);
      self.FFlashBufferPostTag := BLOCK_STREAM_BUFFER_TAG;
      FBlockBuffer.Append(lvBuffer, r); 
      FBlockBuffer.FlushBuffer;
      FreeMem(lvBuffer);

    finally
      self.FFlashBufferPostTag := 0;
      FBlockBuffer.UnLock;
    end;
    {$ELSE}
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
    {$ENDIF}
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

      // �黹�������
      lvTask.Close;
    end;
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
  // ����������������
  ClearTaskListRequest;

  if FCurrentStream <> nil then
  begin           // �ж��˷���
    InnerDoSendStreamDone(-1);
  end;
  FCurrentStreamRemainSize := 0;
  FCurrentStreamEndAction := 0;

  FResponseRef := 0;

  {$IFDEF TRACE_HTTP_DETAIL}
  if FTraceWriter <> nil then
  begin
    FTraceWriter.Free;
    FTraceWriter := nil;
  end;
  {$ENDIF}


  inherited DoCleanUp;
end;

procedure TDiocpHttpClientContext.InnerPushRequest(
  pvRequest: TDiocpHttpRequest);
begin
  FRequestQueue.EnQueue(pvRequest);
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
    //PrintDebugString(Format('- %x: %d', [IntPtr(pvBuffer), r]));
  end else if pvBufferTag = BLOCK_STREAM_BUFFER_TAG then
  begin
    r := ReleaseRef(pvBuffer, Format('- DoSendBufferCompleted(%d)', [pvBufferTag]));
    //PrintDebugString(Format('- %x: %d', [IntPtr(pvBuffer), r]));
  end;
  {$ELSE}
  if pvBufferTag >= BLOCK_BUFFER_TAG then
  begin
    ReleaseRef(pvBuffer);
  end else if pvBufferTag = BLOCK_STREAM_BUFFER_TAG then
  begin
    {$IFDEF DIOCP_SSL}
    // SSL SSL��������(dtFreeTagDataAsObject))

    {$ELSE}
    ReleaseRef(pvBuffer);
    {$ENDIF}
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
    {$IFDEF DIOCP_DEBUG}
    // �����Ѿ��Ͽ�, ���������߼�
    Assert(Self <> nil);

    // �����Ѿ��Ͽ�, ���������߼�
    Assert(FOwner <> nil);

    lvObj.CheckThreadIn;
    lvObj.AddDebugStrings('DoRequest::' + pvRequest.RequestURI);
    {$ENDIF}

    // �Ѿ����ǵ�ʱ��������ӣ� ���������߼�
    if lvObj.FContextDNA <> self.ContextDNA then
    begin
      Exit;
    end;

    // ����ر���, ֱ���˳�
    if self.RequestDisconnectFlag then Exit;     


    // �����¼�
    TDiocpHttpServer(FOwner).DoRequest(lvObj);
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

  // ������ɡ�����Ͷ�ݽ�������
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
      //ȡ��һ������
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
      RequestDisconnect('δ�����������й���', nil);
      Exit;
    end;

    InterlockedIncrement(TDiocpHttpServer(FOwner).FRequestQueueSize);

    {$IFDEF QDAC_QWorker}
    if self.LockContext('InnerTriggerDoRequest', nil) then
    begin
      self.IncRecvRef;
      Workers.Post(OnExecuteJob, FRequestQueue);
    end;
    {$ELSE}
    {$IFDEF DIOCP_TASK}
    if self.LockContext('InnerTriggerDoRequest', nil) then
    begin
      self.IncRecvRef;
      iocpTaskManager.PostATask(OnExecuteJob, FRequestQueue);
    end;
    {$ELSE}
    �����޴������߼��������붨�崦���// {$DEFINE DIOCP_TASK}
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
  //PrintDebugString(Format('+ %2x: %d', [IntPtr(pvBuffer), r]));
  if not Self.PostWSASendRequest(pvBuffer, pvLength, dtNone, n) then
  begin
    r := ReleaseRef(pvBuffer, '- OnBlockBufferWrite PostWSASendRequest false');
    //PrintDebugString(Format('- %2x: %d', [IntPtr(pvBuffer), r]));
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
  try
    {$IFDEF DIOCP_DEBUG}
    Assert(FOwner <> nil);  
    Assert(__free_flag <> FREE_FLAG);
    {$ENDIF}


    // �����Ҫִ��
    if TDiocpHttpServer(FOwner).LogicWorkerNeedCoInitialize then
      pvJob.Worker.ComNeeded();

    while (Self.Active) do
    begin
      //ȡ��һ������
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
    self.UnLockContext('InnerTriggerDoRequest', nil);
  end;

end;

{$ENDIF}

{$IFDEF DIOCP_Task}
procedure TDiocpHttpClientContext.OnExecuteJob(pvTaskRequest: TIocpTaskRequest);
var
  lvObj:TDiocpHttpRequest;
begin
  try
    {$IFDEF DIOCP_DEBUG}
    Assert(FOwner <> nil);  
    Assert(__free_flag <> FREE_FLAG);
    {$ENDIF}


    // �����Ҫִ��
    if TDiocpHttpServer(FOwner).LogicWorkerNeedCoInitialize then
      pvTaskRequest.iocpWorker.checkCoInitializeEx();

    while (Self.Active) do
    begin
      //ȡ��һ������
      lvObj := TDiocpHttpRequest(FRequestQueue.DeQueue);
      if lvObj = nil then
      begin
        Break;;
      end;
      InterlockedDecrement(TDiocpHttpServer(FOwner).FRequestQueueSize);
      InnerDoARequest(lvObj);
    end;                     
  finally
    Self.DecRecvRef;
    self.UnLockContext('InnerTriggerDoRequest', nil);
  end;    
end;
{$ENDIF}



procedure TDiocpHttpClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: Word);
var
  lvTmpBuf: PByte;
  lvByte:Byte;
  lvRemain: Cardinal;
  r:Integer;
  lvTempRequest: TDiocpHttpRequest;
begin
  inherited;
  {$IFDEF TRACE_HTTP_DETAIL}
  if FTraceWriter = nil then
  begin
    FTraceWriter := TSingleFileWriter.Create;
    FTraceWriter.FilePreFix := Format('HTTP_CTX_RECV_%d_', [self.SocketHandle]);
  end;
  FTraceWriter.WriteBuffer(buf, len);
  FTraceWriter.Flush;
  {$ENDIF}

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

        // ��¼��ǰcontextDNA���첽����ʱ�����
        FCurrentRequest.FContextDNA := self.ContextDNA;

//        sfLogger.logMessage(TByteTools.varToHexString(lvTmpBuf^, len));
//        sfLogger.logMessage(ByteBufferToString(lvTmpBuf, len));

      end;

      r := 0;
      try
        lvByte := lvTmpBuf^;
        r := FCurrentRequest.InputBuffer(lvByte);
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
      ///  ���������ﴦ��, responseEnd�����TBlockWriter����ɶ��̷߳���
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

        // ����Ͽ��󻹻ض���أ�����ظ�����
        FCurrentRequest := nil;

        InnerPushRequest(lvTempRequest);

        //self.RequestDisconnect('��Ч��Http����', self);
        Exit;
      end;

      if r = -2 then
      begin
        FCurrentRequest.FDecodeMsg := '����ͷ������С����';

        lvTempRequest := FCurrentRequest;

        // ����Ͽ��󻹻ض���أ�����ظ�����
        FCurrentRequest := nil;

        InnerPushRequest(lvTempRequest);


        Exit;
      end;

      if r = 0 then
      begin
        ; //��Ҫ��������ݽ���
      end else
      if r = 1 then
      begin
        if self.FContextType = Context_Type_WebSocket then
        begin
          lvTempRequest := FCurrentRequest;

          // ����Ͽ��󻹻ض���أ�����ظ�����
          FCurrentRequest := nil;

          InnerPushRequest(lvTempRequest);
        end else
        begin
          if SameText(FCurrentRequest.FInnerRequest.Method, 'POST') or
              SameText(FCurrentRequest.FInnerRequest.Method, 'PUT') then
          begin
            if FCurrentRequest.FInnerRequest.ContentLength = 0 then
            begin
              lvTempRequest := FCurrentRequest;
              // ����Ͽ��󻹻ض���أ�����ظ�����
              FCurrentRequest := nil;
              InnerPushRequest(lvTempRequest);

//              self.RequestDisconnect('��Ч��POST/PUT��������', self);
//              Exit;
            end;
          end else if SameText(FCurrentRequest.FInnerRequest.Method, '_PING') then
          begin
            // ����
            FCurrentRequest.Close;
            FCurrentRequest := nil;
          end else
          begin
            lvTempRequest := FCurrentRequest;

            // ����Ͽ��󻹻ض���أ�����ظ�����
            FCurrentRequest := nil;

            InnerPushRequest(lvTempRequest);
          end;
        end;
      end else
      if r = 2 then
      begin     // ���뵽������
        lvTempRequest := FCurrentRequest;

        // ����Ͽ��󻹻ض���أ�����ظ�����
        FCurrentRequest := nil;

        // �����¼�
        InnerPushRequest(lvTempRequest);
      end;   

      Dec(lvRemain);
      Inc(lvTmpBuf);
    end;
  finally
    // ������ɴ����߼�����
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
    raise Exception.Create('��WebSocket����');
  end;
  
  lvWSFrame := TDiocpWebSocketFrame.Create;
  try
    lvWSFrame.EncodeBuffer(pvBuffer, len, true, opcode, false);
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
    // ����������
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
      raise Exception.Create('����δ�����������');
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
      raise Exception.Create('�������ظ��趨ContextType');
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

  // 4K, ÿ��Ͷ��4k
  FBlockBufferPool := newBufferPool(SEND_BLOCK_SIZE);
end;

destructor TDiocpHttpServer.Destroy;
begin
  FRequestPool.FreeDataObject;
  FRequestPool.Free;

  /// ֻ��Ҫ��������ʱ��黹��Session�����
  FSessionList.Clear;
  FSessionList.Free;

  FSessionObjectPool.WaitFor(10000);
  FSessionObjectPool.Free;
  FreeBufferPool(FBlockBufferPool);
  inherited Destroy;
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

  /// ֻ��Ҫ��������ʱ��黹��Session�����
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
      sfLogger.logMessage('[#] ������IO�����̴߳���Http����');
    {$ELSE}
      {$IFDEF DIOCP_Task}
        sfLogger.logMessage('[#] ��DIOCP-Task�̳߳ش���Http����');
      {$ENDIF}

      {$IFDEF QDAC_QWorker}
        sfLogger.logMessage('[#] ��QDAC-QWorkers����Http����');
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
    lvContext.FRequestingFlag := 1;
    lvContext.BeginBusy;
    SetCurrentThreadInfo('����Http::DoRequest');
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
            lvContext.RequestDisconnect('�յ�WebSocket-Close����');
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
          self.LogMessage('Http�߼������쳣:%s', [e.Message], '', lgvError);
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
        self.LogMessage('*Http�߼������쳣:%s', [e.Message], CORE_LOG_FILE, lgvError);
        //pvRequest.Connection.SetRecvWorkerHint('DoRequest::*Exception - 2');
      end;
    end;
  finally
    lvContext.FRequestingFlag := 0;
    lvContext.EndBusy;             
    lvContext.CheckThreadOut;
    SetCurrentThreadInfo('����Http::DoRequest');
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
    Result.FReleaseLater := false;
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
      'TDiocpHttpServer.GiveBackRequest::�����ظ��ر�:' + s);
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
  if FSessionClass = nil then raise Exception.Create('��δע��SessionClass, ���ܻ�ȡSession');
  Result := FSessionClass.Create();
end;

procedure TDiocpHttpServer.OnSessionRemove(pvData: Pointer);
begin
  try
    // ����Session
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
      // �ᴥ��OnSessionRemove, �黹��Ӧ�Ķ��󵽳�
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

procedure TDiocpHttpServer.WebSocketSendBuffer(pvBuf: Pointer; pvBufLen:
    Integer; OPTCode: Byte);
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
       if lvContext.LockContext('SendWsBuffer', lvContext) then
       try
         if lvContext.ContextType = Context_Type_WebSocket then
         begin
           lvContext.PostWebSocketSendBuffer(pvBuf, pvBufLen, OPTCode);
         end;
       finally
         lvContext.UnLockContext('SendWsBuffer', lvContext);
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
