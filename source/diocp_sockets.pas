(*
 *	 Unit owner: d10.�����
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 ����
 *
 *   2019-05-24 16:45:55
 *     �Ż��ر��¼����ر��¼������첽�ر�
 *)
 
unit diocp_sockets;

{$I 'diocp.inc'}

interface


{$IFDEF DIOCP_HIGH_SPEED}
 {$UNDEF WRITE_LOG}
 {$UNDEF DEBUG_ON}
 {$UNDEF DIOCP_DEBUG}
 {$UNDEF TRACE_IOCP_RECV}
 {$UNDEF TRACE_IOCP_SEND}
{$ENDIF}




uses
  Classes, diocp_sockets_utils, diocp_core_engine,
  winsock, diocp_winapi_winsock2,
{$if CompilerVersion >= 18}
  types,
{$ifend}
  diocp_core_rawWinSocket, SyncObjs, Windows, SysUtils,
  utils_safeLogger,
  utils_hashs,
  {$IFDEF TRACE_IOCP_SEND}
  utils_byteTools,
  {$ENDIF}
  {$IFDEF DIOCP_DEBUG}
  utils_threadinfo,
  {$ENDIF}
  diocp_res,
  utils_queues, utils_locker, utils_async, utils_fileWriter, utils_strings, utils_BufferPool;

const
  CORE_LOG_FILE = 'diocp_core_exception';

  OPERA_SHUTDOWN_CONNECT = $0002;


type
  TDiocpCustom = class;
  TIocpAcceptorMgr = class;
  TDiocpCustomContext = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;


  TIocpSendRequestClass = class of TIocpSendRequest;
  TIocpContextClass = class of TDiocpCustomContext;

  TOnContextError = procedure(pvContext: TDiocpCustomContext; pvErrorCode: Integer; E:Exception)
      of object;

  TOnContextBufferEvent = procedure(pvContext: TDiocpCustomContext; pvBuff: Pointer; len:
      Cardinal; pvBufferTag:Integer; pvBufferTagData: Pointer; pvErrorCode: Integer) of object;

  TOnBufferReceived = procedure(pvContext: TDiocpCustomContext; buf: Pointer; len:
      cardinal; pvErrorCode: Integer) of object;

  TOnContextBufferNotifyEvent = procedure(pvContext: TDiocpCustomContext; buf: Pointer; len: cardinal) of object;
  
  TNotifyContextEvent = procedure(pvContext: TDiocpCustomContext) of object;

  /// <summary>
  ///   on post request is completed
  /// </summary>
  TOnDataRequestCompleted = procedure(pvClientContext:TDiocpCustomContext;
      pvRequest:TIocpRequest) of object;

      
  TDataReleaseType = (dtNone, dtFreeMem, dtDispose);

  TContextArray = array of TDiocpCustomContext;

  TIocpActionRequest = class(TIocpRequest)
  private
    FAction:Integer;
    FDataPtr:Pointer;
  public
    procedure HandleResponse; override;
  end;

  /// <summary>
  ///   client object
  /// </summary>
  TDiocpCustomContext = class(TObject)

  private
    
    FReleaseBack:TNotifyContextEvent;
    FCreateSN:Integer;

    // ���Ͷ���
    FSendQueueSize:Integer;

    // ���ʹ�óأ��رպ󽫻�ع鵽����
    FOwnePool:TSafeQueue;
    
    // ��󽻻����ݵ�ʱ���
    FLastActivity: Cardinal;

    FContextDNA : Integer;

    FSocketHandle : TSocket;
    FContextLocker: TIocpLocker;
    FLastErrorCode:Integer;
    FDebugInfo: string;

    FSendBytesSize:Int64;
    FRecvBytesSize:Int64;
    FDisconnectedCounter:Integer;
    FKickCounter:Integer;
    {$IFDEF DIOCP_DEBUG}
    procedure SetDebugInfo(const Value: string);
    {$ENDIF}
  private

    FDebugStrings:TStrings;

  private
    // 0: ��ʼ״̬, ����״̬ 1����������, 2:���ӳɹ�, 11:�ȴ��ر�, 12:�ر���
    FCtxStateFlag:Integer;
    FCtxStateLocker:Integer;
    {$IFDEF DIOCP_DEBUG}
    FCtxStateDebugStr:String;
    {$ENDIF}
    procedure DoCtxStateLock({$IFDEF DIOCP_DEBUG} const pvDebugStr:string{$ENDIF});
    procedure DoCtxStateUnLock;

    // �Ƿ�ִ�йر�
    function CheckCloseContext: Boolean;
  private
    // �ر��¼�����
    FCloseRequest:TIocpActionRequest;
    FReqDisFlag:Integer;
    procedure PostCloseRequest;
    procedure OnCloseRequestResponse(Sender: TObject);
  private
    {$IFDEF DIOCP_DEBUG}
    FCheckThreadInDebugInfo:String;
    FDebugLocker:Integer;
    {$ENDIF}
    FCheckStartFlag:Byte;
    FOnRecvingFlag:Byte;
    FCloseingFlag:Byte;
    FCloseSocketFlag:Byte;
    FPostWsRecvingCnt:Integer;


    FCheckThreadId:THandle;

    FObjectAlive: Boolean;

    // link
    FPre:TDiocpCustomContext;
    FNext:TDiocpCustomContext;

    /// <summary>
    ///   sending flag
    /// </summary>
    FSending: Boolean;

    FActive: Boolean;
    FConnectedCounter: Integer;

    FOwner: TDiocpCustom;

    FCurrRecvRequest:TIocpRecvRequest;

    //FcurrSendRequest:TIocpSendRequest;

    FData: Pointer;
    FDirectPostRequest: Boolean;

    /// <summary>
    ///   ����ԭ��
    /// </summary>
    FDisconnectedReason: String;

    FOnConnectedEvent: TNotifyContextEvent;
    FOnDisconnectedEvent: TNotifyContextEvent;
    FOnRecvBufferEvent: TOnBufferReceived;
    FOnSendBufferCompleted: TOnContextBufferEvent;
    FOnSocketStateChanged: TNotifyEvent;

    /// sendRequest link
    FSendRequestLink: TIocpRequestSingleLink;

    FRawSocket: TRawSocket;
    /// <summary>
    ///   ReferenceCounter counter
    /// </summary>
    FReferenceCounter: Integer;

    FRemoteAddr: String;

    FRemotePort: Integer;
    /// <summary>
    ///   request discnnect flag, ReferenceCounter is zero then do disconnect
    /// </summary>
    FRequestDisconnect: Boolean;
    FSocketState: TSocketState;

    /// <summary>
    ///   called by recvRequest response
    /// </summary>
    procedure DoReceiveData(pvRecvRequest: TIocpRecvRequest);


    procedure InnerPostSendRequest(lvRequest:TIocpSendRequest);


    /// <summary>
    ///   post next sendRequest
    /// </summary>
    function CheckNextSendRequest: Boolean;

    /// <example>
    ///   �ͷŴ����Ͷ����еķ�������(TSendRequest)
    /// </example>
    procedure CheckReleaseRes;

    {$IFDEF DIOCP_DEBUG}
    function GetDebugInfo: string;
    procedure InnerAddToDebugStrings(pvMsg:String); overload;
    procedure InnerAddToDebugStrings(const pvMsg: string; const args: array of
        const); overload;
    {$ENDIF}

    /// <summary>
    ///   ��Ҫ��֤ һ������ֻ����һ���߳�ִ��
    /// </summary>
    procedure InnerCloseContext(pvDoShutDown: Boolean = True);


    procedure SetOwner(const Value: TDiocpCustom);



    procedure ReleaseBack;
  protected
    /// <summary>
    ///   request recv data
    /// </summary>
    procedure PostWSARecvRequest();

    /// <summary>
    ///   called by sendRequest response
    /// </summary>
    procedure DoSendRequestCompleted(pvRequest: TIocpSendRequest);virtual;

    /// <summary>
    ///   post reqeust to sending queue,
    ///    fail, push back to pool
    ///  ׼������
    /// </summary>
    function PostSendRequestDelete(pvSendRequest:TIocpSendRequest): Boolean;


    /// <summary>
    ///   ��ȡһ��TSendRequest����
    ///   ����Owner���أ�һ��Ӷ�����л�ȡ
    /// </summary>
    function GetSendRequest: TIocpSendRequest;

    procedure DoSendBufferCompleted(pvBuffer: Pointer; len: Cardinal; pvBufferTag,
        pvErrorCode: Integer); virtual;

    procedure DoError(pvErrorCode:Integer);

    procedure DoNotifyDisconnected;

    procedure CheckReleaseBack;

  protected
    /// <summary>
    ///    dec RequestCounter then check counter and Request flag for Disonnect
    /// </summary>
    function DecReferenceCounter(pvDebugInfo: string; pvObj: TObject): Integer;

    // ��ʼ����
    function IncReferenceCounter(pvDebugInfo: string; pvObj: TObject): Boolean;

    /// <summary>
    ///   ֻ��������, connectEx��ʱ����
    /// </summary>
    procedure AddRefernece;

    /// <summary>
    ///   ֻ���м���, connectEx��ʱ����
    /// </summary>
    procedure DecRefernece;

    procedure DoCleanUp;virtual;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); virtual;

    procedure OnDisconnected; virtual;

    procedure OnConnected; virtual;

    procedure SetSocketState(pvState:TSocketState); virtual;

    /// <summary>
    ///   call in response event
    /// </summary>
    procedure DoConnected;
    /// <summary>
    ///   Ͷ�ݵķ���������Ӧʱִ�У�һ��Ӧ������ִ�У�Errcode <> 0Ҳ����Ӧ
    /// </summary>
    procedure DoSendRequestRespnonse(pvRequest: TIocpSendRequest); virtual;
    procedure DoSetCtxState(const state:Integer);


    /// <summary>
    ///  1.post reqeust to sending queue,
    ///    return false if SendQueue Size is greater than maxSize,
    ///
    ///  2.check sending flag, start if sending is false
    /// </summary>
    function InnerPostSendRequestAndCheckStart(pvSendRequest:TIocpSendRequest):
        Boolean;

    procedure lock();

    procedure UnLock;

    procedure PostNextSendRequest; virtual;
  public
    /// <summary>
    ///   lock context avoid disconnect,
    ///     lock succ return false else return false( context request disconnect)
    /// </summary>
    function LockContext(pvDebugInfo: string; pvObj: TObject): Boolean;
    procedure UnLockContext(pvDebugInfo: string; pvObj: TObject);

    {$IFDEF DIOCP_DEBUG}
    procedure AddDebugStrings(const pvDebugInfo: String; pvAddTimePre: Boolean =
        true);
    {$ENDIF}

  public

    /// <summary>
    ///   �ر����ӣ������ٽ������ݵķ���, �����Ͷ����е����ݻ����ȡ��
    /// </summary>
    procedure Close(pvDoShutDown: Boolean = True);

    constructor Create; virtual;

    destructor Destroy; override;

    function CheckActivityTimeOut(pvTimeOut:Integer): Boolean;

    procedure CheckKickOut(pvTimeOut:Integer);






    
    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean =
        true; pvTag: Integer = 0; pvTagData: Pointer = nil): Boolean; overload;
    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvBufReleaseType:
        TDataReleaseType; pvTag: Integer = 0; pvTagData: Pointer = nil): Boolean;
        overload;

    procedure RequestDisconnect(pvReason: string = STRING_EMPTY; pvObj: TObject =
        nil; pvDoShutDown: Boolean = True);


    procedure SetMaxSendingQueueSize(pvSize:Integer);

    /// <summary>
    ///  �Ƿ��Ѿ�����
    /// </summary>
    property Active: Boolean read FActive;

    property Data: Pointer read FData write FData;



    function CheckActivityTimeOutEx(pvTimeOut:Integer): Boolean;

    {$IFDEF DIOCP_DEBUG}
    property DebugInfo: string read GetDebugInfo write SetDebugInfo;
    procedure CheckThreadIn(const pvDebugInfo: String);
    procedure CheckThreadOut;
    function GetDebugStrings: String;
    {$ENDIF}

    property OnConnectedEvent: TNotifyContextEvent read FOnConnectedEvent write
        FOnConnectedEvent;

    property OnDisconnectedEvent: TNotifyContextEvent read FOnDisconnectedEvent
        write FOnDisconnectedEvent;

    property OnRecvBufferEvent: TOnBufferReceived read FOnRecvBufferEvent write
        FOnRecvBufferEvent;

    /// <summary>
    ///   ���͵�Buffer�Ѿ����
    /// </summary>
    property OnSendBufferCompleted: TOnContextBufferEvent read
        FOnSendBufferCompleted write FOnSendBufferCompleted;

    /// <summary>
    ///   ���ӳɹ�����
    /// </summary>
    property ConnectedCounter: Integer read FConnectedCounter;
    property ContextDNA: Integer read FContextDNA;
    property CreateSN: Integer read FCreateSN;
    property CtxStateFlag: Integer read FCtxStateFlag;
    property CurrRecvRequest: TIocpRecvRequest read FCurrRecvRequest;
    property DirectPostRequest: Boolean read FDirectPostRequest write
        FDirectPostRequest;
    property DisconnectedCounter: Integer read FDisconnectedCounter;
    property DisconnectedReason: String read FDisconnectedReason;



    property KickCounter: Integer read FKickCounter;
    property LastActivity: Cardinal read FLastActivity;
    property Owner: TDiocpCustom read FOwner write SetOwner;

    property RawSocket: TRawSocket read FRawSocket;
    property RecvBytesSize: Int64 read FRecvBytesSize;
    property SendBytesSize: Int64 read FSendBytesSize;
    property SendQueueSize: Integer read FSendQueueSize;

    property SocketState: TSocketState read FSocketState;

    property SocketHandle: TSocket read FSocketHandle;

    /// <summary>
    ///   Socket״̬�ı䴥�����¼�
    /// </summary>
    property OnSocketStateChanged: TNotifyEvent read FOnSocketStateChanged write
        FOnSocketStateChanged;

  end;



  /// <summary>
  ///   WSARecv io request
  /// </summary>
  TIocpRecvRequest = class(TIocpRequest)
  private
    FAlive:Boolean;
    FInnerBuffer: diocp_winapi_winsock2.TWsaBuf;
    FRecvBuffer: diocp_winapi_winsock2.TWsaBuf;
    FRecvdFlag: Cardinal;
    FOwner: TDiocpCustom;
    FContext: TDiocpCustomContext;
    FDebugInfo:String;
    /// <summary>
    ///   �黹�ͷ�
    /// </summary>
    procedure ReleaseBack;
  protected

    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;

    /// <summary>
    ///   post recv request to iocp queue
    /// </summary>
    function PostRequest: Boolean; overload;

    /// <summary>
    ///
    /// </summary>
    function PostRequest(pvBuffer:PAnsiChar; len:Cardinal): Boolean; overload;
    procedure ResponseDone; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckCreateRecvBuffer;
  end;



  /// <summary>
  ///   WSASend io request
  /// </summary>
  TIocpSendRequest = class(TIocpRequest)
  private
    FSendBufferReleaseType: TDataReleaseType;
    
    // for singlelinked
    FNext:TIocpSendRequest;

    FIsBusying:Boolean;

    FAlive: Boolean;

    FBytesSize:Cardinal;

    // send buf record
    FWSABuf:TWsaBuf;


    FBuf:Pointer;
    FLen:Cardinal;

    FOwner: TDiocpCustom;

    FContext: TDiocpCustomContext;
    
    FDirectPost: Boolean;


    FOnDataRequestCompleted: TOnDataRequestCompleted;
    procedure CheckClearSendBuffer;
  protected
    /// <summary>
    ///   post send a block
    /// </summary>
    function ExecuteSend: Boolean; virtual;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;

    procedure ResponseDone; override;

    procedure DoCleanUp;virtual;
    
    function GetStateINfo: String; override;

    /// <summary>
    ///   post send buffer to iocp queue
    /// </summary>
    function InnerPostRequest(buf: Pointer; len: Cardinal): Boolean;

    procedure UnBindingSendBuffer;

  public
    constructor Create; virtual;

    destructor Destroy; override;

    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure setBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = true);overload;
    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvBufReleaseType:
        TDataReleaseType); overload;

    /// ֱ��Ͷ��û�о�������
    property DirectPost: Boolean read FDirectPost write FDirectPost;
    property Owner: TDiocpCustom read FOwner;
    /// <summary>
    ///   on entire buf send completed
    /// </summary>
    property OnDataRequestCompleted: TOnDataRequestCompleted read
        FOnDataRequestCompleted write FOnDataRequestCompleted;
  end;


  /// <summary>
  ///   acceptEx request
  /// </summary>
  TIocpAcceptExRequest = class(TIocpRequest)
  private
    /// <summary>
    ///   acceptEx lpOutBuffer[in]
    ///     A pointer to a buffer that receives the first block of data sent on a new connection,
    ///       the local address of the server, and the remote address of the client.
    ///       The receive data is written to the first part of the buffer starting at offset zero,
    ///       while the addresses are written to the latter part of the buffer.
    ///       This parameter must be specified.
    /// </summary>
    FAcceptBuffer: array [0.. (SizeOf(TSockAddrIn) + 16) * 2 - 1] of byte;



    FOwner: TDiocpCustom;

    FAcceptorMgr:TIocpAcceptorMgr;

    FContext: TDiocpCustomContext;
    FOnAcceptedEx: TNotifyEvent;
    /// <summary>
    ///   get socket peer info on acceptEx reponse
    /// </summary>
    procedure getPeerINfo;
  protected
    procedure HandleResponse; override;
    function PostRequest: Boolean;

  protected
  public
    constructor Create(AOwner: TDiocpCustom);
    property OnAcceptedEx: TNotifyEvent read FOnAcceptedEx write FOnAcceptedEx;
  end;

  /// <summary>
  ///   manager acceptEx request
  /// </summary>
  TIocpAcceptorMgr = class(TObject)
  private
    FListenSocket:TRawSocket;
    FOwner: TDiocpCustom;
    FList:TList;
    FLocker: TIocpLocker;
    FMaxRequest:Integer;
    FMinRequest:Integer;

  protected
  public
    constructor Create(AOwner: TDiocpCustom);
    destructor Destroy; override;

    procedure releaseRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure removeRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure checkPostRequest(pvContext: TDiocpCustomContext);
  end;


  /// <summary>
  ///   connectEx io request
  /// </summary>
  TIocpConnectExRequest = class(TIocpRequest)
  private
    FBytesSent: DWORD;
  protected
    FContext: TDiocpCustomContext;
  public
    /// <summary>
    ///   post connectEx request to iocp queue
    /// </summary>                                      l
    function PostRequest(pvHost: string; pvPort: Integer): Boolean;
  public
    constructor Create(AContext: TDiocpCustomContext);
    destructor Destroy; override;
  end;


  /// <summary>
  ///   iocp data monitor
  /// </summary>
  TIocpDataMonitor = class(TObject)
  private
    FDisconnectCounter: Integer;
    FSentSize:Int64;
    FRecvSize:Int64;
    FPostWSASendSize: Int64;

    FPushSendQueueCounter: Integer;
    FResponseSendObjectCounter:Integer;

    FPostWSASendCounter:Integer;
    FResponseWSASendCounter:Integer;

    FPostWSARecvCounter:Integer;
    FResponseWSARecvCounter:Integer;

    FPostWSAAcceptExCounter:Integer;
    FResponseWSAAcceptExCounter:Integer;

    FLocker: TCriticalSection;
    FPostSendObjectCounter: Integer;
    FSendRequestAbortCounter: Integer;
    FSendRequestCreateCounter: Integer;
    FSendRequestOutCounter: Integer;
    FSendRequestReturnCounter: Integer;

    // ��¼��ʼʱ���
    FLastSpeedTick : Cardinal;

    // �����������
    FMaxOnlineCount:Integer;

    // ��¼��ʼʱ���_����
    FLastSpeed_WSASendResponse: Int64;
    FLastSpeed_WSARecvResponse: Int64;
    FLastSpeed_RecvSize:Int64;

    FContextCreateCounter:Integer;
    FContextOutCounter:Integer;
    FContextReturnCounter:Integer;

    FRecvRequestCreateCounter: Integer;
    FRecvRequestOutCounter: Integer;
    FRecvRequestReturnCounter: Integer;

    FSpeed_WSASendResponse: Int64;
    FSpeed_WSARecvResponse: Int64;
    FSpeed_Recv:Int64;

    procedure incSentSize(pvSize:Cardinal);
    procedure incPostWSASendSize(pvSize:Cardinal);
    procedure incRecvdSize(pvSize:Cardinal);

    procedure incPostWSASendCounter();
    procedure incResponseWSASendCounter;

    procedure incPostWSARecvCounter();
    procedure incResponseWSARecvCounter;

    procedure incPushSendQueueCounter;
    procedure incPostSendObjectCounter();
    procedure IncSendRequestCreateCounter;
    procedure IncSendRequestOutCounter;
    procedure IncSendRequestReturnCounter;

    procedure incResponseSendObjectCounter();
    procedure IncDisconnectCounter;
    procedure IncRecvRequestCreateCounter;
    procedure IncRecvRequestOutCounter;
    procedure IncRecvRequestReturnCounter;


   procedure IncContextCreateCounter;
   procedure IncContextOutCounter;
   procedure IncContextReturnCounter;
  public
    constructor Create;
    destructor Destroy; override;
    
    /// <summary>
    ///   ���������������
    /// </summary>
    procedure CalcuMaxOnlineCount(pvOnlineCount:Integer);

    procedure Clear;
    /// <summary>
    ///   ͳ�����ݣ�����ʱ����Ϣ
    /// </summary>
    procedure SpeedCalcuEnd;
    /// <summary>
    ///  ��ʼͳ���ٶ�
    ///  ��¼��ǰ��Ϣ
    /// </summary>
    procedure SpeedCalcuStart;

    property ContextCreateCounter: Integer read FContextCreateCounter;
    property ContextOutCounter: Integer read FContextOutCounter;
    property ContextReturnCounter: Integer read FContextReturnCounter;
    /// <summary>
    ///   ���ߴ���
    /// </summary>
    property DisconnectCounter: Integer read FDisconnectCounter;

    property MaxOnlineCount: Integer read FMaxOnlineCount;
    property PushSendQueueCounter: Integer read FPushSendQueueCounter;
    property PostSendObjectCounter: Integer read FPostSendObjectCounter;
    property ResponseSendObjectCounter: Integer read FResponseSendObjectCounter;

    property PostWSAAcceptExCounter: Integer read FPostWSAAcceptExCounter;
    property PostWSARecvCounter: Integer read FPostWSARecvCounter;
    property PostWSASendCounter: Integer read FPostWSASendCounter;


    property PostWSASendSize: Int64 read FPostWSASendSize;
    property RecvRequestCreateCounter: Integer read FRecvRequestCreateCounter;
    property RecvRequestOutCounter: Integer read FRecvRequestOutCounter;
    property RecvRequestReturnCounter: Integer read FRecvRequestReturnCounter;
    property RecvSize: Int64 read FRecvSize;

    property ResponseWSAAcceptExCounter: Integer read FResponseWSAAcceptExCounter;
    property ResponseWSARecvCounter: Integer read FResponseWSARecvCounter;
    property ResponseWSASendCounter: Integer read FResponseWSASendCounter;
    property SendRequestAbortCounter: Integer read FSendRequestAbortCounter;
    property SendRequestCreateCounter: Integer read FSendRequestCreateCounter;
    property SendRequestOutCounter: Integer read FSendRequestOutCounter;
    property SendRequestReturnCounter: Integer read FSendRequestReturnCounter;
    property SentSize: Int64 read FSentSize;
    property Speed_Recv: Int64 read FSpeed_Recv;
    property Speed_WSARecvResponse: Int64 read FSpeed_WSARecvResponse;
    property Speed_WSASendResponse: Int64 read FSpeed_WSASendResponse;

  end;


  TDiocpCustom = class(TComponent)
  private
    /// <summary>
    ///   ���ü���
    /// </summary>
    FRefCounter:Integer;

    FOperaOptions:Integer;

    FContextPool: TSafeQueue;

    FASyncInvoker:TASyncInvoker;

    FDebugStrings:TStrings;
    
  {$IFDEF DIOCP_DEBUG}
    FDebug_SendRequestCounter:Integer;
  {$ENDIF}

    FIsDestroying :Boolean;
    FWSARecvBufferSize: Cardinal;
    procedure SetWSARecvBufferSize(const Value: Cardinal);

    function IsDestroying: Boolean;
    function LogCanWrite: Boolean;

  protected
    /// 1: ��ֹ���� (��������ӳɹ��ģ�������Ͽ�)
    FDisableConnectFlag: Integer;
    
    FContextClass:TIocpContextClass;

    FIocpSendRequestClass:TIocpSendRequestClass;

    FLocker:TIocpLocker;

    /// <summary>
    ///   ά���������б�
    /// </summary>
    FOnlineContextList : TDHashTable;
  private
    // sendRequest pool
    FSendRequestPool: TBaseQueue;

    FRecvRequestPool: TBaseQueue;

    /// data record
    FDataMoniter: TIocpDataMonitor;

    FActive: Boolean;


    FOnContextConnected: TNotifyContextEvent;
    FOnContextDisconnected: TNotifyContextEvent;

    FOnReceivedBuffer: TOnBufferReceived;


    FOnContextError: TOnContextError;



    FWSASendBufferSize: Cardinal;

    procedure DoClientContextError(pvClientContext: TDiocpCustomContext;
        pvErrorCode: Integer);
    function GetWorkerCount: Integer;

    procedure SetWorkerCount(const Value: Integer);

    procedure SetActive(pvActive:Boolean);

    procedure DoReceiveData(pvIocpContext: TDiocpCustomContext; pvRequest:
        TIocpRecvRequest);
  private
    FOwnerEngine:Boolean;
    FIocpEngine: TIocpEngine;
    procedure CheckDoDestroyEngine;
  protected

    /// <summary>
    ///   pop sendRequest object
    /// </summary>
    function GetSendRequest: TIocpSendRequest;

    function InnerCreateSendRequest: TIocpSendRequest;

    function InnerCreateRecvRequest: TIocpRecvRequest;

    /// <summary>
    ///   push back to pool
    /// </summary>
    function ReleaseSendRequest(pvObject:TIocpSendRequest): Boolean;

  protected

    /// <summary>
    ///   ����һ������ʵ��
    ///   ͨ��ע���ContextClass���д���ʵ��     
    /// </summary>
    function CreateContext: TDiocpCustomContext;

    /// <summary>
    ///   occur on create instance
    /// </summary>
    procedure OnCreateContext(const pvContext: TDiocpCustomContext); virtual;

    /// <summary>
    ///   ������������֣���������������־��¼����ǰ׺��
    /// </summary>
    procedure SetName(const NewName: TComponentName); override;
  private
    FContextDNA : Integer;
    FDefaultMsgType: String;
    FOnSendBufferCompleted: TOnContextBufferEvent;
    FUseObjectPool: Boolean;

    procedure DoSendBufferCompletedEvent(pvContext: TDiocpCustomContext; pvBuff:
        Pointer; len: Cardinal; pvBufferTag:Integer; pvBufferTagData:Pointer;
        pvErrorCode: Integer);
        

    procedure OnIocpException(pvRequest:TIocpRequest; E:Exception);

    function RequestContextDNA: Integer;

    procedure SetWSASendBufferSize(const Value: Cardinal);

  private

    FNoDelayOption: Boolean;
    /// <summary>
    ///   ��ȡ��ǰ��������
    /// </summary>
    function GetOnlineContextCount: Integer;

    /// <summary>
    ///   ��ӵ������б���
    /// </summary>
    procedure AddToOnlineList(pvObject: TDiocpCustomContext);


    /// <summary>
    ///   �������б����Ƴ�
    /// </summary>
    procedure RemoveFromOnOnlineList(pvObject: TDiocpCustomContext); virtual;

    procedure InnerAddToDebugStrings(const pvMsg: String);
    procedure OnASyncWork(pvASyncWorker:TASyncWorker);

  protected
    procedure DoASyncWork(pvFileWritter: TSingleFileWriter; pvASyncWorker:
        TASyncWorker); virtual;
    procedure DoIdle();virtual;
  protected
    procedure DoAfterOpen;virtual;
    procedure DoAfterClose;virtual;
    /// <summary>
    ///   ��ȡһ����������
    /// </summary>
    function GetRecvRequest: TIocpRecvRequest;

    
    function ReleaseRecvRequest(pvObject: TIocpRecvRequest): Boolean;

  public
    /// <summary>
    ///   �ӳ��л�ȡһ�����Ӷ���
    ///   �ӳ��л�ȡ�����Ӳ��ܽ�������(�ر�ʱ�Զ������˳�)
    /// </summary>
    function GetContextFromPool: TDiocpCustomContext;

    procedure ReleaseContext(pvContext:TDiocpCustomContext);

    /// <summary>
    ///   ԭ�Ӳ���������ü���(һ��Ҫ�ж�Ӧ��DecRefCounter);
    ///   Ŀ��: ��ֹ�ͷ�TDiocpCustom
    /// </summary>
    function IncRefCounter: Integer;

    /// <summary>
    ///   ��������
    /// </summary>
    function DecRefCounter:Integer;

    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    procedure RegisterContextClass(pvContextClass: TIocpContextClass);

    /// <summary>
    ///   �������ݼ�ض���,���ڼ�¼�ڲ����ݵ��շ�����
    /// </summary>
    procedure CreateDataMonitor;


    /// <summary>
    ///   check clientContext object is valid.
    /// </summary>
    function CheckClientContextValid(const pvClientContext: TDiocpCustomContext):
        Boolean;

    /// <summary>
    ///   ֹͣIOCP�̣߳��ȴ������߳��ͷ�
    /// </summary>
    procedure Close;

    property Active: Boolean read FActive write SetActive;

    /// <summary>
    ///   ����Ͽ��������ӣ������̷��ء�
    /// </summary>
    procedure DisconnectAll;  virtual;


    
    procedure Open;

    /// <summary>
    ///   ��ȡ�����б�
    /// </summary>
    procedure GetOnlineContextList(pvList:TList);

    /// <summary>
    ///   �������б��в���
    /// </summary>
    function FindContext(pvSocketHandle: THandle): TDiocpCustomContext;

    /// <summary>
    ///   wait for all conntext is off
    /// </summary>
    function WaitForContext(pvTimeOut: Cardinal): Boolean;

    property ContextPool: TSafeQueue read FContextPool;
    /// <summary>
    ///   client connections counter
    /// </summary>
    property OnlineContextCount: Integer read GetOnlineContextCount;

    property DataMoniter: TIocpDataMonitor read FDataMoniter;

    property IocpEngine: TIocpEngine read FIocpEngine;

    /// <summary>
    ///   ��ʱ���, �������Timeoutָ����ʱ�仹û���κ����ݽ������ݼ�¼��
    ///     �ͽ��йر�����
    ///   ʹ��ѭ����⣬������кõķ�������ӭ�ύ���ı������
    /// </summary>
    function KickOut(pvTimeOut:Cardinal = 60000): Integer;

    function GetTimeOutContexts(var vOutList: TContextArray; pvTimeOut: Cardinal =
        60000): Integer;

    /// <summary>
    ///   ���pvContext�Ƿ�����KickOut����
    ///   0: û�ҵ���Ӧ������
    ///   1: �Ѿ���ʱ
    ///   2: δ��ʱ
    /// </summary>
    function CheckTimeOutContext(pvContext:TDiocpCustomContext; pvTimeOut:Cardinal
        = 60000): Integer;

    procedure LogMessage(pvMsg: string; pvMsgType: string = ''; pvLevel: TLogLevel
        = lgvMessage); overload;
        
    procedure logMessage(pvMsg: string; const args: array of const; pvMsgType:
        string = ''; pvLevel: TLogLevel = lgvMessage); overload;
    /// <summary>
    ///   ���͵�Buffer�Ѿ����
    /// </summary>
    property OnSendBufferCompleted: TOnContextBufferEvent read
        FOnSendBufferCompleted write FOnSendBufferCompleted;


    
    procedure AddDebugStrings(pvDebugInfo: String; pvAddTimePre: Boolean = true);
    function GetDebugString: String;


    property DisableConnectFlag: Integer read FDisableConnectFlag;
    /// <summary>
    ///   NoDelay����(Ĭ����false)
    ///   ����Ϊtrue�ǻ����Socket��NoDelay����(����nagle�㷨)
    ///   nagle�㷨�����200ms���ҵ���ʱ
    ///   ����ʱ����ʱ�ı�ĸ����ԣ��������������Ѿ�����������(���Ժ�����������Ч)
    ///
    ///   Ϊ�˼�������ӵ������Ƶģ�����ȴ��㹻�����ݲŷ��ͳ�ȥ�����û���㹻�����ݣ��ͻ�ȴ�Լ200ms���ڲ���ʱ����ʱ���������ͳ�ȥ
    /// </summary>
    property NoDelayOption: Boolean read FNoDelayOption write FNoDelayOption;

  public
    /// <summary>
    ///   ��һ��Iocp����
    /// </summary>
    /// <param name="pvEngine"> (TIocpEngine) </param>
    /// <param name="pvOwner">
    ///   �Ƿ�ӵ���������,
    ///   true: �ͷ�ʱ��������һ���ͷ�
    /// </param>
    procedure BindDiocpEngine(const pvEngine: TIocpEngine; pvOwner: Boolean = true);
    procedure CheckCreatePoolObjects(pvMaxConnection: Integer);
    procedure IncOperaOptions(const pvFlag:Integer);
    procedure DecOperaOptions(const pvFlag:Integer);
    function CheckOperaFlag(const pvFlag:Integer): Boolean;

  published

    /// <summary>
    ///   on disconnected
    /// </summary>
    property OnContextDisconnected: TNotifyContextEvent read FOnContextDisconnected
        write FOnContextDisconnected;

    /// <summary>
    ///   on connected
    /// </summary>
    property OnContextConnected: TNotifyContextEvent read FOnContextConnected write
        FOnContextConnected;

    /// <summary>
    ///   Ĭ����־�ļ�ǰ׺
    /// </summary>
    property DefaultMsgType: String read FDefaultMsgType write FDefaultMsgType;
    /// <summary>
    ///   �Ƿ�ʹ�ö����
    /// </summary>
    property UseObjectPool: Boolean read FUseObjectPool write FUseObjectPool;

    /// <summary>
    ///   default cpu count * 2 -1
    /// </summary>
    property WorkerCount: Integer read GetWorkerCount write SetWorkerCount;


    /// <summary>
    ///   post wsaRecv request block size
    /// </summary>
    property WSARecvBufferSize: Cardinal read FWSARecvBufferSize write
        SetWSARecvBufferSize;


    /// <summary>
    ///   max size for post WSASend
    /// </summary>
    property WSASendBufferSize: Cardinal read FWSASendBufferSize write
        SetWSASendBufferSize;




    /// <summary>
    ///  on work error
    ///    occur in post request methods or iocp worker thread
    /// </summary>
    property OnContextError: TOnContextError read FOnContextError write
        FOnContextError;



    /// <summary>
    ///  on clientcontext received data
    ///    called by iocp worker thread
    /// </summary>
    property OnReceivedBuffer: TOnBufferReceived read FOnReceivedBuffer write
        FOnReceivedBuffer;

  end;

/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean):
    Boolean;

var
  __diocp_logger:TSafeLogger;

/// <summary>
///   ע�����ʹ�õ�SafeLogger
/// </summary>
procedure RegisterDiocpLogger(pvLogger:TSafeLogger);

implementation


resourcestring
  strRecvZero      = '[%d]���յ�0�ֽڵ�����,�����ӽ��Ͽ�!';
  strRecvError     = '[%d]��Ӧ��������ʱ�����˴��󡣴������:%d!';
  strRecvEngineOff = '[%d]��Ӧ��������ʱ����IOCP����ر�';
  strRecvPostError = '[%d]Ͷ�ݽ�������ʱ�����˴��󡣴������:%d!';

  strSendEngineOff = '[%d]��Ӧ������������ʱ����IOCP����ر�';
  strSendErr       = '[%d]��Ӧ������������ʱ�����˴��󡣴������:%d!';
  strSendPostError = '[%d]Ͷ�ݷ�����������ʱ�����˴��󡣴������:%d';
  strSendZero      = '[%d]Ͷ�ݷ�����������ʱ����0�������ݡ����йرմ���'; 
  strSendPushFail  = '[%d]Ͷ�ݷ����������ݰ����������������󳤶�[%d/%d]��';

  strBindingIocpError = '[%d]�󶨵�IOCP���ʱ�������쳣, �������:%d, (%s)';

  strPushFail      = '[%d]ѹ�뵽�����Ͷ���ʧ��, ������Ϣ: %d/%d';

  strOnRecvBufferException = '[%d]��ӦOnRecvBufferʱ�������쳣:%s��';

var
  __innerLogger:TSafeLogger;
  __create_sn:Integer;
  __diocp_counter:Integer;

type
  TContextDoublyLinked = class(TObject)
  private
    FLocker: TIocpLocker;
    FHead:TDiocpCustomContext;
    FTail:TDiocpCustomContext;
    FCount:Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure add(pvContext:TDiocpCustomContext);
    function remove(pvContext:TDiocpCustomContext): Boolean;

    function Pop:TDiocpCustomContext;

    procedure write2List(pvList:TList);

    property Count: Integer read FCount;
    property Locker: TIocpLocker read FLocker;

  end;

//{$IFDEF DIOCP_DEBUG}
//procedure logDebugMessage(pvMsg: string; const args: array of const);
//begin
//  sfLogger.logMessage(pvMsg, args);
//end;
//{$ENDIF}

{$ifopt c+}
procedure call_int3;
asm
  int 3
end;
{$endif}

/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean):
    Boolean;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

procedure RegisterDiocpLogger(pvLogger:TSafeLogger);
begin
  if __diocp_logger <> pvLogger then
  begin
    __diocp_logger := pvLogger;
    if __innerLogger <> nil then
    begin
      __innerLogger.Free;
      __innerLogger := nil;
    end;
  end;
end;


function TDiocpCustomContext.CheckNextSendRequest: Boolean;
var
  lvRequest:TIocpSendRequest;
begin
  Result := false;
  Assert(FOwner <> nil);

  FContextLocker.lock();
  try
    lvRequest := TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest = nil then
    begin
      FSending := false;
      exit;
    end;
  finally
    FContextLocker.UnLock;
  end;

  if lvRequest <> nil then
  begin
    InnerPostSendRequest(lvRequest);
    Result := True;
  end;
end;

procedure TDiocpCustomContext.CheckReleaseRes;
var
  lvRequest:TIocpSendRequest;
begin
  while true do
  begin
    lvRequest :=TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest <> nil then
    begin
      FOwner.releaseSendRequest(lvRequest);
    end else
    begin
      Break;
    end;
  end;
end;

procedure TDiocpCustomContext.Close(pvDoShutDown: Boolean = True);
begin
  RequestDisconnect('TDiocpCustomContext.Close', nil, pvDoShutDown);
end;

constructor TDiocpCustomContext.Create;
begin
  inherited Create;
  FCloseRequest := TIocpActionRequest.Create;
  FCloseRequest.OnResponse := OnCloseRequestResponse;
  FCtxStateFlag := 0;
  FCtxStateLocker := 0;
  
  FCreateSN := InterlockedIncrement(__create_sn);

  FSocketState := ssDisconnected;
  FCtxStateFlag := CTX_STATE_INITIAL;
  FDebugStrings := TStringList.Create;
  FReferenceCounter := 0;
  FDisconnectedCounter := 0;
  FConnectedCounter := 0;
  FContextLocker := TIocpLocker.Create('contextlocker');
  FObjectAlive := False;
  FRawSocket := TRawSocket.Create();
  FActive := false;
  FSendRequestLink := TIocpRequestSingleLink.Create(1000);
end;

procedure TDiocpCustomContext.DoCtxStateLock({$IFDEF DIOCP_DEBUG} const pvDebugStr:string{$ENDIF});
begin
  SpinLock(FCtxStateLocker);
  {$IFDEF DIOCP_DEBUG}
  self.FCtxStateDebugStr := Format('threadid:%d, %s', [utils_strings.GetCurrentThreadID, pvDebugStr]);
  {$ENDIF}
end;

procedure TDiocpCustomContext.DoCtxStateUnLock;
begin
  {$IFDEF DIOCP_DEBUG}
  self.FCtxStateDebugStr := STRING_EMPTY;
  {$ENDIF}
  SpinUnLock(FCtxStateLocker);  
end;

destructor TDiocpCustomContext.Destroy;
begin
  if IsDebugMode then
  begin
    if FReferenceCounter <> 0 then
    begin
      call_int3;   // �ж�
    end;
  end;

  //FRawSocket.close;
  FRawSocket.Free;


  Assert(FSendRequestLink.Count = 0);
  FSendRequestLink.Free;
  FContextLocker.Free;
  FDebugStrings.Free;
  FCloseRequest.Free;
  inherited Destroy;
end;

function TDiocpCustomContext.DecReferenceCounter(pvDebugInfo: string; pvObj:
    TObject): Integer;
var
  r:Integer;
begin
  r := AtomicDecrement(self.FReferenceCounter);
  if r = 0 then
  begin
    PostCloseRequest();    
  end;
//  DoCtxStateLock({$IFDEF DIOCP_DEBUG}'DecReferenceCounter'{$ENDIF});
//  try
//    Dec(FReferenceCounter);
//    Result := FReferenceCounter;
//    {$IFDEF DIOCP_DEBUG}
//    if Length(pvDebugInfo) > 0 then
//      AddDebugStrings(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));
//    {$ENDIF}
//  finally
//     DoCtxStateUnLock;
//  end;
//    
//  CheckCloseContext();
end;

procedure TDiocpCustomContext.DoCleanUp;
begin
  FLastActivity := 0;
  FRequestDisconnect := false;
  FSending := false;

  {$IFDEF DIOCP_DEBUG}
  InnerAddToDebugStrings(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(Self), '-----DoCleanUp-----']));

  if IsDebugMode then
  begin
    if FReferenceCounter <> 0 then
    begin
      Assert(FReferenceCounter = 0);
    end;
    if FActive then
    begin
      Assert(False);
    end;
  end;
  {$ENDIF}
end;

procedure TDiocpCustomContext.DoConnected;
var
  r:Boolean;
  lvError:Integer;
begin
  // һЩ״̬�ĳ�ʼ��
  FRequestDisconnect := false;

  FLastActivity := GetTickCount;

  FReqDisFlag := 0;

  if IncReferenceCounter(STRING_EMPTY, nil) then
  begin
    {$IFDEF DIOCP_DEBUG}
    //�ù���̫��, �п��ܽ���Close����
    CheckThreadIn('DoConnected');
    {$ENDIF}
    try
      //  FRawSocket.SocketHandle;
      FSocketHandle := MakeDiocpHandle;

      Inc(FConnectedCounter);

      Assert(FOwner <> nil);
      if FActive then
      begin
        if IsDebugMode then
        begin
          Assert(not FActive);
        end;
        {$IFDEF DIOCP_DEBUG}
         FOwner.logMessage('on DoConnected event is already actived', CORE_LOG_FILE);
        {$ENDIF}
      end else
      begin
        FContextDNA := FOwner.RequestContextDNA;
        FActive := true;
        FOwner.AddToOnlineList(Self);
        {$IFDEF DIOCP_DEBUG}
        InnerAddToDebugStrings(Format('[%s]:*(%d):(%d:objAddr:%d) %s',
          [NowString, FReferenceCounter, self.SocketHandle, IntPtr(Self), 'DoConnected:��ӵ������б�']));
        {$ENDIF}

        if FOwner.FNoDelayOption then
        begin
          r := FRawSocket.SetNoDelayOption(True);
          if not r then
          begin
            lvError := GetLastError;
            FOwner.logMessage('FRawSocket.SetNoDelayOption, Error:%d', [lvError], CORE_LOG_FILE);
          end;
        end;
      
        if Assigned(FOwner.FOnContextConnected) then
        begin
          FOwner.FOnContextConnected(Self);
        end;

        try
          OnConnected();
        except
        end;
        if Assigned(FOnConnectedEvent) then
        begin
          FOnConnectedEvent(Self);
        end;
        SetSocketState(ssConnected);
        self.DoSetCtxState(CTX_STATE_CONNECTED);
        if (FOwner.FDisableConnectFlag = 1) then
        begin
          self.RequestDisconnect('����ֹͣ, �������');
        end else
        begin
          PostWSARecvRequest;
        end;
      end;

      finally
        {$IFDEF DIOCP_DEBUG}
        CheckThreadOut;
        {$ENDIF}
        self.DecReferenceCounter(STRING_EMPTY, nil);
      end;
  {$IFDEF DIOCP_DEBUG}
  end else
  begin
    Assert(false, '��������, ������');
  {$ENDIF}
  end;

end;

procedure TDiocpCustomContext.DoError(pvErrorCode: Integer);
begin
  FLastErrorCode:= pvErrorCode;
  FOwner.DoClientContextError(Self, pvErrorCode);
end;

procedure TDiocpCustomContext.DoNotifyDisconnected;
begin
  if Assigned(FOwner.FOnContextDisconnected) then
  begin
    FOwner.FOnContextDisconnected(Self);
  end;
  
  //
  OnDisconnected;

  if Assigned(FOnDisconnectedEvent) then FOnDisconnectedEvent(Self);


end;

procedure TDiocpCustomContext.DoReceiveData(pvRecvRequest: TIocpRecvRequest);
begin
  try
    FOnRecvingFlag := 1;
    FLastActivity := GetTickCount;

    Inc(Self.FRecvBytesSize, pvRecvRequest.FBytesTransferred);

    OnRecvBuffer(pvRecvRequest.FRecvBuffer.buf,
      pvRecvRequest.FBytesTransferred,
      pvRecvRequest.ErrorCode);
    if FOwner <> nil then
      FOwner.DoReceiveData(Self, pvRecvRequest);
  except
    on E:Exception do
    begin
      if FOwner <> nil then
      begin
        FOwner.LogMessage(strOnRecvBufferException, [SocketHandle, e.Message]);
        if Assigned(FOwner.OnContextError) then FOwner.OnContextError(Self, -1, E);
      end else
      begin
        __diocp_logger.logMessage(strOnRecvBufferException, [SocketHandle, e.Message]);
      end;
    end;
  end;
  FOnRecvingFlag := 0;

end;

procedure TDiocpCustomContext.CheckReleaseBack;
begin
  if self.FOwnePool <> nil then self.ReleaseBack;
end;

procedure TDiocpCustomContext.DoSendBufferCompleted(pvBuffer: Pointer;
  len: Cardinal; pvBufferTag, pvErrorCode: Integer);
begin
  
end;

procedure TDiocpCustomContext.DoSendRequestCompleted(pvRequest:
    TIocpSendRequest);
begin
  ;
end;

procedure TDiocpCustomContext.DoSetCtxState(const state:Integer);
begin
  DoCtxStateLock({$IFDEF DIOCP_DEBUG}'DoSetCtxState:' + IntToStr(state){$ENDIF});
  FCtxStateFlag := state;
  DoCtxStateUnLock;
end;

function TDiocpCustomContext.GetSendRequest: TIocpSendRequest;
begin
  Result := FOwner.GetSendRequest;
  Assert(Result <> nil);
  Result.FContext := self;
end;

function TDiocpCustomContext.IncReferenceCounter(pvDebugInfo: string; pvObj:
    TObject): Boolean;
var
  r:Integer;
begin
   Assert(Self<> nil);
   if FReferenceCounter < 0 then
   begin
     Result := false;
     Exit;
   end;

   if FReqDisFlag = 1 then
   begin      // ���������
     Result := false;
     Exit;
   end;

   r := AtomicIncrement(self.FReferenceCounter);
   if r > 0 then
   begin
     Result := True;
   end else
   begin
     // һ�㲻�ᵽ����
     AtomicDecrement(self.FReferenceCounter);
   end;



//  DoCtxStateLock({$IFDEF DIOCP_DEBUG}'IncReferenceCounter:' + pvDebugInfo{$ENDIF});
//  try
//    if (not Active) or (FRequestDisconnect) then
//    begin
//      Result := false;
//    end else
//    begin
//      Inc(FReferenceCounter);
//      {$IFDEF DIOCP_DEBUG}
//      if Length(pvDebugInfo) > 0 then
//        AddDebugStrings(Format('+(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));
//      {$ENDIF}
//
//      Result := true;
//    end;
//  finally
//    DoCtxStateUnLock;
//  end;

end;

procedure TDiocpCustomContext.InnerCloseContext(pvDoShutDown: Boolean = True);
begin
  Assert(FOwner <> nil);

  FCloseingFlag := 1;

  {$IFDEF DIOCP_DEBUG}
  FOwner.logMessage('(%d)�ر�����', [SocketHandle], 'InnerCloseContext');

  SetCurrentThreadInfo('InnerCloseContext - 1.0');
  AddDebugStrings(Format('*(%d):(%d:objAddr:%d)->InnerCloseContext- BEGIN, socketstate:%d',
         [FReferenceCounter, self.SocketHandle, IntPtr(Self), Ord(FSocketState)]));

  if FReferenceCounter <> 0 then
    FOwner.logMessage('InnerCloseContext FReferenceCounter:%d', [FReferenceCounter],
    CORE_LOG_FILE);
  if not FActive then
  begin
    FOwner.logMessage('[%d]:InnerCloseContext FActive is false, socketstate:%d, %s',
      [self.SocketHandle, Ord(FSocketState), GetDebugStrings()], CORE_LOG_FILE);
    AddDebugStrings(Format('*[*][%d]:InnerCloseContext FActive is false, socketstate:%d',
       [self.SocketHandle, Ord(FSocketState)]));
    FSocketState := ssDisconnected;
    SetCurrentThreadInfo('InnerCloseContext - 1.1');
    Exit;
  end;
  SetCurrentThreadInfo('InnerCloseContext - 1.2');
  {$ENDIF}
  if not FActive then
  begin
    FSocketState := ssDisconnected;
    Exit;
  end;
  {$IFDEF DIOCP_DEBUG}
  CheckThreadIn('InnerCloseContext');
  {$ENDIF}
  DoSetCtxState(CTX_STATE_CLOSING);
  try
      FSocketState := ssDisconnecting;
      FActive := false;
      FCloseSocketFlag := 1;
      FRawSocket.Close(pvDoShutDown);
      CheckReleaseRes;
      {$IFDEF DIOCP_DEBUG}
      SetCurrentThreadInfo('InnerCloseContext - 1.3');
      {$ENDIF}
      try
      
        DoNotifyDisconnected;

        if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
        begin
          FOwner.DataMoniter.IncDisconnectCounter;
        end;

        DoCleanUp;
        {$IFDEF DIOCP_DEBUG}
        SetCurrentThreadInfo('InnerCloseContext - 1.4');
        {$ENDIF}
      except
  //      on e:Exception do
  //      begin
  //        sfLogger.LogMessage(
  //          Format('InnerCloseContext(%s):%s', [lvDebugStep, e.Message]), 'ִ�йر��쳣');
  //      end;
      end;

  finally
    FCloseingFlag := 0;

    {$IFDEF DIOCP_DEBUG}
    AddDebugStrings(Format('*(%d):(%d:objAddr:%d)->,%s',
      [FReferenceCounter, Self.SocketHandle, IntPtr(Self), 'InnerCloseContext:�Ƴ������б�']));
    SetCurrentThreadInfo('InnerCloseContext - 1.5');

    FOwner.RemoveFromOnOnlineList(Self);
    SetCurrentThreadInfo('InnerCloseContext - 1.5.1');

    // ����Socket״̬
    SetSocketState(ssDisconnected);
    SetCurrentThreadInfo('InnerCloseContext - 1.5.2');
    Inc(FDisconnectedCounter);
    DoSetCtxState(CTX_STATE_INITIAL);
    SetCurrentThreadInfo('InnerCloseContext - 1.5.3');

    CheckThreadOut();

    // ���Թ黹����
    ReleaseBack;
    SetCurrentThreadInfo('InnerCloseContext - 1.6');
    {$ELSE}
    FOwner.RemoveFromOnOnlineList(Self);
    // ����Socket״̬
    SetSocketState(ssDisconnected);
    Inc(FDisconnectedCounter);
    DoSetCtxState(CTX_STATE_INITIAL);
    // ���Թ黹����
    ReleaseBack;
    {$ENDIF}

  end;
end;

procedure TDiocpCustomContext.lock;
begin
  FContextLocker.lock();
end;

function TDiocpCustomContext.LockContext(pvDebugInfo: string; pvObj: TObject):
    Boolean;
begin
  Result := IncReferenceCounter(pvDebugInfo, pvObj);
end;

procedure TDiocpCustomContext.OnCloseRequestResponse(Sender: TObject);
begin
  self.InnerCloseContext();  
end;

procedure TDiocpCustomContext.OnConnected;
begin
  FSendBytesSize:=0;
  FRecvBytesSize:= 0;
end;

procedure TDiocpCustomContext.OnDisconnected;
begin

end;





procedure TDiocpCustomContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode:
    WORD);
begin
  
end;

procedure TDiocpCustomContext.PostCloseRequest;
begin
  DoSetCtxState(CTX_STATE_CLOSING);
  // ���߳�Ͷ��
  self.Owner.IocpEngine.PostRequest(FCloseRequest);
end;

procedure TDiocpCustomContext.postNextSendRequest;
begin
  self.lock;
  try
    if not CheckNextSendRequest then FSending := false;
  finally
    self.UnLock;
  end;
end;

function TDiocpCustomContext.PostSendRequestDelete(
    pvSendRequest:TIocpSendRequest): Boolean;
begin
  Result := false;
  if IncReferenceCounter('TIocpClientContext.PostSendRequestDelete', pvSendRequest) then
  begin
    try
      FContextLocker.lock();   
      try    
        Result := FSendRequestLink.Push(pvSendRequest);
        if Result then
        begin      
          if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
          begin
            FOwner.FDataMoniter.incPushSendQueueCounter;
          end;
          Result := true;

          if not FSending then
          begin
            FSending := true;  // first: set true
            if not CheckNextSendRequest then
              FSending := false;
          end;
        end;
      finally
        FContextLocker.UnLock;
      end;

      if not Result then
      begin
      {$IFDEF DIOCP_DEBUG}
        if FOwner.logCanWrite then
          FOwner.logMessage('Push sendRequest to Sending Queue fail, queue size:%d',
           [FSendRequestLink.Count]);
      {$ENDIF}

        FOwner.releaseSendRequest(pvSendRequest);

        Self.RequestDisconnect(Format('Push sendRequest to Sending Queue fail, queue size:%d',
           [FSendRequestLink.Count]), Self);
      end;
    finally
      DecReferenceCounter('TIocpClientContext.PostSendRequestDelete', pvSendRequest);
    end;
  end else
  begin
    FOwner.releaseSendRequest(pvSendRequest);
  end;
end;

procedure TDiocpCustomContext.PostWSARecvRequest;
var
  lvRecvRequest:TIocpRecvRequest;
  lvSuccFlag:Boolean;
begin
  if IncReferenceCounter(STRING_EMPTY, nil) then
  try
    lvRecvRequest := FOwner.GetRecvRequest;
    lvRecvRequest.FContext := Self;
    InterlockedIncrement(FPostWsRecvingCnt);
    lvSuccFlag := True;
    if not lvRecvRequest.PostRequest then
    begin
      InterlockedDecrement(FPostWsRecvingCnt);
      lvRecvRequest.ReleaseBack;
      lvSuccFlag := False;
    end;
  finally
    if not lvSuccFlag then
      Self.DecReferenceCounter(STRING_EMPTY, nil)
  end;
end;



function TDiocpCustomContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
    pvCopyBuf: Boolean = true; pvTag: Integer = 0; pvTagData: Pointer = nil):
    Boolean;
var
  lvBuf: PAnsiChar;
begin
  if len = 0 then raise Exception.Create('PostWSASendRequest::request buf is zero!');
  if pvCopyBuf then
  begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    Result := PostWSASendRequest(lvBuf, len, dtFreeMem, pvTag, pvTagData);
    if not Result then
    begin            //post fail
      FreeMem(lvBuf);
    end;
  end else
  begin
    lvBuf := buf;
    Result := PostWSASendRequest(lvBuf, len, dtNone, pvTag, pvTagData);
  end;

end;



procedure TDiocpCustomContext.RequestDisconnect(pvReason: string =
    STRING_EMPTY; pvObj: TObject = nil; pvDoShutDown: Boolean = True);
var
  lvStr:String;
begin
  if AtomicCmpExchange(self.FReqDisFlag, 1, 0) = 0 then
  begin
    DoSetCtxState(CTX_STATE_WAITFOR_CLOSE);
    self.FDisconnectedReason := pvReason;
    // cancel
    // ��ʱ����Ч(doss_gw,��Ӧ����������,shutdown��ִ�е�postWSRecv��Ȼ����)
    //FRawSocket.ShutDown();
    //FRawSocket.CancelIOEx;
    FRawSocket.Close(pvDoShutDown);
    //   FRawSocket.CancelIO;
  end;
end;



procedure TDiocpCustomContext.SetOwner(const Value: TDiocpCustom);
begin
  FOwner := Value;
end;

procedure TDiocpCustomContext.SetSocketState(pvState:TSocketState);
begin
  FSocketState := pvState;
  if Assigned(FOnSocketStateChanged) then
  begin
    FOnSocketStateChanged(Self);
  end;
end;

procedure TDiocpCustomContext.UnLock;
begin
  FContextLocker.UnLock;
end;

procedure TDiocpCustomContext.UnLockContext(pvDebugInfo: string; pvObj:
    TObject);
begin
  if Self = nil then
  begin
    Assert(Self<> nil);
  end;
  DecReferenceCounter(pvDebugInfo, pvObj);
end;

function TDiocpCustom.CheckClientContextValid(const pvClientContext:
    TDiocpCustomContext): Boolean;
begin
  Result := (pvClientContext.FOwner = Self);
end;

constructor TDiocpCustom.Create(AOwner: TComponent);
var
  r:Integer;
begin
  inherited Create(AOwner);
  
  
  if Length(Name) = 0 then
  begin
    r := InterlockedIncrement(__diocp_counter);

    Name := Format('%s_%d', [self.ClassName, r]);
  end;



  UseObjectPool := True;

  FOperaOptions := 0;

  FContextPool := TSafeQueue.Create;

  FASyncInvoker := TASyncInvoker.Create;
  
  FDebugStrings := TStringList.Create;

  FRefCounter := 0;
  CheckWinSocketStart;

  FDefaultMsgType := self.ClassName;

  FLocker := TIocpLocker.Create('diocp_tcp_client');

  {$IFDEF DIOCP_DEBUG}
  FDebug_SendRequestCounter:=0;
  {$ENDIF}
  FOnlineContextList := TDHashTable.Create(10949);

  // send requestPool
  FSendRequestPool := TBaseQueue.Create;
  FRecvRequestPool := TBaseQueue.Create;

  // ����Ĭ�ϵ�Diocp����
  StartDiocpEngine;
  FOwnerEngine := False;
  BindDiocpEngine(__defaultDiocpEngine, False);

  // post wsaRecv block size
  FWSARecvBufferSize := 1024 * 4;

  FWSASendBufferSize := 1024 * 8;


end;

function TDiocpCustom.DecRefCounter: Integer;
begin
  Result := InterlockedDecrement(FRefCounter);
end;

destructor TDiocpCustom.Destroy;
begin
  
  FIsDestroying := true;

  Close;

  if FDataMoniter <> nil then FDataMoniter.Free;


  FOnlineContextList.Free;

  CheckDoDestroyEngine;

  FSendRequestPool.Free;
  FRecvRequestPool.Free;

  FLocker.Free;

  FDebugStrings.Free;

  FContextPool.Free;

  FASyncInvoker.Free;
  
  inherited Destroy;
end;

procedure TDiocpCustom.AddDebugStrings(pvDebugInfo: String; pvAddTimePre:
    Boolean = true);
var
  s:string;
begin
  if pvAddTimePre then s := Format('[%s]:%s', [NowString, pvDebugInfo])
  else s := pvDebugInfo;
  FLocker.lock();
  try
    InnerAddToDebugStrings(s);
  finally
    FLocker.unLock;
  end;
end;

procedure TDiocpCustom.AddToOnlineList(pvObject: TDiocpCustomContext);
var
  i:Integer;
begin
  FLocker.lock('AddToOnlineList');
  try
//  ��־���Լ�¼�����б�������������Ժܺõķ�������
//    sfLogger.logMessage(Format('1,%d,%d,%d,%d,%d',
//      [FOnlineContextList.Count, pvObject.SocketHandle, IntPtr(pvObject),
//       IntPtr(pvObject.FRawSocket), pvObject.FRawSocket.SocketHandle]), 'ONLINE');
    FOnlineContextList.Add(pvObject.FSocketHandle, pvObject);
    i := FOnlineContextList.Count;
//    sfLogger.logMessage(Format('2,%d,%d,%d,%d,%d',
//      [FOnlineContextList.Count, pvObject.SocketHandle, IntPtr(pvObject),
//       IntPtr(pvObject.FRawSocket), pvObject.FRawSocket.SocketHandle]), 'ONLINE');
  finally
    FLocker.unLock;
  end;

  if DataMoniter <> nil then
  begin
    DataMoniter.CalcuMaxOnlineCount(i);
  end;
end;

procedure TDiocpCustom.BindDiocpEngine(const pvEngine: TIocpEngine; pvOwner:
    Boolean = true);
begin
  CheckDoDestroyEngine;
    
  FIocpEngine := pvEngine;
  FOwnerEngine := pvOwner;
end;

procedure TDiocpCustom.CheckCreatePoolObjects(pvMaxConnection: Integer);
var
  i, j: Integer;
  lvRecv:TIocpRecvRequest;
begin
  j := pvMaxConnection + 5;
  for i := 0 to j -1 do
  begin
    FContextPool.EnQueueObject(CreateContext, raObjectFree);
  end;

  j := pvMaxConnection * 2;
  for i := 0 to j -1 do
  begin
    self.FSendRequestPool.EnQueueObject(InnerCreateSendRequest, raObjectFree);
  end;

  j := pvMaxConnection + 100;
  for i := 0 to j -1 do
  begin
    lvRecv := InnerCreateRecvRequest;
    lvRecv.CheckCreateRecvBuffer;
    self.FRecvRequestPool.EnQueueObject(lvRecv, raObjectFree);
  end;
end;

procedure TDiocpCustom.CheckDoDestroyEngine;
begin
  if FOwnerEngine then
  begin
    if FIocpEngine <> nil then
    begin
      FIocpEngine.SafeStop();
      FIocpEngine.Free;
      FIocpEngine := nil;
    end;
    FOwnerEngine := False;
  end;
end;



function TDiocpCustom.CheckTimeOutContext(pvContext:TDiocpCustomContext;
    pvTimeOut:Cardinal = 60000): Integer;
var
  I:Integer;
  lvContext:TDiocpCustomContext;
var
  lvBucket, lvNextBucket: PDHashData;
begin
  Result := 0;
  self.IncRefCounter;
  try
    FLocker.lock('CheckTimeOutContext');
    try
      for I := 0 to FOnlineContextList.BucketSize - 1 do
      begin
        lvBucket := FOnlineContextList.Buckets[I];
        while lvBucket<>nil do
        begin
          lvNextBucket := lvBucket.Next;
          if lvBucket.Data <> nil then
          begin
            lvContext := TDiocpCustomContext(lvBucket.Data);
            if lvContext = pvContext then
            begin
              if lvContext.CheckActivityTimeOut(pvTimeOut) then
              begin
                Result := 1;
              end else
              begin
                Result := 2;
              end;
              Break;
            end;
          end;
          lvBucket:= lvNextBucket;
        end;
      end;
    finally
      FLocker.unLock;
    end;
  finally
    self.DecRefCounter;
  end;
end;

procedure TDiocpCustom.DoAfterClose;
begin
  
end;

procedure TDiocpCustom.DoAfterOpen;
begin

end;

procedure TDiocpCustom.DoClientContextError(pvClientContext:
    TDiocpCustomContext; pvErrorCode: Integer);
begin
  if Assigned(FOnContextError) then
    FOnContextError(pvClientContext, pvErrorCode, nil);
end;

procedure TDiocpCustom.DoIdle;
begin
  
end;

procedure TDiocpCustom.DoReceiveData(pvIocpContext: TDiocpCustomContext;
    pvRequest: TIocpRecvRequest);
begin
  if Assigned(pvIocpContext.FOnRecvBufferEvent) then
    pvIocpContext.FOnRecvBufferEvent(pvIocpContext,
      pvRequest.FRecvBuffer.buf, pvRequest.FBytesTransferred,
      pvRequest.ErrorCode);


  if Assigned(FOnReceivedBuffer) then
    FOnReceivedBuffer(pvIocpContext,
      pvRequest.FRecvBuffer.buf, pvRequest.FBytesTransferred,
      pvRequest.ErrorCode);
end;

function TDiocpCustom.GetWorkerCount: Integer;
begin
  Result := FIocpEngine.WorkerCount;
end;

function TDiocpCustom.IsDestroying: Boolean;
begin
  Result := FIsDestroying;  // or (csDestroying in self.ComponentState);
end;

function TDiocpCustom.LogCanWrite: Boolean;
begin
  Result := (not IsDestroying) and __diocp_logger.Enable;
end;



procedure TDiocpCustom.Open;
begin
  if FActive = true then exit;

  FDisableConnectFlag := 0;

  //if FDataMoniter <> nil then FDataMoniter.clear;

  // engine start
  FIocpEngine.CheckStart;

  FActive := True;

  FASyncInvoker.Start(OnASyncWork);

  DoAfterOpen;   
end;

procedure TDiocpCustom.RegisterContextClass(pvContextClass: TIocpContextClass);
begin
  FContextClass := pvContextClass;
end;

function TDiocpCustom.ReleaseSendRequest(pvObject:TIocpSendRequest): Boolean;
begin
  Result := false;
  if self = nil then
  begin
    Assert(False);
  end;
  if FSendRequestPool = nil then
  begin
    // check call stack is crash
    Assert(FSendRequestPool <> nil);
  end;

  if IsDebugMode then
  begin
    Assert(pvObject.FAlive)
  end;

  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then
  begin
    if pvObject.FBuf <> nil then
    begin
      /// Buff�������, ��Ӧ�¼�
      DoSendBufferCompletedEvent(pvObject.FContext, pvObject.FBuf, pvObject.FLen, pvObject.Tag, pvObject.Data, pvObject.ErrorCode);
    end;


    pvObject.DoCleanUp;
    pvObject.FOwner := nil;

    {$IFDEF DIOCP_DEBUG}
    pvObject.CheckThreadOut();
    {$ENDIF}

    if (FDataMoniter <> nil) then
    begin
      FDataMoniter.IncSendRequestReturnCounter;
    end;

    if self.UseObjectPool then
    begin
      FSendRequestPool.EnQueueObject(pvObject, raObjectFree);
    end else
    begin
      pvObject.Free;
    end;
    Result := true;
  end else
  begin
    if IsDebugMode then
    begin
      Assert(false)
    end;
  end;
end;

procedure TDiocpCustom.Close;
begin
  FActive := false;
  FDisableConnectFlag := 1;
  
  FASyncInvoker.Terminate;

  DisconnectAll;

  WaitForContext(30000);

  FASyncInvoker.WaitForStop;

  DoAfterClose;
end;

procedure TDiocpCustom.SetActive(pvActive:Boolean);
begin
  if pvActive <> FActive then
  begin
    if pvActive then
    begin
      Open;
    end else
    begin
      Close;
    end;
  end;
end;

procedure TDiocpCustom.SetWorkerCount(const Value: Integer);
begin
  if FIocpEngine = __defaultDiocpEngine then Exit;
  FIocpEngine.setWorkerCount(Value);
end;

procedure TDiocpCustom.CreateDataMonitor;
begin
  if FDataMoniter = nil then
  begin
    FDataMoniter := TIocpDataMonitor.Create;
  end;
end;

function TDiocpCustom.CreateContext: TDiocpCustomContext;
begin
  if FContextClass <> nil then
  begin
    Result := FContextClass.Create;
  end else
  begin
    Result := TDiocpCustomContext.Create;
  end;
  Result.Owner := Self;
  OnCreateContext(Result);
  if FDataMoniter <> nil then
  begin
    FDataMoniter.IncContextCreateCounter;
  end;
end;

procedure TDiocpCustom.DisconnectAll;
var
  I:Integer;
  lvBucket: PDHashData;
  lvClientContext:TDiocpCustomContext;
begin
  FLocker.lock('DisconnectAll');
  try
    for I := 0 to FOnlineContextList.BucketSize - 1 do
    begin
      lvBucket := FOnlineContextList.Buckets[I];
      while lvBucket<>nil do
      begin
        lvClientContext := TDiocpCustomContext(lvBucket.Data);
        if lvClientContext <> nil then
        begin
          lvClientContext.RequestDisconnect('��������Ͽ���������');
        end;
        lvBucket:=lvBucket.Next;
      end;
    end;
  finally
    FLocker.unLock;
  end;
end;

procedure TDiocpCustom.DoSendBufferCompletedEvent(pvContext:
    TDiocpCustomContext; pvBuff: Pointer; len: Cardinal; pvBufferTag:Integer;
    pvBufferTagData:Pointer; pvErrorCode: Integer);
begin
  if pvContext <> nil then
  begin
    try
      pvContext.DoSendBufferCompleted(pvBuff, len, pvBufferTag, pvErrorCode);
    except
      on e:Exception do
      begin
        LogMessage('pvContext.DoSendBufferCompleted error:' + e.Message, '', lgvError);
      end;
    end;
  end;
  
  try
    if Assigned(pvContext.FOnSendBufferCompleted) then
      pvContext.FOnSendBufferCompleted(pvContext, pvBuff, len, pvBufferTag, pvBufferTagData, pvErrorCode);

    if Assigned(FOnSendBufferCompleted) then
      FOnSendBufferCompleted(pvContext, pvBuff, len, pvBufferTag, pvBufferTagData, pvErrorCode);
  except
    on e:Exception do
    begin
      LogMessage('DoSendBufferCompletedEvent error:' + e.Message, '', lgvError);
    end;
  end;

end;

function TDiocpCustom.FindContext(pvSocketHandle: THandle): TDiocpCustomContext;
begin
  FLocker.lock('FindContext');
  try
    Result := TDiocpCustomContext(FOnlineContextList.FindFirstData(pvSocketHandle));
  finally
    FLocker.UnLock();
  end;
end;

function TDiocpCustom.GetDebugString: String;
begin
  FLocker.lock();
  try
    Result := FDebugStrings.Text;
  finally
    FLocker.unLock;
  end;
end;

function TDiocpCustom.GetTimeOutContexts(var vOutList: TContextArray;
    pvTimeOut: Cardinal = 60000): Integer;
var
  I, j:Integer;
  lvContext:TDiocpCustomContext;
var
  lvBucket, lvNextBucket: PDHashData;
begin
  self.IncRefCounter;
  try
    FLocker.lock('GetTimeOutContexts');
    try
      j := 0;
      SetLength(vOutList, FOnlineContextList.Count);
      for I := 0 to FOnlineContextList.BucketSize - 1 do
      begin
        lvBucket := FOnlineContextList.Buckets[I];
        while lvBucket<>nil do
        begin
          lvNextBucket := lvBucket.Next;
          if lvBucket.Data <> nil then
          begin
            lvContext := TDiocpCustomContext(lvBucket.Data);
            if lvContext.CheckActivityTimeOut(pvTimeOut) then
            begin
              vOutList[j] := lvContext;
              Inc(j);
            end;
          end;
          lvBucket:= lvNextBucket;
        end;
      end;
    finally
      FLocker.unLock;
    end;

    Result := j;
  finally
    self.DecRefCounter;
  end;
end;

function TDiocpCustom.GetOnlineContextCount: Integer;
begin
  Result := FOnlineContextList.Count;
end;

procedure TDiocpCustom.GetOnlineContextList(pvList:TList);
begin
  FLocker.lock('getOnlineContextList');
  try
    FOnlineContextList.GetDatas(pvList);
  finally
    FLocker.unLock;
  end;
end;

function TDiocpCustom.GetSendRequest: TIocpSendRequest;
begin
  if FUseObjectPool then
  begin
    Result := TIocpSendRequest(FSendRequestPool.DeQueueObject);
  end else
  begin
    result := nil;
  end;
  if Result = nil then
  begin
    Result := self.InnerCreateSendRequest;
  end;
  if self.FDataMoniter <> nil then FDataMoniter.IncSendRequestOutCounter;
  Result.FAlive := true;
  Result.DoCleanup;
  Result.FOwner := Self;
  {$IFDEF DIOCP_DEBUG}
  InterlockedIncrement(FDebug_SendRequestCounter);
  {$ENDIF}
  {$IFDEF DIOCP_DEBUG}
  Result.CheckThreadIn();
  {$ENDIF}
end;

function TDiocpCustom.IncRefCounter: Integer;
begin
  Result := InterlockedIncrement(FRefCounter);
end;

procedure TDiocpCustom.InnerAddToDebugStrings(const pvMsg: String);
begin
  FDebugStrings.Add(pvMsg);
  if FDebugStrings.Count > 500 then FDebugStrings.Delete(0);
end;

function TDiocpCustom.KickOut(pvTimeOut:Cardinal = 60000): Integer;
var
  I, j:Integer;
  lvKickOutList: array of TDiocpCustomContext;
  lvContext:TDiocpCustomContext;
var
  lvBucket, lvNextBucket: PDHashData;
begin
  self.IncRefCounter;
  try
    {$IFDEF DIOCP_DEBUG}
    SetCurrentThreadInfo('TDiocpCustom.KickOut-1');
    {$ENDIF}
    FLocker.lock('KickOut');
    try
      j := 0;
      SetLength(lvKickOutList, FOnlineContextList.Count);
      for I := 0 to FOnlineContextList.BucketSize - 1 do
      begin
        lvBucket := FOnlineContextList.Buckets[I];
        while lvBucket<>nil do
        begin
          lvNextBucket := lvBucket.Next;
          if lvBucket.Data <> nil then
          begin
            lvContext := TDiocpCustomContext(lvBucket.Data);
            if lvContext.CheckActivityTimeOut(pvTimeOut) then
            begin
              lvKickOutList[j] := lvContext;
              Inc(j);
            end;
          end;
          lvBucket:= lvNextBucket;
        end;
      end;
      {$IFDEF DIOCP_DEBUG}
      SetCurrentThreadInfo('TDiocpCustom.KickOut-2.1');
      {$ENDIF}
      Result := 0;
      for i := 0 to j - 1 do
      begin
        {$IFDEF DIOCP_DEBUG}

        Self.AddDebugStrings(Format('%d, ִ��KickOut', [lvKickOutList[i].SocketHandle]));
        SetCurrentThreadInfo('TDiocpCustom.KickOut-2.2');
        lvKickOutList[i].RequestDisconnect('��ʱ��������Ͽ�');
        SetCurrentThreadInfo('TDiocpCustom.KickOut-2.3');
        {$ELSE}
        lvKickOutList[i].RequestDisconnect();
        {$ENDIF}
        Inc(Result);
      end;
    finally
      FLocker.unLock;
    end;

  finally
    self.DecRefCounter;
  end;

end;

procedure TDiocpCustom.LogMessage(pvMsg: string; pvMsgType: string = '';
    pvLevel: TLogLevel = lgvMessage);
begin
  if LogCanWrite then
  begin
    if pvMsgType <> '' then
    begin
      __diocp_logger.logMessage(pvMsg, pvMsgType, pvLevel);
    end else
    begin
      __diocp_logger.logMessage(pvMsg, FDefaultMsgType, pvLevel);
    end;
  end;
end;

procedure TDiocpCustom.logMessage(pvMsg: string; const args: array of const;
    pvMsgType: string; pvLevel: TLogLevel);
begin
  if LogCanWrite then
  begin
    if pvMsgType <> '' then
    begin
      __diocp_logger.logMessage(pvMsg, args, pvMsgType, pvLevel);
    end else
    begin
      __diocp_logger.logMessage(pvMsg, args, FDefaultMsgType, pvLevel);
    end;

  end;  
end;

procedure TDiocpCustom.OnASyncWork(pvASyncWorker: TASyncWorker);
var
  lvFileWriter: TSingleFileWriter;  
begin
  lvFileWriter := TSingleFileWriter.Create;
  try
    lvFileWriter.FilePreFix := self.Name + '_���_';
    {$IFDEF DEBUG}
    lvFileWriter.LogMessage('����ʱ��:' + FormatDateTime('yyyy-mm-dd:HH:nn:ss.zzz', Now));
    {$ENDIF}
    while not pvASyncWorker.Terminated do
    begin
      try
        DoASyncWork(lvFileWriter, pvASyncWorker);
        DoIdle();
        if not pvASyncWorker.Terminated then
        begin
          self.FASyncInvoker.WaitForSleep(100);
        end;
      except
        on e:Exception do
        begin
          lvFileWriter.LogMessage('ERR:' + e.Message);
          self.FASyncInvoker.WaitForSleep(1000);
        end;  
      end;
    end;
  finally
    {$IFDEF DEBUG}
    lvFileWriter.LogMessage('ֹͣʱ��:' + FormatDateTime('yyyy-mm-dd:HH:nn:ss.zzz', Now));
    {$ENDIF}
    lvFileWriter.Flush;    
    lvFileWriter.Free;
  end;
end;

procedure TDiocpCustom.DoASyncWork(pvFileWritter: TSingleFileWriter;
    pvASyncWorker: TASyncWorker);
begin
  
end;

function TDiocpCustom.GetContextFromPool: TDiocpCustomContext;
begin
  if self.UseObjectPool then
  begin
    Result := TDiocpCustomContext(FContextPool.DeQueueObject);
  end else
  begin
    Result := nil;
  end;
  if Result = nil then
  begin
    Result := CreateContext;
  end;
  Result.Owner := Self;
  Result.FReleaseBack := self.ReleaseContext;
  Result.FOwnePool := FContextPool;
  Result.FObjectAlive := True;

  if Assigned(FDataMoniter) then FDataMoniter.IncContextOutCounter;
end;

procedure TDiocpCustom.ReleaseContext(pvContext:TDiocpCustomContext);
begin
  if Assigned(FDataMoniter) then FDataMoniter.IncContextReturnCounter;
  if self.UseObjectPool then
  begin
    self.FContextPool.EnQueueObject(pvContext, raObjectFree);
  end else
  begin
    pvContext.Free;
  end;
end;

function TDiocpCustom.GetRecvRequest: TIocpRecvRequest;
begin
  if UseObjectPool then
  begin
    Result := TIocpRecvRequest(FRecvRequestPool.DeQueueObject);
    if Result = nil then
    begin
      Result := InnerCreateRecvRequest;
    end;
  end else
  begin
    Result := InnerCreateRecvRequest;
  end;
  Result.Tag := 0;
  Result.FAlive := true;
  if FDataMoniter <> nil then FDataMoniter.IncRecvRequestOutCounter;
  {$IFDEF DIOCP_DEBUG}
  Result.CheckThreadIn();
  {$ENDIF}
end;


procedure TDiocpCustom.OnIocpException(pvRequest:TIocpRequest; E:Exception);
begin
  try
    if pvRequest <> nil then
    begin
      LogMessage('δ�����쳣:%s, ����(%s)��Ϣ:%s',[E.Message, pvRequest.ClassName, pvRequest.Remark],
        CORE_LOG_FILE, lgvError);
    end else
    begin
      LogMessage('δ�����쳣:%s',[E.Message], CORE_LOG_FILE, lgvError);
    end;
  except
  end;
end;

function TDiocpCustom.ReleaseRecvRequest(pvObject: TIocpRecvRequest): Boolean;
begin
  if self = nil then
  begin
    Assert(False);
  end;
  if FRecvRequestPool = nil then
  begin
    // check call stack is crash
    Assert(FRecvRequestPool <> nil);
  end;

  if IsDebugMode then
  begin
    Assert(pvObject.FAlive)
  end;

 {$IFDEF DIOCP_DEBUG}
 pvObject.CheckThreadOut();
 {$ENDIF}

  if UseObjectPool then
  begin
    if lock_cmp_exchange(True, False, pvObject.FAlive) = True then
    begin
      if FDataMoniter <> nil then FDataMoniter.IncRecvRequestReturnCounter;

  //    if pvObject.FBuf <> nil then
  //    begin
  //      /// Buff�������, ��Ӧ�¼�
  //      DoSendBufferCompletedEvent(pvObject.FClientContext, pvObject.FBuf, pvObject.FLen, pvObject.Tag, pvObject.ErrorCode);
  //    end;

      // ����Buffer
     // pvObject.DoCleanUp;
      pvObject.FContext := nil;
      FRecvRequestPool.EnQueueObject(pvObject, raObjectFree);
      Result := true;
    end else
    begin
      Result := false;
    end;
  end else
  begin
    pvObject.Free;
    Result := True;
  end;
end;

procedure TDiocpCustom.RemoveFromOnOnlineList(pvObject: TDiocpCustomContext);
{$IFDEF DIOCP_DEBUG}
  var
    lvSucc:Boolean;
{$ENDIF}
begin
  FLocker.lock('RemoveFromOnOnlineList');
  try
    {$IFDEF DIOCP_DEBUG}

//  ��־���Լ�¼�����б��ɾ����������Ժܺõķ�������
//    sfLogger.logMessage(Format('3,%d,%d,%d,%d,%d',
//      [FOnlineContextList.Count, pvObject.SocketHandle, IntPtr(pvObject),
//       IntPtr(pvObject.FRawSocket), pvObject.FRawSocket.SocketHandle]), 'ONLINE');
    lvSucc := FOnlineContextList.DeleteFirst(pvObject.FSocketHandle);
//    sfLogger.logMessage(Format('4,%d,%d,%d,%d,%d',
//      [FOnlineContextList.Count, pvObject.SocketHandle, IntPtr(pvObject),
//      IntPtr(pvObject.FRawSocket), pvObject.FRawSocket.SocketHandle]), 'ONLINE'); 
    Assert(lvSucc);
    {$ELSE}
    FOnlineContextList.DeleteFirst(pvObject.FSocketHandle);
    {$ENDIF}                                               
  finally
    FLocker.unLock;
  end;
end;

function TDiocpCustom.RequestContextDNA: Integer;
begin
  Result := InterlockedIncrement(FContextDNA);
end;

procedure TDiocpCustom.SetName(const NewName: TComponentName);
begin
  inherited;
end;

procedure TDiocpCustom.SetWSARecvBufferSize(const Value: Cardinal);
begin
  FWSARecvBufferSize := Value;
  if FWSARecvBufferSize = 0 then
  begin
    FWSARecvBufferSize := 1024 * 4;
  end;
end;

procedure TDiocpCustom.SetWSASendBufferSize(const Value: Cardinal);
begin
  FWSASendBufferSize := Value;
  if FWSASendBufferSize <=0 then
    FWSASendBufferSize := 1024 * 8;
end;

procedure TDiocpCustom.IncOperaOptions(const pvFlag:Integer);
begin
  FOperaOptions := FOperaOptions or pvFlag;
end;

procedure TDiocpCustom.DecOperaOptions(const pvFlag:Integer);
begin
  FOperaOptions := FOperaOptions AND (not pvFlag);
end;

function TDiocpCustom.CheckOperaFlag(const pvFlag:Integer): Boolean;
begin
  Result := ((FOperaOptions and FOperaOptions) <> 0);
end;

function TDiocpCustom.InnerCreateRecvRequest: TIocpRecvRequest;
begin
  Result := TIocpRecvRequest.Create;
  Result.FOwner := Self;
  if FDataMoniter <> nil then FDataMoniter.IncRecvRequestCreateCounter;

end;

function TDiocpCustom.InnerCreateSendRequest: TIocpSendRequest;
begin
  if FIocpSendRequestClass <> nil then
  begin
    Result := FIocpSendRequestClass.Create;
  end else
  begin
    Result := TIocpSendRequest.Create;
  end;
  if (FDataMoniter <> nil) then FDataMoniter.IncSendRequestCreateCounter;
end;

procedure TDiocpCustom.OnCreateContext(const pvContext: TDiocpCustomContext);
begin

end;



function TDiocpCustom.WaitForContext(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
  c:Integer;
begin
  l := GetTickCount;
  c := FOnlineContextList.Count;
  while (c > 0) or (FRefCounter > 0) do
  begin
    Sleep(10);

    if GetTickCount - l > pvTimeOut then
    begin
      {$IFDEF DIOCP_DEBUG}
       if LogCanWrite then
        logMessage('WaitForContext End Current num:%d', [c], CORE_LOG_FILE);
      {$ENDIF}
      Break;
    end;
    c := FOnlineContextList.Count;
  end;

  Result := FOnlineContextList.Count = 0;  
end;

procedure TIocpAcceptorMgr.checkPostRequest(pvContext: TDiocpCustomContext);
var
  lvRequest:TIocpAcceptExRequest;
begin
  FLocker.lock;
  try
    if FList.Count > FMinRequest then Exit;

    // post request
    while FList.Count < FMaxRequest do
    begin
      lvRequest := TIocpAcceptExRequest.Create(FOwner);
      lvRequest.FContext := pvContext;
      lvRequest.FAcceptorMgr := self;
      FList.Add(lvRequest);
      lvRequest.PostRequest;

      if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        InterlockedIncrement(FOwner.FDataMoniter.FPostWSAAcceptExCounter);
      end;
    end;
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpAcceptorMgr.Create(AOwner: TDiocpCustom);
begin
  inherited Create;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'acceptorLocker';
  FMaxRequest := 200;
  FMinRequest := 10;  
  FList := TList.Create;
  FOwner := AOwner;
end;

destructor TIocpAcceptorMgr.Destroy;
begin
  FList.Free;
  FLocker.Free;
  inherited Destroy;
end;

procedure TIocpAcceptorMgr.releaseRequestObject(
  pvRequest: TIocpAcceptExRequest);
begin
  pvRequest.Free; 
end;

procedure TIocpAcceptorMgr.removeRequestObject(pvRequest: TIocpAcceptExRequest);
begin
  FLocker.lock;
  try
    FList.Remove(pvRequest);
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpAcceptExRequest.Create(AOwner: TDiocpCustom);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TIocpAcceptExRequest.getPeerINfo;
var
  localAddr: PSockAddr;
  remoteAddr: PSockAddr;
  localAddrSize : Integer;
  remoteAddrSize : Integer;
begin
  localAddrSize := SizeOf(TSockAddr) + 16;
  remoteAddrSize := SizeOf(TSockAddr) + 16;
  IocpGetAcceptExSockaddrs(@FAcceptBuffer[0],
                        0,
                        SizeOf(localAddr^) + 16,
                        SizeOf(remoteAddr^) + 16,
                        localAddr,
                        localAddrSize,
                        remoteAddr,
                        remoteAddrSize);

  FContext.FRemoteAddr := string(inet_ntoa(TSockAddrIn(remoteAddr^).sin_addr));
  FContext.FRemotePort := ntohs(TSockAddrIn(remoteAddr^).sin_port);
end;

procedure TIocpAcceptExRequest.HandleResponse;
begin
  if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FOwner.FDataMoniter.FResponseWSAAcceptExCounter);
  end;

  try
    if ErrorCode = 0 then
    begin
      // msdn
      // The socket sAcceptSocket does not inherit the properties of the socket
      //  associated with sListenSocket parameter until SO_UPDATE_ACCEPT_CONTEXT
      //  is set on the socket.
      FAcceptorMgr.FListenSocket.UpdateAcceptContext(FContext.FRawSocket.SocketHandle);

      getPeerINfo();
    end;
    if Assigned(FOnAcceptedEx) then FOnAcceptedEx(Self);
  finally
    FAcceptorMgr.releaseRequestObject(Self);
  end;
end;

function TIocpAcceptExRequest.PostRequest: Boolean;
var
  dwBytes: Cardinal;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:POverlapped;
begin
  FContext.FRawSocket.createTcpOverlappedSocket;
  dwBytes := 0;
  lp := @FOverlapped;
  lvRet := IocpAcceptEx(FAcceptorMgr.FListenSocket.SocketHandle
                , FContext.FRawSocket.SocketHandle
                , @FAcceptBuffer[0]
                , 0
                , SizeOf(TSockAddrIn) + 16
                , SizeOf(TSockAddrIn) + 16
                , dwBytes
                , lp);
  if not lvRet then
  begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then
    begin
      FOwner.DoClientContextError(FContext, lvErrCode);
    end;
  end else
  begin
    Result := True;
  end;
end;

constructor TIocpRecvRequest.Create;
begin
  inherited Create;
end;

destructor TIocpRecvRequest.Destroy;
begin
  if FInnerBuffer.len > 0 then
  begin
    FreeMem(FInnerBuffer.buf, FInnerBuffer.len);
  end;
  inherited Destroy;
end;

procedure TIocpRecvRequest.CheckCreateRecvBuffer;
begin
  if FInnerBuffer.len <> FOwner.FWSARecvBufferSize then
  begin
    if FInnerBuffer.len > 0 then FreeMem(FInnerBuffer.buf);
    FInnerBuffer.len := FOwner.FWSARecvBufferSize;
    GetMem(FInnerBuffer.buf, FInnerBuffer.len);
  end;
end;

procedure TIocpRecvRequest.HandleResponse;
var
  lvFlag:Byte;
begin
  {$IFDEF DIOCP_DEBUG}
  InterlockedDecrement(FOverlapped.refCount);
  if FOverlapped.refCount <> 0 then
    Assert(FOverlapped.refCount <>0);
  Assert(FOwner <> nil);
  {$ENDIF}

  try
    lvFlag := 0;
    if (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incResponseWSARecvCounter;
      FOwner.FDataMoniter.incRecvdSize(FBytesTransferred);
    end;

    if not FOwner.Active then
    begin
      {$IFDEF DIOCP_DEBUG}
        FOwner.logMessage(
          Format(strRecvEngineOff, [FContext.FSocketHandle])
        );
      {$ENDIF}
      // avoid postWSARecv
      FContext.RequestDisconnect(
        Format(strRecvEngineOff, [FContext.FSocketHandle])
        , Self);
    end else if ErrorCode <> 0 then
    begin
      {$IFDEF DIOCP_DEBUG}
      FOwner.logMessage(
        Format(strRecvError, [FContext.FSocketHandle, FErrorCode])
        );
      {$ENDIF}
      FContext.DoError(ErrorCode);
      FContext.RequestDisconnect(
        Format(strRecvError, [FContext.FSocketHandle, FErrorCode])
        ,  Self);
    end else if (FBytesTransferred = 0) then
    begin      // no data recvd, socket is break
      {$IFDEF DIOCP_DEBUG}
      FOwner.logMessage(strRecvZero,  [FContext.FSocketHandle]);
      {$ENDIF}
      FContext.RequestDisconnect(
        Format(strRecvZero,  [FContext.FSocketHandle]),  Self);
    end else
    begin
      lvFlag := 1;
      FContext.DoReceiveData(Self);
    end;
  finally
    InterlockedDecrement(FContext.FPostWsRecvingCnt);
    
    if lvFlag = 1 then     // �޴���
    begin
      FContext.PostWSARecvRequest;
    end;

//   ResponseDone dec
//    // ���ټ��������ܻᴥ���ر��¼�
//    FContext.DecReferenceCounter(
//      Format('debugInfo:%s, refcount:%d, TIocpRecvRequest.WSARecvRequest.HandleResponse',
//        [FDebugInfo,FOverlapped.refCount]), Self);


  end;
end;

function TIocpRecvRequest.PostRequest(pvBuffer: PAnsiChar;
  len: Cardinal): Boolean;
var
  lvRet:Integer;
  lpNumberOfBytesRecvd: Cardinal;
  lvDataMonitor:TIocpDataMonitor;
  lvOwner:TDiocpCustom;
begin
  Result := false;

  lpNumberOfBytesRecvd := 0;
  FRecvdFlag := 0;

  FRecvBuffer.buf := pvBuffer;
  FRecvBuffer.len := len;

  lvDataMonitor := nil;
  lvOwner := FOwner;
  // ������󵽱��ر����������������Ӧ�ɹ���(Self)�ͷ�
  if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
  begin
    lvDataMonitor := FOwner.FDataMoniter;
  end;
  
  {$IFDEF DIOCP_DEBUG}
  InterlockedIncrement(FOverlapped.refCount);
  FDebugInfo := IntToStr(intPtr(FContext));
  {$ENDIF}


  lvRet := diocp_winapi_winsock2.WSARecv(FContext.FRawSocket.SocketHandle,
     @FRecvBuffer,
     1,
     lpNumberOfBytesRecvd,
     FRecvdFlag,
     LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
     nil
     );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
      {$IFDEF DIOCP_DEBUG}
      lvOwner.logMessage(strRecvPostError, [FContext.SocketHandle, lvRet]);
      InterlockedDecrement(FOverlapped.refCount);
      {$ENDIF}
      // trigger error event
      lvOwner.DoClientContextError(FContext, lvRet);
    end else
    begin
      if lvDataMonitor <> nil then
      begin
        lvDataMonitor.incPostWSARecvCounter;
      end;
    end;
  end else
  begin
    Result := True;    
     if lvDataMonitor <> nil then
    begin
      lvDataMonitor.incPostWSARecvCounter;
    end;
  end;

end;

function TIocpRecvRequest.PostRequest: Boolean;
begin
  self.CheckCreateRecvBuffer;
  Result := PostRequest(FInnerBuffer.buf, FInnerBuffer.len);
end;

procedure TIocpRecvRequest.ReleaseBack;
begin
  FOwner.ReleaseRecvRequest(Self);
end;

procedure TIocpRecvRequest.ResponseDone;
var
  lvContext:TDiocpCustomContext;
begin
  inherited;
  if FOwner = nil then
  begin
    if IsDebugMode then
    begin
      Assert(FOwner <> nil);
      Assert(Self.FAlive);
    end;
  end else
  begin
    // fclientcontext is nil
    lvContext := FContext;
    try
      FOwner.ReleaseRecvRequest(Self);
    finally
      {$IFDEF DIOCP_DEBUG}
      lvContext.DecReferenceCounter('TIocpRecvRequest.WSARecvRequest.Response Done', Self);
      {$ELSE}
      lvContext.DecReferenceCounter(STRING_EMPTY, Self);
      {$ENDIF}
    end;
  end;
  {$IFDEF DIOCP_DEBUG}
  SetCurrentThreadInfo('TIocpRecvRequest.ResponseDone');
  {$ENDIF}
end;

function TIocpSendRequest.ExecuteSend: Boolean;
begin
  if (FBuf = nil) or (FLen = 0) then
  begin
    {$IFDEF DIOCP_DEBUG}
     FOwner.logMessage(
       Format(strSendZero, [FContext.FSocketHandle])
       );
    {$ENDIF}
    Result := False;
  end else
  begin
    Result := InnerPostRequest(FBuf, FLen);
  end;
end;

constructor TIocpSendRequest.Create;
begin
  inherited Create;
end;

destructor TIocpSendRequest.Destroy;
begin
  CheckClearSendBuffer;
  inherited Destroy;
end;

procedure TIocpSendRequest.CheckClearSendBuffer;
begin
  if FLen > 0 then
  begin
    case FSendBufferReleaseType of
      dtDispose: Dispose(FBuf);
      dtFreeMem: FreeMem(FBuf);
    end;
  end;
  FSendBufferReleaseType := dtNone;
  FLen := 0;
end;

procedure TIocpSendRequest.DoCleanUp;
begin
  CheckClearSendBuffer;
  FBytesSize := 0;
  FNext := nil;
  FOwner := nil;
  FContext := nil;
  FBuf := nil;
  FLen := 0;
  FDirectPost := False;
end;

procedure TIocpSendRequest.HandleResponse;
var
  lvContext:TDiocpCustomContext;
begin
  lvContext := FContext;
  FIsBusying := false;
  try
    Assert(FOwner<> nil);
    if (FOwner.FDataMoniter <> nil) then
    begin                                                       
      FOwner.FDataMoniter.incSentSize(FBytesTransferred);
      FOwner.FDataMoniter.incResponseWSASendCounter;
    end;

    lvContext.DoSendRequestRespnonse(Self);


    if not FOwner.Active then
    begin
      {$IFDEF DIOCP_DEBUG}
      FOwner.logMessage(
          Format(strSendEngineOff, [lvContext.FSocketHandle])
          );
      {$ENDIF}
      // avoid postWSARecv
      lvContext.RequestDisconnect(
        Format(strSendEngineOff, [lvContext.FSocketHandle])
        , Self);
    end else if FErrorCode <> 0 then
    begin
      {$IFDEF DIOCP_DEBUG}
      FOwner.logMessage(
          Format(strSendErr, [lvContext.FSocketHandle, FErrorCode])
          );
      {$ENDIF}
      FOwner.DoClientContextError(lvContext, FErrorCode);
      lvContext.RequestDisconnect(
         Format(strSendErr, [lvContext.FSocketHandle, FErrorCode])
          , Self);

//      FOwner.DoClientContextError(lvContext, ErrorCode);
//
//      lvContext.RequestDisconnect(Format('TIocpSendRequest.HandleResponse FErrorCode:%d',  [FErrorCode]), Self);
    end else
    begin
      
      lvContext.FLastActivity := GetTickCount;
      // succ
      if FOwner.FDataMoniter <> nil then
      begin
        FOwner.FDataMoniter.incResponseSendObjectCounter;
      end;
      Inc(lvContext.FSendBytesSize, FBytesTransferred);
      if Assigned(FOnDataRequestCompleted) then
      begin
        FOnDataRequestCompleted(lvContext, Self);
      end;

      lvContext.DoSendRequestCompleted(Self);

      if not self.DirectPost then
      begin
        lvContext.PostNextSendRequest;
      end;
    end;
  finally
    // maybe release context
    lvContext.decReferenceCounter('TIocpSendRequest.WSASendRequest.Response', Self);
  end;
end;

function TIocpSendRequest.InnerPostRequest(buf: Pointer; len: Cardinal):
    Boolean;
var
  lvErrorCode, lvRet: Integer;
  dwFlag: Cardinal;
  lpNumberOfBytesSent:Cardinal;
  lvContext:TDiocpCustomContext;
  lvOwner:TDiocpCustom;
begin
  Result := false;
  FIsBusying := True;
  FBytesSize := len;
  FWSABuf.buf := buf;
  FWSABuf.len := len;
  dwFlag := 0;
  lvErrorCode := 0;
  lpNumberOfBytesSent := 0;

  // maybe on HandleResonse and release self
  lvOwner := FOwner;  
  lvContext := FContext;
  if lvContext.incReferenceCounter('InnerPostRequest::WSASend_Start', self) then
  try
    {$IFDEF DIOCP_DEBUG}
    if lvContext.FSendQueueSize > 0 then
    begin
      Assert(lvContext.FSendQueueSize > 0, '���Ͷ��߳�');
    end;
    {$ENDIF}
    
    {$IFDEF TRACE_IOCP_SEND}
    TByteTools.AppendBufToFile(buf, len, Format('%d_%s.send', [lvContext.SocketHandle, FormatDateTime('hhnn', Now())]));
    {$ENDIF}
    AtomicIncrement(lvContext.FSendQueueSize);
    lvRet := WSASend(lvContext.FRawSocket.SocketHandle,
                      @FWSABuf,
                      1,
                      lpNumberOfBytesSent,
                      dwFlag,
                      LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
                      nil
    );
    if lvRet = SOCKET_ERROR then
    begin
      lvErrorCode := WSAGetLastError;
      Result := lvErrorCode = WSA_IO_PENDING;
      if not Result then
      begin
       AtomicDecrement(lvContext.FSendQueueSize);
       FIsBusying := False;
       {$IFDEF DIOCP_DEBUG}
       lvOwner.logMessage(
         Format(strSendPostError, [lvContext.FSocketHandle, lvErrorCode])
         );
       {$ENDIF}
        /// request kick out
       lvContext.RequestDisconnect(
          Format(strSendPostError, [lvContext.FSocketHandle, lvErrorCode])
          , Self);

      end else
      begin      // maybe on HandleResonse and release self
        if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then
        begin
          lvOwner.FDataMoniter.incPostWSASendSize(len);
          lvOwner.FDataMoniter.incPostWSASendCounter;
        end;
      end;
    end else
    begin       // maybe on HandleResonse and release self
      Result := True;
      if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then
      begin
        lvOwner.FDataMoniter.incPostWSASendSize(len);
        lvOwner.FDataMoniter.incPostWSASendCounter;
      end;
    end;
  finally
    if not Result then
    begin      // post fail, dec ref, if post succ, response dec ref
      if IsDebugMode then
      begin
        Assert(lvContext = FContext);
      end;
      lvContext.decReferenceCounter(
        Format('InnerPostRequest::WSASend_Fail, ErrorCode:%d', [lvErrorCode])
         , Self);

    end;

    // if result is true, maybe on HandleResponse dispose and push back to pool

  end;
end;

procedure TIocpSendRequest.setBuffer(buf: Pointer; len: Cardinal; pvCopyBuf:
    Boolean = true);
var
  lvBuf: PAnsiChar;
begin
  if pvCopyBuf then
  begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    SetBuffer(lvBuf, len, dtFreeMem);
  end else
  begin
    SetBuffer(buf, len, dtNone);
  end;
end;

function TIocpSendRequest.GetStateINfo: String;
begin
  Result :=Format('%s %s', [Self.ClassName, self.Remark]);
  if FResponding then
  begin
    Result :=Result + sLineBreak + Format('start:%s, datalen:%d',
      [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime), FWSABuf.len]);
  end else
  begin
    Result :=Result + sLineBreak + Format('start:%s, end:%s, datalen:%d',
      [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime),
        FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondEndTime),
        FWSABuf.len]);
  end;
end;

procedure TIocpSendRequest.ResponseDone;
begin
  inherited;
  if FOwner = nil then
  begin
    if IsDebugMode then
    begin
      Assert(FOwner <> nil);
      Assert(Self.FAlive);
    end;
  end else
  begin
    FOwner.ReleaseSendRequest(Self);
  end;
end;

procedure TIocpSendRequest.SetBuffer(buf: Pointer; len: Cardinal;
    pvBufReleaseType: TDataReleaseType);
begin
  CheckClearSendBuffer;
  FBuf := buf;
  FLen := len;
  FSendBufferReleaseType := pvBufReleaseType;
end;

procedure TIocpSendRequest.UnBindingSendBuffer;
begin
  FBuf := nil;
  FLen := 0;
  FSendBufferReleaseType := dtNone;
end;

procedure TIocpDataMonitor.Clear;
begin
  FLocker.Enter;
  try
    FSentSize:=0;
    FRecvSize:=0;
    FPostWSASendSize:=0;

    FPostWSASendCounter:=0;
    FResponseWSASendCounter:=0;

    FPostWSARecvCounter:=0;
    FResponseWSARecvCounter:=0;

    FPushSendQueueCounter := 0;
    FResponseSendObjectCounter := 0;
    
    FPostWSAAcceptExCounter:=0;
    FResponseWSAAcceptExCounter:=0;
  finally
    FLocker.Leave;
  end;
end;

constructor TIocpDataMonitor.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
  FMaxOnlineCount := 0;
end;

destructor TIocpDataMonitor.Destroy;
begin
  FLocker.Free;
  inherited Destroy;
end;

procedure TIocpDataMonitor.CalcuMaxOnlineCount(pvOnlineCount: Integer);
begin
  if pvOnlineCount > FMaxOnlineCount then FMaxOnlineCount := pvOnlineCount;
end;

procedure TIocpDataMonitor.IncContextCreateCounter;
begin
  InterlockedIncrement(FContextCreateCounter);
end;

procedure TIocpDataMonitor.IncDisconnectCounter;
begin
  InterlockedIncrement(FDisconnectCounter);
end;

procedure TIocpDataMonitor.incPushSendQueueCounter;
begin
  InterlockedIncrement(FPushSendQueueCounter);
end;

procedure TIocpDataMonitor.incPostSendObjectCounter;
begin
  InterlockedIncrement(FPostSendObjectCounter);
end;

procedure TIocpDataMonitor.incPostWSARecvCounter;
begin
  InterlockedIncrement(FPostWSARecvCounter);
end;

procedure TIocpDataMonitor.incPostWSASendCounter;
begin
  InterlockedIncrement(FPostWSASendCounter);
end;

procedure TIocpDataMonitor.incPostWSASendSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  try
    FPostWSASendSize := FPostWSASendSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TIocpDataMonitor.incRecvdSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  try
    FRecvSize := FRecvSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TIocpDataMonitor.IncRecvRequestCreateCounter;
begin
  InterlockedIncrement(FRecvRequestCreateCounter);
end;

procedure TIocpDataMonitor.IncRecvRequestOutCounter;
begin
  InterlockedIncrement(FRecvRequestOutCounter);
end;

procedure TIocpDataMonitor.IncRecvRequestReturnCounter;
begin
  InterlockedIncrement(FRecvRequestReturnCounter);
end;

procedure TIocpDataMonitor.IncContextOutCounter;
begin
  InterlockedIncrement(FContextOutCounter);

end;

procedure TIocpDataMonitor.IncContextReturnCounter;
begin
  InterlockedIncrement(FContextReturnCounter);
end;

procedure TIocpDataMonitor.incResponseSendObjectCounter;
begin
  InterlockedIncrement(FResponseSendObjectCounter);
end;

procedure TIocpDataMonitor.incResponseWSARecvCounter;
begin
  InterlockedIncrement(FResponseWSARecvCounter);
end;

procedure TIocpDataMonitor.incResponseWSASendCounter;
begin
  InterlockedIncrement(FResponseWSASendCounter);
end;


procedure TIocpDataMonitor.IncSendRequestCreateCounter;
begin
  InterlockedIncrement(FSendRequestCreateCounter);
end;

procedure TIocpDataMonitor.IncSendRequestOutCounter;
begin
  InterlockedIncrement(FSendRequestOutCounter);
end;

procedure TIocpDataMonitor.IncSendRequestReturnCounter;
begin
  InterlockedIncrement(FSendRequestReturnCounter);

end;

procedure TIocpDataMonitor.incSentSize(pvSize:Cardinal);
begin
  FLocker.Enter;
  try
    FSentSize := FSentSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TIocpDataMonitor.SpeedCalcuEnd;
var
  lvTick:Cardinal;
  lvSec:Double;
begin
  if FLastSpeedTick = 0 then exit;

  lvTick := tick_diff(FLastSpeedTick, GetTickCount);
  if lvTick = 0 then Exit;

  lvSec := (lvTick / 1000.000);
  if lvSec = 0 then Exit;

  FSpeed_WSASendResponse := Trunc((FResponseWSASendCounter - FLastSpeed_WSASendResponse) / lvSec);


  FSpeed_WSARecvResponse := Trunc((self.FResponseWSARecvCounter - FLastSpeed_WSARecvResponse) / lvSec);
  FSpeed_Recv := Trunc((self.FRecvSize - FLastSpeed_RecvSize) / lvSec);
end;

procedure TIocpDataMonitor.SpeedCalcuStart;
begin
  FLastSpeedTick := GetTickCount;
  FLastSpeed_WSASendResponse := FResponseWSASendCounter;
  FLastSpeed_WSARecvResponse := FResponseWSARecvCounter;
  FLastSpeed_RecvSize := FRecvSize;
end;

procedure TContextDoublyLinked.add(pvContext: TDiocpCustomContext);
begin
  FLocker.lock;
  try
    if FHead = nil then
    begin
      FHead := pvContext;
    end else
    begin
      if FTail = nil then
      begin
        FCount := FCount;
      end;
      FTail.FNext := pvContext;
      pvContext.FPre := FTail;
    end;

    FTail := pvContext;
    FTail.FNext := nil;

    if FTail = nil then
    begin
      FCount := FCount;
    end;

    inc(FCount);
  finally
    FLocker.unLock;
  end;
end;

constructor TContextDoublyLinked.Create;
begin
  inherited Create;
  FCount := 0;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'onlineContext';
  FHead := nil;
  FTail := nil;
end;

destructor TContextDoublyLinked.Destroy;
begin
  FreeAndNil(FLocker);
  inherited Destroy;
end;

function TContextDoublyLinked.Pop: TDiocpCustomContext;
begin
  FLocker.lock;
  try
    Result := FHead;
    if FHead <> nil then
    begin
      FHead := FHead.FNext;
      if FHead = nil then FTail := nil;
      Dec(FCount);
      Result.FPre := nil;
      Result.FNext := nil;  
    end;  
  finally
    FLocker.unLock;
  end;
end;

function TContextDoublyLinked.remove(pvContext:TDiocpCustomContext): Boolean;
begin

  Result := false;
  FLocker.lock;
  try
    if pvContext.FPre <> nil then
    begin
      pvContext.FPre.FNext := pvContext.FNext;
      if pvContext.FNext <> nil then
        pvContext.FNext.FPre := pvContext.FPre;
    end else if pvContext.FNext <> nil then
    begin    // pre is nil, pvContext is FHead
      pvContext.FNext.FPre := nil;
      FHead := pvContext.FNext;
    end else
    begin   // pre and next is nil
      if pvContext = FHead then
      begin
        FHead := nil;
      end else
      begin
        exit;
      end;
    end;
    Dec(FCount);

    if FCount < 0 then
    begin
      Assert(FCount > 0);
    end;

    //  set pvConext.FPre is FTail
    if FTail = pvContext then
      FTail := pvContext.FPre;

    if FTail = nil then
    begin
      FCount := FCount;
      FTail := nil;
    end;

    if FHead = nil then
    begin
      FCount := FCount;
      FHead := nil;
    end;

    pvContext.FPre := nil;
    pvContext.FNext := nil;
    Result := true;
  finally
    FLocker.unLock;
  end;
end;

procedure TContextDoublyLinked.write2List(pvList: TList);
var
  lvItem:TDiocpCustomContext;
begin
  FLocker.lock;
  try
    lvItem := FHead;
    while lvItem <> nil do
    begin
      pvList.Add(lvItem);
      lvItem := lvItem.FNext;
    end;
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpConnectExRequest.Create(AContext: TDiocpCustomContext);
begin
  inherited Create;
  FContext := AContext;
end;

destructor TIocpConnectExRequest.Destroy;
begin
  inherited Destroy;
end;

function TIocpConnectExRequest.PostRequest(pvHost: string; pvPort: Integer):
    Boolean;
var
  lvSockAddrIn:TSockAddrIn;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:Pointer;
  lvRemoteIP:String;
begin
  {$IFDEF DIOCP_DEBUG}
  self.Remark := Format('��������%s(%d)', [pvHost, pvPort]);
  {$ENDIF}

  try
    lvRemoteIP := FContext.RawSocket.GetIpAddrByName(pvHost);
  except
    lvRemoteIP := pvHost;
  end;

  FContext.setSocketState(ssConnecting);
  lvSockAddrIn := GetSocketAddr(lvRemoteIP, pvPort);

  FContext.RawSocket.bind('0.0.0.0', 0);

  lp :=@FOverlapped;
  lvRet := IocpConnectEx(FContext.RawSocket.SocketHandle,
        @lvSockAddrIn
        , sizeOf(lvSockAddrIn)
        , nil
        , 0
        , FBytesSent
        , lp
        );
  if not lvRet then
  begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then
    begin
      FContext.DoError(lvErrCode);
      FContext.RequestDisconnect('TIocpConnectExRequest.PostRequest');
    end;
  end else
  begin
    Result := True;
  end;

end;

{$IFDEF DIOCP_DEBUG}
procedure TDiocpCustomContext.AddDebugStrings(const pvDebugInfo: String;
    pvAddTimePre: Boolean = true);
var
  s:string;
begin
  if pvAddTimePre then s := Format('[%s]:%s', [NowString, pvDebugInfo])
  else s := pvDebugInfo;
  SpinLock(FDebugLocker);
  try
    InnerAddToDebugStrings(s);
  finally
    SpinUnLock(FDebugLocker);
  end;
end;

procedure TDiocpCustomContext.SetDebugInfo(const Value: string);
begin
  SpinLock(FDebugLocker);
  try
    FDebugInfo := Value;
  finally
    SpinUnLock(FDebugLocker);
  end;
end;

{$ENDIF}

procedure TDiocpCustomContext.AddRefernece;
var
  lvDebugInfo:STring;
begin
  DoCtxStateLock({$IFDEF DIOCP_DEBUG}'AddRefernece:'{$ENDIF});
  {$IFDEF DIOCP_DEBUG}
  lvDebugInfo := Format('*(%d):[%d]->AddRefernece', [self.FReferenceCounter, self.SocketHandle]);
  AddDebugStrings(lvDebugInfo);
  {$ENDIF}
  Inc(FReferenceCounter);
  DoCtxStateUnLock;
end;

function TDiocpCustomContext.CheckActivityTimeOut(pvTimeOut:Integer): Boolean;
begin
  Result := (tick_diff(FLastActivity, GetTickCount) > Cardinal(pvTimeOut));
end;

function TDiocpCustomContext.CheckActivityTimeOutEx(pvTimeOut:Integer): Boolean;
begin
  Result := (FLastActivity <> 0) and (tick_diff(FLastActivity, GetTickCount) > Cardinal(pvTimeOut));
end;

function TDiocpCustomContext.CheckCloseContext: Boolean;
var
  v:Boolean;
begin
  DoCtxStateLock({$IFDEF DIOCP_DEBUG}'CheckCloseContext:'{$ENDIF});
  v := (FCtxStateFlag = CTX_STATE_CONNECTED) and (FReferenceCounter = 0);
  if v then
  begin
    FCtxStateFlag := CTX_STATE_WAITFOR_CLOSE;     // �ȴ��ر�
  end;
  DoCtxStateUnLock;

  if v then
  begin
    InnerCloseContext(False);
    Result := true;
  end else
  begin
    Result := False;
  end;
end;

procedure TDiocpCustomContext.CheckKickOut(pvTimeOut:Integer);
begin
  if CheckActivityTimeOut(pvTimeOut) then
  begin
    self.RequestDisconnect('��ʱ�����Ͽ�');
  end;
end;

{$IFDEF DIOCP_DEBUG}
procedure TDiocpCustomContext.CheckThreadIn(const pvDebugInfo: String);
begin
  if FCheckThreadId <> 0 then
  begin
    //s := GetDebugString;
    raise Exception.CreateFmt('��ǰ�߳�(%d:%s),��������Ѿ��������߳�(%d: %s)����ʹ��',
       [utils_strings.GetCurrentThreadID, pvDebugInfo, FCheckThreadId, FCheckThreadInDebugInfo]);
  end;
  FCheckThreadId := utils_strings.GetCurrentThreadID;
  FCheckThreadInDebugInfo := pvDebugInfo;
end;

procedure TDiocpCustomContext.CheckThreadOut;
begin
  FCheckThreadInDebugInfo := STRING_EMPTY;
  FCheckThreadId := 0;
end;
{$ENDIF}

procedure TDiocpCustomContext.DecRefernece;
begin
  DoCtxStateLock({$IFDEF DIOCP_DEBUG}'DecRefernece:'{$ENDIF});
  Dec(FReferenceCounter);
  DoCtxStateUnLock;
end;

procedure TDiocpCustomContext.DoSendRequestRespnonse(pvRequest:
    TIocpSendRequest);
begin
  self.FCheckStartFlag := 0;
  FLastActivity := GetTickCount;
  AtomicDecrement(FSendQueueSize);
end;

{$IFDEF DIOCP_DEBUG}
function TDiocpCustomContext.GetDebugInfo: string;
begin
  FContextLocker.lock();
  try
    if Length(FDebugInfo) > 0 then
      Result := Copy(FDebugInfo, 0, Length(FDebugInfo))
    else
      Result := '';
  finally
    FContextLocker.unLock;
  end;
end;

function TDiocpCustomContext.GetDebugStrings: String;
begin
  SpinLock(FDebugLocker);
  try
    Result := FDebugStrings.Text;
  finally
    SpinUnLock(FDebugLocker);
  end;
end;

procedure TDiocpCustomContext.InnerAddToDebugStrings(const pvMsg: string; const
    args: array of const);
begin
  InnerAddToDebugStrings(Format(pvMsg, args));
end;

procedure TDiocpCustomContext.InnerAddToDebugStrings(pvMsg:String);
begin
  FDebugStrings.Add(pvMsg);
  if FDebugStrings.Count > 50 then FDebugStrings.Delete(0);
end;
{$ENDIF}

function TDiocpCustomContext.InnerPostSendRequestAndCheckStart(
    pvSendRequest:TIocpSendRequest): Boolean;
var
  lvStart:Boolean;
begin
  lvStart := false;
  FContextLocker.lock();
  try
    Result := FSendRequestLink.Push(pvSendRequest);
    if Result then
    begin
      if not FSending then
      begin
        FSending := true;
        lvStart := true;  // start send work
      end;
    end;
  finally
    FContextLocker.UnLock;
  end;

  {$IFDEF DIOCP_DEBUG}
  if not Result then
  begin
    FOwner.logMessage(
      strSendPushFail, [FSocketHandle, FSendRequestLink.Count, FSendRequestLink.MaxSize]);
  end;
  {$ENDIF}

  if lvStart then
  begin      // start send work
    if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incPushSendQueueCounter;
    end;
    CheckNextSendRequest;
    FCheckStartFlag := 1;
  end;
end;

procedure TDiocpCustomContext.InnerPostSendRequest(lvRequest:TIocpSendRequest);
begin
  //FcurrSendRequest := lvRequest;
  if lvRequest.ExecuteSend then
  begin
    if (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incPostSendObjectCounter;
    end;
  end else
  begin
    //FcurrSendRequest := nil;

    /// cancel request
    lvRequest.CancelRequest;

    {$IFDEF DIOCP_DEBUG}
    FOwner.logMessage('[%d]TIocpClientContext.CheckNextSendRequest.ExecuteSend return false',
       [self.SocketHandle]);
    {$ENDIF}
    /// kick out the clientContext
    RequestDisconnect('CheckNextSendRequest::lvRequest.checkSendNextBlock Fail', lvRequest);

    // ���ͷ�Buff
    FOwner.ReleaseSendRequest(lvRequest);
  end;
end;



function TDiocpCustomContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
    pvBufReleaseType: TDataReleaseType; pvTag: Integer = 0; pvTagData: Pointer
    = nil): Boolean;
var
  lvRequest:TIocpSendRequest;
  lvErrStr :String;
begin
  Result := false;
  if len = 0 then raise Exception.Create('PostWSASendRequest::request buf is zero!');
  if self.Active then
  begin
    if self.IncReferenceCounter('PostWSASendRequest', Self) then
    begin
      try
        lvRequest := GetSendRequest;
        lvRequest.SetBuffer(buf, len, pvBufReleaseType);
        lvRequest.Tag := pvTag;
        lvRequest.Data := pvTagData;
        if FDirectPostRequest then
        begin
          lvRequest.DirectPost := true;
          Result := True;
          InnerPostSendRequest(lvRequest);
        end else
        begin
          Result := InnerPostSendRequestAndCheckStart(lvRequest);
          if not Result then
          begin
            /// Push Fail unbinding buf
            lvRequest.UnBindingSendBuffer;

            lvErrStr := Format(strSendPushFail, [SocketHandle,
              FSendRequestLink.Count, FSendRequestLink.MaxSize]);
            Self.RequestDisconnect(lvErrStr,
              lvRequest);


            lvRequest.CancelRequest;
            FOwner.ReleaseSendRequest(lvRequest);
          end;
        end;
      finally
        self.DecReferenceCounter('PostWSASendRequest', Self);
      end;
    end;
  end;
end;

procedure TDiocpCustomContext.ReleaseBack;
begin
  if Assigned(FReleaseBack) then
  begin    // ֻ��GetFromPool�Ĳ���Ҫ���й黹����
    Assert(FObjectAlive=True, '�����ظ����й黹�ز���');
    Self.FObjectAlive := False;
    FReleaseBack(self);
  end;


end;

procedure TDiocpCustomContext.SetMaxSendingQueueSize(pvSize:Integer);
begin
  FSendRequestLink.setMaxSize(pvSize);
end;

{ TIocpActionRequest }

procedure TIocpActionRequest.HandleResponse;
begin
  inherited;
  
end;

initialization
  __innerLogger := TSafeLogger.Create();
  __innerLogger.setAppender(TLogFileAppender.Create(True));
  __diocp_logger := __innerLogger;

finalization
  if __innerLogger <> nil then
  begin
    __innerLogger.Free;
  end;

end.
