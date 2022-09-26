
(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft

 *   2015-02-22 08:29:43
 *     DIOCP-V5 ����
 *
 *   2015-04-10 18:00:52
 *     ֹͣʱ����ȴ����е�Ͷ�ݵ�AcceptEx����ع�ع���ٽ��йر�IOCP����,(�ᵼ��Ͷ�ݳ�ȥ��AcceptEx�޷��ع�(XP�³���й©))
 *     ��л Xjumping  990669769, ����bug
 *
 *
 *   thanks qsl's suggestion
 *
 *
 *)
 
unit diocp_tcp_server;

{$I 'diocp.inc'}



{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
  //�����־��¼���뿪��
  {$DEFINE WRITE_LOG}
{$ENDIF}

// 
{.$DEFINE MONITOR_SPIN_LOCK}

// ��ϸ��¼������Ϣ
{.$DEFINE TRACE_IOCP_RECV}

// ��ϸ��¼������Ϣ
{.$DEFINE TRACE_IOCP_SEND}

{.$DEFINE DIRECT_SEND}


{$IFDEF DIOCP_HIGH_SPEED}
  {$UNDEF WRITE_LOG}
  {$UNDEF DEBUG_ON}
  {$UNDEF DIOCP_DEBUG}
  {$UNDEF TRACE_IOCP_RECV}
  {$UNDEF TRACE_IOCP_SEND}
{$ENDIF}




interface 

uses
  Classes, diocp_sockets_utils, diocp_core_engine,
  winsock, diocp_winapi_winsock2, diocp_res,

{$if CompilerVersion >= 18}
  types,
{$ifend}

  diocp_core_rawWinSocket, SyncObjs, Windows, SysUtils,
  utils_safeLogger,
  utils_hashs,
  utils_queues, utils_locker, utils_strings, utils_threadinfo
  
  , utils_byteTools
  , utils_BufferPool, utils_grouptask;

const
  SOCKET_HASH_SIZE = $FFFF;
  FREE_FLAG = $01;

  CORE_LOG_FILE = 'diocp_core_exception';
  CORE_DEBUG_FILE = 'diocp_core_debug';

  ADDRESS_LENGTH_EX = 18;

  IP_V4 = 0;
  IP_V6 = 1;

type
  TDiocpTcpServer = class;
  TIocpAcceptorMgr = class;
  TIocpClientContext = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;
  TIocpDisconnectExRequest = class;

  TDataReleaseType = (dtNone, dtFreeMem, dtFreeTagDataAsObject, dtBufReleaseRef, dtDispose);

  TIocpClientContextClass = class of TIocpClientContext;

  TOnContextEvent = procedure(pvClientContext: TIocpClientContext; pvTag:Integer; pvTagData:Pointer) of object;
  
      
  TOnContextError = procedure(pvClientContext: TIocpClientContext; errCode:
      Integer) of object;

  TOnSendRequestResponse = procedure(pvContext:TIocpClientContext;
      pvRequest:TIocpSendRequest) of object;

  TOnDataReceived = procedure(pvClientContext:TIocpClientContext;
      buf:Pointer; len:cardinal; errCode:Integer) of object;

  TOnBufferEvent = procedure(pvContext: TIocpClientContext; pvBuff: Pointer; len:
      Cardinal; pvBufferTag: Integer; pvTagData: Pointer; pvErrorCode: Integer)
      of object;

  TOnContextAcceptEvent = procedure(pvSocket: THandle; pvAddr: String; pvPort:
      Integer; var vAllowAccept: Boolean) of object;

  TContextNotifyEvent = procedure(pvClientContext: TIocpClientContext) of object;

  /// <summary>
  ///   on post request is completed
  /// </summary>
  TOnDataRequestCompleted = procedure(pvClientContext:TIocpClientContext;
      pvRequest:TIocpRequest) of object;


  TDiocpListener = class(TObject)
  private
    FAcceptorMgr: TIocpAcceptorMgr;
    FListenAddress:string;
    FListenPort: Word;
    FIPVersion: Integer;
  public
    constructor Create(AOwnerTcpServer: TDiocpTcpServer);
    destructor Destroy; override;
    procedure Start(pvIocpEngine: TIocpEngine);
    procedure PostAcceptExRequest(pvNum: Integer);
    procedure WaitForCancel(pvTimeOut:Integer);
    procedure Close;
  end;

  /// <summary>
  ///   ������̰߳�ȫ���벻Ҫ���̲߳���һ������
  /// </summary>
  TDiocpListeners = class(TObject)
  private
    FList: TList;
    FOwnerServer:TDiocpTcpServer;
    procedure Clear;
  public
    constructor Create(AOwnerTcpServer: TDiocpTcpServer);
    destructor Destroy; override;


    procedure Bind(const pvBindingAddress: string; const pvPort: Integer;
        pvIPVersion: Integer = IP_V4; pvClientContextClass: TIocpClientContextClass
        = nil);

    procedure Close;

    procedure WaitForCancel(pvTimeOut:Integer);

    procedure ClearObjects;
    procedure PostAcceptExRequest(pvNum: Integer);
    procedure Start(pvIocpEngine: TIocpEngine);

  end;

  /// <summary>
  ///   Զ��������
  ///   ��Ӧ�ͻ��˵�һ������
  /// </summary>
  TIocpClientContext = class(TObject)
  private
    __free_flag:Integer;

    FCloseingFlag:Byte;
    FOnRecvingFlag:Byte;

    FCreateSN:Integer;

    FSendQueueSize:Integer;

    // ������Ҫ����Ķ���
    FRecvQueue: TSafeQueue;

    FProcessFlag: Integer;



    // ����0ʱ���ᱻKickOut
    FBusingCounter: Integer;

    // ��ǰ�����������׽��־��
    FSocketHandle:TSocket;

    FCheckThreadId: THandle;

    // ��󽻻����ݵ�ʱ���
    FLastActivity: Cardinal;

    FDebugStrings:TStrings;

    {$IFDEF SOCKET_REUSE}
    FDisconnectExRequest:TIocpDisconnectExRequest;
    {$ENDIF}

    FSocketState: TSocketState;

    /// <summary>
    ///   �����õļ�����, ��������Ϊ0ʱ���Խ��йر��ͷ�
    /// </summary>
    FReferenceCounter:Integer;

    /// <summary>
    ///   �Ƿ�����رյı�־�����Ϊtrueʱ �����ü�����Ϊ0 ʱ���������Ĺر�����
    /// </summary>
    FRequestDisconnectFlag: Boolean;



    FWSARecvRef:Integer;

    FDebugInfo: string;
    procedure SetDebugInfo(const Value: string);


    procedure DoProcessRecvQueue();



    /// <summary>
    ///   �������ü���
    /// </summary>
    /// <returns>
    ///   �ɹ�����true,
    ///   ʧ�ܷ���false ��ǰ������������ر�
    /// </returns>
    /// <param name="pvDebugInfo"> ���Լ�¼��Ϣ </param>
    /// <param name="pvObj"> ���Լ�¼���� </param>
    function IncReferenceCounter(const pvDebugInfo: string; pvObj: TObject = nil):
        Boolean;

    /// <summary>
    ///  �������ü���
    ///  �����ü����� = 0������رձ�־Ϊtrueʱ������öϿ�����(InnerCloseContext)
    /// </summary>
    function DecReferenceCounter(const pvDebugInfo: string; pvObj: TObject = nil):
        Integer;

    /// <summary>
    ///   �������ü�����������ر�
    ///   �����ü����� = 0ʱ������öϿ�����(InnerCloseContext)
    /// </summary>
    procedure DecReferenceCounterAndRequestDisconnect(const pvDebugInfo: string;
        pvObj: TObject= nil);


  {$IFDEF SOCKET_REUSE}
    /// <summary>
    ///   �׽�������ʱʹ�ã�������ӦDisconnectEx�����¼�
    /// </summary>
    procedure OnDisconnectExResponse(pvObject:TObject);
  {$ENDIF}
  private
    /// <summary>
    ///   ����ԭ��
    /// </summary>
    FDisconnectedReason:String;

    FAlive:Boolean;
    
    

    FInnerLockerFlag: Integer;

    /// ��ʼ����ʱ��
    FWorkerStartTick:Cardinal;

    /// ��������ʱ��
    FWorkerEndTick: Cardinal;

    FContextLocker: TCriticalSection;


    /// <summary>
    ///  ���ڷ��ͱ��
    /// </summary>
    FSending: Boolean;

    FActive: Boolean;


    /// <summary>
    ///   ��������ر�,����Ӧ�κεĽ��������¼�, PostWSACloseʱ����ǰ���øñ�־
    /// </summary>
    FRequestClose: Byte;


    /// <summary>
    ///  ��ǰ���ڷ��͵�����
    /// </summary>
    FCurrSendRequest:TIocpSendRequest;

    FCurrRecvRequest:TIocpRecvRequest;
    
    FData: Pointer;

    /// <summary>
    ///  ���ӵ�DNA��ÿ�����Ӷ������һ���ۼ�
    /// </summary>
    FContextDNA: Integer;

    /// <summary>
    ///   ������������Ĭ������б��СΪ100,�����ڿ�������ǰ��������
    /// </summary>
    FSendRequestLink: TIocpRequestSingleLink;

    FRawSocket: TRawSocket;

    FRemoteAddr: String;
    FRemotePort: Integer;
    FTagStr: String;

    procedure InnerDoRecv(pvRecvRequest: TIocpRecvRequest);


    /// <summary>
    ///   ��Ͷ�ݵĽ���������Ӧ�е��ã��������������¼�
    /// </summary>
    procedure DoReceiveData(pvRecvRequest: TIocpRecvRequest);



    /// <summary>
    ///   post next sendRequest
    ///    must single thread operator
    /// </summary>
    procedure CheckNextSendRequest;

    /// <example>
    ///   �ͷ���Դ
    ///    1.���Ͷ����е�δ���͵�����(TSendRequest), ����÷���ʵ����TSendRequest.CancelRequest
    /// </example>
    procedure CheckReleaseRes;


    procedure SetOwner(const Value: TDiocpTcpServer);
    function GetDebugInfo: string;


    /// <summary>
    ///   �黹�ͷ�
    /// </summary>
    procedure ReleaseBack;

  protected
    FAcceptorMgr:TIocpAcceptorMgr;


    FOwner: TDiocpTcpServer;
    
    /// <summary>
    ///   ���ӵĽ�������ʵ��
    /// </summary>
    /// FRecvRequest:TIocpRecvRequest;

    /// <summary>
    ///   Ͷ�ݽ�������
    /// </summary>
    procedure PostWSARecvRequest();virtual;

    procedure OnAfterCreateSocket();virtual;

    /// <summary>
    ///   called by sendRequest response
    /// </summary>
    procedure DoSendRequestCompleted(pvRequest: TIocpSendRequest); virtual;

    /// <summary>
    ///
    /// </summary>
    function GetSendRequest: TIocpSendRequest;

    /// <summary>
    ///   Give Back
    /// </summary>
    function ReleaseSendRequest(pvObject:TIocpSendRequest): Boolean;

    /// <summary>
    ///  1.post reqeust to sending queue,
    ///    return false if SendQueue Size is greater than maxSize,
    ///
    ///  2.check sending flag, start if sending is false
    /// </summary>
    function InnerPostSendRequestAndCheckStart(pvSendRequest:TIocpSendRequest): Boolean;

    /// <summary>
    ///   ִ�����������ӶϿ������������¼�
    ///   ȷ��:���̵߳���
    /// </summary>
    procedure InnerCloseContext;

    /// <summary>
    ///   Ͷ����ɺ󣬼���Ͷ����һ������,
    ///     ֻ��HandleResponse�е���
    /// </summary>
    procedure PostNextSendRequest; virtual;


    /// <summary>
    ///   Ͷ�ݵķ���������Ӧʱִ�У�һ��Ӧ������ִ�У�Errcode <> 0Ҳ����Ӧ
    /// </summary>
    procedure DoSendRequestRespnonse(pvRequest: TIocpSendRequest); virtual;

    procedure DoOwnerClientContext(pvErrorCode: Integer);

    procedure InnerLock();{$IFDEF HAVE_INLINE} inline;{$ENDIF}
    procedure InnerUnLock();{$IFDEF HAVE_INLINE} inline;{$ENDIF}
    procedure Lock();{$IFDEF HAVE_INLINE} inline;{$ENDIF}
    procedure UnLock;{$IFDEF HAVE_INLINE} inline;{$ENDIF}

    procedure DecRecvRef;
    procedure IncRecvRef;
  protected
    procedure DoConnected;

    procedure DoDisconnected;

    /// <summary>
    ///   �黹����ʱ���е���
    /// </summary>
    procedure DoCleanUp;virtual;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); virtual;

    procedure OnDisconnected; virtual;

    procedure OnConnected; virtual;

    procedure SetSocketState(pvState:TSocketState); virtual;

    procedure DoSendBufferCompleted(pvBuffer: Pointer; len: Cardinal; pvBufferTag:
        Integer; pvTagData: Pointer; pvErrorCode: Integer); virtual;

    procedure RecordWorkerStartTick;
    procedure RecordWorkerEndTick;
  public
    
    procedure AddDebugString(const pvString: string);

    /// <summary>
    ///   ��⵱ǰ���ڹ�������ʱ��
    /// </summary>
    /// <returns>
    ///  ���ú�����, ���û�й����������Ѿ���������)���򷵻�0
    /// </returns>
    function CheckWorkingTick: Cardinal;

    /// <summary>
    ///   ��ȡ��ǰ�����Ͷ����е���������
    /// </summary>
    function GetSendQueueSize: Integer;

    procedure BeginBusy();
    procedure EndBusy;


    constructor Create; virtual;
    destructor Destroy; override;
    procedure CheckThreadIn(const pvDebugInfo: String);
    procedure CheckThreadOut;

    procedure DoDisconnect;

    /// <summary>
    ///   ����Context���ӣ�����رչ黹��Context�����
    ///    �����ɹ�����True, ���򷵻�False(�����Ѿ����Ͽ���������Ͽ�)
    /// </summary>
    function LockContext(const pvDebugInfo: string; pvObj: TObject): Boolean;


    procedure UnLockContext(const pvDebugInfo: string; pvObj: TObject);

    /// <summary>
    ///   Ͷ�ݹر�����
    ///     �ȴ�ǰ������ݷ���������йرպ�Ȼ����жϿ�����
    /// </summary>
    procedure PostWSACloseRequest();

    /// <summary>
    ///   ��ȫ��������: LockContext, Ȼ�����RequestDisconnect
    /// </summary>
    function RequestDisconnect(const pvReason: string = ''; pvObj: TObject = nil):
        Boolean;

    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean =
        true; pvTag: Integer = 0; pvTagData: Pointer = nil): Boolean; overload;

    /// <summary>
    ///    Ͷ�ݷ�������IOCP����
    ///    post send request to iocp queue, if post successful return true.
    ///      if request is completed, will call DoSendRequestCompleted procedure
    ///    ��� ����Ϊ0, ���ڴ�������ʱ���йرա�
    /// </summary>
    /// <returns>
    ///    ���Ͷ�ݳɹ�����true�����򷵻�false(Ͷ�ݶ�������)
    /// </returns>
    /// <param name="buf"> (Pointer) </param>
    /// <param name="len"> (Cardinal) </param>
    /// <param name="pvBufReleaseType"> �ͷ����� </param>
    /// <param name="pvTag"> -1: ���������ر�,����ʱ�ر� </param>
    /// <param name="pvTagData"> (Pointer) </param>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvBufReleaseType:
        TDataReleaseType; pvTag: Integer = 0; pvTagData: Pointer = nil): Boolean;
        overload;



    procedure SetMaxAcceptNum(pvMax:Integer);

    /// <summary>
    ///   ���õ�ǰ�Ľ����߳���Ϣ
    /// </summary>
    procedure SetRecvWorkerHint(const pvHintStr: String); overload;
    procedure SetRecvWorkerHint(const pvFmtMsg: string; const args: array of
        const); overload;

    property Active: Boolean read FActive;
    property BusingCounter: Integer read FBusingCounter;

    property Data: Pointer read FData write FData;

    property DebugInfo: string read GetDebugInfo write SetDebugInfo;


    /// <summary>
    ///  ����ʱ���� +1
    /// </summary>
    property ContextDNA: Integer read FContextDNA;

    /// <summary>
    ///   ����˳��, ����ʱ +1
    /// </summary>
    property CreateSN: Integer read FCreateSN;
    property CurrRecvRequest: TIocpRecvRequest read FCurrRecvRequest;
    property DisconnectedReason: String read FDisconnectedReason;

    /// <summary>
    ///   ��󽻻����ݵ�ʱ���
    /// </summary>
    property LastActivity: Cardinal read FLastActivity;

    property Owner: TDiocpTcpServer read FOwner write SetOwner;

    property RawSocket: TRawSocket read FRawSocket;

    property RemoteAddr: String read FRemoteAddr;

    property RemotePort: Integer read FRemotePort;
    property RequestDisconnectFlag: Boolean read FRequestDisconnectFlag;
    property SendQueueSize: Integer read FSendQueueSize;



    /// <summary>
    ///   ��ע��
    ///   SocketHandle��������Socket����������Ҫ����������Socket������Է���RawSocket.Handle
    /// </summary>
    property SocketHandle: TSocket read FSocketHandle;
    property SocketState: TSocketState read FSocketState;
    property TagStr: String read FTagStr write FTagStr;
  end;



  /// <summary>
  ///   WSARecv io request
  /// </summary>
  TIocpRecvRequest = class(TIocpRequest)
  private
    FAlive:Boolean;
    __debugFlag : Integer;
    FPostCnt:Integer;
    FCounter:Integer;
    FDebugInfo:String;
    FInnerBuffer: diocp_winapi_winsock2.TWsaBuf;
    FRecvBuffer: diocp_winapi_winsock2.TWsaBuf;
    FRecvdFlag: Cardinal;
    FOwner: TDiocpTcpServer;
    FClientContext:TIocpClientContext;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;

    procedure ResponseDone; override;

    procedure DoRelease;override;
  public
    /// <summary>
    ///   post recv request to iocp queue
    /// </summary>
    function PostRecvRequest: Boolean; overload;

    /// <summary>
    ///
    /// </summary>
    function PostRecvRequest(pvBuffer:PAnsiChar; len:Cardinal): Boolean; overload;

    procedure CheckCreateRecvBuffer;

  public
    constructor Create;
    destructor Destroy; override;
  end;


  TIocpSendRequestClass = class of TIocpSendRequest;
  /// <summary>
  ///   WSASend io request
  /// </summary>
  TIocpSendRequest = class(TIocpRequest)
  private

    FLastMsg : String;
    FSendBufferReleaseType: TDataReleaseType;
    
    FMaxSize:Integer;
    
    // for singlelinked
    FNext:TIocpSendRequest;

    FIsBusying:Boolean;

    FAlive: Boolean;

    FBytesSize:Cardinal;

    // send buf record
    FWSABuf:TWsaBuf;


    FBuf:Pointer;
    FLen:Cardinal;

    FOwner: TDiocpTcpServer;

    procedure CheckClearSendBuffer();
    
    function GetWSABuf: PWsaBuf;
 protected
    FClientContext:TIocpClientContext;

    FOnDataRequestCompleted: TOnDataRequestCompleted;

    procedure UnBindingSendBuffer();
  protected
    /// 0:none, 1:succ, 2:completed, 3: has err, 4:owner is off
    FReponseState:Byte;
    
    /// <summary>
    ///   post send
    /// </summary>
    function ExecuteSend: Integer; virtual;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;


    procedure ResponseDone; override;

    /// <summary>
    ///   give back to sendRequest ObjectPool
    /// </summary>
    procedure DoCleanUp;virtual;


    function GetStateINfo: String; override;
    

    /// <summary>
    ///   Ͷ�ݷ��͵����ݵ�IOCP�������(WSASend)
    /// </summary>
    /// <returns>
    ///   ����ʧ�ܷ���False, ������Ͽ�����
    /// </returns>
    /// <param name="buf"> (Pointer) </param>
    /// <param name="len"> (Cardinal) </param>
    function InnerPostRequest(buf: Pointer; len: Cardinal): Boolean;


  public
    constructor Create; virtual;

    destructor Destroy; override;

    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvBufReleaseType: TDataReleaseType); overload;

    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = true); overload;

    property ClientContext: TIocpClientContext read FClientContext;

    property Owner: TDiocpTcpServer read FOwner;

    /// <summary>
    ///   ��ȡ���һ�β�����Buff��Ϣ
    /// </summary>
    property WSABuf: PWsaBuf read GetWSABuf;

    

    

    /// <summary>
    ///   on entire buf send completed
    /// </summary>
    property OnDataRequestCompleted: TOnDataRequestCompleted read FOnDataRequestCompleted write FOnDataRequestCompleted;
  end;

  TIocpDisconnectExRequest = class(TIocpRequest)
  private
    {$IFDEF WRITE_LOG}
    FOwner: TDiocpTcpServer;
    {$ENDIF}

    FContext:TIocpClientContext;

  protected
    function PostRequest: Boolean;

    /// <summary>
    ///   directly post request,
    /// </summary>
    function DirectlyPost: Boolean;
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
    FAcceptBuffer: array [0.. 127] of byte;

    //FAcceptBufferV4: array [0.. (SizeOf(TSockAddrIn) + 16) * 2 - 1] of byte;

    FOwner: TDiocpTcpServer;
    FAcceptorMgr:TIocpAcceptorMgr;

    FClientContext:TIocpClientContext;
    /// <summary>
    ///   get socket peer info on acceptEx reponse
    /// </summary>
    procedure GetPeerINfo;
  protected
    function PostRequest: Boolean;

  protected
    procedure HandleResponse; override;

    procedure ResponseDone; override;

  public
    constructor Create(AOwner: TDiocpTcpServer);
  end;

  /// <summary>
  ///   manager acceptEx request
  /// </summary>
  TIocpAcceptorMgr = class(TObject)
  private
    FFlag:Integer;
    FOwner: TDiocpTcpServer;

    FCount: Integer;

    // AcceptEx
    FAcceptExRequestPool: TBaseQueue;
    // clientContext pool
    FContextPool: TBaseQueue;  

    FListenSocket: TRawSocket;

    function InnerCreateAcceptExRequest: TIocpAcceptExRequest;

    function InnerCreateClientContext: TIocpClientContext;
  protected
    FClientContextClass: TIocpClientContextClass;
    /// <summary>
    ///    ��ȡһ�����Ӷ�������������û�У���ᴴ��һ���µ�ʵ��
    /// </summary>
    function GetClientContext: TIocpClientContext;
    /// <summary>
    ///   �ͷ����Ӷ��󣬹黹�������
    /// </summary>
    function ReleaseClientContext(pvObject:TIocpClientContext): Boolean;

    procedure CheckCreatePoolObjects(pvMaxNum: Integer);
  public
    constructor Create(AOwner: TDiocpTcpServer);

    destructor Destroy; override;

    function GetRequestObject: TIocpAcceptExRequest;

    procedure ReleaseRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure Close;

    /// <summary>
    ///   ����Ƿ���ҪͶ��AcceptEx
    /// </summary>
    procedure PostAcceptExRequest(pvNum: Integer); overload;
    /// <summary>
    ///   ����Ƿ���ҪͶ��AcceptEx
    /// </summary>
    procedure PostAcceptExRequest; overload;
    procedure RegisterContextClass(pvContextClass: TIocpClientContextClass);

    /// <summary>
    ///   �ȴ��������ӹر�
    /// </summary>
    function WaitForCancel(pvTimeOut: Cardinal): Boolean;

    procedure ClearObjects;

    property ListenSocket: TRawSocket read FListenSocket;



  end;

  /// <summary>
  ///   iocp�����ݼ������
  /// </summary>
  TIocpDataMonitor = class(TObject)
  private
    // ��¼��ʼʱ���
    FLastSpeedTick : Cardinal;
    
    // ��¼��ʼʱ���_����
    FLastSpeed_WSASendResponse: Int64;
    FLastSpeed_WSARecvResponse: Int64;

    // �ѷ����ֽ�
    FLastSpeed_WSASentSize    : Int64;
    // �ѽ����ֽ�
    FLastSpeed_RecvSize       : Int64;

    // �ٶ���Ϣ
    FSpeed_WSASendResponse    : Int64;
    FSpeed_WSARecvResponse    : Int64;
    FSpeed_WSASentSize        : Int64;
    FSpeed_WSARecvSize        : Int64;

    // �����������
    FMaxOnlineCount:Integer;

    FDisconnectedCounter: Integer;

    FSentSize:Int64;
    FRecvSize:Int64;
    FPostWSASendSize: Int64;

    FHandleCreateCounter:Integer;
    FHandleDestroyCounter:Integer;

    FContextCreateCounter: Integer;
    FContextOutCounter:Integer;
    FContextReturnCounter:Integer;

    FPushSendQueueCounter: Integer;
    FResponseSendObjectCounter:Integer;

    FSendRequestCreateCounter: Integer;
    FSendRequestOutCounter:Integer;
    FSendRequestReturnCounter:Integer;
    FSendRequestAbortCounter :Integer;

    FRecvRequestCreateCounter: Integer;
    FRecvRequestReturnCounter:Integer;
    FRecvRequestOutCounter:Integer;

    FCtxRecvQueueSize:Integer;

    FPostWSASendCounter:Integer;
    FResponseWSASendCounter:Integer;

    FPostWSARecvCounter:Integer;
    FResponseWSARecvCounter:Integer;

    FAcceptExObjectCounter: Integer;
    FPostWSAAcceptExCounter:Integer;
    FResponseWSAAcceptExCounter:Integer;

    FLocker: TCriticalSection;
    FPostSendObjectCounter: Integer;

    procedure IncSentSize(pvSize:Cardinal);
    procedure IncPostWSASendSize(pvSize:Cardinal);
    procedure IncRecvdSize(pvSize:Cardinal);
    procedure IncCtxRecvQueueSize();
    procedure DecCtxRecvQueueSize();

    procedure IncPostWSASendCounter;
    procedure IncResponseWSASendCounter;

    procedure IncPostWSARecvCounter;
    procedure IncResponseWSARecvCounter;

    procedure IncRecvRequestCreateCounter;
    procedure IncRecvRequestOutCounter;
    procedure IncRecvRequestReturnCounter;

    procedure IncDisconnectedCounter;



    procedure IncAcceptExObjectCounter;

    procedure IncPushSendQueueCounter;
    procedure IncPostSendObjectCounter;
    procedure IncResponseSendObjectCounter;

    {$IFDEF SOCKET_REUSE}
    procedure incHandleCreateCounter;
    procedure incHandleDestroyCounter;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    /// <summary>
    ///  ��ʼͳ���ٶ�
    ///  ��¼��ǰ��Ϣ
    /// </summary>
    procedure SpeedCalcuStart;

    /// <summary>
    ///   ͳ�����ݣ�����ʱ����Ϣ
    /// </summary>
    procedure SpeedCalcuEnd;

    /// <summary>
    ///   ���������������
    /// </summary>
    procedure CalcuMaxOnlineCount(pvOnlineCount:Integer);

    property AcceptExObjectCounter: Integer read FAcceptExObjectCounter;
    property ContextCreateCounter: Integer read FContextCreateCounter;
    property ContextOutCounter: Integer read FContextOutCounter;
    property ContextReturnCounter: Integer read FContextReturnCounter;
    property CtxRecvQueueSize: Integer read FCtxRecvQueueSize;
    property DisconnectedCounter: Integer read FDisconnectedCounter;
    property HandleCreateCounter: Integer read FHandleCreateCounter;
    property HandleDestroyCounter: Integer read FHandleDestroyCounter;
    property Locker: TCriticalSection read FLocker;
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
    property Speed_WSARecvResponse: Int64 read FSpeed_WSARecvResponse;
    property Speed_WSASendResponse: Int64 read FSpeed_WSASendResponse;
    property Speed_WSASentSize: Int64 read FSpeed_WSASentSize;
 
  end;

  {$IF RTLVersion>22}
  // thanks: �����ٷ�19183455
  //  vcl for win64 ��64λƽ̨�£��ؼ�����)
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}

  TDiocpTcpServer = class(TComponent)
  private
    FClosingFlag:Byte;
    FOpeningFlag:Byte;
    
    FListeners: TDiocpListeners;

    // ��������Link
    FRecvBuffLink:PBufferPool;

    FDefaultListener:TDiocpListener;

    FDebugStrings:TStrings;

    FContextDNA : Integer;
    
    FLogger: TSafeLogger;

    FMaxSendingQueueSize:Integer;

    FIsDestroying :Boolean;
    FWSARecvBufferSize: cardinal;
    procedure SetWSARecvBufferSize(const Value: cardinal);

    function IsDestroying: Boolean;
    function LogCanWrite: Boolean;

    function RequestContextDNA:Integer;
    
  protected
    FIocpSendRequestClass:TIocpSendRequestClass;

    procedure SetName(const NewName: TComponentName); override;



    /// <summary>
    ///   �������µ����Ӷ���ʱ����õĺ���
    ///   ��������������һЩ��ʼ��
    /// </summary>
    procedure OnCreateClientContext(const context: TIocpClientContext); virtual;

    /// <summary>
    ///   ��ӵ������б���
    /// </summary>
    procedure AddToOnlineList(pvObject:TIocpClientContext);

    /// <summary>
    ///   �������б����Ƴ�
    /// </summary>
    procedure RemoveFromOnOnlineList(pvObject:TIocpClientContext); virtual;
  private
    // sendRequest pool
    FSendRequestPool: TBaseQueue;

    FRecvRequestPool: TBaseQueue;

    FClientContextClass: TIocpClientContextClass;

    // extend data
    FDataPtr: Pointer;

    /// data record
    FDataMoniter: TIocpDataMonitor;

    FActive: Boolean;
    FConnectedCount: Integer;
    FDefaultListenAddress: String;



    FKeepAlive: Boolean;
    FNoDelayOption: Boolean;

    FOnAfterOpen: TNotifyEvent;
    FOnContextConnected: TContextNotifyEvent;
    FOnContextDisconnected: TContextNotifyEvent;


    FOnDataReceived: TOnDataReceived;


    FOnContextError: TOnContextError;
    FOnContextAccept: TOnContextAcceptEvent;
    FOnSendBufferCompleted: TOnBufferEvent;
    FOnSendRequestResponse: TOnSendRequestResponse;

    FPort: Integer;
    FUseObjectPool: Boolean;

    procedure DoClientContextError(pvClientContext: TIocpClientContext;
        pvErrorCode: Integer);
        
    function GetWorkerCount: Integer;

    procedure SetWorkerCount(const Value: Integer);

    procedure SetActive(pvActive:Boolean);


    procedure DoReceiveData(pvIocpClientContext:TIocpClientContext;
        pvRequest:TIocpRecvRequest);
  private
    FIocpEngine: TIocpEngine;
    FAllowMaxOnlineCount: Integer;
    FKeepAliveTime: Cardinal;
    FOwnerEngine:Boolean;
    FSendBufCacheSize: Integer;
    FUseAsyncRecvQueue: Boolean;
    
    procedure CheckDoDestroyEngine;
    function InnerCreateSendRequest: TIocpSendRequest;
    function InnerCreateRecvRequest: TIocpRecvRequest;
  protected
    FLocker:TIocpLocker;

    /// <summary>
    ///   ά���������б�
    /// </summary>
    FOnlineContextList : TDHashTable;


    /// <summary>
    ///   ��ȡһ�������������(�����)
    /// </summary>
    function GetSendRequest: TIocpSendRequest;

    /// <summary>
    ///   ��ȡһ����������
    /// </summary>
    function GetRecvRequest: TIocpRecvRequest;

    function ReleaseRecvRequest(pvObject: TIocpRecvRequest): Boolean;

    /// <summary>
    ///   �黹һ�������������(�����)
    /// </summary>
    function ReleaseSendRequest(pvObject:TIocpSendRequest{$IFDEF DIOCP_DEBUG}; pvCtx:TIocpClientContext{$ENDIF}): Boolean;

    procedure DoAfterOpen; virtual;

    procedure DoBeforeOpen; virtual;

    procedure DoAfterClose; virtual;

    procedure DoCleanUpSendRequest;

  private
    /// <summary>
    ///   ��Ͷ�ݵ�AcceptEx������Ӧʱ�е���
    /// </summary>
    /// <param name="pvRequest"> ��Ӧ������ </param>
    procedure DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);

    function DoSetSocketOptn(pvRawSocket:TRawSocket): Boolean;

    function GetClientCount: Integer;

    procedure OnIocpException(pvRequest:TIocpRequest; E:Exception);

    procedure DoSendBufferCompletedEvent(pvContext: TIocpClientContext; pvBuffer:
        Pointer; len: Cardinal; pvBufferTag: Integer; pvTagData: Pointer;
        pvErrorCode: Integer);
    procedure InnerAddToDebugStrings(const pvMsg: String);

    procedure DoInnerCreate(pvInitalizeNum: Integer);
    procedure SetUseAsyncRecvQueue(const Value: Boolean);
  public
    procedure CheckOpen(pvInitalizeNum:Integer);

    procedure CheckCreatePoolObjects(pvMaxNum:Integer);

    /// <summary>
    ///   ��һ��Iocp����
    /// </summary>
    /// <param name="pvEngine"> (TIocpEngine) </param>
    /// <param name="pvOwner">
    ///   �Ƿ�ӵ���������,
    ///   true: �ͷ�ʱ��������һ���ͷ�
    /// </param>
    procedure BindDiocpEngine(const pvEngine: TIocpEngine; pvOwner: Boolean = true);

    /// <summary>
    ///   ��ʱ���, �������Timeoutָ����ʱ�仹û���κ����ݽ������ݼ�¼��
    ///     �ͽ��йر�����
    ///   ʹ��ѭ����⣬������кõķ�������ӭ�ύ���ı������
    /// </summary>
    procedure KickOut(pvTimeOut:Cardinal = 60000);

    /// <summary>
    ///   ��ȡ�߼�����3��δ��ɵ�������Ϣ
    /// </summary>
    function GetContextWorkingInfo(pvTimeOut:Cardinal = 3000): String;

    
    procedure LogMessage(const pvMsg: string; const pvMsgType: string = '';
        pvLevel: TLogLevel = lgvMessage); overload;

    procedure LogMessage(const pvMsg: string; const args: array of const; const
        pvMsgType: string = ''; pvLevel: TLogLevel = lgvMessage); overload;


    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   ��������ÿ��������������Ͷ��У������������ٽ���Ͷ��
    /// </summary>
    procedure SetMaxSendingQueueSize(pvSize:Integer);


    procedure AddDebugStrings(const pvDebugInfo: String; pvAddTimePre: Boolean =
        true);

    /// <summary>
    ///   ����SocketHandle�������б��в��Ҷ�Ӧ��Contextʵ��
    /// </summary>
    function FindContext(pvSocketHandle:TSocket): TIocpClientContext;

    /// <summary>
    ///   ���Һ��������
    /// </summary>
    /// <returns> �����ɹ����� Ctx���󣬷��򷵻�nil, ���óɹ��� ע�����Unlock </returns>
    function TryLockContext(pvSocketHandle: TSocket; const pvDebugInfo: string;
        pvDebugObj: TObject): TIocpClientContext;

    procedure RegisterContextClass(pvContextClass: TIocpClientContextClass);

    procedure RegisterSendRequestClass(pvClass: TIocpSendRequestClass);


    /// <summary>
    ///   ���͵����������ն�
    /// </summary>
    function PostBufferToOnlineClients(pvBuf:Pointer; pvLen:Integer; pvCopyBuf:
        Boolean = true; pvTag: Integer = 0; pvTagData: Pointer = nil): Integer;

    /// <summary>
    ///   ����ѭ���¼�
    /// </summary>
    function OnlineClientsCallBack(pvCallBack: TOnContextEvent; pvTag: Integer;
        pvTagData: Pointer): Integer;

    /// <summary>
    ///   �������ݼ������ʵ��
    /// </summary>
    procedure CreateDataMonitor;


    /// <summary>
    ///   check clientContext object is valid.
    /// </summary>
    function CheckClientContextValid(const pvClientContext: TIocpClientContext):
        Boolean;

    /// <summary>
    ///   �Ͽ�������������
    /// </summary>
    procedure DisconnectAll;

    /// <summary>
    ///   �ȴ��������ӹر�
    /// </summary>
    function WaitForContext(pvTimeOut: Cardinal): Boolean;


    /// <summary>
    ///   get online client list
    /// </summary>
    procedure GetOnlineContextList(pvList:TList);

    /// <summary>
    ///  .�ر���������
    ///  .����Ͽ���������
    ///  .�ȴ��������ӶϿ�
    ///  .�л����ر�״̬
    /// </summary>
    procedure SafeStop;

    procedure Open;

    /// <summary>
    ///   �Ƿ���Թ���
    /// </summary>
    function CanWork: Boolean;

    procedure Close;
    function GetDebugString: String;
    function GetPrintDebugInfo: string;

    /// <summary>
    ///   
    /// </summary>
    function GetStateInfo: String;



    /// <summary>
    ///   client connections counter
    /// </summary>
    property ClientCount: Integer read GetClientCount;

    property ConnectedCount: Integer read FConnectedCount;


    property DataMoniter: TIocpDataMonitor read FDataMoniter;
    property IocpEngine: TIocpEngine read FIocpEngine;

    /// <summary>
    ///   ����Tcp��KeepAliveѡ��, Ĭ�ϲ�����
    ///   (��AcceptEx�ɹ����������)
    /// </summary>
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;

    /// <summary>
    ///   ����KeepAliveʱ,���õ�ʱ��.Ĭ��10000(10��)
    /// </summary>
    property KeepAliveTime: Cardinal read FKeepAliveTime write FKeepAliveTime;

    /// <summary>
    ///   extend data
    /// </summary>
    property DataPtr: Pointer read FDataPtr write FDataPtr;


    property Listeners: TDiocpListeners read FListeners;

    //property IocpAcceptorMgr: TIocpAcceptorMgr read FIocpAcceptorMgr;

    /// <summary>
    ///   SERVER Locker
    /// </summary>
    property Locker: TIocpLocker read FLocker;


    property Logger: TSafeLogger read FLogger;



  published

    /// <summary>
    ///   ������/�ر�
    /// </summary>
    property Active: Boolean read FActive write SetActive;

    /// <summary>
    ///  ʹ���첽�Ľ��մ���ʽ
    ///    ��δ������Ŀ��֤
    /// </summary>
    property UseAsyncRecvQueue: Boolean read FUseAsyncRecvQueue write
        SetUseAsyncRecvQueue;

    /// <summary>
    ///   ���Ĵ����ͻ������, ������ʱ�������趨
    /// </summary>
    property MaxSendingQueueSize: Integer read FMaxSendingQueueSize write SetMaxSendingQueueSize;

    /// <summary>
    ///   �����ӶϿ�ʱ�����¼�
    ///     ��TDiocpTcpServer.ActiveΪFalseʱ�����д���
    ///     ��Iocp�����߳��д���
    /// </summary>
    property OnContextDisconnected: TContextNotifyEvent read FOnContextDisconnected
        write FOnContextDisconnected;

    /// <summary>
    ///   �����ӽ����ɹ�ʱ�����¼�
    ///     ��TDiocpTcpServer.ActiveΪFalseʱ�����д���
    ///     ��Iocp�����߳��д���
    /// </summary>
    property OnContextConnected: TContextNotifyEvent read FOnContextConnected
        write FOnContextConnected;

    /// <summary>
    ///   ����������ʱ�����¼�
    ///     ��TDiocpTcpServer.ActiveΪFalseʱ�����д���
    ///     ��Iocp�����߳��д���
    /// </summary>
    property OnContextAccept: TOnContextAcceptEvent read FOnContextAccept write
        FOnContextAccept;

    /// <summary>
    ///  ��Ӧ��������¼�
    /// </summary>
    property OnSendRequestResponse: TOnSendRequestResponse read
        FOnSendRequestResponse write FOnSendRequestResponse;

    /// <summary>
    ///   Ĭ��������ַ, ���ú�ָ����ĳ��IP
    /// </summary>
    property DefaultListenAddress: String read FDefaultListenAddress write
        FDefaultListenAddress;

    /// <summary>
    ///   ���͵�Buffer�Ѿ����
    /// </summary>
    property OnSendBufferCompleted: TOnBufferEvent read FOnSendBufferCompleted
        write FOnSendBufferCompleted;

    /// <summary>
    ///   ������ִ�еĹ���
    /// </summary>
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;

    /// <summary>
    ///   ������󲢷�����
    /// </summary>
    property AllowMaxOnlineCount: Integer read FAllowMaxOnlineCount write
        FAllowMaxOnlineCount;

    /// <summary>
    ///   NoDelay����(Ĭ����false)
    ///   ����Ϊtrue�ǻ����Socket��NoDelay����(����nagle�㷨)
    ///   nagle�㷨�����200ms���ҵ���ʱ
    ///   ����ʱ����ʱ�ı�ĸ����ԣ��������������Ѿ�����������(���Ժ�����������Ч)
    ///
    ///   Ϊ�˼�������ӵ������Ƶģ�����ȴ��㹻�����ݲŷ��ͳ�ȥ�����û���㹻�����ݣ��ͻ�ȴ�Լ200ms���ڲ���ʱ����ʱ���������ͳ�ȥ
    /// </summary>
    property NoDelayOption: Boolean read FNoDelayOption write FNoDelayOption;
    
    /// <summary>
    ///   Ĭ�������Ķ˿�
    /// </summary>
    property Port: Integer read FPort write FPort;

    /// <summary>
    ///   ���ͻ����С
    /// </summary>
    property SendBufCacheSize: Integer read FSendBufCacheSize write
        FSendBufCacheSize;

    /// <summary>
    ///   �Ƿ�ʹ�ö����
    /// </summary>
    property UseObjectPool: Boolean read FUseObjectPool write FUseObjectPool;









    /// <summary>
    ///   iocp�����߳�
    ///    Ϊ0ʱĬ��Ϊ cpu count * 2 -1
    /// </summary>
    property WorkerCount: Integer read GetWorkerCount write SetWorkerCount;


    /// <summary>
    ///   post wsaRecv request block size
    /// </summary>
    property WSARecvBufferSize: cardinal read FWSARecvBufferSize write
        SetWSARecvBufferSize;



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
    property OnDataReceived: TOnDataReceived read FOnDataReceived write
        FOnDataReceived;


  end;

  TDiocpLogicTask = class(TObject)
  private
    FGroupTask: TGroupTask;
    procedure DoTask(pvSender: TGroupTask; pvWorker: TGroupTaskWorker; pvData:
        Pointer);
  public
    constructor Create;
    destructor Destroy; override;

  end;



var
  __svrLogger:TSafeLogger;



/// <summary>
///   ע�����ʹ�õ�SafeLogger
/// </summary>
procedure RegisterDiocpSvrLogger(pvLogger:TSafeLogger);


procedure StartDiocpLogicWorker(const num:Integer);



implementation

uses
  DateUtils;


var
  __startTime:TDateTime;
  __innerLogger:TSafeLogger;
  __create_sn:Integer;
  __logic_task:TDiocpLogicTask;
  __default_task:TDiocpLogicTask;




/// <summary>
///   ��������TickCountʱ�����ⳬ��49������
///      ��л [��ɽ]�׺�һЦ  7041779 �ṩ
///      copy�� qsl���� 
/// </summary>
function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;



function GetRunTimeINfo: String;
var
  lvMSec, lvRemain:Int64;
  lvDay, lvHour, lvMin, lvSec:Integer;
begin
  lvMSec := MilliSecondsBetween(Now(), __startTime);
  lvDay := Trunc(lvMSec / MSecsPerDay);
  lvRemain := lvMSec mod MSecsPerDay;

  lvHour := Trunc(lvRemain / (MSecsPerSec * 60 * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60 * 60);

  lvMin := Trunc(lvRemain / (MSecsPerSec * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60);

  lvSec := Trunc(lvRemain / (MSecsPerSec));

  if lvDay > 0 then
    Result := Result + IntToStr(lvDay) + ' d ';

  if lvHour > 0 then
    Result := Result + IntToStr(lvHour) + ' h ';

  if lvMin > 0 then
    Result := Result + IntToStr(lvMin) + ' m ';

  if lvSec > 0 then
    Result := Result + IntToStr(lvSec) + ' s ';
end;


///TRunTimeINfoTools
function TransByteSize(pvByte: Int64): String;
var
  lvTB, lvGB, lvMB, lvKB:Word;
  lvRemain:Int64;
begin
  lvRemain := pvByte;

  lvTB := Trunc(lvRemain/BytePerGB/1024);
  //lvRemain := pvByte - (lvTB * BytePerGB * 1024);
  
  lvGB := Trunc(lvRemain/BytePerGB);

  lvGB := lvGB mod 1024;      // trunc TB

  lvRemain := lvRemain mod BytePerGB;

  lvMB := Trunc(lvRemain/BytePerMB);
  lvRemain := lvRemain mod BytePerMB;

  lvKB := Trunc(lvRemain/BytePerKB);
  lvRemain := lvRemain mod BytePerKB;
  Result := Format('%d TB, %d GB, %d MB, %d KB, %d B', [lvTB, lvGB, lvMB, lvKB, lvRemain]);
end;


  

/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean): Boolean; overload;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

procedure RegisterDiocpSvrLogger(pvLogger:TSafeLogger);
begin
  if __svrLogger <> pvLogger then
  begin
    __svrLogger := pvLogger;
    if __innerLogger <> nil then
    begin
      __innerLogger.Free;
      __innerLogger := nil;
    end;
  end;
end;

procedure StartDiocpLogicWorker(const num:Integer);
begin
  if __default_task = nil then __default_task := TDiocpLogicTask.Create;

  __default_task.FGroupTask.CheckCreateWorker(num);

  __logic_task := __default_task;
end;


procedure TIocpClientContext.InnerCloseContext;
var
  s, lvDebugStep:String;
begin
{$IFDEF DIOCP_DEBUG}
  if FOwner = nil then
  begin
    s := FDebugStrings.Text;
    s := Format('��ǰ�����Ѿ�ʧȥOwner:%s',
       [s]);
    sfLogger.logMessage(s, CORE_DEBUG_FILE, lgvWarning);
    Assert(FOwner <> nil);
  end;

  if FRecvQueue.Size > 0 then
  begin
    Assert(FRecvQueue.Size = 0);
  end;
{$ENDIF}


{$IFDEF WRITE_LOG}
  if FReferenceCounter <> 0 then
    FOwner.LogMessage('InnerCloseContext FReferenceCounter:%d',
      [FReferenceCounter], CORE_LOG_FILE, lgvError);

  if not FActive then
  begin
    FOwner.LogMessage('InnerCloseContext FActive is false', CORE_LOG_FILE, lgvError);
    exit;
  end;
{$ENDIF}
  if not FActive then exit;

//  Assert(FReferenceCounter = 0);
//  Assert(FActive);
  try
    FActive := false;
  {$IFDEF SOCKET_REUSE}

  {$ELSE}
    FRawSocket.Close;
  {$ENDIF}

    CheckReleaseRes;

    try
      lvDebugStep := '1.0.0';
      {$IFDEF DIOCP_DEBUG}
      CheckThreadIn('InnerCloseContext');
      try
      {$ENDIF}
        lvDebugStep := '1.0.1';
        if FOwner.Active then
        begin
          lvDebugStep := '1.0.2';
          if Assigned(FOwner.FOnContextDisconnected) then
          begin
            lvDebugStep := '1.0.2';
            FOwner.FOnContextDisconnected(Self);
            lvDebugStep := '1.0.4';
          end;
          lvDebugStep := '1.0.5';
          DoDisconnected;
          lvDebugStep := '1.0.6';
        end;
      {$IFDEF DIOCP_DEBUG}
      finally
        lvDebugStep := '1.0.7';
        CheckThreadOut;
        lvDebugStep := '1.0.8';
      end;
      {$ENDIF}
    except
      on e:Exception do
      begin
        {$IFDEF DIOCP_DEBUG}
        sfLogger.LogMessage(
          Format('InnerCloseContext(%s):%s', [lvDebugStep, e.Message]), CORE_LOG_FILE);
        {$ENDIF}
      end;
    end;
  finally
    {$IFDEF DIOCP_DEBUG}
    SetCurrentThreadInfo('(s)InnerCloseContext - 1.5');
    InnerLock;
    AddDebugString(Format('*(%d):Disconnected, DNA:%d', [self.FReferenceCounter, self.FContextDNA]));
    InnerUnLock;
    SetCurrentThreadInfo('(s)InnerCloseContext - 1.5.1');
    FOwner.RemoveFromOnOnlineList(Self);
    SetCurrentThreadInfo('(s)InnerCloseContext - 1.5.2');
    ReleaseBack();
    SetCurrentThreadInfo('(s)InnerCloseContext - 1.5.9');
    {$ELSE}
    FOwner.RemoveFromOnOnlineList(Self);
    ReleaseBack();
    {$ENDIF}


  end;

end;

procedure TIocpClientContext.InnerDoRecv(pvRecvRequest: TIocpRecvRequest);
begin
  if FOwner <> nil then
  begin
    if FOwner.UseAsyncRecvQueue then
    begin        // ѹ����еȴ�����
      pvRecvRequest.AddRef;
      self.FRecvQueue.EnQueueObject(pvRecvRequest);

      if Assigned(FOwner.FDataMoniter) then FOwner.FDataMoniter.IncCtxRecvQueueSize;


      // Ͷ�ݵ�����
      __logic_task.FGroupTask.PostATask(self);
    end else
    begin      // ֱ�Ӵ���
      DoReceiveData(pvRecvRequest);
    end;
  end;
end;

procedure TIocpClientContext.InnerLock;
begin
  {$IFDEF USE_CSLOCK}
  FContextLocker.Enter();
  {$ELSE}
  utils_strings.SpinLock(FInnerLockerFlag);
  {$ENDIF}
end;

procedure TIocpClientContext.InnerUnLock;
begin
  {$IFDEF USE_CSLOCK}
  FContextLocker.Leave();
  {$ELSE}
  utils_strings.SpinUnLock(FInnerLockerFlag);
  {$ENDIF}

end;


procedure TIocpClientContext.lock;
begin
  FContextLocker.Enter();
end;

function TIocpClientContext.LockContext(const pvDebugInfo: string; pvObj:
    TObject): Boolean;
begin
  {$IFDEF DIOCP_DEBUG}
  if Length(pvDebugInfo)=0 then
  begin
    Result := IncReferenceCounter('LockContext', pvObj);
  end else
  begin
    Result := IncReferenceCounter(pvDebugInfo, pvObj);
  end;
  {$ELSE}
  Result := IncReferenceCounter(pvDebugInfo, pvObj);
  {$ENDIF}
end;

procedure TIocpClientContext.UnLockContext(const pvDebugInfo: string; pvObj:
    TObject);
begin
  {$IFDEF DIOCP_DEBUG}
  if Self = nil then
  begin
    Assert(Self<> nil);
  end;
  if Length(pvDebugInfo)=0 then
  begin
    DecReferenceCounter('UnLockContext', pvObj);
  end else
  begin
    DecReferenceCounter(pvDebugInfo, pvObj);
  end;
  {$ELSE}
  DecReferenceCounter(pvDebugInfo, pvObj);
  {$ENDIF}

end;


procedure TIocpClientContext.BeginBusy;
begin
  InterlockedIncrement(FBusingCounter);
end;

procedure TIocpClientContext.CheckNextSendRequest;
var
  lvRequest:TIocpSendRequest;
  r:Integer;
begin
{$IFDEF DIOCP_DEBUG}
  Assert(FOwner <> nil);
{$ENDIF}

  InnerLock;
  try
    lvRequest := TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest = nil then
    begin
      FSending := false;
      exit;
    end;
  finally
    InnerUnLock;
  end;

  if lvRequest <> nil then
  begin   
    FcurrSendRequest := lvRequest;
    r := lvRequest.ExecuteSend;
    if r = 0 then
    begin
      if (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostSendObjectCounter;
      end;
    end else
    begin
      FCurrSendRequest := nil;

      /// ȡ������
      lvRequest.CancelRequest;

      if r = -2 then
      begin
        // 
        FRawSocket.ShutDown();
        //RequestDisconnect(strWSACloseRequestEx, lvRequest);
      end else
      begin
       /// �߳�����
        RequestDisconnect(Format(strFuncFail,
          [self.SocketHandle,'CheckNextSendRequest::lvRequest.ExecuteSend', lvRequest.FLastMsg]), lvRequest);
      end;
      FOwner.ReleaseSendRequest(lvRequest{$IFDEF DIOCP_DEBUG},Self{$ENDIF});
    end;
  end;
end;

procedure TIocpClientContext.CheckReleaseRes;
var
  lvRequest:TIocpSendRequest;
begin
  while true do
  begin
    lvRequest :=TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest <> nil then
    begin
      if (FOwner.FDataMoniter <> nil) then
      begin
        InterlockedIncrement(FOwner.FDataMoniter.FSendRequestAbortCounter);
      end;

      lvRequest.CancelRequest;
      FOwner.ReleaseSendRequest(lvRequest{$IFDEF DIOCP_DEBUG},Self{$ENDIF});
    end else
    begin
      Break;
    end;
  end;
end;

constructor TIocpClientContext.Create;
begin
  inherited Create;
  FWSARecvRef := 0;
  
  FCreateSN := InterlockedIncrement(__create_sn);

  FDebugStrings := TStringList.Create;
  FReferenceCounter := 0;
  FContextLocker := TCriticalSection.Create();
  FAlive := False;
  FRawSocket := TRawSocket.Create();
  FActive := false;

  FSendRequestLink := TIocpRequestSingleLink.Create(100);


//  FRecvRequest := TIocpRecvRequest.Create;
//  FRecvRequest.FClientContext := self;

  FRecvQueue := TSafeQueue.Create();

  {$IFDEF SOCKET_REUSE}
  FDisconnectExRequest:=TIocpDisconnectExRequest.Create;
  FDisconnectExRequest.FContext := Self;
  FDisconnectExRequest.OnResponse := OnDisconnectExResponse;
  {$ENDIF}
end;

function TIocpClientContext.IncReferenceCounter(const pvDebugInfo: string;
    pvObj: TObject = nil): Boolean;
begin
  InnerLock;
  try
    if (not Active) or FRequestDisconnectFlag then
    begin
      Result := false;
    end else
    begin
      Assert(FReferenceCounter >= 0);
      
      Inc(FReferenceCounter);

      {$IFDEF DIOCP_DEBUG}
      if Length(pvDebugInfo) = 0 then
      begin
        Assert(Length(pvDebugInfo) > 0)
      end;
      AddDebugString(Format('+(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));
      {$ENDIF}

      Result := true;
    end;
  finally
    InnerUnLock;
  end;
end;


procedure TIocpClientContext.AddDebugString(const pvString: string);
begin
  if __free_flag = FREE_FLAG then
  begin
    Assert(__free_flag <> FREE_FLAG);
  end;
  FDebugStrings.Add(pvString);
  if FDebugStrings.Count > 100 then FDebugStrings.Delete(0);
end;

function TIocpClientContext.DecReferenceCounter(const pvDebugInfo: string;
    pvObj: TObject = nil): Integer;
var
  lvCloseContext:Boolean;
  {$IFDEF DIOCP_DEBUG}
  lvFmt:String;
  {$ENDIF}
begin
  {$IFDEF DIOCP_DEBUG}
  if __free_flag = FREE_FLAG then
  begin
    Assert(__free_flag <> FREE_FLAG);
  end;

  if self = nil then
  begin
    Assert(False);
  end;

  if FOwner = nil then
  begin
    lvFmt := format('TIocpClientContext.DecReferenceCounter:%d, DebugInfo:%s%s',
      [FReferenceCounter, sLineBreak, FDebugStrings.Text]);
    sfLogger.logMessage(lvFmt, CORE_DEBUG_FILE, lgvError);
    Assert(False, lvFmt);
  end;
  {$ENDIF}

  lvCloseContext := false;
  InnerLock;
  try
//    {$IFDEF WRITE_LOG}
//    FOwner.logMessage(Format('(%s:%d[%d]):%s', [self.RemoteAddr, self.RemotePort, self.SocketHandle, pvDebugInfo]),
//        strRequestDisconnectFileID, lgvDebug);
//    {$ENDIF}
    Dec(FReferenceCounter);
    Result := FReferenceCounter;
    {$IFDEF DIOCP_DEBUG}
    if Length(pvDebugInfo) = 0 then
    begin
      if IsDebugMode then
      begin
        Assert(Length(pvDebugInfo) > 0);
      end;
    end;
    AddDebugString(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));
    {$ENDIF}

    if FReferenceCounter < 0 then
    begin  // С��0�����������
      {$IFDEF DIOCP_DEBUG}
      if IsDebugMode then
      begin
        sfLogger.logMessage('TIocpClientContext.DecReferenceCounter:%d, DebugInfo:%s%s',
          [FReferenceCounter, sLineBreak, FDebugStrings.Text], CORE_DEBUG_FILE, lgvError);
        Assert(FReferenceCounter >=0,Format('TIocpClientContext.DecReferenceCounter:%d, DebugInfo:%s',
          [FReferenceCounter, FDebugStrings.Text]));
      end else
      begin
        FOwner.logMessage('TIocpClientContext.DecReferenceCounter:%d, DebugInfo:%s',
            [FReferenceCounter, FDebugStrings.Text], CORE_DEBUG_FILE, lgvError);
      end;
      {$ENDIF}
      FReferenceCounter :=0;
    end;
    if FReferenceCounter = 0 then
      if FRequestDisconnectFlag then lvCloseContext := true;
  finally
    InnerUnLock;
  end;

  if lvCloseContext then InnerCloseContext;
end;

procedure TIocpClientContext.DecReferenceCounterAndRequestDisconnect(const
    pvDebugInfo: string; pvObj: TObject= nil);
var
  lvCloseContext:Boolean;
begin
  lvCloseContext := false;

  InnerLock;
  try
    {$IFDEF DIOCP_DEBUG}
      FOwner.logMessage(Format('(%s:%d[%d]):%s', [self.RemoteAddr, self.RemotePort, self.SocketHandle, pvDebugInfo]),
        strRequestDisconnectFileID, lgvDebug);
    {$ENDIF}

    FRequestDisconnectFlag := true;
    Dec(FReferenceCounter);
  
    {$IFDEF DIOCP_DEBUG}
    if Length(pvDebugInfo) = 0 then
    begin
      if IsDebugMode then Assert(Length(pvDebugInfo) > 0)
    end;
    AddDebugString(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));
    {$ENDIF}

    if FReferenceCounter < 0 then
    begin
      {$IFDEF DIOCP_DEBUG}
      if IsDebugMode then
      begin
        Assert(FReferenceCounter >=0);
      end else
      begin
        FOwner.logMessage('TIocpClientContext.DecReferenceCounterAndRequestDisconnect:%d, DebugInfo:%s',
            [FReferenceCounter, FDebugStrings.Text], CORE_DEBUG_FILE, lgvError);
      end;
      {$ENDIF}
      FReferenceCounter :=0;
    end;
    if FReferenceCounter = 0 then
      lvCloseContext := true;
    
  finally
    InnerUnLock;
  end;
  
  if lvCloseContext then InnerCloseContext;
end;

procedure TIocpClientContext.ReleaseBack;
begin
  // �黹�����������ĳ�
  FAcceptorMgr.ReleaseClientContext(Self);
end;

function TIocpClientContext.ReleaseSendRequest(
  pvObject: TIocpSendRequest): Boolean;
begin
  Result := FOwner.ReleaseSendRequest(pvObject{$IFDEF DIOCP_DEBUG},Self{$ENDIF});
end;

function TIocpClientContext.RequestDisconnect(const pvReason: string = '';
    pvObj: TObject = nil): Boolean;
var
  lvCloseContext:Boolean;
begin
  Result := False;
  if not FActive then exit;

{$IFDEF WRITE_LOG}
  if FOwner <> nil then
  begin
    FOwner.logMessage(Format('(%s:%d[%d]):%s', [self.RemoteAddr, self.RemotePort, self.SocketHandle, pvReason]), strRequestDisconnectFileID, lgvDebug);
  end;
{$ENDIF}

  lvCloseContext := False;
  InnerLock;
  try
    if FAlive = False then
    begin
      Exit;
    end;

    if FRequestDisconnectFlag then
    begin
      Exit;
    end;


    // �ر�����
    FRequestClose := 1;

    {$IFDEF SOCKET_REUSE}
    lvCloseContext := False;
    if not FRequestDisconnectFlag then
    begin
      {$IFDEF DIOCP_DEBUG}
      if Length(pvReason) = 0 then
      begin
        Assert(Length(pvReason) > 0)
      end;
      AddDebugString(Format('*(%d):%d,�������:%s', [FReferenceCounter, IntPtr(pvObj), pvReason]));

      {$ENDIF}
      FDisconnectedReason := pvReason;

      // cancel
      FRawSocket.ShutDown();
      FRawSocket.CancelIO;

      // post succ, in handleReponse Event do
      if not FDisconnectExRequest.PostRequest then
      begin      // post fail,
        FRawSocket.close;
        if FReferenceCounter = 0 then  lvCloseContext := true;    //      lvCloseContext := true;   //directly close
      end;
      FRequestDisconnectFlag := True;
    end;
    {$ELSE}
    if not FRequestDisconnectFlag then
    begin
      {$IFDEF DIOCP_DEBUG}
      if Length(pvReason) = 0 then
      begin
        Assert(Length(pvReason) > 0)
      end;
      AddDebugString(Format('*(%d):%d,�������:%s', [FReferenceCounter, IntPtr(pvObj), pvReason]));
      {$ENDIF}
      FDisconnectedReason := pvReason;
      FRequestDisconnectFlag := True;
    end;
    {$ENDIF}

    Result := True;


    if FReferenceCounter = 0 then  lvCloseContext := true;
  finally
    InnerUnLock;
  end;

  {$IFDEF SOCKET_REUSE}
  if lvCloseContext then InnerCloseContext;
  {$ELSE}
  if lvCloseContext then InnerCloseContext else
  begin
    FRawSocket.Close(False);
  end;
  {$ENDIF}
end;

destructor TIocpClientContext.Destroy;
begin
{$IFDEF DIOCP_DEBUG}
  if FRecvQueue.Size > 0 then
  begin
    Assert(FRecvQueue.Size = 0);
  end;

  if FCloseingFlag = 1 then
  begin
    Assert(FCloseingFlag = 0, '�ر��¼���δ����');
  end;

  if FOnRecvingFlag = 1 then
  begin
    Assert(FOnRecvingFlag = 0, 'OnRecv�¼���δ����');
  end;

  if FWSARecvRef > 0 then
  begin
     Assert(FWSARecvRef = 0, Format('OnRecv�¼���δ����, FWSARecvRef:%d',[FWSARecvRef]));
  end;


  if IsDebugMode then
  begin
    if FReferenceCounter <> 0 then
    begin
      sfLogger.logMessage(Format('[%d]:%s', [self.SocketHandle, self.FDebugStrings.Text]), 'debugstr');
      Assert(FReferenceCounter = 0);
    end;

    if FSendRequestLink.Count > 0 then
    begin
      Assert(FSendRequestLink.Count = 0);
    end;

    if FRawSocket.SocketValid then
    begin
      sfLogger.logMessage(Format('[%d]:%s', [self.SocketHandle, self.FDebugStrings.Text]), 'debugstr');
      Assert(False);
    end;
  end;


{$ENDIF}
  FRawSocket.Close;
  FRawSocket.Free;


//  if FRecvRequest.Responding then
//  begin
//    // ������Ӧ���������ͷ�
//    FRecvRequest.DestroyOnResponseEnd := true;
//  end else
//  begin
//    FRecvRequest.__debugFlag2 := -2;
//    FRecvRequest.FClientContext := nil;
//    FRecvRequest.Free;
//  end;
  
  if IsDebugMode then
  begin
    Assert(FSendRequestLink.Count = 0);
    Assert(FRecvQueue.Size = 0);
  end;

  {$IFDEF SOCKET_REUSE}
  FDisconnectExRequest.Free;
  {$ENDIF}

  FSendRequestLink.Free;
  FContextLocker.Free;
  FDebugStrings.Free;
  FDebugStrings := nil;
  FRecvQueue.Free;

  __free_flag := FREE_FLAG;
  inherited Destroy;
end;

procedure TIocpClientContext.CheckThreadIn(const pvDebugInfo: String);
begin
  if FCheckThreadId <> 0 then
  begin
    //s := GetDebugString;
    raise Exception.CreateFmt('%s=>(%d,%d)��ǰ�����Ѿ��������߳�����ʹ��',
       [pvDebugInfo, utils_strings.GetCurrentThreadID, FCheckThreadId]);
  end;
  FCheckThreadId := utils_strings.GetCurrentThreadID;
  FDebugInfo := pvDebugInfo;
end;

procedure TIocpClientContext.CheckThreadOut;
begin
  FDebugInfo := STRING_EMPTY;
  FCheckThreadId := 0;
end;

function TIocpClientContext.CheckWorkingTick: Cardinal;
begin
  Result := 0;
  // �Ѿ���ɹ���
  if FWorkerEndTick <> 0 then Exit;

  // ��û�п�ʼ����
  if FWorkerStartTick = 0 then Exit;

  Result := tick_diff(FWorkerStartTick, GetTickCount);  
end;

procedure TIocpClientContext.DecRecvRef;
var
  j:Integer;
begin
  j := InterlockedDecrement(FWSARecvRef);
  if j < 0 then
  begin
    Assert(false, 'error');
  end;
  
//  {$IFDEF DIOCP_DEBUG}
//  // ������븨������ECHO,
//  //  Http�л����0
//  if j > 0 then
//  begin
//    sfLogger.logMessage(Format('[%d]:%s', [self.SocketHandle, self.FDebugStrings.Text]), 'debugstr');
//    Assert(j = 0);
//  end;
//  {$ENDIF}
  
  if j = 0 then
  begin
    if (not FRequestDisconnectFlag) then
    begin
      PostWSARecvRequest;
    end;
  end;
end;

procedure TIocpClientContext.DoCleanUp;
begin
  {$IFDEF DIOCP_DEBUG}
  CheckThreadIn('DoCleanUp');
  try
  {$ENDIF}
    FLastActivity := 0;
    FRequestClose := 0;

    FWSARecvRef := 0;

    FOwner := nil;
    FRequestDisconnectFlag := false;
    FSending := false;

    FWorkerEndTick := 0;
    FWorkerStartTick := 0;

    {$IFDEF DIOCP_DEBUG}
    InnerLock;
    AddDebugString(Format('*(%d):%d,%s', [FReferenceCounter, IntPtr(Self), '-----DoCleanUp-----']));
    InnerUnLock;
    {$ENDIF}
    if IsDebugMode then
    begin
      Assert(FReferenceCounter = 0, Format('TIocpClientContext.DoCleanUp::FReferenceCounter=%d', [FReferenceCounter]));
      Assert(not FActive, 'DoCleanUp::Context is active');
    end;
{$IFDEF DIOCP_DEBUG}
  finally
    CheckThreadOut;
  end;
{$ENDIF}


//  if FActive then
//  begin
//    FRawSocket.close;
//    FActive := false;
//    CheckReleaseRes;
//  end;
end;

procedure TIocpClientContext.DoConnected;
begin
  FLastActivity := GetTickCount;

  //FContextLocker.lock('DoConnected');
  try
    FSocketHandle := MakeDiocpHandle;
    //  FRawSocket.SocketHandle;
    Assert(FOwner <> nil);
    if FActive then
    begin
      if IsDebugMode then
      begin
        Assert(not FActive);
      end;
      {$IFDEF WRITE_LOG}
       FOwner.logMessage(strDoConnectedError, [SocketHandle], CORE_DEBUG_FILE, lgvError);
      {$ENDIF}
    end else
    begin
      InterlockedIncrement(FOwner.FConnectedCount);

      FContextDNA := FOwner.RequestContextDNA;
      FActive := true;

      {$IFDEF DIOCP_DEBUG}
      InnerLock;
      AddDebugString(Format('*(%d):Connected, DNA:%d', [self.FReferenceCounter, FContextDNA]));
      InnerUnLock;
      {$ENDIF}
      FOwner.AddToOnlineList(Self);

      if self.LockContext('onConnected', Self) then
      try
        try
          if Assigned(FOwner.FOnContextConnected) then
          begin
            FOwner.FOnContextConnected(Self);
          end;

          OnConnected();
        except
          on e:Exception do
          begin
            {$IFDEF DIOCP_DEBUG}
            sfLogger.LogMessage(
              Format('DoConnected:%s', [e.Message]), CORE_LOG_FILE);
            {$ENDIF}
          end;
        end;

        PostWSARecvRequest;
      finally
        Self.UnLockContext('OnConnected', Self);
      end;
    end;
  finally
    //FContextLocker.unLock;
  end;
end;

procedure TIocpClientContext.DoDisconnect;
begin
  RequestDisconnect('DoDisconnect');
end;

procedure TIocpClientContext.DoDisconnected;
begin
  self.FCloseingFlag := 1;
  try
    if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then FOwner.FDataMoniter.IncDisconnectedCounter;

    OnDisconnected;
  finally
    self.FCloseingFlag := 0;
  end;
end;

procedure TIocpClientContext.DoOwnerClientContext(pvErrorCode: Integer);
begin
  Owner.DoClientContextError(self, pvErrorCode);
end;

procedure TIocpClientContext.DoProcessRecvQueue;
var
  lvRecv:TIocpRecvRequest;
begin
  if AtomicCmpExchange(self.FProcessFlag, 1, 0) = 0 then
  begin
    try
      while True do
      begin
        lvRecv := TIocpRecvRequest(FRecvQueue.DeQueueObject);
        if lvRecv = nil then Break;
        try
          self.DoReceiveData(lvRecv);
          if Assigned(FOwner.FDataMoniter) then FOwner.FDataMoniter.DecCtxRecvQueueSize;
        finally
          lvRecv.DecRef;
        end;
      end;
    finally
      self.FProcessFlag := 0;
    end;
  end;

end;

procedure TIocpClientContext.DoReceiveData(pvRecvRequest: TIocpRecvRequest);
begin
  try
    if FRequestClose = 1 then
    begin
      // ������Ӧ�κεĽ�����������
      Exit;
    end;
    FOnRecvingFlag := 1;
    BeginBusy;
    try
      FLastActivity := GetTickCount;

      OnRecvBuffer(pvRecvRequest.FRecvBuffer.buf,
        pvRecvRequest.FBytesTransferred,
        pvRecvRequest.FErrorCode);
      if FOwner <> nil then
        FOwner.doReceiveData(Self, pvRecvRequest);
    finally
      EndBusy;
      FOnRecvingFlag := 0;
    end;
  except
    on E:Exception do
    begin
      if FOwner <> nil then
      begin       
        FOwner.LogMessage(strOnRecvBufferException, [SocketHandle, e.Message], '�쳣', lgvError);

        FOwner.DoClientContextError(Self, -1);
      end else
      begin
        __svrLogger.logMessage(strOnRecvBufferException, [SocketHandle, e.Message], '�쳣', lgvError);
      end;
    end;
  end;
end;

procedure TIocpClientContext.DoSendBufferCompleted(pvBuffer: Pointer; len:
    Cardinal; pvBufferTag: Integer; pvTagData: Pointer; pvErrorCode: Integer);
begin

end;

procedure TIocpClientContext.DoSendRequestCompleted(pvRequest:
    TIocpSendRequest);
begin
  ;
end;

procedure TIocpClientContext.DoSendRequestRespnonse(
  pvRequest: TIocpSendRequest);
begin
  FLastActivity := GetTickCount;
  AtomicDecrement(FSendQueueSize); 
  if Assigned(FOwner.FOnSendRequestResponse) then
  begin
    FOwner.FOnSendRequestResponse(Self, pvRequest);
  end;
end;

procedure TIocpClientContext.EndBusy;
begin
  InterlockedDecrement(FBusingCounter);
end;

function TIocpClientContext.GetDebugInfo: string;
begin
  InnerLock;
  try
    if Length(FDebugInfo) > 0 then
      Result := Copy(FDebugInfo, 0, Length(FDebugInfo))
    else
      Result := '';
  finally
    InnerUnLock;
  end;
end;

function TIocpClientContext.GetSendQueueSize: Integer;
begin
  Result := FSendRequestLink.Count;
end;

function TIocpClientContext.GetSendRequest: TIocpSendRequest;
begin
  Result := FOwner.GetSendRequest;
  {$IFDEF DIOCP_DEBUG}
  Result.AddDebugString('*', 'GetSendRequest', Self);
  Assert(Result <> nil);
  {$ENDIF}
  Result.FClientContext := self;
end;

procedure TIocpClientContext.IncRecvRef;
var
  j:Integer;
begin
  j := InterlockedIncrement(FWSARecvRef);
  Assert(j > 0, 'error IncRecvRef');

end;


procedure TIocpClientContext.OnAfterCreateSocket;
begin

end;

procedure TIocpClientContext.OnConnected;
begin

end;

procedure TIocpClientContext.OnDisconnected;
begin

end;


{$IFDEF SOCKET_REUSE}
procedure TIocpClientContext.OnDisconnectExResponse(pvObject:TObject);
var
  lvRequest:TIocpDisconnectExRequest;
begin
  if FActive then
  begin   // already connected
    lvRequest :=TIocpDisconnectExRequest(pvObject);
    if lvRequest.FErrorCode <> 0 then
    begin
      RawSocket.close;
      if (FOwner.FDataMoniter <> nil) then
        FOwner.FDataMoniter.incHandleDestroyCounter;

      DecReferenceCounter(
          Format('TIocpDisconnectExRequest.HandleResponse.Error, %d', [lvRequest.FErrorCode])
          , lvRequest
        );
    end else
    begin
      DecReferenceCounter(
          'TIocpDisconnectExRequest.HandleResponse', lvRequest
        );
    end;
  end else
  begin
    // not connected, onaccept allow is false
    FOwner.releaseClientContext(Self)
  end;
end;
{$ENDIF}


procedure TIocpClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD);
begin
    
end;

procedure TIocpClientContext.PostNextSendRequest;
begin
  CheckNextSendRequest;
end;

function TIocpClientContext.InnerPostSendRequestAndCheckStart(
    pvSendRequest:TIocpSendRequest): Boolean;
var
  lvStart:Boolean;
begin
  lvStart := false;
  InnerLock;
  try
    Result := FSendRequestLink.Push(pvSendRequest);
    if Result then
    begin
      if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPushSendQueueCounter;
      end;
      if not FSending then
      begin
        FSending := true;
        lvStart := true;  // start send work
      end;
    end;
  finally
    InnerUnLock;
  end;



  if lvStart then
  begin      // start send work

    CheckNextSendRequest;
  end;

  if FSendRequestLink.Count > 10 then
  begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}
  end;  
end;


procedure TIocpClientContext.SetRecvWorkerHint(const pvFmtMsg: string; const
    args: array of const);
begin
  SetRecvWorkerHint(Format(pvFmtMsg, args));
end;

procedure TIocpClientContext.PostWSACloseRequest;
begin
  if not FActive then exit;
  
  FRequestClose := 1;
  PostWSASendRequest(nil, 0, dtNone, -1);
  
end;

procedure TIocpClientContext.PostWSARecvRequest;
var
  lvRecvRequest:TIocpRecvRequest;
begin
  if FOwner = nil then
  begin
    Assert(FOwner<> nil);
  end;
  lvRecvRequest := FOwner.GetRecvRequest;
  lvRecvRequest.FClientContext := Self;
  if not lvRecvRequest.PostRecvRequest then
  begin
    lvRecvRequest.DecRef;
  end;
end;



function TIocpClientContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
    pvCopyBuf: Boolean = true; pvTag: Integer = 0; pvTagData: Pointer = nil):
    Boolean;
var
  lvBuf: PAnsiChar;
{$IFDEF DIRECT_SEND}
  lvRequest: TIocpSendRequest;
{$ENDIF}
begin
  if len = 0 then raise Exception.Create('PostWSASendRequest::request buf is zero!');
  if pvCopyBuf then
  begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    {$IFDEF DIRECT_SEND}
    lvRequest := GetSendRequest;     // һ����
    lvRequest.SetBuffer(lvBuf, len, dtFreeMem);
    lvRequest.Tag := pvTag;
    lvRequest.Data := pvTagData;
    Result := lvRequest.ExecuteSend = 0;  // ����spinlock, spinunlock
    if not Result then
    begin
      lvRequest.CheckClearSendBuffer;
      lvRequest.UnBindingSendBuffer;
      Self.RequestDisconnect();   
      FOwner.ReleaseSendRequest(lvRequest);
    end;
    {$ELSE}

    Result := PostWSASendRequest(lvBuf, len, dtFreeMem, pvTag, pvTagData);
    if not Result then
    begin            //post fail
      FreeMem(lvBuf);
    end;
    {$ENDIF}

  end else
  begin
    lvBuf := buf;
    {$IFDEF DIRECT_SEND}
    lvRequest := GetSendRequest;
    lvRequest.SetBuffer(lvBuf, len, dtNone);
    lvRequest.Tag := pvTag;
    lvRequest.Data := pvTagData;
    Result := lvRequest.ExecuteSend = 0;
    if not Result then
    begin 
      lvRequest.UnBindingSendBuffer;
      Self.RequestDisconnect();

      FOwner.ReleaseSendRequest(lvRequest);
    end;
    {$ELSE}
    Result := PostWSASendRequest(lvBuf, len, dtNone, pvTag, pvTagData);
    {$ENDIF}  
  end;

end;

function TIocpClientContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
    pvBufReleaseType: TDataReleaseType; pvTag: Integer = 0; pvTagData: Pointer
    = nil): Boolean;
var
  lvRequest:TIocpSendRequest;
  s:String;
begin
  Result := false;
  if self.Active then
  begin
    {$IFDEF DIOCP_DEBUG}
    if self.IncReferenceCounter('PostWSASendRequest', Self) then
    {$ELSE}
    if self.IncReferenceCounter(STRING_EMPTY) then
    {$ENDIF}
    begin
      try
        lvRequest := GetSendRequest;
        lvRequest.SetBuffer(buf, len, pvBufReleaseType);
        lvRequest.Tag := pvTag;
        lvRequest.Data := pvTagData;
        {$IFDEF DIRECT_SEND}
        Result := lvRequest.ExecuteSend = 0;
        if not Result then
        begin
          lvRequest.UnBindingSendBuffer;
          Self.RequestDisconnect();   
          FOwner.ReleaseSendRequest(lvRequest);
        end;
        {$ELSE}
        Result := InnerPostSendRequestAndCheckStart(lvRequest);
        if not Result then
        begin
          /// Push Fail unbinding buf
          lvRequest.UnBindingSendBuffer;

          s := Format(strSendPushFail, [FSocketHandle, FSendRequestLink.Count, FSendRequestLink.MaxSize]);

          Self.RequestDisconnect(s,lvRequest);

          FOwner.ReleaseSendRequest(lvRequest{$IFDEF DIOCP_DEBUG},Self{$ENDIF});
        end;
        {$ENDIF}
      finally
        {$IFDEF DIOCP_DEBUG}
        self.DecReferenceCounter('PostWSASendRequest', Self);
        {$ELSE}
        self.DecReferenceCounter(STRING_EMPTY);
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TIocpClientContext.RecordWorkerEndTick;
begin
  FWorkerEndTick := GetTickCount;
end;

procedure TIocpClientContext.RecordWorkerStartTick;
begin
  FWorkerStartTick := GetTickCount;
  FWorkerEndTick := 0;
end;



procedure TIocpClientContext.SetDebugInfo(const Value: string);
begin
  InnerLock;
  try
    FDebugInfo := Value;
  finally
    InnerUnLock;
  end;
end;

procedure TIocpClientContext.SetMaxAcceptNum(pvMax: Integer);
begin
  ;
end;

procedure TIocpClientContext.SetOwner(const Value: TDiocpTcpServer);
begin
  FOwner := Value;
  //FRecvRequest.FOwner := FOwner;
  {$IFDEF SOCKET_REUSE}
  {$IFDEF WRITE_LOG}
  FDisconnectExRequest.FOwner := FOwner;
  {$ENDIF}
  {$ENDIF}
end;

procedure TIocpClientContext.SetRecvWorkerHint(const pvHintStr: String);
begin
//  if FRecvRequest <> nil then
//    FRecvRequest.SetWorkHintInfo(pvHintStr);
end;

procedure TIocpClientContext.SetSocketState(pvState:TSocketState);
begin
  FSocketState := pvState;
//  if Assigned(FOnSocketStateChanged) then
//  begin
//    FOnSocketStateChanged(Self);
//  end;
end;

procedure TIocpClientContext.UnLock;
begin
  FContextLocker.Leave;
end;


procedure TDiocpTcpServer.AddToOnlineList(pvObject: TIocpClientContext);
begin
  FLocker.lock('AddToOnlineList');
  try
    FOnlineContextList.Add(pvObject.FSocketHandle, pvObject);
    if DataMoniter <> nil then
    begin
      DataMoniter.CalcuMaxOnlineCount(FOnlineContextList.Count);
    end;
  finally
    FLocker.unLock;
  end;


end;

function TDiocpTcpServer.CheckClientContextValid(const pvClientContext:
    TIocpClientContext): Boolean;
begin
  Result := (pvClientContext.FOwner = Self);
end;

procedure TDiocpTcpServer.CheckDoDestroyEngine;
begin
  if FOwnerEngine then
  begin
    if FIocpEngine <> nil then
    begin
      if not FIocpEngine.StopWorkers(10000) then
      begin        // record info
        SafeWriteFileMsg('EngineWorkerInfo:' +
           sLineBreak + FIocpEngine.GetStateINfo + sLineBreak +
           '================================================' + sLineBreak +
           'TcpServerInfo:' +
           sLineBreak + GetStateINfo, Self.Name + '_SafeStopTimeOut');
      end;
      FIocpEngine.SafeStop();
      FIocpEngine.Free;
      FIocpEngine := nil;
    end;
    FOwnerEngine := False;
  end;
end;

procedure TDiocpTcpServer.Close;
begin
  SetActive(False);
end;

constructor TDiocpTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoInnerCreate(1000);
end;

destructor TDiocpTcpServer.Destroy;
begin
  FLogger.Enable := false;

  FIsDestroying := true;

  SafeStop;

  if FDataMoniter <> nil then FDataMoniter.Free;



  FSendRequestPool.FreeDataObject;
  FRecvRequestPool.FreeDataObject;

  CheckDoDestroyEngine;
  
  FOnlineContextList.Free;

  FSendRequestPool.Free;
  FRecvRequestPool.Free;

  FLogger.Free;

  FLocker.Free;
  FDebugStrings.Free;
  FListeners.Free;
  FDefaultListener.Free;
  inherited Destroy;
end;

procedure TDiocpTcpServer.AddDebugStrings(const pvDebugInfo: String;
    pvAddTimePre: Boolean = true);
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

procedure TDiocpTcpServer.BindDiocpEngine(const pvEngine: TIocpEngine; pvOwner:
    Boolean = true);
begin
  CheckDoDestroyEngine;
    
  FIocpEngine := pvEngine;
  FOwnerEngine := pvOwner;
end;

function TDiocpTcpServer.CanWork: Boolean;
begin
  Result := (FActive and (self.FClosingFlag = 0)) or (FOpeningFlag = 1);
end;

procedure TDiocpTcpServer.CheckCreatePoolObjects(pvMaxNum:Integer);
var
  i, j:Integer;
  lvRecv:TIocpRecvRequest;
begin
  FDefaultListener.FAcceptorMgr.CheckCreatePoolObjects(pvMaxNum);

  // Ԥ�ȴ������󣬱���������ڴ�鹲���ڴ�
  j := pvMaxNum * 10;
  for i := FSendRequestPool.Size to j - 1 do
  begin
    FSendRequestPool.EnQueue(InnerCreateSendRequest);
  end;


  // Ԥ�ȴ������󣬱���������ڴ�鹲���ڴ�
  j := pvMaxNum * 2;
  for i := FRecvRequestPool.Size to j - 1 do
  begin
    lvRecv := InnerCreateRecvRequest;
    lvRecv.CheckCreateRecvBuffer;
    FRecvRequestPool.EnQueue(lvRecv);
  end;
end;

procedure TDiocpTcpServer.CheckOpen(pvInitalizeNum:Integer);
begin
  if FActive then Exit;

  FOpeningFlag := 1;
  try
  
    // ����IOCP����
    FIocpEngine.CheckStart;

    if UseAsyncRecvQueue then
    begin
      FRecvBuffLink := NewBufferPool(self.FWSARecvBufferSize, 0);
    end;
  
    DoBeforeOpen();

    if FListeners.FList.Count = 0 then
    begin
      FDefaultListener.FListenAddress := FDefaultListenAddress;
      FDefaultListener.FListenPort := FPort;
      FDefaultListener.FIPVersion := IP_V4;
      FDefaultListener.Start(FIocpEngine);
      FDefaultListener.PostAcceptExRequest(pvInitalizeNum);
    end else
    begin
      FListeners.Start(FIocpEngine);
      FListeners.PostAcceptExRequest(pvInitalizeNum);
    end;

    FActive := True;

    DoAfterOpen;
    if Assigned(FOnAfterOpen) then FOnAfterOpen(Self);
  finally
    FOpeningFlag := 0;
  end;
end;



procedure TDiocpTcpServer.DisconnectAll;
var
  I:Integer;
  lvBucket: PDHashData;
  lvClientContext:TIocpClientContext;
begin
  FLocker.lock('DisconnectAll');
  try
    for I := 0 to FOnlineContextList.BucketSize - 1 do
    begin
      lvBucket := FOnlineContextList.Buckets[I];
      while lvBucket<>nil do
      begin
        lvClientContext := TIocpClientContext(lvBucket.Data);
        if lvClientContext <> nil then
        begin
          {$IFDEF DIOCP_DEBUG}
          lvClientContext.RequestDisconnect('DisconnectAll');
          {$ELSE}
          lvClientContext.RequestDisconnect;
          {$ENDIF}


        end;
        lvBucket:=lvBucket.Next;
      end;
    end;
  finally
    FLocker.unLock;
  end;
end;


function TDiocpTcpServer.LogCanWrite: Boolean;
begin
  Result := (not IsDestroying) and FLogger.Enable;
end;


procedure TDiocpTcpServer.LogMessage(const pvMsg: string; const args: array of
    const; const pvMsgType: string = ''; pvLevel: TLogLevel = lgvMessage);
begin
  if LogCanWrite then
  begin
    __svrLogger.logMessage(pvMsg, args, pvMsgType, pvLevel);
  end;
end;

procedure TDiocpTcpServer.LogMessage(const pvMsg: string; const pvMsgType:
    string = ''; pvLevel: TLogLevel = lgvMessage);
begin
  if LogCanWrite then
  begin
    __svrLogger.logMessage(pvMsg, pvMsgType, pvLevel);
  end;
end;

procedure TDiocpTcpServer.DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);

{$IFDEF SOCKET_REUSE}
var
  lvErrCode:Integer;
{$ELSE}
var
  lvRet:Integer;
  lvErrCode:Integer;
{$ENDIF}
  function DoAfterAcceptEx():Boolean;
  begin
    Result := true;
    if (FAllowMaxOnlineCount>0) and (self.ClientCount >= FAllowMaxOnlineCount) then
    begin
      Result := False;
      {$IFDEF DIOCP_DEBUG}
      logMessage('DoAfterAcceptEx, Out of AllowMaxOnlineCount(%d/%d)', [self.ClientCount, FAllowMaxOnlineCount]);
      {$ENDIF}
      Exit;
    end;
        
    if Assigned(FOnContextAccept) then
    begin
      FOnContextAccept(pvRequest.FClientContext.RawSocket.SocketHandle,
         pvRequest.FClientContext.RemoteAddr, pvRequest.FClientContext.RemotePort, Result);

      if not Result then
      begin
        {$IFDEF DIOCP_DEBUG}
        logMessage('OnAcceptEvent vAllowAccept = false');
        {$ENDIF}
      end;
    end;
    // ����listen���� UpdateAcceptContext���listenr��socket���и���
//    if Result then
//    begin
//      if FKeepAlive then
//      begin
//        Result := SetKeepAlive(pvRequest.FClientContext.FRawSocket.SocketHandle, FKeepAliveTime);
//        if not Result then
//        begin
//          lvErrCode := GetLastError;
//          {$IFDEF DIOCP_DEBUG}
//          logMessage('FClientContext.FRawSocket.setKeepAliveOption, Error:%d', [lvErrCode]);
//          {$ENDIF}
//        end;
//      end;
//
//      if FSendBufCacheSize > 0 then
//      begin
//        lvRet := pvRequest.FClientContext.RawSocket.GetSendBufferLength;
//        if lvRet <> FSendBufCacheSize then
//        begin
//          Result := pvRequest.FClientContext.RawSocket.SetSendBufferLength(FSendBufCacheSize) <> SOCKET_ERROR;
//          if not Result then
//          begin
//            {$IFDEF DIOCP_DEBUG}
//            lvErrCode := GetLastError;
//
//            logMessage('FClientContext.FRawSocket.SetSendBufferLength, Error:%d', [lvErrCode]);
//            {$ENDIF}
//          end else
//          begin
//            lvRet := pvRequest.FClientContext.RawSocket.GetSendBufferLength;
//            if lvRet <> FSendBufCacheSize then
//            begin
//              logMessage('FClientContext.FRawSocket.SetSendBufferLength, Curr length:%d', [lvRet]);
//
//            end;
//          end;
//        end;
//      end;
//
//      if pvRequest.FClientContext.FRawSocket.GetNoDelayOption <> FNoDelayOption then
//      begin
//        Result := pvRequest.FClientContext.FRawSocket.SetNoDelayOption(FNoDelayOption);
//        if not Result then
//        begin
//          {$IFDEF DIOCP_DEBUG}
//          lvErrCode := GetLastError;
//
//          logMessage('FClientContext.FRawSocket.SetNoDelayOption, Error:%d', [lvErrCode]);
//          {$ENDIF}
//        end;
//      end;
//    end;

  end;
begin
  //LogMessage('DoAcceptExResponse_Start', CORE_DEBUG_FILE);
  if pvRequest.FErrorCode = 0 then
  begin
    if DoAfterAcceptEx then
    begin
      pvRequest.FClientContext.SetSocketState(ssConnected);
     {$IFDEF SOCKET_REUSE}
      pvRequest.FClientContext.DoConnected;
     {$ELSE}
      lvRet := FIocpEngine.IocpCore.Bind2IOCPHandle(
         pvRequest.FClientContext.FRawSocket.SocketHandle, 0);
      if lvRet = 0 then
      begin     // binding error

        lvErrCode := GetLastError;

        if pvRequest.FOwner.CanWork then
        begin
          {$IFDEF WRITE_LOG}
          logMessage(
              'bind2IOCPHandle(%d) in TDiocpTcpServer.DoAcceptExResponse occur Error :%d',
              [pvRequest.FClientContext.RawSocket.SocketHandle, lvErrCode]);
          {$ENDIF}
        end;
        DoClientContextError(pvRequest.FClientContext, lvErrCode);

        pvRequest.FClientContext.FRawSocket.Close;

        // relase client context object
        pvRequest.FClientContext.ReleaseBack;
        pvRequest.FClientContext := nil;
      end else
      begin
        pvRequest.FClientContext.DoConnected;
      end;
      {$ENDIF}
    end else
    begin
     {$IFDEF SOCKET_REUSE}
      pvRequest.FClientContext.FRawSocket.ShutDown;

      // post disconnectEx
      pvRequest.FClientContext.FDisconnectExRequest.DirectlyPost;
      pvRequest.FClientContext := nil;
     {$ELSE}
      pvRequest.FClientContext.FRawSocket.Close;

      // return to pool
      pvRequest.FClientContext.ReleaseBack;
      pvRequest.FClientContext := nil;
      {$ENDIF}
    end;
  end else
  begin
   {$IFDEF SOCKET_REUSE}
    
   {$ELSE}
    pvRequest.FClientContext.RawSocket.Close;
   {$ENDIF}
    // �黹�����������ĳ�
    pvRequest.FClientContext.ReleaseBack;
    pvRequest.FClientContext := nil;
  end;

  if FActive then
  begin
    pvRequest.FAcceptorMgr.PostAcceptExRequest;
    //LogMessage('DoAcceptExResponse_END_Active', CORE_DEBUG_FILE);
  end else
  begin
    ;
    //LogMessage('DoAcceptExResponse_END_UnActive', CORE_DEBUG_FILE);
  end;
end;

procedure TDiocpTcpServer.DoClientContextError(pvClientContext:
    TIocpClientContext; pvErrorCode: Integer);
begin
  if Assigned(FOnContextError) then
    FOnContextError(pvClientContext, pvErrorCode);
end;

procedure TDiocpTcpServer.DoReceiveData(pvIocpClientContext:TIocpClientContext;
    pvRequest:TIocpRecvRequest);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(pvIocpClientContext,
      pvRequest.FRecvBuffer.buf, pvRequest.FBytesTransferred,
      pvRequest.FErrorCode);
end;

function TDiocpTcpServer.FindContext(pvSocketHandle:TSocket):
    TIocpClientContext;
{$IFDEF USE_HASHTABLE}

{$ELSE}
var
  lvHash:Integer;
  lvObj:TIocpClientContext;
{$ENDIF}
begin
  FLocker.lock('FindContext');
  try
    {$IFDEF USE_HASHTABLE}
    Result := TIocpClientContext(FOnlineContextList.FindFirstData(pvSocketHandle));
    {$ELSE}
    Result := nil;
    lvHash := pvSocketHandle and SOCKET_HASH_SIZE;
    lvObj := FClientsHash[lvHash];
    while lvObj <> nil do
    begin
      if lvObj.FRawSocket.SocketHandle = pvSocketHandle then
      begin
        Result := lvObj;
        break;
      end;
      lvObj := lvObj.FNextForHash;
    end;
    {$ENDIF}
  finally
    FLocker.unLock;
  end;
end;


function TDiocpTcpServer.GetWorkerCount: Integer;
begin
  Result := FIocpEngine.WorkerCount;
end;

function TDiocpTcpServer.IsDestroying: Boolean;
begin                       // �̲߳���ȫ��
  Result := FIsDestroying;  // or (csDestroying in self.ComponentState)
end;

procedure TDiocpTcpServer.OnCreateClientContext(const context:
    TIocpClientContext);
begin
end;

procedure TDiocpTcpServer.Open;
begin
  SetActive(true);
end;

procedure TDiocpTcpServer.RegisterContextClass(pvContextClass:
    TIocpClientContextClass);
begin
  FClientContextClass := pvContextClass;
end;

procedure TDiocpTcpServer.RegisterSendRequestClass(pvClass:
    TIocpSendRequestClass);
begin
  FIocpSendRequestClass := pvClass;
end;

function TDiocpTcpServer.ReleaseRecvRequest(pvObject: TIocpRecvRequest):
    Boolean;
begin

  {$IFDEF DIOCP_DEBUG}
  
  pvObject.CheckThreadOut;

  if self = nil then
  begin
    Assert(False);
  end;

  if pvObject.RefCounter > 0 then
  begin
    Assert(pvObject.RefCounter  = 0);
  end;

 
  if FRecvRequestPool = nil then
  begin
    // check call stack is crash
    Assert(FRecvRequestPool <> nil);
  end;

  if IsDebugMode then
  begin
    Assert(pvObject.FAlive);
  end;
  {$ENDIF}
  if UseAsyncRecvQueue then
  begin
    if (pvObject.FInnerBuffer.buf <> nil) then
    begin
      ReleaseRef(PByte(pvObject.FInnerBuffer.buf));
      pvObject.FInnerBuffer.buf := nil;
      pvObject.FInnerBuffer.len := 0;
    end;
  end;

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
      pvObject.FClientContext := nil;

      FRecvRequestPool.EnQueue(pvObject);
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

function TDiocpTcpServer.ReleaseSendRequest(pvObject:TIocpSendRequest{$IFDEF DIOCP_DEBUG};pvCtx:TIocpClientContext{$ENDIF}): Boolean;
begin
{$IFDEF DIOCP_DEBUG}
  if pvObject.FOverlapped.RefCount <> 0 then
  begin        // ���ü����쳣
    sfLogger.logMessage('(%X)TDiocpTcpServer.ReleaseSendRequest:%s%s',
      [intPtr(pvCtx), sLineBreak, pvObject.GetDebugString], CORE_DEBUG_FILE, lgvError);

    if IsDebugMode then
    begin
      Assert(pvObject.FOverlapped.RefCount = 0);
    end;
  end;
  
  pvObject.AddDebugString('*', 'ReleaseSendRequest', pvCtx);

  
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

  pvObject.CheckThreadOut;
{$ENDIF}

  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then
  begin 
    if (FDataMoniter <> nil) then
    begin
      InterlockedIncrement(FDataMoniter.FSendRequestReturnCounter);
    end;

    if pvObject.FBuf <> nil then
    begin
      /// Buff�������, ��Ӧ�¼�
      DoSendBufferCompletedEvent(pvObject.FClientContext, pvObject.FBuf, pvObject.FLen, pvObject.Tag, pvObject.Data,
        pvObject.ErrorCode);
    end;

    if UseObjectPool then
    begin
      // ����Buffer
      pvObject.DoCleanUp;


    
      FSendRequestPool.EnQueue(pvObject);
    end else
    begin
      pvObject.Free;
    end;
    Result := true;

  end else
  begin
    Result := False;
{$IFDEF DIOCP_DEBUG}
    Assert(False);
{$ENDIF}
  end;

end;

procedure TDiocpTcpServer.RemoveFromOnOnlineList(pvObject: TIocpClientContext);
{$IFDEF USE_HASHTABLE}
  {$IFDEF DEBUG_ON}
    var
      lvSucc:Boolean;
  {$ENDIF}
{$ELSE}
var
  lvHash:Integer;
{$ENDIF}
begin
{$IFDEF USE_HASHTABLE}
  FLocker.lock('RemoveFromOnOnlineList');
  try
    {$IFDEF DEBUG_ON}
    lvSucc := FOnlineContextList.DeleteFirst(pvObject.FSocketHandle);
    Assert(lvSucc);
    {$ELSE}
    FOnlineContextList.DeleteFirst(pvObject.FSocketHandle);
    {$ENDIF}                                               
  finally
    FLocker.unLock;
  end;
{$ELSE} 
  FOnlineContextList.remove(pvObject);

  FLocker.lock('RemoveFromOnOnlineList');
  try
    // hash
    if pvObject.FPreForHash <> nil then
    begin
      pvObject.FPreForHash.FNextForHash := pvObject.FNextForHash;
      if pvObject.FNextForHash <> nil then
        pvObject.FNextForHash.FPreForHash := pvObject.FPreForHash;
    end else
    begin     // first ele
      lvHash := pvObject.RawSocket.SocketHandle and SOCKET_HASH_SIZE;
      FClientsHash[lvHash] := pvObject.FNextForHash;
      if FClientsHash[lvHash] <> nil then
        FClientsHash[lvHash].FPreForHash := nil;
    end;
  finally
    FLocker.unLock;
  end;

  pvObject.FNextForHash := nil;
  pvObject.FPreForHash := nil;
{$ENDIF}

end;

function TDiocpTcpServer.RequestContextDNA: Integer;
begin
  Result := InterlockedIncrement(FContextDNA);
end;

procedure TDiocpTcpServer.SafeStop;
begin
  if FActive then
  begin
    FClosingFlag := 1;
    try
      if FIocpEngine.WorkingCount = 0 then
      begin
        Assert(False);
      end;

      FListeners.Close;
      FDefaultListener.Close;

      DisconnectAll;

      // �ȵ����е�Ͷ�ݵ�AcceptEx����ع�
      // ��л Xjumping  990669769, ����bug
      FListeners.WaitForCancel(12000);
      FDefaultListener.WaitForCancel(10000);


      if not WaitForContext(20000) then
      begin  // wait time out
        Sleep(10);

        // �ȴ�Context�Ͽ���ʱ
        SafeWriteFileMsg('�ȴ�Context�Ͽ���ʱ', Self.Name + '_SafeStopTimeOut');
      end;

      FOnlineContextList.FreeAllDataAsObject;
      FOnlineContextList.Clear;

      FListeners.ClearObjects;
      FDefaultListener.FAcceptorMgr.ClearObjects;

      FSendRequestPool.FreeDataObject;
      FSendRequestPool.Clear;

      FRecvRequestPool.FreeDataObject;
      FRecvRequestPool.Clear;

      if Assigned(FRecvBuffLink) then
      begin
        FreeBufferPool(FRecvBuffLink);
        FRecvBuffLink := nil;
      end;

      DoAfterClose;

      /// �л����ر�״̬
      FActive := false;
    finally
      FClosingFlag := 0;
    end;
  end;
end;

procedure TDiocpTcpServer.SetActive(pvActive:Boolean);
begin
  if pvActive <> FActive then
  begin
    if pvActive then
    begin
      CheckOpen(100);

    end else
    begin
      SafeStop;
    end;
  end;
end;

procedure TDiocpTcpServer.SetMaxSendingQueueSize(pvSize:Integer);
begin
  if pvSize <= 0 then
  begin
    FMaxSendingQueueSize := 10;
  end else
  begin
    FMaxSendingQueueSize := pvSize;
  end;
end;

procedure TDiocpTcpServer.SetName(const NewName: TComponentName);
begin
  inherited;
  if FLogger.Appender is TLogFileAppender then
  begin
    if NewName <> '' then
    begin
      TLogFileAppender(FLogger.Appender).FilePreFix := NewName + '_';
    end;
  end;
end;

procedure TDiocpTcpServer.SetWorkerCount(const Value: Integer);
begin
  // ������Ĭ�����湤���̵߳�����
  // if FIocpEngine = __defaultDiocpEngine then Exit;

  FIocpEngine.SetWorkerCount(Value);
end;

procedure TDiocpTcpServer.CreateDataMonitor;
begin
  if FDataMoniter = nil then
  begin
    FDataMoniter := TIocpDataMonitor.Create;
  end;
end;

function TDiocpTcpServer.InnerCreateRecvRequest: TIocpRecvRequest;
begin
  Result := TIocpRecvRequest.Create;
  Result.FOwner := Self;
  if FDataMoniter <> nil then FDataMoniter.IncRecvRequestCreateCounter;

end;

function TDiocpTcpServer.InnerCreateSendRequest: TIocpSendRequest;
begin
  if FIocpSendRequestClass <> nil then
  begin
    Result := FIocpSendRequestClass.Create;
  end else
  begin
    Result := TIocpSendRequest.Create;
  end;
  if (FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FDataMoniter.FSendRequestCreateCounter);
  end;
end;

procedure TDiocpTcpServer.DoAfterClose;
begin
  
end;

procedure TDiocpTcpServer.DoAfterOpen;
begin

end;

procedure TDiocpTcpServer.DoBeforeOpen;
begin
  
end;

procedure TDiocpTcpServer.DoCleanUpSendRequest;
begin
  FSendRequestPool.FreeDataObject;
  FSendRequestPool.Clear;
end;

procedure TDiocpTcpServer.DoInnerCreate(pvInitalizeNum: Integer);
begin
  FKeepAliveTime := 10000;
  FListeners := TDiocpListeners.Create(Self);
  FDefaultListener := TDiocpListener.Create(Self);

  FDebugStrings := TStringList.Create;
  CheckWinSocketStart;
  FUseObjectPool := true;
  FContextDNA := 0;
  FLocker := TIocpLocker.Create('diocp_tcp_server');
  FLogger:=TSafeLogger.Create();
  FLogger.setAppender(TLogFileAppender.Create(True));

  // Ĭ�ϲ���������ѡ��
  FKeepAlive := False;
  

  FSendRequestPool := TBaseQueue.Create;
  FRecvRequestPool := TBaseQueue.Create;
    

  // ����Ĭ�ϵ�Diocp����
  StartDiocpEngine;
  FOwnerEngine := False;

  BindDiocpEngine(__defaultDiocpEngine, False);

  FOnlineContextList := TDHashTable.Create(10949);


  FMaxSendingQueueSize := 1000;

  // post wsaRecv block size
  FWSARecvBufferSize := 1024 * 4;

  {$IFDEF DEBUG_ON}

  {$ELSE}
  FLogger.LogFilter := [lgvError];
  {$ENDIF}
end;

procedure TDiocpTcpServer.DoSendBufferCompletedEvent(pvContext:
    TIocpClientContext; pvBuffer: Pointer; len: Cardinal; pvBufferTag: Integer;
    pvTagData: Pointer; pvErrorCode: Integer);
begin
  if pvContext <> nil then
  begin
    try
      pvContext.DoSendBufferCompleted(pvBuffer, len, pvBufferTag, pvTagData, pvErrorCode);
    except
      on e:Exception do
      begin
        LogMessage('pvContext.DoSendBufferCompleted error:' + e.Message, '', lgvError);
      end;
    end;
  end;

  if Assigned(FOnSendBufferCompleted) then
  begin
    try
      FOnSendBufferCompleted(pvContext, pvBuffer, len, pvBufferTag, pvTagData, pvErrorCode);
    except
      on e:Exception do
      begin
        LogMessage('DoSendBufferCompletedEvent error:' + e.Message, '', lgvError);
      end;
    end;
  end;  
end;

function TDiocpTcpServer.DoSetSocketOptn(pvRawSocket:TRawSocket): Boolean;
var
  lvRet, lvErrCode:Integer;
begin
  Result := True;
  if FKeepAlive then
  begin
    Result := SetKeepAlive(pvRawSocket.SocketHandle, FKeepAliveTime);
    if not Result then
    begin
      {$IFDEF DIOCP_DEBUG}
      lvErrCode := GetLastError;
      logMessage('FRawSocket.setKeepAliveOption, Error:%d', [lvErrCode]);
      {$ENDIF}
    end;
  end;

  if FSendBufCacheSize > 0 then
  begin
    lvRet := pvRawSocket.GetSendBufferLength;
    if lvRet <> FSendBufCacheSize then
    begin
      Result := pvRawSocket.SetSendBufferLength(FSendBufCacheSize) <> SOCKET_ERROR;
      if not Result then
      begin
        {$IFDEF DIOCP_DEBUG}
        lvErrCode := GetLastError;

        logMessage('FClientContext.FRawSocket.SetSendBufferLength, Error:%d', [lvErrCode]);
        {$ENDIF}
      end else
      begin
        lvRet := pvRawSocket.GetSendBufferLength;
        if lvRet <> FSendBufCacheSize then
        begin
          logMessage('FClientContext.FRawSocket.SetSendBufferLength, Curr length:%d', [lvRet]);

        end;
      end;
    end;
  end;

  if pvRawSocket.GetNoDelayOption <> FNoDelayOption then
  begin
    Result := pvRawSocket.SetNoDelayOption(FNoDelayOption);
    if not Result then
    begin
      {$IFDEF DIOCP_DEBUG}
      lvErrCode := GetLastError;

      logMessage('FClientContext.FRawSocket.SetNoDelayOption, Error:%d', [lvErrCode]);
      {$ENDIF}
    end;
  end;
end;

function TDiocpTcpServer.GetClientCount: Integer;
begin
  Result := FOnlineContextList.Count;
end;

function TDiocpTcpServer.GetContextWorkingInfo(pvTimeOut:Cardinal = 3000):
    String;
var
  lvList:TList;
  lvContext:TIocpClientContext;
  i:Integer;
  lvUseTime:Cardinal;
begin
  lvList := TList.Create;
  try
    Result := '';
    //SetCurrentThreadInfo('GetContextWorkingInfo::- 0');
    GetOnlineContextList(lvList);
    //SetCurrentThreadInfo('GetContextWorkingInfo::- 0.1');
    for i := 0 to lvList.Count - 1 do
    begin
      lvContext := TIocpClientContext(lvList[i]);
      lvUseTime := lvContext.CheckWorkingTick;
      if lvUseTime > pvTimeOut then
      begin
        Result := Result + Format('[%s:%d(t:%d)]:%s',
          [lvContext.RemoteAddr, lvContext.RemotePort, lvUseTime, lvContext.DebugInfo]) + sLineBreak;
      end;
    end;
    //SetCurrentThreadInfo('GetContextWorkingInfo::- end');
  finally
    lvList.Free;
  end;
end;

function TDiocpTcpServer.GetDebugString: String;
begin
  FLocker.lock();
  try
    Result := FDebugStrings.Text;
  finally
    FLocker.unLock;
  end;
end;

procedure TDiocpTcpServer.GetOnlineContextList(pvList:TList);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  FLocker.lock('GetOnlineContextList');
  try
    for I := 0 to FOnlineContextList.BucketSize - 1 do
    begin
      lvBucket := FOnlineContextList.Buckets[I];
      while lvBucket<>nil do
      begin
        if lvBucket.Data <> nil then
        begin
           pvList.Add(lvBucket.Data);
        end;
        lvBucket:=lvBucket.Next;
      end;
    end;
  finally
    FLocker.unLock;
  end;

end;

function TDiocpTcpServer.GetPrintDebugInfo: string;
begin
  Result := Format('recv request obj:%d', [Self.FRecvRequestPool.Size]);
end;

function TDiocpTcpServer.GetRecvRequest: TIocpRecvRequest;
var
  lvStep:Integer;
begin
  if UseObjectPool then
  begin
    Result := TIocpRecvRequest(FRecvRequestPool.DeQueue);
    lvStep := 0;
    if Result = nil then
    begin
      Result := InnerCreateRecvRequest;
      lvStep := 1;
    end;
  end else
  begin
    Result := InnerCreateRecvRequest;
    lvStep := 2;
  end;
  {$IFDEF DIOCP_DEBUG}
  if Result.RefCounter > 0 then
  begin
    Assert(Result.RefCounter = 0, Format('step:%d', [lvStep]));
  end;
  if Result.FAlive then
  begin
//    sfLogger.logMessage('TDiocpTcpServer.GetRecvRequest:%d, DebugInfo:%s',
//      [FReferenceCounter, FDebugStrings.Text], CORE_DEBUG_FILE, lgvError);
    Assert(False);
  end;
  Result.CheckThreadIn;
  {$ENDIF}
  Result.FPostSucc := 0;
  Result.Tag := 0;
  Result.FAlive := true;
  Result.AddRef;

  if UseAsyncRecvQueue then
  begin
    Result.FInnerBuffer.buf := PAnsiChar(GetBuffer(self.FRecvBuffLink));
    Result.FInnerBuffer.len := self.FRecvBuffLink.FBlockSize;
    AddRef(PByte(Result.FInnerBuffer.buf));
  end;

  if FDataMoniter <> nil then FDataMoniter.IncRecvRequestOutCounter;
end;

function TDiocpTcpServer.GetSendRequest: TIocpSendRequest;
begin
  {$IFDEF DIOCP_DEBUG}
  if Self = nil then
  begin
    if IsDebugMode then
    begin
      Assert(Self <> nil)
    end;
    Result := nil;
    Exit;
  end;
  {$ENDIF}

  if UseObjectPool then
  begin
    Result := TIocpSendRequest(FSendRequestPool.DeQueue);
  end else
  begin
    Result := nil;
  end;
  if Result = nil then
  begin
    Result := InnerCreateSendRequest;
  end;
  {$IFDEF DIOCP_DEBUG}
  Result.CheckThreadIn;
  Assert(not Result.FAlive);
  {$ENDIF}
  Result.FPostSucc := 0;
  Result.Tag := 0;
  Result.FAlive := true;
  //Result.DoCleanup;
  Result.FOwner := Self;
  if (FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FDataMoniter.FSendRequestOutCounter);
  end;
end;

function TDiocpTcpServer.GetStateInfo: String;
var
  lvStrings:TStrings;
begin
  Result := '';
  if FDataMoniter = nil then exit;
  lvStrings := TStringList.Create;
  try
    if Active then
    begin
      lvStrings.Add(strState_Active);
    end else
    begin
      lvStrings.Add(strState_Off);
    end;


    lvStrings.Add(Format(strRecv_PostInfo,
         [
           DataMoniter.PostWSARecvCounter,
           DataMoniter.ResponseWSARecvCounter,
           DataMoniter.PostWSARecvCounter -
           DataMoniter.ResponseWSARecvCounter,
           DataMoniter.Speed_WSARecvResponse
         ]
        ));


    lvStrings.Add(Format(strRecv_SizeInfo, [TransByteSize(DataMoniter.RecvSize)]));


    //  Format('post:%d, response:%d, recvd:%d',
    //     [
    //       FIocpTcpServer.DataMoniter.PostWSARecvCounter,
    //       FIocpTcpServer.DataMoniter.ResponseWSARecvCounter,
    //       FIocpTcpServer.DataMoniter.RecvSize
    //     ]
    //    );

    lvStrings.Add(Format(strSend_Info,
       [
         DataMoniter.PostWSASendCounter,
         DataMoniter.ResponseWSASendCounter,
         DataMoniter.PostWSASendCounter -
         DataMoniter.ResponseWSASendCounter,
         DataMoniter.Speed_WSASendResponse
       ]
      ));

    lvStrings.Add(Format(strSendRequest_Info,
       [
         DataMoniter.SendRequestCreateCounter,
         DataMoniter.SendRequestOutCounter,
         DataMoniter.SendRequestReturnCounter
       ]
      ));

    lvStrings.Add(Format(strSendQueue_Info,
       [
         DataMoniter.PushSendQueueCounter,
         DataMoniter.PostSendObjectCounter,
         DataMoniter.ResponseSendObjectCounter,
         DataMoniter.SendRequestAbortCounter
       ]
      ));

    lvStrings.Add(Format(strSend_SizeInfo, [TransByteSize(DataMoniter.SentSize)]));

    lvStrings.Add(Format(strAcceptEx_Info,
       [
         DataMoniter.PostWSAAcceptExCounter,
         DataMoniter.ResponseWSAAcceptExCounter
       ]
      ));

    lvStrings.Add(Format(strSocketHandle_Info,
       [
         DataMoniter.HandleCreateCounter,
         DataMoniter.HandleDestroyCounter
       ]
      ));

    lvStrings.Add(Format(strContext_Info,
       [
         DataMoniter.ContextCreateCounter,
         DataMoniter.ContextOutCounter,
         DataMoniter.ContextReturnCounter
       ]
      ));

    lvStrings.Add(Format(strOnline_Info, [ClientCount, DataMoniter.MaxOnlineCount]));

    lvStrings.Add(Format(strWorkers_Info, [WorkerCount]));

    lvStrings.Add(Format(strRunTime_Info, [GetRunTimeINfo]));

    Result := lvStrings.Text;
  finally
    lvStrings.Free;

  end;
end;

procedure TDiocpTcpServer.InnerAddToDebugStrings(const pvMsg: String);
begin
  FDebugStrings.Add(pvMsg);
  if FDebugStrings.Count > 500 then FDebugStrings.Delete(0);
end;

procedure TDiocpTcpServer.KickOut(pvTimeOut:Cardinal = 60000);
var
  //lvNowTickCount:Cardinal;
  I, j:Integer;
  lvContext:TIocpClientContext;
  lvKickOutList: array of TIocpClientContext;
{$IFDEF USE_HASHTABLE}
var    
  lvBucket, lvNextBucket: PDHashData;
{$ELSE}
  lvNextContext :TIocpClientContext;
{$ENDIF}
begin
  //lvNowTickCount := GetTickCount;
  {$IFDEF USE_HASHTABLE}
  FLocker.lock('KickOut');
  try
    j := 0;
    if FOnlineContextList.Count > 0 then
    begin
      SetLength(lvKickOutList, FOnlineContextList.Count);
    end else
    begin
      Exit;
    end;
    for I := 0 to FOnlineContextList.BucketSize - 1 do
    begin
      lvBucket := FOnlineContextList.Buckets[I];
      while lvBucket<>nil do
      begin
        lvNextBucket := lvBucket.Next;
        if lvBucket.Data <> nil then
        begin
          lvContext := TIocpClientContext(lvBucket.Data);
          if lvContext.FLastActivity <> 0 then
          begin
            if (lvContext.FBusingCounter = 0)    // �������(>0), �Ͳ�����KickOut
               and (tick_diff(lvContext.FLastActivity, GetTickCount) > pvTimeOut) then
            begin
              // ����ر�(�첽����ر�,��ֱ����RequestDisconnect()����ֱ���Ƴ�FOnlineContextList�б�)
              lvKickOutList[j] := lvContext;
              Inc(j);
            end;
          end;
        end;
        lvBucket:= lvNextBucket;
      end;
    end;

    for i := 0 to j - 1 do
    begin
      lvKickOutList[i].RequestDisconnect('��ʱ��������Ͽ�');
    end;
  finally
    FLocker.unLock;
  end;
  {$ELSE}
  FLocker.lock('KickOut');
  try
    lvContext := FOnlineContextList.FHead;

    // request all context discounnt
    while lvContext <> nil do
    begin
      lvNextContext := lvContext.FNext;
      if lvContext.FLastActivity <> 0 then
      begin
        if tick_diff(lvContext.FLastActivity, GetTickCount) > pvTimeOut then
        begin
          // ����ر�(�첽����ر�,��ֱ����RequestDisconnect()����ֱ���Ƴ�FOnlineContextList�б�)
          lvContext.PostWSACloseRequest();
        end;
      end;
      lvContext := lvNextContext;
    end;
  finally
    FLocker.unLock;
  end;
  {$ENDIF}
end;

procedure TDiocpTcpServer.OnIocpException(pvRequest:TIocpRequest; E:Exception);
begin
  try
    {$IFDEF DIOCP_DEBUG}
    if pvRequest <> nil then
    begin
      LogMessage('δ�����쳣:%s, ����(%s)��Ϣ:%s',[E.Message, pvRequest.ClassName, pvRequest.Remark],
        CORE_LOG_FILE, lgvError);
    end else
    begin
      LogMessage('δ�����쳣:%s',[E.Message], CORE_LOG_FILE, lgvError);
    end;
    {$ENDIF}
  except
  end;
end;

function TDiocpTcpServer.OnlineClientsCallBack(pvCallBack: TOnContextEvent;
    pvTag: Integer; pvTagData: Pointer): Integer;
var
  I:Integer;
  lvBucket: PDHashData;
  lvContext: TIocpClientContext;
begin
  Result := 0;
  FLocker.lock('GetOnlineContextList');
  try
    for I := 0 to FOnlineContextList.BucketSize - 1 do
    begin
      lvBucket := FOnlineContextList.Buckets[I];
      while lvBucket<>nil do
      begin
        if lvBucket.Data <> nil then
        begin
          lvContext := TIocpClientContext(lvBucket.Data);
          if lvContext.LockContext(STRING_EMPTY, nil) then
          try
            pvCallBack(lvContext, pvTag, pvTagData);
            Inc(Result);
          finally
            lvContext.UnLockContext(STRING_EMPTY, nil);
          end;
        end;
        lvBucket:=lvBucket.Next;
      end;
    end;
  finally
    FLocker.unLock;
  end;
  
end;

function TDiocpTcpServer.PostBufferToOnlineClients(pvBuf:Pointer;
    pvLen:Integer; pvCopyBuf: Boolean = true; pvTag: Integer = 0; pvTagData:
    Pointer = nil): Integer;
var
  I:Integer;
  lvBucket: PDHashData;
  lvContext: TIocpClientContext;
begin
  Result := 0;
  FLocker.lock('GetOnlineContextList');
  try
    for I := 0 to FOnlineContextList.BucketSize - 1 do
    begin
      lvBucket := FOnlineContextList.Buckets[I];
      while lvBucket<>nil do
      begin
        if lvBucket.Data <> nil then
        begin
          lvContext := TIocpClientContext(lvBucket.Data);
          if lvContext.PostWSASendRequest(pvBuf, pvlen, pvCopyBuf, pvTag, pvTagData) then
          begin
            Inc(Result);
          end;
        end;
        lvBucket:=lvBucket.Next;
      end;
    end;
  finally
    FLocker.unLock;
  end;
end;

procedure TDiocpTcpServer.SetUseAsyncRecvQueue(const Value: Boolean);
begin
  if self.Active then raise Exception.Create('����ģʽ�²����޸ĸ�ѡ��');
  if FUseAsyncRecvQueue <> Value then
  begin
    FUseAsyncRecvQueue := Value;
  end;
end;

procedure TDiocpTcpServer.SetWSARecvBufferSize(const Value: cardinal);
begin
  if self.Active then raise Exception.Create('����ģʽ�²����޸ĸ�ѡ��');
  FWSARecvBufferSize := Value;
  if FWSARecvBufferSize = 0 then
  begin
    FWSARecvBufferSize := 1024 * 4;
  end;
end;

function TDiocpTcpServer.TryLockContext(pvSocketHandle: TSocket; const
    pvDebugInfo: string; pvDebugObj: TObject): TIocpClientContext;
{$IFDEF USE_HASHTABLE}

{$ELSE}
var
  lvHash:Integer;
  lvObj:TIocpClientContext;
{$ENDIF}
begin
  FLocker.lock('FindContext');
  try
    {$IFDEF USE_HASHTABLE}
    Result := TIocpClientContext(FOnlineContextList.FindFirstData(pvSocketHandle));
    {$ELSE}
    Result := nil;
    lvHash := pvSocketHandle and SOCKET_HASH_SIZE;
    lvObj := FClientsHash[lvHash];
    while lvObj <> nil do
    begin
      if lvObj.FRawSocket.SocketHandle = pvSocketHandle then
      begin
        Result := lvObj;
        break;
      end;
      lvObj := lvObj.FNextForHash;
    end;
    {$ENDIF}

    if (Result <> nil) and (not Result.LockContext(pvDebugInfo, pvDebugObj)) then
    begin
      Result := nil;
    end;
  finally
    FLocker.unLock;
  end;
end;

function TDiocpTcpServer.WaitForContext(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
  c:Integer;
begin
  l := GetTickCount;
  c := FOnlineContextList.Count;
  while (c > 0) do
  begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}

    if GetTickCount - l > pvTimeOut then
    begin
      {$IFDEF WRITE_LOG}
      logMessage('WaitForContext End Current Online num:%d', [c], CORE_LOG_FILE, lgvError);
      {$ENDIF}
      Break;
    end;
    c := FOnlineContextList.Count;
  end;

  Result := FOnlineContextList.Count = 0;
end;

procedure TIocpAcceptorMgr.PostAcceptExRequest(pvNum: Integer);
var
  i:Integer;
begin
  Assert(FOwner <> nil);

  for i := 0 to pvNum -1 do
  begin
    PostAcceptExRequest();
  end;

end;

procedure TIocpAcceptorMgr.ClearObjects;
begin
  FAcceptExRequestPool.FreeDataObject;
  FAcceptExRequestPool.Clear;

  FContextPool.FreeDataObject;
  FContextPool.Clear;
end;

procedure TIocpAcceptorMgr.Close;
begin
  FListenSocket.Close();
end;

constructor TIocpAcceptorMgr.Create(AOwner: TDiocpTcpServer);
begin
  inherited Create;
  FContextPool := TBaseQueue.Create;
  FCount := 0;
  FOwner := AOwner;
  FListenSocket := TRawSocket.Create;

  FAcceptExRequestPool := TBaseQueue.Create;

end;

destructor TIocpAcceptorMgr.Destroy;
begin
  FListenSocket.Free;

  FContextPool.FreeDataObject;
  FContextPool.Free;

  FAcceptExRequestPool.FreeDataObject;
  FAcceptExRequestPool.Free;

  inherited Destroy;
end;

procedure TIocpAcceptorMgr.CheckCreatePoolObjects(pvMaxNum: Integer);
var
  i: Integer;
begin
  for i := FAcceptExRequestPool.Size to 110 do
  begin
    self.FAcceptExRequestPool.EnQueue(InnerCreateAcceptExRequest);
  end;

  for i := FContextPool.Size to pvMaxNum + 200 -1 do
  begin
    FContextPool.EnQueue(InnerCreateClientContext);
  end;
end;

function TIocpAcceptorMgr.GetClientContext: TIocpClientContext;
var
  lvClientClass:TIocpClientContextClass;
begin
  if not FOwner.FUseObjectPool then
  begin
    Result := InnerCreateClientContext;
  end else
  begin
    Result := TIocpClientContext(FContextPool.DeQueue);
    if Result = nil then
    begin
      Result := InnerCreateClientContext;
    end;
  end;
  if (FOwner.FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FOwner.FDataMoniter.FContextOutCounter);
  end;
  Result.FAlive := True;
  Result.DoCleanUp;
  Result.Owner := FOwner;
end;

procedure TIocpAcceptorMgr.PostAcceptExRequest;
var
  lvRequest:TIocpAcceptExRequest;
  i:Integer;
begin            
  if not FListenSocket.SocketValid then Exit;
  i :=0;
  {$IFDEF DIOCP_DEBUG}
  Assert(FOwner <> nil);
  {$ENDIF}
  if not self.FOwner.CanWork then Exit;

  try
    lvRequest := GetRequestObject;
{$IFDEF DIOCP_DEBUG}
    lvRequest.CheckThreadIn;
{$ENDIF}
    lvRequest.FClientContext := GetClientContext;
    lvRequest.FAcceptorMgr := Self;

    if lvRequest.PostRequest then
    begin
      if (FOwner.FDataMoniter <> nil) then
      begin
        InterlockedIncrement(FOwner.FDataMoniter.FPostWSAAcceptExCounter);
      end;
    end else
    begin     // post fail
      Inc(i);
      try
        // �����쳣��ֱ���ͷ�Context
        lvRequest.FClientContext.RawSocket.Close(False);
        lvRequest.FClientContext.FAlive := false;
        lvRequest.FClientContext.Free;
        lvRequest.FClientContext := nil;
      except
      end;
      // �黹�������
      ReleaseRequestObject(lvRequest);

      {$IFDEF DIOCP_DEBUG}
      FOwner.logMessage('TIocpAcceptorMgr.PostAcceptExRequest errCounter:%d', [i], CORE_LOG_FILE);
      {$ENDIF}
    end;
  except
    on E:Exception do
    begin
       {$IFDEF DIOCP_DEBUG}
       FOwner.logMessage('TIocpAcceptorMgr.PostAcceptExRequest Err:%s', [e.Message], CORE_LOG_FILE);
       {$ENDIF}
    end;
  end;
end;

function TIocpAcceptorMgr.GetRequestObject: TIocpAcceptExRequest;
begin
  if FOwner.FUseObjectPool then
  begin
    Result := TIocpAcceptExRequest(FAcceptExRequestPool.DeQueue);
  end else
  begin
    Result := nil;
  end;
  
  if Result = nil then
  begin
    Result := InnerCreateAcceptExRequest;
  end;
  InterlockedIncrement(FCount);
end;

 procedure TIocpAcceptorMgr.ReleaseRequestObject(pvRequest:TIocpAcceptExRequest);
 begin
 {$IFDEF DIOCP_DEBUG}
   pvRequest.CheckThreadOut;
 {$ENDIF}
   pvRequest.FAcceptorMgr := nil;
   pvRequest.FClientContext := nil;
   if FOwner.FUseObjectPool then
   begin
     FAcceptExRequestPool.EnQueue(pvRequest);
   end else
   begin
     pvRequest.Free;
   end;
   InterlockedDecrement(FCount);
 end;

function TIocpAcceptorMgr.InnerCreateAcceptExRequest: TIocpAcceptExRequest;
begin
  Result := TIocpAcceptExRequest.Create(FOwner);
  if (FOwner.FDataMoniter <> nil) then
    FOwner.DataMoniter.IncAcceptExObjectCounter;
end;

function TIocpAcceptorMgr.InnerCreateClientContext: TIocpClientContext;
var
  lvClientClass:TIocpClientContextClass;
begin
  lvClientClass := FClientContextClass;
  if lvClientClass = nil then lvClientClass := FOwner.FClientContextClass;
  if lvClientClass = nil then lvClientClass := TIocpClientContext;

  Result := lvClientClass.Create;
  Result.FAcceptorMgr := Self;
  FOwner.OnCreateClientContext(Result);
  if (FOwner.FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FOwner.FDataMoniter.FContextCreateCounter);
  end;
  Result.FSendRequestLink.SetMaxSize(FOwner.FMaxSendingQueueSize);
end;

procedure TIocpAcceptorMgr.RegisterContextClass(pvContextClass:
    TIocpClientContextClass);
begin
  FClientContextClass := pvContextClass;
end;

function TIocpAcceptorMgr.ReleaseClientContext(pvObject:TIocpClientContext):
    Boolean;
begin
{$IFDEF DIOCP_DEBUG}
  if pvObject.FRecvQueue.Size > 0 then
  begin
    Assert(pvObject.FRecvQueue.Size = 0);
  end;

  if pvObject.FWSARecvRef > 0 then
  begin
     Assert(pvObject.FWSARecvRef = 0, Format('ReleaseClientContext:OnRecv�¼���δ����, FWSARecvRef:%d',[pvObject.FWSARecvRef]));
  end;
{$ENDIF}

  if not FOwner.FUseObjectPool then
  begin
    pvObject.Free;
    Result := true;
    if (FOwner.FDataMoniter <> nil) then
      InterlockedIncrement(FOwner.FDataMoniter.FContextReturnCounter);
  end else
  begin
    if lock_cmp_exchange(True, False, pvObject.FAlive) = true then
    begin
      pvObject.DoCleanUp;
      FContextPool.EnQueue(pvObject);
      if (FOwner.FDataMoniter <> nil) then
        InterlockedIncrement(FOwner.FDataMoniter.FContextReturnCounter);

      Result := true;
    end else
    begin
      Result := false;
    end;
  end;
end;


function TIocpAcceptorMgr.WaitForCancel(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
begin
  l := GetTickCount;
  while (FCount > 0) do
  begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}

    if GetTickCount - l > pvTimeOut then
    begin
      {$IFDEF WRITE_LOG}

      FOwner.logMessage('WaitForCancel End Current AccepEx num:%d', [FCount], CORE_LOG_FILE, lgvError);
      FOwner.logMessage('WaitForCancel false:' + sLineBreak +  FOwner.IocpEngine.GetStateINfo, CORE_LOG_FILE, lgvError);

      {$ENDIF}
      Break;
    end;
  end;

  Result := FCount = 0;
end;

constructor TIocpAcceptExRequest.Create(AOwner: TDiocpTcpServer);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TIocpAcceptExRequest.GetPeerINfo;
var
  localAddr: PSockAddr;
  remoteAddr: PSockAddr;
  localAddrSize : Integer;
  remoteAddrSize : Integer;
begin
  if self.FAcceptorMgr.FListenSocket.IPVersion = IP_V6 then
  begin
    localAddrSize := SizeOf(TSockAddrIn6) + ADDRESS_LENGTH_EX;
    remoteAddrSize := SizeOf(TSockAddrIn6) + ADDRESS_LENGTH_EX;
    IocpGetAcceptExSockaddrs(@FAcceptBuffer[0],
                          0,
                          SizeOf(TSockAddrIn6) + ADDRESS_LENGTH_EX,
                          SizeOf(TSockAddrIn6) + ADDRESS_LENGTH_EX,
                          localAddr,
                          localAddrSize,
                          remoteAddr,
                          remoteAddrSize);
    //remoteAddr := PSockAddr(@FAcceptBuffer[10]);

    FClientContext.FRemoteAddr := TranslateTInAddrToString(PSockAddrIn6(remoteAddr).sin6_addr, IP_V6);
    FClientContext.FRemotePort := ntohs(PSockAddrIn6(remoteAddr).sin6_port);
  end else
  begin
    localAddrSize := SizeOf(TSockAddr) + ADDRESS_LENGTH_EX;
    remoteAddrSize := SizeOf(TSockAddr) + ADDRESS_LENGTH_EX;
    IocpGetAcceptExSockaddrs(@FAcceptBuffer[0],
                          0,
                          SizeOf(TSockAddrIn) + ADDRESS_LENGTH_EX,
                          SizeOf(TSockAddrIn) + ADDRESS_LENGTH_EX,
                          localAddr,
                          localAddrSize,
                          remoteAddr,
                          remoteAddrSize);

    FClientContext.FRemoteAddr := string(inet_ntoa(TSockAddrIn(remoteAddr^).sin_addr));
    FClientContext.FRemotePort := ntohs(TSockAddrIn(remoteAddr^).sin_port);
  end;
end;

procedure TIocpAcceptExRequest.HandleResponse;
begin
  Assert(FOwner <> nil);
  ///
  if (FOwner.FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FOwner.FDataMoniter.FResponseWSAAcceptExCounter);
  end;

  if FErrorCode = 0 then
  begin
    // msdn
    // The socket sAcceptSocket does not inherit the properties of the socket
    //  associated with sListenSocket parameter until SO_UPDATE_ACCEPT_CONTEXT
    //  is set on the socket.
    FAcceptorMgr.FListenSocket.UpdateAcceptContext(FClientContext.FRawSocket.SocketHandle);

    GetPeerINfo();
  end;
  FOwner.DoAcceptExResponse(Self);
end;

function TIocpAcceptExRequest.PostRequest: Boolean;
var
  dwBytes: Cardinal;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:POverlapped;
  {$IFDEF SOCKET_REUSE}
  lvRetCode:Integer;
  {$ENDIF}
  lvListenSocket:TRawSocket;
begin
  lvListenSocket := FAcceptorMgr.ListenSocket;
  {$IFDEF SOCKET_REUSE}
  if
    (FClientContext.FRawSocket.SocketHandle = INVALID_SOCKET)
    or
    (FClientContext.FRawSocket.SocketHandle = 0) then
  begin
    if (FOwner.FDataMoniter <> nil) then
      FOwner.FDataMoniter.incHandleCreateCounter;
    FClientContext.FRawSocket.IPVersion := lvListenSocket.IPVersion;
    FClientContext.FRawSocket.CreateTcpOverlappedSocket;
    FClientContext.OnAfterCreateSocket;

    lvRetCode := FOwner.IocpEngine.IocpCore.Bind2IOCPHandle(
      FClientContext.FRawSocket.SocketHandle, 0);
    if lvRetCode = 0 then
    begin     // binding error
      lvErrCode := GetLastError;
      {$IFDEF DIOCP_DEBUG}
      FOwner.logMessage(
         Format(strAcceptExError,
           [FClientContext.FRawSocket.SocketHandle, lvErrCode, 'TIocpAcceptExRequest.PostRequest(SOCKET_REUSE)'])
         , CORE_LOG_FILE);
      {$ENDIF}
      FClientContext.FRawSocket.close;
      if (FOwner.FDataMoniter <> nil) then
        FOwner.FDataMoniter.incHandleDestroyCounter;
      Result := false;
      Exit;
    end;
  end;
  {$ELSE}
  FClientContext.FRawSocket.IPVersion := lvListenSocket.IPVersion;
  FClientContext.FRawSocket.CreateTcpOverlappedSocket;
  FClientContext.OnAfterCreateSocket;
  {$ENDIF}
  dwBytes := 0;
  lp := @FOverlapped;

  FClientContext.SetSocketState(ssAccepting);

  if FClientContext.RawSocket.IPVersion = IP_V6 then
  begin
    lvRet := IocpAcceptEx(lvListenSocket.SocketHandle
                  , FClientContext.FRawSocket.SocketHandle
                  , @FAcceptBuffer[0]
                  , 0
                  , SizeOf(TSockAddrIn6) + ADDRESS_LENGTH_EX
                  , SizeOf(TSockAddrIn6) + ADDRESS_LENGTH_EX
                  , dwBytes
                  , lp);
  end else
  begin
    lvRet := IocpAcceptEx(lvListenSocket.SocketHandle
                  , FClientContext.FRawSocket.SocketHandle
                  , @FAcceptBuffer[0]
                  , 0
                  , SizeOf(TSockAddrIn) + ADDRESS_LENGTH_EX
                  , SizeOf(TSockAddrIn) + ADDRESS_LENGTH_EX
                  , dwBytes
                  , lp);
  end;
  if not lvRet then
  begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then
    begin
      {$IFDEF DIOCP_DEBUG}
      FOwner.logMessage(
         Format(strAcceptExError,
           [FClientContext.FRawSocket.SocketHandle, lvErrCode, 'TIocpAcceptExRequest.PostRequest'])
         , CORE_LOG_FILE);
      {$ENDIF}


      FOwner.DoClientContextError(FClientContext, lvErrCode);

      /// destroy socket
      FClientContext.RawSocket.close;
    end;
  end else
  begin
    Result := True;
  end;
end;

procedure TIocpAcceptExRequest.ResponseDone;
begin
  inherited;
  FAcceptorMgr.ReleaseRequestObject(Self);
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

constructor TIocpRecvRequest.Create;
begin
  inherited Create;
end;

destructor TIocpRecvRequest.Destroy;
begin
  if FInnerBuffer.len > 0 then
  begin
    if __debugFlag <> 0 then
    begin
      Assert(false, 'error');
    end;
    FreeMem(FInnerBuffer.buf, FInnerBuffer.len);
  end;
  __debugFlag := GetCurrentThreadID;
  inherited Destroy;
end;

procedure TIocpRecvRequest.DoRelease;
var
  lvContext : TIocpClientContext;
  lvPostSucc:Boolean;
begin
  inherited;
  lvPostSucc := False;
  lvContext := FClientContext;
  try
    {$IFDEF DIOCP_DEBUG}
    lvContext.InnerLock;
    lvContext.AddDebugString(Format('*(%d):%d,RefCnt:%d TIocpRecvRequest.DoRelease', [lvContext.FReferenceCounter, IntPtr(Self), self.RefCounter]));
    lvContext.InnerUnLock;
    {$ENDIF}
    lvPostSucc := self.FPostSucc = 1;
    
    // ִ�к󣬻ع鵽����أ�self�Ͳ��ڰ�ȫ��
    FOwner.ReleaseRecvRequest(Self);
  finally
    if lvPostSucc then  // ���û��Ͷ�ݳɹ�, �Ѿ�������DecRef, (����Ͷ��ʧ��ֱ��ʹ�õ�DecRecRef)
      lvContext.DecReferenceCounter('TIocpRecvRequest.WSARecvRequest.Response DoRelease', Self);
  end;
end;

procedure TIocpRecvRequest.HandleResponse;
var
  lvDNACounter:Integer;
  lvDebugInfo:String;
  lvDebugStep:Integer;
begin
  // �ù���ִ���У�����ȷ�����ᱻ�ر�
  //   FClientCtx���ᱻ�ع�
  //   ��Ͷ��֮ǰ����������
  lvDebugStep := 1;
  lvDNACounter := Self.FCounter;

  {$IFDEF DIOCP_DEBUG}  
  InterlockedDecrement(FOverlapped.RefCount);
  if FOverlapped.RefCount <> 0 then
  begin        // ���ü����쳣
    if IsDebugMode then
    begin
      Assert(FOverlapped.RefCount <>0);
    end;
    FOwner.logMessage(strRecvResponseErr,
        [Integer(self.FClientContext), Integer(Self), FOverlapped.RefCount],
        CORE_LOG_FILE, lgvError);
  end;
  {$ENDIF}


  try
    FClientContext.IncRecvRef;

    Assert(FOwner <> nil);
    try
      {$IFDEF TRACE_IOCP_RECV}
      TByteTools.AppendBufToFile(FRecvBuffer.buf,
        FBytesTransferred, Format('%s_%d_%s.recv',
        [FClientContext.RemoteAddr, FClientContext.RemotePort, FormatDateTime('hhnnsszzz', Now())]));
      {$ENDIF}

      FClientContext.FCurrRecvRequest := Self;
      lvDebugStep := 2;
      if (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.IncResponseWSARecvCounter;
        FOwner.FDataMoniter.IncRecvdSize(FBytesTransferred);
      end;

      if not FOwner.Active then
      begin
        lvDebugStep := 10;
        {$IFDEF WRITE_LOG}
        FOwner.logMessage(
            Format(strRecvEngineOff, [FClientContext.FSocketHandle])
          );
        {$ENDIF}
        // ��������ظ�Ͷ�ݽ�������
        FClientContext.RequestDisconnect(
          Format(strRecvEngineOff, [FClientContext.FSocketHandle])
          , Self);
        lvDebugStep := 19;
      end else if FErrorCode <> 0 then
      begin
        lvDebugStep := 20;
        if not FClientContext.FRequestDisconnectFlag then
        begin   // �������رգ����������־,�ʹ�������
          {$IFDEF WRITE_LOG}
          FOwner.logMessage(
            Format(strRecvError, [FClientContext.FSocketHandle, FErrorCode])
            );
          {$ENDIF}
          FOwner.DoClientContextError(FClientContext, FErrorCode);
          FClientContext.RequestDisconnect(
            Format(strRecvError, [FClientContext.FSocketHandle, FErrorCode])
            ,  Self);
        end;
        lvDebugStep := 29;
      end else if (FBytesTransferred = 0) then
      begin      // no data recvd, socket is break
        lvDebugStep := 30;
        if not FClientContext.FRequestDisconnectFlag then
        begin
          FClientContext.RequestDisconnect(
            Format(strRecvZero,  [FClientContext.FSocketHandle]),  Self);
        end;
        lvDebugStep := 35;
      end else
      begin
        lvDebugStep := 40;
        FClientContext.InnerDoRecv(Self);
        lvDebugStep := 49;
      end;
    finally
      lvDebugInfo := FDebugInfo;

      // PostWSARecv before decReferenceCounter
      FClientContext.DecRecvRef;



      // response done�� Dec
      // may return to pool
//      FClientContext.DecReferenceCounter(
//        Format('TIocpRecvRequest.WSARecvRequest.HandleResponse, DNACounter:%d, debugInfo:%s, refcount:%d',
//          [lvDNACounter, lvDebugInfo, lvRefCount]), Self);

  //  for debug context DebugStrings
  //    if FClientContext.FRequestDisconnect then
  //    begin
  //      lvBreak := true;
  //    end else
  //    begin
  //      lvBreak := False
  //    end;
  //    // may return to pool
  //    FClientContext.decReferenceCounter(
  //      Format('TIocpRecvRequest.WSARecvRequest.HandleResponse, DNACounter:%d, debugInfo:%s, refcount:%d',
  //        [lvDNACounter, FDebugInfo, FOverlapped.refCount]), Self);
  //    if lvBreak then
  //    begin
  //      FClientContext.PostWSARecvRequest;
  //    end;

    end;
  except
    on E:Exception do
    begin
     {$IFDEF DIOCP_DEBUG}
      __svrLogger.logMessage(
        Format('TIocpRecvRequest.WSARecvRequest.HandleResponse, DNACounter:%d, debugInfo:%s, step:%d, overlapped.refcount:%d, emsg:%s',
          [lvDNACounter, FDebugInfo, lvDebugStep, FOverlapped.RefCount, e.Message]));
     {$ENDIF}
    end;

  end;
end;

function TIocpRecvRequest.PostRecvRequest(pvBuffer:PAnsiChar; len:Cardinal):
    Boolean;
var
  lvRet, lvDNACounter:Integer;
  lpNumberOfBytesRecvd: Cardinal;
  lvOwner:TDiocpTcpServer;
begin
  Result := False;
  lpNumberOfBytesRecvd := 0;
  FRecvdFlag := 0;

  lvOwner := self.FOwner;

  FRecvBuffer.buf := pvBuffer;
  FRecvBuffer.len := len;

  {$IFDEF DIOCP_DEBUG}lvDNACounter := {$ENDIF}InterlockedIncrement(FCounter);
  {$IFDEF DIOCP_DEBUG}
  if FClientContext.IncReferenceCounter(Format(
    'TIocpRecvRequest.WSARecvRequest.Post, DNACounter:%d', [lvDNACounter]), Self) then
  {$ELSE}
  if FClientContext.IncReferenceCounter(STRING_EMPTY, nil) then
  {$ENDIF}
  begin
    {$IFDEF DIOCP_DEBUG}
    if FOverlapped.RefCount <> 0 then
    begin
      Assert(FOverlapped.RefCount = 0);
    end;
    InterlockedIncrement(FOverlapped.refCount);
    {$ENDIF}

    self.FPostSucc := 1;

    FDebugInfo := IntToStr(intPtr(FClientContext));
    lvRet := diocp_winapi_winsock2.WSARecv(FClientContext.FRawSocket.SocketHandle,
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
        self.FPostSucc := 2;
        {$IFDEF WRITE_LOG}
        lvOwner.logMessage(strRecvPostError, [FClientContext.SocketHandle, lvRet]);
        {$ENDIF}
        {$IFDEF DIOCP_DEBUG}
        InterlockedDecrement(FOverlapped.refCount);
        {$ENDIF}

        // trigger error event
        lvOwner.DoClientContextError(FClientContext, lvRet);

        // decReferenceCounter
        {$IFDEF DIOCP_DEBUG}
        FClientContext.DecReferenceCounterAndRequestDisconnect(
          Format('TIocpRecvRequest.WSARecvRequest.Error:%d', [lvRet]), Self);
        {$ELSE}
        FClientContext.DecReferenceCounterAndRequestDisconnect(STRING_EMPTY, Self);
        {$ENDIF}

      end else
      begin
        if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then
        begin
          lvOwner.FDataMoniter.incPostWSARecvCounter;
        end;
      end;
    end else
    begin
      Result := True;
    
      if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then
      begin
        lvOwner.FDataMoniter.incPostWSARecvCounter;
      end;
    end;   
  end;
end;

procedure TIocpRecvRequest.ResponseDone;
var
  lvContext:TIocpClientContext;
begin
  inherited;
{$IFDEF DIOCP_DEBUG}
  lvContext := self.FClientContext;

  if FOwner = nil then
  begin
    if IsDebugMode then
    begin
      Assert(FOwner <> nil);
      Assert(Self.FAlive);
    end;
  end;
//  lvContext.InnerLock;
//  lvContext.AddDebugString(Format('*(%d):%d,RefCnt:%d TIocpRecvRequest.ResponseDone', [lvContext.FReferenceCounter, IntPtr(Self), self.RefCounter]));
//  lvContext.InnerUnLock;
{$ENDIF}

  self.DecRef;

end;

function TIocpRecvRequest.PostRecvRequest: Boolean;
begin
  CheckCreateRecvBuffer;
  Result := PostRecvRequest(FInnerBuffer.buf, FInnerBuffer.len);
end;

function TIocpSendRequest.ExecuteSend: Integer;
begin
  if Tag = -1 then
  begin
    FLastMsg := strWSACloseRequest;
    Result := -2;
  end else if (FBuf = nil) or (FLen = 0) then
  begin
    FLastMsg := strWSACloseRequest;
    Result := -2;
  end else
  begin
    if InnerPostRequest(FBuf, FLen) then
    begin
      Result := 0;
    end else
    begin
      Result := -1;
    end;
  end;

end;

procedure TIocpSendRequest.CheckClearSendBuffer;
begin
  if FLen > 0 then
  begin
    case FSendBufferReleaseType of
      dtDispose: Dispose(FBuf);
      dtFreeMem: FreeMem(FBuf);
      dtFreeTagDataAsObject :
        begin
          TObject(self.Data).Free;
          self.Data := nil;
        end;
      dtBufReleaseRef:
        begin
          ReleaseRef(FBuf);
        end;
    end;
  end;
  FWSABuf.len := 0;
  FWSABuf.buf := nil;
  FBuf := nil;
  FSendBufferReleaseType := dtNone;
  FLen := 0;
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

procedure TIocpSendRequest.DoCleanUp;
begin
  CheckClearSendBuffer;
  FBytesSize := 0;
  FNext := nil;
  FOwner := nil;
  FClientContext := nil;
  FReponseState := 0;
  Tag := 0;


  //FMaxSize := 0;
end;

procedure TIocpSendRequest.HandleResponse;
var
  lvContext:TIocpClientContext;
begin
  lvContext := FClientContext;
  FIsBusying := false;
  try
    Assert(FOwner<> nil);
    if (FOwner.FDataMoniter <> nil) then
    begin                                                       
      FOwner.FDataMoniter.incSentSize(FBytesTransferred);
      FOwner.FDataMoniter.incResponseWSASendCounter;
    end;



    // ��Ӧ����¼�
    lvContext.DoSendRequestRespnonse(Self);

    if not FOwner.Active then
    begin
      FReponseState := 4;
      {$IFDEF WRITE_LOG}
      FOwner.logMessage(
          Format(strSendEngineOff, [FClientContext.FSocketHandle])
          );
      {$ENDIF}
      // avoid postWSARecv
      FClientContext.RequestDisconnect(
        Format(strSendEngineOff, [FClientContext.FSocketHandle])
        , Self);
    end else if FErrorCode <> 0 then
    begin
      FReponseState := 3;

      if not FClientContext.FRequestDisconnectFlag then
      begin   // �������رգ����������־,�ʹ�������
        {$IFDEF WRITE_LOG}
        FOwner.logMessage(
            Format(strSendErr, [FClientContext.FSocketHandle, FErrorCode])
            );
        {$ENDIF}
        FOwner.DoClientContextError(FClientContext, FErrorCode);
        FClientContext.RequestDisconnect(
           Format(strSendErr, [FClientContext.FSocketHandle, FErrorCode])
            , Self);
      end;
    end else if FBytesTransferred <> FBytesSize then
    begin
      FClientContext.RequestDisconnect(
        Format(strSendSizeErr, [FClientContext.FSocketHandle, FBytesSize, FBytesTransferred])
        , Self);
    end else
    begin
      FReponseState := 2;
      if FOwner.FDataMoniter <> nil then
      begin
        FOwner.FDataMoniter.IncResponseSendObjectCounter;
      end;

      if Assigned(FOnDataRequestCompleted) then
      begin
        FOnDataRequestCompleted(FClientContext, Self);
      end;

      FClientContext.DoSendRequestCompleted(Self);

      {$IFDEF DIRECT_SEND}
      {$ELSE}
      FClientContext.PostNextSendRequest;   // �麯���������context���Լ̳��ع�
      {$ENDIF}
    end;
  finally

//    if FClientContext = nil then
//    begin
//      Assert(False);
//      FReponseState := lvResponseState;
//    end;
    // response done�� Dec

  end;
end;

function TIocpSendRequest.InnerPostRequest(buf: Pointer; len: Cardinal):
    Boolean;
var
  lvErrorCode, lvRet: Integer;
  dwFlag: Cardinal;
  lpNumberOfBytesSent:Cardinal;
  lvContext:TIocpClientContext;
  lvOwner:TDiocpTcpServer;
begin
  Result := false;
  FIsBusying := True;
  FBytesSize := len;
  FWSABuf.buf := buf;
  FWSABuf.len := len;
  dwFlag := 0;
  lpNumberOfBytesSent := 0;

  // maybe on HandleResonse and release self
  lvOwner := FOwner;

  lvContext := FClientContext;


  // �������û�������ɹ�
  {$IFDEF DIOCP_DEBUG}
  if lvContext.IncReferenceCounter('InnerPostRequest::WSASend_Start', self) then
  {$ELSE}
  if lvContext.IncReferenceCounter(STRING_EMPTY, self) then
  {$ENDIF}
  try
    {$IFDEF DIOCP_DEBUG}
    if lvContext.FSendQueueSize > 0 then
    begin
      Assert(lvContext.FSendQueueSize > 0, '���Ͷ��߳�');
    end;
    {$ENDIF}

    {$IFDEF TRACE_IOCP_SEND}
    TByteTools.AppendBufToFile(buf, len, Format('%s_%d_%s.send', [lvContext.RemoteAddr, lvContext.RemotePort, FormatDateTime('hhnnsszzz', Now())]));
    {$ENDIF}
    // Ͷ�ݳɹ�
    AtomicIncrement(lvContext.FSendQueueSize);
    self.FPostSucc := 1;

    {$IFDEF DIOCP_DEBUG}
    InterlockedIncrement(FOverlapped.refCount);
    self.AddDebugString('+', 'InnerPostRequest::WSASend_Start', lvContext);
    if FOverlapped.RefCount <> 1 then
    begin  
      sfLogger.logMessage('(%X)TIocpClientContext.DecReferenceCounter:%d, DebugInfo:%s%s',
        [intPtr(lvContext), lvContext.FReferenceCounter, sLineBreak, self.GetDebugString], CORE_DEBUG_FILE, lgvError);
      Assert(FOverlapped.RefCount = 1);
    end;
    {$ENDIF}
    
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
       self.FPostSucc := 2;
       // Ͷ��ʧ��
       AtomicDecrement(lvContext.FSendQueueSize);
       FIsBusying := False;
       {$IFDEF WRITE_LOG}
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
      {$IFDEF DIOCP_DEBUG}
      if IsDebugMode then
      begin
        Assert(lvContext = FClientContext);
      end;
      InterlockedDecrement(FOverlapped.RefCount);
      self.AddDebugString('-', Format('InnerPostRequest::WSASend_Fail, ErrorCode:%d', [lvErrorCode]), lvContext);

      if FOverlapped.RefCount <> 0 then
      begin        // ���ü����쳣
        if IsDebugMode then
        begin
          Assert(FOverlapped.RefCount = 0);
        end;
        FOwner.logMessage(strRecvResponseErr,
            [Integer(self.FClientContext), Integer(Self), FOverlapped.RefCount],
            CORE_LOG_FILE, lgvError);
      end;

      lvContext.DecReferenceCounter(
        Format('InnerPostRequest::WSASend_Fail, ErrorCode:%d', [lvErrorCode])
         , Self);
     {$ELSE}
      lvContext.DecReferenceCounter(STRING_EMPTY, Self);
     {$ENDIF}                                           
    end;
  end;
end;

procedure TIocpSendRequest.ResponseDone;
var
  lvContext:TIocpClientContext;
begin
  inherited;
  lvContext := FClientContext;
{$IFDEF DIOCP_DEBUG}
  if FOwner = nil then
  begin
    if IsDebugMode then
    begin
      Assert(FOwner <> nil);
      Assert(Self.FAlive);
    end;
  end;
  Assert(lvContext <> nil);

  InterlockedDecrement(FOverlapped.RefCount);
  self.AddDebugString('-', Format('ResponseDone, ErrorCode:%d', [self.ErrorCode]), lvContext);
  if FOverlapped.RefCount <> 0 then
  begin        // ���ü����쳣
    if IsDebugMode then
    begin
      Assert(FOverlapped.RefCount = 0);
    end;
    FOwner.logMessage(strRecvResponseErr,
        [Integer(self.FClientContext), Integer(Self), FOverlapped.RefCount],
        CORE_LOG_FILE, lgvError);
  end;


{$ENDIF}


  try
    {$IFDEF DIOCP_DEBUG}
    lvContext.InnerLock;
    lvContext.AddDebugString(Format('*(%d):%d,RefCnt:%d ResponseDone.DoRelease', [lvContext.FReferenceCounter, IntPtr(Self), self.RefCounter]));
    lvContext.InnerUnLock;


    {$ENDIF}
    FOwner.ReleaseSendRequest(Self{$IFDEF DIOCP_DEBUG},lvContext{$ENDIF});
  finally
    lvContext.DecReferenceCounter('TIocpSendRequest.WSASendRequest.Response Done', Self);
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

procedure TIocpSendRequest.SetBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = true);
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

//
//  if pvCopyBuf then
//  begin
//    if FCopyBuf.len > 0 then FreeMem(FCopyBuf.buf);
//
//    FCopyBuf.len := len;
//    GetMem(FCopyBuf.buf, FCopyBuf.len);
//    Move(buf^, FCopyBuf.buf^, FCopyBuf.len);
//    FBuf := FCopyBuf.buf;
//    FLen := FCopyBuf.len;
//  end else
//  begin
//    FBuf := buf;
//    FLen := len;
//  end;
//  FPosition := 0;
end;

procedure TIocpSendRequest.UnBindingSendBuffer;
begin
  FBuf := nil;
  FLen := 0;
  FSendBufferReleaseType := dtNone;
end;

function TIocpSendRequest.GetStateINfo: String;
begin
  Result :=Format('%s %s', [Self.ClassName, self.Remark]);
  if FResponding then
  begin
    Result :=Result + sLineBreak + Format('start:%s, datalen:%d, max:%d',
      [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime), FWSABuf.len, FMaxSize]);
  end else
  begin
    Result :=Result + sLineBreak + Format('start:%s, end:%s, datalen:%d, max:%d',
      [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime),
        FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondEndTime),
        FWSABuf.len, FMaxSize]);
  end;
end;

function TIocpSendRequest.GetWSABuf: PWsaBuf;
begin
  Result := @FWSABuf;
end;

procedure TIocpDataMonitor.CalcuMaxOnlineCount(pvOnlineCount: Integer);
begin
  if pvOnlineCount > FMaxOnlineCount then FMaxOnlineCount := pvOnlineCount;
end;

procedure TIocpDataMonitor.Clear;
begin
  FLocker.Enter;
  try
    FSentSize:=0;
    FRecvSize:=0;
    FPostWSASendSize:=0;

    FContextCreateCounter := 0;
    FPostWSASendCounter:=0;
    FResponseWSASendCounter:=0;

    FSendRequestCreateCounter := 0;
    FSendRequestOutCounter := 0;
    FSendRequestReturnCounter := 0;
    FSendRequestAbortCounter := 0;
    FPostSendObjectCounter := 0;


    
    FPostWSARecvCounter:=0;
    FResponseWSARecvCounter:=0;

    FRecvRequestCreateCounter:=0;
    FRecvRequestReturnCounter:=0;
    FRecvRequestOutCounter:=0;
    

    FPushSendQueueCounter := 0;
    FResponseSendObjectCounter := 0;

    FSpeed_WSASentSize := 0;
    FSpeed_WSARecvSize := 0;
    FLastSpeed_RecvSize := 0;
    FLastSpeed_WSASentSize := 0;

    FDisconnectedCounter := 0;



    //FPostWSAAcceptExCounter:=0;
    //FResponseWSAAcceptExCounter:=0;
  finally
    FLocker.Leave;
  end;
end;

constructor TIocpDataMonitor.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
end;



procedure TIocpDataMonitor.DecCtxRecvQueueSize;
begin
  AtomicDecrement(Self.FCtxRecvQueueSize);
end;

destructor TIocpDataMonitor.Destroy;
begin
  FLocker.Free;
  inherited Destroy;
end;

procedure TIocpDataMonitor.IncAcceptExObjectCounter;
begin
   InterlockedIncrement(FAcceptExObjectCounter); 
end;

procedure TIocpDataMonitor.IncCtxRecvQueueSize;
begin
  AtomicIncrement(Self.FCtxRecvQueueSize);
end;

procedure TIocpDataMonitor.IncDisconnectedCounter;
begin
  InterlockedIncrement(FDisconnectedCounter);
end;

procedure TIocpDataMonitor.IncPushSendQueueCounter;
begin
  InterlockedIncrement(FPushSendQueueCounter);
end;

{$IFDEF SOCKET_REUSE}
procedure TIocpDataMonitor.incHandleCreateCounter;
begin
  InterlockedIncrement(FHandleCreateCounter);
end;

procedure TIocpDataMonitor.incHandleDestroyCounter;
begin
  InterlockedIncrement(FHandleDestroyCounter);
end;
{$ENDIF}

procedure TIocpDataMonitor.IncPostSendObjectCounter;
begin
  InterlockedIncrement(FPostSendObjectCounter);
end;


procedure TIocpDataMonitor.IncPostWSARecvCounter;
begin
  InterlockedIncrement(FPostWSARecvCounter);
end;

procedure TIocpDataMonitor.IncPostWSASendCounter;
begin
  InterlockedIncrement(FPostWSASendCounter);
end;

procedure TIocpDataMonitor.IncPostWSASendSize(pvSize:Cardinal);
begin
  {$IFDEF MONITOR_SPIN_LOCK}
  AtomicAdd64(FPostWSASendSize, pvSize);
  {$ELSE}
  FLocker.Enter;
  FPostWSASendSize := FPostWSASendSize + pvSize;
  FLocker.Leave;
  {$ENDIF}


end;

procedure TIocpDataMonitor.IncRecvdSize(pvSize:Cardinal);
begin
  {$IFDEF MONITOR_SPIN_LOCK}
  AtomicAdd64(FRecvSize, pvSize);
  {$ELSE}
  FLocker.Enter;
  FRecvSize := FRecvSize + pvSize;
  FLocker.Leave;
  {$ENDIF}
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

procedure TIocpDataMonitor.IncResponseSendObjectCounter;
begin
  InterlockedIncrement(FResponseSendObjectCounter);
end;

procedure TIocpDataMonitor.IncResponseWSARecvCounter;
begin
  InterlockedIncrement(FResponseWSARecvCounter);
end;

procedure TIocpDataMonitor.IncResponseWSASendCounter;
begin
  InterlockedIncrement(FResponseWSASendCounter);
end;

procedure TIocpDataMonitor.IncSentSize(pvSize:Cardinal);
begin
  {$IFDEF MONITOR_SPIN_LOCK}
  AtomicAdd64(FSentSize, pvSize);
  {$ELSE}
  FLocker.Enter;
  FSentSize := FSentSize + pvSize;
  FLocker.Leave;
  {$ENDIF}
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
  FSpeed_WSASentSize := Trunc((FSentSize - FLastSpeed_WSASentSize) / lvSec);
  FSpeed_WSARecvResponse := Trunc((self.FResponseWSARecvCounter - FLastSpeed_WSARecvResponse) / lvSec);

end;

procedure TIocpDataMonitor.SpeedCalcuStart;
begin
  FLastSpeedTick := GetTickCount;
  FLastSpeed_WSASendResponse := FResponseWSASendCounter;
  FLastSpeed_WSARecvResponse := FResponseWSARecvCounter;
  FLastSpeed_WSASentSize := FSentSize;
end;

{ TIocpDisconnectExRequest }


function TIocpDisconnectExRequest.DirectlyPost: Boolean;
var
  lvErrorCode:Integer;
begin
  Result := IocpDisconnectEx(FContext.RawSocket.SocketHandle, @FOverlapped, TF_REUSE_SOCKET, 0);
  if not Result then
  begin
    lvErrorCode := WSAGetLastError;
    if lvErrorCode <> ERROR_IO_PENDING then
    begin
      // do normal close;
      FContext.RawSocket.close;
      {$IFDEF WRITE_LOG}
      FOwner.logMessage('TIocpDisconnectExRequest.PostRequest Error:%d',  [lvErrorCode]);
      {$ENDIF}

      // context may return to pool
      FContext.decReferenceCounter(
        Format('TIocpDisconnectExRequest.PostRequest Error: %d', [lvErrorCode]), Self
        );
      Result := false;
    end else
    begin
      Result := true;
    end;
  end;
end;

function TIocpDisconnectExRequest.PostRequest: Boolean;
var
  lvErrorCode:Integer;
begin
  Result := False;

  if FContext.incReferenceCounter('TIocpDisconnectExRequest.PostRequest', Self) then
  begin
    Result := IocpDisconnectEx(FContext.RawSocket.SocketHandle, @FOverlapped, TF_REUSE_SOCKET, 0);
    if not Result then
    begin
      lvErrorCode := WSAGetLastError;
      if lvErrorCode <> ERROR_IO_PENDING then
      begin
        // do normal close;
        FContext.RawSocket.close;
        {$IFDEF WRITE_LOG}
        FOwner.logMessage('TIocpDisconnectExRequest.PostRequest Error:%d',  [lvErrorCode]);
        {$ENDIF}

        // context may return to pool
        FContext.decReferenceCounter(
          Format('TIocpDisconnectExRequest.PostRequest Error: %d', [lvErrorCode]), Self
          );
        Result := false;
      end else
      begin
        Result := true;
      end;
    end;
  end;
end;

procedure TDiocpListeners.Close;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    TDiocpListener(FList[i]).FAcceptorMgr.FListenSocket.Close(False);
  end;
end;

constructor TDiocpListeners.Create(AOwnerTcpServer: TDiocpTcpServer);
begin
  inherited Create;
  FOwnerServer := AOwnerTcpServer;
  FList := TList.Create();
end;

destructor TDiocpListeners.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TDiocpListeners.Bind(const pvBindingAddress: string; const pvPort:
    Integer; pvIPVersion: Integer = IP_V4; pvClientContextClass:
    TIocpClientContextClass = nil);
var
  lvItem:TDiocpListener;
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    lvItem := TDiocpListener(FList[i]);
    if
      (SameText(lvItem.FListenAddress, pvBindingAddress))
      and
      (lvItem.FListenPort = pvPort)
      and
      (lvItem.FIPVersion = pvIPVersion)
    then
    begin
      raise Exception.Create(Format('�벻Ҫ�ظ���(%s:%d)', [pvBindingAddress,pvPort]));
    end;
  end;

  lvItem := TDiocpListener.Create(FOwnerServer);
  lvItem.FListenAddress := pvBindingAddress;
  lvItem.FListenPort := pvPort;
  lvItem.FIPVersion := pvIPVersion;
  lvItem.FAcceptorMgr.FClientContextClass := pvClientContextClass;
  FList.Add(lvItem);
end;

procedure TDiocpListeners.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    TDiocpListener(FList[i]).Free;
  end;
  FList.Clear;

end;

procedure TDiocpListeners.ClearObjects;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    TDiocpListener(FList[i]).FAcceptorMgr.ClearObjects;
  end;

end;

procedure TDiocpListeners.PostAcceptExRequest(pvNum: Integer);
var
  i: Integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    TDiocpListener(FList[i]).PostAcceptExRequest(pvNum);
  end;
end;

procedure TDiocpListeners.Start(pvIocpEngine: TIocpEngine);
var
  i: Integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    TDiocpListener(FList[i]).Start(pvIocpEngine);
  end;
end;

procedure TDiocpListeners.WaitForCancel(pvTimeOut:Integer);
var
  i: Integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    TDiocpListener(FList[i]).FAcceptorMgr.WaitForCancel(pvTimeOut)
  end;
end;

procedure TDiocpListener.Close;
begin
  FAcceptorMgr.FListenSocket.Close(False);
end;

constructor TDiocpListener.Create(AOwnerTcpServer: TDiocpTcpServer);
begin
  inherited Create;
  FAcceptorMgr := TIocpAcceptorMgr.Create(AOwnerTcpServer);
end;

destructor TDiocpListener.Destroy;
begin
  FreeAndNil(FAcceptorMgr);
  inherited Destroy;
end;


procedure TDiocpListener.PostAcceptExRequest(pvNum: Integer);
begin
  FAcceptorMgr.PostAcceptExRequest(pvNum);
end;

procedure TDiocpListener.Start(pvIocpEngine: TIocpEngine);
var
  lvListenSocket:TRawSocket;
begin
  try
    lvListenSocket := FAcceptorMgr.FListenSocket;
    lvListenSocket.IPVersion  := FIPVersion;
    lvListenSocket.CreateTcpOverlappedSocket;
    FAcceptorMgr.FOwner.DoSetSocketOptn(lvListenSocket);    
    

    // �������˿�
    if not lvListenSocket.Bind(FListenAddress, FListenPort) then
    begin
      try
        RaiseLastOSError;
      finally
        lvListenSocket.Close(False);
      end;
    end;

    // ��������
    if not lvListenSocket.listen() then
    begin
      try
        RaiseLastOSError;
      finally
        lvListenSocket.Close(False);
      end;
    end;

    // �������׽��ְ󶨵�IOCP���
    if pvIocpEngine.IocpCore.Bind2IOCPHandle(lvListenSocket.SocketHandle, 0) <= 0 then
    begin
      try
        RaiseLastOSError;
      finally
        lvListenSocket.Close(False);
      end;
    end;
  except
    on E:Exception do
    begin
       Raise Exception.Create(Format(strListenFail, [FListenAddress, FListenPort, e.Message]));
    end;
  end;
end;

procedure TDiocpListener.WaitForCancel(pvTimeOut: Integer);
begin
  FAcceptorMgr.WaitForCancel(pvTimeOut);
end;

constructor TDiocpLogicTask.Create;
begin
  inherited Create;
  FGroupTask := TGroupTask.Create();
  FGroupTask.OnWorkerExecute := DoTask;
end;

destructor TDiocpLogicTask.Destroy;
begin
  FGroupTask.StopWorker(120000);
  FreeAndNil(FGroupTask);
  inherited Destroy;
end;

{ TDiocpLogicTask }

procedure TDiocpLogicTask.DoTask(pvSender: TGroupTask; pvWorker:
    TGroupTaskWorker; pvData: Pointer);
begin
  TIocpClientContext(pvData).DoProcessRecvQueue;
end;

initialization
  __startTime :=  Now();
  __innerLogger := TSafeLogger.Create();
  __innerLogger.setAppender(TLogFileAppender.Create(True));

  {$IFDEF DIOCP_HIGH_SPEED}
  __innerLogger.LogFilter := [lgvError, lgvWarning, lgvWriteFile];
  {$ENDIF}

  __svrLogger := __innerLogger;

finalization
  if __innerLogger <> nil then
  begin
    __innerLogger.Free;
  end;

  if __default_task <> nil then
  begin
    __default_task.Free;
  end;



end.
