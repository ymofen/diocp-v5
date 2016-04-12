(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
 
unit diocp.sockets;

{$I 'diocp.inc'}

interface


uses
  Classes, diocp.sockets.utils, diocp.core.engine,
  winsock, diocp.winapi.winsock2,

  diocp.core.rawWinSocket, SyncObjs, Windows, SysUtils,
  utils.safeLogger,
  utils.hashs,
  utils.queues, utils.locker, diocp.tcp.server;

const
  CORE_LOG_FILE = 'diocp_core_exception';


type
  TDiocpCustom = class;
  TIocpAcceptorMgr = class;
  TDiocpCustomContext = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;


  TIocpSendRequestClass = class of TIocpSendRequest;
  TIocpContextClass = class of TDiocpCustomContext;

  TOnContextError = procedure(pvContext: TDiocpCustomContext; pvErrorCode: Integer)
      of object;

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

  /// <summary>
  ///   client object
  /// </summary>
  TDiocpCustomContext = class(TObject)
  private
    // 最后交互数据的时间点
    FLastActivity: Cardinal;

    FContextDNA : Integer;

    FSocketHandle : TSocket;
    FContextLocker: TIocpLocker;
    FLastErrorCode:Integer;
    FDebugINfo: string;

    procedure SetDebugINfo(const Value: string);

  private

    FDebugStrings:TStrings;

    /// <summary>
    ///   dec RequestCounter and requestDisconnect then check counter flag for Disonnect
    /// </summary>
    procedure DecReferenceCounterAndRequestDisconnect(pvDebugInfo: string; pvObj:TObject);

  private
    FAlive:Boolean;

    // link
    FPre:TDiocpCustomContext;
    FNext:TDiocpCustomContext;

    /// <summary>
    ///   sending flag
    /// </summary>
    FSending: Boolean;

    FActive: Boolean;

    FOwner: TDiocpCustom;

    FRecvRequest:TIocpRecvRequest;

    FcurrSendRequest:TIocpSendRequest;

    FData: Pointer;
    FOnConnectedEvent: TNotifyContextEvent;
    FOnDisconnectedEvent: TNotifyContextEvent;
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
    ///    request discnnect flag, ReferenceCounter is zero then do disconnect
    /// </summary>
    FRequestDisconnect: Boolean;
    FSocketState: TSocketState;

    /// <summary>
    ///   called by recvRequest response
    /// </summary>
    procedure DoReceiveData;

    /// <summary>
    ///   called by sendRequest response
    /// </summary>
    procedure DoSendRequestCompleted(pvRequest: TIocpSendRequest);



    /// <summary>
    ///   post next sendRequest
    /// </summary>
    function CheckNextSendRequest: Boolean;

    /// <example>
    ///   释放待发送队列中的发送请求(TSendRequest)
    /// </example>
    procedure CheckReleaseRes;


    procedure SetOwner(const Value: TDiocpCustom);
  protected
    /// <summary>
    ///   request recv data
    /// </summary>
    procedure PostWSARecvRequest();

    /// <summary>
    ///   post reqeust to sending queue,
    ///    fail, push back to pool
    ///  准备抛弃
    /// </summary>
    function PostSendRequestDelete(pvSendRequest:TIocpSendRequest): Boolean;


    /// <summary>
    ///   获取一个TSendRequest对象
    ///   调用Owner返回，一般从对象池中获取
    /// </summary>
    function GetSendRequest: TIocpSendRequest;


    procedure DoError(pvErrorCode:Integer);

  protected
    /// <summary>
    ///    dec RequestCounter then check counter and Request flag for Disonnect
    /// </summary>
    function DecReferenceCounter(pvDebugInfo: string; pvObj: TObject): Integer;
    function IncReferenceCounter(pvDebugInfo: string; pvObj: TObject): Boolean;

    /// <summary>
    ///   只进行增加, connectEx的时候用
    /// </summary>
    procedure AddRefernece;

    /// <summary>
    ///   只进行减少, connectEx的时候用
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
    ///   call in response event
    /// </summary>
    procedure DoDisconnect;


    procedure InnerCloseContext;
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
    procedure unLockContext(pvDebugInfo: string; pvObj: TObject);
  public

    /// <summary>
    ///   关闭连接，不会再进行数据的发送, 待发送队列中的数据会进行取消
    /// </summary>
    procedure Close;

    constructor Create; virtual;

    destructor Destroy; override;


    
    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean =
        true): Boolean; overload;
    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvBufReleaseType:
        TDataReleaseType): Boolean; overload;
    procedure RequestDisconnect(pvDebugInfo: string = ''; pvObj: TObject = nil);
    procedure SetMaxSendingQueueSize(pvSize:Integer);
    /// <summary>
    ///   设置当前的接收线程信息
    /// </summary>
    procedure SetRecvWorkerHint(pvHintStr: String); overload;
    procedure SetRecvWorkerHint(pvFmtMsg: string; const args: array of const);
        overload;

    /// <summary>
    ///  是否已经连接
    /// </summary>
    property Active: Boolean read FActive;

    property Data: Pointer read FData write FData;

    property DebugINfo: string read FDebugINfo write SetDebugINfo;
    
    property OnConnectedEvent: TNotifyContextEvent read FOnConnectedEvent write
        FOnConnectedEvent;

    property OnDisconnectedEvent: TNotifyContextEvent read FOnDisconnectedEvent
        write FOnDisconnectedEvent;
    property Owner: TDiocpCustom read FOwner write SetOwner;

    property RawSocket: TRawSocket read FRawSocket;

    property SocketState: TSocketState read FSocketState;

    property SocketHandle: TSocket read FSocketHandle;

    /// <summary>
    ///   Socket状态改变触发的事件
    /// </summary>
    property OnSocketStateChanged: TNotifyEvent read FOnSocketStateChanged write
        FOnSocketStateChanged;
  end;

  /// <summary>
  ///   WSARecv io request
  /// </summary>
  TIocpRecvRequest = class(TIocpRequest)
  private
    FInnerBuffer: diocp.winapi.winsock2.TWsaBuf;
    FRecvBuffer: diocp.winapi.winsock2.TWsaBuf;
    FRecvdFlag: Cardinal;
    FOwner: TDiocpCustom;
    FContext: TDiocpCustomContext;
    FDebugInfo:String;
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

  public
    constructor Create;
    destructor Destroy; override;
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

    // 记录开始时间点
    FLastSpeedTick : Cardinal;

    // 最高在线数量
    FMaxOnlineCount:Integer;

    // 记录开始时间点_数据
    FLastSpeed_WSASendResponse: Int64;
    FLastSpeed_WSARecvResponse: Int64;

    FSpeed_WSASendResponse: Int64;
    FSpeed_WSARecvResponse: Int64;

    procedure incSentSize(pvSize:Cardinal);
    procedure incPostWSASendSize(pvSize:Cardinal);
    procedure incRecvdSize(pvSize:Cardinal);

    procedure incPostWSASendCounter();
    procedure incResponseWSASendCounter;

    procedure incPostWSARecvCounter();
    procedure incResponseWSARecvCounter;

    procedure incPushSendQueueCounter;
    procedure incPostSendObjectCounter();
    procedure incResponseSendObjectCounter();
  public
    constructor Create;
    destructor Destroy; override;
    
    /// <summary>
    ///   计算最高在线数量
    /// </summary>
    procedure CalcuMaxOnlineCount(pvOnlineCount:Integer);

    procedure clear;
    /// <summary>
    ///   统计数据，计算时间信息
    /// </summary>
    procedure SpeedCalcuEnd;
    /// <summary>
    ///  开始统计速度
    ///  记录当前信息
    /// </summary>
    procedure SpeedCalcuStart;

    property MaxOnlineCount: Integer read FMaxOnlineCount;
    property PushSendQueueCounter: Integer read FPushSendQueueCounter;
    property PostSendObjectCounter: Integer read FPostSendObjectCounter;
    property ResponseSendObjectCounter: Integer read FResponseSendObjectCounter;

    property PostWSAAcceptExCounter: Integer read FPostWSAAcceptExCounter;
    property PostWSARecvCounter: Integer read FPostWSARecvCounter;
    property PostWSASendCounter: Integer read FPostWSASendCounter;


    property PostWSASendSize: Int64 read FPostWSASendSize;
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
  end;

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


  TDiocpCustom = class(TComponent)
  private
    FSafeLogger:TSafeLogger;


    /// <summary>
    ///   引用计数
    /// </summary>
    FRefCounter:Integer;
    
  {$IFDEF DEBUG_ON}
    FDebug_SendRequestCounter:Integer;
  {$ENDIF}

    FIsDestroying :Boolean;
    FWSARecvBufferSize: Cardinal;
    procedure SetWSARecvBufferSize(const Value: Cardinal);

    function isDestroying:Boolean;
    function logCanWrite:Boolean;

  protected
    FContextClass:TIocpContextClass;

    FIocpSendRequestClass:TIocpSendRequestClass;

    FLocker:TIocpLocker;

    /// <summary>
    ///   维护的在线列表
    /// </summary>
    FOnlineContextList : TDHashTable;
  private
    // sendRequest pool
    FSendRequestPool: TBaseQueue;

    /// data record
    FDataMoniter: TIocpDataMonitor;

    FActive: Boolean;

    FIocpEngine: TIocpEngine;

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
  protected
    /// <summary>
    ///   pop sendRequest object
    /// </summary>
    function GetSendRequest: TIocpSendRequest;

    /// <summary>
    ///   push back to pool
    /// </summary>
    function ReleaseSendRequest(pvObject:TIocpSendRequest): Boolean;

  protected

    /// <summary>
    ///   创建一个连接实例
    ///   通过注册的ContextClass进行创建实例     
    /// </summary>
    function CreateContext: TDiocpCustomContext;

    /// <summary>
    ///   occur on create instance
    /// </summary>
    procedure OnCreateContext(const context: TDiocpCustomContext); virtual;

    /// <summary>
    ///   设置组件的名字，重载用于设置日志记录器的前缀符
    /// </summary>
    procedure SetName(const NewName: TComponentName); override;
  private
    FContextDNA : Integer;

    function GetOnlineContextCount: Integer;
    procedure OnIocpException(pvRequest:TIocpRequest; E:Exception);

    function RequestContextDNA: Integer;

    procedure SetWSASendBufferSize(const Value: Cardinal);

  protected

    /// <summary>
    ///   添加到在先列表中
    /// </summary>
    procedure AddToOnlineList(pvObject: TDiocpCustomContext);

    /// <summary>
    ///   从在线列表中移除
    /// </summary>
    procedure RemoveFromOnOnlineList(pvObject: TDiocpCustomContext); virtual;

  public
    /// <summary>
    ///   原子操作添加引用计数(一定要有对应的DecRefCounter);
    ///   目的: 阻止释放连接
    /// </summary>
    function IncRefCounter: Integer;

    /// <summary>
    ///   减少引用
    /// </summary>
    function DecRefCounter:Integer;

    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    procedure RegisterContextClass(pvContextClass: TIocpContextClass);

    /// <summary>
    ///   创建数据监控对象,用于记录内部数据的收发数据
    /// </summary>
    procedure CreateDataMonitor;


    /// <summary>
    ///   check clientContext object is valid.
    /// </summary>
    function CheckClientContextValid(const pvClientContext: TDiocpCustomContext):
        Boolean;

    /// <summary>
    ///   停止IOCP线程，等待所有线程释放
    /// </summary>
    procedure Close;

    property Active: Boolean read FActive write SetActive;

    /// <summary>
    ///   请求断开所有连接，会立刻返回。
    /// </summary>
    procedure DisconnectAll;
    
    procedure Open;

    /// <summary>
    ///   get online client list
    /// </summary>
    procedure GetOnlineContextList(pvList:TList);

    /// <summary>
    ///   wait for all conntext is off
    /// </summary>
    function WaitForContext(pvTimeOut: Cardinal): Boolean;

    /// <summary>
    ///   client connections counter
    /// </summary>
    property OnlineContextCount: Integer read GetOnlineContextCount;

    property DataMoniter: TIocpDataMonitor read FDataMoniter;

    property IocpEngine: TIocpEngine read FIocpEngine;

    /// <summary>
    ///   超时检测, 如果超过Timeout指定的时间还没有任何数据交换数据记录，
    ///     就进行关闭连接
    ///   使用循环检测，如果你有好的方案，欢迎提交您的宝贵意见
    /// </summary>
    procedure KickOut(pvTimeOut:Cardinal = 60000);
    procedure LogMessage(pvMsg: string; pvMsgType: string = ''; pvLevel: TLogLevel
        = lgvMessage); overload;
        
    procedure logMessage(pvMsg: string; const args: array of const; pvMsgType:
        string = ''; pvLevel: TLogLevel = lgvMessage); overload;
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
///   注册服务使用的SafeLogger
/// </summary>
procedure RegisterDiocpLogger(pvLogger:TSafeLogger);

implementation

uses
  diocp.res;

resourcestring
  strRecvZero      = '[%d]接收到0字节的数据,该连接将断开!';
  strRecvError     = '[%d]响应接收请求时出现了错误。错误代码:%d!';
  strRecvEngineOff = '[%d]响应接收请求时发现IOCP服务关闭';
  strRecvPostError = '[%d]投递接收请求时出现了错误。错误代码:%d!';

  strSendEngineOff = '[%d]响应发送数据请求时发现IOCP服务关闭';
  strSendErr       = '[%d]响应发送数据请求时出现了错误。错误代码:%d!';
  strSendPostError = '[%d]投递发送数据请求时出现了错误。错误代码:%d';
  strSendZero      = '[%d]投递发送请求数据时遇到0长度数据。进行关闭处理'; 
  strSendPushFail  = '[%d]投递发送请求数据包超出队列允许的最大长度[%d/%d]。';

  strBindingIocpError = '[%d]绑定到IOCP句柄时出现了异常, 错误代码:%d, (%s)';

  strPushFail      = '[%d]压入到待发送队列失败, 队列信息: %d/%d';

  strOnRecvBufferException = '[%d]响应OnRecvBuffer时出现了异常:%s。';

var
  __innerLogger:TSafeLogger;

//{$IFDEF DEBUG_ON}
//procedure logDebugMessage(pvMsg: string; const args: array of const);
//begin
//  sfLogger.logMessage(pvMsg, args);
//end;
//{$ENDIF}



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
    FcurrSendRequest := lvRequest;
    if lvRequest.ExecuteSend then
    begin
      if (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostSendObjectCounter;
      end;
    end else
    begin
      FcurrSendRequest := nil;

      /// cancel request
      lvRequest.CancelRequest;

      {$IFDEF DEBUG_ON}
      FOwner.logMessage('[%d]TIocpClientContext.CheckNextSendRequest.ExecuteSend return false',
         [self.SocketHandle]);
      {$ENDIF}
      /// kick out the clientContext
      RequestDisconnect('CheckNextSendRequest::lvRequest.checkSendNextBlock Fail', lvRequest);
    
      FOwner.ReleaseSendRequest(lvRequest);
    end;
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

procedure TDiocpCustomContext.Close;
begin
  RequestDisconnect('TDiocpCustomContext.Close');
end;

constructor TDiocpCustomContext.Create;
begin
  inherited Create;
  FDebugStrings := TStringList.Create;
  FReferenceCounter := 0;
  FContextLocker := TIocpLocker.Create('contextlocker');
  FAlive := False;
  FRawSocket := TRawSocket.Create();
  FActive := false;
  FSendRequestLink := TIocpRequestSingleLink.Create(10);
  FRecvRequest := TIocpRecvRequest.Create;
  FRecvRequest.FContext := self;
end;

destructor TDiocpCustomContext.Destroy;
begin
  if IsDebugMode then
  begin
    if FReferenceCounter <> 0 then
    begin
      Assert(FReferenceCounter = 0);
    end;
  end;

  FRawSocket.close;
  FRawSocket.Free;

  FRecvRequest.Free;


  Assert(FSendRequestLink.Count = 0);
  FSendRequestLink.Free;
  FContextLocker.Free;
  FDebugStrings.Free;
  inherited Destroy;
end;

function TDiocpCustomContext.DecReferenceCounter(pvDebugInfo: string; pvObj:
    TObject): Integer;
var
  lvCloseContext:Boolean;
begin
  lvCloseContext := false;
  FContextLocker.lock('DecReferenceCounter');
  Dec(FReferenceCounter);
  Result := FReferenceCounter;
  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));

  if FDebugStrings.Count > 20 then
    FDebugStrings.Delete(0);


  if FReferenceCounter < 0 then
    Assert(FReferenceCounter >=0 );
  if FReferenceCounter = 0 then
    if FRequestDisconnect then lvCloseContext := true;
    
  FContextLocker.UnLock; 

  if lvCloseContext then
  begin
    InnerCloseContext;
  end;
end;

procedure TDiocpCustomContext.DecReferenceCounterAndRequestDisconnect(
    pvDebugInfo: string; pvObj: TObject);
var
  lvCloseContext:Boolean;
begin
  lvCloseContext := false;
{$IFDEF DEBUG_ON}
  FOwner.logMessage('(%d)断开请求信息*:%s', [SocketHandle, pvDebugInfo],
      'RequestDisconnect');
{$ENDIF}
  FContextLocker.lock('DecReferenceCounter');
  FRequestDisconnect := true;
  Dec(FReferenceCounter);
  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));

  if FDebugStrings.Count > 20 then
    FDebugStrings.Delete(0);
  if FReferenceCounter < 0 then
    Assert(FReferenceCounter >=0 );
  if FReferenceCounter = 0 then
    lvCloseContext := true;
    
  FContextLocker.UnLock; 

  if lvCloseContext then InnerCloseContext; 
end;

procedure TDiocpCustomContext.DoCleanUp;
begin
  FLastActivity := 0;
  FRequestDisconnect := false;
  FSending := false;

  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(Self), '-----DoCleanUp-----']));

  if IsDebugMode then
  begin
    if FReferenceCounter <> 0 then
    begin
      Assert(FReferenceCounter = 0);
    end;
    Assert(not FActive);
  end;
end;

procedure TDiocpCustomContext.DoConnected;
begin
  // 一些状态的初始化
  FRequestDisconnect := false;

  FLastActivity := GetTickCount;

  FContextLocker.lock('DoConnected');
  try
    FSocketHandle := FRawSocket.SocketHandle;
    Assert(FOwner <> nil);
    if FActive then
    begin
      if IsDebugMode then
      begin
        Assert(not FActive);
      end;
      {$IFDEF DEBUG_ON}
       FOwner.logMessage('on DoConnected event is already actived', CORE_DEBUG_FILE);
      {$ENDIF}
    end else
    begin


      FContextDNA := FOwner.RequestContextDNA;
      FActive := true;
      FOwner.AddToOnlineList(Self);

      if self.LockContext('onConnected', Self) then
      try
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
        PostWSARecvRequest;
      finally
        self.unLockContext('OnConnected', Self);
      end;
    end;
  finally
    FContextLocker.UnLock;
  end;
  
//
//  FContextLocker.lock('DoConnected');
//  try
//    FSocketHandle := FRawSocket.SocketHandle;
//    FSending := false;
//    FRequestDisconnect := false;
//    if not FActive then
//    begin
//      Assert(FOwner <> nil);
//      FActive := true;
//
//      FOwner.FOnlineContextList.add(Self);
//      try
//
//        if Assigned(FOwner.FOnContextConnected) then
//        begin
//          FOwner.FOnContextConnected(Self);
//        end;
//        OnConnected();
//        if Assigned(FOnConnectedEvent) then
//        begin
//          FOnConnectedEvent(Self);
//        end;
//      except
//      end;
//
//      SetSocketState(ssConnected);
//      PostWSARecvRequest;
//    end;
//  finally
//    FContextLocker.UnLock;
//  end;

end;

procedure TDiocpCustomContext.DoDisconnect;
begin
  InnerCloseContext;
end;

procedure TDiocpCustomContext.DoError(pvErrorCode: Integer);
begin
  FLastErrorCode:= pvErrorCode;
  FOwner.DoClientContextError(Self, pvErrorCode);
end;

procedure TDiocpCustomContext.DoReceiveData;
begin
  try
    FLastActivity := GetTickCount;

    OnRecvBuffer(FRecvRequest.FRecvBuffer.buf,
      FRecvRequest.FBytesTransferred,
      FRecvRequest.ErrorCode);
    if FOwner <> nil then
      FOwner.DoReceiveData(Self, FRecvRequest);
  except
    on E:Exception do
    begin
      if FOwner <> nil then
      begin
        FOwner.LogMessage(strOnRecvBufferException, [SocketHandle, e.Message]);
        FOwner.OnContextError(Self, -1);
      end else
      begin
        __svrLogger.logMessage(strOnRecvBufferException, [SocketHandle, e.Message]);
      end;
    end;
  end;

end;

procedure TDiocpCustomContext.DoSendRequestCompleted(pvRequest:
    TIocpSendRequest);
begin
  ;
end;

function TDiocpCustomContext.GetSendRequest: TIocpSendRequest;
begin
  Result := FOwner.GetSendRequest;
  Assert(Result <> nil);
  Result.FContext := self;
end;

function TDiocpCustomContext.IncReferenceCounter(pvDebugInfo: string; pvObj:
    TObject): Boolean;
begin
  Assert(Self<> nil);
  FContextLocker.lock('IncReferenceCounter');
  if (not Active) or FRequestDisconnect then
  begin
    Result := false;
  end else
  begin
    Inc(FReferenceCounter);
    FDebugStrings.Add(Format('+(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));

    if FDebugStrings.Count > 20 then FDebugStrings.Delete(0);

    Result := true;
  end;
  FContextLocker.UnLock;
end;

procedure TDiocpCustomContext.InnerCloseContext;
begin
  Assert(FOwner <> nil);
{$IFDEF DEBUG_ON}
  if FReferenceCounter <> 0 then
    FOwner.logMessage('InnerCloseContext FReferenceCounter:%d', [FReferenceCounter],
    CORE_LOG_FILE);
  if not FActive then
  begin
    FOwner.logMessage('InnerCloseContext FActive is false', CORE_LOG_FILE);
    exit;
  end;
{$ENDIF}
  if not FActive then exit;

  try
    FActive := false;
    FRawSocket.close;
    CheckReleaseRes;

    try
      if Assigned(FOwner.FOnContextDisconnected) then
      begin
        FOwner.FOnContextDisconnected(Self);
      end;

      // 
      OnDisconnected;

      if Assigned(FOnDisconnectedEvent) then FOnDisconnectedEvent(Self);

      // 设置Socket状态
      SetSocketState(ssDisconnected);

      DoCleanUp;
    except
    end;
  finally
    FOwner.RemoveFromOnOnlineList(Self);
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

procedure TDiocpCustomContext.OnConnected;
begin

end;

procedure TDiocpCustomContext.OnDisconnected;
begin

end;





procedure TDiocpCustomContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode:
    WORD);
begin
  
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
      {$IFDEF DEBUG_ON}
        if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage('Push sendRequest to Sending Queue fail, queue size:%d',
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
begin
  FRecvRequest.PostRequest;
end;



function TDiocpCustomContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
    pvCopyBuf: Boolean = true): Boolean;
var
  lvBuf: PAnsiChar;
begin
  if len = 0 then raise Exception.Create('PostWSASendRequest::request buf is zero!');
  if pvCopyBuf then
  begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    Result := PostWSASendRequest(lvBuf, len, dtFreeMem);
    if not Result then
    begin            //post fail
      FreeMem(lvBuf);
    end;
  end else
  begin
    lvBuf := buf;
    Result := PostWSASendRequest(lvBuf, len, dtNone);
  end;

end;

procedure TDiocpCustomContext.RequestDisconnect(pvDebugInfo: string = '';
    pvObj: TObject = nil);
var
  lvCloseContext:Boolean;
begin
  if not FActive then exit;
{$IFDEF DEBUG_ON}
  FOwner.logMessage('(%d)断开请求信息:%s', [SocketHandle, pvDebugInfo],
      'RequestDisconnect');
{$ENDIF}

  lvCloseContext := false;

  Assert(FContextLocker <> nil, 'error...');

  FContextLocker.lock('RequestDisconnect');
 
  if pvDebugInfo <> '' then
  begin
    FDebugStrings.Add(Format('*(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));
    if FDebugStrings.Count > 40 then FDebugStrings.Delete(0);
  end;
  FRequestDisconnect := True;

  //
  if FReferenceCounter = 0 then
  begin
    lvCloseContext := true;
  end;
  
  FContextLocker.UnLock; 

  if lvCloseContext then InnerCloseContext else FRawSocket.close; 
end;

procedure TDiocpCustomContext.SetDebugINfo(const Value: string);
begin
  FDebugINfo := Value;
end;

procedure TDiocpCustomContext.SetOwner(const Value: TDiocpCustom);
begin
  FOwner := Value;
  FRecvRequest.FOwner := FOwner;
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

procedure TDiocpCustomContext.unLockContext(pvDebugInfo: string; pvObj: TObject);
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
begin
  inherited Create(AOwner);
  FRefCounter := 0;
  CheckWinSocketStart;
  FSafeLogger:=TSafeLogger.Create();
  FSafeLogger.setAppender(TLogFileAppender.Create(True));

  FLocker := TIocpLocker.Create('diocp.tcp.client');

  {$IFDEF DEBUG_ON}
  FDebug_SendRequestCounter:=0;
  {$ENDIF}
  FOnlineContextList := TDHashTable.Create(10949);

  // send requestPool
  FSendRequestPool := TBaseQueue.Create;
    
  FIocpEngine := TIocpEngine.Create();
  FIocpEngine.IocpCore.OnIocpException := self.OnIocpException;

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
  FSafeLogger.Enable := false;
  
  FIsDestroying := true;

  Close;

  if FDataMoniter <> nil then FDataMoniter.Free;

  FSendRequestPool.FreeDataObject;

  FOnlineContextList.Free;

  FIocpEngine.Free;

  FSendRequestPool.Free;
  
  FSafeLogger.Free;

  FLocker.Free;
  inherited Destroy;
end;

procedure TDiocpCustom.AddToOnlineList(pvObject: TDiocpCustomContext);
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

procedure TDiocpCustom.DoClientContextError(pvClientContext:
    TDiocpCustomContext; pvErrorCode: Integer);
begin
  if Assigned(FOnContextError) then
    FOnContextError(pvClientContext, pvErrorCode);
end;

procedure TDiocpCustom.DoReceiveData(pvIocpContext: TDiocpCustomContext;
    pvRequest: TIocpRecvRequest);
begin


  if Assigned(FOnReceivedBuffer) then
    FOnReceivedBuffer(pvIocpContext,
      pvRequest.FRecvBuffer.buf, pvRequest.FBytesTransferred,
      pvRequest.ErrorCode);
end;

function TDiocpCustom.GetWorkerCount: Integer;
begin
  Result := FIocpEngine.WorkerCount;
end;

function TDiocpCustom.isDestroying: Boolean;
begin
  Result := FIsDestroying or (csDestroying in self.ComponentState);
end;

function TDiocpCustom.logCanWrite: Boolean;
begin
  Result := (not isDestroying) and FSafeLogger.Enable;
end;



procedure TDiocpCustom.Open;
begin
  if FActive = true then exit;

  if FDataMoniter <> nil then FDataMoniter.clear;

  // engine start
  FIocpEngine.checkStart;

  FActive := True;
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
    if (FDataMoniter <> nil) then
    begin
      InterlockedIncrement(FDataMoniter.FSendRequestReturnCounter);
    end;
    pvObject.DoCleanUp;
    pvObject.FOwner := nil;
    FSendRequestPool.EnQueue(pvObject);
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

  DisconnectAll;

  WaitForContext(30000);

  // engine Stop
  FIocpEngine.safeStop;
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
    OnCreateContext(Result);
  end else
  begin
    Result := TDiocpCustomContext.Create;
    OnCreateContext(Result);
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
          lvClientContext.RequestDisconnect('DisconnectAll');
        end;
        lvBucket:=lvBucket.Next;
      end;
    end;
  finally
    FLocker.unLock;
  end;
end;

function TDiocpCustom.GetOnlineContextCount: Integer;
begin
  Result := FOnlineContextList.Count;
end;

procedure TDiocpCustom.GetOnlineContextList(pvList:TList);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  FLocker.lock('getOnlineContextList');
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

function TDiocpCustom.GetSendRequest: TIocpSendRequest;
begin
  Result := TIocpSendRequest(FSendRequestPool.DeQueue);
  if Result = nil then
  begin
    if FIocpSendRequestClass <> nil then
    begin
      Result := FIocpSendRequestClass.Create;
    end else
    begin
      Result := TIocpSendRequest.Create;
    end;
  end;
  Result.FAlive := true;
  Result.DoCleanup;
  Result.FOwner := Self;
  {$IFDEF DEBUG_ON}
  InterlockedIncrement(FDebug_SendRequestCounter);
  {$ENDIF}
end;

function TDiocpCustom.IncRefCounter: Integer;
begin
  Result := InterlockedIncrement(FRefCounter);
end;

procedure TDiocpCustom.KickOut(pvTimeOut:Cardinal = 60000);
var
  lvNowTickCount:Cardinal;
  I, j:Integer;
  lvKickOutList: array of TDiocpCustomContext;
  lvContext:TDiocpCustomContext;
var
  lvBucket, lvNextBucket: PDHashData;
begin
  lvNowTickCount := GetTickCount;
  self.IncRefCounter;
  try
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
            if lvContext.FLastActivity <> 0 then
            begin
              if tick_diff(lvContext.FLastActivity, lvNowTickCount) > pvTimeOut then
              begin
                // 请求关闭(异步请求关闭,不直接用RequestDisconnect()避免直接移除FOnlineContextList列表)
                //lvContext.PostWSACloseRequest();
                lvKickOutList[j] := lvContext;
                Inc(j);
              end;
            end;
          end;
          lvBucket:= lvNextBucket;
        end;
      end;
    finally
      FLocker.unLock;
    end;

    for i := 0 to j - 1 do
    begin
      lvKickOutList[i].RequestDisconnect('超时检测主动断开');   
    end;


  finally
    self.DecRefCounter;
  end;

end;

procedure TDiocpCustom.LogMessage(pvMsg: string; pvMsgType: string = '';
    pvLevel: TLogLevel = lgvMessage);
begin
  if logCanWrite then
  begin
    FSafeLogger.logMessage(pvMsg, pvMsgType, pvLevel);
  end;
end;

procedure TDiocpCustom.logMessage(pvMsg: string; const args: array of const;
    pvMsgType: string; pvLevel: TLogLevel);
begin
  if logCanWrite then
  begin
    FSafeLogger.logMessage(pvMsg, args, pvMsgType, pvLevel);
  end;  
end;

procedure TDiocpCustom.OnCreateContext(const context: TDiocpCustomContext);
begin

end;

procedure TDiocpCustom.OnIocpException(pvRequest:TIocpRequest; E:Exception);
begin
  try
    if pvRequest <> nil then
    begin
      LogMessage('未处理异常:%s, 请求(%s)信息:%s',[E.Message, pvRequest.ClassName, pvRequest.Remark],
        CORE_LOG_FILE, lgvError);
    end else
    begin
      LogMessage('未处理异常:%s',[E.Message], CORE_LOG_FILE, lgvError);
    end;
  except
  end;
end;

procedure TDiocpCustom.RemoveFromOnOnlineList(pvObject: TDiocpCustomContext);
{$IFDEF DEBUG_ON}
  var
    lvSucc:Boolean;
{$ENDIF}
begin
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
end;

function TDiocpCustom.RequestContextDNA: Integer;
begin
  Result := InterlockedIncrement(FContextDNA);
end;

procedure TDiocpCustom.SetName(const NewName: TComponentName);
begin
  inherited;
{$IFDEF DEBUG_ON}
  if FSafeLogger.Appender is TLogFileAppender then
  begin
    if NewName <> '' then
    begin
      TLogFileAppender(FSafeLogger.Appender).FilePreFix := NewName + '_';
    end;
  end;
{$ENDIF}
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
      {$IFDEF DEBUG_ON}
       if logCanWrite then
        FSafeLogger.logMessage('WaitForContext End Current num:%d', [c], CORE_LOG_FILE);
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

procedure TIocpRecvRequest.HandleResponse;
begin
  {$IFDEF DEBUG_ON}
  InterlockedDecrement(FOverlapped.refCount);
  if FOverlapped.refCount <> 0 then
    Assert(FOverlapped.refCount <>0);
  Assert(FOwner <> nil);
  {$ENDIF}

  try
    if (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incResponseWSARecvCounter;
      FOwner.FDataMoniter.incRecvdSize(FBytesTransferred);
    end;

    if not FOwner.Active then
    begin
      {$IFDEF DEBUG_ON}
        FOwner.logMessage(
          Format(strRecvEngineOff, [FContext.FSocketHandle])
        );
      {$ENDIF}
      // avoid postWSARecv
      FContext.RequestDisconnect(
        Format(strRecvEngineOff, [FContext.FSocketHandle])
        , Self);
//      {$IFDEF DEBUG_ON}
//      if FOwner.logCanWrite then
//        FOwner.FSafeLogger.logMessage('TIocpRecvRequest Owner Off', 'DEBUG_', lgvDebug);
//      {$ENDIF}
//      FContext.RequestDisconnect('IocpRecvRequest response server enginee is off', Self);
    end else if ErrorCode <> 0 then
    begin
      {$IFDEF DEBUG_ON}
      FOwner.FSafeLogger.logMessage(
        Format(strRecvError, [FContext.FSocketHandle, FErrorCode])
        );
      {$ENDIF}
      FContext.DoError(ErrorCode);
      FContext.RequestDisconnect(
        Format(strRecvError, [FContext.FSocketHandle, FErrorCode])
        ,  Self);
//
//    {$IFDEF DEBUG_ON}
//      if FOwner.logCanWrite then
//        FOwner.FSafeLogger.logMessage('TIocpRecvRequest response ErrorCode:%d',
//          [ErrorCode], 'DEBUG_', lgvDebug);
//    {$ENDIF}
//
//      FContext.DoError(ErrorCode);
//
//      FContext.RequestDisconnect('IocpRecvRequest response Error',  Self);
    end else if (FBytesTransferred = 0) then
    begin      // no data recvd, socket is break
      {$IFDEF DEBUG_ON}
      FOwner.logMessage(strRecvZero,  [FContext.FSocketHandle]);
      {$ENDIF}
      FContext.RequestDisconnect(
        Format(strRecvZero,  [FContext.FSocketHandle]),  Self);

//    {$IFDEF DEBUG_ON}
//      if FOwner.logCanWrite then
//        FOwner.FSafeLogger.logMessage('IocpRecvRequest response FBytesTransferred is zero',
//         [], 'DEBUG_', lgvDebug);
//    {$ENDIF}
//      FContext.RequestDisconnect('IocpRecvRequest response FBytesTransferred is zero',  Self);
    end else
    begin
      FContext.DoReceiveData;
    end;
  finally

    if not FContext.FRequestDisconnect then
    begin
      FContext.PostWSARecvRequest;
    end;

    FContext.decReferenceCounter(
      Format('debugInfo:%s, refcount:%d, TIocpRecvRequest.WSARecvRequest.HandleResponse',
        [FDebugInfo,FOverlapped.refCount]), Self);


  end;
end;

function TIocpRecvRequest.PostRequest(pvBuffer: PAnsiChar;
  len: Cardinal): Boolean;
var
  lvRet:Integer;
  lpNumberOfBytesRecvd: Cardinal;
begin
  Result := false;
  
  lpNumberOfBytesRecvd := 0;
  FRecvdFlag := 0;

  FRecvBuffer.buf := pvBuffer;
  FRecvBuffer.len := len;



  if FContext.incReferenceCounter('TIocpRecvRequest.WSARecvRequest.Post', Self) then
  begin
    {$IFDEF DEBUG_ON}
    InterlockedIncrement(FOverlapped.refCount);
    FDebugInfo := IntToStr(intPtr(FContext));
    {$ENDIF}


    lvRet := diocp.winapi.winsock2.WSARecv(FContext.FRawSocket.SocketHandle,
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
        {$IFDEF DEBUG_ON}
        FOwner.logMessage(strRecvPostError, [FContext.SocketHandle, lvRet]);
        InterlockedDecrement(FOverlapped.refCount);
        {$ENDIF}
        // trigger error event
        FOwner.DoClientContextError(FContext, lvRet);

        // decReferenceCounter
        FContext.decReferenceCounterAndRequestDisconnect(
        'TIocpRecvRequest.WSARecvRequest.Error', Self);


      end else
      begin
        if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
        begin
          FOwner.FDataMoniter.incPostWSARecvCounter;
        end;
      end;
    end else
    begin
      Result := True;
    
      if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostWSARecvCounter;
      end;
    end;
  end;
end;

function TIocpRecvRequest.PostRequest: Boolean;
begin
  if FInnerBuffer.len <> FOwner.FWSARecvBufferSize then
  begin
    if FInnerBuffer.len > 0 then FreeMem(FInnerBuffer.buf);
    FInnerBuffer.len := FOwner.FWSARecvBufferSize;
    GetMem(FInnerBuffer.buf, FInnerBuffer.len);
  end;
  Result := PostRequest(FInnerBuffer.buf, FInnerBuffer.len);
end;

function TIocpSendRequest.ExecuteSend: Boolean;
begin
  if (FBuf = nil) or (FLen = 0) then
  begin
    {$IFDEF DEBUG_ON}
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

    if not FOwner.Active then
    begin
      {$IFDEF DEBUG_ON}
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
      {$IFDEF DEBUG_ON}
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
      // succ
      if FOwner.FDataMoniter <> nil then
      begin
        FOwner.FDataMoniter.incResponseSendObjectCounter;
      end;

      if Assigned(FOnDataRequestCompleted) then
      begin
        FOnDataRequestCompleted(lvContext, Self);
      end;

      lvContext.DoSendRequestCompleted(Self);

      lvContext.PostNextSendRequest;
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
       FIsBusying := False;
       {$IFDEF DEBUG_ON}
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

procedure TIocpDataMonitor.clear;
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

end;

procedure TIocpDataMonitor.SpeedCalcuStart;
begin
  FLastSpeedTick := GetTickCount;
  FLastSpeed_WSASendResponse := FResponseWSASendCounter;
  FLastSpeed_WSARecvResponse := FResponseWSARecvCounter;
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
  {$IFDEF DEBUG_ON}
  self.Remark := Format('正在连接%s(%d)', [pvHost, pvPort]);
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

procedure TDiocpCustomContext.AddRefernece;
begin
  FContextLocker.lock('AddRefernece');
  Inc(FReferenceCounter);
  FContextLocker.UnLock;
end;

procedure TDiocpCustomContext.DecRefernece;
begin
  FContextLocker.lock('DecRefernece');
  Dec(FReferenceCounter);
  FContextLocker.UnLock;
end;


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

  {$IFDEF DEBUG_ON}
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
  end;
end;

function TDiocpCustomContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
    pvBufReleaseType: TDataReleaseType): Boolean;
var
  lvRequest:TIocpSendRequest;
  {$IFDEF DEBUG_ON}
  lvErrStr :String;
  {$ENDIF}
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
        Result := InnerPostSendRequestAndCheckStart(lvRequest);
        if not Result then
        begin
          /// Push Fail unbinding buf
          lvRequest.UnBindingSendBuffer;

          {$IFDEF DEBUG_ON}
          lvErrStr := Format(strSendPushFail, [SocketHandle,
            FSendRequestLink.Count, FSendRequestLink.MaxSize]);
          Self.RequestDisconnect(lvErrStr,
            lvRequest);
          {$ELSE}
          Self.RequestDisconnect('', lvRequest);
          {$ENDIF}

          lvRequest.CancelRequest;
          FOwner.ReleaseSendRequest(lvRequest);
        end;
      finally
        self.DecReferenceCounter('PostWSASendRequest', Self);
      end;
    end;
  end;
end;

procedure TDiocpCustomContext.SetMaxSendingQueueSize(pvSize:Integer);
begin
  FSendRequestLink.setMaxSize(pvSize);
end;

procedure TDiocpCustomContext.SetRecvWorkerHint(pvHintStr: String);
begin
  if FRecvRequest <> nil then
    FRecvRequest.SetWorkHintInfo(pvHintStr);
end;

procedure TDiocpCustomContext.SetRecvWorkerHint(pvFmtMsg: string; const args:
    array of const);
begin
  SetRecvWorkerHint(Format(pvFmtMsg, args));
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
