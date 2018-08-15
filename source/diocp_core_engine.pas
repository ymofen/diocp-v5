(*
 *	 Unit owner: D10.Mofen
 *	       blog: http://www.cnblogs.com/dksoft
 *
  *   2015-02-22 08:29:43
  *     DIOCP-V5 发布
 *
 *   thanks qsl's suggestion


*  优化IocpEngine的开启和关闭过程, SafeStop时关闭IOCP句柄，在Start(开启引擎)时重建IOCP句柄。
    (SafeStop在等待所有工作线程停止时会重复投递退出请求，如果不重建句柄，会在重新工作线程的时候，可能会继续收到退出请求，导致工作线程退出。)
	  可能会导致，程序无法退出，并有内存泄漏, 因为重新开启服务后的工作线程为0时,无法处理任何的IOCP请求。
	  2015-10-13 21:05:34
 *)
unit diocp_core_engine;


interface

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

uses
  Windows, diocp_sockets_utils, SysUtils, Classes, SyncObjs
{$if CompilerVersion >= 18}
  , types
{$ifend}
  , diocp_winapi_winsock2

  , ComObj, ActiveX, utils_locker;


{$IF CompilerVersion> 23}
  {$define varNativeUInt}
{$IFEND}

{$if CompilerVersion >= 18}
  {$DEFINE INLINE}
{$IFEND}

const
  WORKER_ISBUSY =  $01;    // worker is busy
  WORKER_ISWATING = $02;   // waiting for task
  WORKER_RESERVED = $04;   // worker is reserved
  WORKER_OVER = $08;       // worker is dead


type
  TIocpRequest = class;
  TIocpEngine = class;
  TIocpWorker = class;

  POVERLAPPEDEx = ^OVERLAPPEDEx;
  OVERLAPPEDEx = packed record
    Overlapped: OVERLAPPED;
    iocpRequest: TIocpRequest;
    RefCount: Integer;
  end;

  TThreadStackFunc = function(AThread:TThread):string;

  TDiocpExceptionEvent = procedure(pvRequest:TIocpRequest; E:Exception) of object;



  /// <summary>
  ///   iocp request root class
  /// </summary>
  TIocpRequest = class(TObject)
  private
    __free_flag:Integer;
    FThreadID : THandle;
    FData: Pointer;
    FWorkerThreadID:THandle;

    /// io request response info
    FIocpWorker: TIocpWorker;

    FPre: TIocpRequest;

    FRemark: String;

    // next Request
    FNext: TIocpRequest;
    
    FOnResponse: TNotifyEvent;
    FOnResponseDone: TNotifyEvent;
    FTag: Integer;
  protected
    FResponding: Boolean;
    FRespondStartTickCount:Cardinal;
    FRespondStartTime: TDateTime;
    FRespondEndTime: TDateTime;

    FErrorCode: Integer;

    //post request to iocp queue.
    FOverlapped: OVERLAPPEDEx;
    FBytesTransferred: NativeUInt;
    FCompletionKey: NativeUInt;

  protected

    procedure HandleResponse; virtual;

    function GetStateInfo: String; virtual;

    /// <summary>
    ///   响应请求最后完成,在IOCP线程,执行请求时执行
    ///   不管请求响应时有没有出现异常，都会执行
    /// </summary>
    procedure ResponseDone; virtual;

    /// <summary>
    ///   请求取消,在未投递的请求下, 不得不取消请求
    /// </summary>
    procedure CancelRequest; virtual;

  public
    /// <summary>
    ///   设置线程内部信息, 请在线程内部执行
    /// </summary>
    procedure SetWorkHintInfo(const pvHint: String);

    constructor Create;

    destructor Destroy; override;
    procedure CheckThreadIn;
    procedure CheckThreadOut;

    property IocpWorker: TIocpWorker read FIocpWorker;

    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;

    property OnResponseDone: TNotifyEvent read FOnResponseDone write FOnResponseDone;

    property BytesTransferred: NativeUInt read FBytesTransferred;
    property CompletionKey: NativeUInt read FCompletionKey;
    property ErrorCode: Integer read FErrorCode;

    procedure CancelIoEx(const pvHandle: THandle);



    /// <summary>
    ///   remark
    /// </summary>
    property Remark: String read FRemark write FRemark;

    //
    property Responding: Boolean read FResponding;

    /// <summary>
    ///   扩展Data数据
    /// </summary>
    property Data: Pointer read FData write FData;


    property Tag: Integer read FTag write FTag;


    


    function OverlappedPtr: POVERLAPPEDEx;





  end;

  TIocpASyncRequest = class;


{$IFDEF UNICODE}
  TDiocpASyncEvent = reference to procedure(pvRequest: TIocpASyncRequest);
{$ELSE}
  TDiocpASyncEvent = procedure(pvRequest: TIocpASyncRequest) of object;
{$ENDIF}

  TIocpASyncRequest = class(TIocpRequest)
  private
    FStartTime: Cardinal;
    FEndTime: Cardinal;
    FOnASyncEvent: TDiocpASyncEvent;
  protected
    procedure HandleResponse; override;
    function GetStateINfo: String; override;
  public
    destructor Destroy; override;
    procedure DoCleanUp;

    /// <summary>
    ///   异步执行事件在iocp线程中触发
    /// </summary>
    property OnASyncEvent: TDiocpASyncEvent read FOnASyncEvent write FOnASyncEvent;
  end;

  /// <summary>
  ///  request single link
  /// </summary>
  TIocpRequestSingleLink = class(TObject)
  private
    FCount: Integer;
    FHead: TIocpRequest;
    FTail: TIocpRequest;
    FMaxSize:Integer;
  public
    constructor Create(pvMaxSize: Integer = 1024);
    procedure SetMaxSize(pvMaxSize:Integer);
    destructor Destroy; override;
    function Push(pvRequest:TIocpRequest): Boolean;
    function Pop:TIocpRequest;
    property Count: Integer read FCount;
    property MaxSize: Integer read FMaxSize;
  end;

  /// <summary>
  ///  request doublyLinked
  /// </summary>
  TIocpRequestDoublyLinked = class(TObject)
  private
    FLocker: TIocpLocker;
    FHead: TIocpRequest;
    FTail: TIocpRequest;
    FCount:Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure add(pvContext: TIocpRequest);
    function remove(pvContext: TIocpRequest): Boolean;
    function Pop: TIocpRequest;
    procedure write2List(pvList:TList);  
    property Count: Integer read FCount; 
  end;

  /// <summary>
  ///   iocp core object
  ///     iocp core function
  /// </summary>
  TIocpCore = class(TObject)
  private
    /// <summary>
    ///   IOCP core handle
    /// </summary>
    FIOCPHandle: NativeUInt;
    FOnIocpException: TDiocpExceptionEvent;

    // <summary>
    //   create IOCP handle
    // </summary>
    function CreateIOCPHandle: Boolean;

  public

    /// <summary>
    ///   binding a Handle to IOCPHandle
    /// </summary>
    function Bind2IOCPHandle(pvHandle: THandle; pvCompletionKey: ULONG_PTR):
        THandle;

    /// <summary>
    ///   initialize engine
    ///     create iocp handle
    /// </summary>
    procedure DoInitialize;

    /// <summary>
    ///   finalize engine
    /// </summary>
    procedure DoFinalize;

    /// <summary>
    ///   handle io exception
    /// </summary>
    procedure HandleException(pvRequest: TIocpRequest; E: Exception);


    /// <summary>
    ///   post EXIT request into iocp queue
    /// </summary>
    function PostIOExitRequest: Boolean;

    /// <summary>
    ///   投递请求到IO
    /// </summary>
    function PostRequest(dwCompletionKey: DWORD; lpOverlapped: POverlapped):
        Boolean;

    property OnIocpException: TDiocpExceptionEvent read FOnIocpException write
        FOnIocpException;






  end;

  /// <summary>
  ///    worker do process io request
  /// </summary>
  TIocpWorker = class(TThread)
  private
    FResponseCounter:Cardinal;

    FFlags: Integer;

    FIocpEngine: TIocpEngine;
    
    FIocpCore: TIocpCore;

    FCoInitialized:Boolean;

    FData: Pointer;

    FLastRequest:TIocpRequest;

    FHintInfo: string;

    FLastResponseClassStr: String;

    FLastResponseStart:TDateTime;

    FLastResponseEnd: TDateTime;
    FTagStr: String;

    /// <summary>
    ///  设置线程当前信息(由线程内部调用执行)
    /// </summary>
    procedure SetHintInfo(pvHint:String);

  public
    constructor Create(AIocpCore: TIocpCore);
    
    procedure Execute; override;

    procedure WriteStateInfo(const pvStrings: TStrings);  

    procedure SetFlag(pvFlag:Integer);{$IFDEF INLINE} inline; {$ENDIF}

    procedure RemoveFlag(pvFlag:Integer);

    function CheckFlag(pvFlag:Integer): Boolean;

    /// <summary>
    ///   current worker invoke
    /// </summary>
    procedure CheckCoInitializeEx(pvReserved: Pointer = nil; coInit: Longint = 0);

    /// <summary>
    ///   附加数据，可以通过当前IocpRequest.iocpWorker.Data获取到当前执行线程的附加数据
    /// </summary>
    property Data: Pointer read FData write FData;

    /// <summary>
    ///   the last handle respond iocp request
    /// </summary>
    property LastRequest: TIocpRequest read FLastRequest;

    /// <summary>
    ///   单线程访问
    /// </summary>
    property TagStr: String read FTagStr write FTagStr;


  end;


  /// <summary>
  ///  IOCP引擎, 管理IOCP工作线程
  /// </summary>
  TIocpEngine = class(TObject)
  private

    FWorkerNeedCoInitialize: Boolean;

    FWorkerLocker: TIocpLocker;

    FMaxWorkerCount: Word;

    FActive: Boolean;

    // alive worker count
    FActiveWorkerCount:Integer;

    // iocp core object
    FIocpCore: TIocpCore;
    FName: String;

    // worker(thread) list
    FWorkerList: TList;

    // 
    FSafeStopSign: TEvent;

    // set worker count
    FWorkerCount: Word;

    /// <summary>
    ///   check worker thread is alive
    /// </summary>
    function WorkersIsAlive: Boolean;

    procedure IncAliveWorker;
    procedure DecAliveWorker(const pvWorker: TIocpWorker);
    function GetWorkingCount: Integer;
  public
    constructor Create;

    destructor Destroy; override;
  public
    procedure WriteStateINfo(const pvStrings:TStrings);

    function GetStateINfo: String;

    procedure PostAExitRequest;

    /// <summary>
    ///   get worker handle response
    /// </summary>
    function GetWorkerStateInfo(pvTimeOut: Cardinal = 3000): string;

    /// <summary>
    ///  获取工作线程信息
    /// </summary>
    function GetWorkersInfo: String;

    /// <summary>
    ///  获取工作线程信息
    /// </summary>
    function GetWorkersHtmlInfo(const pvTableID: String = ''): String;

    /// <summary>
    ///   get thread call stack
    /// </summary>
    function GetWorkerStackInfos(pvThreadStackFunc: TThreadStackFunc; pvTimeOut:
        Cardinal = 3000): string;


    /// <summary>
    ///   set worker count, don't clear workers
    /// </summary>
    procedure SetWorkerCount(AWorkerCount: Integer);

    /// <summary>
    ///   设置最大的工作线程
    /// </summary>
    procedure SetMaxWorkerCount(AWorkerCount: Word);


    /// <summary>尝试创建一个工作线程, </summary>
    /// <returns> true,成功创建一个工作线程.</returns>
    /// <param name="pvIsTempWorker"> 临时工作线程 </param>
    function CheckCreateWorker(pvIsTempWorker: Boolean): Boolean;
    /// <summary>尝试创建一个工作线程, </summary>
    /// <returns> true,成功创建一个工作线程.</returns>
    /// <param name="pvIsTempWorker"> 临时工作线程 </param>
    function CreateWorker(pvIsTempWorker: Boolean): Boolean;

    /// <summary>
    ///   开启IOCP引擎，创建工作线程
    /// </summary>
    procedure Start;


    /// <summary>
    ///    stop and wait worker thread
    ///    default 120's
    /// </summary>
    procedure SafeStop(pvTimeOut: Integer = 120000);

    /// <summary>
    ///   check active, Start
    /// </summary>
    procedure CheckStart;

    /// <summary>
    ///  Stop workers
    /// </summary>
    function StopWorkers(pvTimeOut: Cardinal): Boolean;

    procedure PostRequest(pvRequest:TIocpRequest);



    property Active: Boolean read FActive;
    property ActiveWorkerCount: Integer read FActiveWorkerCount;

    /// <summary>
    ///   core object, read only
    /// </summary>
    property IocpCore: TIocpCore read FIocpCore;


    /// <summary>
    ///   最大的工作线程数
    /// </summary>
    property MaxWorkerCount: Word read FMaxWorkerCount write SetMaxWorkerCount;



    /// <summary>
    ///  获取工作线程数量
    /// </summary>
    property WorkerCount: Word read FWorkerCount;

    /// <summary>
    ///   Engine name
    /// </summary>
    property Name: String read FName write FName;

    /// <summary>
    ///   工作线程需要进行CoInitalize初始化
    /// </summary>
    property WorkerNeedCoInitialize: Boolean read FWorkerNeedCoInitialize write FWorkerNeedCoInitialize;

    /// <summary>
    ///   正在工作的线程数量
    /// </summary>
    property WorkingCount: Integer read GetWorkingCount;
  end;

var
  __ProcessIDStr:String;
  
  __defaultDiocpEngine:TIocpEngine;

procedure StartDiocpEngine;
procedure StopDiocpEngine;
function CreateNewDiocpEngine(const pvWorkerNum:Integer): TIocpEngine;

procedure SetDiocpEngineWorkerNum(const pvWorkerNum:Integer);

function IsDebugMode: Boolean;
procedure SafeWriteFileMsg(const pvMsg, pvFilePre: String);
function tick_diff(tick_start, tick_end: Cardinal): Cardinal;

function TraceDateString(pvDate:TDateTime): String;


implementation


{$IFDEF DEBUG_ON}
var
  workerCounter:Integer;
{$ENDIF}

resourcestring
  strDebugINfo               = '状态 : %s, 工作线程: %d';
  strDebug_WorkerTitle       = '----------------------- 工作线程(%d) --------------------';
  strDebug_Worker_INfo       = '线程id: %d, 响应数量: %d';
  strDebug_Worker_StateINfo  = '正在工作:%s, 等待:%s, 保留线程:%s ';
  strDebug_Request_Title     = '请求状态信息:';
  
  strDebug_RequestState      = '完成: %s, 耗时(ms): %d';

function TraceDateString(pvDate:TDateTime): String;
begin
  if pvDate > 0 then Result := FormatDateTime('yy-MM-dd hh:nn:ss.zzz', pvDate)
  else Result := '';

end;

procedure SafeWriteFileMsg(const pvMsg, pvFilePre: String);
var
  lvFileName, lvBasePath:String;
  lvLogFile: TextFile;
begin
  try
    lvBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
    ForceDirectories(lvBasePath);
    lvFileName :=lvBasePath + '\' + __ProcessIDStr+ '_' + pvFilePre +
     FormatDateTime('mmddhhnnsszzz', Now()) + '.log';

    AssignFile(lvLogFile, lvFileName);
    if (FileExists(lvFileName)) then
      append(lvLogFile)
    else
      rewrite(lvLogFile);

    writeln(lvLogFile, pvMsg);
    flush(lvLogFile);
    CloseFile(lvLogFile);
  except
    ;
  end;
end;

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;


function IsDebugMode: Boolean;
begin
{$IFDEF MSWINDOWS}
{$warn symbol_platform off}
  Result := Boolean(DebugHook);
{$warn symbol_platform on}
{$ELSE}
  Result := false;
{$ENDIF}
end;

function GetCPUCount: Integer;
{$IFDEF MSWINDOWS}
var
  si: SYSTEM_INFO;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  GetSystemInfo(si);
  Result := si.dwNumberOfProcessors;
  {$ELSE}// Linux,MacOS,iOS,Andriod{POSIX}
  {$IFDEF POSIX}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ELSE}// unkown system, default 1
  Result := 1;
  {$ENDIF !POSIX}
  {$ENDIF !MSWINDOWS}
end;

procedure StartDiocpEngine;
begin
  if __defaultDiocpEngine = nil then
  begin
    __defaultDiocpEngine := TIocpEngine.Create;
  end;
end;

procedure StopDiocpEngine;
begin
  if __defaultDiocpEngine <> nil then
  begin
    __defaultDiocpEngine.SafeStop();
    __defaultDiocpEngine.Free;
    __defaultDiocpEngine := nil;
  end;
end;

procedure SetDiocpEngineWorkerNum(const pvWorkerNum:Integer);
begin
  if __defaultDiocpEngine = nil then
  begin
    __defaultDiocpEngine.SetWorkerCount(pvWorkerNum);    
  end;
end;

function CreateNewDiocpEngine(const pvWorkerNum:Integer): TIocpEngine;
begin
  Result := TIocpEngine.Create;
  Result.SetWorkerCount(pvWorkerNum);
end;

function TIocpCore.Bind2IOCPHandle(pvHandle: THandle; pvCompletionKey:
    ULONG_PTR): THandle;
begin
   Result := CreateIoCompletionPort(pvHandle, FIOCPHandle, pvCompletionKey, 0);
end;

function TIocpCore.CreateIOCPHandle: Boolean;
begin
  FIOCPHandle := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  Result := (FIOCPHandle <> 0) and (FIOCPHandle <> INVALID_HANDLE_VALUE);
  if not Result then
  begin
    RaiseLastOSError;
  end;
end;

procedure TIocpCore.DoFinalize;
begin
  if FIOCPHandle <> 0 then
  begin
    CloseHandle(FIOCPHandle);
    FIOCPHandle := 0;
  end;
end;

procedure TIocpCore.DoInitialize;
begin
  if FIOCPHandle = 0 then CreateIOCPHandle;
end;

procedure TIocpCore.HandleException(pvRequest: TIocpRequest; E: Exception);
begin
  if Assigned(FOnIocpException) then
  begin
    FOnIocpException(pvRequest, E);
  end;
end;

function TIocpCore.PostIOExitRequest: Boolean;
begin
  Result := PostQueuedCompletionStatus(FIOCPHandle, 0, 0, nil);
end;

function TIocpCore.PostRequest(dwCompletionKey: DWORD; lpOverlapped:
    POverlapped): Boolean;
begin
  Result := PostQueuedCompletionStatus(FIOCPHandle, 0, dwCompletionKey, lpOverlapped);
end;

procedure TIocpWorker.CheckCoInitializeEx(pvReserved: Pointer = nil; coInit:
    Longint = 0);
begin
  if not FCoInitialized then
  begin
    CoInitializeEx(pvReserved, coInit);
    FCoInitialized := true;
  end;
end;

function TIocpWorker.CheckFlag(pvFlag:Integer): Boolean;
begin
  Result := ((FFlags and pvFlag) <> 0);
end;

constructor TIocpWorker.Create(AIocpCore: TIocpCore);
begin
  inherited Create(True);
  FIocpCore := AIocpCore;
  FFlags := WORKER_RESERVED;  // default is reserved
  FResponseCounter := 0;
end;

{ TIocpWorker }

procedure TIocpWorker.Execute;
var
  lvBytesTransferred:DWORD;
  lvResultStatus:BOOL;
  lvErrCode:Integer;
  lpOverlapped:POVERLAPPEDEx;

  lpCompletionKey:ULONG_PTR;
  lvTempRequest:TIocpRequest;
begin

  if FIocpEngine.FWorkerNeedCoInitialize then
  begin
    CheckCoInitializeEx();
  end;
  
  FIocpEngine.incAliveWorker;

{$IFDEF DEBUG_ON}
  InterlockedIncrement(workerCounter);
{$ENDIF}

  while (not self.Terminated) do
  begin
    try
      FFlags := (FFlags or WORKER_ISWATING) and (not WORKER_ISBUSY);

      if CheckFlag(WORKER_RESERVED) then
      begin
        lvResultStatus := GetQueuedCompletionStatus(FIocpCore.FIOCPHandle,
          lvBytesTransferred,  lpCompletionKey,
          POverlapped(lpOverlapped),
          INFINITE);
      end else
      begin
        // 临时工作线程, 30秒后没有任务可能会被释放
        lvResultStatus := GetQueuedCompletionStatus(FIocpCore.FIOCPHandle,
          lvBytesTransferred,  lpCompletionKey,
          POverlapped(lpOverlapped),
          30000);
      end;

      FFlags := (FFlags or WORKER_ISBUSY) and (not WORKER_ISWATING);


      if Assigned(lpOverlapped) then
      begin
        if not lvResultStatus then
        begin
          lvErrCode := GetLastError;
        end else
        begin
          lvErrCode := 0;
        end;

        Inc(FResponseCounter);
        FLastResponseStart := Now();
        lvTempRequest := lpOverlapped.iocpRequest;
        FLastRequest := lvTempRequest;
        try
          if lvTempRequest.__free_flag > 0 then
          begin
            Assert(false, 'error');
          end;
          if FLastRequest = nil then
          begin
            Assert(FLastRequest<>NIL);
          end;

          FLastResponseClassStr := lvTempRequest.ClassName;

          try
            lvTempRequest.FResponding := true;
            lvTempRequest.FRespondStartTime := Now();
            lvTempRequest.FRespondStartTickCount := GetTickCount;
            lvTempRequest.FRespondEndTime := 0;
            lvTempRequest.FiocpWorker := Self;
            lvTempRequest.FWorkerThreadID := self.ThreadID; 
            lvTempRequest.FErrorCode := lvErrCode;
            lvTempRequest.FBytesTransferred := lvBytesTransferred;
            lvTempRequest.FCompletionKey := lpCompletionKey;
            if Assigned(lvTempRequest.FOnResponse) then
            begin
              lvTempRequest.FOnResponse(lvTempRequest);
            end else
            begin
              lvTempRequest.HandleResponse();
            end;
          except
            on E:Exception do
            begin                          
              FIocpCore.HandleException(lvTempRequest, E);
            end;
          end;
        finally
          FLastResponseEnd := Now();
          try
            try
              lvTempRequest.FRespondEndTime := Now();
              lvTempRequest.FResponding := false;
              if Assigned(lvTempRequest.OnResponseDone) then
              begin
                lvTempRequest.FOnResponseDone(lvTempRequest);
              end else
              begin
                lvTempRequest.ResponseDone();   // done后有可能回归到池或者释放 之后请不要进行操作
              end;
            except
              on E:Exception do
              begin
                FIocpCore.HandleException(lvTempRequest, E);
              end;
            end;
          except
            on E:Exception do
            begin
              FIocpCore.HandleException(lvTempRequest, E);
            end;
          end;
        end; 
      end else
      begin
        /// exit
        Break;
      end;
    except
      on E: Exception do
      begin
        try
          FIocpCore.HandleException(nil, E);
        except
        end;
      end;
    end;
  end;

  FFlags := WORKER_OVER;

  ///
  if FCoInitialized then CoUninitialize();

  
{$IFDEF DEBUG_ON}
  InterlockedDecrement(workerCounter);
{$ENDIF}

  try
    FIocpEngine.DecAliveWorker(Self);
  except
    //Assert(False, ('diocp_core_engine name:' + FIocpEngine.Name));
  end;
end;

procedure TIocpWorker.RemoveFlag(pvFlag:Integer);
begin
  FFlags := FFlags AND (not pvFlag);
end;

procedure TIocpWorker.SetFlag(pvFlag: Integer);
begin
  FFlags := FFlags or pvFlag;
end;

procedure TIocpWorker.SetHintInfo(pvHint:String);
begin
  FHintInfo := pvHint;
end;

procedure TIocpWorker.WriteStateInfo(const pvStrings: TStrings);
var
  s:String;
  lvLastRequest:TIocpRequest;
begin
  pvStrings.Add(Format(strDebug_Worker_INfo, [self.ThreadID, FResponseCounter]));
  if FHintInfo <> '' then
  begin
    pvStrings.Add('last hint:' + self.FHintInfo);
  end;
  if CheckFlag(WORKER_OVER) then
  begin
    pvStrings.Add('work done!!!');
  end else
  begin
    pvStrings.Add(Format(strDebug_Worker_StateINfo,
       [boolToStr(CheckFlag(WORKER_ISBUSY), true),
        boolToStr(CheckFlag(WORKER_ISWATING), true),
        boolToStr(CheckFlag(WORKER_RESERVED), true)]));

    lvLastRequest := FLastRequest;

    if (lvLastRequest <> nil) then
    begin
      s := lvLastRequest.getStateINfo;
      if s <> '' then
      begin
        pvStrings.Add(strDebug_Request_Title);
        pvStrings.Add(s);
      end;
    end;
  end;
end;

function TIocpEngine.CheckCreateWorker(pvIsTempWorker: Boolean): Boolean;
var
  i:Integer;
  AWorker:TIocpWorker;
begin
  Result := false;
  FWorkerLocker.lock;
  try
    if (FMaxWorkerCount > 0) and (FWorkerList.Count >= FMaxWorkerCount) then exit;
    for i := 0 to FWorkerList.Count -1 do
    begin
      if TIocpWorker(FWorkerList[i]).checkFlag(WORKER_ISWATING) then
      begin
        Exit;
      end;
    end;

    AWorker := TIocpWorker.Create(FIocpCore);
    if pvIsTempWorker then
    begin
      AWorker.removeFlag(WORKER_RESERVED);
    end else
    begin
      AWorker.setFlag(WORKER_RESERVED);
    end;
    AWorker.FIocpEngine := Self;
    AWorker.FreeOnTerminate := True;
    FWorkerList.Add(AWorker);
    {$IFDEF UNICODE}
    AWorker.Start;
    {$ELSE}
    AWorker.Resume;
    {$ENDIF}

  finally
    FWorkerLocker.unLock;
  end;
end;

procedure TIocpEngine.CheckStart;
begin
  if not FActive then Start;
end;

constructor TIocpEngine.Create;
begin
  inherited Create;
  FWorkerLocker := TIocpLocker.Create;

  FWorkerCount := GetCPUCount shl 1 - 1;
  if FWorkerCount <=1 then FWorkerCount := 2;
  FWorkerList := TList.Create();
  FIocpCore := TIocpCore.Create;
  FIocpCore.doInitialize;
end;

procedure TIocpEngine.DecAliveWorker(const pvWorker: TIocpWorker);
var
  lvCount:Integer;
begin
  FWorkerLocker.lock;
  try
    FWorkerList.Remove(pvWorker);
    lvCount := InterlockedDecrement(FActiveWorkerCount);
  finally
    FWorkerLocker.unLock;
  end;

  if lvCount = 0 then
  begin
    // 移除到外面避免 在关闭时提前释放了资源
    if FSafeStopSign <> nil then FSafeStopSign.SetEvent;
  end;
end;

destructor TIocpEngine.Destroy;
begin
  SafeStop;

  // wait thread's res back
  Sleep(10);

  FIocpCore.doFinalize;
  FIocpCore.Free;
  FreeAndNil(FWorkerList);
  FWorkerLocker.Free;
  FWorkerLocker := nil;
  inherited Destroy;
end;

function TIocpEngine.CreateWorker(pvIsTempWorker: Boolean): Boolean;
var
  i:Integer;
  AWorker:TIocpWorker;
begin
  Result := false;
  FWorkerLocker.lock;
  try
    if (FMaxWorkerCount > 0) and (FWorkerList.Count >= FMaxWorkerCount) then exit;

    AWorker := TIocpWorker.Create(FIocpCore);
    if pvIsTempWorker then
    begin
      AWorker.removeFlag(WORKER_RESERVED);
    end else
    begin
      AWorker.setFlag(WORKER_RESERVED);
    end;
    AWorker.FIocpEngine := Self;
    AWorker.FreeOnTerminate := True;
    FWorkerList.Add(AWorker);
    {$IFDEF UNICODE}
    AWorker.Start;
    {$ELSE}
    AWorker.Resume;
    {$ENDIF}

  finally
    FWorkerLocker.unLock;
  end;
end;

function TIocpEngine.GetStateINfo: String;
var
  lvStrings :TStrings;
begin
  lvStrings := TStringList.Create;
  try
    WriteStateINfo(lvStrings);
    Result := lvStrings.Text;
  finally
    lvStrings.Free;
  end;
end;

function TIocpEngine.GetWorkersHtmlInfo(const pvTableID: String = ''): String;
var
  lvStrings :TStrings;
  i:Integer;
  s:String;
  lvWorker:TIocpWorker;
begin
  lvStrings := TStringList.Create;
  try
    lvStrings.Add('<table id = ' + pvTableID + '>');
    lvStrings.Add('<tr>');
    lvStrings.Add('<td>id</td>');
    lvStrings.Add('<td>busy</td>');
    lvStrings.Add('<td>reserved</td>');
    lvStrings.Add('<td>counter</td>');
    lvStrings.Add('<td>last_request</td>');
    lvStrings.Add('<td>last_start</td>');
    lvStrings.Add('<td>last_end</td>');
    lvStrings.Add('<td>hint</td>');
    lvStrings.Add('</tr>');
    self.FWorkerLocker.lock;
    try
      for i := 0 to FWorkerList.Count - 1 do
      begin
        lvWorker := TIocpWorker(FWorkerList[i]);
        s := Format('<tr><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>hint:%s</td></tr>',
            [lvWorker.ThreadID,
             Ord(lvWorker.CheckFlag(WORKER_ISBUSY)),
             Ord(lvWorker.CheckFlag(WORKER_RESERVED)),
             lvWorker.FResponseCounter,
             lvWorker.FLastResponseClassStr,
             TraceDateString(lvWorker.FLastResponseStart),
             TraceDateString(lvWorker.FLastResponseEnd),
             lvWorker.FHintInfo
            ]
             );
        lvStrings.Add(s);
      end;
    finally
      lvStrings.Add('</table>');
      self.FWorkerLocker.Leave;
    end;
    Result := lvStrings.Text; 
  finally
    lvStrings.Free;
  end;
end;

function TIocpEngine.GetWorkersInfo: String;
var
  lvStrings :TStrings;
  i:Integer;
  s:String;
  lvWorker:TIocpWorker;
begin
  lvStrings := TStringList.Create;
  try
    self.FWorkerLocker.lock;
    try
      for i := 0 to FWorkerList.Count - 1 do
      begin
        lvWorker := TIocpWorker(FWorkerList[i]);
        s := Format('id: %d, busy: %d, reserved:%d, counter:%d, last_request:%s, last_start:%s, last_end:%s, hint:%s',
            [lvWorker.ThreadID,
             Ord(lvWorker.CheckFlag(WORKER_ISBUSY)),
             Ord(lvWorker.CheckFlag(WORKER_RESERVED)),
             lvWorker.FResponseCounter,
             lvWorker.FLastResponseClassStr,
             TraceDateString(lvWorker.FLastResponseStart),
             TraceDateString(lvWorker.FLastResponseEnd),
             lvWorker.FHintInfo
            ]
             );
        lvStrings.Add(s);
      end;
    finally
      self.FWorkerLocker.Leave;
    end;
    Result := lvStrings.Text; 
  finally
    lvStrings.Free;
  end;
end;

function TIocpEngine.GetWorkerStackInfos(pvThreadStackFunc: TThreadStackFunc;
    pvTimeOut: Cardinal = 3000): string;
var
  lvStrings :TStrings;
  i, j:Integer;
  lvWorker:TIocpWorker;
  lvLastRequest:TIocpRequest;
begin
  Assert(Assigned(pvThreadStackFunc));

  lvStrings := TStringList.Create;
  try
    j := 0;
    lvStrings.Add(Format(strDebugINfo, [BoolToStr(self.FActive, True), self.WorkerCount]));
    self.FWorkerLocker.lock;
    try
      for i := 0 to FWorkerList.Count - 1 do
      begin
        lvWorker := TIocpWorker(FWorkerList[i]);

        lvLastRequest := lvWorker.FLastRequest;

        if (lvLastRequest<> nil) and (lvWorker.checkFlag(WORKER_ISBUSY)) then
        begin
          if GetTickCount - lvLastRequest.FRespondStartTickCount > pvTimeOut then
          begin
            lvStrings.Add(Format(strDebug_WorkerTitle, [i + 1]));
            lvStrings.Add(pvThreadStackFunc(lvWorker));
            inc(j);
          end;
        end;
      end;
    finally
      self.FWorkerLocker.Leave;
    end;
    if j > 0 then
    begin
      Result := lvStrings.Text;
    end else
    begin
      Result := '';
    end;
  finally
    lvStrings.Free;
  end;
end;

function TIocpEngine.GetWorkerStateInfo(pvTimeOut: Cardinal = 3000): string;
var
  lvStrings :TStrings;
  i, j:Integer;
  lvWorker:TIocpWorker;
  lvTickcount:Cardinal;
  lvLastRequest:TIocpRequest;
begin
  lvStrings := TStringList.Create;
  try
    j := 0;
    lvTickcount := GetTickCount;
    lvStrings.Add(Format(strDebugINfo, [BoolToStr(self.FActive, True), self.WorkerCount]));
    self.FWorkerLocker.lock;
    try
      for i := 0 to FWorkerList.Count - 1 do
      begin
        lvWorker := TIocpWorker(FWorkerList[i]);
        lvLastRequest := lvWorker.LastRequest;
        if (lvLastRequest <> nil) and (lvWorker.CheckFlag(WORKER_ISBUSY)) then
        begin
          if tick_diff(lvLastRequest.FRespondStartTickCount, lvTickcount) > pvTimeOut then
          begin
            lvStrings.Add(Format('t_s:%d, t_now:%d', [lvLastRequest.FRespondStartTickCount, lvTickcount]));
            lvStrings.Add(Format(strDebug_WorkerTitle, [i + 1]));
            lvWorker.WriteStateINfo(lvStrings);
            inc(j);
          end;
        end;
      end;
    finally
      self.FWorkerLocker.Leave;
    end;
    if j > 0 then
    begin
      Result := lvStrings.Text;
    end else
    begin
      Result := '';
    end;
  finally
    lvStrings.Free;
  end;
end;

function TIocpEngine.GetWorkingCount: Integer;
begin
  // TODO -cMM: TIocpEngine.GetWorkingCount default body inserted
  Result := FWorkerList.Count;
end;

procedure TIocpEngine.IncAliveWorker;
begin
  InterlockedIncrement(FActiveWorkerCount);
end;

procedure TIocpEngine.PostAExitRequest;
begin
  FIocpCore.PostIOExitRequest;
end;

procedure TIocpEngine.PostRequest(pvRequest: TIocpRequest);
begin
  /// post request to iocp queue
  if not IocpCore.postRequest(0, POverlapped(@pvRequest.FOverlapped)) then
  begin
    RaiseLastOSError;
  end;
end;

procedure TIocpEngine.SafeStop(pvTimeOut: Integer = 120000);
begin
  try
    if FActiveWorkerCount > 0 then
    begin
      StopWorkers(pvTimeOut);
    end;

    FWorkerList.Clear;
    FActive := false;
  finally
    /// 关闭IO句柄等工作，重新开启时, 重建句柄(可以避免在停止时，投递了多余的退出请求，而导致重新开启服务时，又处理消息)
    FIocpCore.DoFinalize();
  end;
end;

procedure TIocpEngine.SetMaxWorkerCount(AWorkerCount: Word);
begin
  FMaxWorkerCount := AWorkerCount;
end;

procedure TIocpEngine.SetWorkerCount(AWorkerCount: Integer);
var
  j:Integer;
  i: Integer;
begin
  //if FActive then SafeStop;
  if AWorkerCount <= 0 then
    FWorkerCount := (GetCPUCount shl 1) -1
  else
    FWorkerCount := AWorkerCount;

  if not FActive then Exit;

  j := FWorkerCount - FActiveWorkerCount;
  if j > 0 then
  begin
    for i := 0 to j - 1 do
    begin
      CreateWorker(False);
    end;
  end else
  begin
    j := -j;
    for i := 0 to j - 1 do
    begin
      PostAExitRequest;
    end;
  end;

end;

procedure TIocpEngine.Start;
var
  i: Integer;
  AWorker: TIocpWorker;
  lvCpuCount:Integer;
begin
  lvCpuCount := GetCPUCount;

  FIocpCore.DoInitialize;

  if FSafeStopSign <> nil then
  begin
    FSafeStopSign.Free;
  end;

  FSafeStopSign := TEvent.Create(nil, True, False, '');
  for i := 0 to FWorkerCount - 1 do
  begin
    AWorker := TIocpWorker.Create(FIocpCore);
    AWorker.FIocpEngine := Self;

    AWorker.FreeOnTerminate := True;
    {$IFDEF UNICODE}
    AWorker.Start;
    {$ELSE}
    AWorker.Resume;
    {$ENDIF}
    FWorkerList.Add(AWorker);

    // set worker use processor
    SetThreadIdealProcessor(AWorker.Handle, i mod lvCpuCount);
  end;
  FActive := true;
end;

function TIocpEngine.StopWorkers(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
  i:Integer;
  lvEvent:TEvent;
  lvWrited:Boolean;
begin
  Result := False;
  if not FActive then
  begin
    Result := true;
    exit;
  end;

  if WorkersIsAlive then
  begin
    for i := 0 to FWorkerList.Count -1 do
    begin
      if not FIocpCore.PostIOExitRequest then
      begin
        RaiseLastOSError;
      end;
    end;
  end else
  begin
    // all worker thread is dead
    FWorkerList.Clear;
    if FSafeStopSign <> nil then FSafeStopSign.SetEvent;
  end;

  lvWrited := false;
  if FSafeStopSign <> nil then
  begin
    lvEvent := FSafeStopSign; 
    l := GetTickCount;
    while True do
    begin
      {$IFDEF MSWINDOWS}
      SwitchToThread;
      {$ELSE}
      TThread.Yield;
      {$ENDIF}

      Sleep(10);

      // 继续投递，避免响应失败的工作线程
      FIocpCore.PostIOExitRequest;

      // wait all works down
      if lvEvent.WaitFor(1000) = wrSignaled then
      begin
        FSafeStopSign.Free;
        FSafeStopSign := nil;
        Result := true;
        Break;
      end;

      if not lvWrited then
      begin
        lvWrited := True;
        SafeWriteFileMsg(GetStateINfo, Name + '_STOP');
      end;

      if tick_diff(l, GetTickCount) > pvTimeOut then
      begin
        Result := false;
        Break;
      end;
    end;
  end;  
end;

function TIocpEngine.WorkersIsAlive: Boolean;
var
  i: Integer;
  lvCode:Cardinal;
begin
  Result := false;
  for i := FWorkerList.Count -1 downto 0 do
  begin
    if GetExitCodeThread(TThread(FWorkerList[i]).Handle, lvCode) then
    begin
      if lvCode=STILL_ACTIVE then
      begin
        Result := true;
        Break;
      end;
    end;
  end;

end;

procedure TIocpEngine.WriteStateINfo(const pvStrings:TStrings);
var
  i:Integer;
begin
  pvStrings.Add(Format(strDebugINfo, [BoolToStr(self.FActive, True), self.WorkerCount]));

  self.FWorkerLocker.lock;
  try
    for i := 0 to FWorkerList.Count - 1 do
    begin
      pvStrings.Add(Format(strDebug_WorkerTitle, [i + 1]));
      TIocpWorker(FWorkerList[i]).WriteStateINfo(pvStrings);
    end;
  finally
    self.FWorkerLocker.Leave;
  end;
end;

procedure TIocpRequest.CancelRequest;
begin
  
end;

constructor TIocpRequest.Create;
begin
  inherited Create;
  FThreadID := 0;
  FOverlapped.iocpRequest := self;
  FOverlapped.refCount := 0;
end;

destructor TIocpRequest.Destroy;
begin
  if __free_flag = -1 then
  begin
    Assert(__free_flag = 0);
  end;
  __free_flag := -1;
  inherited;
end;

constructor TIocpRequestSingleLink.Create(pvMaxSize: Integer = 1024);
begin
  inherited Create;
  FMaxSize := pvMaxSize;
end;

destructor TIocpRequestSingleLink.Destroy;
begin
  inherited Destroy;
end;


function TIocpRequestSingleLink.Pop: TIocpRequest;
begin
  Result := nil;

  if FHead <> nil then
  begin
    Result := FHead;
    FHead := FHead.FNext;
    if FHead = nil then FTail := nil;

    Dec(FCount);
  end;

end;

function TIocpRequestSingleLink.Push(pvRequest:TIocpRequest): Boolean;
begin
  if FCount < FMaxSize then
  begin
    pvRequest.FNext := nil;

    if FHead = nil then
      FHead := pvRequest
    else
      FTail.FNext := pvRequest;

    FTail := pvRequest;

    Inc(FCount);
    Result := true;
  end else
  begin
    Result := false;
  end;

end;

procedure TIocpRequestSingleLink.SetMaxSize(pvMaxSize:Integer);
begin
  FMaxSize := pvMaxSize;
  if FMaxSize <=0 then FMaxSize := 10;
end;

procedure TIocpRequestDoublyLinked.add(pvContext: TIocpRequest);
begin
  FLocker.lock;
  try
    if FHead = nil then
    begin
      FHead := pvContext;
    end else
    begin
      FTail.FNext := pvContext;
      pvContext.FPre := FTail;
    end;

    FTail := pvContext;
    FTail.FNext := nil;

    inc(FCount);
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpRequestDoublyLinked.Create;
begin
  inherited Create;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'onlineContext';
  FHead := nil;
  FTail := nil;
end;

destructor TIocpRequestDoublyLinked.Destroy;
begin
  FreeAndNil(FLocker);
  inherited Destroy;
end;

function TIocpRequestDoublyLinked.Pop: TIocpRequest;
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

function TIocpRequestDoublyLinked.remove(pvContext: TIocpRequest): Boolean;
begin


  Result := false;
  FLocker.lock;
  try
//    if FCount = 0 then
//    begin
//      FCount := 0;
//    end;
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

    //  set pvConext.FPre is FTail
    if FTail = pvContext then FTail := pvContext.FPre;

    pvContext.FPre := nil;
    pvContext.FNext := nil;
    Result := true;
  finally
    FLocker.unLock;
  end;
end;

procedure TIocpRequestDoublyLinked.write2List(pvList: TList);
var
  lvItem:TIocpRequest;
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

procedure TIocpRequest.CancelIoEx(const pvHandle: THandle);
begin
  if Assigned(DiocpCancelIoEx) then
  begin
    DiocpCancelIoEx(pvHandle, LPWSAOVERLAPPED(OverlappedPtr));
  end;
end;

procedure TIocpRequest.CheckThreadIn;
begin
  if FThreadID <> 0 then
  begin
    raise Exception.CreateFmt('(%d,%d,%d(%s))当前对象已经被其他线程正在使用',
       [GetCurrentThreadID, FThreadID, IntPtr(Self), self.ClassName]);
  end;
  FThreadID := GetCurrentThreadID;
end;

procedure TIocpRequest.CheckThreadOut;
begin
  if FThreadID= 0 then
  begin
    raise Exception.CreateFmt('(%d(%s))请勿重复归还',
       [IntPtr(Self), self.ClassName]);
  end;
  FThreadID := 0;  
end;

function TIocpRequest.GetStateInfo: String;
begin
  Result :=Format('%s %s', [Self.ClassName, FRemark]);
  if FResponding then
  begin
    Result :=Result + sLineBreak + Format('start:%s',
       [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime)]);
  end else
  begin
    Result :=Result + sLineBreak + Format('start:%s, end:%s',
      [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime),
        FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondEndTime)]);
  end;
end;

procedure TIocpRequest.HandleResponse;
begin
  
end;

function TIocpRequest.OverlappedPtr: POVERLAPPEDEx;
begin
  Result := POVERLAPPEDEx(@FOverlapped);
end;

procedure TIocpRequest.ResponseDone;
begin
  
end;

procedure TIocpRequest.SetWorkHintInfo(const pvHint: String);
var
  lvThreadID:THandle;
begin
  if FIocpWorker <> nil then
  begin
    lvThreadID := GetCurrentThreadID();
    Assert(lvThreadID = FWorkerThreadID,
     Format('(%d,%d)只能在本线程中执行设置线程WorkHint信息', [lvThreadID, FWorkerThreadID]));
    FIocpWorker.SetHintInfo(pvHint);
  end;
end;

destructor TIocpASyncRequest.Destroy;
begin

  inherited;
end;

procedure TIocpASyncRequest.DoCleanUp;
begin
  Self.Remark := '';
  FOnASyncEvent := nil;
end;


function TIocpASyncRequest.GetStateINfo: String;
var
  lvEndTime:Cardinal;
begin
  if FEndTime <> 0 then lvEndTime := FEndTime else lvEndTime := GetTickCount;
  Result := '';
  if Remark <> '' then
  begin
    Result := Remark + sLineBreak;
  end;

  Result := Result + Format(strDebug_RequestState, [BoolToStr(FEndTime <> 0, True), lvEndTime - FEndTime]);
end;

procedure TIocpASyncRequest.HandleResponse;
begin
  try
    FStartTime := GetTickCount;
    FEndTime := 0;
    if Assigned(FOnASyncEvent) then FOnASyncEvent(Self);
  finally
    FEndTime := GetTickCount;
  end;
end;

initialization
{$IFDEF DEBUG_ON}
  workerCounter := 0;
{$ENDIF}
  __ProcessIDStr := IntToStr(GetCurrentProcessId);


finalization
  StopDiocpEngine;
{$IFDEF DEBUG_ON}
  if IsDebugMode then
    Assert(workerCounter <= 0, ('diocp_core_engine workerCounter, has dead thread? current worker Counter:' + IntToStr(workerCounter)));
{$ENDIF}


end.
