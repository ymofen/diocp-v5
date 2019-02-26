unit utils_grouptask;

interface

uses
  utils_queues, Classes, SysUtils,
  {$IFDEF MSWINDOWS} Windows, Messages, ActiveX, {$ENDIF}
  SyncObjs, utils_strings;

type
  TGroupTaskWorker = class;

  TGroupTask = class;

  TGroupTaskNotifyEvent = procedure(pvSender: TGroupTask; pvWorker:
      TGroupTaskWorker; pvData: Pointer) of object;


  TGroupTask = class(TObject)
  private
    FMaxWorkerCount:Integer;
    FIdleInterval:Cardinal;
    FWorkerCounter:Integer;
    FTaskCounter:Integer;
    FTerminated:Boolean;
    FLocker:TCriticalSection;
    FDebugInfo: String;
    FDataQueue: TSafeQueue;
    FEnable: Boolean;

    FWorkerAlive: Boolean;
    {$IFDEF MSWINDOWS}
    FNeedCoInitialize: Boolean;
    {$ENDIF}
    FOnCancelTask: TGroupTaskNotifyEvent;
    FOnWorkerDestroy: TGroupTaskNotifyEvent;
    FOnWorkerCreate: TGroupTaskNotifyEvent;
    FOnWorkerException: TGroupTaskNotifyEvent;
    FOnWorkerExecute: TGroupTaskNotifyEvent;    
    FOnWorkerIdle: TGroupTaskNotifyEvent;

    procedure NotifyDestroyWorker(pvWorker: TGroupTaskWorker);
  protected
    // 暂时没用
    procedure DoLock();
    procedure DoUnLock();
  protected
    procedure DoTask(pvWorker: TGroupTaskWorker; pvData: Pointer); virtual;

    /// <summary>
    ///   执行取消队列任务
    /// </summary>
    procedure DoCancelTask();
  public
    constructor Create;
    destructor Destroy; override;

    procedure CheckCreateWorker(const pvNum:Integer);



    /// <summary>
    ///   投递一个任务, 触发工作线程工作
    /// </summary>
    procedure PostATask(pvData:Pointer);

    /// <summary>
    ///   停止工作(设置等待当前工作停止超时(单位ms))
    ///   停止工作线程, 取消剩余任务(清空数据队列)
    /// </summary>
    /// <returns>成功停止返回true</returns>
    function StopWorker(pvTimeOut: Cardinal): Boolean;

    /// <summary>
    ///  是否允许执行任务(不停止工作线程)
    /// </summary>
    property Enable: Boolean read FEnable write FEnable;

    

    


    {$IFDEF MSWINDOWS}
    /// <summary>
    ///   线程中是否需要执行CoInitlize
    ///   Windows系统有效
    /// </summary>
    property NeedCoInitialize: Boolean read FNeedCoInitialize write
        FNeedCoInitialize; 
    {$ENDIF}

    /// <summary>
    ///   取消任务通知
    /// </summary>
    property OnCancelTask: TGroupTaskNotifyEvent read FOnCancelTask write
        FOnCancelTask;

    /// <summary>
    ///   任务回调函数
    /// </summary>
    property OnWorkerExecute: TGroupTaskNotifyEvent read FOnWorkerExecute write
        FOnWorkerExecute;

    property MaxWorkerCount: Integer read FMaxWorkerCount;
    property TaskCounter: Integer read FTaskCounter;
    property Terminated: Boolean read FTerminated;

    property WorkerCounter: Integer read FWorkerCounter;
        
    property OnWorkerCreate: TGroupTaskNotifyEvent read FOnWorkerCreate write FOnWorkerCreate; 

    property OnWorkerDestroy: TGroupTaskNotifyEvent read FOnWorkerDestroy write FOnWorkerDestroy;

    property OnWorkerException: TGroupTaskNotifyEvent read FOnWorkerException write FOnWorkerException;

    property OnWorkerIdle: TGroupTaskNotifyEvent read FOnWorkerIdle write
        FOnWorkerIdle;

  end;

  TGroupTaskWorker = class(TThread)
  private
    FOwner: TGroupTask;
    {$IFDEF MSWINDOWS}
    FCoInitialized:Boolean;
    {$ENDIF}
    FDataPtr: Pointer;
  public
    constructor Create(AOwner: TGroupTask);
    destructor Destroy; override;

    {$IFDEF MSWINDOWS}
    /// <summary>
    ///   current worker invoke
    /// </summary>
    procedure CheckCoInitializeEx(pvReserved: Pointer = nil; coInit: Longint = 0);
    {$ENDIF}

    procedure Execute; override;


    property DataPtr: Pointer read FDataPtr write FDataPtr;

  end;


implementation

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
  Result := 5;
  {$ENDIF !POSIX}
  {$ENDIF !MSWINDOWS}
end;

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;

function CheckThreadIsAlive(const AThread: TThread): Boolean;
var
  lvCode:Cardinal;
begin
  Result := false;
  if (AThread <> nil) and (GetExitCodeThread(AThread.Handle, lvCode)) then
  begin
    if lvCode=STILL_ACTIVE then
    begin
      Result := true;
    end;
  end;
end;

constructor TGroupTask.Create;
begin
  inherited Create;
  FIdleInterval := 1000;
  FLocker := TCriticalSection.Create;
  FDataQueue := TSafeQueue.Create;

  FEnable := true;
end;

destructor TGroupTask.Destroy;
begin
  StopWorker(30000);
  FDataQueue.Free;
  FLocker.Free;
  inherited Destroy;
end;

procedure TGroupTask.CheckCreateWorker(const pvNum:Integer);
var
  i, j: Integer;
  lvWorker:TGroupTaskWorker;
begin
  if pvNum = 0 then
  begin
    FMaxWorkerCount := GetCPUCount;
  end else
    FMaxWorkerCount := pvNum;
  j := FWorkerCounter;  
  for i := j to FMaxWorkerCount -1 do
  begin
    lvWorker := TGroupTaskWorker.Create(Self);
    {$IF RTLVersion<25}
    lvWorker.Resume;
    {$ELSE}
    lvWorker.Start;
    {$IFEND}
  end;   
end;

procedure TGroupTask.DoCancelTask;
var
  lvData:Pointer;
begin
  while FDataQueue.DeQueue(lvData) do
  begin
    if Assigned(FOnCancelTask) then
    begin
      FOnCancelTask(Self, nil, lvData);
    end;
  end;

end;

procedure TGroupTask.DoLock;
begin
  FLocker.Enter;
end;

procedure TGroupTask.DoUnLock;
begin
  FLocker.Leave;
end;

procedure TGroupTask.DoTask(pvWorker: TGroupTaskWorker; pvData: Pointer);
begin
  if Assigned(FOnWorkerExecute) then FOnWorkerExecute(Self, pvWorker, pvData);
end;



procedure TGroupTask.NotifyDestroyWorker(pvWorker: TGroupTaskWorker);
begin
  if Assigned(FOnWorkerDestroy) then
  begin
    FOnWorkerDestroy(Self, pvWorker, nil);
  end;
end;



procedure TGroupTask.PostATask(pvData: Pointer);
begin
  FDataQueue.EnQueue(pvData);
  InterlockedIncrement(FTaskCounter);
end;

function TGroupTask.StopWorker(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
begin
  Result := true;
  FEnable := false;
  self.FTerminated := True;

  l := GetTickCount;
  while FWorkerCounter > 0 do
  begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}

    if tick_diff(l, GetTickCount) > pvTimeOut then
    begin
      Result := false;
      Break;
    end;
  end;


  DoCancelTask;
end;

constructor TGroupTaskWorker.Create(AOwner: TGroupTask);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  FOwner := AOwner;
end;

destructor TGroupTaskWorker.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
procedure TGroupTaskWorker.CheckCoInitializeEx(pvReserved: Pointer = nil; coInit:
    Longint = 0);
begin
  if not FCoInitialized then
  begin
    CoInitializeEx(pvReserved, coInit);
    FCoInitialized := true;
  end;
end;
{$ENDIF}

procedure TGroupTaskWorker.Execute;
var
  lvWaitResult:TWaitResult;
  lvData: Pointer;
  lvIdleTick:Cardinal;
  lvFlag:Integer;
begin
  try
    InterlockedIncrement(FOwner.FWorkerCounter);
    if Assigned(FOwner.FOnWorkerCreate) then
    begin
      FOwner.FOnWorkerCreate(FOwner, self, nil);
    end;
    lvIdleTick := utils_strings.GetTickCount;
    while not FOwner.FTerminated do
    begin
      lvFlag := 0;
      try
        if (FOwner.FEnable) and (FOwner.FTaskCounter > 0) then
        begin
          if FOwner.FDataQueue.DeQueue(lvData) then
          begin
            lvFlag := 1;
            lvIdleTick := utils_strings.GetTickCount;
            InterlockedDecrement(FOwner.FTaskCounter);
            try
              {$IFDEF MSWINDOWS}
              if FOwner.NeedCoInitialize then
              begin
                CheckCoInitializeEx();
              end;
              {$ENDIF}
              FOwner.DoTask(Self, lvData);

            except
              on E:Exception do
              begin
                if Assigned(FOwner.FOnWorkerException) then
                begin
                  FOwner.FOnWorkerException(FOwner, Self, E);
                end;
              end;
            end;
          end;
        end;

        if lvFlag = 0 then
        begin
          if tick_diff(lvIdleTick, utils_strings.GetTickCount) >FOwner.FIdleInterval then
          begin
            if Assigned(FOwner.FOnWorkerIdle) then
            begin
              FOwner.FOnWorkerIdle(FOwner, self, nil);
            end;
            lvIdleTick := utils_strings.GetTickCount;
          end;

          // 降低cpu占用
          Sleep(1);

          {$IFDEF MSWINDOWS}
          SwitchToThread;
          {$ELSE}
          TThread.Yield;
          {$ENDIF}
        end;
      except
        on e:Exception do
        begin
          if Assigned(FOwner.FOnWorkerException) then
          begin
            FOwner.FOnWorkerException(FOwner, Self, E);
          end;
        end;
      end;
    end;
  finally
    FOwner.NotifyDestroyWorker(Self);
    {$IFDEF MSWINDOWS}
    if FCoInitialized then CoUninitialize();
    {$ENDIF}              
    InterlockedDecrement(FOwner.FWorkerCounter);
  end;


end;



end.
