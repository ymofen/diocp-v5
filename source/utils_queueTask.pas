unit utils_queueTask;

interface

uses
  utils_queues, Classes, SysUtils,
  {$IFDEF MSWINDOWS} Windows, Messages, ActiveX, {$ENDIF}
  SyncObjs;

type
  TDTaskWorker = class;
  TDQueueTask = class;
  TQueueTaskNotifyEvent = procedure(pSender: TDQueueTask; pvTaskData: Pointer) of
      object;

  TDQueueTask = class(TObject)
  private
    FLocker:TCriticalSection;
    FWorker: TDTaskWorker; 
    FDebugInfo: String;
    FDataQueue: TSafeQueue;
    FEnable: Boolean;
    FOnExecute: TQueueTaskNotifyEvent;
    FWorkerAlive: Boolean;
    FCommEvent: TEvent;
    FDataPtr: Pointer;
    FLastErrorMessage: String;

    {$IFDEF MSWINDOWS}
    FNeedCoInitialize: Boolean;
    {$ENDIF}
    FOnCancelTask: TQueueTaskNotifyEvent;
    procedure NotifyDestroyWorker;
  protected
    procedure CheckForWorker;

    procedure DoTask(pvData: Pointer); virtual;

    property CommEvent: TEvent read FCommEvent;

    /// <summary>
    ///   执行取消队列任务
    /// </summary>
    procedure DoCancelTask();
  public
    constructor Create;
    destructor Destroy; override;


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
    ///   存放额外数据
    /// </summary>
    property DataPtr: Pointer read FDataPtr write FDataPtr;

    /// <summary>
    ///  是否允许执行任务(不停止工作线程)
    /// </summary>
    property Enable: Boolean read FEnable write FEnable;

    /// <summary>
    ///   任务数据队列
    /// </summary>
    property DataQueue: TSafeQueue read FDataQueue;

    /// <summary>
    ///   最后抛出的一个错误信息
    /// </summary>
    property LastErrorMessage: String read FLastErrorMessage;

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
    property OnCancelTask: TQueueTaskNotifyEvent read FOnCancelTask write
        FOnCancelTask;

    /// <summary>
    ///   任务回调函数
    /// </summary>
    property OnExecute: TQueueTaskNotifyEvent read FOnExecute write FOnExecute;



  end;

  TDTaskWorker = class(TThread)
  private
    FOwner: TDQueueTask;
    FNotify: TEvent;
    {$IFDEF MSWINDOWS}
    FCoInitialized:Boolean;
    {$ENDIF}
  public
    constructor Create(AOwner: TDQueueTask);
    destructor Destroy; override;

    {$IFDEF MSWINDOWS}
    /// <summary>
    ///   current worker invoke
    /// </summary>
    procedure CheckCoInitializeEx(pvReserved: Pointer = nil; coInit: Longint = 0);
    {$ENDIF}

    procedure Execute; override;
  end;


implementation

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

constructor TDQueueTask.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create;
  FDataQueue := TSafeQueue.Create;
  FCommEvent := TEvent.Create(nil,false,false,'');

  FEnable := true;
end;

destructor TDQueueTask.Destroy;
begin
  StopWorker(3000);
  FDataQueue.Free;
  FLocker.Free;
  FCommEvent.Free;
  inherited Destroy;
end;

procedure TDQueueTask.DoCancelTask;
var
  lvData:Pointer;
begin
  while FDataQueue.DeQueue(lvData) do
  begin
    if Assigned(FOnCancelTask) then
    begin
      FOnCancelTask(Self, lvData);
    end;
  end;
  
end;

procedure TDQueueTask.DoTask(pvData: Pointer);
begin
  if Assigned(FOnExecute) then FOnExecute(Self, pvData); 
end;

procedure TDQueueTask.CheckForWorker;
begin
  FLocker.Enter;
  try
    if FWorker = nil then
    begin
      FWorker := TDTaskWorker.Create(Self);
    {$IF RTLVersion<25}
      FWorker.Resume;
    {$ELSE}
      FWorker.Start;
    {$IFEND}
      Sleep(10);
    end;

    if FWorker <> nil then
    begin
      FWorker.FNotify.SetEvent;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TDQueueTask.NotifyDestroyWorker;
begin
  FLocker.Enter;
  try
    FWorkerAlive := False;
    FWorker := nil;
  finally
    FLocker.Leave;
  end;
end;



procedure TDQueueTask.PostATask(pvData: Pointer);
begin
  FDataQueue.EnQueue(pvData);
  CheckForWorker;
end;

function TDQueueTask.StopWorker(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
begin
  Result := true;
  FEnable := false;
  if FWorker <> nil then
  begin
    FWorker.Terminate;
    FWorker.FNotify.SetEvent;
    FCommEvent.SetEvent;


    l := GetTickCount;
    while CheckThreadIsAlive(FWorker) do
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
    FWorker := nil;
  end;

  DoCancelTask;
end;

constructor TDTaskWorker.Create(AOwner: TDQueueTask);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  FNotify := TEvent.Create(nil,false,false,'');
  FOwner := AOwner;
end;

destructor TDTaskWorker.Destroy;
begin
  FNotify.Free;
  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
procedure TDTaskWorker.CheckCoInitializeEx(pvReserved: Pointer = nil; coInit:
    Longint = 0);
begin
  if not FCoInitialized then
  begin
    CoInitializeEx(pvReserved, coInit);
    FCoInitialized := true;
  end;
end;
{$ENDIF}

procedure TDTaskWorker.Execute;
var
  lvWaitResult:TWaitResult;
  lvData: Pointer;
begin
  try
    while not self.Terminated do
    begin
      FOwner.FDebugInfo := 'Thread.Execute::FNotify.WaitFor()';

      lvWaitResult := FNotify.WaitFor(1000 * 30);

      if (lvWaitResult=wrSignaled) then
      begin
        FOwner.FDebugInfo := 'Thread.Execute::FNotify.WaitFor(), succ';
        while not self.Terminated do
        begin
          if not FOwner.FDataQueue.DeQueue(lvData) then Break;
          try
            FOwner.FDebugInfo := 'Thread.Execute::DoTask';
            {$IFDEF MSWINDOWS}
            if FOwner.NeedCoInitialize then
            begin
              CheckCoInitializeEx();
            end;
            {$ENDIF}
            FOwner.DoTask(lvData);
          except
            on E:Exception do
            begin
              FOwner.FLastErrorMessage := E.Message;
              //FOwner.incErrorCounter;
            end;
          end;
        end;
      end else if lvWaitResult = wrTimeout then
      begin
        Break;
      end;
    end;
  finally
    FOwner.NotifyDestroyWorker;
    {$IFDEF MSWINDOWS}
    if FCoInitialized then CoUninitialize();
    {$ENDIF}
  end;


end;



end.
