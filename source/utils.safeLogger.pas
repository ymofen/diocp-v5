(*
 *	 Unit owner: D10.Mofen
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *)

unit utils.safeLogger;

interface

uses
  Classes, utils.queues, SysUtils, SyncObjs{$IFDEF MSWINDOWS}, Windows, Messages {$ENDIF};

type

  TLogLevel=(lgvError, lgvWarning, lgvHint, lgvMessage, lgvDebug);

  TLogLevels = set of TLogLevel;

const
  TLogLevelCaption: array [TLogLevel] of string = ('error', 'warning', 'hint', 'message', 'debug');

  LogAllLevels = [lgvError, lgvWarning, lgvHint, lgvMessage, lgvDebug];

type
  TSafeLogger = class;
  TSyncMainThreadType = (rtSync{$IFDEF MSWINDOWS}, rtPostMessage {$ENDIF});

  TThreadStackFunc = function(AThread:TThread):string;

  TLogDataObject = class(TObject)
  public
    FThreadID:Cardinal;
    FTime:TDateTime;
    FLogLevel:TLogLevel;
    FMsg:string;
    FMsgType:string;
  end;

  TBaseAppender = class(TObject)
  protected
    FOwner:TSafeLogger;
  protected
    procedure AppendLog(pvData:TLogDataObject); virtual; abstract;
  end;

  TConsoleAppender = class(TBaseAppender)
  protected
    procedure AppendLog(pvData:TLogDataObject); override;
  public
  end;

  TStringsAppender = class(TBaseAppender)
  private
    FAddThreadINfo: Boolean;
    FAddTimeInfo: Boolean;
    FAppendLineBreak: Boolean;
    FMaxLines: Integer;
    FStrings: TStrings;
  protected
    procedure AppendLog(pvData:TLogDataObject); override;
  public
    constructor Create(AStrings: TStrings);
    property AddThreadINfo: Boolean read FAddThreadINfo write FAddThreadINfo;
    property AddTimeInfo: Boolean read FAddTimeInfo write FAddTimeInfo;
    property AppendLineBreak: Boolean read FAppendLineBreak write FAppendLineBreak;

    property MaxLines: Integer read FMaxLines write FMaxLines;
  end;

  TLogFileAppender = class(TBaseAppender)
  private
    FProcessIDStr: String;

    FAddProcessID: Boolean;
    FFilePreFix:String;
    FAddThreadINfo: Boolean;
    FAddThreadIDToFileID:Boolean;
    FBasePath: string;
    FLogFile: TextFile;

    FInitialized: Boolean;
    procedure checkInitialized;
    function openLogFile(pvPre: String = ''): Boolean;
  protected
    procedure AppendLog(pvData:TLogDataObject); override;
  public
    constructor Create(pvAddThreadINfo: Boolean);
    property AddProcessID: Boolean read FAddProcessID write FAddProcessID;
    property AddThreadIDToFileID: Boolean read FAddThreadIDToFileID write
        FAddThreadIDToFileID;

    property AddThreadINfo: Boolean read FAddThreadINfo write FAddThreadINfo;

    property FilePreFix: String read FFilePreFix write FFilePreFix;
  end;


  TLogWorker = class(TThread)
  private
    {$IFDEF MSWINDOWS}
    FMessageEvent: TEvent;
    {$ENDIF}
    FSafeLogger: TSafeLogger;
    FNotify: TEvent;
    // temp for sync method
    FTempLogData: TLogDataObject;
    procedure ExecuteLogData(const pvData:TLogDataObject);
    procedure InnerSyncLogData;
  public
    constructor Create(ASafeLogger: TSafeLogger);
    destructor Destroy; override;
    procedure Execute; override;
  end;


  TSafeLogger = class(TObject)
  private
    FDebugInfo: String;
    FDebugData: Pointer;
    FWorkerAlive:Boolean;
    
    FLogWorker:TLogWorker;
    FDataQueue: TBaseQueue;
    FOwnsAppender:Boolean;

    FAppender: TBaseAppender;
    FAppendInMainThread: Boolean;

    FSyncMainThreadType: TSyncMainThreadType;

    procedure ExecuteLogData(const pvData:TLogDataObject);
  private
    FEnable: Boolean;
    FWorkerCounter:Integer;
    FErrorCounter: Integer;
    FPostCounter: Integer;
    FResponseCounter: Integer;

    /// <summary>
    ///   check worker thread is alive
    /// </summary>
    function workersIsAlive(const pvWorker: TLogWorker): Boolean;

    procedure checkForWorker;
    procedure stopWorker(pvTimeOut: Cardinal);
  private
  {$IFDEF MSWINDOWS}
    FLogFilter: TLogLevels;
    FStateLocker:TCriticalSection;
    FWorking:Boolean;
    FMessageHandle: HWND;
    FName: String;
    procedure DoMainThreadWork(var AMsg: TMessage);
    procedure SetWorking(pvWorking:Boolean);
    function isWorking():Boolean;
    procedure DoWork();
  {$ENDIF}
    procedure incWorkerCount;
    procedure decWorker(pvWorker: TLogWorker);
  public
    procedure IncErrorCounter;
    procedure IncResponseCounter;
  public
    constructor Create;
    destructor Destroy; override;

    procedure start;

    /// <summary>
    ///   task current info
    /// </summary>
    function getStateINfo: String;


    procedure setAppender(pvAppender: TBaseAppender; pvOwnsAppender: Boolean =
        true);

    procedure logMessage(pvMsg: string; pvMsgType: string = ''; pvLevel: TLogLevel
        = lgvMessage); overload;

    procedure logMessage(pvMsg: string; const args: array of const; pvMsgType:
        string = ''; pvLevel: TLogLevel = lgvMessage); overload;

    property Appender: TBaseAppender read FAppender;

    property SyncMainThreadType: TSyncMainThreadType read FSyncMainThreadType write
        FSyncMainThreadType;

    property AppendInMainThread: Boolean read FAppendInMainThread write
        FAppendInMainThread;
    property DebugData: Pointer read FDebugData;
    property DebugInfo: String read FDebugInfo;

    property Enable: Boolean read FEnable write FEnable;

    /// <summary>
    ///   设置要写入的日志级别, 默认所有
    /// </summary>
    property LogFilter: TLogLevels read FLogFilter write FLogFilter;
    property Name: String read FName write FName;





  end;

var
  sfLogger:TSafeLogger;
  __ProcessIDStr :String;
  __GetThreadStackFunc: TThreadStackFunc;

procedure SafeWriteFileMsg(pvMsg:String; pvFilePre:string);

implementation


var
  __dataObjectPool:TBaseQueue;

{$IFDEF MSWINDOWS}
const
  WM_SYNC_METHOD = WM_USER + 1;
  WM_NOTIFY_WORK = WM_USER + 2;
{$ENDIF}

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;

procedure writeSafeInfo(pvMsg:string);
var
  lvFileName, lvBasePath:String;
  lvLogFile: TextFile;
begin
  try
    lvBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
    ForceDirectories(lvBasePath);
    lvFileName :=lvBasePath + '\__safe_' + FormatDateTime('mmddhhnnsszzz', Now()) + '.log';

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

procedure SafeWriteFileMsg(pvMsg:String; pvFilePre:string);
var
  lvFileName, lvBasePath:String;
  lvLogFile: TextFile;
begin
  try
    lvBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
    ForceDirectories(lvBasePath);
    lvFileName :=lvBasePath + '\' + __ProcessIDStr+ '_' + pvFilePre +
     FormatDateTime('mmddhhnn', Now()) + '.log';

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

procedure TSafeLogger.checkForWorker;
begin
  if lock_cmp_exchange(False, True, FWorkerAlive) = False then
  begin
    if FLogWorker = nil then
    begin
      FLogWorker := TLogWorker.Create(Self);
    {$IFDEF UNICODE}
      FLogWorker.Start;
    {$ELSE}
      FLogWorker.Resume;
    {$ENDIF}
    end;
  end;
  if FLogWorker <> nil then
  begin
    FLogWorker.FNotify.SetEvent;
  end;
end;

constructor TSafeLogger.Create;
begin
  inherited Create;
  FLogFilter := [lgvError, lgvWarning, lgvHint, lgvMessage, lgvDebug];
  FWorkerAlive := False;
  FEnable := true;
  FSyncMainThreadType := rtSync;
{$IFDEF MSWINDOWS}
  FSyncMainThreadType := rtPostMessage;
  FWorking := False;
  FMessageHandle := AllocateHWnd(DoMainThreadWork);
  FStateLocker := TCriticalSection.Create;
{$ENDIF}
  FDataQueue := TBaseQueue.Create();
  FAppender := nil;
  FOwnsAppender := false;
  FWorkerCounter := 0;
end;

destructor TSafeLogger.Destroy;
begin
  FEnable := false;
  
  stopWorker(30000);

  FDataQueue.FreeDataObject;
  FreeAndNil(FDataQueue);
  if FOwnsAppender then
  begin
    if FAppender <> nil then
    begin
      FAppender.Free;
      FAppender := nil;
    end;
  end;
{$IFDEF MSWINDOWS}
  DeallocateHWnd(FMessageHandle);
  FStateLocker.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
procedure TSafeLogger.DoMainThreadWork(var AMsg: TMessage);
begin
  if AMsg.Msg = WM_SYNC_METHOD then
  begin
    try
      if not FEnable then Exit;
      FDebugInfo := 'DoMainThreadWork:ExecuteLogData(TLogDataObject(AMsg.WParam))__Start';
      ExecuteLogData(TLogDataObject(AMsg.WParam));
      FDebugInfo := 'DoMainThreadWork:ExecuteLogData(TLogDataObject(AMsg.WParam))__END';
    finally
      if AMsg.LPARAM <> 0 then
        TEvent(AMsg.LPARAM).SetEvent;
    end;
  end else if AMsg.Msg = WM_NOTIFY_WORK then
  begin
    FDebugInfo :=   'DoMainThreadWork:WM_NOTIFY_WORK -- Start';
    SetWorking(True);
    try
      DoWork();
    finally
      SetWorking(False);
      FDebugInfo := 'DoMainThreadWork:WM_NOTIFY_WORK -- END';
    end;
  end else
    AMsg.Result := DefWindowProc(FMessageHandle, AMsg.Msg, AMsg.WPARAM, AMsg.LPARAM);
end;

procedure TSafeLogger.DoWork;
var
  lvPData:TLogDataObject;
begin
  while self.FEnable do
  begin 
    if not FDataQueue.DeQueue(Pointer(lvPData)) then Break;
    if lvPData <> nil then
    begin
      try
        FDebugData := lvPData;
        ExecuteLogData(lvPData);
      except
        IncErrorCounter;
      end;
      __dataObjectPool.EnQueue(lvPData);
    end;
  end;
end;

procedure TSafeLogger.SetWorking(pvWorking: Boolean);
begin
  FStateLocker.Enter;
  try
    FWorking := pvWorking;
  finally
    FStateLocker.Leave;
  end;                 
end;


function TSafeLogger.isWorking: Boolean;
begin
  FStateLocker.Enter;
  try
    Result := FWorking;
  finally
    FStateLocker.Leave;
  end;   
end;
{$ENDIF}

procedure TSafeLogger.ExecuteLogData(const pvData:TLogDataObject);
begin
  IncResponseCounter;
  if FAppender = nil then
  begin
    IncErrorCounter;
  end else
  begin
    FAppender.AppendLog(pvData);
  end;

end;

procedure TSafeLogger.IncErrorCounter;
begin
  InterlockedIncrement(FErrorCounter);
end;

procedure TSafeLogger.incWorkerCount;
begin
  InterlockedIncrement(FWorkerCounter);
end;



procedure TSafeLogger.decWorker(pvWorker: TLogWorker);
begin
  InterlockedDecrement(FWorkerCounter);
  FWorkerAlive := false;
  FLogWorker := nil;
end;

function TSafeLogger.getStateINfo: String;
var
  lvDebugINfo:TStrings;
begin
  lvDebugINfo := TStringList.Create;
  try
    lvDebugINfo.Add(Format('enable:%s, workerAlive:%s', [boolToStr(FEnable, True), boolToStr(FWorkerAlive, True)]));
    lvDebugINfo.Add(Format('post/response/error counter:%d / %d / %d',
       [self.FPostCounter,self.FResponseCounter,self.FErrorCounter]));
    Result := lvDebugINfo.Text;
  finally
    lvDebugINfo.Free;
  end;
end;

procedure TSafeLogger.IncResponseCounter;
begin
  InterlockedIncrement(FResponseCounter);
end;

{ TSafeLogger }

procedure TSafeLogger.logMessage(pvMsg: string; pvMsgType: string = '';
    pvLevel: TLogLevel = lgvMessage);
var
  lvPData:TLogDataObject;
begin
  if not FEnable then exit;

  if not (pvLevel in FLogFilter) then Exit;

  try
    lvPData := __dataObjectPool.DeQueue;
    if lvPData = nil then lvPData:=TLogDataObject.Create;
  {$IFDEF MSWINDOWS}
    lvPData.FThreadID := GetCurrentThreadId;
  {$ELSE}
    lvPData.FThreadID := TThread.CurrentThread.ThreadID;
  {$ENDIF};
    lvPData.FTime := Now();
    lvPData.FLogLevel := pvLevel;
    lvPData.FMsg := pvMsg;
    lvPData.FMsgType := pvMsgType;
    FDataQueue.EnQueue(lvPData);
    InterlockedIncrement(FPostCounter);
  {$IFDEF MSWINDOWS}
    if (FAppendInMainThread) and (FSyncMainThreadType = rtPostMessage) then
    begin
      if not isWorking then
      begin
        PostMessage(FMessageHandle, WM_NOTIFY_WORK, 0, 0);
      end;
    end else
    begin
      checkForWorker;
    end;
  {$ELSE}
    checkForWorker;
  {$ENDIF};
  except
    on E:Exception do
    begin
      SafeWriteFileMsg('TSafeLogger.logMessage记录' + pvMsg +'异常:' + e.Message, 'SafeLogger异常_');
    end;                                                                      
  end;
end;

procedure TSafeLogger.logMessage(pvMsg: string; const args: array of const;
    pvMsgType: string = ''; pvLevel: TLogLevel = lgvMessage);
begin
  logMessage(Format(pvMsg, args), pvMsgType, pvLevel);
end;

procedure TSafeLogger.setAppender(pvAppender: TBaseAppender; pvOwnsAppender:
    Boolean = true);
begin
  if (FAppender <> nil) and FOwnsAppender then
  begin
    FAppender.Free;
    FAppender := nil;
  end;

  if pvAppender <> nil then
  begin
    FAppender := pvAppender;
    FOwnsAppender := pvOwnsAppender;
    FAppender.FOwner := Self;
  end;
end;


procedure TSafeLogger.start;
begin
  //nothing to do ...
end;

procedure TSafeLogger.stopWorker(pvTimeOut: Cardinal);
var
  l:Cardinal;
  lvWrite:Boolean;
begin
  if FLogWorker <> nil then
  begin
    FLogWorker.Terminate;
    FLogWorker.FNotify.SetEvent;

    lvWrite := True;
    l := GetTickCount;
    while (FWorkerCounter > 0) and workersIsAlive(FLogWorker) do
    begin
      {$IFDEF MSWINDOWS}
      SwitchToThread;
      {$ELSE}
      TThread.Yield;
      {$ENDIF}

      if lvWrite then
      begin
        if tick_diff(l, GetTickCount) > 10000 then
        begin
          writeSafeInfo(FName + 'is dead, debugInfo:'  + FDebugInfo);
          lvWrite := false;
        end;
      end;
    end;


    FLogWorker := nil;
  end;
end;

function TSafeLogger.workersIsAlive(const pvWorker: TLogWorker): Boolean;
var
  lvCode:Cardinal;
begin
  Result := false;
  if (pvWorker <> nil) and (GetExitCodeThread(pvWorker.Handle, lvCode)) then
  begin
    if lvCode=STILL_ACTIVE then
    begin
      Result := true;
    end;
  end;
end;

constructor TLogWorker.Create(ASafeLogger: TSafeLogger);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  FNotify := TEvent.Create(nil,false,false,'');
  FSafeLogger := ASafeLogger;
  {$IFDEF MSWINDOWS}
  FMessageEvent := TEvent.Create(nil, true, False, '');
  {$ENDIF}


end;

destructor TLogWorker.Destroy;
begin
  FNotify.Free;
  {$IFDEF MSWINDOWS}
  FMessageEvent.Free;
  {$ENDIF}  
  inherited Destroy;
end;

procedure TLogWorker.Execute;
var
  lvPData:TLogDataObject;
  lvWaitResult:TWaitResult;
begin
  FSafeLogger.incWorkerCount;
  try
    while not self.Terminated do
    begin
      FSafeLogger.FDebugInfo := 'Thread.Execute::FNotify.WaitFor()';
      lvWaitResult := FNotify.WaitFor(1000 * 30);
      if (lvWaitResult=wrSignaled) then
      begin
        FSafeLogger.FDebugInfo := 'Thread.Execute::FNotify.WaitFor(), succ';
        while not self.Terminated do
        begin
          lvPData := FSafeLogger.FDataQueue.DeQueue;
          if lvPData = nil then Break;

          try
            FSafeLogger.FDebugData := lvPData;
            ExecuteLogData(lvPData);
          except
            FSafeLogger.incErrorCounter;
          end;
          
          /// push back to logData pool
          __dataObjectPool.EnQueue(lvPData);
        end;
      end else if lvWaitResult = wrTimeout then
      begin
        Break;
      end;
    end;
  finally
    FSafeLogger.decWorker(Self);
  end;
end;

procedure TLogWorker.ExecuteLogData(const pvData:TLogDataObject);
begin
  if FSafeLogger.FAppendInMainThread then
  begin
    if FSafeLogger.FSyncMainThreadType = rtSync then
    begin
      FTempLogData := pvData;
      FSafeLogger.FDebugInfo := 'Synchronize(InnerSyncLogData)';
      Synchronize(InnerSyncLogData);
    end
{$IFDEF MSWINDOWS}
    else if FSafeLogger.FSyncMainThreadType = rtPostMessage then
    begin
      FMessageEvent.ResetEvent;
      FSafeLogger.FDebugInfo := 'PostMessage';
      if PostMessage(FSafeLogger.FMessageHandle, WM_SYNC_METHOD, WPARAM(pvData), LPARAM(FMessageEvent)) then
      begin
        FSafeLogger.FDebugInfo := 'PostMessage succ, waitFor';
        FMessageEvent.WaitFor(INFINITE);
      end else
      begin
        FSafeLogger.incErrorCounter;
        // log exception
      end;
    end
{$ENDIF}
    ;
  end else
  begin
    FSafeLogger.ExecuteLogData(pvData);
  end;
end;

procedure TLogWorker.InnerSyncLogData;
begin
   FSafeLogger.ExecuteLogData(FTempLogData);
end;

constructor TStringsAppender.Create(AStrings: TStrings);
begin
  inherited Create;
  FStrings := AStrings;
  FAddTimeInfo := true;
  FAddThreadINfo := false;
  FAppendLineBreak := true;
  FMaxLines := 500;
end;

procedure TStringsAppender.AppendLog(pvData:TLogDataObject);
var
  lvMsg :String;
begin
  inherited;
  Assert(FStrings <> nil);
  lvMsg := '';
  if FAddTimeInfo then
  begin
    lvMsg := Format('%s[%s]',
        [FormatDateTime('hh:nn:ss:zzz', pvData.FTime)
          , TLogLevelCaption[pvData.FLogLevel]
        ]);
  end;

  if FAddThreadINfo then
  begin
    lvMsg := lvMsg + Format('[PID:%d,ThreadID:%d]',
        [GetCurrentProcessID(), pvData.FThreadID]);
  end;


  if lvMsg <> '' then lvMsg := lvMsg + ':' + pvData.FMsg else lvMsg := pvData.FMsg;


  if FStrings.Count > FMaxLines then FStrings.Clear;

  if Self.AppendLineBreak then
  begin
    FStrings.Add(lvMsg);
  end else
  begin
    FStrings.Add(lvMsg);
  end;
end;

procedure TLogFileAppender.AppendLog(pvData: TLogDataObject);
var
  lvMsg:String;
  lvPreFix :String;
begin
  checkInitialized;
  if FAddThreadIDToFileID then
  begin
    lvPreFix := FFilePreFix + pvData.FMsgType+ '_' + IntToStr(pvData.FThreadID) + '_';
  end else
  begin
    lvPreFix := FFilePreFix + pvData.FMsgType;
  end;


  if FAddProcessID then
    lvPreFix := FProcessIDStr + '_' + lvPreFix;
    
  if OpenLogFile(lvPreFix) then
  begin
    try
      if FAddThreadINfo then
      begin
        if FAddProcessID then
        begin     // 文件名已经添加了ProcessID
          lvMsg := Format('%s[%s][ThreadID:%d]:%s',
              [FormatDateTime('hh:nn:ss:zzz', pvData.FTime)
                , TLogLevelCaption[pvData.FLogLevel]
                , pvData.FThreadID
                , pvData.FMsg
              ]
              );
        end else
        begin
          lvMsg := Format('%s[%s][PID:%s,ThreadID:%d]:%s',
              [FormatDateTime('hh:nn:ss:zzz', pvData.FTime)
                , TLogLevelCaption[pvData.FLogLevel]
                , FProcessIDStr
                , pvData.FThreadID
                , pvData.FMsg
              ]
              );
        end;
      end else
      begin
        lvMsg := Format('%s[%s]:%s',
            [FormatDateTime('hh:nn:ss:zzz', pvData.FTime)
              , TLogLevelCaption[pvData.FLogLevel]
              , pvData.FMsg
            ]
            );
      end;
      writeln(FLogFile, lvMsg);
      flush(FLogFile);
    finally
      CloseFile(FLogFile);
    end;
  end else
  begin
    FOwner.incErrorCounter;
  end;
end;

procedure TLogFileAppender.checkInitialized;
begin
  if FInitialized then exit;
  if not DirectoryExists(FBasePath) then ForceDirectories(FBasePath);
  FInitialized := true;
end;

constructor TLogFileAppender.Create(pvAddThreadINfo: Boolean);
begin
  inherited Create;
  FBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
  FAddThreadINfo := pvAddThreadINfo;
  FAddProcessID := true;
  FProcessIDStr := IntToStr(GetCurrentProcessId);
end;

function TLogFileAppender.openLogFile(pvPre: String = ''): Boolean;
var
  lvFileName:String;
begin 
  lvFileName :=FBasePath + '\' + pvPre + FormatDateTime('yyyymmddhh', Now()) + '.log';
  try
    AssignFile(FLogFile, lvFileName);
    if (FileExists(lvFileName)) then
      append(FLogFile)
    else
      rewrite(FLogFile);

    Result := true;
  except
    Result := false;
  end;
end;

{ TConsoleAppender }

procedure TConsoleAppender.AppendLog(pvData: TLogDataObject);
begin
  Writeln(
    Format('%s[%s]:%s',
      [FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', pvData.FTime)
        , TLogLevelCaption[pvData.FLogLevel]
        , pvData.FMsg
      ]
      ));
end;

initialization
  __ProcessIDStr := IntToStr(GetCurrentProcessId);
  __GetThreadStackFunc := nil;
  __dataObjectPool := TBaseQueue.Create;
  __dataObjectPool.Name := 'safeLoggerDataPool';
  sfLogger := TSafeLogger.Create();
  sfLogger.setAppender(TLogFileAppender.Create(True));


finalization
  __dataObjectPool.FreeDataObject;
  __dataObjectPool.Free;
  sfLogger.Free;

end.
