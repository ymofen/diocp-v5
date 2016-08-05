(*
 *	 Unit owner: D10.Mofen
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *)

unit utils_safeLogger;

interface

uses
  Classes, utils_queues, SysUtils, SyncObjs
  {$IFDEF MSWINDOWS}
  , Windows, Messages
  {$ELSE}
  , System.Diagnostics, IOUtils
  {$ENDIF}

  {$IFDEF ANDROID}
  , Androidapi.Log
  {$ENDIF}
  , utils_threadinfo;


type

  TLogLevel=(lgvError, lgvWarning, lgvHint, lgvMessage, lgvDebug, lgvWriteFile);

  TLogLevels = set of TLogLevel;

const
  TLogLevelCaption: array [TLogLevel] of string = ('error', 'warning', 'hint', 'message', 'debug', '');

  LogAllLevels = [lgvError, lgvWarning, lgvHint, lgvMessage, lgvDebug, lgvWriteFile];

type
  TSafeLogger = class;
  TLogDataObject = class;
  TSyncMainThreadType = (rtSync{$IFDEF MSWINDOWS}, rtPostMessage {$ENDIF});

  TThreadStackFunc = function(AThread:TThread):string;

  TLogProc = procedure(pvLogData:TLogDataObject) of object;

  TLogDataObject = class(TObject)
  public
    FThreadID:Cardinal;
    FTime:TDateTime;
    FLogLevel:TLogLevel;
    FMsg:string;
    FMsgType:string;
    FLogProc:TLogProc;
  end;

  TBaseAppender = class(TObject)
  protected
    FOwner:TSafeLogger;
  protected
    procedure AppendLog(pvData:TLogDataObject); virtual; abstract;

    /// <summary>
    ///   完成一次工作
    /// </summary>
    procedure NotifyOnceEnd(pvCounter:Integer); virtual;
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

    // 0: dead, 1:alive
    FWorkerAlive:Integer;
    
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
    function WorkersIsAlive(const pvWorker: TLogWorker): Boolean;

    procedure CheckForWorker;
    procedure StopWorker(pvTimeOut: Cardinal);
  private

    FLogFilter: TLogLevels;
    FStateLocker:TCriticalSection;
    FWorking:Boolean;
    FName: String;
    {$IFDEF MSWINDOWS}
    FMessageHandle: HWND;
    procedure DoMainThreadWork(var AMsg: TMessage);
    {$ENDIF}
    procedure SetWorking(pvWorking:Boolean);
    function isWorking():Boolean;
    procedure DoWork();

    procedure IncWorkerCount;
    procedure DecWorker(pvWorker: TLogWorker);
    procedure ClearLogData;
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

  // 未处理的数量
  __logCounter : Integer;

  __ProcessIDStr :String;
  __GetThreadStackFunc: TThreadStackFunc;

procedure SafeWriteFileMsg(pvMsg:String; pvFilePre:string);

{$if CompilerVersion < 18} //before delphi 2007
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall; external kernel32 name 'InterlockedCompareExchange';
{$EXTERNALSYM InterlockedCompareExchange}
{$ifend}

implementation

resourcestring
  STRING_ERR_POSTLOGERR = '投递日志信息[%s]时出现了异常:%s';
  STRING_ERR_LOGERR = '记录日志信息时出现了异常:%s';

var
  __dataObjectPool:TBaseQueue;
  {$IFDEF MSWINDOWS}
  {$ELSE}
  {$ENDIF}

{$IFDEF MSWINDOWS}
const
  WM_SYNC_METHOD = WM_USER + 1;
  WM_NOTIFY_WORK = WM_USER + 2;
{$ENDIF}

{$IF RTLVersion<24}
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedCompareExchange(Target, Value, Comparand);
{$ELSE}
  Result := TInterlocked.CompareExchange(Target, Value, Comparand);
{$ENDIF}
end;

function AtomicInc(var Target: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedIncrement(Target);
{$ELSE}
  Result := TInterlocked.Increment(Target);
{$ENDIF}
end;

{$IFEND <XE5}

{$IFDEF MSWINDOWS}
{$ELSE}
function GetCurrentProcessId():Integer;
begin
  Result := 0;
end;
{$ENDIF}


function QueryTickCount: Integer;
begin
{$IFDEF MSWINDOWS}
  Result := GetTickCount;
{$ELSE}
  Result := TThread.GetTickCount;;
{$ENDIF}
end;

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

procedure TSafeLogger.CheckForWorker;
begin
  SetCurrentThreadInfo(Name +  ':logMessage CheckForWorker - 1.0');
  if AtomicCmpExchange(FWorkerAlive, 1, 0) = 0 then
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
  SetCurrentThreadInfo(Name +  ':logMessage CheckForWorker - 2.0');
end;

constructor TSafeLogger.Create;
begin
  inherited Create;
  FLogFilter := LogAllLevels;
  FWorkerAlive := 0;
  FEnable := true;
  FSyncMainThreadType := rtSync;
{$IFDEF MSWINDOWS}
  FSyncMainThreadType := rtPostMessage;
  FMessageHandle := AllocateHWnd(DoMainThreadWork);
{$ENDIF}
  FWorking := False;
  FStateLocker := TCriticalSection.Create;

  FDataQueue := TBaseQueue.Create();
  FAppender := nil;
  FOwnsAppender := false;
  FWorkerCounter := 0;
end;

destructor TSafeLogger.Destroy;
begin
  FEnable := false;
  
  StopWorker(30000);

  ClearLogData;

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

procedure TSafeLogger.ClearLogData;
var
  lvPData:TLogDataObject;
begin
  while True do
  begin
    lvPData :=TLogDataObject(FDataQueue.DeQueueObject);
    if lvPData = nil then Break;
    InterlockedDecrement(__logCounter);

    __dataObjectPool.EnQueueObject(lvPData, raObjectFree);
  end;
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
{$ENDIF}

procedure TSafeLogger.DoWork;
var
  lvPData:TLogDataObject;
begin
  while self.FEnable do
  begin
    lvPData :=TLogDataObject(FDataQueue.DeQueueObject);
    if lvPData = nil then Break;
    InterlockedDecrement(__logCounter);

    try
      FDebugData := lvPData;
      ExecuteLogData(lvPData);
    except
      IncErrorCounter;
    end;
    __dataObjectPool.EnQueueObject(lvPData, raObjectFree);
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
  {$IFDEF MSWINDOWS}
  InterlockedIncrement(FErrorCounter);
  {$ELSE}
  TInterlocked.Increment(FErrorCounter);
  {$ENDIF}
end;

procedure TSafeLogger.IncWorkerCount;
begin
  {$IFDEF MSWINDOWS}
  InterlockedIncrement(FWorkerCounter);
  {$ELSE}
  TInterlocked.Increment(FWorkerCounter);
  {$ENDIF}
end;



procedure TSafeLogger.DecWorker(pvWorker: TLogWorker);
begin
  {$IFDEF MSWINDOWS}
  InterlockedDecrement(FWorkerCounter);
  {$ELSE}
  TInterlocked.Decrement(FWorkerCounter);
  {$ENDIF}

  FWorkerAlive := 0;
  FLogWorker := nil;
end;

function TSafeLogger.getStateINfo: String;
var
  lvDebugINfo:TStrings;
begin
  lvDebugINfo := TStringList.Create;
  try
    lvDebugINfo.Add(Format('enable:%s, workerAlive:%s', [boolToStr(FEnable, True), boolToStr(FWorkerAlive = 1, True)]));
    lvDebugINfo.Add(Format('post/response/error counter:%d / %d / %d',
       [self.FPostCounter,self.FResponseCounter,self.FErrorCounter]));
    Result := lvDebugINfo.Text;
  finally
    lvDebugINfo.Free;
  end;
end;

procedure TSafeLogger.IncResponseCounter;
begin

  {$IFDEF MSWINDOWS}
  InterlockedIncrement(FResponseCounter);
  {$ELSE}
  TInterlocked.Increment(FResponseCounter);

  {$ENDIF}

end;

{ TSafeLogger }

procedure TSafeLogger.logMessage(pvMsg: string; pvMsgType: string = '';
    pvLevel: TLogLevel = lgvMessage);
var
  lvPData:TLogDataObject;
begin
  SetCurrentThreadInfo(Name +  ':logMessage START');
  try
    if not FEnable then exit;

    if not (pvLevel in FLogFilter) then Exit;

    try
      SetCurrentThreadInfo(Name +  ':logMessage START - 1.0');
      lvPData :=TLogDataObject(__dataObjectPool.DeQueueObject);
      SetCurrentThreadInfo(Name +  ':logMessage START - 1.1');
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
      SetCurrentThreadInfo(Name +  ':logMessage START - 2.0');

      InterlockedIncrement(__logCounter);
      
      // dataQueue只引用对象
      FDataQueue.EnQueueObject(lvPData, raNone);

      {$IFDEF CONSOLE}
      if __logCounter > 50000 then
      begin
        Writeln('警告::日志处理器过慢,堆积严重');
      end;
      {$ENDIF}

      SetCurrentThreadInfo(Name +  ':logMessage START - 3.0');
    {$IFDEF MSWINDOWS}
      InterlockedIncrement(FPostCounter);
    {$ELSE}
      TInterlocked.Increment(FPostCounter);

    {$ENDIF}
    {$IFDEF MSWINDOWS}
      if (FAppendInMainThread) and (FSyncMainThreadType = rtPostMessage) then
      begin
        if not isWorking then
        begin
          PostMessage(FMessageHandle, WM_NOTIFY_WORK, 0, 0);
        end;
      end else
      begin
        CheckForWorker;
      end;
    {$ELSE}
      CheckForWorker;
    {$ENDIF};
    except
      on E:Exception do
      begin
        SafeWriteFileMsg(Format(STRING_ERR_POSTLOGERR, [pvMsg, e.Message]), 'sfLogger_err_');
      end;
    end;
  finally
    SetCurrentThreadInfo(Name +  ':logMessage finally');
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

procedure TSafeLogger.StopWorker(pvTimeOut: Cardinal);
var
  l:Cardinal;
  lvWrite:Boolean;
begin
  if FLogWorker <> nil then
  begin
    FLogWorker.Terminate;
    FLogWorker.FNotify.SetEvent;

    lvWrite := True;
    l := QueryTickCount;
    while (FWorkerCounter > 0) and WorkersIsAlive(FLogWorker) do
    begin
      {$IFDEF MSWINDOWS}
      SwitchToThread;
      {$ELSE}
      TThread.Yield;
      {$ENDIF}

      if lvWrite then
      begin
        if tick_diff(l, QueryTickCount) > 10000 then
        begin
          writeSafeInfo(FName + 'is dead, debugInfo:'  + FDebugInfo);
          lvWrite := false;
        end;
      end;
    end;


    FLogWorker := nil;
  end;
end;

function TSafeLogger.WorkersIsAlive(const pvWorker: TLogWorker): Boolean;
var
  lvCode:Cardinal;
begin
  Result := false;
  {$IFDEF MSWINDOWS}
  if (pvWorker <> nil) and (GetExitCodeThread(pvWorker.Handle, lvCode)) then
  begin
    if lvCode=STILL_ACTIVE then
    begin
      Result := true;
    end;
  end;
  {$ELSE}
  {$ENDIF}
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
  i:Integer;
begin
  FSafeLogger.incWorkerCount;
  try
    while not self.Terminated do
    begin
      FSafeLogger.FDebugInfo := 'Thread.Execute::FNotify.WaitFor()';
      SetCurrentThreadInfo(FSafeLogger.Name +  ':Safelogger.Thread.Execute::FNotify.WaitFor()');
      lvWaitResult := FNotify.WaitFor(1000 * 30);
      i := 0;
      if (lvWaitResult=wrSignaled) then
      begin
        try
          try
            FSafeLogger.FDebugInfo := 'Thread.Execute::FNotify.WaitFor(), succ';
            SetCurrentThreadInfo(FSafeLogger.Name + '::Safelogger.Execute::FNotify.WaitFor(), succ');
            while not self.Terminated do
            begin
              lvPData :=TLogDataObject(FSafeLogger.FDataQueue.DeQueueObject);
              if lvPData <> nil then
              begin
                try
                  InterlockedDecrement(__logCounter);
                  FSafeLogger.FDebugData := lvPData;
                  SetCurrentThreadInfo(FSafeLogger.Name + '::Safelogger.Execute::LogDataStart');
                  ExecuteLogData(lvPData);
                  SetCurrentThreadInfo(FSafeLogger.Name + '::Safelogger.Execute::LogDataEnd');
                  inc(i);
                except
                  on E:Exception do
                  begin
                    SafeWriteFileMsg(Format(STRING_ERR_LOGERR, [e.Message]), 'sfLogger_err_');
                    FSafeLogger.IncErrorCounter;
                  end;
                end;
                /// push back to logData pool
                __dataObjectPool.EnQueueObject(lvPData, raObjectFree);
              end else
              begin
                break;
              end;
            end;
          finally
            FSafeLogger.Appender.NotifyOnceEnd(i);
          end;
        except
          on E:Exception do
          begin
              SafeWriteFileMsg(Format(STRING_ERR_LOGERR, [e.Message]), 'sfLogger_err_');
              FSafeLogger.IncErrorCounter;
          end;
        end;
      end else if lvWaitResult = wrTimeout then
      begin
        Sleep(100);
      end;
    end;
  finally
    SetCurrentThreadInfo(FSafeLogger.Name + '::Safelogger.Execute::End finally');
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
        if not FAddProcessID then
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

  FAddThreadINfo := pvAddThreadINfo;
  FAddProcessID := true;
  {$IFDEF MSWINDOWS}
  FProcessIDStr := IntToStr(GetCurrentProcessId);
  FBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
  {$ELSE}
  FProcessIDStr := '0';
  FBasePath := TPath.GetSharedDocumentsPath + TPath.DirectorySeparatorChar;
  {$ENDIF}


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

{ TBaseAppender }

procedure TBaseAppender.NotifyOnceEnd(pvCounter: Integer);
begin
  
end;

initialization
  __logCounter := 0;
  __ProcessIDStr := IntToStr(GetCurrentProcessId);
  __GetThreadStackFunc := nil;
  __dataObjectPool := TBaseQueue.Create;
  __dataObjectPool.Name := 'safeLogger.LogDataPool';
  sfLogger := TSafeLogger.Create();
  sfLogger.Name := 'defaultLogger';
  sfLogger.setAppender(TLogFileAppender.Create(True));
  {$IFDEF MSWINDOWS}
  {$ELSE}
  {$ENDIF}

finalization
  sfLogger.Free;

  //
  __dataObjectPool.Free;


  {$IFDEF MSWINDOWS}
  {$ELSE}

  {$ENDIF}


end.
