unit utils_async;

interface

uses
  Classes, SyncObjs
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}

  {$ENDIF}
  {$IFDEF POSIX}
  , Posix.Base, Posix.Unistd, Posix.Signal, Posix.Pthread,
  Posix.SysTypes
  {$ENDIF}
  ;

{$IF defined(FPC) or (RTLVersion>=18))}
  {$DEFINE HAVE_INLINE}
{$IFEND HAVE_INLINE}

{$if CompilerVersion< 18}
  {$DEFINE LOW_VER}
{$ifend}

type
  TASyncWorker = class;
  TOnASyncEvent = procedure(pvASyncWorker: TASyncWorker) of object;
  TOnASyncGlobalEvent = procedure(pvASyncWorker: TASyncWorker);
  TASyncWorker = class(TThread)
  private
    FData: Pointer;
    FDataObj: TObject;
    FDataTag: Integer;
    FOnAsyncEvent: TOnASyncEvent;
    FOnNotifyEvent: TNotifyEvent;
    procedure SetDataObj(const Value: TObject);
  public
    constructor Create(AOnAsyncEvent: TOnASyncEvent);
    procedure Execute; override;
    property Data: Pointer read FData write FData;
    property DataObj: TObject read FDataObj write SetDataObj;
    property DataTag: Integer read FDataTag write FDataTag;


    property Terminated;     
  end;


  TCriticalSection = class(SyncObjs.TCriticalSection)
  public
    {$IFDEF LOW_VER}
    function TryEnter: Boolean;
    {$ENDIF}

  end;


  TASyncInvoker = class(TObject)
  private
    FOnAsyncEvent: TOnASyncEvent;
    FTerminated: Boolean;
    FStopEvent:TEvent;
    FWaitEvent: TEvent;
    FWorker:TASyncWorker;
    procedure InnerASync(pvWorker:TASyncWorker);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WaitForSleep(pvTime:Cardinal);

    procedure Start(pvASyncEvent: TOnASyncEvent; pvData: Pointer = nil;
        pvDataObject: TObject = nil);overload;

    procedure Start(pvASyncEvent: TOnASyncGlobalEvent; pvData: Pointer = nil;
        pvDataObject: TObject = nil); overload;
    procedure Terminate;
    procedure WaitForStop;

    property Terminated: Boolean read FTerminated write FTerminated;
  end;

function ASyncInvoke(pvASyncProc: TOnASyncEvent; pvData: Pointer = nil;
    pvDataObject: TObject = nil; pvDataTag: Integer = 0): TASyncWorker;

procedure ASyncExecute(const pvCallBack: TNotifyEvent; const pvSender: TObject);

function CreateManualEvent(pvInitState: Boolean = false): TEvent;

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;

function GetTickCount: Cardinal;

function WaitForExpect(var v: Integer; pvExcept: Integer = 0; pvTimeOut:
    Integer = 120000): Boolean;

{$IF RTLVersion<24}
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AtomicIncrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AtomicDecrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
{$IFEND <XE5}

{$IFDEF LOW_VER}
//function TryEnterCriticalSection(var lpCriticalSection: TRTLCriticalSection): BOOL; external 'kernel32.dll' name 'TryEnterCriticalSection';
{$ENDIF}

function GetCPUCount: Integer;

implementation



{$IF RTLVersion < 18}
function InterlockedIncrement(var Addend: Integer): Integer; stdcall; external kernel32 name 'InterlockedIncrement';
{$EXTERNALSYM InterlockedIncrement}
function InterlockedDecrement(var Addend: Integer): Integer; stdcall; external kernel32 name 'InterlockedDecrement';
{$EXTERNALSYM InterlockedDecrement}
function InterlockedExchange(var Target: Integer; Value: Integer): Integer; stdcall;external kernel32 name 'InterlockedExchange';
{$EXTERNALSYM InterlockedExchange}
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall;external kernel32 name 'InterlockedCompareExchange';
{$EXTERNALSYM InterlockedCompareExchange}

function InterlockedExchangeAdd(Addend: PLongint; Value: Longint): Longint; overload; external kernel32 name 'InterlockedExchangeAdd';
function InterlockedExchangeAdd(var Addend: Longint; Value: Longint): Longint; overload; external kernel32 name 'InterlockedExchangeAdd';
{$IFEND <D2007}

function ConvertToOnASyncEvent(const AProc: TOnASyncGlobalEvent): TOnASyncEvent;
begin
  TMethod(Result).Data := nil;
  TMethod(Result).Code := @AProc;
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
{$WARN SYMBOL_PLATFORM OFF}
    Result := sysconf(_SC_NPROCESSORS_ONLN);
{$WARN SYMBOL_PLATFORM ON}
{$ELSE}// 不认识的操作系统，CPU数默认为1
    Result := 1;
{$ENDIF !POSIX}
{$ENDIF !MSWINDOWS}
end;

{$IF RTLVersion<24}
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedCompareExchange(Target, Value, Comparand);
{$ELSE}
  Result := TInterlocked.CompareExchange(Target, Value, Comparand);
{$ENDIF}
end;

function AtomicIncrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedIncrement(Target);
{$ELSE}
  Result := TInterlocked.Increment(Target);
{$ENDIF}
end;

function AtomicDecrement(var Target: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedDecrement(Target);
{$ELSE}
  Result := TInterlocked.Decrement(Target);
{$ENDIF}
end;

{$IFEND <XE5}

/// <summary>
///   计算两个TickCount时间差，避免超出49天后，溢出
///      感谢 [佛山]沧海一笑  7041779 提供
///      copy自 qsl代码
/// </summary>
function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;

function ASyncInvoke(pvASyncProc: TOnASyncEvent; pvData: Pointer = nil;
    pvDataObject: TObject = nil; pvDataTag: Integer = 0): TASyncWorker;
begin
  Result := TASyncWorker.Create(pvASyncProc);
  Result.Data := pvData;
  Result.DataObj := pvDataObject;
  Result.DataTag := pvDataTag;
  {$IFDEF UNICODE}
  Result.Start;
  {$ELSE}
  Result.Resume;
  {$ENDIF}

end;

function CreateManualEvent(pvInitState: Boolean = false): TEvent;
begin
  Result := TEvent.Create(nil, True, pvInitState, '');
end;

function GetTickCount: Cardinal;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.GetTickCount;
  {$ELSE}
  Result := TThread.GetTickCount;
  {$ENDIF}
end;

function WaitForExpect(var v: Integer; pvExcept: Integer = 0; pvTimeOut:
    Integer = 120000): Boolean;
var
  t:Cardinal;
begin
  t := GetTickCount;
  while (v <> pvExcept) and ((GetTickCount - t) < Cardinal(pvTimeOut)) do
  begin
    {$IFDEF MSWINDOWS}
    Sleep(10);
    {$ELSE}
    TThread.Sleep(10);
    {$ENDIF}
  end;        
  Result := v = pvExcept;
end;

procedure ASyncExecute(const pvCallBack: TNotifyEvent; const pvSender: TObject);
var
  lvWorker:TASyncWorker;
begin
  lvWorker := TASyncWorker.Create(nil);
  lvWorker.FOnNotifyEvent := pvCallBack;
  lvWorker.DataObj := pvSender;
  {$IFDEF UNICODE}
  lvWorker.Start;
  {$ELSE}
  lvWorker.Resume;
  {$ENDIF}
end;

constructor TASyncWorker.Create(AOnAsyncEvent: TOnASyncEvent);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOnAsyncEvent := AOnAsyncEvent;
end;

procedure TASyncWorker.Execute;
begin
  if Assigned(FOnAsyncEvent) then
  begin
    FOnAsyncEvent(Self);
  end;
  if Assigned(FOnNotifyEvent) then
  begin
    FOnNotifyEvent(FDataObj);
  end;
end;

procedure TASyncWorker.SetDataObj(const Value: TObject);
begin
  FDataObj := Value;
end;

constructor TASyncInvoker.Create;
begin
  inherited Create;
  FStopEvent := TEvent.Create(nil, True, True, '');
  FWaitEvent := TEvent.Create(nil, True, true, '');
  FTerminated := true;
  FStopEvent.SetEvent;
end;

destructor TASyncInvoker.Destroy;
begin
  FStopEvent.Free;
  FWaitEvent.Free;
  inherited;
end;

procedure TASyncInvoker.InnerASync(pvWorker:TASyncWorker);
begin
  FStopEvent.ResetEvent;
  FOnAsyncEvent(pvWorker);
  FStopEvent.SetEvent;
  FWorker := nil;
  FTerminated := True;
end;

procedure TASyncInvoker.Start(pvASyncEvent: TOnASyncEvent; pvData: Pointer =
    nil; pvDataObject: TObject = nil);
begin
  FTerminated := False;
  FOnAsyncEvent := pvASyncEvent;
  FWorker := ASyncInvoke(InnerASync, pvData, pvDataObject);
end;

procedure TASyncInvoker.Start(pvASyncEvent: TOnASyncGlobalEvent; pvData:
    Pointer = nil; pvDataObject: TObject = nil);
begin
  Start(ConvertToOnASyncEvent(pvASyncEvent), pvData, pvDataObject);
end;

procedure TASyncInvoker.Terminate;
begin
  if FWorker <> nil then FWorker.Terminate;
  FTerminated := True;
  FWaitEvent.SetEvent;
end;

procedure TASyncInvoker.WaitForSleep(pvTime:Cardinal);
begin
  FWaitEvent.ResetEvent;
  FWaitEvent.WaitFor(pvTime);
end;

procedure TASyncInvoker.WaitForStop;
begin
  FStopEvent.WaitFor(MaxInt);
end;

{$IFDEF LOW_VER}
function TCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FSection);
end;
{$ENDIF}

end.
