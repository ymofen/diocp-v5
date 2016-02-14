unit utils_async;

interface

uses
  Classes, SyncObjs;

type
  TASyncWorker = class;
  TOnASyncEvent = procedure(pvASyncWorker: TASyncWorker) of object;
  TASyncWorker = class(TThread)
  private
    FOnAsyncEvent: TOnASyncEvent;
  public
    constructor Create(AOnAsyncEvent: TOnASyncEvent);
    procedure Execute; override;
  end;

  TASyncInvoker = class(TObject)
  private
    FOnAsyncEvent: TOnASyncEvent;
    FTerminated: Boolean;
    FStopEvent:TEvent;
    procedure InnerASync(pvWorker:TASyncWorker);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(pvASyncEvent: TOnASyncEvent);
    procedure Terminate;
    procedure WaitForStop;

    property Terminated: Boolean read FTerminated write FTerminated;
  end;

procedure ASyncInvoke(pvASyncProc:TOnASyncEvent);

function CreateManualEvent(pvInitState: Boolean = false): TEvent;

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;

implementation

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

procedure ASyncInvoke(pvASyncProc:TOnASyncEvent);
begin
  TASyncWorker.Create(pvASyncProc);
end;

function CreateManualEvent(pvInitState: Boolean = false): TEvent;
begin
  Result := TEvent.Create(nil, True, pvInitState, '');
end;

constructor TASyncWorker.Create(AOnAsyncEvent: TOnASyncEvent);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FOnAsyncEvent := AOnAsyncEvent;
end;

procedure TASyncWorker.Execute;
begin
  if Assigned(FOnAsyncEvent) then
  begin
    FOnAsyncEvent(Self);
  end;
end;

constructor TASyncInvoker.Create;
begin
  inherited Create;
  FStopEvent := TEvent.Create(nil, True, True, '');
end;

destructor TASyncInvoker.Destroy;
begin
  FStopEvent.Free;
  inherited;
end;

procedure TASyncInvoker.InnerASync(pvWorker:TASyncWorker);
begin
  FOnAsyncEvent(pvWorker);
  FStopEvent.SetEvent;
end;

procedure TASyncInvoker.Start(pvASyncEvent: TOnASyncEvent);
begin
  FTerminated := False;
  FStopEvent.ResetEvent;
  FOnAsyncEvent := pvASyncEvent;
  ASyncInvoke(InnerASync);
end;

procedure TASyncInvoker.Terminate;
begin
  FTerminated := True;
end;

procedure TASyncInvoker.WaitForStop;
begin
  FStopEvent.WaitFor(MaxInt);
end;

end.
