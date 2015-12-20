unit utils_ThreadTimer;

interface

uses
  Classes, SyncObjs, SysUtils;

type   
  TThreadTimer = class(TObject)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FOnTimer: TNotifyEvent;
    FRunInMainThread: Boolean;
    FWaitEvent: TEvent;
    FTheadWorker:TThread;
    
    procedure DoTimer;
    procedure DoInterval();
    procedure SetEnabled(const Value: Boolean);
    procedure Start;
    procedure Stop;
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Integer read FInterval write FInterval;
    property RunInMainThread: Boolean read FRunInMainThread write FRunInMainThread;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;    
  end;

implementation

type
  TThreadWorker = class(TThread)
  private
    FOwner: TThreadTimer;
    procedure DoMainThread();
  public
    constructor Create(AOwner: TThreadTimer);
    procedure Execute; override;
  end;

constructor TThreadWorker.Create(AOwner: TThreadTimer);
begin
  inherited Create(True);
  FOwner := AOwner;
end;

procedure TThreadWorker.DoMainThread;
begin
  FOwner.DoTimer;
end;

procedure TThreadWorker.Execute;
begin
  while not Self.Terminated do
  begin
    FOwner.DoInterval;
    if not Terminated then
    begin
      if FOwner.RunInMainThread then
      begin
        Synchronize(DoMainThread);
      end else
      begin
        FOwner.DoTimer;
      end;
    end;
  end;
end;

constructor TThreadTimer.Create;
begin
  inherited Create;
  FWaitEvent := TEvent.Create(nil,false,false,'');
end;

destructor TThreadTimer.Destroy;
begin
  FreeAndNil(FWaitEvent);
  inherited Destroy;
end;



procedure TThreadTimer.DoInterval;
begin
  FWaitEvent.WaitFor(FInterval);
end;

procedure TThreadTimer.DoTimer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

procedure TThreadTimer.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if Value then
    begin
      Start;
    end else
    begin
      Stop;
    end;
    FEnabled := Value;
  end;
end;

procedure TThreadTimer.Start;
begin
  if FTheadWorker = nil then
  begin
    FTheadWorker := TThreadWorker.Create(Self);
    FWaitEvent.ResetEvent;
    FTheadWorker.Resume;
  end;
end;

procedure TThreadTimer.Stop;
begin
  if FTheadWorker <> nil then
  begin
    FTheadWorker.Terminate;

    // ½áÊøµÈ´ý
    FWaitEvent.SetEvent;
    FTheadWorker.WaitFor();
    FTheadWorker.Free;
    FTheadWorker := nil;
  end;
end;

end.
