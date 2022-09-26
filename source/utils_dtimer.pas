unit utils_dtimer;

interface

uses
  Windows, mmsystem, classes;

type
  TDTimer = class(TObject)
  private
    FTimeHandle: THandle;
    FInterval:Cardinal;
    FOnTimer: TNotifyEvent;
    procedure DoTimer;
  public
    constructor Create;
    procedure Start(pvInterval_msecs: Cardinal);
    procedure Stop();
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;

  end;

implementation

procedure timecallbackProc(utimerid, umessage: uint; dwuser: DWORD_PTR; dw1,
    dw2: dword); stdcall;
begin
  TDTimer(Pointer(dwuser)).DoTimer();
end;

constructor TDTimer.Create;
begin

end;

procedure TDTimer.DoTimer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self); 

end;

procedure TDTimer.Start(pvInterval_msecs: Cardinal);
begin
  FInterval := pvInterval_msecs;
  FTimeHandle := timeSetEvent(Cardinal(FInterval), 0, @timecallbackProc, DWORD_PTR(self), 1);
end;

procedure TDTimer.Stop;
begin
  timeKillEvent(FTimeHandle);
end;

end.
