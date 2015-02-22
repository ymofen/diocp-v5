unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, diocp.task, StdCtrls;

type
  TfrmMain = class(TForm)
    btnPostTask: TButton;
    Memo1: TMemo;
    SpeedTester: TButton;
    btnState: TButton;
    btnSignal: TButton;
    btnRegister: TButton;
    btnUnRegister: TButton;
    procedure btnPostTaskClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure btnSignalClick(Sender: TObject);
    procedure btnStateClick(Sender: TObject);
    procedure btnUnRegisterClick(Sender: TObject);
    procedure SpeedTesterClick(Sender: TObject);
  private
    FLogTask: TIocpTaskMananger;
    { Private declarations }
    procedure onLogMsg(pvStrData:string);

    procedure logMessage(pvMsg:string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
    procedure OnTaskWork();overload;
    procedure OnTaskWork(pvStrData:string);overload;

    procedure OnSignalWork(pvTaskRequest:TIocpTaskRequest);

    procedure DoJobProc(pvTaskRequest:TIocpTaskRequest);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{$IFDEF MSWINDOWS}
type
  TGetTickCount64 = function: Int64;
{$ENDIF MSWINDOWS}

var
{$IFDEF NEXTGEN}
  _Watch: TStopWatch;
{$ELSE}
  GetTickCount64: TGetTickCount64;
  _PerfFreq: Int64;
{$ENDIF}

procedure TaskProcGlobal(pvTaskRequest: TIocpTaskRequest);
begin
  // pvTaskRequest.Remark := '≤‚ ‘µ»¥˝ 10√Î....';
  // Sleep(1000 * 10);
  ShowMessage('TaskProcGlobal invoke');
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogTask := TIocpTaskMananger.Create();
  FLogTask.IocpEngine.setWorkerCount(1);
  FLogTask.Active := true;
end;

destructor TfrmMain.Destroy;
begin
  FLogTask.Enable := false;
  iocpTaskManager.Enable := False;
//
//  FLogTask.PostATask(onLogMsg, 'abcd', True, rtPostMessage);
//  Sleep(100);
  FLogTask.Active := false;
  FLogTask.Free;
  inherited Destroy;
end;

procedure TfrmMain.DoJobProc(pvTaskRequest: TIocpTaskRequest);
begin
  InterlockedIncrement(PInteger(pvTaskRequest.TaskData)^);
end;

procedure TfrmMain.logMessage(pvMsg: string);
begin
  FLogTask.PostATask(onLogMsg, pvMsg, True, rtPostMessage);
end;

procedure TfrmMain.onLogMsg(pvStrData: string);
begin
  Memo1.Lines.Add(pvStrData);
end;

procedure TfrmMain.btnPostTaskClick(Sender: TObject);
begin
  iocpTaskManager.PostATask(TaskProcGlobal, nil, True);
end;

procedure TfrmMain.btnRegisterClick(Sender: TObject);
begin
  iocpTaskManager.registerSignal(1, self.OnSignalWork);
end;

procedure TfrmMain.btnSignalClick(Sender: TObject);
begin
  iocpTaskManager.SignalATask(1, TSimpleDataObject.Create('signal param'), ftFreeAsObject);
end;

procedure TfrmMain.btnStateClick(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Lines.Add(iocpTaskManager.getStateINfo());
end;

procedure TfrmMain.btnUnRegisterClick(Sender: TObject);
begin
  iocpTaskManager.UnregisterSignal(1);
end;

procedure TfrmMain.OnSignalWork(pvTaskRequest:TIocpTaskRequest);
var
  lvData:TSimpleDataObject;
begin
  lvData:= TSimpleDataObject(pvTaskRequest.TaskData);
  if GetCurrentThreadId = MainThreadID then
  begin
    Memo1.Lines.Add('exeucte signal task in main thead:' + lvData.DataString1);
  end else
  begin
    logMessage('exeucte signal task in thread:' + lvData.DataString1);
  end;
end;

{ TfrmMain }

procedure TfrmMain.OnTaskWork(pvStrData:string);
begin
  Memo1.Lines.Add(pvStrData +': currentIsMainThread:' + BoolToStr(GetCurrentThreadId = MainThreadID, True));
end;

procedure TfrmMain.OnTaskWork;
var
  lvMsg:String;
begin
  lvMsg := 'currentIsMainThread:' + BoolToStr(GetCurrentThreadId = MainThreadID, True);
  logMessage(lvMsg);
end;



procedure TfrmMain.SpeedTesterClick(Sender: TObject);
const
  ACount:Integer=10000000;
var
  I,ARuns:Integer;
  T1:Int64;
  ANeedRuns:Int64;
begin
ARuns:=0;
ANeedRuns:=ACount;
T1:=GetTickCount;
for I := 0 to ACount-1 do
  begin
  iocpTaskManager.PostATask(DoJobProc,@ARuns);
  end;
while (ARuns<ANeedRuns) do
  {$IFDEF UNICODE}
  TThread.Yield;
  {$ELSE}
  SwitchToThread;
  {$ENDIF}
T1:=GetTickCount-T1;
ShowMessage('Time Used='+IntToStr(T1)+'ms,Runs='+IntToStr(ARuns)+
  ',Speed='+IntToStr(Int64(ARuns)*1000 div T1) + '/s'); 
end;

initialization



end.
