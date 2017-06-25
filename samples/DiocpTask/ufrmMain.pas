unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, diocp_task, StdCtrls, utils_async,
  utils_queues, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    btnPostTask: TButton;
    Memo1: TMemo;
    SpeedTester: TButton;
    btnState: TButton;
    btnSignal: TButton;
    btnRegister: TButton;
    btnUnRegister: TButton;
    tmrSpeed: TTimer;
    lblSpeed: TLabel;
    btnQueueSpeed: TButton;
    procedure btnPostTaskClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure btnSignalClick(Sender: TObject);
    procedure btnStateClick(Sender: TObject);
    procedure btnUnRegisterClick(Sender: TObject);
    procedure btnQueueSpeedClick(Sender: TObject);
    procedure SpeedTesterClick(Sender: TObject);
    procedure tmrSpeedTimer(Sender: TObject);
  private
    FLogTask: TIocpTaskMananger;

    FStartTick:Cardinal;
    FEndTick:Cardinal;
    FMaxCounter:Integer;
    FSpeedCounter:Integer;
    { Private declarations }
    procedure onLogMsg(const pvStrData: string);

    procedure logMessage(pvMsg: string);
    procedure DoPostSpeedTask(ASync:TASyncWorker);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnTaskWork(); overload;
    procedure OnTaskWork(pvStrData: string); overload;

    procedure OnSignalWork(pvTaskRequest: TIocpTaskRequest);

    procedure DoJobProc(pvTaskRequest: TIocpTaskRequest);
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
  // pvTaskRequest.Remark := '测试等待 10秒....';
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
  iocpTaskManager.Enable := false;
  //
  // FLogTask.PostATask(onLogMsg, 'abcd', True, rtPostMessage);
  // Sleep(100);
  FLogTask.Active := false;
  FLogTask.Free;
  inherited Destroy;
end;

procedure TfrmMain.DoJobProc(pvTaskRequest: TIocpTaskRequest);
begin
  if InterlockedDecrement(PInteger(pvTaskRequest.TaskData)^) = 0 then
  begin
    FEndTick := GetTickCount;
  end;
end;

procedure TfrmMain.logMessage(pvMsg: string);
begin
  FLogTask.PostATask(onLogMsg, pvMsg, true, rtPostMessage);
end;

procedure TfrmMain.onLogMsg(const pvStrData: string);
begin
  Memo1.Lines.Add(pvStrData);
end;

procedure TfrmMain.btnPostTaskClick(Sender: TObject);
begin
  iocpTaskManager.PostATask(TaskProcGlobal, nil, true);
end;

procedure TfrmMain.btnRegisterClick(Sender: TObject);
begin
  iocpTaskManager.registerSignal(1, self.OnSignalWork);
end;

procedure TfrmMain.btnSignalClick(Sender: TObject);
begin
  iocpTaskManager.SignalATask(1, TSimpleDataObject.Create('signal param'),
    ftFreeAsObject);
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

procedure TfrmMain.btnQueueSpeedClick(Sender: TObject);
const
  ACount: Integer = 10000000;
var
  i:Integer;
  lvTick:Cardinal;
  lvRequest:TIocpTaskRequest;
  lvQueue:TSafeQueue;
begin
  lvQueue := TSafeQueue.Create;
  lvRequest := TIocpTaskRequest.Create;
  lvQueue.EnQueue(lvRequest);

  lvTick := GetTickCount;
  for i := 0 to ACount - 1 do
  begin
    lvRequest := TIocpTaskRequest(lvQueue.DeQueue);
    try


    finally
      lvQueue.EnQueue(lvRequest);
    end;
  end;

  lvTick := GetTickCount - lvTick;
  ShowMessage(Format('投递处理次数：%d，时间：%1.3f秒，速率：%1.0f次/秒',
            [ACount, lvTick / 1000.000,
                      (ACount) / (lvTick) * 1000.000]));

end;

procedure TfrmMain.DoPostSpeedTask(ASync:TASyncWorker);
var
  i:Integer;
begin
  for I := 0 to FMaxCounter - 1 do
  begin
    iocpTaskManager.PostATask(DoJobProc, @FSpeedCounter);
  end;
end;

procedure TfrmMain.OnSignalWork(pvTaskRequest: TIocpTaskRequest);
var
  lvData: TSimpleDataObject;
begin
  lvData := TSimpleDataObject(pvTaskRequest.TaskData);
  if GetCurrentThreadId = MainThreadID then
  begin
    Memo1.Lines.Add('exeucte signal task in main thead:' + lvData.DataString1);
  end
  else
  begin
    logMessage('exeucte signal task in thread:' + lvData.DataString1);
  end;
end;

{ TfrmMain }

procedure TfrmMain.OnTaskWork(pvStrData: string);
begin
  Memo1.Lines.Add(pvStrData + ': currentIsMainThread:' +
    BoolToStr(GetCurrentThreadId = MainThreadID, true));
end;

procedure TfrmMain.OnTaskWork;
var
  lvMsg: String;
begin
  lvMsg := 'currentIsMainThread:' +
    BoolToStr(GetCurrentThreadId = MainThreadID, true);
  logMessage(lvMsg);
end;

procedure TfrmMain.SpeedTesterClick(Sender: TObject);
const
  ACount: Integer = 10000000;
var
  I, ARuns: Integer;
  T1: Int64;
  ANeedRuns: Int64;
begin
  FMaxCounter := ACount;
  FSpeedCounter := ACount;

  FEndTick := 0;
  FStartTick := GetTickCount;
  tmrSpeed.Enabled := true;

  ASyncInvoke(DoPostSpeedTask);



//  while (ARuns < ANeedRuns) do
//{$IFDEF UNICODE}
//    TThread.Yield;
//{$ELSE}
//    SwitchToThread;
//{$ENDIF}
//  T1 := GetTickCount - T1;

end;

procedure TfrmMain.tmrSpeedTimer(Sender: TObject);
var
  lvEndTick:Cardinal;
begin
  if FEndTick <> 0 then
  begin
    lvEndTick := FEndTick;
    tmrSpeed.Enabled := false;
  end else
  begin
    lvEndTick := GetTickCount;
  end;

  lvEndTick := lvEndTick - FStartTick;

  lblSpeed.Caption := Format('投递处理次数：%d，时间：%1.3f秒，速率：%1.0f次/秒',
            [FMaxCounter - FSpeedCounter, lvEndTick / 1000.000,
                      (FMaxCounter - FSpeedCounter) / (lvEndTick) * 1000.000]);
end;

initialization

end.
