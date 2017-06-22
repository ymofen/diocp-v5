unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, utils_safeLogger, utils_async, utils_threadinfo,
  ExtCtrls, utils_queues;

type
  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    btnStart: TButton;
    edtThreadNum: TEdit;
    mmoInfo: TMemo;
    btnThreadInfo: TButton;
    edtPerNum: TEdit;
    Label1: TLabel;
    edtSleep: TEdit;
    edtObjCounter: TEdit;
    Button1: TButton;
    btnPushN: TButton;
    btnGetMem: TButton;
    procedure btnGetMemClick(Sender: TObject);
    procedure btnPushNClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnThreadInfoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure OnASyncWorker(pvWorker:TASyncWorker);
  end;

  TEmptyAppender4SafeLogger = class(TBaseAppender)
  public
    procedure AppendLog(pvData:TLogDataObject); override;
  end;

var
  frmMain: TfrmMain;

  __inCounter:Integer;
  __outCounter:Integer;

  __runCounter:Integer;
  __terminated:Boolean;
  __num:Integer;
  __sleep:Integer;

  __objects:array of TSafeQueue;

implementation



{$R *.dfm}

procedure ClearObjects();
var
  i: Integer;
begin
  for i := 0 to Length(__objects) - 1 do
  begin
    __objects[i].Free;
  end;
  SetLength(__objects, 0);
end;

procedure DoOutQueue(pvQueue:TBaseQueue);
var
  lvObj:TObject;
begin
  while True do
  begin
    lvObj :=TObject(pvQueue.DeQueue);
    if lvObj <> nil then
    begin
      InterlockedIncrement(__outCounter);
      lvObj.Free;
    end else
    begin
      Break;
    end;
  end;

end;


procedure TfrmMain.btnGetMemClick(Sender: TObject);
var
  lvQue:TList;
  i: Integer;
  lvBlock:Pointer;
begin
  lvQue := TList.Create;
  for i := 0 to 102400 do
  begin
    GetMem(lvBlock, 4096);
    lvQue.Add(lvBlock);
  end;

  for i := 0 to 102400 do
  begin
    FreeMem(lvQue[i]);
  end;
  lvQue.Clear;
  lvQue.Free;

end;

procedure TfrmMain.btnPushNClick(Sender: TObject);
var
  lvQue:TSimpleQueue;
  i: Integer;
  lvBlock:Pointer;
begin
  lvQue := TSimpleQueue.Create;
  for i := 0 to 102400 do
  begin
    GetMem(lvBlock, 4096);
    lvQue.EnQueue(lvBlock, raFreeMem);
  end;
  lvQue.Clear;
  lvQue.Free;

end;

procedure TfrmMain.btnStartClick(Sender: TObject);
var
  i: Integer;
begin
  if btnStart.Tag = 0 then
  begin
    if __runCounter <> 0 then raise Exception.Create(Format('还有:%d线程在运行, 请先等待现场完成任务', [__runCounter]));

    ResetThreadHintInfo();

    ClearObjects();
    __outCounter := 0;
    __inCounter := 0;


    SetLength(__objects, StrToInt(edtObjCounter.Text));
    for i := 0 to length(__objects) - 1 do
    begin
      __objects[i] := TSafeQueue.Create;
      __objects[i].Name := '__obj' + IntToStr(i);
    end;


    __terminated := False;
    btnStart.Tag := 1;
    btnStart.Caption := '点击停止';
    __sleep := StrToInt(edtSleep.Text);
    __num := StrToInt(edtPerNum.Text);
    for i := 0 to StrToInt(edtThreadNum.Text) -1 do
    begin
      ASyncInvoke(OnASyncWorker, nil, nil, i);
    end;
  end else
  begin
    __terminated := True;
    btnStart.Tag := 0;
    btnStart.Caption := '启动';
  end;
end;

procedure TfrmMain.btnThreadInfoClick(Sender: TObject);
begin
  mmoInfo.Clear;
  mmoInfo.Lines.Add(Format('in:%d, out:%d', [__inCounter, __outCounter]));
  mmoInfo.Lines.Add(GetThreadsHintInfo);

end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  lvQue:TSafeQueue;
  i: Integer;
begin
  lvQue := TSafeQueue.Create;
  for i := 0 to 102400 do
  begin
    lvQue.EnQueue(Self);
  end;
  lvQue.Clear;
  lvQue.Free;
end;

procedure TfrmMain.OnASyncWorker(pvWorker:TASyncWorker);
var
  i, j, c: Integer;
  lvTickCount, t:Cardinal;
  lvObj:TObject;

begin
  InterlockedIncrement(__runCounter);
  lvTickCount := GetTickCount;
  SetCurrentThreadInfo('[%d]开始', [pvWorker.DataTag]);
  for i := 1 to __num do
  begin
    j := i mod Length(__objects);

    t := GetTickCount - lvTickCount;
    if t = 0 then t := 1;
    SetCurrentThreadInfo('[%d]测试, 当前:%d, 耗时:%d, 次数:%d, ops:%f',
      [pvWorker.DataTag, i, t, i, (i) / (t / 1000.000) ]);




    lvObj := TObject.Create;
    __objects[j].EnQueue(lvObj);
    InterlockedIncrement(__inCounter);

    DoOutQueue(__objects[j]);

    inc(c);

    if __sleep >= 0 then Sleep(__sleep);
    if __terminated then Break;
  end;



  t := GetTickCount - lvTickCount;
  if t = 0 then t := 1;
  SetCurrentThreadInfo('[%d]完成, 耗时:%d, 次数:%d, ops:%f',
    [pvWorker.DataTag, t, c, (c) / (t / 1000.000) ]);

  InterlockedDecrement(__runCounter);
end;

procedure TEmptyAppender4SafeLogger.AppendLog(pvData:TLogDataObject);
begin
  inherited;
end;

end.
