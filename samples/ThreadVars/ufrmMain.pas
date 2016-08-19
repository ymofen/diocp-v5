unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, utils_BufferPool, StdCtrls, utils_async, utils_byteTools,
  utils_strings, utils_queues, utils_safeLogger, ExtCtrls, utils_threadvars,
  utils_dvalue;

type
  PData = ^TData;
  TData = record
    obj:TObject;
    ptr:Pointer;
  end;
  TForm1 = class(TForm)
    btnNewPool: TButton;
    btnFreePool: TButton;
    btnSimpleTester: TButton;
    btnThreadDefaultVarsTester: TButton;
    btnThreadTester2: TButton;
    mmoLog: TMemo;
    btnPoolInfo: TButton;
    btnClear: TButton;
    btnSpeedTester: TButton;
    edtThread: TEdit;
    pnlTop: TPanel;
    btnDefaultVarsTester: TButton;
    Label1: TLabel;
    Button1: TButton;
    procedure btnDefaultVarsTesterClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFreePoolClick(Sender: TObject);
    procedure btnNewPoolClick(Sender: TObject);
    procedure btnSpeedTesterClick(Sender: TObject);
    procedure btnSpinLockerClick(Sender: TObject);
    procedure btnThreadDefaultVarsTesterClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FThreadVars: PThreadVars;
    FTesterCount: Integer;
    FTesterTerminate: Boolean;

    procedure DefaultVarsTester(ASyncWorker:TASyncWorker);
    procedure TesterForSpeed(ASyncWorker:TASyncWorker);
    procedure TesterForThreadCallback(ASyncWorker:TASyncWorker);

    procedure Tester2(ASyncWorker:TASyncWorker);
    procedure OnSimpleBufferWrite(pvSender:TObject; pvBuffer:Pointer; pvLength:Integer);
    procedure OnBufferWriteInThread(pvSender:TObject; pvBuffer:Pointer;
        pvLength:Integer);
  public
    destructor Destroy; override;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  utils_threadinfo, ComObj;


{$R *.dfm}

destructor TForm1.Destroy;
begin
  FinalizeForThreadInfo;
  inherited Destroy;
end;

procedure TForm1.btnDefaultVarsTesterClick(Sender: TObject);
var
  i, s:Integer;
begin
  //if FThreadVars = nil then raise Exception.Create('请先初始化FThreadVars');
  if FTesterCount > 0 then
  begin
    FTesterTerminate := true;
    if not WaitForExpect(FTesterCount) then
    begin
      raise Exception.Create('等待结束超时！');
    end;
    TButton(Sender).Caption := '点击开始测试';
  end else
  begin
    FTesterTerminate := False;
    for i := 1 to StrToInt(edtThread.Text) do
    begin
      ASyncInvoke(TesterForThreadCallback);
    end;
    TButton(Sender).Caption := '点击停止';
  end;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  mmoLog.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitalizeForThreadInfo;
  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;
  TStringsAppender(sfLogger.Appender).AddThreadINfo := true;
end;

procedure TForm1.OnSimpleBufferWrite(pvSender: TObject; pvBuffer: Pointer;
  pvLength: Integer);
begin
  AddRef(pvBuffer);
  try
    mmoLog.Lines.Add(ByteBufferToString(pvBuffer, pvLength));
  finally
    ReleaseRef(pvBuffer);
  end;
end;

procedure TForm1.btnFreePoolClick(Sender: TObject);
begin
  DisposeThreadVars(FThreadVars);
end;

procedure TForm1.btnNewPoolClick(Sender: TObject);
begin
  FThreadVars := NewThreadVars;
end;

procedure TForm1.btnSpeedTesterClick(Sender: TObject);
var
  i, s, lvCpuCount:Integer;
  lvWorker:TASyncWorker;
begin
  if FThreadVars = nil then raise Exception.Create('请先初始化FThreadVars');
  lvCpuCount := GetCPUCount;
  for i := 1 to StrToInt(edtThread.Text) do
  begin
    lvWorker := ASyncInvoke(TesterForSpeed);
    SetThreadIdealProcessor(lvWorker.Handle, i mod lvCpuCount);
  end;
end;

procedure TForm1.btnSpinLockerClick(Sender: TObject);
var
  lvTarget:Integer;
begin
  lvTarget := 0;
  if AtomicCmpExchange(lvTarget, 1, 0) = 0 then
  begin
    ShowMessage('OK');
  end;

  if AtomicCmpExchange(lvTarget, 0, 1) = 1 then
  begin
    ShowMessage('OK');
  end;
  ;
end;

procedure TForm1.btnThreadDefaultVarsTesterClick(Sender: TObject);
var
  i:Integer;
begin
  // 多个线程同时获取内存块，并进行读写归还

  for i := 1 to StrToInt(edtThread.Text) do
  begin
    ASyncInvoke(DefaultVarsTester);
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  PtrData:PData;
begin
  New(PtrData);
  if PtrData.obj = nil then
  begin
    mmoLog.Lines.Add('obj');
  end;

  if PtrData.ptr = nil then
  begin
    mmoLog.Lines.Add('Ptr');
  end;

  Dispose(PtrData);

end;

procedure TForm1.OnBufferWriteInThread(pvSender:TObject; pvBuffer:Pointer;
    pvLength:Integer);
var
  lvBuffer:TBytes;
begin
  AddRef(pvBuffer);
  try
    SetLength(lvBuffer, pvLength);
    Move(pvBuffer^, lvBuffer[0], pvLength);
  finally
    ReleaseRef(pvBuffer);
  end;
end;

procedure TForm1.DefaultVarsTester(ASyncWorker:TASyncWorker);
var
  s, s1:WideString;
  i:Integer;
begin
  Sleep(0);
  i := 1;
  while i < 100 do
  begin
    s := CreateClassID;
    GetCurrentThreadDValue.ForceByName('name').AsStringW := s;
    s1 := GetCurrentThreadDValue.ForceByName('name').AsStringW;
    Assert(s = s1);

    Sleep(0);
    // ResetThreadVars();
    // Assert(GetCurrentThreadDValue.ForceByName('name').AsStringW = '');



    Sleep(0);
    inc(i);
  end;
end;

procedure TForm1.TesterForSpeed(ASyncWorker:TASyncWorker);
var
  lvBuff:PByte;
  i:Integer;
  l:Cardinal;
begin
  Sleep(0);
  i := 0;
  l := GetTickCount;
  while i < 10000000 do
  begin
    try

    except
      on e:Exception do
      begin
        sfLogger.logMessage('%d:err:%s', [GetCurrentThreadID, e.Message]);
      end;                                                                
    end;
  end;
  l := GetTickCount - l;
  SetCurrentThreadInfo('Speed end...');
  sfLogger.logMessage('id:%d, t:%d, c:%d, speed:%f / s', [GetCurrentThreadId, l, i, (i * 1000.0000) / l]);
end;

procedure TForm1.Tester2(ASyncWorker:TASyncWorker);
begin
  sfLogger.logMessage(TByteTools.varToHexString(ASyncWorker.Data, 5));
  sleep(0);
  ReleaseRef(ASyncWorker.Data);  
end;

procedure TForm1.TesterForThreadCallback(ASyncWorker:TASyncWorker);
var
  s, s1:DStringW;
  t:Cardinal;
  l:Integer;
begin
  InterlockedIncrement(FTesterCount);
  try
    l := 0;
    t := GetTickCount;
    while not FTesterTerminate do
    begin
      s := CreateClassID;
      GetCurrentThreadDValue.ForceByName('name').AsStringW := s;
      s1 := GetCurrentThreadDValue.ForceByName('name').AsStringW;
      Assert(s = s1);
      inc(l);
    end;
    t := GetTickCount - t;
    sfLogger.logMessage('t:%d, num:%d, speed:%f', [t, l, (l/t) * 1000.00]);
  finally
    InterlockedDecrement(FTesterCount);
  end;
end;

end.
