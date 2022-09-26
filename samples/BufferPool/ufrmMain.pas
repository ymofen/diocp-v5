unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, utils_BufferPool, StdCtrls, utils_async, utils_byteTools,
  utils_strings, utils_queues, utils_safeLogger, ExtCtrls;

type
  TForm1 = class(TForm)
    btnNewPool: TButton;
    btnFreePool: TButton;
    btnSimpleTester: TButton;
    btnThreadTester: TButton;
    btnThreadTester2: TButton;
    mmoLog: TMemo;
    btnPoolInfo: TButton;
    btnClear: TButton;
    btnCheckBounds: TButton;
    btnOutOfBounds: TButton;
    btnSpeedTester: TButton;
    edtThread: TEdit;
    btnSpinLocker: TButton;
    pnlTop: TPanel;
    btnSimpleBlockBuffer: TButton;
    edtBlockSize: TEdit;
    btnBlockBufferTester: TButton;
    Label1: TLabel;
    Label2: TLabel;
    btnAttachObject: TButton;
    btnRefBuffer: TButton;
    procedure btnAttachObjectClick(Sender: TObject);
    procedure btnBlockBufferTesterClick(Sender: TObject);
    procedure btnSimpleBlockBufferClick(Sender: TObject);
    procedure btnCheckBoundsClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFreePoolClick(Sender: TObject);
    procedure btnNewPoolClick(Sender: TObject);
    procedure btnOutOfBoundsClick(Sender: TObject);
    procedure btnPoolInfoClick(Sender: TObject);
    procedure btnRefBufferClick(Sender: TObject);
    procedure btnSimpleTesterClick(Sender: TObject);
    procedure btnSpeedTesterClick(Sender: TObject);
    procedure btnSpinLockerClick(Sender: TObject);
    procedure btnThreadTester2Click(Sender: TObject);
    procedure btnThreadTesterClick(Sender: TObject);
  private
    { Private declarations }
    FBuffer:Pointer;
    FPool: PBufferPool;
    FBlockBufferTesterCount:Integer;
    FBlockBufferTesterTerminate:Boolean;

    procedure Tester(ASyncWorker:TASyncWorker);
    procedure TesterForSpeed(ASyncWorker:TASyncWorker);
    procedure TesterForBlockBuffer(ASyncWorker:TASyncWorker);

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
  utils_threadinfo;


{$R *.dfm}

destructor TForm1.Destroy;
begin
  FinalizeForThreadInfo;
  inherited Destroy;
end;

procedure TForm1.btnAttachObjectClick(Sender: TObject);
var
  lvObj:TStrings;
  lvBuff:PByte;
begin
  if FPool = nil then raise Exception.Create('���ȳ�ʼ��FPool');
  lvObj := TStringList.Create;
  lvObj.Add('ok');
  lvBuff := GetBuffer(FPool);
  AttachData(lvBuff, lvObj, FREE_TYPE_OBJECTFREE);
  AddRef(lvBuff);
  ReleaseRef(lvBuff, False, STRING_EMPTY);
end;

procedure TForm1.btnBlockBufferTesterClick(Sender: TObject);
var
  i, s:Integer;
begin
  if FPool = nil then raise Exception.Create('���ȳ�ʼ��FPool');
  if FBlockBufferTesterCount > 0 then
  begin
    FBlockBufferTesterTerminate := true;
    if not WaitForExpect(FBlockBufferTesterCount) then
    begin
      raise Exception.Create('�ȴ�������ʱ��');
    end;
    btnBlockBufferTester.Caption := '�����ʼ����BlockBuffer';
  end else
  begin
    FBlockBufferTesterTerminate := False;
    for i := 1 to StrToInt(edtThread.Text) do
    begin
      ASyncInvoke(TesterForBlockBuffer);
    end;
    btnBlockBufferTester.Caption := '���ֹͣ';
  end;
end;

procedure TForm1.btnSimpleBlockBufferClick(Sender: TObject);
var
  lvBlockBuffer:TBlockBuffer;
  lvAnsiStr:AnsiString;
begin
  Assert(FPool <> nil, '���ȳ�ʼ��BufferPool');
  lvBlockBuffer := TBlockBuffer.Create(FPool);
  try
    lvBlockBuffer.OnBufferWrite := OnSimpleBufferWrite;
    SetLength(lvAnsiStr, 100);
    FillChar(PAnsiChar(lvAnsiStr)^, 100, '1');
    lvBlockBuffer.Append(PAnsiChar(lvAnsiStr), 100);

    lvBlockBuffer.FlushBuffer;
  finally
    lvBlockBuffer.Free;
  end;
end;

procedure TForm1.btnCheckBoundsClick(Sender: TObject);
var
  r:Integer;
begin
  r := CheckBufferBounds(FPool);
  sfLogger.logMessage('���й���:%d���ڴ��, ����[%d]���ڴ��д��Խ������', [FPool.FSize, r]);
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
  FreeBufferPool(FPool);
end;

procedure TForm1.btnNewPoolClick(Sender: TObject);
begin
  FPool := NewBufferPool(StrToInt(edtBlockSize.Text));
end;

procedure TForm1.btnOutOfBoundsClick(Sender: TObject);
var
  lvBuff:PByte;
begin
  lvBuff := GetBuffer(FPool);
  AddRef(lvBuff);


  FillChar(lvBuff^, FPool.FBlockSize, 1);

  // Խ��д��
  PByte(Integer(lvBuff) + FPool.FBlockSize)^ := $FF;

  sfLogger.logMessage(TByteTools.varToHexString(lvBuff^, FPool.FBlockSize + 8));

  ReleaseRef(lvBuff);
end;

procedure TForm1.btnPoolInfoClick(Sender: TObject);
begin
  sfLogger.logMessage('get:%d, put:%d, addRef:%d, releaseRef:%d, size:%d', [FPool.FGet, FPool.FPut, FPool.FAddRef, FPool.FReleaseRef, FPool.FSize]);

  sfLogger.logMessage('threadinfo:' + GetThreadsHintInfo);
end;

procedure TForm1.btnRefBufferClick(Sender: TObject);
var
  lvBuf:Pointer;
begin
  lvBuf := GetBuffer(1024);
  AddRef(lvBuf);
  AddRef(lvBuf);
  try


  finally
    ReleaseRef(lvBuf);
    ReleaseRef(lvBuf);
  end;

end;

procedure TForm1.btnSimpleTesterClick(Sender: TObject);
var
  lvAnsiStr:AnsiString;
begin
  FBuffer := GetBuffer(FPool);

  SetLength(lvAnsiStr, 100);
  FillChar(PAnsiChar(lvAnsiStr)^, 100, '1');

  Move(PAnsiChar(lvAnsiStr)^, FBuffer^, 100);

  //AttachData(lvBuff, TSafeQueue.Create, FREE_TYPE_OBJECTFREE);
  AddRef(FBuffer);
  //AddRef(lvBuff);
  //PByte(Integer(lvBuff) + 8)^ := $FF;

 // ReleaseRef(lvBuff);
  ReleaseRef(FBuffer);
end;

procedure TForm1.btnSpeedTesterClick(Sender: TObject);
var
  i, s, lvCpuCount:Integer;
  lvWorker:TASyncWorker;
begin
  if FPool = nil then raise Exception.Create('���ȳ�ʼ��FPool');
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

procedure TForm1.btnThreadTester2Click(Sender: TObject);
var
  lvBuf:PByte;
  i: Integer;
begin
  lvBuf := GetBuffer(FPool);
  lvBuf^ := 1;
  // ͬһ�ڴ��ɶ���߳�ȥ�����黹

  for i := 1 to 100 do
  begin
    AddRef(lvBuf);
    ASyncInvoke(Tester2, lvBuf);
  end;
end;

procedure TForm1.btnThreadTesterClick(Sender: TObject);
var
  i:Integer;
begin
  // ����߳�ͬʱ��ȡ�ڴ�飬�����ж�д�黹

  for i := 1 to 100 do
  begin
    ASyncInvoke(Tester);
  end;

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

procedure TForm1.Tester(ASyncWorker:TASyncWorker);
var
  lvBuff:PByte;
  i:Integer;
  lvQueue:TSafeQueue;
begin
  Sleep(0);
  i := 1;
  while i < 100 do
  begin
    lvBuff := GetBuffer(FPool);
    lvBuff^ := 1;
    // ��Ӹ�������
    AttachData(lvBuff, TSafeQueue.Create, FREE_TYPE_OBJECTFREE);

    AddRef(lvBuff);

    SetCurrentThreadInfo('AddRef');

    // ��ȡ��������
    Assert(GetAttachData(lvBuff, Pointer(lvQueue)) = 0);
    
    Sleep(0);
    ReleaseRef(lvBuff, False, STRING_EMPTY);
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
      lvBuff := GetBuffer(FPool);
      lvBuff^ := 1;
      AddRef(lvBuff);
      FillChar(lvBuff^, 16, 0);

      ReleaseRef(lvBuff);
      inc(i);
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

procedure TForm1.TesterForBlockBuffer(ASyncWorker:TASyncWorker);
var
  lvBlockBuffer:TBlockBuffer;
  t, c:Cardinal;
  lvBlock:TBytes;
  l:Integer;
begin
  InterlockedIncrement(FBlockBufferTesterCount);
  try
    lvBlockBuffer := TBlockBuffer.Create(FPool);
    try
      lvBlockBuffer.OnBufferWrite := OnBufferWriteInThread;
      t := GetTickCount;
      c := 0;

      while not FBlockBufferTesterTerminate do
      begin
        try
          Randomize;
          l := 1 + Random(102400);
          SetLength(lvBlock, l);
          FillChar(lvBlock[0], l, Ord('a'));
          lvBlockBuffer.Append(@lvBlock[0], l);
          Inc(c);
        except
          on e:Exception do
          begin
            sfLogger.logMessage('%d:err:%s', [GetCurrentThreadID, e.Message]);
          end;   
        end;
      end;
      t := GetTickCount - t;
      sfLogger.logMessage('id:%d, t:%d, c:%d, speed:%f / s', [GetCurrentThreadId, t, c, (c * 1000.0000) / t]);
    finally
      lvBlockBuffer.Free;
    end;
  finally
    InterlockedDecrement(FBlockBufferTesterCount);
  end;
end;

end.
