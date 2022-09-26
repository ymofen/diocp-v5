unit utils_dtimewheel;

interface

uses
  SyncObjs,
  {$IFDEF MSWINDOWS}
  Windows, ActiveX,
  {$ENDIF}
  utils_dtimer, utils_strings, Classes, utils_sync_object,
  utils_hashs, SysUtils;

type
  TDTimeWheel = class;
  TTaskCallBack = procedure(pvTimeWheel:TDTimeWheel; pvUserData:Pointer) of object;

  PDTimeWheelTaskRec = ^TDTimeWheelTaskRec;
  PDTimeWheelChan = ^TDTimeWheelChan;

  TDTimeWheelTaskRec = record
    __type:Word;    // Operatype
    TaskID: Integer;
    CallBack: TTaskCallBack;
    UserData: Pointer;
    UserDataRelease: TDataProc;
    Interval: Cardinal;
    RepeatNum: Cardinal;
    RunCounter: Int64;
    CyleNum: Integer;
    Chan: PDTimeWheelChan;
    Front: PDTimeWheelTaskRec;
    Next: PDTimeWheelTaskRec;
  end;


  TDTimeWheelChan = record
    first: PDTimeWheelTaskRec;
    last: PDTimeWheelTaskRec;

    size: Integer;
  end;

  TDTimeWheel = class(TObject)
  private
{$IFDEF MSWINDOWS}
    FCoInitialized:Boolean;
{$ENDIF}
  private
    FInterval_msecs:Cardinal;



    FLock:Integer;
    FEventChan: PDTimeWheelChan;

    // ��������
    FSlotIdx:Integer;
    FSlotNum:Integer;
    FSlot:array of PDTimeWheelChan;
    FTaskIDCounter:Integer;
    FTaskMap: TDHashTable;

    FHandles: array[0..2] of THandle;
    FTicker: TDTimer;
    FIsTerminated:Boolean;
    FOnException: TExceptionNotifyEvent;

    procedure Execute;
    procedure OnTickerTimer(pvSender: TObject);
    procedure DoTimeWheelEvent;
    procedure DoCallBack(pvRec:PDTimeWheelTaskRec);
    procedure DoReleaseRec(pvRec:PDTimeWheelTaskRec);
    function ProcessCmdChan: Boolean;
    procedure InnerRemove(pvTaskID:Integer);
    procedure InnerRemoveAllTask;
    procedure InnerRemoveSlotList;

    procedure DoIdle;
    procedure InnerStart(pvSender:TObject);
    procedure InitialChan(pvChan:PDTimeWheelChan);
    procedure Add2Chan(pvChan: PDTimeWheelChan; pvTaskRec: PDTimeWheelTaskRec;
        pvNum: Integer = 0);

    /// <summary>TDTimeWheel.AddSlot
    /// </summary>
    /// <param name="pvRec"> (PDTimeWheelTaskRec) </param>
    /// <param name="pvCurrIdx"> ���ú����������������Slot���¼�������slot����ţ����û�д�����-1 </param>
    procedure AddSlot(pvRec: PDTimeWheelTaskRec; pvCurrIdx: Integer);
    procedure PostRequestCloseCmd();

    function FIFOChan(pvChan:PDTimeWheelChan): PDTimeWheelTaskRec;
  public
{$IFDEF MSWINDOWS}
    procedure CheckCoInitializeEx(pvReserved: Pointer = nil; coInit: Longint = 0);
{$ENDIF}
  public
    /// <summary>TDTimeWheel.Create
    /// </summary>
    /// <param name="pvMSecs4Interval"> ʱ���ֶ��ת��һ�� </param>
    /// <param name="pvSlotNum"> ʱ���֣������� </param>
    constructor Create(pvMSecs4Interval: Cardinal; pvSlotNum: Integer);
    destructor Destroy; override;


    /// <summary>TDTimeWheel.AddTask
    /// </summary>
    /// <param name="pvInterval"> ִ���ӳ�(���), �����pvMSecs����������������ǣ����н����ӳ� </param>
    /// <param name="pvTaskCb"> �ص� </param>
    /// <param name="pvUserData"> �ص��û����� </param>
    /// <param name="pvRepeatNum"> �ظ�����, 0Ϊ�����ظ� </param>
    /// <returns>����һ������ID, �Ƴ�ʱʹ�� </returns>
    function AddTask(pvInterval: Cardinal; pvTaskCb: TTaskCallBack; pvUserData:
        Pointer; pvRepeatNum: Cardinal; pvUserDataFreeProc: TDataProc): Integer;


    procedure RemoveTask(pvTaskID: Integer);

    procedure Start(pvIsAsync: Boolean);
    procedure Stop;
    
    property OnException: TExceptionNotifyEvent read FOnException write
        FOnException;




  end;



var
  Dtw:TDTimeWheel;

procedure StopDtw;
procedure InitialDtw(pvIntervalMSecs:Cardinal);

implementation


function NewTaskRec():PDTimeWheelTaskRec;
begin
  New(Result);
  Result.Front := nil;
  Result.Next := nil;
  Result.UserDataRelease := nil;
  Result.CallBack := nil;
  Result.UserData := nil;
end;

procedure StopDtw;
begin
  if Dtw <> nil then
  begin
    Dtw.Stop;
    Dtw.Free;
    Dtw := nil;
  end;
end;

procedure InitialDtw(pvIntervalMSecs:Cardinal);
begin
  if Dtw <> nil then Exit;
  
  Dtw := TDTimeWheel.Create(pvIntervalMSecs, 128);
  Dtw.Start(True);
end;


function TDTimeWheel.AddTask(pvInterval: Cardinal; pvTaskCb: TTaskCallBack;
    pvUserData: Pointer; pvRepeatNum: Cardinal; pvUserDataFreeProc: TDataProc):
    Integer;
var
  lvItm:PDTimeWheelTaskRec;
begin
  lvItm := NewTaskRec;
  lvItm.TaskID := AtomicIncrement(self.FTaskIDCounter);
  lvItm.CallBack := pvTaskCb;
  lvItm.UserData := pvUserData;
  lvItm.Interval := pvInterval;
  lvItm.RepeatNum := pvRepeatNum;
  lvItm.UserDataRelease := pvUserDataFreeProc;
  lvItm.__type := 1;
  SpinLock(FLock);
  try
    Add2Chan(FEventChan, lvItm, 1);
  finally
    SpinUnLock(FLock);
  end;

  SetEvent(self.FHandles[1]);
  Result := lvItm.TaskID;
end;

{$IFDEF MSWINDOWS}
procedure TDTimeWheel.CheckCoInitializeEx(pvReserved: Pointer; coInit: Integer);
begin
  if not FCoInitialized then
  begin
    CoInitializeEx(pvReserved, coInit);
    FCoInitialized := true;
  end;
end;
{$ENDIF}

constructor TDTimeWheel.Create(pvMSecs4Interval: Cardinal; pvSlotNum: Integer);
begin
  inherited Create;
  FIsTerminated := true;
  FTaskMap := TDHashTable.Create();
  FTaskIDCounter := 1;
  FInterval_msecs := pvMSecs4Interval;
  FTicker := TDTimer.Create();
  New(FEventChan);
  InitialChan(FEventChan);
  FSlotNum := pvSlotNum;
  SetLength(FSlot, FSlotNum);
  FillChar(FSlot[0], SizeOf(FSlot), 0);
  FSlotIdx := 0;
  FTicker.OnTimer := self.OnTickerTimer;
  FHandles[0] := CreateEvent(nil, False, False, nil);
  FHandles[1] := CreateEvent(nil, False, False, nil);
  FHandles[2] := CreateEvent(nil, true, False, nil);
end;

destructor TDTimeWheel.Destroy;
begin
  FTicker.Free;
  Dispose(FEventChan);
  FTaskMap.Free;
  inherited Destroy;
end;

procedure TDTimeWheel.Add2Chan(pvChan: PDTimeWheelChan; pvTaskRec:
    PDTimeWheelTaskRec; pvNum: Integer = 0);
var
  j:Integer;
  lvItm:PDTimeWheelTaskRec;
begin
  if pvChan.first = nil then
  begin
    pvChan.first := pvTaskRec;
    pvChan.last := pvTaskRec;
    pvChan.size := 1;
  end else
  begin
    pvChan.last.Next := pvTaskRec;
    pvTaskRec.Front := pvChan.last;
    pvChan.last := pvTaskRec;
    Inc(pvChan.size);
  end;
  pvChan.last.Chan := pvChan;
  j := 1;
  if (pvNum = 0) or (pvNum >= j) then
  begin
    lvItm := pvTaskRec.Next;
    while lvItm <> nil do
    begin
      Inc(pvChan.size);
      pvChan.last := lvItm;
      pvChan.last.Chan := pvChan;
      lvItm := lvItm.Next;
      Inc(j);
      if j = pvNum then break;
    end;
  end;

  pvChan.last.Next := nil;
end;

procedure TDTimeWheel.AddSlot(pvRec: PDTimeWheelTaskRec; pvCurrIdx: Integer);
var
  lvIdx:Integer;
  lvChan:PDTimeWheelChan;
begin
  lvIdx :=Trunc(pvRec.Interval / self.FInterval_msecs + 0.500001);
  pvRec.CyleNum := lvIdx div FSlotNum;   // N Cycle

  lvIdx := FSlotIdx + lvIdx;

  lvIdx := lvIdx mod FSlotNum;
  lvChan := FSlot[lvIdx];
  if lvChan = nil then
  begin
    New(lvChan);
    InitialChan(lvChan);
    FSlot[lvIdx] := lvChan;
  end;

  if (pvCurrIdx <> -1) and (pvCurrIdx = lvIdx) then
  begin  // �����ǰ�����Slot���¸��ӵ�Slot��ͬһ������Ҫ��ѭ����-1����Ϊ��ǰSlot�ڵ�Task�Ѿ�����
         // ���統ǰ��0, �¸��Ӻ���0����������һ���ڵ�0��������һ������Ҫ���д���������1��Ҫ-1
    if pvRec.CyleNum > 0 then
    begin
      Dec(pvRec.CyleNum);
    end;
  end;


  Add2Chan(lvChan, pvRec, 1);
end;

procedure TDTimeWheel.DoCallBack(pvRec: PDTimeWheelTaskRec);
begin
  if Assigned(pvRec.CallBack) then
  begin
    try
      pvRec.CallBack(self, pvRec.UserData);
    except
      on E:Exception do
      begin
        if Assigned(FOnException) then
          FOnException(self, E, 0);
      end;
    end;
  end;
  pvRec.RunCounter := pvRec.RunCounter + 1;
end;

procedure TDTimeWheel.PostRequestCloseCmd;
var
  lvItm:PDTimeWheelTaskRec;
begin
  lvItm := NewTaskRec;
  lvItm.__type := 0;
  SpinLock(FLock);
  try
    Add2Chan(FEventChan, lvItm, 1);
  finally
    SpinUnLock(FLock);
  end;

  SetEvent(self.FHandles[1]);
end;

function TDTimeWheel.ProcessCmdChan: Boolean;
var
  lvRec, lvRec2:PDTimeWheelTaskRec;
  lvEmpty:Boolean;
begin
  Result := True;

  lvEmpty := false;
  while True do
  begin
    SpinLock(FLock);
    try
      if FEventChan.first = nil then Break;
      lvRec := FIFOChan(FEventChan);
      if lvRec = nil then break;
    finally
      SpinUnLock(FLock);
    end;

    if lvRec.__type = 0 then
    begin   // ��Ҫ�˳���
      Result :=false;
      lvEmpty := True;
    end;

    if FIsTerminated or lvEmpty then
    begin   // ֱ����ն��С�
      DoReleaseRec(lvRec);
    end else
    begin
      if lvRec.__type = 1 then
      begin    // �ҵ�ʱ����
        AddSlot(lvRec, -1);
        FTaskMap.Values[lvRec.TaskID] := lvRec;
      end else if lvRec.__type = 2 then
      begin     // ɾ��ʱ�����е��¼�
        InnerRemove(lvRec.TaskID);
        DoReleaseRec(lvRec);
      end;
    end;
  end;
end;

procedure TDTimeWheel.DoIdle;
begin
  ;
end;

procedure TDTimeWheel.DoReleaseRec(pvRec:PDTimeWheelTaskRec);
begin
  if Assigned(pvRec.UserDataRelease) then
  begin
    pvRec.UserDataRelease(pvRec.UserData);
  end;
  Dispose(pvRec);

end;


procedure TDTimeWheel.DoTimeWheelEvent;
var
  lvChan:PDTimeWheelChan;
  j:Integer;
  lvRec:PDTimeWheelTaskRec;
begin
  lvChan := FSlot[FSlotIdx];
  if lvChan = nil then
  begin
    Exit;
  end;

  j := lvChan.size;
  if j = 0 then
  begin
    Exit;
  end;

  while j > 0 do
  begin
    lvRec := FIFOChan(lvChan);
    if lvRec = nil then Break;
    if lvRec.CyleNum = 0 then
    begin
      DoCallBack(lvRec);
      if (lvRec.RepeatNum = 0) or (lvRec.RepeatNum > lvRec.RunCounter) then
        AddSlot(lvRec, FSlotIdx)    // ���¸���
      else
      begin
        DoReleaseRec(lvRec);
      end;
    end else
    begin    // ��û��ʱ��, ���Ǹ��ӵ�����Slot��
      Dec(lvRec.CyleNum);
      Add2Chan(lvChan, lvRec);
    end;
    Dec(j);
  end;

end;

{ TDTimeWheel }

procedure TDTimeWheel.Execute;
var
  r:Integer;
begin
  while True do
  begin
    r := WaitForMultipleObjects(2, PWOHandleArray(@FHandles[0]), False,1000);
    case r of
      WAIT_OBJECT_0:
        begin   // ʱ�䵽, ����ʱ����
          DoTimeWheelEvent();
          Inc(FSlotIdx);
          FSlotIdx := FSlotIdx mod FSlotNum;
        end;
      WAIT_OBJECT_0 + 1:
        begin
          if not ProcessCmdChan() then
          begin
            Break;
          end;
        end;
      WAIT_TIMEOUT:
        begin
          DoIdle();
        end;
    else
        begin
          DoIdle();
        end;
    end;
  end;
end;

function TDTimeWheel.FIFOChan(pvChan:PDTimeWheelChan): PDTimeWheelTaskRec;
begin
  Result := pvChan.first;
  if Result = nil then exit;

  pvChan.first := Result.Next;
  if pvChan.first <> nil then
    pvChan.first.Front := nil;
  Dec(pvChan.size);

  Result.Next := nil;
  Result.Front := nil;
end;

procedure TDTimeWheel.InitialChan(pvChan: PDTimeWheelChan);
begin
  pvChan.first := nil;
  pvChan.last := nil;
  pvChan.size := 0;
end;

procedure TDTimeWheel.InnerRemove(pvTaskID: Integer);
var
  lvRec2:PDTimeWheelTaskRec;
begin
  lvRec2 := PDTimeWheelTaskRec(FTaskMap.Values[pvTaskID]);
  if lvRec2 <> nil then
  begin      // �Ƴ�ʱ�����е��¼�, ���м�����¼�
    if lvRec2.Front <> nil then
    begin
      lvRec2.Front.Next := lvRec2.Next;
    end;

    if lvRec2.Next<>nil then
    begin
      lvRec2.Next.Front := lvRec2.Front;
    end;

    if lvRec2.Chan <> nil then
    begin
      if lvRec2.Chan.last = lvRec2 then   // ����Ƴ��������һ��
        lvRec2.Chan.last := lvRec2.Front;
      if lvRec2.Chan.first = lvRec2 then  // ����Ƴ����ǵ�һ��
        lvRec2.Chan.first := lvRec2.Next;

      Dec(lvRec2.Chan.size);
    end;




    // �ͷ�
    DoReleaseRec(lvRec2);

    FTaskMap.DeleteFirst(pvTaskID);
  end;
end;

procedure TDTimeWheel.InnerRemoveAllTask;
var
  lvList:TList;
  i: Integer;
begin
  lvList:=TList.Create;
  try
    FTaskMap.GetDatas(lvList);
    for i := 0 to lvList.Count -1 do
    begin
      InnerRemove(PDTimeWheelTaskRec(lvList[i]).TaskID);
    end;
    FTaskMap.Clear;
  finally
    lvList.Free;
  end;
end;

procedure TDTimeWheel.InnerRemoveSlotList;
var
  i: Integer;
  lvChan:PDTimeWheelChan;
begin
  for i := 0 to length(FSlot)-1 do
  begin
    lvChan := FSlot[i];
    if lvChan <> nil then
    begin
      Dispose(lvChan);
    end;
    FSlot[i] := nil;
  end;
end;

procedure TDTimeWheel.InnerStart(pvSender: TObject);
begin
  ResetEvent(FHandles[2]);
  try
    FTicker.Start(FInterval_msecs);
    Self.Execute;
    InnerRemoveAllTask;
    InnerRemoveSlotList;
    
{$IFDEF MSWINDOWS}
    if FCoInitialized then CoUninitialize();
{$ENDIF}
    
  finally
    SetEvent(FHandles[2]);
  end;
end;

procedure TDTimeWheel.OnTickerTimer(pvSender: TObject);
begin
  SetEvent(Self.FHandles[0]);
end;

procedure TDTimeWheel.RemoveTask(pvTaskID: Integer);
var
  lvItm:PDTimeWheelTaskRec;
begin
  lvItm := NewTaskRec;
  lvItm.TaskID := pvTaskID;
  lvItm.__type := 2;
  SpinLock(FLock);
  try
    Add2Chan(FEventChan, lvItm, 1);
  finally
    SpinUnLock(FLock);
  end;

  SetEvent(self.FHandles[1]);
end;

procedure TDTimeWheel.Start(pvIsAsync: Boolean);
begin
  FIsTerminated := False;
  if pvIsAsync then
  begin
    ASyncExecute(InnerStart, Self);
  end else
  begin
    InnerStart(Self);
  end;
end;

procedure TDTimeWheel.Stop;
begin
  if not FIsTerminated then
  begin
    FIsTerminated := true;
    PostRequestCloseCmd();
    FTicker.Stop;
    WaitForSingleObject(self.FHandles[2], INFINITE);
  end;
end;


initialization


finalization
  StopDtw;



end.
