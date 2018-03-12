unit utils_threadinfo;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  {$ENDIF}
  Classes, SysUtils, SyncObjs, utils_hashs, utils_strings;

const
  TYPE_NONE = 0;
  TYPE_FREE_OBJECT = 1;

type
  TThreadInfoObject = class
  private
    FLockFlag:Integer;
    FHintInfo: string;
    FThreadID: THandle;
    FObject: TObject;
    FObjectFreeType: Integer;
    procedure ClearObject;
    function GetHintInfo: string;
    procedure SetHintInfo(const Value: string);
    procedure Lock;
    procedure UnLock;
  public
    destructor Destroy; override;
    procedure BindObject(pvObject: TObject; pvFreeType: Integer = TYPE_NONE);
    property HintInfo: string read GetHintInfo write SetHintInfo;
    property ThreadID: THandle read FThreadID write FThreadID;
  end;


procedure SetCurrentThreadInfo(const pvInfo: String; pvAddTimePre: Boolean =
    true); overload;
procedure SetCurrentThreadInfo(pvFmtMsg: string; const args: array of const);
    overload;
procedure BindThreadObject(pvObject: TObject; pvFreeType: Integer = TYPE_NONE);
function GetCurrentThreadBindObject: TObject;
function GetCurrentThreadHintInfo: String;
function GetThreadsHintInfo: String;
procedure ResetThreadHintInfo;

procedure InitalizeForThreadInfo;
procedure FinalizeForThreadInfo;

procedure StartRecordThreadInfo(pvInterval: Integer = 10000);

function GetModuleID: String;

procedure SleepInThread(pvInterval:Cardinal; pvThread:TThread);


implementation

uses
  utils_fileWriter;

type
  TCrackThread = class(TThread);
  TRecordWorker = class(TThread)
  private
    FInterval: Integer; 
  public
    constructor Create(AInterval: Integer = 10000);
    procedure Execute; override;
    property Terminated;
  end;

var
  __info_list: TDHashTableSafe;
  __worker: TRecordWorker;
  __waitEvent:TEvent;

function GetModuleID: String;
begin                                                
  Result := ExtractFileName(GetModuleName(HInstance));
  //if  then
  
end;

function IsDebugMode: Boolean;
begin
{$IFDEF MSWINDOWS}
{$warn symbol_platform off}
  Result := Boolean(DebugHook);
{$warn symbol_platform on}
{$ELSE}
  Result := false;
{$ENDIF}
end;

function GetCurrentInfoObject:TThreadInfoObject;
var
  lvCurrentID:THandle;
  lvInfo:TThreadInfoObject;
begin
  Result := nil;
  {$IFDEF MSWINDOWS}
  lvCurrentID := GetCurrentThreadId;
  {$ELSE}
  lvCurrentID := TThread.CurrentThread.ThreadID;
  {$ENDIF}
  if __info_list = nil then Exit;
  //  Assert(__info_list <> nil, 'GetCurrentInfoObject not initalize');
  __info_list.Lock;
  try
    lvInfo := TThreadInfoObject(__info_list.Values[lvCurrentID]);
    if lvInfo = nil then
    begin
      lvInfo := TThreadInfoObject.Create;
      lvInfo.ThreadID := lvCurrentID;
      {$IFDEF AUTOREFCOUNT}
      lvInfo.__ObjAddRef();
      {$ENDIF}
      __info_list.Values[lvCurrentID] := lvInfo;
    end;
  finally
    __info_list.unLock;
  end;

  Result := lvInfo;
end;

procedure SetCurrentThreadInfo(const pvInfo: String; pvAddTimePre: Boolean =
    true);
var
  lvInfo:TThreadInfoObject;
begin
  try
    lvInfo := GetCurrentInfoObject;
    if lvInfo <> nil then
    begin
      if pvAddTimePre then
      begin
        lvInfo.HintInfo := Format('[%s]:%s', [NowString, pvInfo]);
      end else
      begin
        lvInfo.HintInfo := pvInfo;
      end;
    end;
  except
    on e: Exception do
    begin     // 关闭时可能对象已经被释放了
      if IsDebugMode then
      begin
        Assert(False, e.Message);
      end;
    end;
  end;
end;

function GetThreadsHintInfo: String;
var
  lvList:TList;
  i: Integer;
  lvInfo:TThreadInfoObject;
  lvBuilder:TDStringBuilder;
begin
  lvBuilder := TDStringBuilder.Create;
  lvList := TList.Create;
  try
    __info_list.Lock;
    try
      __info_list.GetDatas(lvList);
    finally
      __info_list.UnLock;
    end;

    for i := 0 to lvList.Count - 1 do
    begin
      lvInfo := TThreadInfoObject(lvList[i]);
      lvBuilder.AppendLine(Format('%d,%s', [lvInfo.FThreadID, lvInfo.HintInfo]));
    end;
    Result := lvBuilder.ToString;
  finally
    lvList.Free;
    lvBuilder.Free;
  end;
end;

procedure BindThreadObject(pvObject: TObject; pvFreeType: Integer = TYPE_NONE);
var
  lvInfo:TThreadInfoObject;
begin
  lvInfo := GetCurrentInfoObject;
  lvInfo.BindObject(pvObject, pvFreeType);
end;


function GetCurrentThreadBindObject: TObject;
var
  lvInfo:TThreadInfoObject;
begin
  lvInfo := GetCurrentInfoObject;
  Result := lvInfo.FObject;
end;

procedure InitalizeForThreadInfo;
begin
  if __info_list = nil then
    __info_list := TDHashTableSafe.Create();
end;

procedure FinalizeForThreadInfo;
begin
  if __worker <> nil then
  begin
    __worker.Terminate;
    if (__waitEvent.WaitFor(10000) = wrSignaled) then
    begin
      __waitEvent.Free;
      __waitEvent := nil;
    end else
    begin
      // warning....
    end;
    __worker := nil;
  end;
  if __info_list <> nil then
  begin
    __info_list.FreeAllDataAsObject;
    __info_list.Free;
    __info_list := nil;
  end;
end;

procedure SetCurrentThreadInfo(pvFmtMsg: string; const args: array of const);
begin
  SetCurrentThreadInfo(Format(pvFmtMsg, args));
end;

procedure StartRecordThreadInfo(pvInterval: Integer = 10000);
begin
  if __worker <> nil then exit;

  if __waitEvent = nil then __waitEvent := TEvent.Create(nil, True, False, '');
  __worker := TRecordWorker.Create(pvInterval);
  
end;

procedure ResetThreadHintInfo;
begin
  __info_list.Lock;
  try
    __info_list.FreeAllDataAsObject;
    __info_list.Clear;
  finally
    __info_list.UnLock;
  end;
end;

function GetCurrentThreadHintInfo: String;
var
  lvInfoObj:TThreadInfoObject;
begin
  try
    lvInfoObj := GetCurrentInfoObject;
    Result := lvInfoObj.GetHintInfo;
  except
    on e:Exception do
    begin
      Result := 'GetCurrentThreadHintInfo Err:' + e.Message;
    end;
  end;
end;

procedure SleepInThread(pvInterval:Cardinal; pvThread:TThread);
var
  lvTick:Cardinal;
begin
  lvTick := GetTickCount;
  while (not TCrackThread(pvThread).Terminated) and (tick_diff(lvTick, GetTickCount) < pvInterval) do
  begin
    Sleep(100);  
  end;  
end;





destructor TThreadInfoObject.Destroy;
begin
  ClearObject;
  inherited Destroy;
end;

procedure TThreadInfoObject.BindObject(pvObject: TObject; pvFreeType: Integer =
    TYPE_NONE);
begin
  ClearObject;
  FObject := pvObject;
  FObjectFreeType := pvFreeType;
end;

procedure TThreadInfoObject.ClearObject;
begin
  if FObject = nil then Exit;
  if FObjectFreeType = TYPE_FREE_OBJECT then
  begin
    FObject.Free;
  end;           
  FObject := nil;
end;

function TThreadInfoObject.GetHintInfo: string;
begin
  Lock();
  Result := FHintInfo;
  UnLock();
end;

procedure TThreadInfoObject.Lock;
begin
  SpinLock(FLockFlag);  
end;

procedure TThreadInfoObject.SetHintInfo(const Value: string);
begin
  Lock();
  FHintInfo := Value;
  UnLock;
end;

procedure TThreadInfoObject.UnLock;
begin
  SpinUnLock(FLockFlag);
end;

constructor TRecordWorker.Create(AInterval: Integer = 10000);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FInterval := AInterval;
end;


procedure TRecordWorker.Execute;
var
  lvWriter:TSingleFileWriter;
  s:String;
begin
  __waitEvent.ResetEvent;
  lvWriter := TSingleFileWriter.Create;
  try
    lvWriter.CacheSize := 0;
    if HInstance <> MainInstance then
    begin    
      lvWriter.FilePreFix := '线程_监控_' + ExtractFileName(GetModuleName(HInstance)) + '_';
    end else
    begin
      lvWriter.FilePreFix := '线程_监控_';
    end;
    lvWriter.LogMessage('启动时间:' + FormatDateTime('yyyy-mm-dd:HH:nn:ss.zzz', Now));
    while not self.Terminated do
    begin
      try
        s := GetThreadsHintInfo;
        if s <> '' then
        begin
          lvWriter.LogMessage('当前状态:' + sLineBreak + s);
        end;
      except
        on E:Exception do
        begin
          lvWriter.LogMessage('ERR:记录状态失败:' + e.Message);
        end;
      end;
      SleepInThread(FInterval, Self);
    end;
  finally
    __worker := nil;
    lvWriter.Free;
  end;

  __waitEvent.SetEvent;
end;


initialization
  InitalizeForThreadInfo;

finalization
  FinalizeForThreadInfo;
  Assert(__info_list = nil, 'utils_thread_memoery_leak');




end.
