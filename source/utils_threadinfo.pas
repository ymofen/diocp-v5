unit utils_threadinfo;

interface

uses
  utils.hashs,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  {$ENDIF}
  Classes, utils.strings, SysUtils, SyncObjs, utils_BufferPool;

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


procedure SetCurrentThreadInfo(pvInfo: String); overload;
procedure SetCurrentThreadInfo(pvFmtMsg: string; const args: array of const);
    overload;
procedure BindThreadObject(pvObject: TObject; pvFreeType: Integer = TYPE_NONE);
function GetCurrentThreadBindObject: TObject;
function GetThreadsHintInfo: String;

procedure InitalizeForThreadInfo;
procedure FinalizeForThreadInfo;

procedure StartRecordThreadInfo(pvInterval: Integer = 10000);


implementation

uses
  utils_fileWriter;

type
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
  {$IFDEF MSWINDOWS}
  lvCurrentID := GetCurrentThreadId;
  {$ELSE}
  lvCurrentID := TThread.CurrentThread.ThreadID;
  {$ENDIF}
  Assert(__info_list <> nil, 'GetCurrentInfoObject not initalize');
  __info_list.Lock;
  try
    lvInfo := TThreadInfoObject(__info_list.Values[lvCurrentID]);
    if lvInfo = nil then
    begin
      lvInfo := TThreadInfoObject.Create;
      lvInfo.ThreadID := lvCurrentID;
      __info_list.Values[lvCurrentID] := lvInfo;
    end;
  finally
    __info_list.unLock;
  end;

  Result := lvInfo;
end;

procedure SetCurrentThreadInfo(pvInfo: String);
var
  lvInfo:TThreadInfoObject;
begin
  try
    lvInfo := GetCurrentInfoObject;
    lvInfo.HintInfo := pvInfo;
  except
    on e: Exception do
    begin
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
  if __info_list <> nil then
  begin
    __info_list.FreeAllDataAsObject;
    __info_list.Free;
    __info_list := nil;
  end;

  if __worker <> nil then
  begin
    __worker.Terminate;
    __waitEvent.SetEvent;
    __worker := nil;
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
  lvWriter := TSingleFileWriter.Create;
  try
    lvWriter.CacheSize := 0;
    lvWriter.FilePreFix := '线程_监控_';
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
      __waitEvent.WaitFor(FInterval);
    end;
  finally
    __worker := nil;
    FreeAndNil(__waitEvent);
    lvWriter.Free;
  end;


end;


initialization
  InitalizeForThreadInfo;

finalization
  FinalizeForThreadInfo;
  Assert(__info_list = nil, 'utils_thread_memoery_leak');




end.
