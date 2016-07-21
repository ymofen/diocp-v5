unit utils_threadvars;

interface

uses
  utils_hashs,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  {$ENDIF}
  Classes, SysUtils, SyncObjs, utils_dvalue, utils_BufferPool;

const
  MAX_THREADID_VALUE = 65535;

type
  TPointerNotifyProc = procedure(const sender:Pointer; const v:Pointer);

  PThreadVarRecord = ^TThreadVarRecord;
  TThreadVarRecord = record
    FVar:Pointer;
    FReleaseCallBack:TPointerNotifyProc;
    FLastActivity:Cardinal;
  end;

  PThreadVars = ^TThreadVars;
  TThreadVars = record
    FVarArray:array[1..MAX_THREADID_VALUE] of PThreadVarRecord;
    FListLocker:Integer;
    FList:TDHashTable;
  end;

function NewThreadVars: PThreadVars;
procedure DisposeThreadVars(const p_thread_vars:PThreadVars);
function GetCurrentThreadVar(const p_thread_vars: PThreadVars): Pointer;
procedure SetCurrentThreadVar(const p_thread_vars: PThreadVars; const v:
    Pointer; pvReleaseCallBack: TPointerNotifyProc);


function GetCurrentThreadDValue: TDValue;
procedure ResetThreadVars;

procedure InitalizeForThreadVars;
procedure FinalizeForThreadVars;


implementation


var
  __info_list: TDHashTableSafe;
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

function GetCurrentThreadDValue: TDValue;
var
  lvCurrentID:THandle;
  lvInfo:TDValue;
begin
  Result := nil;
  {$IFDEF MSWINDOWS}
  lvCurrentID := GetCurrentThreadId;
  {$ELSE}
  lvCurrentID := TThread.CurrentThread.ThreadID;
  {$ENDIF}
  if __info_list = nil then Exit;
  //  Assert(__info_list <> nil, 'GetCurrentThreadDValue not initalize');
  __info_list.Lock;
  try
    lvInfo := TDValue(__info_list.Values[lvCurrentID]);
    if lvInfo = nil then
    begin
      lvInfo := TDValue.Create;
      lvInfo.ForceByName('__threadid').AsInteger := lvCurrentID;
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


procedure InitalizeForThreadVars;
begin
  if __info_list = nil then
    __info_list := TDHashTableSafe.Create();
end;

procedure FinalizeForThreadVars;
begin
  if __info_list <> nil then
  begin
    __info_list.FreeAllDataAsObject;
    __info_list.Free;
    __info_list := nil;
  end;
end;

procedure ResetThreadVars;
begin
  __info_list.Lock;
  try
    __info_list.FreeAllDataAsObject;
    __info_list.Clear;
  finally
    __info_list.UnLock;
  end;
end;

function NewThreadVars: PThreadVars;
begin
  New(Result);
  Result.FList := TDHashTable.Create;
  Result.FListLocker := 0;
end;

procedure DisposeThreadVars(const p_thread_vars:PThreadVars);
begin
  DisposeThreadVars(p_thread_vars);
end;

procedure InnerReleaseVar(const p_thread_vars: PThreadVars; const
    p_thread_varrecord: PThreadVarRecord);
begin
  if p_thread_varrecord.FVar <> nil then
  begin
    p_thread_varrecord.FReleaseCallBack(p_thread_vars, p_thread_varrecord.FVar);
    p_thread_varrecord.FVar := nil;
  end;
end;

function GetCurrentThreadVar(const p_thread_vars: PThreadVars): Pointer;
var
  lvThreadID:Cardinal;
  lvPVar:PThreadVarRecord;
begin
  lvThreadID := GetCurrentThreadId;
  if lvThreadID > MAX_THREADID_VALUE then
  begin
    SpinLock(p_thread_vars.FListLocker);
    try
      lvPVar := p_thread_vars.FList.Values[lvThreadID];       
    finally
      SpinUnLock(p_thread_vars.FListLocker);
    end;
  end else
  begin
    lvPVar := p_thread_vars.FVarArray[lvThreadID];
  end;
  if lvPVar <> nil then
  begin
    Result := lvPVar.FVar;
    lvPVar.FLastActivity := GetTickCount;
  end else
  begin
    Result := nil;
  end;
end;

procedure SetCurrentThreadVar(const p_thread_vars: PThreadVars; const v:
    Pointer; pvReleaseCallBack: TPointerNotifyProc);
var
  lvThreadID:Cardinal;
  lvPVar:PThreadVarRecord;
  procedure innerProcessVar();
  begin
    if lvPVar = nil then
    begin
      New(lvPVar);
      p_thread_vars.FList.Values[lvThreadID] := lvPVar;
      lvPVar.FVar := v;
    end else
    begin
      InnerReleaseVar(p_thread_vars, lvPVar);
    end;
    lvPVar.FLastActivity := GetTickCount;
  end;
begin
  lvThreadID := GetCurrentThreadId;
  if lvThreadID > MAX_THREADID_VALUE then
  begin
    SpinLock(p_thread_vars.FListLocker);
    try
      lvPVar := p_thread_vars.FList.Values[lvThreadID];
      innerProcessVar;
    finally
      SpinUnLock(p_thread_vars.FListLocker);
    end;
  end else
  begin
    lvPVar := p_thread_vars.FVarArray[lvThreadID];
    innerProcessVar;
  end;


end;


initialization
  InitalizeForThreadVars;

finalization
  FinalizeForThreadVars;
  Assert(__info_list = nil, 'utils_thread_memoery_leak');




end.
