unit utils_threadvars;

interface

uses
  utils_hashs,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  {$ENDIF}
  Classes, SysUtils, SyncObjs, utils_dvalue, utils_strings;

const
  MAX_THREADID_VALUE = 65535;

type
  TPointerNotifyProc = procedure(const sender:Pointer; const v:Pointer);

  PThreadVarRecord = ^TThreadVarRecord;
  PThreadVars = ^TThreadVars;

  TThreadVarRecord = record
    FVar:Pointer;
    FReleaseCallBack:TPointerNotifyProc;
    FLastActivity:Cardinal;
    FOwner:PThreadVars;
    FFlag:Integer;
  end;                 
    
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
procedure BindObjectAsThreadVar(const p_thread_vars: PThreadVars; const pvObj:
    TObject; pvOwnObject: Boolean);

procedure InnerSetThreadVarsFlag(const p_thread_vars:PThreadVars;
    pvFlag:Integer);


procedure CallBack_AsFreeObject(const sender:Pointer; const v:Pointer);
procedure CallBack_AsFreeMem(const sender:Pointer; const v:Pointer);
procedure CallBack_AsDisposeMem(const sender:Pointer; const v:Pointer);


function GetCurrentThreadDValue: TDValue;
procedure ResetThreadVars;

procedure InitalizeForThreadVars;
procedure FinalizeForThreadVars;




implementation

const
  FLAG_CLEAN_VAR = $0001;


var

  __defaultVars:PThreadVars;

procedure InnerReleaseVar(const pvPVar:PThreadVarRecord);
begin
  if Assigned(pvPVar.FReleaseCallBack) then
  begin
    pvPVar.FReleaseCallBack(pvPVar.FOwner, pvPVar.FVar);
  end; 
  pvPVar.FVar := nil;
end;


procedure CallBack_ForHashTableCleanUp(const sender:Pointer; const v:Pointer);
var
  lvPVar:PThreadVarRecord;
begin
  lvPVar := PDHashData(sender).Data;
  if (Assigned(lvPVar) and Assigned(lvPVar.FReleaseCallBack)) then
  begin
    lvPVar.FReleaseCallBack(lvPVar.FOwner, lvPVar.FVar);
  end; 
  Dispose(lvPVar);
  PDHashData(sender).Data := nil; 
end;

procedure CallBack_ForHashTableSetFlag(const sender:Pointer; const v:Pointer);
var
  lvPVar:PThreadVarRecord;
begin
  lvPVar := PDHashData(sender).Data;
  if (Assigned(lvPVar)) then
  begin
    lvPVar.FFlag := Integer(v);
  end; 
end;

procedure InnerSetThreadVarsFlag(const p_thread_vars:PThreadVars;
    pvFlag:Integer);
var
  i:Integer;
  lvPVar:PThreadVarRecord;
  lvParam:Pointer;
begin    
  for i := Low(p_thread_vars.FVarArray) to High(p_thread_vars.FVarArray) do
  begin
    lvPVar := p_thread_vars.FVarArray[i];
    if (Assigned(lvPVar)) then
    begin
      lvPVar.FFlag := pvFlag;
    end;
  end;

  lvParam := Pointer(pvFlag);
  p_thread_vars.FList.ForEach(CallBack_ForHashTableSetFlag, lvParam);
end;

/// <summary>
///   清理线程变量数据。
///   非线程安全
/// </summary>
procedure InnerCleanUpThreadVars(const p_thread_vars:PThreadVars);
var
  i:Integer;
  lvPVar:PThreadVarRecord;
begin
  for i := Low(p_thread_vars.FVarArray) to High(p_thread_vars.FVarArray) do
  begin
    lvPVar := p_thread_vars.FVarArray[i];
    if Assigned(lvPVar) then
    begin
      if Assigned(lvPVar.FReleaseCallBack) then
        lvPVar.FReleaseCallBack(p_thread_vars, lvPVar.FVar);

      Dispose(lvPVar);
      p_thread_vars.FVarArray[i] := nil;
    end;
  end;

  p_thread_vars.FList.ForEach(CallBack_ForHashTableCleanUp); 
  p_thread_vars.FList.Free;  
end;

procedure CallBack_AsFreeObject(const sender:Pointer; const v:Pointer);
begin
  FreeObject(TObject(v));
end;

procedure CallBack_AsFreeMem(const sender:Pointer; const v:Pointer);
begin
  FreeMem(v);  
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

function GetCurrentThreadDValue: TDValue;
begin
  Result := GetCurrentThreadVar(__defaultVars);
  if Result = nil then
  begin
    Result := TDValue.Create();
    BindObjectAsThreadVar(__defaultVars, Result, True);
  end;                                                 
end;


procedure InitalizeForThreadVars;
begin
  __defaultVars := NewThreadVars;
end;

procedure FinalizeForThreadVars;
begin
  DisposeThreadVars(__defaultVars);
end;

procedure ResetThreadVars;
begin
  InnerSetThreadVarsFlag(__defaultVars, FLAG_CLEAN_VAR);
end;

function NewThreadVars: PThreadVars;
begin
  New(Result);
  ZeroMemory(@Result.FVarArray[1], Length(Result.FVarArray) * SizeOf(PThreadVarRecord));
  Result.FList := TDHashTable.Create;
  Result.FListLocker := 0;
end;

procedure DisposeThreadVars(const p_thread_vars:PThreadVars);
begin
  InnerCleanUpThreadVars(p_thread_vars);
  Dispose(p_thread_vars);
end;

//procedure InnerReleaseVar(const p_thread_vars: PThreadVars; const
//    p_thread_varrecord: PThreadVarRecord);
//begin
//  if p_thread_varrecord.FVar <> nil then
//  begin
//    p_thread_varrecord.FReleaseCallBack(p_thread_vars, p_thread_varrecord.FVar);
//  end;
//  p_thread_varrecord.FVar := nil;
//end;

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
    if lvPVar.FFlag = FLAG_CLEAN_VAR then
    begin
      InnerReleaseVar(lvPVar);
      lvPVar.FFlag := 0;     // 还原 
    end;
    
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
      lvPVar.FOwner := p_thread_vars;
      lvPVar.FVar := v;
    end else
    begin
      // 清理原有对象
      InnerReleaseVar(lvPVar);
    end;
    lvPVar.FLastActivity := GetTickCount;
    lvPVar.FReleaseCallBack := pvReleaseCallBack;
  end;
begin
  lvThreadID := GetCurrentThreadId;
  if lvThreadID > MAX_THREADID_VALUE then
  begin
    SpinLock(p_thread_vars.FListLocker);
    try
      lvPVar := p_thread_vars.FList.Values[lvThreadID];
      if lvPVar = nil then
      begin
        innerProcessVar;
        p_thread_vars.FList.Values[lvThreadID] := lvPVar;
      end else
      begin
        innerProcessVar;
      end;
    finally
      SpinUnLock(p_thread_vars.FListLocker);
    end;
  end else
  begin
    lvPVar := p_thread_vars.FVarArray[lvThreadID];
    if lvPVar = nil then
    begin
      innerProcessVar;
      p_thread_vars.FVarArray[lvThreadID] := lvPVar;
    end else
    begin
      innerProcessVar;
    end;
  end;
end;

procedure BindObjectAsThreadVar(const p_thread_vars: PThreadVars; const pvObj:
    TObject; pvOwnObject: Boolean);
begin
  {$IFDEF AUTOREFCOUNT}
  AObject.__ObjAdd;
  {$ELSE}

  {$ENDIF}  
  if pvOwnObject then
  begin
    SetCurrentThreadVar(p_thread_vars, pvObj, CallBack_AsFreeObject);
  end else
  begin
    SetCurrentThreadVar(p_thread_vars, pvObj, nil);   
  end;
end;

procedure CallBack_AsDisposeMem(const sender:Pointer; const v:Pointer);
begin
  Dispose(v);    
end;    




initialization
  InitalizeForThreadVars;

finalization
  FinalizeForThreadVars;




end.
