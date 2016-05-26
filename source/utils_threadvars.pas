unit utils_threadvars;

interface

uses
  utils_hashs,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  {$ENDIF}
  Classes, SysUtils, SyncObjs, utils_dvalue;


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


initialization
  InitalizeForThreadVars;

finalization
  FinalizeForThreadVars;
  Assert(__info_list = nil, 'utils_thread_memoery_leak');




end.
