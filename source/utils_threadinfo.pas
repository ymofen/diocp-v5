unit utils_threadinfo;

interface

uses
  utils.hashs,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  {$ENDIF}
  Classes, utils.strings, SysUtils;

const
  TYPE_NONE = 0;
  TYPE_FREE_OBJECT = 1;

type
  TThreadInfoObject = class
  private
    FHintInfo: string;
    FThreadID: THandle;
    FObject: TObject;
    FObjectFreeType: Integer;
    procedure ClearObject;
  public
    destructor Destroy; override;
    procedure BindObject(pvObject: TObject; pvFreeType: Integer = TYPE_NONE);
    property HintInfo: string read FHintInfo write FHintInfo;
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

implementation

var
  __info_list: TDHashTableSafe;

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
  lvInfo := GetCurrentInfoObject;
  lvInfo.HintInfo := pvInfo;
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
  __info_list := TDHashTableSafe.Create();
end;

procedure FinalizeForThreadInfo;
begin
  __info_list.FreeAllDataAsObject;
  __info_list.Free;
  __info_list := nil;
end;

procedure SetCurrentThreadInfo(pvFmtMsg: string; const args: array of const);
begin
  SetCurrentThreadInfo(Format(pvFmtMsg, args));
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


initialization






end.
