unit utils_sync_4ui;

interface

uses
  utils_strings, Classes
{$IFDEF MSWINDOWS}
  , windows, messages
{$ENDIF};


const
  WM_SYNC_MSG = WM_USER + 1;

type
  TSyncTaskObject = class(TObject)
  private
    FCb:TDataEvent;
    FData:Pointer;
    FDataFreeProc:TDataProc;
  public
    procedure DoCallBack();
    procedure DoTaskForSync;
  end;




  TSync4UI = class(TObject)
  private

  protected
  {$IFDEF MSWINDOWS}
    FMessageHandle: HWND;
    procedure DoMainThreadWork(var AMsg: TMessage);
  {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure RaiseMessage(const pvMsg:string);
    procedure Post(pvCb:TDataEvent; pvData:Pointer; pvDataFreeProc:TDataProc);
  end;

implementation

constructor TSync4UI.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
  FMessageHandle := AllocateHWnd(DoMainThreadWork);
  {$ENDIF}
end;

destructor TSync4UI.Destroy;
begin
  {$IFDEF MSWINDOWS}
  DeallocateHWnd(FMessageHandle);
  {$ENDIF}
  inherited;
end;

{$IFDEF MSWINDOWS}
procedure TSync4UI.DoMainThreadWork(var AMsg: TMessage);
var
  lvSyncObj:TSyncTaskObject;
begin
  if AMsg.Msg = WM_SYNC_MSG then
  begin
    try
      lvSyncObj := TSyncTaskObject(AMsg.WPARAM);
      lvSyncObj.DoCallBack;
    finally
      lvSyncObj.Free;
    end;
  end else
    AMsg.Result := DefWindowProc(FMessageHandle, AMsg.Msg, AMsg.WPARAM, AMsg.LPARAM);
end;
{$ENDIF}


procedure TSync4UI.Post(pvCb:TDataEvent; pvData:Pointer;
    pvDataFreeProc:TDataProc);
var
  lvTaskData:TSyncTaskObject;
begin
  lvTaskData := TSyncTaskObject.Create;
  lvTaskData.FCb := pvCb;
  lvTaskData.FData := pvData;
  lvTaskData.FDataFreeProc := pvDataFreeProc;
  TThread.Queue(nil, lvTaskData.DoTaskForSync);

//  {$IFDEF MSWINDOWS}
//  PostMessage(FMessageHandle, WM_SYNC_MSG, WPARAM(lvTaskData), 0)
//  {$ELSE}
//
//  {$ENDIF}
end;

procedure TSync4UI.RaiseMessage(const pvMsg: string);
begin
  ;
end;

{ TSyncTaskObject }

procedure TSyncTaskObject.DoCallBack;
begin
  try
    FCb(self.FData);
  finally
    if Assigned(FDataFreeProc) then
    begin
      FDataFreeProc(self.FData);
    end;
  end;
end;

procedure TSyncTaskObject.DoTaskForSync;
begin
  try
    Self.DoCallBack;
  finally
    Self.Free;
  end;
end;

end.
