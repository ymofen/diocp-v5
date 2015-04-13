(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-04-13 13:03:47
 *     通用对象池
 *
 *)
unit utils.objectPool;

interface

uses
  utils.queues, Windows, SysUtils;

type
{$IFDEF UNICODE}
  TOnCreateObjectEvent = reference to function:TObject;
{$ELSE}
  TOnCreateObjectEvent = function:TObject of object;
{$ENDIF}

  TObjectPool = class(TObject)
  private
    FCreateCounter:Integer;
    FName: String;
    FOutCounter:Integer;
    
    FObjectList: TBaseQueue;
    FOnCreateObjectEvent: TOnCreateObjectEvent;
  public
    constructor Create(AOnCreateObjectEvent: TOnCreateObjectEvent);

    destructor Destroy; override;
    
    /// <summary>
    ///   等待所有对象归还
    /// </summary>
    function WaitFor(pvTimeOut: Cardinal): Boolean;


    /// <summary>
    ///   获取对象
    /// </summary>
    function GetObject:TObject;

    /// <summary>
    ///   归还对象
    /// </summary>
    procedure ReleaseObject(pvObject:TObject);

    property Name: String read FName write FName;

    /// <summary>
    ///   创建对象事件
    /// </summary>
    property OnCreateObjectEvent: TOnCreateObjectEvent read FOnCreateObjectEvent
        write FOnCreateObjectEvent;



  end;

implementation

var
  __ProcessIDStr :String;

procedure WriteFileMsg(pvMsg:String; pvFilePre:string);
var
  lvFileName, lvBasePath:String;
  lvLogFile: TextFile;
begin
  try
    lvBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
    ForceDirectories(lvBasePath);
    lvFileName :=lvBasePath + '\' + __ProcessIDStr+ '_' + pvFilePre +
     FormatDateTime('mmddhhnn', Now()) + '.log';

    AssignFile(lvLogFile, lvFileName);
    if (FileExists(lvFileName)) then
      append(lvLogFile)
    else
      rewrite(lvLogFile);

    writeln(lvLogFile, pvMsg);
    flush(lvLogFile);
    CloseFile(lvLogFile);
  except
    ;
  end;
end;

constructor TObjectPool.Create(AOnCreateObjectEvent: TOnCreateObjectEvent);
begin
  inherited Create;
  FOutCounter := 0;
  FObjectList := TBaseQueue.Create();
  FOnCreateObjectEvent := AOnCreateObjectEvent;
end;

destructor TObjectPool.Destroy;
begin
  FObjectList.FreeDataObject;
  FObjectList.Free;
  inherited Destroy;
end;

function TObjectPool.GetObject: TObject;
begin
  Result := FObjectList.DeQueue;
  if Result = nil then
  begin
    Assert(Assigned(FOnCreateObjectEvent));
    Result := FOnCreateObjectEvent();
    Assert(Result <> nil);
    InterlockedIncrement(FCreateCounter);
  end;
  InterlockedIncrement(FOutCounter); 
end;

procedure TObjectPool.ReleaseObject(pvObject:TObject);
begin
  FObjectList.EnQueue(pvObject);
  InterlockedDecrement(FOutCounter);
end;

function TObjectPool.WaitFor(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
  c:Integer;
begin
  l := GetTickCount;
  c := FOutCounter;
  while (c > 0) do
  begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}

    if GetTickCount - l > pvTimeOut then
    begin
      WriteFileMsg(Format('(%s)WaitFor 等待超时, 当前未归还数量:%d', [FName, c]), 'WaitFor');
      Break;
    end;
    c := FOutCounter;
  end;

  Result := FOutCounter = 0;
end;

initialization
  __ProcessIDStr := IntToStr(GetCurrentProcessId);

end.
