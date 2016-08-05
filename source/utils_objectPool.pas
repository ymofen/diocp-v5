(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-04-13 13:03:47
 *     通用对象池
 *
 *)
unit utils_objectPool;

interface

uses
  utils_queues, Windows, SysUtils;

type
{$IFDEF UNICODE}
  TOnCreateObjectEvent = reference to function:TObject;
{$ELSE}
  TOnCreateObjectEvent = function:TObject of object;
{$ENDIF}

  TObjectPool = class(TObject)
  private
    FName: String;
    FUsingCount: Integer;

    FObjectList: TBaseQueue;
    FOnCreateObjectEvent: TOnCreateObjectEvent;
    FCount: Integer;
    FOutTime: Integer;
    FReleaseTime: Integer;
  public
    constructor Create(AOnCreateObjectEvent: TOnCreateObjectEvent);

    destructor Destroy; override;

    /// <summary>
    ///   清理对象，非线程安全
    /// </summary>
    procedure Clear;
    
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

    /// <summary>
    ///   正在使用数量
    /// </summary>
    property UsingCount: Integer read FUsingCount;

    /// <summary>
    ///   对象数量
    /// </summary>
    property Count: Integer read FCount;

    /// <summary>
    ///   名称，可以用于监控调试
    /// </summary>
    property Name: String read FName write FName;


    /// <summary>
    ///  借出次数
    /// </summary>
    property OutTime: Integer read FOutTime write FOutTime;

    /// <summary>
    ///   还回次数
    /// </summary>
    property ReleaseTime: Integer read FReleaseTime write FReleaseTime;








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

procedure TObjectPool.Clear;
begin
  FUsingCount := 0;
  FCount := 0;
  FObjectList.FreeDataObject;
  FObjectList.Clear;
end;

constructor TObjectPool.Create(AOnCreateObjectEvent: TOnCreateObjectEvent);
begin
  inherited Create;
  FCount := 0;
  FUsingCount := 0;
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
    InterlockedIncrement(FCount);
  end;
  InterlockedIncrement(FUsingCount);
  
  InterlockedIncrement(FOutTime);

end;

procedure TObjectPool.ReleaseObject(pvObject:TObject);
begin
  FObjectList.EnQueue(pvObject);
  InterlockedDecrement(FUsingCount);
  InterlockedIncrement(FReleaseTime);
end;

function TObjectPool.WaitFor(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
  c:Integer;
begin
  l := GetTickCount;
  c := FUsingCount;
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
    c := FUsingCount;
  end;

  Result := FUsingCount = 0;
end;

initialization
  __ProcessIDStr := IntToStr(GetCurrentProcessId);

end.
