unit utils_yunlogger;

interface

uses
  utils_dvalue, diocp_ex_httpClient, SysUtils, utils_async, utils_queues,
  utils_strings;

type
  TDiocpYunLogger = class(TObject)
  private
    FYunServerURL: String;
    FHttpClient: TDiocpHttpClient;
    FASync: TASyncInvoker;
    FLogDataQueue: TSafeQueue;
    
    procedure InnerDoLog(const s:string);
    procedure InnerWorker(pvASyncWorker: TASyncWorker);
    procedure Start;
    procedure Stop;
  public
    procedure SetYunServer(pvURL:string);

    constructor Create;
    destructor Destroy; override;
    procedure LogMessage(const pvAccessToken, pvMsg: string; const pvType: string =
        STRING_EMPTY; const pvLevel: string = STRING_EMPTY);
  end;

var
  yunLogger:TDiocpYunLogger;

procedure FinalizeYunLogger;
procedure InitalizeYunLogger;

implementation

uses
  utils_dvalue_json;

procedure FinalizeYunLogger;
begin
  if yunLogger <> nil then
  begin
    yunLogger.Stop;
    yunLogger.Free;
    yunLogger := nil;
  end;
end;

procedure InitalizeYunLogger;
begin
  if yunLogger = nil then
  begin
    yunLogger := TDiocpYunLogger.Create;
    yunLogger.Start;
  end;  
end;

constructor TDiocpYunLogger.Create;
begin
  inherited Create;
  FHttpClient := TDiocpHttpClient.Create(nil);
  FASync := TASyncInvoker.Create();
  FLogDataQueue := TSafeQueue.Create();
  FYunServerURL := 'http://123.232.98.202:9007/api/yunlogger';
end;

destructor TDiocpYunLogger.Destroy;
begin
  FLogDataQueue.DisposeAllData;
  FreeAndNil(FASync);
  FreeAndNil(FHttpClient);
  FreeAndNil(FLogDataQueue);
  inherited Destroy;
end;

procedure TDiocpYunLogger.InnerDoLog(const s:string);
begin
  try
    FHttpClient.RequestContentType := 'text/json;chartset=utf-8';
    FHttpClient.RequestBody.Clear;
    FHttpClient.SetRequestBodyAsString(s, True);
    FHttpClient.Post(FYunServerURL);
  except
    on e:Exception do
    begin
          
    end;
  end;
end;

procedure TDiocpYunLogger.InnerWorker(pvASyncWorker: TASyncWorker);
var
  s:string;
  lvData:Pointer;
begin
  while not pvASyncWorker.Terminated do
  begin
    if FLogDataQueue.DeQueue(lvData) then
    begin
      s := GetStringFromPString(lvData);
      Dispose(lvData);
      InnerDoLog(s);
    end else
    begin
      FASync.WaitForSleep(1000);
    end;
  end;
end;

procedure TDiocpYunLogger.LogMessage(const pvAccessToken, pvMsg: string; const
    pvType: string = STRING_EMPTY; const pvLevel: string = STRING_EMPTY);
var
  lvData:String;
  lvJSON:TDValue;
begin

  if FLogDataQueue.Size > 5000 then
  begin
    PrintDebugString('QueueLogger队列超过5000, 日志将被丢弃');
  end;

  lvJSON := TDValue.Create();
  try
    lvJSON.Add('accesstoken', pvAccessToken);
    lvJSON.Add('msg', pvMsg);
    lvJSON.Add('type', pvType);
    lvJSON.Add('level', pvLevel);
    lvJSON.Add('time', NowString);
    lvData := JSONEncode(lvJSON, True, False);
  finally
    lvJSON.Free;
  end; 

  FLogDataQueue.EnQueue(NewPString(lvData));
end;

procedure TDiocpYunLogger.SetYunServer(pvURL:string);
begin
  FYunServerURL := pvURL;
end;

procedure TDiocpYunLogger.Start;
begin
  FASync.Start(InnerWorker);
end;

procedure TDiocpYunLogger.Stop;
begin
  FASync.Terminate;
  FASync.WaitForStop;
end;

initialization
  InitalizeYunLogger;

finalization
  FinalizeYunLogger;

end.
