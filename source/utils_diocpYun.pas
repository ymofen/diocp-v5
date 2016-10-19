unit utils_diocpYun;

interface

uses
  utils_dvalue, diocp_ex_httpClient, SysUtils, utils_async, utils_queues,
  utils_strings;

type
  TDiocpYunStorage = class(TObject)
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
    procedure SetValue(const pvAccessToken, pvPath, pvValue: string);
    procedure SetJSON(const pvAccessToken, pvPath: string; const pvValue: TDValue);overload;
    procedure SetJSON(const pvAccessToken, pvPath: string; const pvJSON: String);
        overload;
  end;

var
  yunStorage:TDiocpYunStorage;

procedure FinalizeYunStorage;
procedure InitalizeYunStorage;

implementation

uses
  utils_dvalue_json;

procedure FinalizeYunStorage;
begin
  if yunStorage <> nil then
  begin
    yunStorage.Stop;
    yunStorage.Free;
    yunStorage := nil;
  end;
end;

procedure InitalizeYunStorage;
begin
  if yunStorage = nil then
  begin
    yunStorage := TDiocpYunStorage.Create;
    yunStorage.Start;
  end;  
end;

constructor TDiocpYunStorage.Create;
begin
  inherited Create;
  FHttpClient := TDiocpHttpClient.Create(nil);
  FASync := TASyncInvoker.Create();
  FLogDataQueue := TSafeQueue.Create();
  FYunServerURL := 'http://123.232.98.202:9007/api/yunStorageWriter';
end;

destructor TDiocpYunStorage.Destroy;
begin
  FLogDataQueue.DisposeAllData;
  FreeAndNil(FASync);
  FreeAndNil(FHttpClient);
  FreeAndNil(FLogDataQueue);
  inherited Destroy;
end;

procedure TDiocpYunStorage.InnerDoLog(const s:string);
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

procedure TDiocpYunStorage.InnerWorker(pvASyncWorker: TASyncWorker);
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

procedure TDiocpYunStorage.SetValue(const pvAccessToken, pvPath, pvValue:
    string);
var
  lvData:String;
  lvJSON:TDValue;
begin

  if FLogDataQueue.Size > 5000 then
  begin
    PrintDebugString('YunStorage队列超过5000, 将被丢弃');
  end;

  lvJSON := TDValue.Create();
  try
    lvJSON.Add('accesstoken', pvAccessToken);
    lvJSON.Add('path', pvPath);
    lvJSON.Add('value', pvValue);
    lvData := JSONEncode(lvJSON, True, False);
  finally
    lvJSON.Free;
  end; 

  FLogDataQueue.EnQueue(NewPString(lvData));
end;

procedure TDiocpYunStorage.SetJSON(const pvAccessToken, pvPath: string; const
    pvValue: TDValue);
var
  lvData:String;
  lvJSON:TDValue;
begin
  if FLogDataQueue.Size > 5000 then
  begin
    PrintDebugString('YunStorage队列超过5000, 将被丢弃');
  end;

  lvJSON := TDValue.Create();
  try
    lvJSON.Add('accesstoken', pvAccessToken);
    lvJSON.Add('path', pvPath);
    lvJSON.AttachDValue('value', pvValue.Clone());
    lvData := JSONEncode(lvJSON, True, False);
  finally
    lvJSON.Free;
  end;    
  FLogDataQueue.EnQueue(NewPString(lvData));
end;

procedure TDiocpYunStorage.SetJSON(const pvAccessToken, pvPath: string; const
    pvJSON: String);
var
  lvValue:TDValue;
begin
  lvValue := TDValue.Create();
  try
    JSONParser(pvJSON, lvValue);
    SetJSON(pvAccessToken, pvPath, lvValue);
  finally
    lvValue.Free;
  end;


end;

procedure TDiocpYunStorage.SetYunServer(pvURL:string);
begin
  FYunServerURL := pvURL;
end;

procedure TDiocpYunStorage.Start;
begin
  FASync.Start(InnerWorker);
end;

procedure TDiocpYunStorage.Stop;
begin
  FASync.Terminate;
  FASync.WaitForStop;
end;

initialization
  InitalizeYunStorage;

finalization
  FinalizeYunStorage;

end.
