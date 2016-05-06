unit SingleLogFileAppender4SafeLogger;

interface

uses
  utils_safeLogger, SysUtils, utils_dvalue, Classes, utils_strings;

type
  TSingleLogFileAppender4SafeLogger = class(TBaseAppender)
  private
    FFileStream: TFileStream;
    FWriter: TWriter;
    FFileSN:Integer;
    FBasePath: string;
    FCacheSize: Integer;
    FFilePreFix: String;
    FInitialized: Boolean;
    FCurrentWriter: TWriter;
    FFilePerSize: Integer;
    FOutputToConsole: Boolean;
    FWriteFile: Boolean;
    procedure CheckInitialized;
    function OpenLogFile(pvPre: String = ''): Boolean;
    procedure SetFilePerSize(const Value: Integer);
  protected
    procedure AppendLog(pvData:TLogDataObject); override;
    procedure NotifyOnceEnd(pvCounter:Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    property BasePath: string read FBasePath write FBasePath;
    property CacheSize: Integer read FCacheSize write FCacheSize;
    property FilePerSize: Integer read FFilePerSize write SetFilePerSize;
    property FilePreFix: String read FFilePreFix write FFilePreFix;

    property OutputToConsole: Boolean read FOutputToConsole write FOutputToConsole;
    property WriteFile: Boolean read FWriteFile write FWriteFile;

  end;

implementation

function CanAccess(AFileName: string): Boolean;
var
  AHandle: THandle;
begin
  AHandle := FileOpen(AFileName, fmOpenReadWrite);
  if AHandle = THandle(-1) then
    Result := False
  else
  begin
    Result := true;
    FileClose(AHandle);
  end;
end;

function CreateUniqueFileName(pvBasePath, pvPreFix: String; vStartSN: Integer;
    pvExt: string): string;
var
  lvFileName:String;
begin
  while True do
  begin
    lvFileName := pvBasePath + pvPreFix + '_' + IntToStr(vStartSN) + pvExt;
    if FileExists(lvFileName) then
    begin
      Inc(vStartSN);
    end else
    begin
      Result := lvFileName;
      Break;
    end;
  end;
end;

constructor TSingleLogFileAppender4SafeLogger.Create;
begin
  inherited Create;
  FWriteFile := true;
  FBasePath :=ExtractFilePath(ParamStr(0)) + 'log\';
  FFileSN := 0;
  FFilePerSize := 1024 * 1024 * 80;
  FCacheSize := 1024 * 10;
end;

destructor TSingleLogFileAppender4SafeLogger.Destroy;
begin
  if FFileStream <> nil then
  begin
    FWriter.FlushBuffer;
    FWriter.Free;
    FFileStream.Free;
  end;

  inherited Destroy;
end;

procedure TSingleLogFileAppender4SafeLogger.AppendLog(pvData: TLogDataObject);
var
  lvMsg:String;
  lvPreFix :String;
  lvBytes:TBytes;
begin
  CheckInitialized;
  lvPreFix := FFilePreFix;

  if OpenLogFile(lvPreFix) then
  begin
    lvMsg := Format('%s,%s,%s,%s'#13#10,
        [FormatDateTime('hh:nn:ss:zzz', pvData.FTime)
          , TLogLevelCaption[pvData.FLogLevel]
          , pvData.FMsgType
          , pvData.FMsg
        ]
        );
    lvBytes := StringToUtf8Bytes(lvMsg);
    if FWriteFile then
    begin
      FWriter.Write(lvBytes[0], Length(lvBytes));
    end;

    if FOutputToConsole then
    begin
      if pvData.FLogLevel <> lgvWriteFile then
      begin
        Write(lvMsg);
      end;
    end;

  end else
  begin
    FOwner.incErrorCounter;
  end;
end;

procedure TSingleLogFileAppender4SafeLogger.CheckInitialized;
begin
  if FInitialized then exit;
  if not DirectoryExists(FBasePath) then ForceDirectories(FBasePath);
  FInitialized := true;
end;

procedure TSingleLogFileAppender4SafeLogger.NotifyOnceEnd(pvCounter:Integer);
begin
  inherited;
  if FCacheSize = 0 then
  begin
    FWriter.FlushBuffer;
  end;
end;

function TSingleLogFileAppender4SafeLogger.OpenLogFile(pvPre: String = ''): Boolean;
var
  lvFileName, lvNewFileName:String;
  lvFileSize: Integer;
  lvItem:TDValue;
begin
  Result := False;
  try
    if FFileStream = nil then
    begin
      lvFileName := CreateUniqueFileName(FBasePath,
        pvPre + FormatDateTime('YY.M.D', Now()), FFileSN, '.log');

      FFileStream := TFileStream.Create(lvFileName, fmCreate);
      FreeObject(FFileStream);
      
      FFileStream := TFileStream.Create(lvFileName,
        fmOpenWrite or fmShareDenyWrite);

      if FCacheSize > 0 then
        FWriter := TWriter.Create(FFileStream, FCacheSize)
      else
        FWriter := TWriter.Create(FFileStream, 1024);
    end else
    begin
      if FFileStream.Size >= (FFilePerSize) then
      begin  // 10M
        FWriter.FlushBuffer;
        FWriter.Free;
        FFileStream.Free;
        
        lvFileName := CreateUniqueFileName(FBasePath,
          pvPre + FormatDateTime('YY.M.D', Now()), FFileSN, '.log');

        FFileStream := TFileStream.Create(lvFileName, fmCreate);
        FreeObject(FFileStream);

        // 拒绝其他写入
        FFileStream := TFileStream.Create(lvFileName,
          fmOpenWrite or fmShareDenyWrite);

        if FCacheSize > 0 then
          FWriter := TWriter.Create(FFileStream, FCacheSize)
        else
          FWriter := TWriter.Create(FFileStream, 1024);
      end;
    end;
    Result := true;
  except
    on E:Exception do
    begin
      SafeWriteFileMsg('创建日志文件发送异常:' + e.Message, '');
    end;

  end;
end;

procedure TSingleLogFileAppender4SafeLogger.SetFilePerSize(const Value: Integer);
begin
  if Value <= 0 then
  begin
    FFilePerSize := 1024 * 1024 * 50;  //
  end else
  begin
    FFilePerSize := Value;
  end;

  
end;

end.
