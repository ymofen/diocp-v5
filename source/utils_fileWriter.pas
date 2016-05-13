unit utils_fileWriter;

interface

uses
  Classes, utils_strings, SysUtils;

type
  /// <summary>
  ///   单文件 写入
  ///     用于单线程写入数据
  /// </summary>
  TSingleFileWriter = class(TObject)
  private
    FFileStream: TFileStream;
    FLastFileID:String;
    FWriter: TWriter;
    FFileSN:Integer;
    FBasePath: string;
    FCacheSize: Integer;
    FFilePreFix: String;
    FInitialized: Boolean;
    FFilePerSize: Integer;
    procedure CheckInitialized;
    function OpenLogFile(pvPre: String = ''): Boolean;
    procedure SetCacheSize(const Value: Integer);
    procedure SetFilePerSize(const Value: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteString(pvData: string);
    procedure WriteBuffer(pvBuf: Pointer; pvBufLen: Integer);
    procedure LogMessage(pvData:string); overload;
    procedure Flush;
    procedure LogMessage(pvFmtMsg: string; const args: array of const); overload;
    property BasePath: string read FBasePath write FBasePath;
    property CacheSize: Integer read FCacheSize write SetCacheSize;
    property FilePerSize: Integer read FFilePerSize write SetFilePerSize;
    property FilePreFix: String read FFilePreFix write FFilePreFix;
  end;

implementation

procedure FreeObject(AObject: TObject);
begin
{$IFDEF AUTOREFCOUNT}
  AObject.DisposeOf;
{$ELSE}
  AObject.Free;
{$ENDIF}
end;

constructor TSingleFileWriter.Create;
begin
  inherited Create;
  FBasePath :=ExtractFilePath(ParamStr(0)) + 'log\';
  FFileSN := 0;
  FFilePerSize := 1024 * 1024 * 50;
  FCacheSize := 0;
  FLastFileID := '';
end;


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

destructor TSingleFileWriter.Destroy;
begin
  if FFileStream <> nil then
  begin
    FWriter.FlushBuffer;
    FFileStream.Free;
  end;
  inherited Destroy;
end;

procedure TSingleFileWriter.WriteBuffer(pvBuf: Pointer; pvBufLen: Integer);
begin
  try
    CheckInitialized;
    if OpenLogFile(FFilePreFix) then
    begin
      FWriter.Write(pvBuf^, pvBufLen);
      if FCacheSize <= 0 then Flush;
    end;
  except
  end;                              
end;

procedure TSingleFileWriter.WriteString(pvData: string);
var
  lvBytes:TBytes;
begin
  try
    CheckInitialized;
    if OpenLogFile(FFilePreFix) then
    begin
      lvBytes := StringToUtf8Bytes(pvData);
      FWriter.Write(lvBytes[0], Length(lvBytes));
      if FCacheSize <= 0 then Flush;
    end;
  except
  end;
end;

procedure TSingleFileWriter.CheckInitialized;
begin
  if FInitialized then exit;
  if not DirectoryExists(FBasePath) then ForceDirectories(FBasePath);
  FInitialized := true;
end;

procedure TSingleFileWriter.Flush;
begin
  FWriter.FlushBuffer;
end;

procedure TSingleFileWriter.LogMessage(pvData:string);
begin
  WriteString(FormatDateTime('MM-dd:hh:nn:ss.zzz', Now()) + ',' + pvData);
end;

function TSingleFileWriter.OpenLogFile(pvPre: String = ''): Boolean;
var
  lvFileName, lvNewFileName, lvNewFileID:String;
  lvFileSize: Integer;
  procedure CheckNewFile();
  begin
    lvNewFileID := FormatDateTime('YY.M.D', Now());
    if FFileStream <> nil then
    begin
      if FFileStream.Size >= (FFilePerSize) then
      begin  // 10M
        FWriter.FlushBuffer;
        FWriter.Free;
        FreeObject(FFileStream);
        FFileStream := nil;
      end else if lvNewFileID <> FLastFileID then
      begin
        FWriter.FlushBuffer;
        FWriter.Free;
        FreeObject(FFileStream);
        FFileStream := nil;
      end;
    end;

    if FFileStream = nil then
    begin
      lvFileName := CreateUniqueFileName(FBasePath,
        pvPre + lvNewFileID, FFileSN, '.log');
      FFileStream := TFileStream.Create(lvFileName, fmCreate);
      FreeObject(FFileStream);

      // 独占写入
      FFileStream := TFileStream.Create(lvFileName,
        fmOpenWrite or fmShareDenyWrite);

      if FCacheSize <= 0 then
      begin
        FWriter := TWriter.Create(FFileStream, 1024);
      end else
      begin
        FWriter := TWriter.Create(FFileStream, FCacheSize);
      end;

      FLastFileID := lvNewFileID;
    end;
  end;
begin
  Result := False;
  try
    CheckNewFile;
    Result := true;
  except
    on E:Exception do
    begin
      // SafeWriteFileMsg('创建日志文件发送异常:' + e.Message, '');
    end;

  end;
end;

procedure TSingleFileWriter.SetCacheSize(const Value: Integer);
begin
  FCacheSize := Value;
end;

procedure TSingleFileWriter.SetFilePerSize(const Value: Integer);
begin
  if Value <= 0 then
  begin
    FFilePerSize := 1024 * 1024 * 50;  //
  end else
  begin
    FFilePerSize := Value;
  end;  
  
end;

procedure TSingleFileWriter.LogMessage(pvFmtMsg: string; const args: array of
    const);
begin
  LogMessage(Format(pvFmtMsg, args));
end;

end.
