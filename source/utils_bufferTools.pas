unit utils_bufferTools;

interface

uses
  SysUtils;


type
  // 25:XE5
  {$IF CompilerVersion<=25}
  IntPtr=Integer;
  {$IFEND}

  EOutOfBufferException = class(Exception);
  IBufferWriter = interface
    ['{71D25B1E-1662-4B89-AAD7-2E75E2FC248C}']
    procedure write(const buf; l: cardinal); stdcall;
  end;

  IBufferReader = interface
    ['{71D25B1E-1662-4B89-AAD7-2E75E2FC248C}']
    function read(var buf; l:cardinal): Integer; stdcall;
    function readInteger: Integer; stdcall;
  end; 

  TBaseBufferOperator = class(TInterfacedObject)
  private
    FIgnoreCheck:Boolean;
    FBufferData: Pointer;
    FRemainLen: cardinal;
    procedure checkBufferLen(l:Cardinal);
  public
    constructor create(ABufferData: Pointer; pvBufferLen: cardinal);
  end;

  TBufferWriter = class(TBaseBufferOperator, IBufferWriter)
  public
    procedure write(const buf; l: cardinal); stdcall;
  end;

  TBufferReader = class(TBaseBufferOperator, IBufferReader)
  public
    function read(var buf; l:cardinal): Integer; stdcall;
    function readInteger: Integer; stdcall;
  end;


implementation

uses
  Math;

procedure TBufferWriter.write(const buf; l: cardinal);
begin
  checkBufferLen(l);
  Move(buf, FBufferData^, l);
  FBufferData := Pointer(IntPtr(FBufferData) + Integer(l));
  if not FIgnoreCheck then FRemainLen := FRemainLen - l;
end;

procedure TBaseBufferOperator.checkBufferLen(l: Cardinal);
begin
  if l > FRemainLen then
    raise EOutOfBufferException.CreateFmt('可使用Buffer长度为[%d]', [FRemainLen]);
end;

constructor TBaseBufferOperator.create(ABufferData: Pointer; pvBufferLen: cardinal);
begin
  inherited Create;
  FBufferData := ABufferData;
  FRemainLen := pvBufferLen;
  if pvBufferLen = 0 then
  begin
    FIgnoreCheck := true;
  end;
end;

function TBufferReader.read(var buf; l: cardinal): Integer;
begin
  Result := Min(l, FRemainLen); 
  Move(FBufferData^, buf, Result);
  FBufferData := Pointer(IntPtr(FBufferData) + l);
  if not FIgnoreCheck then FRemainLen := FRemainLen - l;
end;

function TBufferReader.readInteger: Integer;
begin
  checkBufferLen(SizeOf(Integer));
  read(Result, SizeOf(Integer));
end;

end.
