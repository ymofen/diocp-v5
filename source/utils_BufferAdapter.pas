unit utils_BufferAdapter;

interface

uses
  Classes, SysUtils;

type
  {$IF CompilerVersion<=25}
  IntPtr=Integer;
  {$IFEND}
  TBufferAdapterStream = class(TStream)
  private
    FMaxSize:Integer;
    FMemory: Pointer;
    FSize, FPosition: Int64;
  protected
    procedure SetPointer(Ptr: Pointer; Size: Longint);
  public
    constructor Create(pvBuffer:Pointer; pvSize:Integer);
    function Read(var Buffer; Count: Longint): Longint; override;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Memory: Pointer read FMemory;
  end;

implementation

constructor TBufferAdapterStream.Create(pvBuffer:Pointer; pvSize:Integer);
begin
  inherited Create;
  SetPointer(pvBuffer, pvSize);
  FMaxSize := pvSize;
end;

function TBufferAdapterStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      Move(Pointer(IntPtr(FMemory) + FPosition)^, Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

procedure TBufferAdapterStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBufferAdapterStream.SaveToStream(Stream: TStream);
begin
  if FSize <> 0 then Stream.WriteBuffer(FMemory^, FSize);
end;

function TBufferAdapterStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

{ TBufferAdapterStream }

procedure TBufferAdapterStream.SetPointer(Ptr: Pointer; Size: Longint);
begin
  FMemory := Ptr;
  FSize := Size;
end;

procedure TBufferAdapterStream.SetSize(NewSize: Longint);
var
  OldPosition: Longint;
begin
  if NewSize > FMaxSize then
    raise Exception.Create('TBufferAdapterStream::can not resize');
    
  OldPosition := FPosition;
  if OldPosition > NewSize then Seek(0, soFromEnd);
  FSize := NewSize;
end;

function TBufferAdapterStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Longint;
begin
  if (Position >= 0) and (Count >= 0) then
  begin
    Pos := Position + Count;
    if Pos > 0 then
    begin
      if Pos > Size then
      begin
        if Pos > FMaxSize then raise Exception.Create('TBufferAdapterStream::overflow');
      end;
      System.Move(Buffer, Pointer(IntPtr(FMemory) + FPosition)^, Count);
      FPosition := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

end.
