unit DiocpStreamProtocol;


interface

uses
  Classes, SysUtils;

const
  MAX_BLOCK_SIZE = 1024 * 1;   // 500K
  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。
  SYNC_PACK_FLAG = $D10;
  //PACK_FLAG  + CRC_VALUE + STREAM_LEN + STREAM_DATA

type
  TDiocpStreamObject = class;
  TDiocpStreamObjectEvent = procedure(pvRawObject:TDiocpStreamObject) of object;
  TDiocpStreamObject = class(TObject)
  private
    FContent: TMemoryStream;
    FHeader:array[0..9] of Byte;
    FHeaderLen:Byte;
    FContentLength: Integer;
    FCrcValue:Integer;

    FInnerContent: TMemoryStream;
    FDecodeFlag : Integer;
    FReadPosition: Int64;
  protected
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure ResetReadPosition;
    function ReadBlock(pvBuf: PByte; pvLength: Cardinal): Integer;
    procedure WrapContent(pvStream: TMemoryStream);
    function InputBuffer(pvData: Byte): Integer;
    property Content: TMemoryStream read FContent;
    property ContentLength: Integer read FContentLength;
  end;

implementation

uses
  utils.byteTools;

constructor TDiocpStreamObject.Create;
begin
  inherited;
  FInnerContent := TMemoryStream.Create;
  FContent := FInnerContent;
  FHeaderLen := 0;
end;

destructor TDiocpStreamObject.Destroy;
begin
  FInnerContent.Free;
  inherited;
end;

procedure TDiocpStreamObject.Clear;
begin
  FInnerContent.Clear;
  if FContent <> nil then FContent.Clear; 
  FDecodeFlag := 0;
  FHeaderLen := 0;
  FReadPosition := 0; 
end;

function TDiocpStreamObject.ReadBlock(pvBuf: PByte; pvLength: Cardinal):
    Integer;
var
  lvBytes:TBytes;
  l, lvCrc:Integer;
begin
  if pvLength < 1024 then raise Exception.Create('Buffer长度必须大于1024');
  
  if FReadPosition = 0 then
  begin
    lvCrc := TByteTools.verifyData(FContent.Memory^, FContent.Size);
    PWord(@FHeader[0])^ := SYNC_PACK_FLAG;
    PInteger(@FHeader[2])^ := lvCrc;
    PInteger(@FHeader[6])^ := TByteTools.swap32(FContent.Size);
    Move(FHeader[0], pvBuf^, 10);
    FReadPosition := pvLength - 10;
    if FReadPosition > FContent.Size then FReadPosition := FContent.Size;
    FContent.Position := 0;
    Inc(pvBuf, 10);
    FReadPosition := FContent.Read(pvBuf^, FReadPosition);
    Result := FReadPosition + 10;
    if FReadPosition = 0 then FReadPosition := -1;  //没有数据了
  end else if FReadPosition = -1 then
  begin
    Result := 0;
  end else
  begin
    FContent.Position := FReadPosition;
    Result := FContent.Read(pvBuf^, pvLength);
    FReadPosition := FContent.Position;
  end;
end;

function TDiocpStreamObject.InputBuffer(pvData: Byte): Integer;
var
  lvBytes:TBytes;
  lvIntValue, lvCrcValue:Integer;
begin
  if FDecodeFlag = 0 then
  begin
    FHeader[FHeaderLen] := pvData;
    Inc(FHeaderLen);

    if FHeaderLen = 1 then
    begin
      if pvData <> $10 then
      begin
        Clear;
        Result := -1;
        Exit;
      end;
      Result := 0;
      Exit;
    end;

    if FHeaderLen = 2 then
    begin
      if pvData <> $0D then
      begin
        Clear;
        Result := -1;
        Exit;
      end;
      Result := 0;
      Exit;
    end;

    if FHeaderLen < 10 then
    begin
      Result := 0;
      Exit;
    end;

    FCrcValue := PInteger(@FHeader[2])^;
    lvIntValue := PInteger(@FHeader[6])^;
    FContentLength := TByteTools.swap32(lvIntValue);
    if FContentLength > MAX_OBJECT_SIZE then
    begin
      Result := -2;
      Exit;
    end;
    FDecodeFlag := 1;  //数据流
    if FContentLength = 0 then
    begin
      Result := 1;
      Exit;
    end else
    begin
      Result := 0;
      Exit;
    end;
  end;

  if FDecodeFlag = 1 then
  begin
    FContent.WriteBuffer(pvData, 1);
    if FContent.Size = FContentLength then
    begin
      lvCrcValue := TByteTools.verifyData(TMemoryStream(FContent).Memory^, FContent.Size);
      if lvCrcValue = FCrcValue then
      begin
        Result :=1;
      end else
      begin
        Result := -1;
        Clear;
      end;
      Exit;
    end else
    begin
      Result := 0;
      Exit;
    end;
  end;
end;

procedure TDiocpStreamObject.ResetReadPosition;
begin
  FReadPosition := 0;
end;

procedure TDiocpStreamObject.WrapContent(pvStream: TMemoryStream);
begin
  FContent := pvStream;
end;

end.
