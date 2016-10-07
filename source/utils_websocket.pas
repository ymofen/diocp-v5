unit utils_websocket;

interface

uses
  utils_strings, SysUtils, SHA, utils_base64, utils_byteTools;

const
  MHSTR: AnsiString = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  OPT_CONTINUE:BYTE = $00;
  OPT_TEXT:Byte     = $01;
  OPT_BINARY:BYTE   = $02;
  OPT_CLOSE: BYTE   = $08;
  OPT_PING:  BYTE   = $09;
  OPT_PONG:  BYTE   = $0A;

  WS_MSG_PONG: Word = $008A;
  WS_MSG_PING: Word = $0089;
  
//%x0 ����һ������֡
//%x1 ����һ���ı�֡
//%x2 ����һ��������֡
//%x3-7 ��������δ���ķǿ���֡
//%x8 �������ӹر�
//%x9 ����ping
//%xA ����pong
//%xB-F ��������δ���Ŀ���֡



type
  TDiocpWebSocketFrame = class(TObject)
  private
    FDecodeState:Integer;
    FFlag : Integer;
    FBuffer: TDBufferBuilder;
    FContentLength:Integer;
  private
    FDataLength:Int64;
    FHeadLength: Byte;
    FPlayload:Byte;
    function GetMaskState: Byte;
    function DecodeByte12: Integer;

    /// <summary>
    ///  ʹ��Mask����
    /// </summary>
    function DecodeWithMask: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function InputBuffer(const buf:Byte): Integer;

    procedure EncodeBuffer(const buf: Pointer; len: Int64; pvFIN: Boolean;
        pvOpcode: Byte);

    procedure DoCleanUp();

    function DataBuffer: PByte;

    /// <summary>
    ///   ��ȡOptCode
    ///   (������ɳɹ�)
    /// </summary>
    function GetOptCode: Byte;

    function DecodeDataWithUtf8: string;
    property Buffer: TDBufferBuilder read FBuffer;
  end;

function GetWebSocketAccept(pvWebSocketKey:AnsiString): AnsiString;

implementation

function GetWebSocketAccept(pvWebSocketKey:AnsiString): AnsiString;
var
  Key: AnsiString;
  Bin: TBytes;
begin
  Key := pvWebSocketKey + MHSTR;
  Bin := SHA1Bin(Key);
  Result := Base64Encode(@Bin[0], Length(Bin));
end;

constructor TDiocpWebSocketFrame.Create;
begin
  inherited Create;
  FBuffer := TDBufferBuilder.Create();
end;

function TDiocpWebSocketFrame.GetMaskState: Byte;
begin
  Result := TByteTools.GetBit(FBuffer.MemoryBuffer(1)^, 7);
end;

destructor TDiocpWebSocketFrame.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

function TDiocpWebSocketFrame.DataBuffer: PByte;
begin
  Result := FBuffer.MemoryBuffer(FHeadLength);
end;

procedure TDiocpWebSocketFrame.DoCleanUp;
begin
  FFlag := 0;
end;

function TDiocpWebSocketFrame.DecodeByte12: Integer;
begin
  FHeadLength := 2;
  FPlayload := FBuffer.MemoryBuffer(1)^ and $7F;
  if FPlayload < 126 then
  begin
    FDataLength := FPlayload;
  end else if FPlayload = 126 then
  begin
    FHeadLength := FHeadLength + 2;
  end else if FPlayload = 127 then
  begin
    FHeadLength := FHeadLength + 8;
  end;

  if GetMaskState = 1 then
  begin
    FHeadLength := FHeadLength + 4;
  end;
      
  Result := 0;
end;

function TDiocpWebSocketFrame.DecodeDataWithUtf8: string;
begin
  Result := Utf8BufferToString(DataBuffer, FDataLength);
end;

function TDiocpWebSocketFrame.DecodeWithMask: Integer;
var
  lvMask, lvTmpMask:PByte;
  P:PByte;
  I:Integer;
begin
  lvMask := FBuffer.MemoryBuffer(FHeadLength - 4);
  P := lvMask;
  Inc(P, 4);
  for I := 0 to FDataLength - 1 do
  begin
    lvTmpMask := lvMask;
    Inc(lvTmpMask, I mod 4);

    PByte(P)^ := Ord(P^) xor lvTmpMask^;
    Inc(P);
  end;      
end;

procedure TDiocpWebSocketFrame.EncodeBuffer(const buf: Pointer; len: Int64;
    pvFIN: Boolean; pvOpcode: Byte);
begin
  FBuffer.Clear;
  FBuffer.Append(Byte($00));
  if pvFIN then
    TByteTools.SetBit(FBuffer.MemoryBuffer(0)^, 7, 1);
  TByteTools.SetLow4Bit(FBuffer.MemoryBuffer(0)^, pvOpcode);

  // д�볤��
  if len < 126 then
    FBuffer.Append(Byte(Len))
  else if Len <= $FFFF then begin
    FBuffer.Append(Byte(126));
    FBuffer.Append(TByteTools.swap16(Word(len)));
  end else begin
    FBuffer.Append(Byte(127));
    FBuffer.Append(TByteTools.swap64(len));
  end;
  // д��Buf
  if Len > 0 then
    FBuffer.AppendBuffer(PByte(buf), Len);
end;

function TDiocpWebSocketFrame.GetOptCode: Byte;
begin
  Result := TByteTools.GetLow4Bit(FBuffer.MemoryBuffer(0)^);
end;

function TDiocpWebSocketFrame.InputBuffer(const buf:Byte): Integer;
begin
  if FFlag = 0 then
  begin
    FFlag := 1;
    FBuffer.Clear;
    FDataLength := 0;
  end;

  FBuffer.Append(buf);

  if FBuffer.Length < 2 then
  begin
    Result := 0;
    Exit;
  end;

  if FBuffer.Length = 2  then
  begin
    DecodeByte12;
  end;

  if FBuffer.Length < FHeadLength then
  begin
    Result := 0; 
    Exit;
  end else if (FBuffer.Length = FHeadLength) then
  begin 
    if (FPlayload = 126) and (FBuffer.Length = 4) then
    begin  // ���볤��
      FDataLength := TByteTools.swap16(FBuffer.MemoryBuffer(2)^);//  Ord(SrcData[2]) shl 8 + Ord(SrcData[3])
    end else if (FPlayload = 127) and (FBuffer.Length = 10) then
    begin
      FDataLength := TByteTools.swap64(PInt64(FBuffer.MemoryBuffer(2))^);  //   Swap64(PInt64(@SrcData[2])^);
    end;
  end;

  if FBuffer.Length = (FHeadLength + FDataLength) then
  begin   // ��������
    FFlag := 0; 
    if GetMaskState = 1 then
    begin
      DecodeWithMask;
      Result := 1;
      Exit;
    end else
    begin
      Result := 1;
      Exit;
    end;

  end;

  Result := 0;

end;

end.