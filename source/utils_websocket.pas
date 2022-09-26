unit utils_websocket;

interface

uses
  utils_strings, SysUtils, utils_base64, utils_byteTools;

const
  // �����ʹ��
  MHSTR: string = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  OPT_CONTINUE: BYTE = $00;
  OPT_TEXT: BYTE = $01;
  OPT_BINARY: BYTE = $02;
  OPT_CLOSE: BYTE = $08;
  OPT_PING: BYTE = $09;
  OPT_PONG: BYTE = $0A;

  // ���ֽڣ����λ����������Ϣ�Ƿ����,���Ϊ1�����ϢΪ��Ϣβ��,���Ϊ�����к������ݰ�;
  FIN_EOF: BYTE = 1;

  WS_MSG_PONG: Word = $008A;
  WS_MSG_PING: Word = $0089;

  // %x0 ����һ������֡
  // %x1 ����һ���ı�֡
  // %x2 ����һ��������֡
  // %x3-7 ��������δ���ķǿ���֡
  // %x8 �������ӹر�
  // %x9 ����ping
  // %xA ����pong
  // %xB-F ��������δ���Ŀ���֡

type
  TDiocpWebSocketFrame = class(TObject)
  private
    FFlag: Integer;
    FBuffer: TDBufferBuilder;
  private
    FContentLength: Int64;
    FContentMaxSize: Integer;
    FHeadLength: BYTE;
    FPlayload: BYTE;
    function GetMaskState: BYTE;
    function DecodeByte12: Integer;

    /// <summary>
    /// ʹ��Mask����
    /// </summary>
    function DecodeWithMask: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///
    /// </summary>
    /// <returns>
    /// -2: ����̫��
    /// </returns>
    function InputBuffer(const buf: BYTE): Integer;

    procedure EncodeBuffer(const buf: Pointer; len: Int64; pvFIN: Boolean; pvOpcode: BYTE; Masked: Boolean);

    procedure DoCleanUp();

    function ContentBuffer: PByte;

    /// <summary>
    /// ��ȡOptCode
    /// (������ɳɹ�)
    /// </summary>
    function GetOptCode: BYTE;

    /// <summary>
    /// ��ȡFIN״̬
    /// </summary>
    function GetFIN: BYTE;

    function DecodeDataWithUtf8: string;
    property Buffer: TDBufferBuilder read FBuffer;
    property ContentLength: Int64 read FContentLength;

    property ContentMaxSize: Integer read FContentMaxSize write FContentMaxSize;

  end;

function GetOptionCaption(pvOptCode: Integer): String;

implementation

function GetOptionCaption(pvOptCode: Integer): String;
begin

  if pvOptCode = OPT_PING then
    Result := 'PING'
  else if pvOptCode = OPT_PONG then
    Result := 'PONG'
  else if pvOptCode = OPT_BINARY then
    Result := 'BINARY'
  else if pvOptCode = OPT_TEXT then
    Result := 'TEXT'
  else
    Result := Format('%d-UNKOWN', [pvOptCode])

end;

constructor TDiocpWebSocketFrame.Create;
begin
  inherited Create;
  FBuffer := TDBufferBuilder.Create();
  FContentMaxSize := 1024 * 1024 * 10;
end;

function TDiocpWebSocketFrame.GetFIN: BYTE;
begin
  Result := TByteTools.GetBit(FBuffer.MemoryBuffer(0)^, 7);
end;

function TDiocpWebSocketFrame.GetMaskState: BYTE;
begin
  Result := TByteTools.GetBit(FBuffer.MemoryBuffer(1)^, 7);
end;

destructor TDiocpWebSocketFrame.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

function TDiocpWebSocketFrame.ContentBuffer: PByte;
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
  // $7F = 0111 1111
  FPlayload := FBuffer.MemoryBuffer(1)^ and $7F;
  if FPlayload < 126 then
  begin
    FContentLength := FPlayload;
  end
  else if FPlayload = 126 then
  begin
    FHeadLength := FHeadLength + 2;
  end
  else if FPlayload = 127 then
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
  Result := Utf8BufferToString(ContentBuffer, FContentLength);
end;

function TDiocpWebSocketFrame.DecodeWithMask: Integer;
var
  lvMask, lvTmpMask: PByte;
  P: PByte;
  I: Integer;
begin
  lvMask := FBuffer.MemoryBuffer(FHeadLength - 4);
  P := lvMask;
  Inc(P, 4);
  for I := 0 to FContentLength - 1 do
  begin
    lvTmpMask := lvMask;
    Inc(lvTmpMask, I mod 4);

    PByte(P)^ := Ord(P^) xor lvTmpMask^;
    Inc(P);
  end;
  Result := 0;
end;

procedure TDiocpWebSocketFrame.EncodeBuffer(const buf: Pointer; len: Int64; pvFIN: Boolean; pvOpcode: BYTE; Masked: Boolean);
var
  masking: LongWord;
  i64:Int64;
begin
  FBuffer.Clear;
  FBuffer.Append(BYTE($00));
  if pvFIN then
    TByteTools.SetBit(FBuffer.MemoryBuffer(0)^, 7, 1);
  TByteTools.SetLow4Bit(FBuffer.MemoryBuffer(0)^, pvOpcode);

  // д�볤��
  if len < 126 then
    FBuffer.Append(BYTE(len))
  else if len <= $FFFF then
  begin
    FBuffer.Append(BYTE(126));
    FBuffer.Append(TByteTools.swap16(Word(len)));
  end
  else
  begin
    FBuffer.Append(BYTE(127));
    i64 := TByteTools.swap64(len);
    FBuffer.AppendBuffer(@i64, 8);
  end;

  if (Masked) then
  begin
    TByteTools.SetBit(FBuffer.MemoryBuffer(1)^, 7, 1);
    masking := GetTickCount;
    FBuffer.AppendBuffer(@masking, 4);
  end;

  if len > 0 then
    FBuffer.AppendBuffer(PByte(buf), len);

  if (Masked) then
  begin
    DecodeByte12;
    DecodeWithMask;
  end;
end;

function TDiocpWebSocketFrame.GetOptCode: BYTE;
begin
  Result := TByteTools.GetLow4Bit(FBuffer.MemoryBuffer(0)^);
end;

function TDiocpWebSocketFrame.InputBuffer(const buf: BYTE): Integer;
begin
  if FFlag = 0 then
  begin
    FFlag := 1;
    FBuffer.Clear;
    FContentLength := 0;
  end;

  FBuffer.Append(buf);

  if FBuffer.Length < 2 then
  begin
    Result := 0;
    Exit;
  end;

  if FBuffer.Length = 2 then
  begin
    DecodeByte12;
  end;

  if FBuffer.Length < FHeadLength then
  begin
    Result := 0;
    Exit;
  end
  else if (FBuffer.Length = FHeadLength) then
  begin
    if (FPlayload = 126) then
    begin // ���볤��
      FContentLength := TByteTools.swap16(PWord(FBuffer.MemoryBuffer(2))^); // Ord(SrcData[2]) shl 8 + Ord(SrcData[3])
    end
    else if (FPlayload = 127) then
    begin
      FContentLength := TByteTools.swap64(PInt64(FBuffer.MemoryBuffer(2))^); // Swap64(PInt64(@SrcData[2])^);
    end;
    if FContentLength > FContentMaxSize then
    begin // ����10M ��С
      Result := -2;
      Exit;
    end;
  end;

  if FBuffer.Length > FContentMaxSize then
  begin
    Assert(FBuffer.Length < FContentMaxSize);
  end;

  if FBuffer.Length = (FHeadLength + FContentLength) then
  begin // ��������
    FFlag := 0;
    if GetMaskState = 1 then
    begin
      DecodeWithMask;
      Result := 1;
      Exit;
    end
    else
    begin
      Result := 1;
      Exit;
    end;
  end;

  Result := 0;

end;

end.
