unit utils_websocket;


interface

uses
  utils_strings, SysUtils, utils_base64, utils_byteTools;

const
  // 服务端使用
  MHSTR: string = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  OPT_CONTINUE:BYTE = $00;
  OPT_TEXT:Byte     = $01;
  OPT_BINARY:BYTE   = $02;
  OPT_CLOSE: BYTE   = $08;
  OPT_PING:  BYTE   = $09;
  OPT_PONG:  BYTE   = $0A;

  // 首字节：最高位用于描述消息是否结束,如果为1则该消息为消息尾部,如果为零则还有后续数据包;
  FIN_EOF: BYTE = 1;

  WS_MSG_PONG: Word = $008A;
  WS_MSG_PING: Word = $0089;
  
//%x0 代表一个继续帧
//%x1 代表一个文本帧
//%x2 代表一个二进制帧
//%x3-7 保留用于未来的非控制帧
//%x8 代表连接关闭
//%x9 代表ping
//%xA 代表pong
//%xB-F 保留用于未来的控制帧



type
  TDiocpWebSocketFrame = class(TObject)
  private
    FFlag : Integer;
    FBuffer: TDBufferBuilder;
  private
    FContentLength: Int64;
    FHeadLength: Byte;
    FPlayload:Byte;
    function GetMaskState: Byte;
    function DecodeByte12: Integer;

    /// <summary>
    ///  使用Mask解码
    /// </summary>
    function DecodeWithMask: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function InputBuffer(const buf:Byte): Integer;

    procedure EncodeBuffer(const buf: Pointer; len: Int64; pvFIN: Boolean;
        pvOpcode: Byte);

    procedure DoCleanUp();

    function ContentBuffer: PByte;

    /// <summary>
    ///   获取OptCode
    ///   (解码完成成功)
    /// </summary>
    function GetOptCode: Byte;

    /// <summary>
    ///  获取FIN状态
    /// </summary>
    function GetFIN:Byte;

    function DecodeDataWithUtf8: string;
    property Buffer: TDBufferBuilder read FBuffer;
    property ContentLength: Int64 read FContentLength;
  end;



implementation

constructor TDiocpWebSocketFrame.Create;
begin
  inherited Create;
  FBuffer := TDBufferBuilder.Create();
end;

function TDiocpWebSocketFrame.GetFIN: Byte;
begin
  Result := TByteTools.GetBit(FBuffer.MemoryBuffer(0)^, 7);
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
  Result := Utf8BufferToString(ContentBuffer, FContentLength);
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
  for I := 0 to FContentLength - 1 do
  begin
    lvTmpMask := lvMask;
    Inc(lvTmpMask, I mod 4);

    PByte(P)^ := Ord(P^) xor lvTmpMask^;
    Inc(P);
  end;
  Result := 0;      
end;

procedure TDiocpWebSocketFrame.EncodeBuffer(const buf: Pointer; len: Int64;
    pvFIN: Boolean; pvOpcode: Byte);
begin
  FBuffer.Clear;
  FBuffer.Append(Byte($00));
  if pvFIN then
    TByteTools.SetBit(FBuffer.MemoryBuffer(0)^, 7, 1);
  TByteTools.SetLow4Bit(FBuffer.MemoryBuffer(0)^, pvOpcode);

  // 写入长度
  if len < 126 then
    FBuffer.Append(Byte(Len))
  else if Len <= $FFFF then begin
    FBuffer.Append(Byte(126));
    FBuffer.Append(TByteTools.swap16(Word(len)));
  end else begin
    FBuffer.Append(Byte(127));
    FBuffer.Append(TByteTools.swap64(len));
  end;
  // 写入Buf
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
    FContentLength := 0;
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
    if (FPlayload = 126) then
    begin  // 解码长度
      FContentLength := TByteTools.swap16(PWord(FBuffer.MemoryBuffer(2))^);//  Ord(SrcData[2]) shl 8 + Ord(SrcData[3])
    end else if (FPlayload = 127) then
    begin
      FContentLength := TByteTools.swap64(PInt64(FBuffer.MemoryBuffer(2))^);  //   Swap64(PInt64(@SrcData[2])^);
    end;
  end;

  if FBuffer.Length = (FHeadLength + FContentLength) then
  begin   // 完整数据
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
