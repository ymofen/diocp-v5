unit uDIOCPStreamCoder;

interface

uses
  uIocpCoder, uBuffer, Classes, SysUtils, iocpTcpServer;

type
  TIOCPStreamDecoder = class(TIOCPDecoder)
  public
    /// <summary>
    ///   解码收到的数据,如果有接收到数据,调用该方法,进行解码
    /// </summary>
    /// <returns>
    ///   返回解码好的对象
    /// </returns>
    /// <param name="inBuf"> 接收到的流数据 </param>
    function Decode(const inBuf: TBufferLink; pvContext: TObject): TObject;
        override;
  end;


  TIOCPStreamEncoder = class(TIOCPEncoder)
  public
    /// <summary>
    ///   编码要发生的对象
    /// </summary>
    /// <param name="pvDataObject"> 要进行编码的对象 </param>
    /// <param name="ouBuf"> 编码好的数据 </param>
    procedure Encode(pvDataObject:TObject; const ouBuf: TBufferLink); override;
  end;

function verifyData(const buf; len:Cardinal): Cardinal;

implementation

uses
  uByteTools;

function verifyData(const buf; len: Cardinal): Cardinal;
var
  i:Cardinal;
  p:PByte;
begin
  i := 0;
  Result := 0;
  p := PByte(@buf);
  while i < len do
  begin
    Result := Result + p^;
    Inc(p);
    Inc(i);
  end;
end;

const
  PACK_FLAG = $D10;

  //PACK_FLAG  + CRC_VALUE + STREAM_LEN + STREAM_DATA

  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。



function TIOCPStreamDecoder.Decode(const inBuf: TBufferLink; pvContext:
    TObject): TObject;
var
  lvValidCount, lvReadL:Integer;
  lvPACK_FLAG:Word;
  lvDataLen: Integer;
  lvVerifyValue, lvVerifyDataValue:Cardinal;
begin
  Result := nil;

  //如果缓存中的数据长度不够包头长度，
  lvValidCount := inBuf.validCount;   //pack_flag + head_len + buf_len
  if (lvValidCount < SizeOf(Word) + SizeOf(Integer)) then
  begin
    Exit;
  end;

  //记录读取位置
  inBuf.markReaderIndex;
  inBuf.readBuffer(@lvPACK_FLAG, 2);

  if lvPACK_FLAG <> PACK_FLAG then
  begin
    //错误的包数据
    Result := TObject(-1);
    exit;
  end;

  //veri value
  inBuf.readBuffer(@lvVerifyValue, SizeOf(lvVerifyValue));

  //headlen
  inBuf.readBuffer(@lvReadL, SizeOf(lvReadL));
  lvDataLen := TByteTools.swap32(lvReadL);

  if lvDataLen > 0 then
  begin
    //文件头不能过大
    if lvDataLen > MAX_OBJECT_SIZE  then
    begin
      Result := TObject(-1);
      exit;
    end;

    if inBuf.validCount < lvDataLen then
    begin
      //返回buf的读取位置
      inBuf.restoreReaderIndex;
      exit;
    end;

    Result := TMemoryStream.Create;
    TMemoryStream(Result).SetSize(lvDataLen);
    inBuf.readBuffer(TMemoryStream(Result).Memory, lvDataLen);
    TMemoryStream(Result).Position := 0;

    lvVerifyDataValue := verifyData(TMemoryStream(Result).Memory^, lvDataLen);

    if lvVerifyValue <> lvVerifyDataValue then
    begin
      Result.Free;
      Result := TObject(-2);
    end;  
  end else
  begin
    Result := nil;
  end;
end;

{ TIOCPStreamEncoder }

procedure TIOCPStreamEncoder.Encode(pvDataObject: TObject;
  const ouBuf: TBufferLink);
var
  lvPACK_FLAG: WORD;
  lvDataLen, lvWriteIntValue: Integer;
  lvBuf: TBytes;
  lvVerifyValue:Cardinal;
begin
  lvPACK_FLAG := PACK_FLAG;

  TStream(pvDataObject).Position := 0;

  if TStream(pvDataObject).Size > MAX_OBJECT_SIZE then
  begin
    raise Exception.CreateFmt('数据包太大,请在业务层分拆发送,最大数据包[%d]!', [MAX_OBJECT_SIZE]);
  end;




  //pack_flag
  ouBuf.AddBuffer(@lvPACK_FLAG, 2);

  //
  lvDataLen := TStream(pvDataObject).Size;
  // stream data
  SetLength(lvBuf, lvDataLen);
  TStream(pvDataObject).Read(lvBuf[0], lvDataLen);

  //veri value
  lvVerifyValue := verifyData(lvBuf[0], lvDataLen);

  ouBuf.AddBuffer(@lvVerifyValue, SizeOf(lvVerifyValue));


  // data_len
  lvWriteIntValue := TByteTools.swap32(lvDataLen);

  // stream len
  ouBuf.AddBuffer(@lvWriteIntValue, SizeOf(lvWriteIntValue));



  // stream
  ouBuf.AddBuffer(@lvBuf[0], lvDataLen);  
end;

end.
