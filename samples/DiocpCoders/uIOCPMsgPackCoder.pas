unit uIOCPMsgPackCoder;

interface

uses
  uIocpCoder, uBuffer, qmsgpack, Classes, 
  uZipTools, SysUtils, uMsgPackObject, uByteTools;

const
  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。

type
  TIOCPMsgPackEncoder = class(TIOCPEncoder)
  public
    /// <summary>
    ///   编码要发生的对象
    /// </summary>
    /// <param name="pvDataObject"> 要进行编码的对象 </param>
    /// <param name="ouBuf"> 编码好的数据 </param>
    procedure Encode(pvDataObject:TObject; const ouBuf: TBufferLink); override;
  end;


  TIOCPMsgPackDecoder = class(TIOCPDecoder)
  private

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

implementation

const
  PACK_FLAG = $0818;

  MAX_HEAD_LEN = 1024;

  //PACK_FLAG  + HEAD_LEN + HEAD_DATA + Buffer_LEN + Buffer_DATA

  //HEAD  zip(byte(0,1)), cmd.namespaceid(integer)
  //数据> 100K进行压缩




procedure TIOCPMsgPackEncoder.Encode(pvDataObject:TObject; const ouBuf:
    TBufferLink);
var
  lvMsgPack:TQMsgPack;
  lvBytes :SysUtils.TBytes;
  lvDataLen, lvWriteL: Integer;
  lvHeadlen, lvNameSpaceID: Integer;
  lvZiped:Byte;
  lvPACK_FLAG:Word;
begin
  if pvDataObject = nil then exit;
  lvMsgPack := TQMsgPack(pvDataObject);
  lvBytes := lvMsgPack.Encode;
  lvDataLen :=Length(lvBytes);

  if lvDataLen > 1024 * 100 then  // >100K 进行压缩
  begin
    lvBytes := SysUtils.TBytes(TZipTools.compressBuf(lvBytes[0], lvDataLen));
    lvDataLen := Length(lvBytes);
    lvZiped := 1;
  end else
  begin
    lvZiped := 0;   //未进行压缩
  end;

  if lvDataLen > MAX_OBJECT_SIZE then
    raise Exception.CreateFmt('数据包太大,请在业务层分拆发送,最大数据包[%d]!', [MAX_OBJECT_SIZE]);


  if lvMsgPack.ItemByPath('cmd.namespaceid') <> nil then
  begin
    lvNameSpaceID := lvMsgPack.ForcePath('cmd.namespaceid').AsInteger;
  end else
  begin
    lvNameSpaceID := 0;
  end;

  lvPACK_FLAG := PACK_FLAG;
  //pack_flag
  ouBuf.AddBuffer(@lvPACK_FLAG, 2);
  //Head_len: zip + namespaceid
  lvHeadlen := SizeOf(lvZiped) + SizeOf(lvNameSpaceID);

  lvWriteL := TByteTools.swap32(lvHeadlen);
  //head_len
  ouBuf.AddBuffer(@lvWriteL, SizeOf(lvWriteL));

  //zip
  ouBuf.AddBuffer(@lvZiped, SizeOf(lvZiped));
  //namesapceid
  ouBuf.AddBuffer(@lvNameSpaceID, SizeOf(lvNameSpaceID));

  //data_len
  lvWriteL := TByteTools.swap32(lvDataLen);
  ouBuf.AddBuffer(@lvWriteL, SizeOf(lvWriteL));
  //data
  ouBuf.AddBuffer(@lvBytes[0], lvDataLen);
end;

{ TIOCPMsgPackDecoder }

function TIOCPMsgPackDecoder.Decode(const inBuf: TBufferLink; pvContext:
    TObject): TObject;
var
  lvBytes, lvHeadBytes:SysUtils.TBytes;
  lvValidCount, lvReadL:Integer;
  lvPACK_FLAG:Word;
  lvDataLen: Integer;
  lvHeadlen, lvNameSpaceID: Integer;
  lvZiped:Byte;
  lvMsgPackObject:TMsgPackObject;
begin
  Result := nil;

  //如果缓存中的数据长度不够包头长度，
  lvValidCount := inBuf.validCount;   //pack_flag + head_len + buf_len
  if (lvValidCount < SizeOf(Word) + SizeOf(Integer) + SizeOf(Integer)) then
  begin
    Exit;
  end;

  //记录读取位置
  inBuf.markReaderIndex;
  //setLength(lvBytes, 2);
  inBuf.readBuffer(@lvPACK_FLAG, 2);

  if lvPACK_FLAG <> PACK_FLAG then
  begin
    //错误的包数据
    Result := TObject(-1);
    exit;
  end;

  //headlen
  inBuf.readBuffer(@lvReadL, SizeOf(lvReadL));
  lvHeadlen := TByteTools.swap32(lvReadL);

  if lvHeadlen > 0 then
  begin
    //文件头不能过大
    if lvHeadlen > MAX_HEAD_LEN  then
    begin
      Result := TObject(-1);
      exit;
    end;

    if inBuf.validCount < lvHeadlen then
    begin
      //返回buf的读取位置
      inBuf.restoreReaderIndex;
      exit;
    end;
    
    //head
    setLength(lvHeadBytes, lvHeadlen);
    inBuf.readBuffer(@lvHeadBytes[0], lvHeadlen);
  end else if lvHeadlen < 0 then
  begin
    //错误的包数据
    Result := TObject(-1);
    exit;
  end;

  //buf_len
  inBuf.readBuffer(@lvReadL, SizeOf(lvReadL));
  lvDataLen := TByteTools.swap32(lvReadL);

  ///如果数据过大，
  if (lvDataLen > MAX_OBJECT_SIZE)  then
  begin
    //错误的包数据
    Result := TObject(-1);
    exit;
  end;
  

  //如果缓存中的数据不够json的长度和流长度<说明数据还没有收取完毕>解码失败
  lvValidCount := inBuf.validCount;
  if lvValidCount < (lvDataLen) then
  begin
    //返回buf的读取位置
    inBuf.restoreReaderIndex;
    exit;
  end;

  lvMsgPackObject := TMsgPackObject.create;
  result := lvMsgPackObject;
  lvMsgPackObject.setHeadBytes(lvHeadBytes);

  //读取数据长度
  if lvDataLen > 0 then
  begin
    setLength(lvBytes, lvDataLen);
    inBuf.readBuffer(@lvBytes[0], lvDataLen);
    lvMsgPackObject.setBufferBytes(lvBytes);
  end;
end;

end.
