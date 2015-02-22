(*
   unit Owner: d10.ymofen

   first release:
      2014-10-11 12:40:33

   examples:
      TMsgPackCoder.SendObject(TRawTcpClientCoderImpl.Create(FRawTcpClient), lvStream);
      TMsgPackCoder.RecvObject(TRawTcpClientCoderImpl.Create(FRawTcpClient), lvStream);
*)
unit uMsgPackCoder_ICodeSocket;

interface

uses
  uICoderSocket, Classes, SysUtils, uZipTools, qmsgPack, uBufferRW;

type
  {$if CompilerVersion < 18}
    TBytes = array of Byte;
  {$IFEND}

  TMsgPackCoder = class(TObject)
  private
    /// <summary>
    ///   从头Buffer和数据buffer中还原TQMsgPack
    /// </summary>
    class procedure decodeFromBuffer(pvObject:TQMsgPack; pvHeadBuffer,
        pvBuffer:TBytes);

    class function SendStream(pvSocket: ICoderSocket; pvStream: TStream): Integer;
  public
    /// <summary>
    ///   接收解码
    /// </summary>
    /// <returns> Boolean
    /// </returns>
    /// <param name="pvSocket"> (TClientSocket) </param>
    /// <param name="pvObject"> (TObject) </param>
    class function RecvObject(pvSocket: ICoderSocket; pvObject: TObject): Boolean;

    /// <summary>
    ///   编码发送
    /// </summary>
    /// <param name="pvSocket"> (TClientSocket) </param>
    /// <param name="pvDataObject"> (TObject) </param>
    class function SendObject(pvSocket: ICoderSocket; pvObject: TObject): Integer;
  end;


implementation

  //PACK_FLAG  + CRC_VALUE + STREAM_LEN + STREAM_DATA

uses
  uByteTools;

const
  PACK_FLAG = $0818;  
  MAX_HEAD_LEN = 1024;

  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。

const
  BUF_BLOCK_SIZE = 1024 * 8;




resourcestring
  strRecvException_ErrorFlag = '错误的包头数据,断开与服务器的连接';
  strRecvException_ErrorData = '错误的数据,断开与服务器的连接';
  strRecvException_VerifyErr = '错误的数据包，校验失败!';
  strSendException_TooBig = '数据包太大,请在业务层分拆发送,最大数据包[%d]!';
  strSendException_NotEqual = '发送Buffer错误指定发送%d,实际发送:%d';





class procedure TMsgPackCoder.decodeFromBuffer(pvObject: TQMsgPack;
    pvHeadBuffer, pvBuffer: TBytes);
var
  lvBufferReader:IBufferReader;
  lvZiped:Byte;
  lvNameSpaceID:Integer;
  lvBuffers:TBytes;
begin
  lvZiped:= 0;
  if Length(pvHeadBuffer) > 0 then
  begin
    lvBufferReader := TBufferReader.create(@pvHeadBuffer[0], length(pvHeadBuffer));
    lvBufferReader.read(lvZiped, SizeOf(lvZiped));
    lvBufferReader.read(lvNameSpaceID, SizeOf(lvNameSpaceID));
  end;

  //压缩了
  if lvZiped = 1 then
  begin
    lvBuffers := TBytes(TZipTools.unCompressBuf(pvBuffer[0], Length(pvBuffer)));
    pvObject.Parse(lvBuffers);
  end else
  begin
    //直接解析
    pvObject.Parse(pvBuffer);
  end;
end;

{ TMsgPackCoder }

class function TMsgPackCoder.RecvObject(pvSocket: ICoderSocket;
  pvObject: TObject): Boolean;
var
  lvBytes, lvHeadBytes:SysUtils.TBytes;
  lvReadL:Integer;
  lvPACK_FLAG:Word;
  lvDataLen: Integer;
  lvHeadlen: Integer;
begin
  pvSocket.recvBuf(@lvPACK_FLAG, 2);
  if lvPACK_FLAG <> PACK_FLAG then
  begin
    //错误的包数据
    raise Exception.Create('错误的包头数据,断开与服务器的连接');
  end;

  //headlen
  pvSocket.recvBuf(@lvReadL, SizeOf(lvReadL));
  lvHeadlen := TByteTools.swap32(lvReadL);

  if lvHeadlen > 0 then
  begin
    //文件头不能过大
    if lvHeadlen > MAX_HEAD_LEN  then
    begin
      //错误的包数据
      raise Exception.Create('错误的包头数据,断开与服务器的连接');
    end;


    //head
    setLength(lvHeadBytes, lvHeadlen);
    pvSocket.recvBuf(@lvHeadBytes[0], lvHeadlen);
  end else if lvHeadlen < 0 then
  begin
    //错误的包数据
    raise Exception.Create('错误的包头数据,断开与服务器的连接');
  end;

  //buf_len
  pvSocket.recvBuf(@lvReadL, SizeOf(lvReadL));
  lvDataLen := TByteTools.swap32(lvReadL);

  ///如果数据过大，
  if (lvDataLen > MAX_OBJECT_SIZE)  then
  begin
    //错误的包数据
    raise Exception.Create('错误的数据,断开与服务器的连接');
  end;


  //读取数据长度
  if lvDataLen > 0 then
  begin
    setLength(lvBytes, lvDataLen);
    pvSocket.recvBuf(@lvBytes[0], lvDataLen);
  end;

  decodeFromBuffer(TQMsgPack(pvObject), lvHeadBytes, lvBytes);
  Result := true;                            
end;

class function TMsgPackCoder.SendObject(pvSocket: ICoderSocket; pvObject:
    TObject): Integer;
var
  lvMsgPack:TQMsgPack;
  lvBytes :SysUtils.TBytes;
  lvDataLen, lvWriteL: Integer;
  lvHeadlen, lvNameSpaceID: Integer;
  lvZiped:Byte;
  lvPACK_FLAG:Word;
  lvStream: TMemoryStream;
begin
  Result := 0;
  if pvObject = nil then exit;
  lvStream := TMemoryStream.create();
  try
    lvMsgPack := TQMsgPack(pvObject);
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
    lvStream.Write(lvPACK_FLAG, 2);
    //Head_len: zip + namespaceid
    lvHeadlen := SizeOf(lvZiped) + SizeOf(lvNameSpaceID);
    lvWriteL := TByteTools.swap32(lvHeadlen);
    //head_len
    lvStream.Write(lvWriteL, SizeOf(lvWriteL));

    //zip
    lvStream.Write(lvZiped, SizeOf(lvZiped));
    //namesapceid
    lvStream.Write(lvNameSpaceID, SizeOf(lvNameSpaceID));

    //data_len
    lvWriteL := TByteTools.swap32(lvDataLen);
    lvStream.Write(lvWriteL, SizeOf(lvWriteL));
    //data
    lvStream.Write(lvBytes[0], lvDataLen);

    lvStream.Position := 0;
    Result := sendStream(pvSocket,lvStream);
  finally
    lvStream.Free;
  end;
end;

class function TMsgPackCoder.SendStream(pvSocket: ICoderSocket; pvStream:
    TStream): Integer;
var
  lvBufBytes:array[0..BUF_BLOCK_SIZE-1] of byte;
  l, j, r, lvTotal:Integer;
  P:PByte;
begin
  Result := 0;
  if pvStream = nil then Exit;
  if pvStream.Size = 0 then Exit;
  lvTotal :=0;
  
  pvStream.Position := 0;
  repeat
    //FillMemory(@lvBufBytes[0], SizeOf(lvBufBytes), 0);
    l := pvStream.Read(lvBufBytes[0], SizeOf(lvBufBytes));
    if (l > 0) then
    begin
      P := PByte(@lvBufBytes[0]);
      j := l;
      while j > 0 do
      begin
        r := pvSocket.sendBuf(P, j);
        if r = -1 then
        begin
          RaiseLastOSError;
        end;
        Inc(P, r);
        Dec(j, r);
      end;
      lvTotal := lvTotal + l;
    end else Break;
  until (l = 0);
  Result := lvTotal;
end;

end.
