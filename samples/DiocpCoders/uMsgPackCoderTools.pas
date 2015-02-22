unit uMsgPackCoderTools;

interface

uses
  qmsgpack, Classes, sysUtils, uZipTools, uByteTools;

type
  TMsgPackCoderTools = class(TObject)
  public
    /// <summary>
    ///   编码到流
    /// </summary>
    /// <param name="pvSocket"> (TClientSocket) </param>
    /// <param name="pvMsgPack"> (TObject) </param>
    class procedure Encode(pvMsgPack: TQMsgPack; const pvStream: TStream);

    class procedure setResult(pvMsgPack: TQMsgPack; pvResult: Boolean);

    class procedure setResultMsg(pvMsgPack: TQMsgPack; pvMsg: string);

  end;

implementation

const
  PACK_FLAG = $0818;
  MAX_HEAD_LEN = 1024;

const
  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。

class procedure TMsgPackCoderTools.Encode(pvMsgPack: TQMsgPack; const pvStream:
    TStream);
var
  lvBytes :SysUtils.TBytes;
  lvDataLen, lvWriteL: Integer;
  lvHeadlen, lvNameSpaceID: Integer;
  lvZiped:Byte;
  lvPACK_FLAG:Word;
  lvStream: TStream;
begin
  if pvMsgPack = nil then exit;
  lvStream := pvStream;
  lvBytes := pvMsgPack.Encode;
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


  if pvMsgPack.ItemByPath('cmd.namespaceid') <> nil then
  begin
    lvNameSpaceID := pvMsgPack.ForcePath('cmd.namespaceid').AsInteger;
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
end;

class procedure TMsgPackCoderTools.setResult(pvMsgPack: TQMsgPack; pvResult:
    Boolean);
begin
  pvMsgPack.ForcePath('__result.result').AsBoolean := pvResult;
end;

class procedure TMsgPackCoderTools.setResultMsg(pvMsgPack: TQMsgPack; pvMsg:
    string);
begin
  pvMsgPack.ForcePath('__result.msg').AsString := pvMsg;
end;

end.
