unit diocp_ex_strObjectCoder;

interface

uses
  diocp.coder.baseObject, diocp.tcp.server, Classes, SysUtils, utils.buffer;

type
  TMessageHead = packed record
    HEAD_FLAG : Word;
    DATA_LEN  : Integer;
    RESERVE   : array[0..7] of Byte;  // 保留位
  end;
  PMessageHead = ^TMessageHead;

const
  HEAD_SIZE  = SizeOf(TMessageHead);
  PACK_FLAG = $D10;
  MAX_OBJECT_SIZE = 1024 * 1024 * 50;  //最大对象大小 50M , 超过则会认为错误的包。


type
  TStringObject = class(TObject)
  private
    FDataString: String;
  public
    property DataString: String read FDataString write FDataString;
  end;

type
  TDiocpStrObjectDecoder = class(TIOCPDecoder)
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


  TDiocpStrObjectEncoder = class(TIOCPEncoder)
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
  utils.byteTools;

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




function TDiocpStrObjectDecoder.Decode(const inBuf: TBufferLink; pvContext:
    TObject): TObject;
var
  lvValidCount, lvReadL:Integer;
  lvHead :TMessageHead;
  lvVerifyValue, lvVerifyDataValue:Cardinal;
  lvBytes:TBytes;
begin
  Result := nil;

  //如果缓存中的数据长度不够包头长度，
  lvValidCount := inBuf.validCount;   //pack_flag + reserve(4) + len
  if (lvValidCount < HEAD_SIZE) then
  begin
    Exit;
  end;

  //记录读取位置
  inBuf.markReaderIndex;
  inBuf.readBuffer(@lvHead, HEAD_SIZE);

  if lvHead.HEAD_FLAG <> PACK_FLAG then
  begin
    //错误的包数据
    Result := TObject(-1);
    exit;
  end;

  if lvHead.DATA_LEN > 0 then
  begin
    //文件头不能过大
    if lvHead.DATA_LEN > MAX_OBJECT_SIZE  then
    begin
      Result := TObject(-1);
      exit;
    end;

    if inBuf.validCount < lvHead.DATA_LEN then
    begin
      //返回buf的读取位置
      inBuf.restoreReaderIndex;
      exit;
    end;

    SetLength(lvBytes, lvHead.DATA_LEN + 1);  // 留一个结束符
    inBuf.readBuffer(@lvBytes[0], lvHead.DATA_LEN);

    Result := TStringObject.Create;
    TStringObject(Result).FDataString := Utf8ToAnsi(PAnsiChar(@lvBytes[0]));
  end else
  begin
    Result := nil;
  end;
end;

{ TDiocpStrObjectEncoder }

procedure TDiocpStrObjectEncoder.Encode(pvDataObject: TObject;
  const ouBuf: TBufferLink);
var
  lvDataLen:Integer;
  lvHead :TMessageHead;
  lvRawString:AnsiString;
begin
  lvHead.HEAD_FLAG := PACK_FLAG;

  lvRawString :=UTF8Encode(TStringObject(pvDataObject).FDataString);
  lvDataLen := Length(lvRawString);
  lvHead.DATA_LEN := lvDataLen;
  

  if lvDataLen > MAX_OBJECT_SIZE then
  begin
    raise Exception.CreateFmt('数据包太大,请在业务层分拆发送,最大数据包[%d]!', [MAX_OBJECT_SIZE]);
  end;


  // HEAD
  ouBuf.AddBuffer(@lvHead, HEAD_SIZE);
  ouBuf.AddBuffer(PAnsiChar(lvRawString), lvDataLen);
end;

end.
