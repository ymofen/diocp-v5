unit diocp_ex_StreamCoder;

interface

uses
  diocp_coder_baseObject, Classes, SysUtils, utils_buffer, utils_BufferPool, diocp_ex_streamProtocol;

type
  TIOCPStreamDecoder = class(TDiocpDecoder)
  private
    FBuf: PByte;
    FLength: Integer;
    FStreamObj: TDiocpStreamObject;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///   输入数据
    /// </summary>
    procedure SetRecvBuffer(const buf:Pointer; len:Cardinal); override;

    /// <summary>
    ///   获取解码好的数据
    /// </summary>
    function GetData: Pointer; override;


    /// <summary>
    ///   释放解码好的数据
    /// </summary>
    procedure ReleaseData(const pvData:Pointer); override;

    /// <summary>
    ///   解码收到的数据,如果有接收到数据,调用该方法,进行解码
    /// </summary>
    /// <returns>
    ///   0：需要更多的数据
    ///   1: 解码成功
    ///  -1: 解码失败
    /// </returns>
    /// <param name="inBuf"> 接收到的流数据 </param>
    function Decode: Integer; override;
  end;


  TIOCPStreamEncoder = class(TDiocpEncoder)
  public
    /// <summary>
    ///   编码要发送的对象
    /// </summary>
    /// <param name="pvDataObject"> 要进行编码的对象 </param>
    /// <param name="pvBufWriter"> 数据写入 </param>
    procedure Encode(const pvDataObject: Pointer; const pvBufWriter: TBlockBuffer);
        override;
  end;

function verifyData(const buf; len:Cardinal): Cardinal;

implementation

uses
  utils_byteTools;

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



constructor TIOCPStreamDecoder.Create;
begin
  inherited Create;
  FStreamObj := TDiocpStreamObject.Create();
end;

destructor TIOCPStreamDecoder.Destroy;
begin
  FreeAndNil(FStreamObj);
  inherited Destroy;
end;

function TIOCPStreamDecoder.Decode: Integer;
begin
  Result := 0;
  while FLength > 0 do
  begin
    Result := FStreamObj.InputBuffer(FBuf^);
    Inc(FBuf);
    Dec(FLength);
    if Result <> 0 then
    begin
      Break;
    end;
  end;
end;

function TIOCPStreamDecoder.GetData: Pointer;
begin
  Result := FStreamObj.Content;
end;

procedure TIOCPStreamDecoder.SetRecvBuffer(const buf:Pointer; len:Cardinal);
begin
  FBuf := PByte(buf);
  FLength := len;
end;

procedure TIOCPStreamDecoder.ReleaseData(const pvData:Pointer);
begin
  inherited;
end;

{ TIOCPStreamEncoder }

procedure TIOCPStreamEncoder.Encode(const pvDataObject: Pointer; const
    pvBufWriter: TBlockBuffer);
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



  pvBufWriter.Append(@lvPACK_FLAG,2);

  lvDataLen := TStream(pvDataObject).Size;
  SetLength(lvBuf, lvDataLen);

  TStream(pvDataObject).Read(lvBuf[0], lvDataLen);
  lvVerifyValue := verifyData(lvBuf[0], lvDataLen);

  pvBufWriter.Append(@lvVerifyValue,SizeOf(lvVerifyValue));
  lvWriteIntValue := TByteTools.swap32(lvDataLen);

  pvBufWriter.Append(@lvWriteIntValue, SizeOf(lvWriteIntValue));
  pvBufWriter.Append(@lvbuf[0],lvDataLen);

  
end;

end.
