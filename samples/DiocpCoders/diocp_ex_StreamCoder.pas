unit diocp_ex_StreamCoder;

interface

uses
  diocp_coder_baseObject, Classes, SysUtils, utils_buffer, utils_BufferPool, diocp_ex_streamProtocol;

type
  TDiocpStreamCoderExchange = class(TDiocpContextCoderExchange)
  private
    FRecvBuf: PByte;
    FRecvLength: Integer;
    FRecvStreamObj: TDiocpStreamObject;
    FContentStream:TMemoryStream;

    /// <summary>
    ///   ������һ��Content����������, ԭContent���ٹ���
    ///   (�ⲿGetDataʱ��ԭ������������)
    /// </summary>
    procedure NewContent;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CleanUp; override;
    
    /// <summary>
    ///   �����յ�������,����н��յ�����,���ø÷���,���н���
    /// </summary>
    /// <returns>
    ///   0����Ҫ���������
    ///   1: ����ɹ�
    ///  -1: ����ʧ��
    /// </returns>
    /// <param name="inBuf"> ���յ��������� </param>
    function Decode: Integer;

  end;


  TIOCPStreamDecoder = class(TDiocpDecoder)
  public
    /// <summary>
    ///   ��������
    /// </summary>
    procedure SetRecvBuffer(const pvExchange: TDiocpContextCoderExchange; const
        buf:Pointer; len:Cardinal); override;

    /// <summary>
    ///   ��ȡ����õ�����
    /// </summary>
    function GetData(const pvExchange: TDiocpContextCoderExchange; const pvCopy:
        Boolean): Pointer; override;


    /// <summary>
    ///   �ͷŽ���õ�����
    /// </summary>
    procedure ReleaseData(const pvExchange: TDiocpContextCoderExchange; const
        pvData: Pointer; const pvCopy: Boolean); override;

    /// <summary>
    ///   �����յ�������,����н��յ�����,���ø÷���,���н���
    /// </summary>
    /// <returns>
    ///   0����Ҫ���������
    ///   1: ����ɹ�
    ///  -1: ����ʧ��
    /// </returns>
    /// <param name="inBuf"> ���յ��������� </param>
    function Decode(const pvExchange: TDiocpContextCoderExchange): Integer;  override;
  end;


  TIOCPStreamEncoder = class(TDiocpEncoder)
  public
    /// <summary>
    ///   ����Ҫ���͵Ķ���
    /// </summary>
    /// <param name="pvDataObject"> Ҫ���б���Ķ��� </param>
    /// <param name="pvBufWriter"> ����д�� </param>
    procedure Encode(const pvExchange: TDiocpContextCoderExchange; const
        pvDataObject: Pointer; const pvBufWriter: TBlockBuffer); override;
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

  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //�������С 10M , ����10M �����Ϊ����İ���



function TIOCPStreamDecoder.Decode(
  const pvExchange: TDiocpContextCoderExchange): Integer;
var
  lvExchange:TDiocpStreamCoderExchange;
begin
  lvExchange := TDiocpStreamCoderExchange(pvExchange);
  Result := lvExchange.Decode();
end;

function TIOCPStreamDecoder.GetData(const pvExchange:
    TDiocpContextCoderExchange; const pvCopy: Boolean): Pointer;
var
  lvExchange:TDiocpStreamCoderExchange;
begin
  lvExchange := TDiocpStreamCoderExchange(pvExchange);
  Result := lvExchange.FRecvStreamObj.Content;
  if pvCopy then
  begin       
    lvExchange.NewContent;
  end;
end;

procedure TIOCPStreamDecoder.SetRecvBuffer(const pvExchange:
    TDiocpContextCoderExchange; const buf:Pointer; len:Cardinal);
begin
  TDiocpStreamCoderExchange(pvExchange).FRecvBuf := PByte(buf);
  TDiocpStreamCoderExchange(pvExchange).FRecvLength := len;
end;

procedure TIOCPStreamDecoder.ReleaseData(const pvExchange:
    TDiocpContextCoderExchange; const pvData: Pointer; const pvCopy: Boolean);
begin
  inherited;
  if pvCopy then
  begin
    TMemoryStream(pvData).Free;
  end;
end;

{ TIOCPStreamEncoder }

procedure TIOCPStreamEncoder.Encode(const pvExchange:
    TDiocpContextCoderExchange; const pvDataObject: Pointer; const pvBufWriter:
    TBlockBuffer);
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
    raise Exception.CreateFmt('���ݰ�̫��,����ҵ���ֲ���,������ݰ�[%d]!', [MAX_OBJECT_SIZE]);
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

procedure TDiocpStreamCoderExchange.CleanUp;
begin
  inherited;
  FRecvStreamObj.Clear;
end;

constructor TDiocpStreamCoderExchange.Create;
begin
  inherited Create;

  FRecvStreamObj := TDiocpStreamObject.Create();
  NewContent;
end;

destructor TDiocpStreamCoderExchange.Destroy;
begin
  FreeAndNil(FRecvStreamObj);
  if FContentStream <> nil then
  begin
    FContentStream.Free;
    FContentStream := nil;
  end;
  inherited Destroy;
end;

function TDiocpStreamCoderExchange.Decode: Integer;
begin
  Result := 0;
  while FRecvLength > 0 do
  begin
    Result := FRecvStreamObj.InputBuffer(FRecvBuf^);
    Inc(FRecvBuf);
    Dec(FRecvLength);
    if Result <> 0 then
    begin
      Break;
    end;
  end;
end;

procedure TDiocpStreamCoderExchange.NewContent;
begin
  FContentStream := TMemoryStream.Create;
  FRecvStreamObj.WrapContent(FContentStream);
end;

end.
