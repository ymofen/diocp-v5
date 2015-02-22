unit uIOCPJSonStreamEncoder;

interface

uses
  uIocpCoder, uBuffer, JSonStream, Classes,
  uZipTools, SysUtils, uMyTypes, AnsiStringTools, uByteTools;

type
  TIOCPJSonStreamEncoder = class(TIOCPEncoder)
  public
    /// <summary>
    ///   编码要发生的对象
    /// </summary>
    /// <param name="pvDataObject"> 要进行编码的对象 </param>
    /// <param name="ouBuf"> 编码好的数据 </param>
    procedure Encode(pvDataObject:TObject; const ouBuf: TBufferLink); override;
  end;

implementation

procedure TIOCPJSonStreamEncoder.Encode(pvDataObject:TObject; const ouBuf:
    TBufferLink);
var
  lvJSonStream:TJsonStream;
  lvJSonLength:Integer;
  lvStreamLength:Integer;
  sData:String;
  lvStream:TStream;
  lvTempBuf:PAnsiChar;
  lvBytes, lvTempBytes:TBytes;
begin
  if pvDataObject = nil then exit;
  lvJSonStream := TJsonStream(pvDataObject);

  //是否压缩流
  if (lvJSonStream.Stream <> nil) then
  begin
    if lvJSonStream.Json.O['config.stream.zip'] <> nil then
    begin
      if lvJSonStream.Json.B['config.stream.zip'] then
      begin
        //压缩流
        TZipTools.compressStreamEx(lvJSonStream.Stream);
      end;
    end else if lvJSonStream.Stream.Size > 0 then
    begin
      //压缩流
      TZipTools.compressStreamEx(lvJSonStream.Stream);
      lvJSonStream.Json.B['config.stream.zip'] := true;
    end;
  end;   

  sData := lvJSonStream.JSon.AsJSon(True);


  lvBytes := TAnsiStringTools.ansiString2Utf8Bytes(sData);

  lvJSonLength := Length(lvBytes);
  lvStream := lvJSonStream.Stream;

  lvJSonLength := TByteTools.swap32(lvJSonLength);
  ouBuf.AddBuffer(@lvJSonLength, SizeOf(lvJSonLength));


  if lvStream <> nil then
  begin
    lvStreamLength := lvStream.Size;
  end else
  begin
    lvStreamLength := 0;
  end;

  lvStreamLength := TByteTools.swap32(lvStreamLength);
  ouBuf.AddBuffer(@lvStreamLength, SizeOf(lvStreamLength));

  //json bytes
  ouBuf.AddBuffer(@lvBytes[0], Length(lvBytes));

  if lvStream.Size > 0 then
  begin
    //stream bytes
    GetMem(lvTempBuf, lvStream.Size);
    try
      lvStream.Position := 0;
      lvStream.ReadBuffer(lvTempBuf^, lvStream.Size);
      ouBuf.AddBuffer(lvTempBuf, lvStream.Size);
    finally
      FreeMem(lvTempBuf, lvStream.Size);
    end;
  end;

end;

end.
