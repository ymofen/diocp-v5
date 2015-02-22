unit uIOCPJSonStreamDecoder;

interface

uses
  uIocpCoder, uBuffer, Classes, JSonStream, uIOCPFileLogger, SysUtils,
  uMyTypes, iocpTcpServer;

const
  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。 

type
  TIOCPJSonStreamDecoder = class(TIOCPDecoder)
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

uses
  Windows, superobject, uZipTools, FileLogger, uByteTools, AnsiStringTools;

function TIOCPJSonStreamDecoder.Decode(const inBuf: TBufferLink; pvContext:
    TObject): TObject;
var
  lvJSonLength, lvStreamLength:Integer;
  lvMsg, lvData:String;


  lvBufData:PAnsiChar;
  lvStream:TMemoryStream;
  lvJsonStream:TJsonStream;
  lvBytes:TBytes;
  lvValidCount:Integer;
begin
  Result := nil;

  //如果缓存中的数据长度不够包头长度，解码失败<json字符串长度,流长度>
  lvValidCount := inBuf.validCount;
  if (lvValidCount < SizeOf(Integer) + SizeOf(Integer)) then
  begin
    Exit;
  end;

  //记录读取位置
  inBuf.markReaderIndex;
  inBuf.readBuffer(@lvJSonLength, SizeOf(Integer));
  inBuf.readBuffer(@lvStreamLength, SizeOf(Integer));


  lvJSonLength := TByteTools.swap32(lvJSonLength);
  lvStreamLength := TByteTools.swap32(lvStreamLength);


  ///如果数据过大，
  if (lvJSonLength > MAX_OBJECT_SIZE)
     or (lvStreamLength > MAX_OBJECT_SIZE)
     or ((lvJSonLength + lvStreamLength) >= MAX_OBJECT_SIZE)  then
  begin
    
    lvMsg := Format('超过服务端允许的最大未处理数据包大小%d, %d, 最大数据包(%d)',
      [lvJSonLength , lvStreamLength, MAX_OBJECT_SIZE]);
    TFileLogger.instance.logMessage(lvMsg, 'DECODER_Warning_');

    //清理掉数据
    inBuf.clearBuffer;

    lvJsonStream := TJsonStream.Create;
    lvJsonStream.setResult(False);
    lvJsonStream.setResultMsg('超过服务端允许的最大未处理数据包大小');
    Result := lvJsonStream;
    Exit;
  end;
  

  //如果缓存中的数据不够json的长度和流长度<说明数据还没有收取完毕>解码失败
  lvValidCount := inBuf.validCount;
  if lvValidCount < (lvJSonLength + lvStreamLength) then
  begin
    //返回buf的读取位置
    inBuf.restoreReaderIndex;
    exit;
  end else if (lvJSonLength + lvStreamLength) = 0 then
  begin
    //两个都为0<两个0>客户端可以用来作为自动重连使用
    TIOCPFileLogger.logMessage('接收到一次[00]数据!', 'DECODER_Warning_');
    Exit;
  end;



  //解码成功
  lvJsonStream := TJsonStream.Create;
  Result := lvJsonStream;

  //读取json字符串
  if lvJSonLength > 0 then
  begin
    SetLength(lvBytes, lvJSonLength);
    ZeroMemory(@lvBytes[0], lvJSonLength);
    inBuf.readBuffer(@lvBytes[0], lvJSonLength);

    lvData := TAnsiStringTools.Utf8Bytes2AnsiString(lvBytes);

    lvJsonStream.Json := SO(lvData);
  end else
  begin
    TFileLogger.instance.logMessage('接收到一次JSon为空的一次数据请求!', 'DECODER_Warning_');
  end;


  //读取流数据 
  if lvStreamLength > 0 then
  begin
    GetMem(lvBufData, lvStreamLength);
    try
      inBuf.readBuffer(lvBufData, lvStreamLength);
      lvJsonStream.Stream.Size := 0;
      lvJsonStream.Stream.WriteBuffer(lvBufData^, lvStreamLength);

      //解压流
      if lvJsonStream.Json.B['config.stream.zip'] then
      begin
        //解压
        TZipTools.unCompressStreamEX(lvJsonStream.Stream);
      end;
    finally
      FreeMem(lvBufData, lvStreamLength);
    end;
  end;
end;

end.
