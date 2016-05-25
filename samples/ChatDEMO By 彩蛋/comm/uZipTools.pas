unit uZipTools;

///2013年5月27日 15:26:37
///  加入对XE支持

///2013年5月27日 09:35:03
///  加入对流的压缩函数

interface

uses
  ZLib, Windows, Classes, SysUtils;

{$if CompilerVersion>= 21}
  {$define NEWZLib}
{$IFEND}

type
//2007以上直接=TBytes
{$if CompilerVersion< 18.5}
  TBytes = array of Byte;
{$IFEND}

  TZipTools = class(TObject)
  public
    //压缩字符串(与JAVA兼容)
    class function compressStr(pvData: string): TBytes;

    //解压字符串(与JAVA兼容)
    class function unCompressStr(pvData: TBytes; pvDataSize: Integer = 0): string;

    //压缩(与JAVA兼容)
    class procedure compressStreamEX(const pvStream:TStream);

    //解压(与JAVA兼容)
    class procedure unCompressStreamEX(const pvStream:TStream);

    //压缩(与JAVA兼容)
    class function compressStream(const pvStream, pvZipStream:TStream): Boolean;

    //解压(与JAVA兼容)
    class function unCompressStream(const pvZipStream, pvStream:TStream): Boolean;


    //压缩(与JAVA兼容)
    class function compressBuf(const Buffer; Count: Longint): TBytes;

    //解压(与JAVA兼容)
    class function unCompressBuf(const zipBuffer; Count: Longint): TBytes;

    class function verifyData(const buf; len:Cardinal):Cardinal;

    class function verifyStream(pvStream:TStream; len:Cardinal): Cardinal;
  end;

implementation

class function TZipTools.compressBuf(const Buffer; Count: Longint): TBytes;
var
  OutBuf: Pointer;
  OutBytes: Integer;
begin
  {$if defined(NEWZLib)}
    ZLib.ZCompress(@Buffer, Count, OutBuf, OutBytes);
  {$ELSE}
    ZLib.CompressBuf(@Buffer, Count, OutBuf, OutBytes);
  {$ifend}
    try
      SetLength(Result, OutBytes);
      CopyMemory(@Result[0], OutBuf, OutBytes);
    finally
      FreeMem(OutBuf, OutBytes);
    end;
end;

class function TZipTools.unCompressBuf(const zipBuffer; Count: Longint): TBytes;
var
  lvSize:Cardinal;
  OutBuf: Pointer;
  OutBytes: Integer;
begin
  lvSize := Count;
  {$if defined(NEWZLib)}
    Zlib.ZDecompress(@zipBuffer, lvSize, OutBuf, OutBytes);
  {$ELSE}
    Zlib.DecompressBuf(@zipBuffer, lvSize, 0, OutBuf, OutBytes);
  {$ifend}
    try
      SetLength(Result, OutBytes);
      CopyMemory(@Result[0], OutBuf, OutBytes);
    finally
      FreeMem(OutBuf, OutBytes);
    end;

end;

class function TZipTools.compressStr(pvData: string): TBytes;
begin
  result := compressBuf(PAnsiChar(AnsiString(pvData))^, Length(AnsiString(pvData)));
end;

class procedure TZipTools.compressStreamEX(const pvStream:TStream);
begin
  compressStream(pvStream, pvStream);
end;


class function TZipTools.compressStream(const pvStream, pvZipStream:TStream):
    Boolean;
var
  lvBytes: TBytes;
  OutBuf: Pointer;
  OutBytes: Integer;
  l: Integer;
begin
  Result := False;
  if pvStream= nil then exit;

  l := pvStream.Size;

  if l = 0 then Exit;

  setLength(lvBytes, l);
  pvStream.Position := 0;
  pvStream.ReadBuffer(lvBytes[0], l);

  {$if defined(NEWZLib)}
    ZLib.ZCompress(@lvBytes[0], l, OutBuf, OutBytes);
  {$ELSE}
    ZLib.CompressBuf(@lvBytes[0], l, OutBuf, OutBytes);
  {$ifend}
    try
      pvZipStream.Size := OutBytes;
      pvZipStream.Position := 0;
      pvZipStream.WriteBuffer(OutBuf^, OutBytes);
      Result := true;
    finally
      FreeMem(OutBuf, OutBytes);
    end;

end;

class procedure TZipTools.unCompressStreamEX(const pvStream:TStream);
begin
  unCompressStream(pvStream, pvStream)
end;



class function TZipTools.verifyData(const buf; len: Cardinal): Cardinal;
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

class function TZipTools.unCompressStream(const pvZipStream, pvStream:TStream):
    Boolean;
var
  l:Integer;
  lvBytes: TBytes;
  OutBuf: Pointer;
  OutBytes: Integer;

begin
  Result := false;
  if pvZipStream= nil then exit;
  l := pvZipStream.Size;
  if l = 0 then Exit;

  setLength(lvBytes, l);
  pvZipStream.Position := 0;
  pvZipStream.ReadBuffer(lvBytes[0], l);

  {$if defined(NEWZLib)}
    ZLib.ZDecompress(@lvBytes[0], l, OutBuf, OutBytes);
  {$ELSE}
    Zlib.DecompressBuf(@lvBytes[0], l, 0, OutBuf, OutBytes);
  {$ifend}
    try
      pvStream.Size := OutBytes;
      pvStream.Position := 0;
      pvStream.WriteBuffer(OutBuf^, OutBytes);
      Result := true;
    finally
      FreeMem(OutBuf, OutBytes);
    end;

end;

class function TZipTools.unCompressStr(pvData: TBytes; pvDataSize: Integer =
    0): string;
var
  lvSize:Cardinal;
  lvOutBytes:TBytes;

  s:AnsiString;
begin
  lvSize := pvDataSize;
  if lvSize = 0 then lvSize := Length(AnsiString(pvData));

  lvOutBytes := self.unCompressBuf(pvData[0], lvSize);
  SetLength(s, Length(lvOutBytes));
  CopyMemory(@s[1], @lvOutBytes[0], Length(lvOutBytes));
  Result := s;

end;

class function TZipTools.verifyStream(pvStream:TStream; len:Cardinal): Cardinal;
var
  l, j:Cardinal;
  lvBytes:TBytes;
begin
  SetLength(lvBytes, 1024);

  if len = 0 then
  begin
    j := pvStream.Size - pvStream.Position;
  end else
  begin
    j := len;
  end;

  Result := 0;

  while j > 0 do
  begin
    if j <1024 then l := j else l := 1024;
    
    pvStream.ReadBuffer(lvBytes[0], l);

    Result := Result + verifyData(lvBytes[0], l);
    Dec(j, l);
  end;
end;


end.
