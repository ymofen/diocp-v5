(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *
 *)
unit utils_zipTools;

interface

{$if CompilerVersion>= 21}
  {$define NEWZLib}
{$IFEND}

uses
  Classes, Zlib, SysUtils;

type
{$if CompilerVersion< 18.5}
  TBytes = array of Byte;
{$IFEND}

  TZipTools = class(TObject)
  public
    /// <summary>
    ///   解压
    /// </summary>
    class procedure UnZipStream(const pvInStream, pvOutStream: TStream);

    /// <summary>
    ///   压缩
    /// </summary>
    class procedure ZipStream(const pvInStream, pvOutStream: TStream);


    class function verifyData(const buf; len:Cardinal): Cardinal;
    class function verifyStream(pvStream:TStream; len:Cardinal): Cardinal;
  end;

implementation

class procedure TZipTools.UnZipStream(const pvInStream, pvOutStream: TStream);
var
  l:Integer;

{$IFDEF POSIX}
var
  lvBytes, lvOutBytes:TBytes;
{$ELSE}
var
  lvBytes:TBytes;
  OutBuf: Pointer;
  OutBytes: Integer;
{$ENDIF}
begin
  if pvInStream= nil then exit;
  l := pvInStream.Size;
  if l = 0 then Exit;
{$IFDEF POSIX}
  SetLength(lvBytes, l);
  pvInStream.Position := 0;
  pvInStream.Read(lvBytes[0], pvInStream.Size);
  ZLib.ZDecompress(lvBytes, lvOutBytes);  //POSIX下，只支持该方式
  pvOutStream.Size := Length(lvOutBytes);
  pvOutStream.Position := 0;
  pvOutStream.Write(lvOutBytes[0], Length(lvOutBytes));
{$ELSE}
  setLength(lvBytes, l);
  pvInStream.Position := 0;
  pvInStream.ReadBuffer(lvBytes[0], l);
  {$if defined(NEWZLib)}
  ZLib.ZDecompress(@lvBytes[0], l, OutBuf, OutBytes);
  {$ELSE}
  Zlib.DecompressBuf(@lvBytes[0], l, 0, OutBuf, OutBytes);
  {$ifend}
  try
    pvOutStream.Size := OutBytes;
    pvOutStream.Position := 0;
    pvOutStream.WriteBuffer(OutBuf^, OutBytes);
  finally
    FreeMem(OutBuf, OutBytes);
  end;
{$ENDIF}
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

class function TZipTools.verifyStream(pvStream:TStream; len:Cardinal):
    Cardinal;
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

class procedure TZipTools.ZipStream(const pvInStream, pvOutStream: TStream);
{$IFDEF POSIX}
var
  lvBytes, lvOutBytes:TBytes;
{$ELSE}
var
  lvInBuf: TBytes;
  OutBuf: Pointer;
  OutBytes: Integer;
{$ENDIF}
var
  l: Integer;
begin
  if pvInStream= nil then exit;
  l := pvInStream.Size;
  if l = 0 then Exit;

{$IFDEF POSIX}
  SetLength(lvBytes, pvInStream.Size);
  pvInStream.Position := 0;
  pvInStream.Read(lvBytes[0], pvInStream.Size);
  ZLib.ZCompress(lvBytes, lvOutBytes);                 // POSIX下只支持该中方式的压缩
  pvOutStream.Size := Length(lvOutBytes);
  pvOutStream.Position := 0;
  pvOutStream.Write(lvOutBytes[0], Length(lvOutBytes));
{$ELSE}
  SetLength(lvInBuf, l);
  pvInStream.Position := 0;
  pvInStream.ReadBuffer(lvInBuf[0], l);
  {$if defined(NEWZLib)}
  ZLib.ZCompress(@lvInBuf[0], l, OutBuf, OutBytes);
  {$ELSE}
  ZLib.CompressBuf(@lvInBuf[0], l, OutBuf, OutBytes);
  {$ifend}
  try
    pvOutStream.Size := OutBytes;
    pvOutStream.Position := 0;
    pvOutStream.WriteBuffer(OutBuf^, OutBytes);
  finally
    FreeMem(OutBuf, OutBytes);
  end;
{$ENDIF}


end;

end.
