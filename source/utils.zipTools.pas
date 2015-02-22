(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 ∑¢≤º
 *
 *
 *)
unit utils.zipTools;

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
    ///   Ω‚—π
    /// </summary>
    class procedure UnZipStream(const pvInStream, pvOutStream: TStream);

    /// <summary>
    ///   —πÀı
    /// </summary>
    class procedure ZipStream(const pvInStream, pvOutStream: TStream);


    class function verifyData(const buf; len:Cardinal): Cardinal;
    class function verifyStream(pvStream:TStream; len:Cardinal): Cardinal;
  end;

implementation

class procedure TZipTools.UnZipStream(const pvInStream, pvOutStream: TStream);
{$if defined(NEWZLib)}
var
  lvBytes, lvOutBytes:TBytes;
{$ELSE}
var
  l:Integer;
  lvBytes: TBytes;
  OutBuf: Pointer;
  OutBytes: Integer;
{$ifend}
begin
  {$if defined(NEWZLib)}
  SetLength(lvBytes, pvInStream.Size);
  pvInStream.Position := 0;
  pvInStream.Read(lvBytes[0], pvInStream.Size);
  ZLib.ZDecompress(lvBytes, lvOutBytes);
  pvOutStream.Size := 0;
  pvOutStream.Write(lvOutBytes[0], Length(lvOutBytes));
  {$ELSE}
  if pvInStream= nil then exit;
  l := pvInStream.Size;
  if l = 0 then Exit;

  setLength(lvBytes, l);
  pvInStream.Position := 0;
  pvInStream.ReadBuffer(lvBytes[0], l);
  Zlib.DecompressBuf(@lvBytes[0], l, 0, OutBuf, OutBytes);
  try
    pvOutStream.Size := OutBytes;
    pvOutStream.Position := 0;
    pvOutStream.WriteBuffer(OutBuf^, OutBytes);
  finally
    FreeMem(OutBuf, OutBytes);
  end;
  {$ifend}



//  if pvInStream= nil then Exit;
//  if pvInStream.Size = 0 then Exit;
//  ZLib.ZDecompressStream(pvInStream, pvOutStream);
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
{$if defined(NEWZLib)}
var
  lvBytes, lvOutBytes:TBytes;
{$ELSE}
var
  lvBytes: TBytes;
  OutBuf: Pointer;
  OutBytes: Integer;
  l: Integer;
{$ifend}
begin
{$if defined(NEWZLib)}
  SetLength(lvBytes, pvInStream.Size);
  pvInStream.Position := 0;
  pvInStream.Read(lvBytes[0], pvInStream.Size);
  ZLib.ZCompress(lvBytes, lvOutBytes);

  pvOutStream.Size := 0;
  pvOutStream.Write(lvOutBytes[0], Length(lvOutBytes));
{$ELSE}
  if pvInStream= nil then exit;

  l := pvInStream.Size;

  if l = 0 then Exit;

  setLength(lvBytes, l);
  pvInStream.Position := 0;
  pvInStream.ReadBuffer(lvBytes[0], l);

  ZLib.CompressBuf(@lvBytes[0], l, OutBuf, OutBytes);
  try
    pvOutStream.Size := OutBytes;
    pvOutStream.Position := 0;
    pvOutStream.WriteBuffer(OutBuf^, OutBytes);
  finally
    FreeMem(OutBuf, OutBytes);
  end;
{$ifend}


  //ZLib.ZDecompress(pvInStream, pvOutStream);
end;

end.
