unit uCRCTools;

interface

uses
  SysUtils, Windows, Classes;

type
  TCRCTools = class(TObject)
  private
    class procedure ContinueCRC32(var CRC: Cardinal; const buf; Len: Cardinal);
  public
    class function crc32Buf(const buf; Len: Cardinal): Cardinal;
    class function crc32Stream(const pvInStream: TStream): Cardinal;
    class function crc32String(pvString:string): Cardinal;
    class function crc32File(const FileName: string; FSize: PCardinal = nil):
        Cardinal;
  end;





implementation



const
  CRC32_POLYNOMIAL = $EDB88320;
  MAX_BUFFER_SIZE = 4 * 1024 * 1024;

var
  Ccitt32Table: array[0..255] of DWORD;

procedure BuildCRCTable;
var i, j: longint;
  value: DWORD;
begin
  for i := 0 to 255 do begin
    value := i;
    for j := 8 downto 1 do
      if ((value and 1) <> 0) then
        value := (value shr 1) xor CRC32_POLYNOMIAL
      else
        value := value shr 1;
    Ccitt32Table[i] := value;
  end
end;

class procedure TCRCTools.ContinueCRC32(var CRC: Cardinal; const buf; Len:
    Cardinal);
var
  j: Cardinal;
  p: PByte;
begin
  p := PByte(@buf);
  for j := 1 to Len do
  begin
    CRC := (((CRC shr 8) and $00FFFFFF) xor (Ccitt32Table[(CRC xor p^) and $FF]));
    inc(p);
  end;
end;

class function TCRCTools.crc32Buf(const buf; Len: Cardinal): Cardinal;
var
  j: Cardinal;
  p: PByte;
begin
  Result := $FFFFFFFF;
  if len <= 0 then exit;
  p := PByte(@buf);
  for j := 1 to Len do
  begin
    result := (((result shr 8) and $00FFFFFF) xor (Ccitt32Table[(result xor p^) and $FF]));
    inc(p);
  end;
end;

class function TCRCTools.crc32Stream(const pvInStream: TStream): Cardinal;
var
  vBuffer: string;
  lvStreamLen, lvReadLen: Cardinal;
begin
  result := $FFFFFFFF;

  pvInStream.Position := 0;

  lvStreamLen := pvInStream.Size;
  if lvStreamLen = 0 then Exit;

  while lvStreamLen > 0 do
  begin
    if lvStreamLen > MAX_BUFFER_SIZE then lvReadLen := MAX_BUFFER_SIZE else lvReadLen := lvStreamLen;
    setlength(vBuffer, MAX_BUFFER_SIZE);
    lvReadLen := pvInStream.Read(vBuffer[1], lvReadLen);
    continuecrc32(result, vBuffer[1], lvReadLen);
    dec(lvStreamLen, lvReadLen);
  end;
end;

class function TCRCTools.crc32String(pvString:string): Cardinal;
begin
  Result := 0;
  if pvString = '' then exit;
  
  Result := crc32Buf(pvString[1], Length(pvString));
end;

class function TCRCTools.crc32File(const FileName: string; FSize: PCardinal =
    nil): Cardinal;
var
  FileHandle: THandle;
  sz, rsz: Cardinal;
  tmp: string;
begin
  result := $FFFFFFFF;
  if not fileexists(filename) then exit;

  FileHandle := CreateFile(pChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then
  try
    sz := GetFileSize(FileHandle, nil);
    if fsize <> nil then
      fsize^ := sz;
    setlength(tmp, MAX_BUFFER_SIZE);
    while sz > 0 do
    begin
      if sz > MAX_BUFFER_SIZE then rsz := MAX_BUFFER_SIZE else rsz := sz;
      fileread(filehandle, tmp[1], rsz);
      continuecrc32(result, tmp[1], rsz);
      dec(sz, rsz);
    end;
  finally
    CloseHandle(FileHandle);
  end;
end;

initialization
  BuildCRCTable;

finalization
  ;

end.

