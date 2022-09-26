(*
 *	 Unit owner: d10.�����
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 ����
 *
 *)
 
unit utils_byteTools;

interface

uses
  SysUtils, Classes, utils_strings;

type

// ��utils_strings�д��ڣ������ظ����壬���D7�޷�����
//  {$IF RTLVersion<25}
//  IntPtr=Integer;
//  {$IFEND IntPtr}
//
//  {$if CompilerVersion < 18} //before delphi 2007
//  TBytes = array of Byte;
//  {$ifend}


  TByteTools = class(TObject)
  public

     class function varToByteString(const v; len: Cardinal; Split: string = ' '):
         String;

     class function varToHexString(const v; len: Cardinal; Split: string = ' '):
         String;

     /// <summary>
     ///   ת����2���Ƶ��ַ���
     ///   $0FFF = 00001111 11111111
     /// </summary>
     class function varToBinaryString(const v; len: Cardinal; Split: string = ' '):
         String;


     /// <summary>
     ///  16����ת ������
     /// </summary>
     class function HexToBin(pvHexStr:String; buf:Pointer):Integer;

     class function HexStrToBytes(pvHexStr:String): TBytes; overload;

     class function HexStrToBytes(pvHexStr:String; outBuf:Pointer):Integer; overload;
     
     

     /// <summary>
     ///  16�����ַ���������
     /// </summary>
     class function HexValue(c: Char): Integer;

     /// <summary>
     ///   �Ƿ�16�����ַ�
     /// </summary>
     class function IsHexChar(c: Char): Boolean;

     /// <summary>
     ///   �ߵ�λ���н���
     /// </summary>
     class function swap32(const v: Integer): Integer;

     /// <summary>
     ///   �ߵ�λ���н���
     /// </summary>
     class function swap64(const v: int64): Int64;

     {$if CompilerVersion >= 18} //after delphi 2007
     /// <summary>
     ///   �ߵ�λ���н���
     /// </summary>
     class function swap16(const v):Word; overload;
     {$ifend}

     class function swap16(const v:Word):Word; overload;



     class procedure SwapBuff(buf: Pointer; offset, len: Integer);

     /// <summary>
     ///   ��������У����
     /// </summary>
     class function verifyData(const buf; len:Cardinal): Cardinal;

     class function crc16(const buf; len:Cardinal): Byte;

     /// <summary>
     ///  ��������У����
     /// </summary>
     class function verifyStream(pvStream:TStream; len:Cardinal): Cardinal;

     /// <summary>
     ///   �ļ�����TBytes
     /// </summary>
     class function FileToBytes(pvFileName:string): TBytes;

     /// <summary>
     ///   buff���ַ���,���buf��Ϊ0���滻��?
     /// </summary>
     class function BufShowAsString(pvBuf:Pointer; pvBufLength:Integer): String;

     /// <summary>
     ///   ���Buf���ļ�β��
     /// </summary>
     class procedure AppendBufToFile(pvBuf:Pointer; pvBufLength:Integer;
         pvFileName:string);


     /// <summary>
     ///   ��Buffer�н�ȡ�����ֽں���ΪUInt64   
     /// </summary>
     class function GetUInt64(pvBuf:Pointer; pvStart, pvLen:Integer): UInt64;

     /// <summary>
     ///  ��ȡλ
     ///    1 byte = 8 bit
     ///  bin = $FE $EE
     ///  GetBitU(bin, 0, 4) = $F
     ///  GetBitU(bin, 4, 4) = $E
     ///  GetBitU(bin, 8, 8) = $EE
     /// </summary>
     class function GetBitU(pvBuf:Pointer; pvStart:Integer; pvLen:Integer): UInt64;

     /// <summary>
     ///   ��ȡһλ��ֵ
     ///   pvOffset ȡֵ��ΧΪ (0..7)
     /// </summary>
     class function GetBit(const pvByte:Byte; pvOffset:Byte): Byte;

     /// <summary>
     ///   �����ֽڵ�Bitλ
     ///   pvOffset ȡֵΪ(0..7)
     ///   pvBitValue Ϊλֵ(0,1)
     /// </summary>
     class procedure SetBit(var vByte: Byte; pvOffset, pvBitValue: Byte);

     /// <summary>
     ///   ���õ�4λ��ֵ
     ///   pvL4Bit ��4λֵ(ֻȡ��4λ)
     ///   SetLow4Bit($81, $FE) = $8E
     /// </summary>
     class procedure SetLow4Bit(var vByte: Byte; pvL4Bit:Byte);

     /// <summary>
     ///   ��ȡ����λֵ
     ///   GetLow4Bit($81) = $01
     /// </summary>
     class function GetLow4Bit(const pvByte:Byte): Byte;
     class function HexStrToRawString(const pvHexStr: String): string; overload;

     /// <summary>
     ///   ���ø�4λ��ֵ
     ///   pvHigh4Bit ��4λֵ(ֻȡ��4λ)
     ///   SetHigh4Bit($81, $FE) = $F1
     /// </summary>
     class procedure SetHigh4Bit(var vByte: Byte; pvHigh4Bit: Byte);

  end;

/// <summary>
///   0: С��
///   1: ���
/// </summary>
{
    short int x;

����char x0,x1;

����x=0x1122;

����x0=((char*)&x)[0]; //�͵�ַ��Ԫ

����x1=((char*)&x)[1]; //�ߵ�ַ��Ԫ

������x0=0x11,���Ǵ��; ��x0=0x22,����С��......
}
function CheckIsLittleEndian: Boolean;

implementation

const
  U1 :UInt64 = 1;



function CheckIsLittleEndian: Boolean;
var
  lvWord:Word;
  lvPtr:PByte;
begin
  lvWord := $1122;
  lvPtr := PByte(@lvWord);
  if lvPtr^ = $22 then
  begin
    Result := true;
  end else begin
    Result := False;
  end;


end;

class procedure TByteTools.AppendBufToFile(pvBuf:Pointer; pvBufLength:Integer;
    pvFileName:string);
var
  lvStream:TFileStream;
begin
  if FileExists(pvFileName) then
  begin
    lvStream := TFileStream.Create(pvFileName, fmOpenWrite);
  end else
  begin
    lvStream := TFileStream.Create(pvFileName, fmCreate);
  end;
  try
    //lvStream.Position := lvStream.Size;
    lvStream.Seek(0, soEnd);
    lvStream.WriteBuffer(pvBuf^, pvBufLength);
  finally
    lvStream.Free;
  end;

end;

class function TByteTools.BufShowAsString(pvBuf:Pointer; pvBufLength:Integer):
    String;
var
  lvBytes:TBytes;
  lvBuf:PByte;
  i: Integer;
begin
  SetLength(lvBytes, pvBufLength + 1);
  lvBuf := PByte(pvBuf);
  for i := 0 to pvBufLength - 1 do
  begin
    if lvBuf^ = 0 then
    begin
      lvBytes[i] := 63;  // ?
    end else
    begin
      lvBytes[i] := lvBuf^;
    end;
    Inc(lvBuf);
  end;
  lvBytes[pvBufLength] := 0;
  {$IFDEF MSWINDOWS}
  Result := PAnsiChar(@lvBytes[0]);
  {$ELSE}
  Result := TEncoding.Default.GetString(lvBytes);
  {$ENDIF}

end;

class function TByteTools.crc16(const buf; len:Cardinal): Byte;
var
  lvPtr:PByte;
  iCheckSum:Byte;
  i: Integer;
begin
  // $GPGGA,092108.00,3030.32313974,N,11423.63228885,E,1,28,0.5,149.258,M,-14.263,M,,*4A
  lvPtr := @buf;
  i := len;

  // first
  iCheckSum := Byte(lvPtr^);
  Inc(lvPtr);
  Dec(i);

  while i > 0 do  
  begin
    if i = 1 then
    begin
      i := 1;
    end;
    iCheckSum := iCheckSum xor Byte(lvPtr^);
    Inc(lvPtr);
    Dec(i);
  end;
  Result := iCheckSum;
end;

class function TByteTools.FileToBytes(pvFileName:string): TBytes;
var
  lvStream:TFileStream;
begin
  lvStream := TFileStream.Create(pvFileName, fmOpenRead);
  try
    SetLength(Result, lvStream.Size);
    lvStream.Read(Result[0], lvStream.Size);
  finally
    lvStream.Free;
  end;
end;

class function TByteTools.GetBit(const pvByte:Byte; pvOffset:Byte): Byte;
begin
  Assert(pvOffset in [0..7]);
  Result := pvByte and (1 shl pvOffset) shr pvOffset;
end;

class function TByteTools.GetBitU(pvBuf:Pointer; pvStart:Integer;
    pvLen:Integer): UInt64;
const
  v :UInt64 = 1;
var
  i, j: Integer;
  b :Byte;
begin
//    unsigned int bits=0;
//    int i;
//    for (i=pos;i<pos+len;i++) bits=(bits<<1)+((buff[i/8]>>(7-i%8))&1u);
//    return bits;
  Result := 0;
  for i := pvStart to (pvStart + pvLen - 1) do
  begin
    b := PByte((IntPtr(pvBuf) +trunc(i / 8)))^;
    j := (7 - i mod 8);
    Result := (Result shl 1) + ((b shr j) and v);
  end;
end;

class function TByteTools.GetLow4Bit(const pvByte:Byte): Byte;
begin
  Result := pvByte and $0F;
end;

class function TByteTools.GetUInt64(pvBuf:Pointer; pvStart, pvLen:Integer):
    UInt64;
var
  lvBytes:TBytes;
begin
  Assert(pvLen<=8);
  SetLength(lvBytes, pvLen);
  Move(Pointer(IntPtr(pvBuf) + pvStart)^, lvBytes[0], pvLen);

  Result := PInt64(@lvBytes[0])^;

end;

class function TByteTools.HexStrToBytes(pvHexStr:String): TBytes;
var
  lvStr:String;
  l, r:Integer;
begin
  lvStr := StringReplace(pvHexStr, ' ', '', [rfReplaceAll]);
  lvStr := StringReplace(lvStr, #13, '', [rfReplaceAll]);
  lvStr := StringReplace(lvStr, #10, '', [rfReplaceAll]);
  l := Length(lvStr);
  l := l shr 1;
  SetLength(Result, l);
  r := HexToBin(lvStr, @Result[0]);
  Assert(r = l, 'TByteTools.HexStrToBytes');
end;

class function TByteTools.HexStrToBytes(pvHexStr: String;
  outBuf: Pointer): Integer;
var
  lvStr:String;
  l, r:Integer;
begin
  lvStr := StringReplace(pvHexStr, ' ', '', [rfReplaceAll]);
  lvStr := StringReplace(lvStr, #13, '', [rfReplaceAll]);
  lvStr := StringReplace(lvStr, #10, '', [rfReplaceAll]);
  l := Length(lvStr);
  l := l shr 1;
  r := HexToBin(lvStr, outBuf);
  Assert(r = l, 'TByteTools.HexStrToBytes');
  Result := r;  
end;

class function TByteTools.HexStrToRawString(const pvHexStr: String): string;
var
  lvStr:String;
  lvBytes:TBytes;
var
  l, r:Integer;
begin
  lvStr := StringReplace(pvHexStr, ' ', '', [rfReplaceAll]);
  lvStr := StringReplace(lvStr, #13, '', [rfReplaceAll]);
  lvStr := StringReplace(lvStr, #10, '', [rfReplaceAll]);
  l := Length(lvStr);
  l := l shr 1;
  SetLength(lvBytes, l + 1);
  r := HexToBin(lvStr, @lvBytes[0]);
  lvBytes[Length(lvBytes)] := 0;
  Result := StrPas(PAnsiChar(@lvBytes[0]));
end;

class function TByteTools.HexToBin(pvHexStr: String;
  buf: Pointer): Integer;
var
  l: Integer;
  p, ps: PChar;
  pd: PByte;
begin
  l := Length(pvHexStr);
  p := PChar(pvHexStr);
  ps := p;
  pd := PByte(buf);
  Result := 0;
  while p - ps < l do
  begin
    if IsHexChar(p[0]) and IsHexChar(p[1]) then
    begin
      pd^ := (HexValue(p[0]) shl 4) + HexValue(p[1]);
      inc(Result);
      Inc(pd);
      Inc(p, 2);
      end
    else
    begin
      Exit;
    end;
  end;
end;

class function TByteTools.HexValue(c: Char): Integer;
begin
  if (c >= '0') and (c <= '9') then
    Result := Ord(c) - Ord('0')
  else if (c >= 'a') and (c <= 'f') then
    Result := 10 + Ord(c) - Ord('a')
  else
    Result := 10 + Ord(c) - Ord('A');
end;

class function TByteTools.IsHexChar(c: Char): Boolean;
begin
  Result := ((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or ((c >= 'A') and (c <= 'F'));
end;

class procedure TByteTools.SetBit(var vByte: Byte; pvOffset, pvBitValue: Byte);
begin
  Assert(pvOffset in [0..7]);
  Assert(pvBitValue in [0,1]);

  if pvBitValue = 0 then
    vByte := vByte and Byte(((1 shl pvOffset) xor $FFFFFFFF))
  else
    vByte := vByte or (1 shl pvOffset);
end;

class procedure TByteTools.SetHigh4Bit(var vByte: Byte; pvHigh4Bit: Byte);
begin
  vByte := (vByte and $0F) OR (pvHigh4Bit and $F0);
end;

class procedure TByteTools.SetLow4Bit(var vByte: Byte; pvL4Bit:Byte);
begin
  vByte := (vByte and $F0) OR (pvL4Bit and $0F);
end;

{$if CompilerVersion >= 18} //after delphi 2007
class function TByteTools.swap16(const v): Word;
begin
  // FF, EE : EE->1, FF->2
  PByte(@result)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 1)^ := PByte(@v)^;
end;
{$ifend}

class function TByteTools.swap16(const v: Word): Word;
var
  lvPByte : PByte;
begin
  result := v;
  lvPByte := PByte(@result);
  PByte(lvPByte)^ := byte(v shr 8);
  PByte(IntPtr(lvPByte) + 1)^ := byte(v);
end;


class function TByteTools.swap32(const v: Integer): Integer;
var
  lvPByte : PByte;
begin
  result := v;
  lvPByte := PByte(@result);
  PByte(lvPByte)^ := byte(v shr 24);
  PByte(IntPtr(lvPByte) + 1)^ := byte(v shr 16);
  PByte(IntPtr(lvPByte) + 2)^ := byte(v shr 8);
  PByte(IntPtr(lvPByte) + 3)^ := byte(v);
end;

class function TByteTools.swap64(const v: int64): Int64;
var
  lvPByte : PByte;
begin
  result := v;
  lvPByte := PByte(@result);
  PByte(lvPByte)^ := byte(v shr 56);  //8 * 7
  PByte(IntPtr(lvPByte) + 1)^ := byte(v shr 48); //6
  PByte(IntPtr(lvPByte) + 2)^ := byte(v shr 40); //5
  PByte(IntPtr(lvPByte) + 3)^ := byte(v shr 32); //4
  PByte(IntPtr(lvPByte) + 4)^ := byte(v shr 24); //3
  PByte(IntPtr(lvPByte) + 5)^ := byte(v shr 16); //2
  PByte(IntPtr(lvPByte) + 6)^ := byte(v shr 8); //2
  PByte(IntPtr(lvPByte) + 7)^ := byte(v); //1
end;

class procedure TByteTools.SwapBuff(buf: Pointer; offset, len: Integer);
var
  lvStart, lvEnd: PByte;
  lvByte: Byte;
begin
  lvStart := PByte(buf);
  Inc(lvStart, offset);
  
  lvEnd := lvStart;
  Inc(lvEnd, len - 1);

  while IntPtr(lvStart) < IntPtr(lvEnd) do
  begin
    lvByte := lvStart^;
    lvStart^ := lvEnd^;
    lvEnd^ := lvByte;
    Inc(lvStart);
    Dec(lvEnd);
  end;
end;

class function TByteTools.varToBinaryString(const v; len: Cardinal; Split:
    string = ' '): String;
var
  i, j, l1: Integer;
  v1, b :Byte;
  lvBuf:Pointer;
  lvPtr:PChar;
begin
  l1 := Length(Split);
  SetLength(Result, Integer(len) * 8 + l1 * Integer(len));
  lvPtr := PChar(Result);
  lvBuf := @v;
  for i := 0 to (len * 8 - 1) do
  begin
    b := PByte((IntPtr(lvBuf) +trunc(i / 8)))^;
    j := (7 - i mod 8);
    v1 := Byte((b shr j) and U1);
    if v1 = 1 then lvPtr^ := '1' else lvPtr^ :='0';
    Inc(lvPtr);
    if (l1 > 0) and (i > 0) and ((i mod 8)=7) then
    begin
      {$IFDEF UNICODE}
      Move(PChar(Split)^, lvPtr^, l1 shl 1);
      {$ELSE}
      Move(PChar(Split)^, lvPtr^, l1);
      {$ENDIF}
      Inc(lvPtr, length(Split));
    end;
  end;
end;

class function TByteTools.varToByteString(const v; len: Cardinal; Split: string
    = ' '): String;
var
  lvSource:PByte;
  i: Integer;
begin
  lvSource := PByte(@v);
  for i := 1 to len do
  begin
    Result := Result + IntToStr(lvSource^) + Split;
    Inc(lvSource);
  end;

end;

class function TByteTools.varToHexString(const v; len: Cardinal; Split: string
    = ' '): String;
var
  lvSource:PByte;
  i: Integer;
begin
  Result := '';
  lvSource := PByte(@v);
  for i := 1 to len do
  begin
    Result := Result + IntToHex(lvSource^, 2) + Split;
    Inc(lvSource);
  end;   
end;

class function TByteTools.verifyData(const buf; len: Cardinal): Cardinal;
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

class function TByteTools.verifyStream(pvStream:TStream; len:Cardinal):
    Cardinal;
var
  l, j, m:Cardinal;
  lvBytes:TBytes;
begin
  SetLength(lvBytes, 1024);

  m := pvStream.Size - pvStream.Position;
  if len = 0 then
  begin
    j := m;
  end else
  begin
    j := len;
    if j > m then
    begin
      j := m;
    end;
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
