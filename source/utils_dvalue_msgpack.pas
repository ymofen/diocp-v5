unit utils_dvalue_msgpack;

interface

uses
  utils_dvalue, utils_strings, classes,
  {$IFDEF USE_AES}
  AES, 
  {$ENDIF}
  SysUtils, utils_BufferAdapter;

procedure MsgPackEncode(pvInDValue: TDValue; pvOutStream: TStream;
    pvIgnoreTypes: TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);

procedure MsgPackEncodeAES(pvInDValue: TDValue; pvOutStream: TStream; const
    pvKey: string; pvIgnoreTypes: TDValueDataTypes = [vdtInterface, vdtObject,
    vdtPtr]);

procedure MsgPackEncode2File(pvInDValue: TDValue; pvFileName: String;
    pvIgnoreTypes: TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);

procedure MsgPackParseFromStream(pvInStream: TStream; pvOutDValue: TDValue);

procedure MsgPackParseFromAESStream(pvInStream: TStream; pvOutDValue: TDValue;
    const pvKey: string);

procedure MsgPackParseFromBuffer(pvInBuffer:Pointer; pvSize:Integer;
    pvOutDValue: TDValue);

procedure MsgPackParseFromFile(pvFileName: string; pvOutDValue: TDValue);

implementation

resourcestring
  SVariantConvertNotSupport = 'type to convert not support!。';
  SCannotAddChild = 'Can''t add child in this node!';

type
  TMsgPackValue= packed record
    ValueType:Byte;
    case Integer of
      0:(U8Val:Byte);
      1:(I8Val:Shortint);
      2:(U16Val:Word);
      3:(I16Val:Smallint);
      4:(U32Val:Cardinal);
      5:(I32Val:Integer);
      6:(U64Val:UInt64);
      7:(I64Val:Int64);
      //8:(F32Val:Single);
      //9:(F64Val:Double);
      10:(BArray:array[0..16] of Byte);
  end;

  PEncodeObj = ^TEncodeObj;
  TEncodeObj = record
    InValue:TDValue;
    OutValue:TStream;
    IgnoreTypes: TDValueDataTypes;
  end;





procedure EncodeDValue(pvEncodeObj: PEncodeObj); forward;

procedure EncodeMap(pvEncodeObj: PEncodeObj); forward;

function swap16(const v): Word;
begin
  // FF, EE : EE->1, FF->2
  PByte(@result)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 1)^ := PByte(@v)^;
end;

function swap32(const v): Cardinal;
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@result)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 3)^ := PByte(@v)^;
end;

function swap64(const v): Int64;
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@result)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@result) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@result) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 7)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap64Ex(const v; out outVal);
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@outVal)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@outVal) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@outVal) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@outVal) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@outVal) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@outVal) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 7)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap32Ex(const v; out outVal);
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@outVal)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@outVal) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 3)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap16Ex(const v; out outVal);
begin
  // FF, EE : EE->1, FF->2
  PByte(@outVal)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(@v)^;
end;

// overload swap, result type is integer, because single maybe NaN
function swap(v:Single): Integer; overload;
begin
  swap32Ex(v, Result);
end;

// overload swap
function swap(v:word): Word; overload;
begin
  swap16Ex(v, Result);
end;

// overload swap
function swap(v:Cardinal):Cardinal; overload;
begin
  swap32Ex(v, Result);
end;

// swap , result type is Int64, because Double maybe NaN
function swap(v:Double): Int64; overload;
begin
  swap64Ex(v, Result);
end;


// copy from qstring
function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): string;
const
  B2HConvert: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  B2HConvertL: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  pd: PChar;
  pb: PByte;
begin
  if SizeOf(Char) = 2 then
  begin
    SetLength(Result, l shl 1);
  end else
  begin
    SetLength(Result, l);
  end;
  pd := PChar(Result);
  pb := p;
  if ALowerCase then
  begin
    while l > 0 do
    begin
      pd^ := B2HConvertL[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvertL[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end
  else
  begin
    while l > 0 do
    begin
      pd^ := B2HConvert[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvert[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end;
end;



function getFirst(var strPtr: PChar; splitChars: TSysCharSet): string;
var
  oPtr:PChar;
  l:Cardinal;
begin
  oPtr := strPtr;
  Result := '';
  while True do
  begin
    if CharInSet(strPtr^ , splitChars) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
      {$IFDEF UNICODE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l shl 1);
      {$ELSE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l);
      {$ENDIF}
        break;
      end;
    end else if (strPtr^ = #0) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
      {$IFDEF UNICODE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l shl 1);
      {$ELSE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l);
      {$ENDIF}
      end;
      break;
    end;
    Inc(strPtr);
  end;
end;


function Utf8DecodeEx(pvValue:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF}; len:Cardinal):string;
{$IFDEF UNICODE}
var             
  lvBytes:TBytes;
{$ENDIF}
begin
{$IFDEF UNICODE}
  lvBytes := TEncoding.Convert(TEncoding.UTF8, TEncoding.Unicode, pvValue);
  SetLength(Result, Length(lvBytes) shr 1);
  Move(lvBytes[0], PChar(Result)^, Length(lvBytes));
{$ELSE}
  result:= UTF8Decode(pvValue);
{$ENDIF}
end;

function Utf8EncodeEx(pvValue:string):{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
{$IFDEF UNICODE}
var
  lvBytes:TBytes;
  len:Cardinal;
{$ENDIF}
begin
{$IFDEF UNICODE}
  len := length(pvValue) shl 1;
  SetLength(lvBytes, len);
  Move(PChar(pvValue)^, lvBytes[0], len);
  Result := TEncoding.Convert(TEncoding.Unicode, TEncoding.UTF8, lvBytes);
{$ELSE}
  result:= UTF8Encode(pvValue);
{$ENDIF}
end;


// copy from qmsgPack
procedure WriteString(pvValue: string; pvStream: TStream);
var

  lvRawData:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  l:Integer;
  lvValue:TMsgPackValue;
begin
  lvRawData := Utf8EncodeEx(pvValue);
  l:=Length(lvRawData);

  //
  //fixstr stores a byte array whose length is upto 31 bytes:
  //+--------+========+
  //|101XXXXX|  data  |
  //+--------+========+
  //
  //str 8 stores a byte array whose length is upto (2^8)-1 bytes:
  //+--------+--------+========+
  //|  0xd9  |YYYYYYYY|  data  |
  //+--------+--------+========+
  //
  //str 16 stores a byte array whose length is upto (2^16)-1 bytes:
  //+--------+--------+--------+========+
  //|  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
  //+--------+--------+--------+========+
  //
  //str 32 stores a byte array whose length is upto (2^32)-1 bytes:
  //+--------+--------+--------+--------+--------+========+
  //|  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
  //+--------+--------+--------+--------+--------+========+
  //
  //where
  //* XXXXX is a 5-bit unsigned integer which represents N
  //* YYYYYYYY is a 8-bit unsigned integer which represents N
  //* ZZZZZZZZ_ZZZZZZZZ is a 16-bit big-endian unsigned integer which represents N
  //* AAAAAAAA_AAAAAAAA_AAAAAAAA_AAAAAAAA is a 32-bit big-endian unsigned integer which represents N
  //* N is the length of data

  if L<=31 then
  begin
    lvValue.ValueType:=$A0+Byte(L);
    pvStream.WriteBuffer(lvValue.ValueType,1);
  end
  else if L<=255 then
  begin
    lvValue.ValueType:=$d9;
    lvValue.U8Val:=Byte(L);
    pvStream.WriteBuffer(lvValue,2);
  end
  else if L<=65535 then
  begin
    lvValue.ValueType:=$da;
    lvValue.U16Val:=((L shr 8) and $FF) or ((L shl 8) and $FF00);
    pvStream.Write(lvValue,3);
  end else
  begin
    lvValue.ValueType:=$db;
    lvValue.BArray[0]:=(L shr 24) and $FF;
    lvValue.BArray[1]:=(L shr 16) and $FF;
    lvValue.BArray[2]:=(L shr 8) and $FF;
    lvValue.BArray[3]:=L and $FF;
    pvStream.WriteBuffer(lvValue,5);
  end;

  {$IFDEF UNICODE}
  pvStream.Write(PByte(@lvRawData[0])^, l);
  {$ELSE}
  pvStream.Write(PByte(lvRawData)^, l);
  {$ENDIF};

end;

procedure WriteBinary(p: PByte; l: Integer; pvStream: TStream);
var
  lvValue:TMsgPackValue;
begin
  if l <= 255 then
  begin
    lvValue.ValueType := $C4;
    lvValue.U8Val := Byte(l);
    pvStream.WriteBuffer(lvValue, 2);
  end
  else if l <= 65535 then
  begin
    lvValue.ValueType := $C5;
    lvValue.BArray[0] := (l shr 8) and $FF;
    lvValue.BArray[1] := l and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $C6;
    lvValue.BArray[0] := (l shr 24) and $FF;
    lvValue.BArray[1] := (l shr 16) and $FF;
    lvValue.BArray[2] := (l shr 8) and $FF;
    lvValue.BArray[3] := l and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;
  pvStream.WriteBuffer(p^, l);
end;

// copy from qmsgPack
procedure WriteInt(const iVal: Int64; AStream: TStream);
var
  lvValue:TMsgPackValue;
begin
  if iVal>=0 then
    begin
    if iVal<=127 then
      begin
      lvValue.U8Val:=Byte(iVal);
      AStream.WriteBuffer(lvValue.U8Val,1);
      end
    else if iVal<=255 then//UInt8
      begin
      lvValue.ValueType:=$cc;
      lvValue.U8Val:=Byte(iVal);
      AStream.WriteBuffer(lvValue,2);
      end
    else if iVal<=65535 then
      begin
      lvValue.ValueType:=$cd;
      lvValue.BArray[0]:=(iVal shr 8);
      lvValue.BArray[1]:=(iVal and $FF);
      AStream.WriteBuffer(lvValue,3);
      end
    else if iVal<=Cardinal($FFFFFFFF) then
      begin
      lvValue.ValueType:=$ce;
      lvValue.BArray[0]:=(iVal shr 24) and $FF;
      lvValue.BArray[1]:=(iVal shr 16) and $FF;
      lvValue.BArray[2]:=(iVal shr 8) and $FF;
      lvValue.BArray[3]:=iVal and $FF;
      AStream.WriteBuffer(lvValue,5);
      end
    else
      begin
      lvValue.ValueType:=$cf;
      lvValue.BArray[0]:=(iVal shr 56) and $FF;
      lvValue.BArray[1]:=(iVal shr 48) and $FF;
      lvValue.BArray[2]:=(iVal shr 40) and $FF;
      lvValue.BArray[3]:=(iVal shr 32) and $FF;
      lvValue.BArray[4]:=(iVal shr 24) and $FF;
      lvValue.BArray[5]:=(iVal shr 16) and $FF;
      lvValue.BArray[6]:=(iVal shr 8) and $FF;
      lvValue.BArray[7]:=iVal and $FF;
      AStream.WriteBuffer(lvValue,9);
      end;
    end
  else//<0
    begin
    if iVal<=Low(Integer) then  //-2147483648  // 64 bit
    begin
      lvValue.ValueType:=$d3;
      lvValue.BArray[0]:=(iVal shr 56) and $FF;
      lvValue.BArray[1]:=(iVal shr 48) and $FF;
      lvValue.BArray[2]:=(iVal shr 40) and $FF;
      lvValue.BArray[3]:=(iVal shr 32) and $FF;
      lvValue.BArray[4]:=(iVal shr 24) and $FF;
      lvValue.BArray[5]:=(iVal shr 16) and $FF;
      lvValue.BArray[6]:=(iVal shr 8) and $FF;
      lvValue.BArray[7]:=iVal and $FF;
      AStream.WriteBuffer(lvValue,9);
    end
    else if iVal<=Low(SmallInt) then     // -32768    // 32 bit
      begin
      lvValue.ValueType:=$d2;
      lvValue.BArray[0]:=(iVal shr 24) and $FF;
      lvValue.BArray[1]:=(iVal shr 16) and $FF;
      lvValue.BArray[2]:=(iVal shr 8) and $FF;
      lvValue.BArray[3]:=iVal and $FF;
      AStream.WriteBuffer(lvValue,5);
      end
    else if iVal<=-128 then
      begin
      lvValue.ValueType:=$d1;
      lvValue.BArray[0]:=(iVal shr 8);
      lvValue.BArray[1]:=(iVal and $FF);
      AStream.WriteBuffer(lvValue,3);
      end
    else if iVal<-32 then
      begin
      lvValue.ValueType:=$d0;
      lvValue.I8Val:=iVal;
      AStream.WriteBuffer(lvValue,2);
      end
    else
      begin
      lvValue.I8Val:=iVal;
      AStream.Write(lvValue.I8Val,1);
      end;
    end;//End <0
end;

procedure WriteFloat(pvVal: Double; AStream: TStream);
var
  lvValue:TMsgPackValue;
begin   
  lvValue.i64Val := swap(pvVal);
  lvValue.ValueType := $CB;
  AStream.WriteBuffer(lvValue, 9);
end;

procedure WriteSingle(pvVal: Single; AStream: TStream);
var
  lvValue:TMsgPackValue;
begin
  lvValue.I32Val := swap(pvVal);
  lvValue.ValueType := $CA;
  AStream.WriteBuffer(lvValue, 5);
end;

procedure WriteNull(pvStream:TStream);
var
  lvByte:Byte;
begin
  lvByte := $C0;
  pvStream.Write(lvByte, 1);
end;

procedure WriteBoolean(pvValue:Boolean; pvStream:TStream);
var
  lvByte:Byte;
begin
  if pvValue then lvByte := $C3 else lvByte := $C2;
  pvStream.Write(lvByte, 1);
end;


procedure EncodeArray(pvEncodeObj: PEncodeObj);
var
  c, i:Integer;
  lvValue:TMsgPackValue;
  lvNode:TDValue;
  obj:TDValue;
  pvStream:TStream;
begin
  obj := pvEncodeObj.InValue;
  pvStream := pvEncodeObj.OutValue;
  C:=obj.Count;

  if C <= 15 then
  begin
    lvValue.ValueType := $90 + C;
    pvStream.WriteBuffer(lvValue.ValueType, 1);
  end
  else if C <= 65535 then
  begin
    lvValue.ValueType := $DC;
    lvValue.BArray[0] := (C shr 8) and $FF;
    lvValue.BArray[1] := C and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $DD;
    lvValue.BArray[0] := (C shr 24) and $FF;
    lvValue.BArray[1] := (C shr 16) and $FF;
    lvValue.BArray[2] := (C shr 8) and $FF;
    lvValue.BArray[3] := C and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;

  for I := 0 to C-1 do
  begin
    lvNode:=obj.Items[i];
    pvEncodeObj.InValue := lvNode;
    EncodeDValue(pvEncodeObj);
  end;
  pvEncodeObj.InValue := obj;
end;


procedure WriteDValueItem(pvValue: TDValueItem; pvStream: TStream);
begin
  case pvValue.DataType of
    vdtUnset: WriteNull(pvStream);
    vdtNull: WriteNull(pvStream);
    vdtBoolean: WriteBoolean(pvValue.AsBoolean, pvStream);
    vdtSingle: WriteSingle(pvValue.AsFloat, pvStream);
    vdtFloat: WriteFloat(pvValue.AsFloat, pvStream);
    vdtInteger, vdtInt64: WriteInt(pvValue.AsInteger, pvStream);
    vdtCurrency: WriteFloat(pvValue.AsFloat, pvStream);
    vdtGuid: WriteString(pvValue.AsString, pvStream);
    vdtDateTime: WriteFloat(pvValue.AsFloat, pvStream);
    vdtString: WriteString(pvValue.AsString, pvStream);
    vdtStringW: WriteString(pvValue.AsString, pvStream);
    vdtStream:
      begin
        WriteBinary(PByte(pvValue.AsStream.Memory), pvValue.AsStream.Size, pvStream);
      end;
    vdtInterface: ;
    vdtPtr: ;
    vdtObject: ;
    vdtArray: ;
  end;
  
//
//      mptString: WriteString(Self.getAsString, pvStream);
//      mptInteger: WriteInt(self.getAsInteger, pvStream);
//      mptBoolean: WriteBoolean(self.GetAsBoolean, pvStream);
//      mptDateTime, mptFloat: WriteFloat(GetAsFloat, pvStream);
//      mptSingle: WriteSingle(GetAsSingle, pvStream);
//      mptBinary: WriteBinary(PByte(@FValue[0]), Length(FValue), pvStream);
end;


procedure EncodeDValue(pvEncodeObj: PEncodeObj);
var
  lvInValue:TDValue;
  pvStream:TStream;
begin
  lvInValue := pvEncodeObj.InValue;
  pvStream := pvEncodeObj.OutValue;
  case lvInValue.ObjectType of
    vntNull: WriteNull(pvStream);
    vntObject: EncodeMap(pvEncodeObj);
    vntArray: EncodeArray(pvEncodeObj);
    vntValue:
    begin
      if not (lvInValue.Value.DataType in pvEncodeObj.IgnoreTypes) then
        WriteDValueItem(lvInValue.Value, pvStream)
      else
        WriteNull(pvStream);   // 名称写入了(值也要写一个)
    end;
  end;
  pvEncodeObj.InValue := lvInValue;
end;


procedure EncodeMap(pvEncodeObj: PEncodeObj);
var
  c, i:Integer;
  lvValue:TMsgPackValue;
  lvNode:TDValue;
  lvInValue:TDValue;
begin
  lvInValue := pvEncodeObj.InValue;
  C:=lvInValue.Count;
  if C<=15 then
  begin
    lvValue.ValueType:=$80+C;
    pvEncodeObj.OutValue.WriteBuffer(lvValue.ValueType,1);
  end
  else if C<=65535 then
  begin
    lvValue.ValueType:=$de;
    lvValue.BArray[0]:=(C shr 8) and $FF;
    lvValue.BArray[1]:=C and $FF;
    pvEncodeObj.OutValue.WriteBuffer(lvValue,3);
  end
  else
  begin
    lvValue.ValueType:=$df;
    lvValue.BArray[0]:=(C shr 24) and $FF;
    lvValue.BArray[1]:=(C shr 16) and $FF;
    lvValue.BArray[2]:=(C shr 8) and $FF;
    lvValue.BArray[3]:=C and $FF;
    pvEncodeObj.OutValue.WriteBuffer(lvValue,5);
  end;
  for I := 0 to C-1 do
  begin
    lvNode:=lvInValue.Items[i];
    WriteDValueItem(lvNode.Name, pvEncodeObj.OutValue);
    pvEncodeObj.InValue := lvNode;
    EncodeDValue(pvEncodeObj);
  end;
  pvEncodeObj.InValue := lvInValue;
end;

procedure MsgPackEncode(pvInDValue: TDValue; pvOutStream: TStream;
    pvIgnoreTypes: TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);
var
  lvEncodeObj:TEncodeObj;
begin
  lvEncodeObj.InValue := pvInDValue;
  lvEncodeObj.OutValue := pvOutStream;
  lvEncodeObj.IgnoreTypes := pvIgnoreTypes;
  EncodeDValue(@lvEncodeObj);
end;

procedure InnerParseFromStream(pvInStream: TStream; pvOutDValue: TDValue);
var
  lvByte:Byte;
  lvBData: array[0..15] of Byte;
  lvSwapData: array[0..7] of Byte;
  lvAnsiStr:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  l, i:Cardinal;
  i64 :Int64;
  lvObj:TDValue;

  procedure __innerReadString(l:Integer);
  var
    lvAnsiStr:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  begin
    if l > 0 then  // check is empty ele
    begin
      SetLength(lvAnsiStr, l);
      {$IFDEF UNICODE}
      pvInStream.Read(lvAnsiStr[0], l);
      {$ELSE}
      pvInStream.Read(PAnsiChar(lvAnsiStr)^, l);
      {$ENDIF};
      pvOutDValue.AsString := (UTF8DecodeEx(lvAnsiStr, l));
    end else
    begin
      pvOutDValue.AsString := '';
    end;
  end;

  procedure __innerReadMapObject(l:Integer);
  var
    i:Integer;
  begin
    pvOutDValue.Clear;
    pvOutDValue.CheckSetNodeType(vntObject);
    if l > 0 then  // check is empty ele
    begin
      for I := 0 to l - 1 do
      begin
        lvObj := pvOutDValue.Add;

        // map key
        InnerParseFromStream(pvInStream, lvObj);
        lvObj.Name.AsString := lvObj.AsString;

          // value
        InnerParseFromStream(pvInStream, lvObj);
      end;
    end;
  end;

  procedure __innerReadArray(l:Integer);
  var
    i:Integer;
  begin
    pvOutDValue.Clear;
    pvOutDValue.CheckSetNodeType(vntArray);
    if l > 0 then  // check is empty ele
    begin
      for I := 0 to l - 1 do
      begin
        lvObj := pvOutDValue.AddArrayChild;

        // value
        InnerParseFromStream(pvInStream, lvObj);
      end;
    end;
  end;

  procedure SetAsInteger(v:Int64);
  begin
    pvOutDValue.AsInteger := v;
  end;
begin
  pvInStream.Read(lvByte, 1);
  if lvByte in [$00 .. $7F] then   //positive fixint	0xxxxxxx	0x00 - 0x7f
  begin
    //  +--------+
    //  |0XXXXXXX|
    //  +--------+
    pvOutDValue.AsInteger := lvByte;
  end else if lvByte in [$80 .. $8F] then //fixmap	1000xxxx	0x80 - 0x8f
  begin
    l := lvByte - $80;
    __innerReadMapObject(l);
  end else if lvByte in [$90 .. $9F] then //fixarray	1001xxxx	0x90 - 0x9f
  begin
    l := lvByte - $90;
    __innerReadArray(l);
  end else if lvByte in [$A0 .. $BF] then //fixstr	101xxxxx	0xa0 - 0xbf
  begin
    l := lvByte - $A0;   // str len
    __innerReadString(l);
  end else if lvByte in [$E0 .. $FF] then
  begin
    //  negative fixnum stores 5-bit negative integer
    //  +--------+
    //  |111YYYYY|
    //  +--------+
    pvOutDValue.AsInteger := Shortint(lvByte);
  end else
  begin
    case lvByte of
      $C0: // null
        begin
          pvOutDValue.Clear;
        end;
      $C1: // (never used)
        raise Exception.Create('(never used) type $c1');
      $C2: // False
        begin
          pvOutDValue.AsBoolean :=False;
        end;
      $C3: // True
        begin
          pvOutDValue.AsBoolean := True;
        end;
      $C4: // 短二进制，最长255字节
        begin
          l := 0; // fill zero
          pvInStream.Read(l, 1);

          pvOutDValue.AsStream.SetSize(l);
          pvInStream.Read(pvOutDValue.AsStream.Memory^, l);
        end;
      $C5: // 二进制，16位，最长65535B
        begin
          l := 0; // fill zero
          pvInStream.Read(l, 2);
          l := swap16(l);

          pvOutDValue.AsStream.SetSize(l);
          pvInStream.Read(pvOutDValue.AsStream.Memory^, l);
        end;
      $C6: // 二进制，32位，最长2^32-1
        begin
          l := 0; // fill zero
          pvInStream.Read(l, 4);
          l := swap32(l);

          pvOutDValue.AsStream.SetSize(l);
          pvInStream.Read(pvOutDValue.AsStream.Memory^, l);
        end;
      $c7,$c8,$c9:      //ext 8	11000111	0xc7, ext 16	11001000	0xc8, ext 32	11001001	0xc9
        begin
          raise Exception.Create('(ext8,ext16,ex32) type $c7,$c8,$c9');
        end;
      $CA: // float 32
        begin
          pvInStream.Read(lvBData[0], 4);

          swap32Ex(lvBData[0], lvSwapData[0]);

          pvOutDValue.AsFloat := PSingle(@lvSwapData[0])^;
        end;
      $cb: // Float 64
        begin

          pvInStream.Read(lvBData[0], 8);

          // swap to int64, and lvBData is not valid double value (for IEEE)
          i64 := swap64(lvBData[0]);

          //
          pvOutDValue.AsFloat := PDouble(@i64)^;

         // AsFloat := swap(PDouble(@lvBData[0])^);
        end;
      $cc: // UInt8
        begin
          //      uint 8 stores a 8-bit unsigned integer
          //      +--------+--------+
          //      |  0xcc  |ZZZZZZZZ|
          //      +--------+--------+
          l := 0;
          pvInStream.Read(l, 1);
          pvOutDValue.AsInteger := l;
        end;
      $cd:
        begin
          //    uint 16 stores a 16-bit big-endian unsigned integer
          //    +--------+--------+--------+
          //    |  0xcd  |ZZZZZZZZ|ZZZZZZZZ|
          //    +--------+--------+--------+
          l := 0;
          pvInStream.Read(l, 2);
          l := swap16(l);
          pvOutDValue.AsInteger := Word(l);
        end;
      $ce:
        begin
          //  uint 32 stores a 32-bit big-endian unsigned integer
          //  +--------+--------+--------+--------+--------+
          //  |  0xce  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ
          //  +--------+--------+--------+--------+--------+
          l := 0;
          pvInStream.Read(l, 4);
          l := swap32(l);
          pvOutDValue.AsInteger :=Cardinal(l);
        end;
      $cf:
        begin
          // delphi中没有uint64
          //  uint 64 stores a 64-bit big-endian unsigned integer
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          //  |  0xcf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          i64 := 0;
          pvInStream.Read(i64, 8);
          i64 := swap64(i64);
          pvOutDValue.AsInteger :=i64;
        end;
      $dc: // array 16
        begin
          //      +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          //      |  0xdc  |YYYYYYYY|YYYYYYYY|    N objects    |
          //      +--------+--------+--------+~~~~~~~~~~~~~~~~~+


          l := 0; // fill zero
          pvInStream.Read(l, 2);

          l := swap16(l);
          __innerReadArray(l);
        end;
      $dd: // Array 32
        begin
        //  +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
        //  |  0xdd  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|    N objects    |
        //  +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+

          l := 0; // fill zero
          pvInStream.Read(l, 4);


          l := swap32(l);
          __innerReadArray(l);
        end;
      $d9:   //str 8 , 255
        begin
          //  str 8 stores a byte array whose length is upto (2^8)-1 bytes:
          //  +--------+--------+========+
          //  |  0xd9  |YYYYYYYY|  data  |
          //  +--------+--------+========+
          l := 0;
          pvInStream.Read(l, 1);
          __innerReadString(l);
  //        SetLength(lvBytes, l + 1);
  //        lvBytes[l] := 0;
  //        pvInStream.Read(lvBytes[0], l);
  //        setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
        end;
      $DE: // Object map 16
        begin
          //    +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          //    |  0xde  |YYYYYYYY|YYYYYYYY|   N*2 objects   |
          //    +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          l := 0; // fill zero
          pvInStream.Read(l, 2);
          l := swap16(l);
          __innerReadMapObject(l);
        end;
      $DF: //Object map 32
        begin
          //    +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
          //    |  0xdf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|   N*2 objects   |
          //    +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+

          l := 0; // fill zero
          pvInStream.Read(l, 4);

          l := swap32(l);
          __innerReadMapObject(l);
        end;
      $da:    // str 16
        begin
          //      str 16 stores a byte array whose length is upto (2^16)-1 bytes:
          //      +--------+--------+--------+========+
          //      |  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
          //      +--------+--------+--------+========+

          l := 0; // fill zero
          pvInStream.Read(l, 2);
          l := swap16(l);
          __innerReadString(l);
        end;
      $db:    // str 16
        begin
          //  str 32 stores a byte array whose length is upto (2^32)-1 bytes:
          //  +--------+--------+--------+--------+--------+========+
          //  |  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
          //  +--------+--------+--------+--------+--------+========+

          l := 0; // fill zero
          pvInStream.Read(l, 4);
          l := swap32(l);
          __innerReadString(l);
        end;
      $d0:   //int 8
        begin
          //      int 8 stores a 8-bit signed integer
          //      +--------+--------+
          //      |  0xd0  |ZZZZZZZZ|
          //      +--------+--------+

          l := 0;
          pvInStream.Read(l, 1);
          SetAsInteger(ShortInt(l));
        end;
      $d1:
        begin
          //    int 16 stores a 16-bit big-endian signed integer
          //    +--------+--------+--------+
          //    |  0xd1  |ZZZZZZZZ|ZZZZZZZZ|
          //    +--------+--------+--------+

          l := 0;
          pvInStream.Read(l, 2);
          l := swap16(l);
          SetAsInteger(SmallInt(l));
        end;

      $d2:
        begin
          //  int 32 stores a 32-bit big-endian signed integer
          //  +--------+--------+--------+--------+--------+
          //  |  0xd2  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          //  +--------+--------+--------+--------+--------+
          l := 0;
          pvInStream.Read(l, 4);
          l := swap32(l);
          setAsInteger(Integer(l));
        end;
      $d3:
      begin
        //  int 64 stores a 64-bit big-endian signed integer
        //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
        //  |  0xd3  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
        //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
        i64 := 0;
        pvInStream.Read(i64, 8);
        i64 := swap64(i64);
        setAsInteger(Int64(i64));
      end;   
    end;
  end;
  
end;

procedure MsgPackParseFromStream(pvInStream: TStream; pvOutDValue: TDValue);
begin
  InnerParseFromStream(pvInStream, pvOutDValue);
end;

procedure MsgPackParseFromFile(pvFileName: string; pvOutDValue: TDValue);
var
  lvFileStream:TFileStream;
begin
  lvFileStream := TFileStream.Create(pvFileName, fmOpenRead);
  try
    MsgPackParseFromStream(lvFileStream, pvOutDValue);
  finally
    lvFileStream.Free;
  end;
  
end;

procedure MsgPackParseFromBuffer(pvInBuffer:Pointer; pvSize:Integer;
    pvOutDValue: TDValue);
var
  lvStream:TBufferAdapterStream;
begin
  lvStream:=TBufferAdapterStream.Create(pvInBuffer, pvSize);
  try
    MsgPackParseFromStream(lvStream, pvOutDValue);
  finally
    lvStream.Free;    
  end;
end;

procedure MsgPackEncode2File(pvInDValue: TDValue; pvFileName: String;
    pvIgnoreTypes: TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);
var
  lvFileStream:TFileStream;
begin
  lvFileStream := TFileStream.Create(pvFileName, fmCreate);
  try
    MsgPackEncode(pvInDValue, lvFileStream, pvIgnoreTypes);
  finally
    lvFileStream.Free;
  end;                                                     
end;

procedure MsgPackEncodeAES(pvInDValue: TDValue; pvOutStream: TStream; const
    pvKey: string; pvIgnoreTypes: TDValueDataTypes = [vdtInterface, vdtObject,
    vdtPtr]);
var
  lvSrc:TStream;
  lvDest:TMemoryStream;
begin
  {$IFDEF USE_AES}
  lvSrc := TMemoryStream.Create;
  try
    MsgPackEncode(pvInDValue, lvSrc);
    lvSrc.Position := 0;
    EncryptStream(lvSrc, pvKey, pvOutStream);
  finally
    lvSrc.Free;
  end;
  {$ELSE}
  Assert(false, '工程中需要定义编译宏 USE_AES');
  {$ENDIF}
end;

procedure MsgPackParseFromAESStream(pvInStream: TStream; pvOutDValue: TDValue;
    const pvKey: string);
var
  lvDest:TMemoryStream;
begin
  {$IFDEF USE_AES}
  lvDest := TMemoryStream.Create;
  try
    lvDest.Position := 0;
    DecryptStream(pvInStream, pvKey, lvDest);
    lvDest.Position := 0;
    MsgPackParseFromStream(lvDest, pvOutDValue);
  finally
    lvDest.Free;
  end;
  {$ELSE}
  Assert(false, '工程中需要定义编译宏 USE_AES');
  {$ENDIF}
end;

end.
