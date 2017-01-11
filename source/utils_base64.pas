unit utils_base64;

interface

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
  {$DEFINE USE_AnsiStrings}
{$IFEND >=XE5}

uses
  utils_strings, SysUtils, Classes
{$IFDEF USE_AnsiStrings}
  , System.AnsiStrings     
{$ENDIF}

{$IFDEF MSWINDOWS}
    , windows
{$ELSE}
    , System.NetEncoding
{$ENDIF}
  ;

{$if CompilerVersion < 15}  // <D7
type
  UInt64 = int64;//0..18446744073709551615;
{$ifend}



function Base64Encode(pvStream: TStream; CharsPerLine: Integer = 0): string;
    overload;

function Base64Encode(buf: PByte; len: Integer; CharsPerLine: Integer = 0):
    string; overload;
function Base64Encode(pvString: string; CharsPerLine: Integer = 0): string;
    overload;
function Base64Decode(pvSrc:PByte; pvSrcLen:Integer; pvDest:PByte): Integer; overload;
function Base64Decode(pvSrc:PByte; pvSrcLen:Integer; pvOutStream:TStream):
    Integer; overload;
function Base64Decode(pvString:string): string; overload;
function Base64Decode(pvString:string; pvOutStream:TStream): Integer; overload;


/// <summary>
///   预估编码后的长度
/// </summary>
function EstimateEncodeLength(const InputLength: UInt64; CharsPerLine:
    Integer): UInt64;

/// <summary>
///  预估解码后的长度
/// </summary>
function EstimateDecodeLength(const InputLength: UInt64): UInt64;

implementation

type
  Int8    = ShortInt;


const
    LineSeparator : array[0..1] of Byte = (13,10);

    DecodeTable: array[0..79] of Int8 = (
      62,  -1,  -1,  -1,  63,  52,  53,  54,  55,  56,  57, 58, 59, 60, 61, -1,
      -1,  -1,  -2,  -1,  -1,  -1,   0,   1,   2,   3,   4,  5,  6,  7,  8,  9,
      10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20, 21, 22, 23, 24, 25,
      -1,  -1,  -1,  -1,  -1,  -1,  26,  27,  28,  29,  30, 31, 32, 33, 34, 35,
      36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46, 47, 48, 49, 50, 51);

    EncodeTable: array[0..63] of Byte = (
      Ord('A'),Ord('B'),Ord('C'),Ord('D'),Ord('E'),Ord('F'),Ord('G'),Ord('H'),Ord('I'),Ord('J'),Ord('K'),Ord('L'),Ord('M'),
      Ord('N'),Ord('O'),Ord('P'),Ord('Q'),Ord('R'),Ord('S'),Ord('T'),Ord('U'),Ord('V'),Ord('W'),Ord('X'),Ord('Y'),Ord('Z'),
      Ord('a'),Ord('b'),Ord('c'),Ord('d'),Ord('e'),Ord('f'),Ord('g'),Ord('h'),Ord('i'),Ord('j'),Ord('k'),Ord('l'),Ord('m'),
      Ord('n'),Ord('o'),Ord('p'),Ord('q'),Ord('r'),Ord('s'),Ord('t'),Ord('u'),Ord('v'),Ord('w'),Ord('x'),Ord('y'),Ord('z'),
      Ord('0'),Ord('1'),Ord('2'),Ord('3'),Ord('4'),Ord('5'),Ord('6'),Ord('7'),Ord('8'),Ord('9'),Ord('+'),Ord('/'));

  type
    TEncodeStep = (EncodeStepA, EncodeStepB, EncodeStepC);
    TDecodeStep = (DecodeStepA, DecodeStepB, DecodeStepC, DecodeStepD);

    TEncodeState = record
      Step: TEncodeStep;
      Result: Byte;
      StepCount: Integer;
    end;

    TDecodeState = record
      Step: TDecodeStep;
      Result: Byte;
    end;   

procedure InitDecodeState(var State: TDecodeState);
begin
  State.Step := DecodeStepA;
  State.Result := 0;
end;

procedure InitEncodeState(var State: TEncodeState);
begin
  State.Step := EncodeStepA;
  State.Result := 0;
  State.StepCount := 0;
end;


function EncodeValue(const Code: Integer): Byte;
begin
  Result := EncodeTable[Code];
end;

function EstimateDecodeLength(const InputLength: UInt64): UInt64;
begin
  Result := InputLength div 4 * 3 + 4;
end;

function EstimateEncodeLength(const InputLength: UInt64; CharsPerLine:
    Integer): UInt64;
begin
  Result := InputLength div 3 * 4 + 4;
  if CharsPerLine > 0 then
    Result := Result + Result div CharsPerLine * Length(LineSeparator);
end;

function DecodeValue(const Code: Byte): Integer;
var
  LCode: Integer;
begin
  LCode := Code - 43;
  if (LCode < 0) or (LCode > 80) then
    Result := -1
  else
    Result := DecodeTable[LCode];
end;

function DecodeBytes(Input, Output: PByte;
  InputLen: Integer; CharSize: SmallInt; var State: TDecodeState): Integer;
var
  POut: PByte;
  Fragment: Integer;
  P, PEnd: PByte;

begin
  POut := Output;
  P := Input;
  PEnd := P;
  Inc(PEnd, InputLen);
  //PEnd := P + InputLen;
  POut^ := State.Result;
  while True do
  begin
    case State.Step of
      DecodeStepA:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := POut^;
            Result := SpanPointer(Output, POut);
            Exit;
            //Exit(POut - Output);
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        POut^ := (Fragment and $03F) shl 2;
        State.Step := DecodeStepB;
      end;

      DecodeStepB:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := POut^;
            Result := SpanPointer(Output, POut);
            Exit;
            //Exit(POut - Output);
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        POut^ := (POut^ or ((Fragment and $030) shr 4));
        Inc(POut);
        POut^ :=           ((Fragment and $00F) shl 4);
        State.Step := DecodeStepC;
      end;

      DecodeStepC:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := POut^;
            Result := SpanPointer(Output, POut);
            Exit;
            //Exit(POut - Output);
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        POut^ := (POut^ or ((Fragment and $03C) shr 2));
        Inc(POut);
        POut^ :=           ((Fragment and $003) shl 6);
        State.Step := DecodeStepD;
      end;

      DecodeStepD:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := POut^;
            Result := SpanPointer(Output, POut);
            Exit;
            //Exit(POut - Output);
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        POut^ := (POut^ or (Fragment and $03F));
        Inc(POut);
        State.Step := DecodeStepA;
      end;
    end;
  end;
end;

function EncodeBytes(Input, Output: PByte; InputLen: Integer; CharSize:
    SmallInt; LineSeparator: array of Byte; var State: TEncodeState;
    CharsPerLine: Integer): Integer;
var
  B, C: Byte;
  P, PEnd, POut: PByte;
begin
  P := Input;
  PEnd := P;
  Inc(PEnd, InputLen);

  //PEnd := P + InputLen;
  POut := Output;
  C := State.Result;
  while P <> PEnd do
  begin
    case State.Step of
      EncodeStepA:
      begin
        B := P^;
        Inc(P);
        C := (B and $FC) shr 2;
        POut^ := EncodeValue(C);
        Inc(POut, CharSize);
        C := (B and $3) shl 4;
        State.Step := EncodeStepB;
      end;

      EncodeStepB:
      begin
        B := P^;
        Inc(P);
        C := C or (B and $F0) shr 4;
        POut^ := EncodeValue(C);
        Inc(POut, CharSize);
        C := (B and $F) shl 2;
        State.Step := EncodeStepC;
      end;

      EncodeStepC:
      begin
        B := P^;
        Inc(P);
        C := C or (B and $C0) shr 6;
        POut^ := EncodeValue(C);
        Inc(POut, CharSize);
        C := (B and $3F) shr 0;
        POut^ := EncodeValue(C);
        Inc(POut, CharSize);
        Inc(State.StepCount);
        if (CharsPerLine > 0) and (State.StepCount >= CharsPerLine/4)  then
        begin
          Move(LineSeparator[0], POut^, Length(LineSeparator));
          Inc(POut, Length(LineSeparator));
          State.StepCount := 0;
        end;
        State.Step := EncodeStepA;
      end;
    end;
  end;
  State.Result := C;

  Result := SpanPointer(Output, POut);
  //Exit(POut - Output);
end;

function EncodeBytesEnd(Output: PByte; CharSize: SmallInt;
  var State: TEncodeState): Integer;
var
  POut: PByte;
begin
  POut := Output;
  case State.Step of
    EncodeStepB:
    begin
      POut^ := EncodeTable[State.Result];
      Inc(POut, CharSize);
      POut^ := Byte('=');
      Inc(POut, CharSize);
      POut^ := Byte('=');
      Inc(POut, CharSize);
    end;
    EncodeStepC:
    begin
      POut^ := EncodeTable[State.Result];
      Inc(POut, CharSize);
      POut^ := Byte('=');
      Inc(POut, CharSize);
    end;
  end;
  Result := SpanPointer(Output, POut);
end;

function Base64Encode(buf: PByte; len: Integer; CharsPerLine: Integer = 0):
    string;
var
  lvPOut:PByte;
  State: TEncodeState;
  l, r:Integer;
  lvBuilder:TDBufferBuilder;
begin
  lvBuilder := TDBufferBuilder.Create;
  try
    InitEncodeState(State);
    l := EstimateEncodeLength(len, CharsPerLine) + 2;  // #0#0
    lvPOut := lvBuilder.GetLockBuffer(l);  //
    try
      l := 0;
      r := EncodeBytes(buf, lvPOut, len, 1, LineSeparator, State, CharsPerLine);

      Inc(lvPOut, r);
      Inc(l, r);

      // 3
      r := EncodeBytesEnd(lvPOut, 1, State);
      inc(lvPOut, r);
      Inc(l, r);

      lvPOut^ := 0;
      Inc(lvPOut);
      lvPOut^ := 0;
    finally
      lvBuilder.ReleaseLockBuffer(l);
    end;

    {$IFDEF MSWINDOWS}
      {$IFDEF USE_AnsiStrings}
      Result := String(System.AnsiStrings.StrPas(PAnsiChar(lvBuilder.Memory)));
      {$ELSE}
      Result := StrPas(PAnsiChar(lvBuilder.Memory));
      {$ENDIF}
    {$ELSE}
      Result := MarshaledAString(lvBuilder.Memory);
    {$ENDIF}
  finally
    lvBuilder.Free;
  end;
end;

function Base64Encode(pvString: string; CharsPerLine: Integer = 0): string;
var
  lvBytes:TBytes;
begin
  lvBytes := StringToUtf8Bytes(pvString);
  Result := Base64Encode(@lvBytes[0], length(lvBytes), CharsPerLine);
end;

function Base64Decode(pvSrc:PByte; pvSrcLen:Integer; pvDest:PByte): Integer;
var
  State: TDecodeState;
begin
  InitDecodeState(State);
  Result := DecodeBytes(pvSrc, pvDest, pvSrcLen, 1, State);
end;

function Base64Decode(pvString:string): string; overload;
var
  lvBuilder:TDBufferBuilder;
  lvPBuffer:PByte;
  l:Integer;
  {$IFDEF UNICODE}
  lvBytes:TBytes;
  {$ENDIF}
begin
  lvBuilder := TDBufferBuilder.Create;
  try
    l := 0;
    lvPBuffer := lvBuilder.GetLockBuffer(Length(pvString) + 2);
    try
      {$IFDEF UNICODE}
      lvBytes := StringToUtf8Bytes(pvString);
      l := Base64Decode(@lvBytes[0], Length(lvBytes), lvPBuffer);
      {$ELSE}
      l := Base64Decode(PByte(pvString), Length(pvString), lvPBuffer);
      {$ENDIF}
      Inc(lvPBuffer, l);
      
      lvPBuffer^ := 0;
      Inc(lvPBuffer);
      lvPBuffer^ := 0;
    finally
      lvBuilder.ReleaseLockBuffer(l);
    end;
    Result := Utf8BytesToString(lvBuilder.ToBytes, 0);

  finally
    lvBuilder.Free;
  end;
end;

function Base64Encode(pvStream: TStream; CharsPerLine: Integer = 0): string;
const
  BLOCK_SIZE = 8192;
var
  lvPOut:PByte;
  State: TEncodeState;
  l, r, x:Integer;
  lvBuilder:TDBufferBuilder;
  lvBytes:array[0..BLOCK_SIZE -1] of Byte;
begin
  lvBuilder := TDBufferBuilder.Create;
  try
    InitEncodeState(State);
    while true do
    begin
      x := pvStream.Read(lvBytes[0], BLOCK_SIZE);
      if x = 0 then Break;

      l := EstimateEncodeLength(x, CharsPerLine);  // #0#0
      lvPOut := lvBuilder.GetLockBuffer(l);  //
      try
        l := 0;
        r := EncodeBytes(@lvBytes[0], lvPOut, x, 1, LineSeparator, State, CharsPerLine);

        // 真正的长度
        l := r;
      finally
        lvBuilder.ReleaseLockBuffer(l);
      end;
    end;

    l := 0;
    lvPOut := lvBuilder.GetLockBuffer(10);  //
    try
      // 3个字符
      r := EncodeBytesEnd(lvPOut, 1, State);
      inc(lvPOut, r);
      Inc(l, r);

      // 后面弄2个0(避免有其他字符串)
      lvPOut^ := 0;
      Inc(lvPOut);
      lvPOut^ := 0;
    finally
      lvBuilder.ReleaseLockBuffer(l);
    end;

    {$IFDEF MSWINDOWS}
    {$IFDEF USE_AnsiStrings}
    Result := String(System.AnsiStrings.StrPas(PAnsiChar(lvBuilder.Memory)));
    {$ELSE}
    Result := StrPas(PAnsiChar(lvBuilder.Memory));
    {$ENDIF}
    {$ELSE}
    Result := MarshaledAString(lvBuilder.Memory);
    {$ENDIF}
  finally
    lvBuilder.Free;
  end;

end;

function DecodeBuffer(Input: PByte; pvOutStream: TStream; InputLen: Integer;
    CharSize: SmallInt; var State: TDecodeState): Integer;
var
  POut: TStream;
  Fragment: Integer;
  P, PEnd: PByte;
  lvCurrentValue:Byte;
  lvPrePos:Integer;

begin
  lvPrePos := pvOutStream.Position;
  POut := pvOutStream;
  P := Input;
  PEnd := P;
  Inc(PEnd, InputLen);
  //PEnd := P + InputLen;
  lvCurrentValue := State.Result;
  //POut^ := State.Result;
  while True do
  begin
    case State.Step of
      DecodeStepA:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := lvCurrentValue;
            Result := pvOutStream.Position - lvPrePos;
            Exit;
            //Exit(POut - pvOutStream);
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        lvCurrentValue := (Fragment and $03F) shl 2;
        //POut^ := (Fragment and $03F) shl 2;

        State.Step := DecodeStepB;
      end;

      DecodeStepB:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := lvCurrentValue;
            Result := pvOutStream.Position - lvPrePos;
            Exit;
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        lvCurrentValue := (lvCurrentValue or ((Fragment and $030) shr 4));
        POut.Write(lvCurrentValue, 1);
        lvCurrentValue :=                     ((Fragment and $00F) shl 4);
//        POut^ := (POut^ or ((Fragment and $030) shr 4));
//        Inc(POut);
//        POut^ :=           ((Fragment and $00F) shl 4);
        State.Step := DecodeStepC;
      end;

      DecodeStepC:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := lvCurrentValue;
            Result := pvOutStream.Position - lvPrePos;
            Exit;
            //Exit(POut - pvOutStream);
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        lvCurrentValue := (lvCurrentValue or ((Fragment and $03C) shr 2));
        POut.Write(lvCurrentValue, 1);
        lvCurrentValue :=                     ((Fragment and $003) shl 6);

//        POut^ := (POut^ or ((Fragment and $03C) shr 2));
//        Inc(POut);
//        POut^ :=           ((Fragment and $003) shl 6);
        State.Step := DecodeStepD;
      end;

      DecodeStepD:
      begin
        repeat
          if P = PEnd then
          begin
            State.Result := lvCurrentValue;
            Result := pvOutStream.Position - lvPrePos;
            Exit;
            //Exit(POut - pvOutStream);
          end;
          Fragment := DecodeValue(Ord(P^));
          Inc(P, CharSize);
        until (Fragment >= 0) ;
        lvCurrentValue := (lvCurrentValue or (Fragment and $03F));
        POut.Write(lvCurrentValue, 1);

//        POut^ := (POut^ or (Fragment and $03F));
//        Inc(POut);
        State.Step := DecodeStepA;
      end;
    end;
  end;
end;

function Base64Decode(pvString:string; pvOutStream:TStream): Integer; overload;
var
  l :Integer;
  {$IFDEF UNICODE}
  lvBytes:TBytes;
  {$ENDIF}
begin
  {$IFDEF UNICODE}
  lvBytes := StringToUtf8Bytes(pvString);
  l := Base64Decode(@lvBytes[0], Length(lvBytes), pvOutStream);
  {$ELSE}
  l := Base64Decode(PByte(pvString), Length(pvString), pvOutStream);
  {$ENDIF}
  Result := l;
end;

function Base64Decode(pvSrc:PByte; pvSrcLen:Integer; pvOutStream:TStream):
    Integer; overload;
var
  State: TDecodeState;
begin
  InitDecodeState(State);
  Result := DecodeBuffer(pvSrc, pvOutStream, pvSrcLen, 1, State);
end;

end.
