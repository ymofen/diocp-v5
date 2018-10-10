unit utils_dvalue_json;

// #34  "
// #39  '
// #32  空格
// #58  :
// #9   TAB

interface

uses
  utils_dvalue, utils_strings, classes, Math, utils_textfile;

type
  TByteChar = record
  case integer of
    0: (a, b: Byte);
    1: (c: WideChar);
  end;
  TJsonParser = class(TObject)
  private
    FLastStrValue: String;
    FLastValue: String;
  end;

  PJsonBuffer = ^TJsonBuffer;
  TJsonBuffer = record
    FBuffer:TStream;
    FLevel:Integer;
    FState:Integer;  // 0 search begin, 1, search end, 2: String, 3: escape
    FBeginChr: array [0..32] of Byte;
    FEndChr: array [0..32] of Byte;
    FStringEndChr:Byte;

  end;

function JSONParser(const s: string; pvDValue: TDValue): Integer;
function JSONEncode(v: TDValue; ADoEscape: Boolean = true; ADoFormat: Boolean =
    true; pvExceptValueTypes: TDValueDataTypes = [vdtInterface, vdtObject,
    vdtPtr]): String;

function JSONParseFromUtf8NoBOMFile(pvFile:string; pvDValue:TDValue): Integer;
function JSONParseFromFile(pvFile:string; pvDValue:TDValue): Integer;
procedure JSONWriteToUtf8NoBOMFile(const pvFile: string; pvDValue: TDValue);

function InputJsonBuffer(const jsonBuffer:PJsonBuffer; pvByte:Integer): Integer;
procedure ResetJsonBuffer(const jsonBuffer:PJsonBuffer);


procedure JSONEscapeWithoutDoEscape(ABuilder: TDStringBuilder; const S: String);

procedure JSONEscape(ABuilder: TDStringBuilder; const S: DStringW; ADoEscape: Boolean);

function StringJSONEscape(const s: String; ADoEscape: Boolean = false): String;


    
implementation

uses
  SysUtils;



resourcestring
  SCharNeeded = '当前位置应该是 "%s" ，而不是 "%s"。';

function JSONParseEx(var ptrData: PChar; pvDValue: TDValue; pvParser:
    TJsonParser; pvBuilder:TDStringBuilder): Integer; forward;

function JSONSkipSpaceAndComment(var ptrData: PChar; pvParser: TJsonParser):
    Integer;forward;

function CreateIndentBlock(pvLevel: Integer; pvBlockSize: Integer = 4): String;
var
  l:Integer;
  i: Integer;
  lvPtr:PChar;
begin
  l := pvLevel * pvBlockSize;
  SetLength(Result, l);
  
  i := 0;
  lvPtr := PChar(Result);
  while i < l do
  begin
    lvPtr^ := ' ';
    inc(lvPtr);
    inc(i);
  end;
end;

procedure JSONEscape(ABuilder: TDStringBuilder; const S: DStringW; ADoEscape:
    Boolean);
var
  ps: PDCharW;
const
  CharNum1: String = '1';
  CharNum0: String = '0';
  Char7: String = '\b';
  Char9: String = '\t';
  Char10: String = '\n';
  Char12: String = '\f';
  Char13: String = '\r';
  CharQuoter: String = '\"';
  CharBackslash: String = '\\';
  CharCode: String = '\u00';
  CharEscape: String = '\u';
begin
  ps := PDCharW(S);
  while ps^ <> #0 do
  begin
    case ps^ of
      #7:
        ABuilder.Append(Char7);
      #9:
        ABuilder.Append(Char9);
      #10:
        ABuilder.Append(Char10);
      #12:
        ABuilder.Append(Char12);
      #13:
        ABuilder.Append(Char13);
      '\':
        ABuilder.Append(CharBackslash);
      '"':
        ABuilder.Append(CharQuoter);
    else
      begin
        if (not ADoEscape) then
        begin
          ABuilder.Append(ps^);
        end else if ps^ < #$1F then
        begin
          ABuilder.Append(CharCode);
          if ps^ > #$F then
            ABuilder.Append(CharNum1)
          else
            ABuilder.Append(CharNum0);
          ABuilder.Append(HexChar(Ord(ps^) and $0F));
        end else if (ps^ <= #$7E) then // 英文字符区
          ABuilder.Append(ps^)
        else
          ABuilder.Append(CharEscape).Append(HexChar((PWord(ps)^ shr 12) and $0F))
            .Append(HexChar((PWord(ps)^ shr 8) and $0F))
            .Append(HexChar((PWord(ps)^ shr 4) and $0F))
            .Append(HexChar(PWord(ps)^ and $0F));
      end;
    end;
    Inc(ps);
  end;
end;

procedure JSONEscapeWithoutDoEscape(ABuilder: TDStringBuilder; const S: String);
var
  ps: PChar;
const
  CharNum1: String = '1';
  CharNum0: String = '0';
  Char7: String = '\b';
  Char9: String = '\t';
  Char10: String = '\n';
  Char12: String = '\f';
  Char13: String = '\r';
  CharQuoter: String = '\"';
  CharBackslash: String = '\\';
  CharCode: String = '\u00';
  CharEscape: String = '\u';
begin
  ps := PChar(S);
  while ps^ <> #0 do
  begin
    case ps^ of
      #7:
        ABuilder.Append(Char7);
      #12:
        ABuilder.Append(Char12);
      '\':
        ABuilder.Append(CharBackslash);
      '"':
        ABuilder.Append(CharQuoter);
    else
      begin
        ABuilder.Append(ps^);
      end;
    end;
    Inc(ps);
  end;
end;

function JSONEncodeEx(v: TDValue; pvBuilder: TDStringBuilder; pvLevel: Integer;
    ADoEscape, ADoFormat: Boolean; pvExceptValueTypes: TDValueDataTypes):
    Integer;
var
  i, r:Integer;
  lvIndentStr, lvChildIndentStr, lvName:String;
begin
  Result := 0;
  if ADoFormat then
  begin
    lvIndentStr := CreateIndentBlock(pvLevel);
    lvChildIndentStr := CreateIndentBlock(pvLevel + 1);
  end else
  begin
    lvIndentStr := '';
    lvChildIndentStr := '';
  end;
  if (v.ObjectType = vntObject) then
  begin
    if pvLevel > 0 then
    begin      // 首层不需要name
      lvName := v.Name.AsString;
      if Length(lvName) <> 0 then
      begin
        pvBuilder.Append('"');
        JSONEscape(pvBuilder, v.Name.AsString, ADoEscape);
        pvBuilder.Append('"');
        pvBuilder.Append(':');
      end;
    end;
    pvBuilder.Append('{');
    if ADoFormat then pvBuilder.Append(sLineBreak);
    for i := 0 to v.Count - 1 do
    begin
      if ADoFormat then pvBuilder.Append(lvChildIndentStr);
      r := JSONEncodeEx(v.Items[i], pvBuilder, pvLevel + 1, ADoEscape, ADoFormat, pvExceptValueTypes);
      if r = -1 then
      begin
        //noop
      end else
      begin
        if i < v.Count -1 then
        begin
          pvBuilder.Append(',');
        end;
        if ADoFormat then pvBuilder.Append(sLineBreak);
      end;
    end;
    if ADoFormat then pvBuilder.Append(lvIndentStr);
    pvBuilder.Append('}');
  end else if v.ObjectType = vntArray then
  begin
    if pvLevel > 0 then
    begin      // 首层不需要name
      lvName := v.Name.AsString;
      if Length(lvName) <> 0 then
      begin
        pvBuilder.Append('"');
        JSONEscape(pvBuilder, v.Name.AsString, ADoEscape);
        pvBuilder.Append('"');
        pvBuilder.Append(':');
      end;
    end;
    pvBuilder.Append('[');
    if ADoFormat then pvBuilder.Append(sLineBreak);
    for i := 0 to v.Count - 1 do
    begin
      if ADoFormat then pvBuilder.Append(lvChildIndentStr);
      r := JSONEncodeEx(v.Items[i], pvBuilder, pvLevel + 1, ADoEscape, ADoFormat, pvExceptValueTypes);
      if r = -1 then
      begin
        ;
      end else
      begin
        if i < v.Count -1 then
        begin
          pvBuilder.Append(',');
        end;
        if ADoFormat then pvBuilder.Append(sLineBreak);
      end;
    end;
    if ADoFormat then pvBuilder.Append(lvIndentStr);
    pvBuilder.Append(']');
  end else if (v.ObjectType = vntValue) then
  begin
    if not (v.Value.DataType in pvExceptValueTypes) then
    begin    
      if v.Name.AsString <> '' then
      begin                      
        pvBuilder.Append('"');
        JSONEscape(pvBuilder, v.Name.AsString, ADoEscape);
        pvBuilder.Append('"');
        pvBuilder.Append(':');
      end;
      if v.Value.DataType in [vdtString, vdtStringW, vdtPtr, vdtObject, vdtGuid, vdtDateTime] then
      begin
        pvBuilder.Append('"');
        if ADoEscape then
        begin       
          JSONEscape(pvBuilder, v.AsString, ADoEscape);
        end else
        begin
          JSONEscapeWithoutDoEscape(pvBuilder, v.AsString);
        end;        
        pvBuilder.Append('"');
      end else if v.Value.DataType in [vdtNull, vdtUnset] then
      begin
        pvBuilder.Append('null');   // json标准表示
      end else if v.Value.DataType = vdtBoolean then
      begin
        if v.AsBoolean then
        begin
          pvBuilder.Append('true');   // json标准表示
        end else
        begin
          pvBuilder.Append('false');  // json标准表示
        end;
      end else
      begin
        JSONEscape(pvBuilder, v.AsString, ADoEscape);
      end;
    end else
    begin
      Result := -1;
      Exit;
    end;
  end;
           
end;

function JSONParseName(var ptrData:PChar; pvParser:TJsonParser):Integer;
var
  lvEndChar:Char;
  lvStart:PChar;
begin
  if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
  begin
    Result := -1;
    exit;
  end;
  if CharInSet(ptrData^, ['"', '''']) then
  begin
    lvEndChar := ptrData^;
    inc(ptrData);
    lvStart := ptrData;
    while ptrData^ <> #0 do
    begin
      if ptrData^ = lvEndChar then
      begin
        pvParser.FLastStrValue := Copy(lvStart, 0, ptrData - lvStart);
        Inc(ptrData);
        if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
        begin
          Result := -1;
          exit;
        end;
        if ptrData^ <> ':' then
        begin
          Result := -1;
          Exit;
        end;
        Inc(ptrData);
        Result := 0;
        Exit;
      end else
      begin
        Inc(ptrData);
      end;
    end;
    Result := -1;
    Exit;
  end else
  begin
    lvStart := ptrData;
    while ptrData^ <> #0 do
    begin
      if CharInSet(ptrData^, [':']) then
      begin
        pvParser.FLastStrValue := Copy(lvStart, 0, ptrData - lvStart);
        Inc(ptrData);
        Result := 0;
        Exit;
      end else if CharInSet(ptrData^, [#32, #9, #13, #10]) then  // space, tab, \r, \n
      begin  
        pvParser.FLastStrValue := Copy(lvStart, 0, ptrData - lvStart);
        if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
        begin
          Result := -1;
          exit;
        end;
        if ptrData^ <> ':' then
        begin
          Result := -1;
          Exit;
        end;
        Inc(ptrData);
        Result := 0;
        Exit;
      end else
      begin
        Inc(ptrData);
      end;
    end;
    Result := -1;
    Exit;
  end;
end;

procedure UnEscape(var ptrData: PChar; pvBuilder: TDStringBuilder);
var
  s:string;
begin
  case ptrData^ of
    'b':
      begin
        pvBuilder.Append(#7);
        Inc(ptrData);
      end;
    't':
      begin
        pvBuilder.Append(#9);
        Inc(ptrData);
      end;
    'n':
      begin
        pvBuilder.Append(#10);
        Inc(ptrData);
      end;
    'f':
      begin
        pvBuilder.Append(#12);
        Inc(ptrData);
      end;
    'r':
      begin
        pvBuilder.Append(#13);
        Inc(ptrData);
      end;
    '\':
      begin
        pvBuilder.Append('\');
        Inc(ptrData);
      end;
    '''':
      begin
        pvBuilder.Append('''');
        Inc(ptrData);
      end;
    '"':
      begin
        pvBuilder.Append('"');
        Inc(ptrData);
      end;
    'u':
      begin
        // \uXXXX
        if IsHexChar(ptrData[1]) and IsHexChar(ptrData[2]) and IsHexChar(ptrData[3]) and
          IsHexChar(ptrData[4]) then
        begin
          s := WideChar((HexValue(ptrData[1]) shl 12) or (HexValue(ptrData[2]) shl 8)
            or (HexValue(ptrData[3]) shl 4) or HexValue(ptrData[4]));
          pvBuilder.Append(s);
          Inc(ptrData, 5);
        end
        else
          raise Exception.CreateFmt(SCharNeeded,
            ['0-9A-Fa-f', PickString(ptrData, 0, 4)]);
      end;
    '/':
      begin
        pvBuilder.Append('/');
        Inc(ptrData);
      end;
  else
    begin
      pvBuilder.Append(ptrData^);
      Inc(ptrData);
    end;
  end;
end;


function JSONParseValue(var ptrData: PChar; pvDValue: TDValue; pvParser:
    TJsonParser; pvBuilder:TDStringBuilder): Integer;
var
  lvEndChar:Char;
  lvStart:PChar;
  lvStr:String;
  procedure __innerSetValue(pvStr:string);
  var
    lvTempPtr:PChar;
    lvValue:Extended;
  begin
    lvTempPtr := PChar(pvStr);
    if ParseNumeric(lvTempPtr, lvValue) then
    begin
      if SameValue(lvValue, Trunc(lvValue), 5E-324) then
        pvDValue.AsInteger := Trunc(lvValue)
      else
        pvDValue.AsFloat := lvValue;
    end else if StartWith(lvTempPtr, 'false') then
    begin
      pvDValue.AsBoolean := False;
    end else if StartWith(lvTempPtr, 'true') then
    begin
      pvDValue.AsBoolean := True;
    end else if StartWith(lvTempPtr, 'null') then
    begin
      pvDValue.Clear;
    end else            
    begin
      pvDValue.AsString := pvStr;
    end;
  end;
begin
  pvParser.FLastStrValue := '';
  pvParser.FLastValue := '';
  if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
  begin
    Result := -1;
    exit;
  end;
  if CharInSet(ptrData^,['"', '''']) then
  begin
    lvEndChar := ptrData^;
    inc(ptrData);
    pvBuilder.Clear;

    while ptrData^ <> #0 do
    begin
      if ptrData^ = lvEndChar then
      begin                             
        pvDValue.Value.AsString := pvBuilder.ToString;
        Inc(ptrData);
        if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
        begin
          Result := -1;
          exit;
        end;
        if CharInSet(ptrData^ , [',',']','}']) then
        begin
          Result := 1;
          Exit;
        end;
        Result := -1;
        exit;
      end else if ptrData^ = '\' then
      begin
        Inc(ptrData);
        UnEscape(ptrData, pvBuilder);
      end else
      begin
        pvBuilder.Append(ptrData^);
        Inc(ptrData);
      end;
    end;
    Result := -1;
    Exit;
  end else if CharInSet(ptrData^ , ['{', '[']) then
  begin
    JSONParseEx(ptrData, pvDValue, pvParser, pvBuilder);
    Result := 5;    
  end else
  begin
    lvStart := ptrData;
    while ptrData^ <> #0 do
    begin
      if CharInSet(ptrData^ , [',',']','}']) then
      begin
        lvStr := Copy(lvStart, 0, ptrData - lvStart);
        __innerSetValue(lvStr);
        Result := 2;
        Exit;
      end else if CharInSet(ptrData^ , [#32, #9, #13, #10]) then      // space, tab, \r, \n
      begin
        lvStr := Copy(lvStart, 0, ptrData - lvStart);
        __innerSetValue(lvStr);
        if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
        begin
          Result := -1;
          exit;
        end;
        if CharInSet(ptrData^ , [',',']','}']) then
        begin
          Result := 2;
          Exit;
        end;
        Result := -1;
        Exit;
      end else
      begin
        Inc(ptrData);
      end;
    end;
    Result := -1;
    Exit;
  end;
end;

function JSONParseEx(var ptrData: PChar; pvDValue: TDValue; pvParser:
    TJsonParser; pvBuilder:TDStringBuilder): Integer;
var
  lvEndChar:Char;
  lvChild:TDValue;
begin
  Result := 0;
  if CharInSet(ptrData^ , ['{', '[']) then
  begin
    if ptrData^ = '{' then
    begin
      pvDValue.CheckSetNodeType(vntObject);
      lvEndChar := '}';
      Result := 1;
    end else if CharInSet(ptrData^ , ['[']) then
    begin
      pvDValue.CheckSetNodeType(vntArray);
      lvEndChar := ']';
      Result := 2;
    end else
    begin
      lvEndChar := #0;
    end;
    Inc(ptrData);
    if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
    begin
      Result := -1;
      exit;
    end;
    while (ptrData^ <> #0) and (ptrData^ <> lvEndChar) do
    begin
      if (ptrData^ <> lvEndChar) then
      begin
        if pvDValue.ObjectType = vntArray then
        begin
          lvChild := pvDValue.AddArrayChild;
        end else
        begin
          lvChild := pvDValue.Add;
        end;
        if JSONParseEx(ptrData, lvChild, pvParser, pvBuilder) = -1 then
        begin
          Result := -1;
          exit;
        end;
        if ptrData^ = ',' then
        begin
          Inc(ptrData);
          if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
          begin
            Result := -1;
            exit;
          end;
        end;
      end else  // 解析完成
      begin
        Exit;
      end;
    end;  
    if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
    begin
      Result := -1;
      exit;
    end;
    if ptrData^ <> lvEndChar then
    begin
      Result := -1;
      Exit;
    end;
    Inc(ptrData);
    JSONSkipSpaceAndComment(ptrData, pvParser);
  end else if (pvDValue.Parent <> nil) then
  begin
    if (pvDValue.Parent.ObjectType = vntObject) and (pvDValue.Name.DataType in [vdtNull, vdtUnset]) then
    begin
      if JSONParseName(ptrData, pvParser) = -1 then
      begin
        Result := -1;
        Exit;
      end else
      begin
        pvDValue.Name.AsString := pvParser.FLastStrValue;
        Result := JSONParseValue(ptrData, pvDValue, pvParser, pvBuilder);
        Exit;
      end;
    end else if pvDValue.Parent.ObjectType = vntArray then
    begin
      Result := JSONParseValue(ptrData, pvDValue, pvParser, pvBuilder);
      Exit;
    end else
    begin  // must be vntArray, vntObject(vdtNull, vdtUnset can convert to object)
      Result := -1;
      Exit;
    end;
  end else
  begin
    pvDValue.CheckSetNodeType(vntNull);
    Result := -1;
  end;

end;

function JSONSkipSpaceAndComment(var ptrData: PChar; pvParser: TJsonParser):   Integer;
begin
  Result := 0;
  SkipChars(ptrData, [#10, #13, #9, #32]);
  while ptrData^ = '/' do
  begin
    if ptrData[1] = '/' then
    begin
      SkipUntil(ptrData, [#10]);
      SkipChars(ptrData, [#10, #13, #9, #32]);
    end else if ptrData[1] = '*' then
    begin
      Inc(ptrData, 2);
      while ptrData^ <> #0 do
      begin
        if (ptrData[0] = '*') and (ptrData[1] = '/') then
        begin
          Inc(ptrData, 2);
          SkipChars(ptrData, [#10, #13, #9, #32]);
          Break;
        end
        else
          Inc(ptrData);
      end;
    end else
    begin
      Result := -1;
      Exit;
    end;
  end;
end;

function JSONParser(const s: string; pvDValue: TDValue): Integer;
var
  ptrData:PChar;
  j:Integer;
  lvParser:TJsonParser;
  lvStringBuilder:TDStringBuilder;
begin
  Result := -1;
  ptrData := PChar(s);
  lvParser := TJsonParser.Create;
  lvStringBuilder := TDStringBuilder.Create;
  try
    j := JSONSkipSpaceAndComment(ptrData, lvParser);
    if j = -1 then
    begin
      Exit;
    end;

    if CharInSet(ptrData ^ , ['{', '[']) then
    begin 
      JSONParseEx(ptrData, pvDValue, lvParser, lvStringBuilder);
      Result := 0;
    end;
  finally
    lvParser.Free;
    lvStringBuilder.Free;
  end;
end;

function JSONEncode(v: TDValue; ADoEscape: Boolean = true; ADoFormat: Boolean =
    true; pvExceptValueTypes: TDValueDataTypes = [vdtInterface, vdtObject,
    vdtPtr]): String;
var
  lvSB:TDStringBuilder;
begin
  Result := '';
  if v = nil then Exit;
  
  lvSB := TDStringBuilder.Create;
  try
    JSONEncodeEx(v, lvSB, 0, ADoEscape, ADoFormat, pvExceptValueTypes);
    Result := lvSB.ToString();
  finally
    lvSB.Free;
  end;
end;


function JSONParseFromUtf8NoBOMFile(pvFile:string; pvDValue:TDValue): Integer;
var
  s:String;
begin
  if FileExists(pvFile) then
  begin
    s := LoadTextFromFile(pvFile);
    Result := JSONParser(s, pvDValue);
  end else
  begin
    Result := 0;
  end;
end;

procedure JSONWriteToUtf8NoBOMFile(const pvFile: string; pvDValue: TDValue);
var
  s:String;
begin
  s := JSONEncode(pvDValue, False, True);
  WriteStringToUtf8NoBOMFile(pvFile, s);
end;

function InputJsonBuffer(const jsonBuffer:PJsonBuffer; pvByte:Integer): Integer;
begin
  Result := 0;
  if jsonBuffer.FState = 0 then
  begin
    if pvByte = Ord('{') then
    begin
      jsonBuffer.FBeginChr[jsonBuffer.FLevel] := pvByte;
      jsonBuffer.FEndChr[jsonBuffer.FLevel] := Ord('}');
      Inc(jsonBuffer.FLevel);
      jsonBuffer.FState := 1;
      jsonBuffer.FBuffer.Write(pvByte, 1);
      Exit;
    end;
    if pvByte = Ord('[') then
    begin
      jsonBuffer.FBeginChr[jsonBuffer.FLevel] := pvByte;
      jsonBuffer.FEndChr[jsonBuffer.FLevel] := Ord(']');
      Inc(jsonBuffer.FLevel);
      jsonBuffer.FState := 1;
      jsonBuffer.FBuffer.Write(pvByte, 1);
      Exit;
    end;
  end else if jsonBuffer.FState = 1 then
  begin       //
    if pvByte = Ord('"') then
    begin
      jsonBuffer.FState := 2;   // string
      jsonBuffer.FBuffer.Write(pvByte, 1);
      jsonBuffer.FStringEndChr := Ord('"');
      Exit;
    end;

    if pvByte = Ord('''') then
    begin
      jsonBuffer.FState := 2;   // string
      jsonBuffer.FBuffer.Write(pvByte, 1);
      jsonBuffer.FStringEndChr := Ord('''');
      Exit;
    end;

    //
    if pvByte = Ord('{') then
    begin
      jsonBuffer.FBeginChr[jsonBuffer.FLevel] := pvByte;
      jsonBuffer.FEndChr[jsonBuffer.FLevel] := Ord('}');
      Inc(jsonBuffer.FLevel);
      jsonBuffer.FState := 1;
      jsonBuffer.FBuffer.Write(pvByte, 1);
      Exit;
    end;
    if pvByte = Ord('[') then
    begin
      jsonBuffer.FBeginChr[jsonBuffer.FLevel] := pvByte;
      jsonBuffer.FEndChr[jsonBuffer.FLevel] := Ord(']');
      Inc(jsonBuffer.FLevel);
      jsonBuffer.FState := 1;
      jsonBuffer.FBuffer.Write(pvByte, 1);
      Exit;
    end;

    if pvByte = jsonBuffer.FEndChr[jsonBuffer.FLevel - 1] then
    begin
      Dec(jsonBuffer.FLevel);
      jsonBuffer.FBuffer.Write(pvByte, 1);
      if jsonBuffer.FLevel = 0 then
      begin    // 成功
        Result := 1;
      end;
      Exit;
    end;
    

    jsonBuffer.FBuffer.Write(pvByte, 1);
  end else if jsonBuffer.FState = 2 then
  begin
    if pvByte = jsonBuffer.FStringEndChr then
    begin
      jsonBuffer.FState := 1;   // string
      jsonBuffer.FBuffer.Write(pvByte, 1);
      Exit;
    end;

    if pvByte = Ord('\') then
    begin       // 转义符
      jsonBuffer.FState := 3;   // escape
      jsonBuffer.FBuffer.Write(pvByte, 1);
      Exit;
    end;
    jsonBuffer.FBuffer.Write(pvByte, 1);
  end else if jsonBuffer.FState = 3 then
  begin
    jsonBuffer.FState := 2;   // escape
    jsonBuffer.FBuffer.Write(pvByte, 1);
  end else
  begin
    Assert(False, 'impossiable');
  end;
end;

procedure ResetJsonBuffer(const jsonBuffer:PJsonBuffer);
begin
  jsonBuffer.FLevel := 0;
  jsonBuffer.FState := 0;
end;

function JSONParseFromFile(pvFile:string; pvDValue:TDValue): Integer;
var
  s:String;
begin
  if FileExists(pvFile) then
  begin
    s := LoadTextFromFile(pvFile);
    if JSONParser(s, pvDValue) = 0 then
    begin
      Result := 1;
    end else
    begin
      Result := 0;
    end;
  end else
  begin
    Result := 0;
  end; 
end;

function StringJSONEscape(const s: String; ADoEscape: Boolean = false): String;
var
  lvSB:TDStringBuilder;
begin
  lvSB := TDStringBuilder.Create;
  try
    JSONEscape(lvSB, s, ADoEscape);
    Result := lvSB.ToString;
  finally
    lvSB.Free;
  end;
end;

end.
