unit utils_DValue_JSON;

// #34  "
// #39  '
// #32  空格
// #58  :
// #9   TAB

interface

uses
  utils_DValue, utils.strings;

type
  TJsonParser = class(TObject)
  private
    FLastStrValue: String;
    FLastValue: String;
  end;

function JSONParser(s: string; pvDValue: TDValue): Integer;

function JSONEncode(v:TDValue): String;


    
implementation

function JSONParseEx(var ptrData: PChar; pvDValue: TDValue; pvParser:
    TJsonParser): Integer; forward;
    
function JSONSkipSpaceAndComment(var ptrData: PChar; pvParser: TJsonParser):
    Integer;forward;

function CreateIndentBlock(pvLevel: Integer; pvBlockSize: Integer = 4): String;
begin
  SetLength(Result, pvLevel * pvBlockSize);
  FillChar(PChar(Result)^, pvLevel * pvBlockSize, ' ');
end;

procedure JSONEncodeEx(v: TDValue; pvStringBuilder: TDStringBuilder; pvLevel:
    Integer);
var
  i:Integer;
  lvIndentStr, lvChildIndentStr, lvName:String;
begin
  lvIndentStr := CreateIndentBlock(pvLevel);
  lvChildIndentStr := CreateIndentBlock(pvLevel + 1);
  if v.ObjectType = vntObject then
  begin
    lvName := v.Name.AsString;
    if Length(lvName) <> 0 then
    begin
      pvStringBuilder.AppendQuoteStr(v.Name.AsString);
      pvStringBuilder.Append(':');
    end;
    pvStringBuilder.AppendLine('{');
    for i := 0 to v.Count - 1 do
    begin
      pvStringBuilder.Append(lvChildIndentStr);
      JSONEncodeEx(v.Items[i], pvStringBuilder, pvLevel + 1);
      if i < v.Count -1 then
      begin
        pvStringBuilder.Append(',');
      end;
      pvStringBuilder.Append(sLineBreak);
    end;
    pvStringBuilder.Append(lvIndentStr);
    pvStringBuilder.Append('}');
  end else if v.ObjectType = vntArray then
  begin
    lvName := v.Name.AsString;
    if Length(lvName) <> 0 then
    begin
      pvStringBuilder.AppendQuoteStr(v.Name.AsString);
      pvStringBuilder.Append(':');
    end;
    pvStringBuilder.AppendLine('[');
    for i := 0 to v.Count - 1 do
    begin
      pvStringBuilder.Append(lvChildIndentStr);
      JSONEncodeEx(v.Items[i], pvStringBuilder, pvLevel + 1);
      if i < v.Count -1 then
      begin
        pvStringBuilder.Append(',');
      end;
      pvStringBuilder.Append(sLineBreak);
    end;
    pvStringBuilder.Append(lvIndentStr);
    pvStringBuilder.Append(']');
  end else if v.ObjectType = vntValue then
  begin
    if v.Name.AsString <> '' then
    begin
      pvStringBuilder.AppendQuoteStr(v.Name.AsString);
      pvStringBuilder.Append(':');
    end;
    if v.Value.DataType in [vdtString, vdtStringW] then
    begin
      pvStringBuilder.AppendQuoteStr(v.AsString);
    end else
    begin
      pvStringBuilder.Append(v.AsString);
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
  if ptrData^ in ['"', ''''] then
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
      if ptrData^ in [':'] then
      begin
        pvParser.FLastStrValue := Copy(lvStart, 0, ptrData - lvStart);
        Inc(ptrData);
        Result := 0;
        Exit;
      end else if ptrData^ in [#32, #9, #13, #10] then  // space, tab, \r, \n
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


function JSONParseValue(var ptrData: PChar; pvDValue: TDValue; pvParser:
    TJsonParser): Integer;
var
  lvEndChar:Char;
  lvStart:PChar;
begin
  pvParser.FLastStrValue := '';
  pvParser.FLastValue := '';
  if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
  begin
    Result := -1;
    exit;
  end;
  if ptrData^ in ['"', ''''] then
  begin
    lvEndChar := ptrData^;
    inc(ptrData);
    lvStart := ptrData;
    while ptrData^ <> #0 do
    begin
      if ptrData^ = lvEndChar then
      begin
        pvDValue.Value.AsString := Copy(lvStart, 0, ptrData - lvStart);
        Inc(ptrData);
        if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
        begin
          Result := -1;
          exit;
        end;
        if ptrData^ in [',',']','}'] then
        begin
          Result := 1;
          Exit;
        end;
        Result := -1;
        exit;
      end else
      begin
        Inc(ptrData);
      end;
    end;
    Result := -1;
    Exit;
  end else if ptrData^ in ['{', '['] then
  begin
    JSONParseEx(ptrData, pvDValue, pvParser);
    Result := 5;    
  end else
  begin
    lvStart := ptrData;
    while ptrData^ <> #0 do
    begin
      if ptrData^ in [',',']','}'] then
      begin
        pvDValue.Value.AsString := Copy(lvStart, 0, ptrData - lvStart);
        Result := 2;
        Exit;
      end else if ptrData^ in [#32, #9, #13, #10] then      // space, tab, \r, \n
      begin
        pvDValue.Value.AsString := Copy(lvStart, 0, ptrData - lvStart);
        if JSONSkipSpaceAndComment(ptrData, pvParser) = -1 then
        begin
          Result := -1;
          exit;
        end;
        if ptrData^ in [',',']','}'] then
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
    TJsonParser): Integer;
var
  lvEndChar:Char;
  lvChild:TDValue;
  r:Integer;
begin  
  if ptrData^ in ['{', '['] then
  begin
    if ptrData^ = '{' then
    begin
      pvDValue.CheckSetNodeType(vntObject);
      lvEndChar := '}';
      Result := 1;
    end else if ptrData^ = '[' then
    begin
      pvDValue.CheckSetNodeType(vntArray);
      lvEndChar := ']';
      Result := 2;
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
        if JSONParseEx(ptrData, lvChild, pvParser) = -1 then
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
        Exit;
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
        Result := JSONParseValue(ptrData, pvDValue, pvParser);
        Exit;
      end;
    end else if pvDValue.Parent.ObjectType = vntArray then
    begin
      Result := JSONParseValue(ptrData, pvDValue, pvParser);
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

function JSONParser(s: string; pvDValue: TDValue): Integer;
var
  ptrData:PChar;
  j:Integer;
  lvParser:TJsonParser;
begin
  Result := -1;
  ptrData := PChar(s);
  lvParser := TJsonParser.Create;
  try
    j := JSONSkipSpaceAndComment(ptrData, lvParser);
    if j = -1 then
    begin
      Exit;
    end;

    if (ptrData ^ in ['{', '[']) then
    begin 
      JSONParseEx(ptrData, pvDValue, lvParser);
      Result := 0;
    end;
  finally
    lvParser.Free;
  end;
end;

function JSONEncode(v:TDValue): String;
var
  lvSB:TDStringBuilder;
begin
  lvSB := TDStringBuilder.Create;
  try
    JSONEncodeEx(v, lvSB, 0);
    Result := lvSB.ToString();
  finally
    lvSB.Free;
  end;
end;

end.
