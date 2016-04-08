unit utils_dvalue_multiparts;

interface

uses
  utils_dvalue, utils.strings, SysUtils, Classes, Dialogs;

type
  TMultiPartsParser = class(TObject)
  private
    FBoundaryMatchIndex:Integer;
    FBoundaryBytes:TBytes;
    FBoundary: RAWString;
    FDecodeState:Integer;
    FData:TDValue;
    FCurrentHeaders:TDValue;
    FBuffer: TDBufferBuilder;
    FCurrentFieldName: String;
    FCurrentRAWHeader: RAWString;

    function ParseContentDisposition: Integer;
    function DecodeHeader: Integer;
    function DecodeContent: Integer;
    procedure DecodeHeaderLine(pvLine:string);

  public
    constructor Create;
    destructor Destroy; override;
    procedure SetDValue(pvData:TDValue);

    /// <summary>
    ///   0: 需要更多数据
    ///   1: 解码到头
    ///   2：解码到Content
    ///   9: 解码完整
    /// </summary>
    function InputBuffer(pvByte:Byte): Integer;

    property Boundary: RAWString read FBoundary;
    property CurrentFieldName: String read FCurrentFieldName write
        FCurrentFieldName;

    property CurrentRAWHeader: RAWString read FCurrentRAWHeader;
    


  end;


procedure MultiPartsEncode(v: TDValue; pvBuilder: TDBufferBuilder; pvBoundary:
    string; pvIgnoreValueTypes: TDValueDataTypes = [vdtInterface, vdtObject,
    vdtPtr]);

/// <summary>procedure MultiPartsParseFromStream
/// </summary>
/// <returns> Integer
/// </returns>
/// <param name="v"> (TDValue) </param>
/// <param name="pvStream"> (TStream) </param>
function MultiPartsParseFromStream(v: TDValue; pvStream: TStream): Integer;

function MultiPartsParseFromFile(v:TDValue; pvFile:String): Integer;

procedure AddFilePart(v: TDValue; pvFieldID:string; pvFileName:String;
    pvContentType:string = 'application/x-msdownload');




implementation

function GenerateUniqueBoundary: RAWString;
begin
  Result := 'dvalue' + FormatDateTime('mmddyyhhnnsszzz', Now);
end;

procedure MultiPartsEncode(v: TDValue; pvBuilder: TDBufferBuilder; pvBoundary:
    string; pvIgnoreValueTypes: TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);
var
  i, j:Integer;
  lvItem, lvName, lvValue, lvChildItem:TDValue;
  lvBoundary, lvTempStr, lvNameStr:RAWString;
  lvFmt:RAWString;
begin
  lvBoundary := pvBoundary;
  if lvBoundary = '' then
  begin
     lvBoundary := GenerateUniqueBoundary;
  end;

  for i := 0 to v.Count - 1 do
  begin
    lvItem := v.Items[i];
    if lvItem.Count = 0 then
    begin
      pvBuilder.AppendRawStr('--').AppendRawStr(lvBoundary).AppendBreakLineBytes;
      pvBuilder.AppendRawStr('Content-Disposition: form-data; name="').AppendRawStr(lvItem.Name.AsString).AppendRawStr('"').AppendBreakLineBytes;
      pvBuilder.AppendBreakLineBytes;
      pvBuilder.AppendRawStr(lvItem.AsString).AppendBreakLineBytes;
    end else
    begin
      lvName := lvItem.FindByName('name');
      lvValue := lvItem.FindByName('value');
      if (lvName <> nil) and (lvValue <> nil) then
      begin
        pvBuilder.AppendRawStr('--').AppendRawStr(lvBoundary).AppendBreakLineBytes;
        pvBuilder.AppendRawStr('Content-Disposition: form-data; name="').AppendRawStr(lvName.AsString).AppendRawStr('"');
        pvBuilder.AppendRawStr('; filename="').AppendRawStr(lvItem.GetValueByName('fileName', '')).Append(Byte(Ord('"')));
        pvBuilder.AppendBreakLineBytes;
        for j := 0 to lvItem.Count - 1 do
        begin
          lvChildItem := lvItem.Items[j];
          lvNameStr := LowerCase(Trim(lvChildItem.Name.AsString));
          if (lvNameStr <> 'name') and (lvNameStr <> 'value') and (lvNameStr <> 'filename') then
          begin
            pvBuilder.AppendRawStr(lvNameStr).Append(Byte(Ord(':'))).AppendRawStr(lvChildItem.AsString).AppendBreakLineBytes;          
          end;
        end;
        pvBuilder.AppendBreakLineBytes;
        if lvValue.Value.DataType in [vdtObject] then
        begin
          if (lvValue.AsObject is TStream) then
          begin
            TStream(lvValue.AsObject).Position := 0;
            pvBuilder.LoadFromStream(TStream(lvValue.AsObject), TStream(lvValue.AsObject).Size);
          end;
        end else if lvValue.Value.DataType = vdtStream then
        begin
          lvValue.AsStream.Position := 0;
          pvBuilder.LoadFromStream(lvValue.AsStream, lvValue.AsStream.Size);
        end;
        pvBuilder.AppendBreakLineBytes;
      end;
    end;
  end;
  pvBuilder.AppendRawStr('--').AppendRawStr(lvBoundary).AppendRawStr('--').AppendBreakLineBytes;
end;


procedure AddFilePart(v: TDValue; pvFieldID:string; pvFileName:String;
    pvContentType:string = 'application/x-msdownload');
var
  lvItem:TDValue;
begin
  if not FileExists(pvFileName) then
  begin
    raise Exception.CreateFmt('文件[%s]不存在', [pvFileName]);
  end;
  lvItem := v.ForceByName(pvFieldID);
  lvItem.ForceByName('name').AsString := pvFieldID;
  lvItem.ForceByName('fileName').AsString := ExtractFileName(pvFileName);
  lvItem.ForceByName('Content-Type').AsString := pvContentType;
  lvItem.ForceByName('value').AsStream.LoadFromFile(pvFileName);
end;

function MultiPartsParseFromStream(v: TDValue; pvStream: TStream): Integer;
var
  lvParser:TMultiPartsParser;
  lvByte:Byte;
  r: Integer;
begin
  Result := 0;
  lvParser := TMultiPartsParser.Create;
  try
    lvParser.SetDValue(v);
    while (pvStream.Read(lvByte, 1) = 1) do
    begin
      r :=lvParser.InputBuffer(lvByte);
      if r = 1 then
      begin
        ;
      end else if r = 2 then
      begin
        Inc(Result);
      end;
    end;
  finally
    lvParser.Free;
  end;            
end;

function MultiPartsParseFromFile(v:TDValue; pvFile:String): Integer;
var
  lvFileStream:TFileStream;
begin
  lvFileStream := TFileStream.Create(pvFile, fmOpenRead);
  try
    Result := MultiPartsParseFromStream(v, lvFileStream);
  finally
    lvFileStream.Free;
  end;
end;

constructor TMultiPartsParser.Create;
begin
  inherited Create;
  FBuffer := TDBufferBuilder.Create();
end;

destructor TMultiPartsParser.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

function TMultiPartsParser.DecodeContent: Integer;
var
  lvRAW:TMemoryStream;
  l:Integer;
begin
  lvRAW := FCurrentHeaders.ForceByName('__raw').AsStream;
  lvRAW.Clear;
  l := FBuffer.Length - Length(FBoundaryBytes) - 2;
  FCurrentHeaders.ForceByName('__raw-size').AsInteger := l;
  lvRAW.SetSize(l);
  Move(FBuffer.Memory^, lvRAW.Memory^, l);
  Result := 1;
end;

function TMultiPartsParser.DecodeHeader: Integer;
var
  lvPtr:PChar;
  lvLine:String;
begin
  FCurrentRAWHeader := FBuffer.ToRAWString;
  lvPtr := PChar(FCurrentRAWHeader);

  FCurrentHeaders := TDValue.Create;
  try
    Result := -1;
    FCurrentFieldName := '';
    while True do
    begin
      SkipChars(lvPtr,  [#13, #10, ' ', #9]);
      if LeftUntil(lvPtr, [#13, #10], lvLine) = 0 then
      begin
        DecodeHeaderLine(lvLine);
      end else
      begin
        break;
      end;
    end;

    if ParseContentDisposition = 1 then
    begin
      Result := 1;
      FData.AttachDValue(FCurrentFieldName, FCurrentHeaders);
    end;
  finally
    if Result = -1 then
    begin
      FCurrentHeaders.Free;
      FCurrentHeaders := nil;
    end;
  end;
end;

procedure TMultiPartsParser.DecodeHeaderLine(pvLine:string);
var
  lvPtr:PChar;
  lvKey:string;
  r:Integer;
begin
  lvPtr := PChar(pvLine);

  r := LeftUntil(lvPtr, [':'], lvKey);
  if r = -1 then Exit;

  lvKey := LowerCase(Trim(lvKey));

  SkipChars(lvPtr, [':', ' ', #9]);

  FCurrentHeaders.ForceByName(lvKey).AsString := lvPtr;
end;

function TMultiPartsParser.ParseContentDisposition: Integer;
var
  lvPtr:PChar;
  lvValue, lvTempID, lvTempValue, lvFieldName:String;
begin
  Result := -1;
  lvFieldName := '';
  // Content-Disposition: form-data; name="fileID"
  // Content-Disposition: form-data; name="data"; filename="DValueTester.exe"
  lvValue := FCurrentHeaders.GetValueByName('Content-Disposition', '');
  if lvValue = '' then Exit;
  lvPtr := PChar(lvValue);
  SkipUntil(lvPtr, [';']);
  SkipChars(lvPtr, [' ', #9, ';']);  // skip form-data

  while true do
  begin
    lvTempID := LeftUntil(lvPtr, ['=']);
    if lvTempID = '' then Break;
    SkipChars(lvPtr, ['=', ' ', '"']);


    if (LeftUntil(lvPtr, ['"', ';'], lvTempValue) = 0) then
    begin
      if LowerCase(lvTempID) = 'name' then
        lvFieldName := lvTempValue;

      FCurrentHeaders.ForceByName(lvTempID).AsString := lvTempValue;
    end else
    begin
      if LowerCase(lvTempID) = 'name' then
        lvFieldName := lvPtr;
      FCurrentHeaders.ForceByName(lvTempID).AsString := lvPtr;
      Break;
    end;
    SkipChars(lvPtr, ['"', ';', ' ', #9]);
  end;
  if lvFieldName <> '' then
  begin
    FCurrentFieldName := lvFieldName;
    Result := 1;
  end;

end;

function TMultiPartsParser.InputBuffer(pvByte:Byte): Integer;
function CheckHead():Integer;
begin
  if FBuffer.Length > 4096 then
  begin
    Result := -1;
  end else
  begin
    Result := 0;
  end;
end;
begin
  Result := 0;
  FBuffer.Append(pvByte);

  case FDecodeState of
    0, 1:  // 解码开头部分'--'
      begin
        if pvByte = Ord('-') then Inc(FDecodeState)
        else
        begin
          Result := -1;
          FBuffer.Clear;
          FDecodeState := 0;
          Exit;
        end;
      end;
    2:
      begin
        if pvByte = 13 then Inc(FDecodeState);
      end;
    3:      // 解码Boundary
      begin
        if pvByte = 10 then
        begin
          SetLength(FBoundaryBytes, FBuffer.Length - 2);
          Move(FBuffer.Memory^, FBoundaryBytes[0],FBuffer.Length - 2);
          FBoundary := FBuffer.ToRAWString;
          FBuffer.Clear;
          Inc(FDecodeState);
        end else
        begin
          Result := -1;
          FBuffer.Clear;
          FDecodeState := 0;
          Exit;
        end;
      end;
    4:    // 开始解码单个data头部(双#13#10
      begin
        if pvByte = 13 then Inc(FDecodeState);
      end;
    5:
      begin
        if pvByte = 10 then Inc(FDecodeState) else
        begin
          if CheckHead = -1 then
          begin
            Result := -1;
            FBuffer.Clear;
            FDecodeState := 0;
            FBoundaryMatchIndex := 0;
            Exit;
          end;
          FDecodeState := 4;
        end;
      end;
    6:
      begin
        if pvByte = 13 then Inc(FDecodeState) else
        begin
          if CheckHead = -1 then
          begin
            Result := -1;
            FBuffer.Clear;
            FDecodeState := 0;
            FBoundaryMatchIndex := 0;
            Exit;
          end;
          FDecodeState := 4;
        end;
      end;
    7:
      begin
        if pvByte = 10 then
        begin  // 解码到头部
          if DecodeHeader = 1 then
          begin
            FBuffer.Clear;
            Result := 1;
            FBoundaryMatchIndex := 0;
            // 下一步解码数据
            Inc(FDecodeState);
            Exit;
          end else
          begin
            Result := -1;
            FBuffer.Clear;
            FDecodeState := 0;
            Exit;
          end;
        end else
        begin
          if CheckHead = -1 then
          begin
            Result := -1;
            FBuffer.Clear;
            FDecodeState := 0;
            FBoundaryMatchIndex := 0;
            Exit;
          end;
          FDecodeState := 4;
        end;
      end;
    8:       //开始解码数据
      begin
        if pvByte = FBoundaryBytes[FBoundaryMatchIndex] then
        begin
          Inc(FBoundaryMatchIndex);
          if FBoundaryMatchIndex = Length(FBoundaryBytes) then
          begin
            Inc(FDecodeState);
          end;
        end else
        begin
          FBoundaryMatchIndex := 0;
        end;
      end;
    9:
      begin
        if pvByte = 13 then Inc(FDecodeState)
        else if pvByte = Ord('-') then FDecodeState := 12  //解码完成
        else
        begin
          Result := -1;
          FBuffer.Clear;
          FDecodeState := 0;
          FBoundaryMatchIndex := 0;
          Exit;
        end;
      end;
    10:
      begin
        if pvByte = 10 then
        begin  // 解码到一个数据
          if DecodeContent = 1 then
          begin
            Result := 2;
            FDecodeState := 4;
            FBoundaryMatchIndex := 0;
            FBuffer.Clear;
            Exit;
          end else
          begin
            Result := -1;
            FBuffer.Clear;
            FDecodeState := 0;
            Exit;
          end;
        end else
        begin
          Result := -1;
          FBuffer.Clear;
          FDecodeState := 0;
          Exit;
        end;
      end;
    12:
      begin
        if pvByte = Ord('-') then
        begin           // 完整解码
          if DecodeContent = 1 then
          begin
            Result := 9;
            FBuffer.Clear;
            FDecodeState := 0;
            exit;
          end else
          begin
            Result := -1;
            FBuffer.Clear;
            FDecodeState := 0;
            Exit;
          end;
        end else
        begin
          Result := 0;
          FDecodeState := 8;  // 继续解码数据
          FBoundaryMatchIndex := 0;
          Exit;
        end;        
      end;
  end;
end;

procedure TMultiPartsParser.SetDValue(pvData:TDValue);
begin
  FData := pvData;
  FDecodeState := 0;
  FBuffer.Clear;
end;

end.
