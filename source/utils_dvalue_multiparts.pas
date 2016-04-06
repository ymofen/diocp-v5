unit utils_dvalue_multiparts;

interface

uses
  utils_dvalue, utils.strings, SysUtils, Classes, Dialogs;

type
  TMultiPartsParser = class(TObject)
  private
    FBoundary: RAWString;
    FDecodeState:Integer;
    FData:TDValue;
    FBuffer: TDBufferBuilder;
    FCurrentHeader: RAWString;

    function DecodeHeader: Integer;

  public
    constructor Create;
    destructor Destroy; override;
    procedure SetDValue(pvData:TDValue);

    function InputBuffer(pvByte:Byte): Integer;

    property Boundary: RAWString read FBoundary;

    property CurrentHeader: RAWString read FCurrentHeader;


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
  lvParser := TMultiPartsParser.Create;
  try
    lvParser.SetDValue(v);
    while (pvStream.Read(lvByte, 1) = 1) do
    begin
      r :=lvParser.InputBuffer(lvByte);
      if r = 1 then
      begin
        ShowMessage(lvParser.CurrentHeader);
      end else if r = 2 then
      begin
        Result := r;
        exit;
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

function TMultiPartsParser.DecodeHeader: Integer;
begin
  FCurrentHeader := FBuffer.ToRAWString;
  Result := 1;
end;

function TMultiPartsParser.InputBuffer(pvByte:Byte): Integer;
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
    4:    // 开始解码头部
      begin
        if pvByte = 13 then Inc(FDecodeState);
      end;
    5:
      begin
        if pvByte = 10 then Inc(FDecodeState) else
        begin
          Result := -1;
          FBuffer.Clear;
          FDecodeState := 0;
          Exit;
        end;
      end;
    6:
      begin
        if pvByte = 13 then Inc(FDecodeState) else
        begin
          Result := -1;
          FBuffer.Clear;
          FDecodeState := 0;
          Exit;
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
  end;
end;

procedure TMultiPartsParser.SetDValue(pvData:TDValue);
begin
  FData := pvData;
  FDecodeState := 0;
  FBuffer.Clear;
end;

end.
