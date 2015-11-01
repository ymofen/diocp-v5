unit utilsDValue;

interface

uses classes, sysutils, variants, varutils, math;


type
{$IFDEF UNICODE}
  DStringW = UnicodeString;
{$ELSE}
  DStringW = WideString;
{$ENDIF UNICODE}
  PDStringW = ^DStringW;

  {$IF RTLVersion<25}
  IntPtr=Integer;
  {$IFEND IntPtr}

  {$if CompilerVersion < 18} //before delphi 2007
  TBytes = array of Byte;
  {$ifend}

  TDValueDataType = (vdtUnset, vdtNull, vdtBoolean, vdtSingle, vdtFloat,
    vdtInteger, vdtInt64, vdtCurrency, vdtGuid, vdtDateTime,
    vdtString, vdtStream, vdtArray);


  PDValueRecord = ^TDValueRecord;

  /// 一个值对象
  TDValueData = record
    case Integer of
      0:
        (AsBoolean: Boolean);
      1:
        (AsFloat: Double);
      2:
        (AsInteger: Integer);
      3:
        (AsInt64: Int64);
      5:
        (AsGuid: PGuid);
      6:
        (AsDateTime: TDateTime);
      7:
        (AsString: PDStringW);
      8:
        (AsStream: Pointer);
      9:    // Array
        (
          ArrayLength: Cardinal;
          ArrayItemsEntry: PDValueRecord;
        );
      10:
        (AsCurrency: Currency);
      11:
        (AsSingle: Single);
      13:
        (AsShort: Shortint);
      14:
        (AsByte: Byte);
      15:
        (AsSmallint: Smallint);
      16:
        (AsWord: Word);
      17:
        (AsExtend: Extended);
      18:
        (AsPointer: Pointer);
      19:
        (
          ValueType: TDValueDataType;
          Value: PDValueRecord;
        );
  end;

  TDValueRecord = record
    Value: TDValueData;
    ValueType: TDValueDataType;
  end;

  TDValues = array of TDValueRecord;





function GetDValueSize(ADValue: PDValueRecord): Integer;
function GetDValueItem(ADValue: PDValueRecord; pvIndex: Integer): PDValueRecord;

/// <summary>清理DValue内部占用的内存</summary>
/// <param name="ADValue"> (PDValueRecord) </param>
procedure ClearDValue(ADValue:PDValueRecord);
procedure CheckDValueSetType(ADValue:PDValueRecord; AType: TDValueDataType);
procedure CheckDValueSetArrayLength(ADValue: PDValueRecord; ALen: Integer);


procedure DValueSetAsString(ADValue:PDValueRecord; pvString:DStringW);
function DValueGetAsString(ADValue:PDValueRecord): DStringW;

procedure DValueSetAsInt64(ADValue:PDValueRecord; pvValue:Int64);
function DValueGetAsInt64(ADValue: PDValueRecord): Int64;

procedure DValueSetAsInteger(ADValue:PDValueRecord; pvValue:Integer);
function DValueGetAsInteger(ADValue: PDValueRecord): Integer;


procedure DValueSetAsFloat(ADValue:PDValueRecord; pvValue:Double);
function DValueGetAsFloat(ADValue: PDValueRecord): Double;

implementation

resourcestring
  SValueNotArray = '当前值不是数组类型，无法按数组方式访问。';
  SConvertError = '无法将 %s 转换为 %s 类型的值。';
  SUnsupportStreamSource = '无法将 Variant 类型转换为流。';

const
  QValueTypeName: array [TDValueDataType] of String = ('Unassigned', 'NULL',
    'Boolean', 'Single', 'Float', 'Integer', 'Int64', 'Currency', 'Guid',
    'DateTime', 'String', 'Stream', 'Array');

function GetDValueSize(ADValue: PDValueRecord): Integer;
var
  I: Integer;
begin
  Result := 0;
  case ADValue.ValueType of
    vdtBoolean:
      Result := 1;
    vdtSingle:
      Result := SizeOf(Single);
    vdtFloat:
      Result := SizeOf(Double);
    vdtInteger:
      Result := SizeOf(Integer);
    vdtInt64:
      Result := SizeOf(Int64);
    vdtCurrency:
      Result := SizeOf(Currency);
    vdtGuid:
      Result := SizeOf(TGuid);
    vdtDateTime:
      Result := SizeOf(TDateTime);
    vdtString:
      Result := Length(ADValue.Value.AsString^) shl 1;
    vdtStream:
      Result := TMemoryStream(ADValue.Value.AsStream).Size;
    vdtArray:
      begin
        Result := 0;
        for I := 0 to ADValue.Value.ArrayLength - 1 do
          Inc(Result, GetDValueSize(GetDVAlueItem(@ADValue, I)));
      end;
  end;
end;

function GetDValueItem(ADValue: PDValueRecord; pvIndex: Integer): PDValueRecord;
begin
  if ADValue.ValueType = vdtArray then
    Result := PDValueRecord(IntPtr(ADValue.Value.ArrayItemsEntry) + (SizeOf(TDValueRecord) * pvIndex))
  else
    raise Exception.Create(SValueNotArray);
end;

procedure ClearDValue(ADValue:PDValueRecord);
  procedure ClearArray;
  var
    I: Cardinal;
  begin
    I := 0;
    while I < ADValue.Value.ArrayLength do
    begin
      ClearDValue(GetDValueItem(ADValue, I));
      Inc(I);
    end;
    FreeMem(ADValue.Value.ArrayItemsEntry);
  end;

begin
  if ADValue.ValueType <> vdtUnset then
  begin
    case ADValue.ValueType of
      vdtGuid:
        Dispose(ADValue.Value.AsGuid);
      vdtString:
        Dispose(ADValue.Value.AsString);
      vdtStream:
        FreeAndNil(ADValue.Value.AsStream);
      vdtArray:
        ClearArray;
    end;
    ADValue.ValueType := vdtUnset;
  end;
end;

procedure CheckDValueSetType(ADValue:PDValueRecord; AType: TDValueDataType);
begin
  if ADValue.ValueType <> AType then
  begin
    ClearDValue(ADValue);
    case AType of
      vdtGuid:
        New(ADValue.Value.AsGuid);
      vdtString:
        New(ADValue.Value.AsString);
      vdtStream:
        ADValue.Value.AsStream := TMemoryStream.Create;
      vdtArray:
        ADValue.Value.ArrayLength := 0;
    end;
    ADValue.ValueType := AType;
  end;
end;

procedure CheckDValueSetArrayLength(ADValue: PDValueRecord; ALen: Integer);
begin
  CheckDValueSetType(ADValue, vdtArray);
  if ALen > 0 then
  begin
    if ADValue.Value.ArrayLength = 0 then
    begin
      GetMem(ADValue.Value.ArrayItemsEntry, SizeOf(TDValueRecord) * ALen);
      ADValue.Value.ArrayLength := ALen;
    end
    else
    begin
      if Cardinal(ALen) > ADValue.Value.ArrayLength then
      begin
        ReallocMem(ADValue.Value.ArrayItemsEntry, SizeOf(TDValueRecord) * ALen);
        ADValue.Value.ArrayLength := ALen;
      end
      else
      begin
        while ADValue.Value.ArrayLength > Cardinal(ALen) do
        begin
          ClearDValue(GetDValueItem(ADValue, ADValue.Value.ArrayLength - 1));
          Dec(ADValue.Value.ArrayLength);
        end;
      end;
    end;
  end;
end;

procedure DValueSetAsString(ADValue:PDValueRecord; pvString:DStringW);
begin
  CheckDValueSetType(ADValue, vdtString);
  ADValue.Value.AsString^ := pvString;
end;

function DValueGetAsString(ADValue:PDValueRecord): DStringW;
var
  lvHexStr:String;
  function DTToStr(ADValue: PDValueRecord): DStringW;
  begin
    if Trunc(ADValue.Value.AsFloat) = 0 then
      Result := FormatDateTime({$IF RTLVersion>=22} FormatSettings.{$IFEND}LongTimeFormat, ADValue.Value.AsDateTime)
    else if IsZero(ADValue.Value.AsFloat - Trunc(ADValue.Value.AsFloat)) then
      Result := FormatDateTime
        ({$IF RTLVersion>=22}FormatSettings.{$IFEND}LongDateFormat,
        ADValue.Value.AsDateTime)
    else
      Result := FormatDateTime
        ({$IF RTLVersion>=22}FormatSettings.{$IFEND}LongDateFormat + ' ' +
{$IF RTLVersion>=22}FormatSettings.{$IFEND}LongTimeFormat, ADValue.Value.AsDateTime);
  end;

begin
  case ADValue.ValueType of
    vdtString:
      Result := ADValue.Value.AsString^;
    vdtUnset:
      Result := 'default';
    vdtNull:
      Result := 'null';
    vdtBoolean:
      Result := BoolToStr(ADValue.Value.AsBoolean, True);
    vdtSingle:
      Result := FloatToStr(ADValue.Value.AsSingle);
    vdtFloat:
      Result := FloatToStr(ADValue.Value.AsFloat);
    vdtInteger:
      Result := IntToStr(ADValue.Value.AsInteger);
    vdtInt64:
      Result := IntToStr(ADValue.Value.AsInt64);
    vdtCurrency:
      Result := CurrToStr(ADValue.Value.AsCurrency);
    vdtGuid:
      Result := GuidToString(ADValue.Value.AsGuid^);
    vdtDateTime:
      Result := DTToStr(ADValue);
    vdtStream:
      begin
        SetLength(lvHexStr, TMemoryStream(ADValue.Value.AsStream).Size * 2);
        BinToHex(
          TMemoryStream(ADValue.Value.AsStream).Memory,
          PChar(lvHexStr),
          TMemoryStream(ADValue.Value.AsStream).Size);

        Result := lvHexStr;
      end;
    vdtArray:
      Result := '@@Array';
  end;
end;

procedure DValueSetAsInt64(ADValue:PDValueRecord; pvValue:Int64);
begin
  CheckDValueSetType(ADValue, vdtInt64);
  ADValue.Value.AsInt64 := pvValue;
end;

function DValueGetAsInteger(ADValue: PDValueRecord): Integer;
begin
  case ADValue.ValueType of
    vdtInteger:
      Result := ADValue.Value.AsInteger;
    vdtInt64:
      Result := ADValue.Value.AsInt64;
    vdtUnset, vdtNull:
      Result := 0;
    vdtBoolean:
      Result := Integer(ADValue.Value.AsBoolean);
    vdtSingle:
      Result := Trunc(ADValue.Value.AsSingle);
    vdtFloat, vdtDateTime:
      Result := Trunc(ADValue.Value.AsFloat);
    vdtCurrency:
      Result := ADValue.Value.AsInt64 div 10000;
    vdtString:
      Result := StrToInt64(ADValue.Value.AsString^)
  else
    raise EConvertError.CreateFmt(SConvertError, [QValueTypeName[ADValue.ValueType],
      QValueTypeName[vdtInteger]]);
  end;
end;

function DValueGetAsInt64(ADValue: PDValueRecord): Int64;
begin
  case ADValue.ValueType of
    vdtInt64:
      Result := ADValue.Value.AsInt64;
    vdtInteger:
      Result := ADValue.Value.AsInteger;
    vdtUnset, vdtNull:
      Result := 0;
    vdtBoolean:
      Result := Integer(ADValue.Value.AsBoolean);
    vdtSingle:
      Result := Trunc(ADValue.Value.AsSingle);
    vdtFloat, vdtDateTime:
      Result := Trunc(ADValue.Value.AsFloat);
    vdtCurrency:
      Result := ADValue.Value.AsInt64 div 10000;
    vdtString:
      Result := StrToInt64(ADValue.Value.AsString^)
  else
    raise EConvertError.CreateFmt(SConvertError, [QValueTypeName[ADValue.ValueType],
      QValueTypeName[vdtInt64]]);
  end;
end;

procedure DValueSetAsInteger(ADValue:PDValueRecord; pvValue:Integer);
begin
  CheckDValueSetType(ADValue, vdtInteger);
  ADValue.Value.AsInt64 := pvValue;
  
end;

procedure DValueSetAsFloat(ADValue:PDValueRecord; pvValue:Double);
begin
  CheckDValueSetType(ADValue, vdtFloat);
  ADValue.Value.AsFloat := pvValue;
end;

function DValueGetAsFloat(ADValue: PDValueRecord): Double;
begin
  case ADValue.ValueType of
    vdtFloat, vdtDateTime:
      Result := ADValue.Value.AsFloat;
    vdtSingle:
      Result := ADValue.Value.AsSingle;
    vdtUnset, vdtNull:
      Result := 0;
    vdtBoolean:
      Result := Integer(ADValue.Value.AsBoolean);
    vdtInteger:
      Result := ADValue.Value.AsInteger;
    vdtInt64:
      Result := ADValue.Value.AsInt64;
    vdtCurrency:
      Result := ADValue.Value.AsCurrency;
    vdtString:
      Result := StrToFloat(ADValue.Value.AsString^)
  else
    raise EConvertError.CreateFmt(SConvertError, [QValueTypeName[ADValue.ValueType],
      QValueTypeName[vdtFloat]]);
  end;
end;

end.
