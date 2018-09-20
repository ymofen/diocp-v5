{
   数据量大建议采用字符串拼接方式拼接JSON。
   字符串拼接是最有效率的JSON打包方式
}
unit utils_dvalue_dataset;

interface

uses
  DB, SysUtils, utils_dvalue;

procedure ConvertDataSetToDValue(pvDataSet: TDataSet; pvDataList: TDValue);
    overload;
procedure ConvertDataSetToDValue(pvDataSet: TDataSet; pvDataList: TDValue;
    pvFields:string); overload;
procedure ConvertCurrentRecordToDValue(pvDataSet: TDataSet; pvJsonRecord:
    TDValue); overload;

procedure ConvertCurrentRecordToDValue(pvDataSet: TDataSet; pvJsonRecord:
    TDValue; pvFields:string); overload;

procedure AddAFieldValue(pvVal: TDValue; pvField: TField);

procedure AssignFieldValue(pvField:TField; pvVal:TDValue;
    pvIfNullThenEmpty:Boolean);

procedure AppendFromDValueList(pvDataSet: TDataSet; pvDataList: TDValue);
procedure AssignRecordFromDValue(pvDataSet: TDataSet; pvJsonRecord:TDValue);

function toSQLValue(pvField: TDValue; pvPrecision: Byte = 4): string;

implementation

uses
  utils_strings;

procedure ConvertDataSetToDValue(pvDataSet: TDataSet; pvDataList: TDValue);
var
  lvItem:TDValue;
begin
  pvDataSet.First;
  while not pvDataSet.Eof do
  begin
    lvItem := pvDataList.Add();
    ConvertCurrentRecordToDValue(pvDataSet, lvItem);
    pvDataSet.Next;
  end;
end;

procedure ConvertCurrentRecordToDValue(pvDataSet: TDataSet; pvJsonRecord:
    TDValue);
var
  lvField:TField;
  i:Integer;
begin
  for i := 0 to pvDataSet.FieldCount - 1 do
  begin
    lvField := pvDataSet.Fields[i];
    AddAFieldValue(pvJsonRecord, lvField);

  end;
end;

procedure AssignRecordFromDValue(pvDataSet: TDataSet; pvJsonRecord:TDValue);
var
  lvField:TField;
  i:Integer;
  lvValue:TDValue;
begin
  if not (pvDataSet.State in [dsInsert, dsEdit]) then pvDataSet.Edit;

  for i := 0 to pvDataSet.FieldCount - 1 do
  begin
    lvField := pvDataSet.Fields[i];
    lvValue := pvJsonRecord.FindByName(lvField.FieldName);
    AssignFieldValue(lvField, lvValue, False);

  end;
end;

procedure AppendFromDValueList(pvDataSet: TDataSet; pvDataList: TDValue);
var
  i: Integer;
begin
  for i := 0 to pvDataList.Count - 1 do
  begin
    pvDataSet.Append;
    AssignRecordFromDValue(pvDataSet, pvDataList[i]);
  end;
end;

procedure ConvertCurrentRecordToDValue(pvDataSet: TDataSet; pvJsonRecord:
    TDValue; pvFields:string); overload;
var
  lvStrs:TArrayStrings;
  i: Integer;
  lvField:TField;
begin
  lvStrs := SplitToArrayStr(pvFields, [';', ',']);
  for i := 0 to Length(lvStrs) - 1 do
  begin
    lvField := pvDataSet.FindField(lvStrs[i]);
    if lvField <> nil then
    begin
      AddAFieldValue(pvJsonRecord, lvField);
    end;
  end;
end;

procedure AddAFieldValue(pvVal: TDValue; pvField: TField);
begin
  case pvField.DataType of
    ftInteger, ftWord, ftSmallint, ftLargeint:
      pvVal.Add(pvField.FieldName, pvField.AsInteger);
    ftBCD, ftFMTBcd, ftFloat, ftCurrency:
      pvVal.Add(pvField.FieldName, pvField.AsFloat);
    ftBoolean:
      pvVal.Add(pvField.FieldName, pvField.AsBoolean);
    ftDate, ftDateTime, ftTime:
      pvVal.Add(pvField.FieldName, FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', pvField.AsDateTime));
  else
    pvVal.Add(pvField.FieldName, pvField.AsString);
  end;
end;

procedure ConvertDataSetToDValue(pvDataSet: TDataSet; pvDataList: TDValue;
    pvFields:string); overload;
var
  lvItem:TDValue;
begin
  pvDataSet.First;
  while not pvDataSet.Eof do
  begin
    lvItem := pvDataList.Add();
    ConvertCurrentRecordToDValue(pvDataSet, lvItem, pvFields);
    pvDataSet.Next;
  end;
end;

function toSQLValue(pvField: TDValue; pvPrecision: Byte = 4): string;
var
  lvFmt:String;
begin
  if pvField=nil then
     Result := 'NULL'
  else if pvField.Value.DataType in [vdtBoolean] then
    Result := '1'
  else if pvField.Value.DataType in [vdtNull, vdtUnset] then
    Result := 'NULL'
  else if pvField.Value.DataType in [vdtDateTime] then
    Result := '''' +  DateTimeToStr(pvField.AsDateTime) + ''''
  else if pvField.Value.DataType in [vdtFloat, vdtSingle] then
  begin
    lvFmt := '%.' + IntToStr(pvPrecision) + 'f';
    Result := Format(lvFmt, [pvField.AsFloat]);
  end else if pvField.Value.DataType in [vdtInt64, vdtInteger, vdtUInt64] then
    Result := pvField.AsString

  else
    Result := '''' +  pvField.AsString + '''';


end;

procedure AssignFieldValue(pvField:TField; pvVal:TDValue;
    pvIfNullThenEmpty:Boolean);
begin
  if pvField = nil then Exit;
  if pvVal=nil then
  begin
    if pvIfNullThenEmpty then pvField.Clear;
    Exit;
  end;

  if pvVal <> nil then
  begin
    case pvField.DataType of
      ftInteger, ftWord, ftSmallint, ftLargeint:
        pvField.AsVariant := pvVal.AsInteger;
      ftBCD, ftFMTBcd, ftFloat, ftCurrency:
        pvField.AsVariant := pvVal.AsFloat;
      ftBoolean:
        pvField.AsVariant := pvVal.AsBoolean;
      ftDate, ftDateTime, ftTime:
        begin
          pvField.AsVariant := StrToDateTime(pvVal.AsString);
        end
    else
      pvField.AsString := pvVal.AsString;
    end;
  end;

end;

end.
