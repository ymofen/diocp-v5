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

procedure AppendFromDValueList(pvDataSet: TDataSet; pvDataList: TDValue);
procedure AssignRecordFromDValue(pvDataSet: TDataSet; pvJsonRecord:TDValue);

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
    if lvValue <> nil then
    begin
      case lvField.DataType of
        ftInteger, ftWord, ftSmallint, ftLargeint:
          lvField.AsVariant := lvValue.AsInteger;
        ftBCD, ftFMTBcd, ftFloat, ftCurrency:
          lvField.AsVariant := lvValue.AsFloat;
        ftBoolean:
          lvField.AsVariant := lvValue.AsBoolean;
        ftDate, ftDateTime, ftTime:
          begin
            lvField.AsVariant := StrToDateTime(lvValue.AsString);
          end
      else
        lvField.AsString := lvValue.AsString;
      end;
    end;
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
      pvVal.Add(pvField.FieldName, FormatDateTime('yyyy-MM-dd hh:nn.ss.zzz', pvField.AsDateTime));
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

end.
