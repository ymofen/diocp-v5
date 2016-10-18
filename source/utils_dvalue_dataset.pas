unit utils_dvalue_dataset;

interface

uses
  DB, SysUtils, utils_dvalue;

procedure ConvertDataSetToDValue(pvDataSet: TDataSet; pvDataList: TDValue);
procedure ConvertCurrentRecordToDValue(pvDataSet: TDataSet; pvJsonRecord:
    TDValue);

procedure AppendFromDValueList(pvDataSet: TDataSet; pvDataList: TDValue);
procedure AssignRecordFromDValue(pvDataSet: TDataSet; pvJsonRecord:TDValue);

implementation

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
    case lvField.DataType of
      ftInteger, ftWord, ftSmallint, ftLargeint:
        pvJsonRecord.Add(lvField.FieldName, lvField.AsInteger);
      ftBCD, ftFMTBcd, ftFloat, ftCurrency:
        pvJsonRecord.Add(lvField.FieldName, lvField.AsFloat);
      ftBoolean:
        pvJsonRecord.Add(lvField.FieldName, lvField.AsBoolean);
      ftDate, ftDateTime, ftTime:
        pvJsonRecord.Add(lvField.FieldName, FormatDateTime('yyyy-MM-dd hh:nn.ss.zzz', lvField.AsDateTime));
    else
      pvJsonRecord.Add(lvField.FieldName, lvField.AsString);
    end; 
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

end.
