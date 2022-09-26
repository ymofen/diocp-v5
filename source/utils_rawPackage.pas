unit utils_rawPackage;

interface

uses
  classes, SysUtils;

type
  PRawPackage = ^TRawPackage;

  TRawPackage = record
    FRawBytes: TBytes;
    FStartBytes: TBytes;
    FEndBytes: TBytes;
    FStartMatchIndex: Integer;
    FStartBytesLength: Integer;
    FEndMatchIndex: Integer;
    FEndBytesLength: Integer;
    FRawLength: Integer;
  end;

procedure SetPackageMaxLength(pvRaw: PRawPackage; l: Cardinal);
procedure SetPackageStartBytes(pvRaw: PRawPackage; pvStartBytes: TBytes;
  pvOffset, pvLength: Cardinal);
procedure SetPackageEndBytes(pvRaw: PRawPackage; pvEndBytes: TBytes;
  pvOffset, pvLength: Cardinal);
procedure SetPackageEndIsLineBreak(pvRaw: PRawPackage);

procedure ResetPacakge(pvRaw: PRawPackage);

procedure InitialPack(pvRaw:PRawPackage);

function InputBuffer(pvRaw: PRawPackage; pvData: Byte): Integer;


implementation

procedure SetPackageMaxLength(pvRaw: PRawPackage; l: Cardinal);
begin
  SetLength(pvRaw.FRawBytes, l);
end;

procedure SetPackageStartBytes(pvRaw: PRawPackage; pvStartBytes: TBytes;
  pvOffset, pvLength: Cardinal);
begin
  if pvLength = 0 then
  begin
    pvLength := Length(pvStartBytes) - pvOffset;
  end;
  
  SetLength(pvRaw.FStartBytes, pvLength);
  pvRaw.FStartBytesLength := pvLength;
  Move(pvStartBytes[pvOffset], pvRaw.FStartBytes[0], pvLength);
end;

procedure SetPackageEndBytes(pvRaw: PRawPackage; pvEndBytes: TBytes;
  pvOffset, pvLength: Cardinal);
begin
  if pvLength = 0 then
  begin
    pvLength := Length(pvEndBytes) - pvOffset;
  end;
  SetLength(pvRaw.FEndBytes, pvLength);
  pvRaw.FEndBytesLength := pvLength;
  Move(pvEndBytes[pvOffset], pvRaw.FEndBytes[0], pvLength);
end;

procedure ResetPacakge(pvRaw: PRawPackage);
begin
  pvRaw.FStartMatchIndex := 0;
  pvRaw.FEndMatchIndex := 0;
  pvRaw.FRawLength := 0;

  FillChar(pvRaw.FRawBytes[0], Length(pvRaw.FRawBytes), 0);
end;

function InputBuffer(pvRaw: PRawPackage; pvData: Byte): Integer;
var
  lvIndex: Integer;
begin
  lvIndex := pvRaw.FRawLength;
  pvRaw.FRawBytes[lvIndex] := pvData;
  inc(pvRaw.FRawLength);

  if (pvRaw.FRawLength <= pvRaw.FStartBytesLength) then
  begin
    if (pvRaw.FRawBytes[lvIndex] <> pvRaw.FStartBytes[lvIndex]) then
    begin
      ResetPacakge(pvRaw);
      Result := -1;
      Exit;
    end;

    Result := 0;
    Exit;
  end;

  if (pvRaw.FRawBytes[lvIndex] = pvRaw.FEndBytes[pvRaw.FEndMatchIndex]) then
  begin
    inc(pvRaw.FEndMatchIndex);
    if (pvRaw.FEndMatchIndex = pvRaw.FEndBytesLength) then
    begin
      Result := 1;
      Exit;
    end;
  end
  else
  begin
    pvRaw.FEndMatchIndex := 0;
  end;

  if pvRaw.FRawLength = Length(pvRaw.FRawBytes) then
  begin
    Result := -2;
    ResetPacakge(pvRaw);
    Exit;
  end;
  Result := 0;
  Exit;
end;

procedure SetPackageEndIsLineBreak(pvRaw: PRawPackage);
begin
  SetLength(pvRaw.FEndBytes, 2);
  pvRaw.FEndBytesLength := 2;
  pvRaw.FEndBytes[0] := 13;
  pvRaw.FEndBytes[1] := 10;

end;

procedure InitialPack(pvRaw:PRawPackage);
begin
  pvRaw.FStartMatchIndex := 0;
  pvRaw.FStartBytesLength := 0;
  SetLength(pvRaw.FStartBytes, 0);

  pvRaw.FEndMatchIndex := 0;
  pvRaw.FEndBytesLength := 0;
  SetLength(pvRaw.FEndBytes, 0);

  pvRaw.FRawLength := 0;
  SetLength(pvRaw.FRawBytes, 0);

end;



end.
