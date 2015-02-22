unit AnsiStringTools;

interface

uses
  SysUtils;



type
  {$if CompilerVersion <= 18.5}
     TBytes = array of Byte;
  {$ifend}
  TAnsiStringTools = class(TObject)
  public
    class function ansiString2Utf8Bytes(v:AnsiString): TBytes;
    class function Utf8AnsiString2AnsiString(pvData:AnsiString): AnsiString;
    class function Utf8Bytes2AnsiString(pvData: TBytes): AnsiString;
  end;

implementation

class function TAnsiStringTools.ansiString2Utf8Bytes(v:AnsiString): TBytes;
var
  lvTemp:AnsiString;
begin
  lvTemp := AnsiToUtf8(v);
  SetLength(Result, Length(lvTemp));
  Move(lvTemp[1], Result[0],  Length(lvTemp));
end;

class function TAnsiStringTools.Utf8AnsiString2AnsiString(pvData:AnsiString):
    AnsiString;
begin
  Result := Utf8ToAnsi(pvData);
end;

class function TAnsiStringTools.Utf8Bytes2AnsiString(pvData: TBytes):
    AnsiString;
var
  lvTemp:AnsiString;
begin
  SetLength(lvTemp, Length(pvData));
  Move(pvData[0], lvTemp[1],  Length(pvData));
  Result := Utf8ToAnsi(lvTemp);
end;

end.
