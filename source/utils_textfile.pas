unit utils_textfile;

interface

uses
  utils_strings
  {$IFDEF MSWINDOWS}
   , windows
  {$ENDIF}
   ,SysUtils, Classes;

type
  PCharA = ^Byte;
  TDTextEncoding = (teUnknown, { 未知的编码 }
    teAuto, { 自动检测 }
    teAnsi, { Ansi编码 }
    teUnicode16LE, { Unicode LE 编码 }
    teUnicode16BE, { Unicode BE 编码 }
    teUTF8 { UTF8编码 }
    );

/// <summary>
///   检测文本编码
///   copy from qdac.qstring
/// </summary>
function DetectTextEncoding(const p: Pointer; l: Integer; var b: Boolean):
    TDTextEncoding;



function CharSizeU(c: PCharA): Integer;

function LoadTextFromFile(const pvFile: string; AEncoding: TDTextEncoding =
    teUnknown): String;

function LoadTextFromStream(AStream: TStream; AEncoding: TDTextEncoding =
    teUnknown): String; overload;

implementation

//function AnsiEncode(p: PWChar; l: Integer): QStringA;
//var
//  ps: PWChar;
//begin
//  if l <= 0 then
//  begin
//    ps := p;
//    while ps^ <> #0 do
//      Inc(ps);
//    l := ps - p;
//  end;
//  if l > 0 then
//  begin
//{$IFDEF MSWINDOWS}
//    Result.Length := WideCharToMultiByte(CP_ACP, 0, p, l, nil, 0, nil, nil);
//    WideCharToMultiByte(CP_ACP, 0, p, l, LPSTR(Result.Data), Result.Length,
//      nil, nil);
//{$ELSE}
//    Result.Length := l shl 1;
//    Result.FValue[0] := 0;
//    Move(p^, PQCharA(Result)^, l shl 1);
//    Result := TEncoding.Convert(TEncoding.Unicode, TEncoding.ANSI,
//      Result.FValue, 1, l shl 1);
//{$ENDIF}
//  end
//  else
//    Result.Length := 0;
//end;

function CharSizeU(c: PCharA): Integer;
begin
  if (c^ and $80) = 0 then
    Result := 1
  else
  begin
    if (c^ and $FC) = $FC then // 4000000+
      Result := 6
    else if (c^ and $F8) = $F8 then // 200000-3FFFFFF
      Result := 5
    else if (c^ and $F0) = $F0 then // 10000-1FFFFF
      Result := 4
    else if (c^ and $E0) = $E0 then // 800-FFFF
      Result := 3
    else if (c^ and $C0) = $C0 then // 80-7FF
      Result := 2
    else
      Result := 1;
  end
end;

function LoadTextFromFile(const pvFile: string; AEncoding: TDTextEncoding =
    teUnknown): String;
var
  AStream: TStream;
begin
  if FileExists(pvFile) then
  begin
    AStream := TFileStream.Create(pvFile, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadTextFromStream(AStream, AEncoding);
    finally
      AStream.Free;
    end;
  end else
  begin
    Result := STRING_EMPTY;
  end;
end;

function DetectTextEncoding(const p: Pointer; l: Integer; var b: Boolean):
    TDTextEncoding;
var
  pAnsi: PByte;
  pWide: PWideChar;
  I, AUtf8CharSize: Integer;
const
  NoUtf8Char: array [0 .. 3] of Byte = ($C1, $AA, $CD, $A8); // ANSI编码的联通
  function IsUtf8Order(var ACharSize: Integer): Boolean;
  var
    I: Integer;
    ps: PByte;
  const
    Utf8Masks: array [0 .. 4] of Byte = ($C0, $E0, $F0, $F8, $FC);
  begin
    ps := pAnsi;
    ACharSize := CharSizeU(PCharA(ps));
    Result := false;
    if ACharSize > 1 then
    begin
      I := ACharSize - 2;
      if ((Utf8Masks[I] and ps^) = Utf8Masks[I]) then
      begin
        Inc(ps);
        Result := True;
        for I := 1 to ACharSize - 1 do
        begin
          if (ps^ and $80) <> $80 then
          begin
            Result := false;
            Break;
          end;
          Inc(ps);
        end;
      end;
    end;
  end;

begin
  Result := teAnsi;
  b := false;
  if l >= 2 then
  begin
    pAnsi := PByte(p);
    pWide := PWideChar(p);
    b := True;
    if pWide^ = #$FEFF then
      Result := teUnicode16LE
    else if pWide^ = #$FFFE then
      Result := teUnicode16BE
    else if l >= 3 then
    begin
      if (pAnsi^ = $EF) and (PByte(IntPtr(pAnsi) + 1)^ = $BB) and
        (PByte(IntPtr(pAnsi) + 2)^ = $BF) then // UTF-8编码
        Result := teUTF8
      else // 检测字符中是否有符合UFT-8编码规则的字符，11...
      begin
        b := false;
        Result := teUnknown; // 假设为UTF8编码，然后检测是否有不符合UTF-8编码的序列
        I := 0;
        Dec(l, 2);
        while I <= l do
        begin
          if (pAnsi^ and $80) <> 0 then // 高位为1
          begin
            if (l - I >= 4) then
            begin
              if CompareMem(pAnsi, @NoUtf8Char[0], 4) then
              // 联通？是则忽略掉，不做UTF-8编码的判断依据
              begin
                Inc(pAnsi, 4);
                Inc(I, 4);
                Result := teAnsi;
                continue;
              end;
            end;
            if IsUtf8Order(AUtf8CharSize) then
            begin
              Inc(pAnsi, AUtf8CharSize);
              Result := teUTF8;
              Break;
            end
            else
            begin
              Result := teAnsi;
              Break;
            end;
          end
          else
          begin
            if pAnsi^ = 0 then // 00 xx (xx<128) 高位在前，是BE编码
            begin
              if PByte(IntPtr(pAnsi) + 1)^ < 128 then
              begin
                Result := teUnicode16BE;
                Break;
              end;
            end
            else if PByte(IntPtr(pAnsi) + 1)^ = 0 then // xx 00 低位在前，是LE编码
            begin
              Result := teUnicode16LE;
              Break;
            end;
            Inc(pAnsi);
            Inc(I);
          end;
          if Result = teUnknown then
            Result := teAnsi;
        end;
      end;
    end;
  end;
end;

function LoadTextFromStream(AStream: TStream; AEncoding: TDTextEncoding =
    teUnknown): String;
var
  ASize: Integer;
  ABuffer: TBytes;
  ABomExists: Boolean;
begin
  ASize := AStream.Size - AStream.Position;
  if ASize > 0 then
  begin
    SetLength(ABuffer, ASize);
    AStream.ReadBuffer((@ABuffer[0])^, ASize);
    if AEncoding in [teUnknown, teAuto] then
      AEncoding := DetectTextEncoding(@ABuffer[0], ASize, ABomExists)
    else if ASize >= 2 then
    begin
      case AEncoding of
        teUnicode16LE:
          ABomExists := (ABuffer[0] = $FF) and (ABuffer[1] = $FE);
        teUnicode16BE:
          ABomExists := (ABuffer[1] = $FE) and (ABuffer[1] = $FF);
        teUTF8:
          begin
            if ASize >= 3 then
              ABomExists := (ABuffer[0] = $EF) and (ABuffer[1] = $BB) and
                (ABuffer[2] = $BF)
            else
              ABomExists := false;
          end;
      end;
    end
    else
      ABomExists := false;
    if AEncoding = teAnsi then
      Result := ByteBufferToString(@ABuffer[0], ASize)
    else if AEncoding = teUTF8 then
    begin
      if ABomExists then
      begin
        if ASize > 3 then
          Result := Utf8BufferToString(@ABuffer[3], ASize - 3)
        else
          Result := STRING_EMPTY;
      end else
      begin
        Result := Utf8BufferToString(@ABuffer[0], ASize);
        if Length(Result) = 0 then
        begin
          Result := ByteBufferToString(@ABuffer[0], ASize);
        end;
      end;
    end
    else
    begin
      if AEncoding = teUnicode16BE then
        SwapBuff(@ABuffer[0], 0, ASize);
      if ABomExists then
      begin
        Result := WideBufferToStringW(@ABuffer[2], (ASize - 2))
      end else
      begin
        Result := WideBufferToStringW(@ABuffer[0], ASize);
      end;
    end;
  end
  else
    Result := STRING_EMPTY;
end;

end.
