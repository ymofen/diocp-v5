(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-03-05 12:53:38
 *     修复URLEncode，URLDecode在Anddriod和UNICODE下的异常
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
 
unit utils.strings;

interface

uses
  Classes, SysUtils;

type
{$IFDEF MSWINDOWS}  // Windows平台下面可以使用AnsiString
  URLString = AnsiString;
  URLChar = AnsiChar;
{$ELSE}
  // andriod下面使用
  URLString = String;
  URLChar = Char;
  {$DEFINE UNICODE_URL}
{$ENDIF}


/// <summary>
///   跳过字符
/// </summary>
/// <returns>
///   返回跳过的字符
/// </returns>
/// <param name="p"> 开始检测位置 </param>
/// <param name="pvChars"> 遇到这些字符后停止，然后返回 </param>
function SkipUntil(var p:PChar; pvChars: TSysCharSet): Integer;


/// <summary>
///   跳过字符
/// </summary>
/// <returns> Integer
/// </returns>
/// <param name="p"> (PChar) </param>
/// <param name="pvChars"> (TSysCharSet) </param>
function SkipChars(var p:PChar; pvChars: TSysCharSet): Integer;


/// <summary>
///   从左边开始截取字符
/// </summary>
/// <returns> Integer
/// </returns>
/// <param name="p"> (PChar) </param>
/// <param name="pvChars"> (TSysCharSet) </param>
function LeftUntil(var p:PChar; pvChars: TSysCharSet): string;

/// <summary>
///   根据SpliterChars中提供的字符，进行分割字符串，放入到Strings中
///     * 跳过字符前面的空格
/// </summary>
/// <returns>
///   返回分割的个数
/// </returns>
/// <param name="s"> 源字符串 </param>
/// <param name="pvStrings"> 输出到的字符串列表 </param>
/// <param name="pvSpliterChars"> 分隔符 </param>
function SplitStrings(s:String; pvStrings:TStrings; pvSpliterChars
    :TSysCharSet): Integer;

/// <summary>
///   URL数据解码,
///    Get和Post的数据都经过了url编码
/// </summary>
/// <returns>
///   返回解码后的URL数据
/// </returns>
/// <param name="ASrc"> 原始数据 </param>
/// <param name="pvIsPostData"> Post的原始数据中原始的空格经过UrlEncode后变成+号 </param>
function URLDecode(const ASrc: URLString; pvIsPostData: Boolean = true):URLString;

/// <summary>
///  将数据进行URL编码
/// </summary>
/// <returns>
///   返回URL编码好的数据
/// </returns>
/// <param name="S"> 需要编码的数据 </param>
/// <param name="pvIsPostData"> Post的原始数据中原始的空格经过UrlEncode后变成+号 </param>
function URLEncode(S: string; pvIsPostData: Boolean = true): URLString;



implementation

{$if CompilerVersion < 20}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ifend}


function SkipUntil(var p:PChar; pvChars: TSysCharSet): Integer;
var
  ps: PChar;
begin
  ps := p;
  while p^ <> #0 do
  begin
    if CharInSet(p^, pvChars) then
      Break
    else
      Inc(P);
  end;
  Result := p - ps;
end;

function LeftUntil(var p:PChar; pvChars: TSysCharSet): string;
var
  ps: PChar;
  l:Integer;
begin
  ps := p;
  while p^ <> #0 do
  begin
    if CharInSet(p^, pvChars) then
      Break
    else
      Inc(P);
  end;
  l := p-ps;
  if l = 0 then
  begin
    Result := '';
  end else
  begin
    SetLength(Result, l);
    if SizeOf(Char) = 1 then
    begin
      Move(ps^, PChar(Result)^, l);
    end else
    begin
      l := l shl 1;
      Move(ps^, PChar(Result)^, l);
    end;
  end;
end;

function SkipChars(var p:PChar; pvChars: TSysCharSet): Integer;
var
  ps: PChar;
begin
  ps := p;
  while p^ <> #0 do
  begin
    if CharInSet(p^, pvChars) then
      Inc(P)
    else
      Break;
  end;
  Result := p - ps;
end;


function SplitStrings(s:String; pvStrings:TStrings; pvSpliterChars
    :TSysCharSet): Integer;
var
  p:PChar;
  lvValue : String;
begin
  p := PChar(s);
  Result := 0;
  while True do
  begin
    // 跳过空白
    SkipChars(p, [' ']);
    lvValue := LeftUntil(P, pvSpliterChars);
    if lvValue = '' then exit;
    // 跳过分隔符
    SkipChars(p, pvSpliterChars);

    // 添加到列表中
    pvStrings.Add(lvValue);
    inc(Result);
  end;
end;


function URLDecode(const ASrc: URLString; pvIsPostData: Boolean = true): URLString;
var
  i, j: integer;
  {$IFDEF UNICODE_URL}
  lvRawBytes:TBytes;
  lvSrcBytes:TBytes;
  {$ENDIF}
begin

  {$IFDEF UNICODE_URL}
  SetLength(lvRawBytes, Length(ASrc));   // 预留后面一个字符串结束标志
  lvSrcBytes := TEncoding.ANSI.GetBytes(ASrc);
  j := 0;  // 从0开始
  i := 0;
  while i <= Length(ASrc) do
  begin
    if (pvIsPostData) and (lvSrcBytes[i] = 43) then   //43(+) 号变成空格, Post的原始数据中如果有 空格时会变成 +号
    begin
      lvRawBytes[j] := 32; // Ord(' ');
    end else if lvSrcBytes[i] <> 37 then      //'%' = 37
    begin
      lvRawBytes[j] :=lvSrcBytes[i];
    end else
    begin
      Inc(i); // skip the % char
      try
      lvRawBytes[j] := StrToInt('$' +URLChar(lvSrcBytes[i]) + URLChar(lvSrcBytes[i+1]));
      except end;
      Inc(i, 1);  // 再跳过一个字符.

    end;
    Inc(i);
    Inc(j);
  end;
  SetLength(lvRawBytes, j);
  Result := TEncoding.ANSI.GetString(lvRawBytes);
  {$ELSE}
  SetLength(Result, Length(ASrc));   // 预留后面一个字符串结束标志
  j := 1;  // 从1开始
  i := 1;
  while i <= Length(ASrc) do
  begin
    if (pvIsPostData) and (ASrc[i] = '+') then   // + 号变成空格, Post的原始数据中如果有 空格时会变成 +号
    begin
      Result[j] := ' ';
    end else if ASrc[i] <> '%' then
    begin
      Result[j] := ASrc[i];
    end else 
    begin
      Inc(i); // skip the % char
      try
      Result[j] := URLChar(StrToInt('$' + ASrc[i] + ASrc[i+1]));
      except end;
      Inc(i, 1);  // 再跳过一个字符.

    end;
    Inc(i);
    Inc(j);
  end;
  SetLength(Result, j - 1);
  {$ENDIF}

end;




function URLEncode(S: string; pvIsPostData: Boolean = true): URLString;
var
  i: Integer; // loops thru characters in string
  {$IFDEF UNICODE_URL}
  lvRawBytes:TBytes;
  {$ENDIF}
begin
  {$IFDEF UNICODE_URL}
  lvRawBytes := TEncoding.ANSI.GetBytes(S);
  for i := 0 to Length(lvRawBytes) - 1 do
  begin
    case lvRawBytes[i] of
      //'A' .. 'Z', 'a'.. 'z', '0' .. '9', '-', '_', '.':
      65..90, 97..122, 48..57, 45, 95, 46:
        Result := Result + URLChar(lvRawBytes[i]);
      //' ':
      32:
        if pvIsPostData then
        begin     // Post数据如果是空格需要编码成 +
          Result := Result + '+';
        end else
        begin
          Result := Result + '%20';
        end
    else
      Result := Result + '%' + SysUtils.IntToHex(lvRawBytes[i], 2);
    end;
  end;
  {$ELSE}
  Result := '';
  for i := 1 to Length(S) do
  begin
    case S[i] of
      'A' .. 'Z', 'a' .. 'z', '0' .. '9', '-', '_', '.':
        Result := Result + S[i];
      ' ':
        if pvIsPostData then
        begin     // Post数据如果是空格需要编码成 +
          Result := Result + '+';
        end else
        begin
          Result := Result + '%20';
        end
    else
      Result := Result + '%' + SysUtils.IntToHex(Ord(S[i]), 2);
    end;
  end;
  {$ENDIF}
end;

end.
