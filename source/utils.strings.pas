(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
 
unit utils.strings;

interface

uses
  Classes, SysUtils;


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
function URLDecode(const ASrc: String; pvIsPostData: Boolean = true): String;

/// <summary>
///  将数据进行URL编码
/// </summary>
/// <returns>
///   返回URL编码好的数据
/// </returns>
/// <param name="S"> 需要编码的数据 </param>
/// <param name="pvIsPostData"> Post的原始数据中原始的空格经过UrlEncode后变成+号 </param>
function URLEncode(S: string; pvIsPostData: Boolean = true): string;



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


function URLDecode(const ASrc: String; pvIsPostData: Boolean = true): String;
var
  i, j: integer;
  ESC: string;
begin
  i := 1;
  SetLength(Result, Length(ASrc));   // 预留后面一个字符串结束标志
  j := 1;
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
      ESC := ASrc[i] + ASrc[i+1];
      Inc(i, 1); // Then skip it.
      try
        Result[j] := Char(StrToInt('$' + ESC));  {do not localize}
      except end;
    end;
    Inc(i);
    Inc(j);
  end;
  SetLength(Result, j - 1);
end;




function URLEncode(S: string; pvIsPostData: Boolean = true): string;
var
  i: Integer; // loops thru characters in string
begin
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
end;

end.
