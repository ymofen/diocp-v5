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
 *   2015-04-02 12:52:43
 *     修复SplitStrings,分隔符最后一个字符串没有加入的bug  abcd&33&eef
 *       感谢(Xjumping  990669769)反馈bug

 *  修正SearchPointer中的一严重bug(只比较了前两位字符的匹配性)
      2015-09-11 09:08:22
 *)


unit utils_strings;


interface

{$if CompilerVersion>= 28}    // XE7:28
  {$DEFINE USE_NetEncoding}
{$ifend}

uses
  Classes, SysUtils
{$IFDEF MSWINDOWS}
    , windows
{$ELSE}
{$IFDEF USE_NetEncoding}
    , System.NetEncoding
{$ENDIF}
{$ENDIF}

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
    , AnsiStrings
{$IFEND >=XE5}
  , Math;

const
  BUFFER_BLOCK_SIZE = $2000;  // Must be a power of 2
  STRING_EMPTY = '';
{$IFDEF MSWINDOWS}
  STRING_EMPTY_A :AnsiString = '';
{$ENDIF}

type
{$IFDEF MSWINDOWS}
  RAWString = AnsiString;
{$ELSE}
  RAWString = String;
{$ENDIF}

  // 用于存储字符串
  PMAPKeyString = ^MAPKeyString;
  MAPKeyString = record
    key:Integer;
    value: string;
  end; 


{$IF (RTLVersion>=26)}

{$ELSE}
  MarshaledAString = PAnsiChar;
{$IFEND >=XE5}


{$IFDEF MSWINDOWS}  // Windows平台下面可以使用AnsiString
  URLString = AnsiString;
  URLChar = AnsiChar;
{$ELSE}
  // andriod下面使用
  URLString = String;
  URLChar = Char;
  {$DEFINE UNICODE_URL}
{$ENDIF}

{$IFDEF UNICODE}
  WChar = Char;
  PWChar = PChar;
{$ELSE}
  WChar = WideChar;
  PWChar = PWideChar;
{$ENDIF}



{$if (sizeof(Char) = 1)}
  {$IFDEF FPC}
  DStringW = UnicodeString;
  {$ELSE}
  DStringW = WideString;
  {$ENDIF}
  DCharW = WideChar;
  PDCharW = PWideChar;
  PDStringW = ^DStringW;
{$else}
  DCharW = Char;
  PDCharW = PChar;
  DStringW = string;
  PDStringW = ^DStringW;
{$ifend}

  // 25:XE5
  {$IF CompilerVersion<=25}
  IntPtr=Integer;
  {$IFEND}

  {$if CompilerVersion < 18} //before delphi 2007
  TBytes = array of Byte;
  {$ifend}


  TArrayStrings = array of string;
  PArrayStrings = ^ TArrayStrings;

  TCharArray = array of Char;
  
  TDStringBuilder = class(TObject)
  private
    FData: TCharArray;
    FPosition: Integer;
    FCapacity :Integer;
    FLineBreak: String;
    procedure CheckNeedSize(pvSize: LongInt);
    function GetLength: Integer;
  public
    constructor Create;
    procedure Clear;
    function Append(c:Char): TDStringBuilder;  overload;
    function Append(str:string): TDStringBuilder; overload;
    function Append(str:string; pvLeftStr:string; pvRightStr:String):
        TDStringBuilder; overload;
    function Append(v: Boolean; UseBoolStrs: Boolean = True): TDStringBuilder;
        overload;
    function Append(v:Integer): TDStringBuilder; overload;
    function Append(v:Double): TDStringBuilder; overload;
    function AppendQuoteStr(str:string): TDStringBuilder;
    function AppendSingleQuoteStr(str:string): TDStringBuilder;
    function AppendLine(str:string): TDStringBuilder;

    function ToString: string;{$IFDEF UNICODE}override;{$ENDIF}
    property Length: Integer read GetLength;

    /// <summary>
    ///   换行符: 默认#13#10
    /// </summary>
    property LineBreak: String read FLineBreak write FLineBreak;
  end;


  TDBufferBuilder = class(TStream)
  private
    FData: TBytes;
    FPosition: Integer;
    FSize: Integer;
    FCapacity :Integer;
    FBufferLocked:Boolean;
    FLineBreak: String;

    procedure CheckNeedSize(pvSize: LongInt); overload;
    procedure CheckNeedSize(pvOffset, pvSize: LongInt); overload;
    function GetLength: Integer;
    function GetRemain: Integer;
  public
    constructor Create;
    procedure Clear;
    function Append(const aByte:Byte): TDBufferBuilder; overload;
    function Append(const w:Word):TDBufferBuilder; overload;
    function Append(const c: Char): TDBufferBuilder; overload;
    function Append(str:string): TDBufferBuilder; overload;
    function Append(str:string; pvLeftStr:string; pvRightStr:String):
        TDBufferBuilder; overload;
    function Append(v: Boolean; UseBoolStrs: Boolean = True): TDBufferBuilder;
        overload;
    function Append(v:Integer): TDBufferBuilder; overload;
    function Append(v:Double): TDBufferBuilder; overload;
    function AppendUtf8(str:String): TDBufferBuilder;
    function AppendRawStr(pvRawStr:RAWString): TDBufferBuilder;
    function AppendBreakLineBytes: TDBufferBuilder;
    function Append(str: string; pvConvertToUtf8Bytes: Boolean): TDBufferBuilder; overload;
    function AppendQuoteStr(str:string): TDBufferBuilder;
    function AppendSingleQuoteStr(str:string): TDBufferBuilder;
    function AppendLine(str:string): TDBufferBuilder;

    procedure LoadFromFile(pvFileName:string);

    procedure LoadFromStream(pvStream: TStream); overload;
    procedure SaveToFile(pvFile:String);

    procedure SaveToStream(pvStream:TStream);

    /// <summary>
    ///   写入数据
    /// </summary>
    function AppendBuffer(pvBuffer:PByte; pvLength:Integer): TDBufferBuilder;

    /// <summary>
    ///   读取数据
    /// </summary>
    function ReadBuffer(pvBuffer:PByte; pvLength:Integer): Cardinal;

    function PeekBuffer(pvBuffer:PByte; pvLength:Integer): Cardinal;

    /// <summary>
    ///   读取一个字节
    /// </summary>
    function ReadByte(var vByte: Byte): Boolean;

    /// <summary>
    ///   提前获取并且锁定一块Buffer
    /// </summary>
    function GetLockBuffer(pvLength:Integer): PByte;

    /// <summary>
    ///    释放最后一次锁定的Buffer, 并且写入指定长度的数据
    /// </summary>
    function ReleaseLockBuffer(pvLength:Integer): TDBufferBuilder;

    /// <summary>
    ///   整个数据(不移动数据指针)
    /// </summary>
    function ToBytes: TBytes;

    function DecodeUTF8: string;

    function ToRAWString: RAWString;

    /// <summary>
    ///   数据内存指针
    /// </summary>
    function Memory: PByte;

    /// <summary>
    ///  返回一个指针
    ///  pvIndex是数据下标(从0开始)
    /// </summary>
    function MemoryBuffer(const pvIndex: Integer): PByte;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Integer; override;
    procedure SetSize(NewSize: Longint); override;

    /// <summary>
    ///   重新排列可用数据
    /// </summary>
    function ReArrange: TDBufferBuilder;

    function GetInstanceSize: Integer;


    /// <summary>
    ///   所有数据长度
    /// </summary>
    property Length: Integer read GetLength;

    /// <summary>
    ///   换行符: 默认#13#10
    /// </summary>
    property LineBreak: String read FLineBreak write FLineBreak;

    /// <summary>
    ///   剩余数据长度
    /// </summary>
    property Remain: Integer read GetRemain;




  end;


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
///     跳过字符, 没有找到则不移动P
/// </summary>
/// <returns>
///   没找到返回-1
///   如果找到, 则返回跳过的字符
/// </returns>
/// <param name="p"> 开始检测位置 </param>
/// <param name="pvChars"> 遇到这些字符后停止，然后返回 </param>
function SkipUntilEx(var p:PChar; pvChars: TSysCharSet): Integer;



/// <summary>
///   跳过字符
/// </summary>
/// <returns>
///   返回跳过的字符个数
/// </returns>
/// <param name="p"> 源(字符串)位置 </param>
/// <param name="pvChars"> (TSysCharSet) </param>
function SkipChars(var p:PChar; pvChars: TSysCharSet): Integer;

/// <summary>
///   跳过字符串
///   // p = pchar("abcabcefggg");
///   // 执行后 p = "efgg"
///   // 返回结果 = 2 //2个abc
///   SkipStr(p, "abc");
///
/// </summary>
/// <returns>
///   返回跳过的字符串个数
/// </returns>
/// <param name="P"> 源字符，如果符合条件跳过 </param>
/// <param name="pvSkipStr"> 开头需要跳过的字符 </param>
/// <param name="pvIgnoreCase"> 忽略大小写 </param>
function SkipStr(var P:PChar; pvSkipStr: PChar; pvIgnoreCase: Boolean = true):
    Integer;


/// <summary>
///   检测是否以pvStart开头
/// </summary>
/// <returns> 如果为真返回true
/// </returns>
/// <param name="P"> (PChar) </param>
/// <param name="pvStart"> (PChar) </param>
/// <param name="pvIgnoreCase"> (Boolean) </param>
function StartWith(P:PChar; pvStart:PChar; pvIgnoreCase: Boolean = true):
    Boolean;


/// <summary>
///   从左边开始截取字符
/// </summary>
/// <returns>
///   返回截取到的字符串
///   没有匹配到会返回空字符串
/// </returns>
/// <param name="p"> 源(字符串)开始的位置, 匹配成功会出现在pvSpliter的首次出现位置, 否则不会进行移动</param>
/// <param name="pvChars"> (TSysCharSet) </param>
function LeftUntil(var p:PChar; pvChars: TSysCharSet): string; overload;

/// <summary>
///   从左边开始截取字符
/// </summary>
/// <param name="vLeftStr">截取到的字符串</param>
/// <returns>
///    0: 截取成功(p停留在pvChars中首次出现的位置)
///   -1: 匹配失败(p不会移动)
/// </returns>
/// <param name="p"> 源(字符串)开始的位置, 匹配成功会出现在pvChars的首次出现位置, 否则不会进行移动</param>
function LeftUntil(var p: PChar; pvChars: TSysCharSet; var vLeftStr: string):
    Integer; overload;


/// <summary>
///   从左边开始截取字符串
/// </summary>
/// <returns>
///   返回截取到的字符串
/// </returns>
/// <param name="p"> 源(字符串), 匹配成功会出现在pvSpliter的首次出现位置, 否则不会进行移动 </param>
/// <param name="pvSpliter"> 分割(字符串) </param>
function LeftUntilStr(var P: PChar; pvSpliter: PChar; pvIgnoreCase: Boolean =
    true): string;

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
///  将一个字符串分割成2个字符串
///  splitStr("key=abcd", "=", s1, s2)
///  // s1=key, s2=abcd
/// </summary>
/// <returns> 成功返回true
/// </returns>
/// <param name="s"> 要分割的字符串 </param>
/// <param name="pvSpliterStr"> (string) </param>
/// <param name="s1"> (String) </param>
/// <param name="s2"> (String) </param>
function SplitStr(s:string; pvSpliterStr:string; var s1, s2:String): Boolean;

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
function URLEncode(S: URLString; pvIsPostData: Boolean = true): URLString;


/// <summary>
///  在Strings中根据名称搜索值
/// </summary>
/// <returns> String
/// </returns>
/// <param name="pvStrings"> (TStrings) </param>
/// <param name="pvName"> (string) </param>
/// <param name="pvSpliters"> 名字和值的分割符 </param>
function StringsValueOfName(pvStrings: TStrings; const pvName: string;
    pvSpliters: TSysCharSet; pvTrim: Boolean): String;

/// <summary>
///   s := content-type: application/json; chartset=utf-8
///   GetStrValueOfName(s, 'charset', ['=',' ', #13, #10], [';']) = 'utf-8'
/// </summary>
/// <returns> string
/// </returns>
/// <param name="pvStr"> (string) </param>
/// <param name="pvName"> (string) </param>
/// <param name="pvSplitChars"> (TSysCharSet) </param>
/// <param name="pvEndChars"> (TSysCharSet) </param>
function GetStrValueOfName(const pvStr, pvName: string; pvSplitChars,
    pvEndChars: TSysCharSet): string;


/// <summary>
///   查找PSub在P中出现的第一个位置
///   精确查找
///   如果PSub为空字符串(#0, nil)则直接返回P
/// </summary>
/// <returns>
///   如果找到, 返回第一个字符串位置
///   找不到返回False
///   * 来自qdac.qstrings
/// </returns>
/// <param name="P"> 要开始查找(字符串) </param>
/// <param name="PSub"> 要搜(字符串) </param>
function StrStr(P:PChar; PSub:PChar): PChar;

/// <summary>
///   查找PSub在P中出现的第一个位置
///   忽略大小写
///   如果PSub为空字符串(#0, nil)则直接返回P
/// </summary>
/// <returns>
///   如果找到, 返回第一个字符串位置
///   找不到返回nil
///   * 来自qdac.qstrings
/// </returns>
/// <param name="P"> 要开始查找(字符串) </param>
/// <param name="PSub"> 要搜(字符串) </param>
function StrStrIgnoreCase(P, PSub: PChar): PChar;


/// <summary>
///  字符转大写
///  * 来自qdac.qstrings
/// </summary>
function UpperChar(c: Char): Char;

/// <summary>
///  aStr是否在Strs列表中
/// </summary>
/// <returns>
///   如果在列表中返回true
/// </returns>
/// <param name="pvStr"> sensors,1,3.1415926,1.1,1.2,1.3 </param>
/// <param name="pvStringList"> (array of string) </param>
function StrIndexOf(const pvStr: string; const pvStringList: array of string):
    Integer;

/// <summary>
///   查找PSub在P中出现的第一个位置
/// </summary>
/// <returns>
///   如果找到, 返回指向第一个pvSub的位置
///   找不到返回 Nil
/// </returns>
/// <param name="pvSource"> 数据 </param>
/// <param name="pvSourceLen"> 数据长度 </param>
/// <param name="pvSub"> 查找的数据 </param>
/// <param name="pvSubLen"> 查找的数据长度 </param>
function SearchPointer(pvSource: Pointer; pvSourceLen, pvStartIndex: Integer;
    pvSub: Pointer; pvSubLen: Integer): Pointer;


/// <summary>procedure DeleteChars
/// </summary>
/// <returns> string
/// </returns>
/// <param name="s"> (string) </param>
/// <param name="pvCharSets"> (TSysCharSet) </param>
function DeleteChars(const s: string; pvCharSets: TSysCharSet): string;

/// <summary>
///  转换字符串到Bytes
/// </summary>
function StringToUtf8Bytes(pvData:String; pvBytes:TBytes): Integer;overload;
function StringToUtf8Bytes(const pvData: string; pvProcessEndByte: Boolean = false): TBytes; overload;
/// <summary>
///
/// </summary>
function Utf8BytesToString(pvBytes: TBytes; pvOffset: Integer): String;

function Utf8BufferToString(pvBuff: PByte; pvLen: Integer): string;



function WideBufferToStringW(pvBuffer:Pointer; pvBufLength:Integer): DStringW;

function StringToBytes(pvData:String; pvBytes:TBytes): Integer; overload;

function StringToBytes(pvData:string): TBytes; overload;

/// <summary>
///   请注意pvBytes后面不可预计字符串
/// </summary>
function BytesToString(pvBytes: TBytes; pvOffset: Integer): String;
function ByteBufferToString(pvBuff:PByte; pvLen:Cardinal): string;

/// <summary>
///   计算AnsiString字符串的长度(到0截至)
/// </summary>
/// <returns>
///   返回字符串长度，
///    -1：失败
/// </returns>
function AnsiStringLength(pvBuff: Pointer; pvMaxLength: Integer = 0): Integer;

function SpanPointer(const pvStart, pvEnd: PByte): Integer;

function IsHexChar(c: Char): Boolean;

function HexValue(c: Char): Integer;

function HexChar(V: Byte): Char;

function HexToInt(const p:PChar; pvLength:Integer): Integer;

function PickString(p: PChar; pvOffset, pvCount: Integer): String;

/// <summary>
///  从Utf8无BOM格式的文件中加载字符串
/// </summary>
function LoadStringFromUtf8NoBOMFile(pvFile:string): String;

procedure WriteStringToUtf8NoBOMFile(pvFile, pvData: String);

/// <summary>
///   转换字符串,
///   copy from qdac.qstrings.pas
/// </summary>
function ParseNumeric(var S: PChar; var ANum: Extended): Boolean;
function ParseHex(var p: PChar; var Value: Int64): Integer;
function ParseInt(var S: PChar; var ANum: Int64): Integer;

{$if CompilerVersion < 20}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
{$ifend}

function GetTickCount: Cardinal;

function GetCurrentThreadID: Cardinal;

function ObjectHexAddr(pvObj:TObject): String;
function ObjectIntStrAddr(pvObj:TObject): String;

function DateTimeString(pvDateTime:TDateTime): string; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
function NowString: String; {$IFDEF HAVE_INLINE} inline;{$ENDIF}

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;

procedure SwapBuff(buf: Pointer; offset, len: Integer); overload;

/// <summary>
///   为字符串新建一个PString指针，并与s建立对应关系
/// </summary>
function NewPString(const s: string): PString;

function GetStringFromPString(const p:Pointer): string;

function NewMapKeyString(const key:Integer; const s:string): PMAPKeyString;


procedure PrintDebugString(s:string); {$IFDEF HAVE_INLINE} inline;{$ENDIF}

function PosWStr(sub: DStringW; const s: DStringW): Integer;



implementation



{$IFDEF MSWINDOWS}
type
  TMSVCStrStr = function(s1, s2: PAnsiChar): PAnsiChar; cdecl;
  TMSVCStrStrW = function(s1, s2: PWChar): PWChar; cdecl;
  TMSVCMemCmp = function(s1, s2: Pointer; len: Integer): Integer; cdecl;

var
  hMsvcrtl: HMODULE;

{$IFDEF UNICODE}
  VCStrStrW: TMSVCStrStrW;
{$ELSE}
  VCStrStr: TMSVCStrStr;
{$ENDIF}
//  VCMemCmp: TMSVCMemCmp;
{$ENDIF}

procedure PrintDebugString(s:string);
begin
  {$IFDEF MSWINDOWS}
  {$IFDEF UNICODE}
  OutputDebugStringW(PChar(s));
  {$ELSE}
  OutputDebugString(PAnsiChar(s));
  {$ENDIF}
  {$ENDIF}

end;

{$if CompilerVersion < 20}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ifend}

function IsHexChar(c: Char): Boolean;
begin
  Result := ((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or
    ((c >= 'A') and (c <= 'F'));
end;

function HexValue(c: Char): Integer;
begin
  if (c >= '0') and (c <= '9') then
    Result := Ord(c) - Ord('0')
  else if (c >= 'a') and (c <= 'f') then
    Result := 10 + Ord(c) - Ord('a')
  else
    Result := 10 + Ord(c) - Ord('A');
end;

function HexChar(V: Byte): Char;
begin
  if V < 10 then
    Result := Char(V + Ord('0'))
  else
    Result := Char(V - 10 + Ord('A'));
end;

procedure SwapBuff(buf: Pointer; offset, len: Integer);
var
  lvStart, lvEnd: PByte;
  lvByte: Byte;
begin
  lvStart := PByte(buf);
  Inc(lvStart, offset);
  
  lvEnd := lvStart;
  Inc(lvEnd, len - 1);

  while IntPtr(lvStart) < IntPtr(lvEnd) do
  begin
    lvByte := lvStart^;
    lvStart^ := lvEnd^;
    lvEnd^ := lvByte;
    Inc(lvStart);
    Dec(lvEnd);
  end;
end;



function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;


function DeleteChars(const s: string; pvCharSets: TSysCharSet): string;
var
  i, l, times: Integer;
  lvStr: string;
begin
  l := Length(s);
  SetLength(lvStr, l);
  times := 0;
  for i := 1 to l do
  begin
    if not CharInSet(s[i], pvCharSets) then
    begin
      inc(times);
      lvStr[times] := s[i];
    end;
  end;
  SetLength(lvStr, times);
  Result := lvStr;
end;


function StrIndexOf(const pvStr: string; const pvStringList: array of string):
    Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(pvStringList) to High(pvStringList) do
  begin
    if SameText(pvStringList[i], pvStr) then
    begin
      Result := i;
      Break;
    end;
  end;
end;


function UpperChar(c: Char): Char;
begin
  {$IFDEF UNICODE}
  if (c >= #$61) and (c <= #$7A) then
    Result := Char(PWord(@c)^ xor $20)
  else
    Result := c;
  {$ELSE}
  if (c >= #$61) and (c <= #$7A) then
    Result := Char(ord(c) xor $20)
  else
    Result := c;
  {$ENDIF}
end;


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
  lvPTemp: PChar;
  l:Integer;
  lvMatched: Byte;
begin
  lvMatched := 0;
  lvPTemp := p;
  while lvPTemp^ <> #0 do
  begin
    if CharInSet(lvPTemp^, pvChars) then
    begin            // 匹配到
      lvMatched := 1;
      Break;
    end else
      Inc(lvPTemp);
  end;
  if lvMatched = 0 then
  begin   // 没有匹配到
    Result := '';
  end else
  begin   // 匹配到
    l := lvPTemp-P;
    SetLength(Result, l);
    if SizeOf(Char) = 1 then
    begin
      Move(P^, PChar(Result)^, l);
    end else
    begin
      l := l shl 1;
      Move(P^, PChar(Result)^, l);
    end;
    P := lvPTemp;  // 跳转到新位置
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


function ParseHex(var p: PChar; var Value: Int64): Integer;
var
  ps: PChar;
begin
  Value := 0;
  ps := p;
  while IsHexChar(p^) do
  begin
    Value := (Value shl 4) + HexValue(p^);
    Inc(p);
  end;
  Result := p - ps;
end;


function ParseInt(var S: PChar; var ANum: Int64): Integer;
var
  ps: PChar;
  ANeg: Boolean;
begin
  ps := S;
  // 跳过16进制开始字符
  if S[0] = '$' then
  begin
    Inc(S);
    Result := ParseHex(S, ANum);
  end
  else if (S[0] = '0') and ((S[1] = 'x') or (S[1] = 'X')) then
  begin
    Inc(S, 2);
    Result := ParseHex(S, ANum);
  end
  else
  begin
    if (S^ = '-') then
    begin
      ANeg := True;
      Inc(S);
    end
    else
    begin
      ANeg := False;
      if S^ = '+' then
        Inc(S);
    end;
    ANum := 0;
    while (S^ >= '0') and (S^ <= '9') do
    begin
      ANum := ANum * 10 + Ord(S^) - Ord('0');
      if ANum < 0 then // 溢出？
      begin
        Result := 0;
        S := ps;
        Exit;
      end;
      Inc(S);
    end;
    if ANeg then
      ANum := -ANum;
    Result := S - ps;
  end;
end;

function ParseNumeric(var S: PChar; var ANum: Extended): Boolean;
var
  ps: PChar;
  function ParseHexInt: Boolean;
  var
    iVal: Int64;
  begin
    iVal := 0;
    while IsHexChar(S^) do
    begin
      iVal := (iVal shl 4) + HexValue(S^);
      Inc(S);
    end;
    Result := (S <> ps);
    ANum := iVal;
  end;

  function ParseDec: Boolean;
  var
    ACount: Integer;
    iVal: Int64;
    APow: Extended;
    ANeg: Boolean;
  begin
    try
      ANeg := S^ = '-';
      if ANeg then
        Inc(S);
      Result := ParseInt(S, iVal) > 0;
      if not Result then
        Exit;
      if ANeg then
        ANum := -iVal
      else
        ANum := iVal;
      if S^ = '.' then // 小数部分
      begin
        Inc(S);
        ACount := ParseInt(S, iVal);
        if ACount > 0 then
        begin
          if (ANum < 0) or ANeg then
            ANum := ANum - iVal / IntPower(10, ACount)
          else
            ANum := ANum + iVal / IntPower(10, ACount);
        end;
      end;
      if (S^ = 'e') or (S^ = 'E') then
      begin
        Inc(S);
        if ParseNumeric(S, APow) then
        begin
          ANum := ANum * Power(10, APow);

        end;
      end;
      Result := (S <> ps);
    except
      on e: EOverflow do
        Result := False;
    end;
  end;

begin
  ps := S;
  if (S^ = '$') or (S^ = '&') then
  begin
    Inc(S);
    Result := ParseHexInt;
    Exit;
  end
  else if (S[0] = '0') and ((S[1] = 'x') or (S[1] = 'X')) then
  begin
    Inc(S, 2);
    Result := ParseHexInt;
    Exit;
  end
  else
    Result := ParseDec;
  if not Result then
    S := ps;
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

    if lvValue = '' then
    begin
      if P^ <> #0 then
      begin  // 最后一个字符
        // 添加到列表中
        pvStrings.Add(P);
        inc(Result);
      end;
      Exit;
    end else
    begin
      // 跳过分隔符
      SkipChars(p, pvSpliterChars);

      // 添加到列表中
      pvStrings.Add(lvValue);
      inc(Result);
    end;
  end;
end;


function URLDecode(const ASrc: URLString; pvIsPostData: Boolean = true): URLString;
var
  i, j: integer;
  s:String;
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
        s := Format('$%s%s', ['$', ASrc[i], ASrc[i+1]]);
        Result[j] := URLChar(StrToInt(s));
      except end;
      Inc(i, 1);  // 再跳过一个字符.

    end;
    Inc(i);
    Inc(j);
  end;
  SetLength(Result, j - 1);
  {$ENDIF}

end;




function URLEncode(S: URLString; pvIsPostData: Boolean = true): URLString;
var
  i: Integer; // loops thru characters in string
  strTemp:String;
  {$IFDEF UNICODE_URL}
  lvRawBytes:TBytes;
  {$ELSE}
  lvRawStr:URLString;
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
  lvRawStr := s;
  for i := 1 to Length(lvRawStr) do
  begin
    case lvRawStr[i] of
      'A' .. 'Z', 'a' .. 'z', '0' .. '9', '-', '_', '.':
        Result := Result + lvRawStr[i];
      ' ':
        if pvIsPostData then
        begin     // Post数据如果是空格需要编码成 +
          Result := Result + '+';
        end else
        begin
          Result := Result + '%20';
        end
    else
      begin
        strTemp := SysUtils.IntToHex(Ord(lvRawStr[i]), 2);
        Result := Result + '%' + URLString(strTemp);
      end;
    end;
  end;
  {$ENDIF}
end;

function StringsValueOfName(pvStrings: TStrings; const pvName: string;
    pvSpliters: TSysCharSet; pvTrim: Boolean): String;
var
  i : Integer;
  s : string;
  lvName: String;
  p : PChar;
  lvSpliters:TSysCharSet;
begin
  lvSpliters := pvSpliters;
  Result := '';

  // context-length : 256
  for i := 0 to pvStrings.Count -1 do
  begin
    s := pvStrings[i];
    p := PChar(s);

    // 获取名称
    lvName := LeftUntil(p, lvSpliters);

    if pvTrim then lvName := Trim(lvName);

    if CompareText(lvName, pvName) = 0 then
    begin
      // 跳过分隔符
      SkipChars(p, lvSpliters);

      // 获取值
      Result := P;

      // 截取值
      if pvTrim then Result := Trim(Result);

      Exit;
    end;
  end;

end;

function StrStrIgnoreCase(P, PSub: PChar): PChar;
var
  I: Integer;
  lvSubUP: String;
begin
  Result := nil;
  if (P = nil) or (PSub = nil) then
    Exit;
  lvSubUP := UpperCase(PSub);
  PSub := PChar(lvSubUP);
  while P^ <> #0 do
  begin
    if UpperChar(P^) = PSub^ then
    begin
      I := 1;
      while PSub[I] <> #0 do
      begin
        if UpperChar(P[I]) = PSub[I] then
          Inc(I)
        else
          Break;
      end;
      if PSub[I] = #0 then
      begin
        Result := P;
        Break;
      end;
    end;
    Inc(P);
  end;
end;

function StrStr(P: PChar; PSub: PChar): PChar;
var
  I: Integer;
begin
{$IFDEF MSWINDOWS}
{$IFDEF UNICODE}
  if Assigned(VCStrStrW) then
  begin
    Result := VCStrStrW(P, PSub);
    Exit;
  end;
{$ELSE}
  if Assigned(VCStrStr) then
  begin
    Result := VCStrStr(P, PSub);
    Exit;
  end;
{$ENDIF}
{$ENDIF}

  if (PSub = nil) or (PSub^ = #0) then
    Result := P
  else
  begin
    Result := nil;
    while P^ <> #0 do
    begin
      if P^ = PSub^ then
      begin
        I := 1;     // 从后面第二个字符开始对比
        while PSub[I] <> #0 do
        begin
          if P[I] = PSub[I] then
            Inc(I)
          else
            Break;
        end;

        if PSub[I] = #0 then
        begin  // P1和P2已经匹配到了末尾(匹配成功)
          Result := P;
          Break;
        end;
      end;
      Inc(P);
    end;
  end;
end;

function LeftUntilStr(var P: PChar; pvSpliter: PChar; pvIgnoreCase: Boolean =
    true): string;
var
  lvPUntil:PChar;
  l : Integer;
begin
  if pvIgnoreCase then
  begin
    lvPUntil := StrStrIgnoreCase(P, pvSpliter);
  end else
  begin
    lvPUntil := StrStr(P, pvSpliter);
  end;
  if lvPUntil = nil then
  begin
    Result := '';
    //P := nil;
    // 匹配失败不移动P
  end else
  begin
    l := lvPUntil-P;
    if l = 0 then
    begin
      Result := '';
    end else
    begin
      SetLength(Result, l);
      if SizeOf(Char) = 1 then
      begin
        Move(P^, PChar(Result)^, l);
      end else
      begin
        l := l shl 1;
        Move(P^, PChar(Result)^, l);
      end;
      P := lvPUntil;
    end;
  end;
  

end;

function SearchPointer(pvSource: Pointer; pvSourceLen, pvStartIndex: Integer;
    pvSub: Pointer; pvSubLen: Integer): Pointer;
var
  I, j: Integer;
  lvTempP, lvTempPSub, lvTempP2, lvTempPSub2:PByte;
begin
  if (pvSub = nil) then
    Result := nil
  else
  begin
    Result := nil;
    j := pvStartIndex;
    lvTempP := PByte(pvSource);
    Inc(lvTempP, pvStartIndex);

    lvTempPSub := PByte(pvSub);
    while j<pvSourceLen do
    begin
      if lvTempP^ = lvTempPSub^ then
      begin


        // 临时指针，避免移动顺序比较指针
        lvTempP2 := lvTempP;
        Inc(lvTempP2);    // 移动到第二位(前一个已经进行了比较
        I := 1;           // 初始化计数器(从后面第二个字符开始对比)

        // 临时比较字符指针
        lvTempPSub2 := lvTempPSub;
        Inc(lvTempPSub2);  // 移动到第二位(前一个已经进行了比较

        while (I < pvSubLen) do
        begin
          if lvTempP2^ = lvTempPSub2^ then
          begin
            Inc(I);
            inc(lvTempP2);   // 移动到下一位进行比较
            inc(lvTempPSub2);
          end else
            Break;
        end;

        if I = pvSubLen then
        begin  // P1和P2已经匹配到了末尾(匹配成功)
          Result := lvTempP;
          Break;
        end;
      end;
      Inc(lvTempP);
      inc(j);
    end;
  end;
end;


function SkipStr(var P:PChar; pvSkipStr: PChar; pvIgnoreCase: Boolean = true):
    Integer;
var
  lvSkipLen : Integer;
begin
  Result := 0;

  lvSkipLen := Length(pvSkipStr) * SizeOf(Char);

  while True do
  begin
    if StartWith(P, pvSkipStr) then
    begin
      Inc(Result);
      P := PChar(IntPtr(P) + lvSkipLen);
    end else
    begin
      Break;
    end;    
  end; 
end;

function StartWith(P:PChar; pvStart:PChar; pvIgnoreCase: Boolean = true):
    Boolean;
var
  lvSubUP: String;
  PSubUP : PChar;
begin
  Result := False;

  if pvIgnoreCase then
  begin
    PSubUP := pvStart;
    if (P = nil) or (PSubUP = nil) then  Exit;
    
    if P^ = #0 then Exit;
    while PSubUP^ <> #0 do
    begin
      if UpperChar(P^) =UpperChar(PSubUP^) then
      begin
        Inc(P);
        Inc(PSubUP);
      end else
        Break;
    end;
    if PSubUP^ = #0 then  // 比较到最后
    begin
      Result := true;
    end;

  end else
  begin
    Result := CompareMem(P, pvStart, Length(pvStart));
  end;
end;

function SplitStr(s:string; pvSpliterStr:string; var s1, s2:String): Boolean;
var
  pSource, pSpliter:PChar;
  lvTemp:string;
begin
  pSource := PChar(s);

  pSpliter := PChar(pvSpliterStr);

  // 跳过开头的分隔符
  SkipStr(pSource, pSpliter);

  lvTemp := LeftUntilStr(pSource, pSpliter);
  if lvTemp <> '' then
  begin
    Result := true;
    s1 := lvTemp;
    // 跳过开头的分隔符
    SkipStr(pSource, pSpliter);
    s2 := pSource;
  end else
  begin
    Result := False;
  end;  

end;

function StringToUtf8Bytes(pvData:String; pvBytes:TBytes): Integer;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.UTF8.GetBytes(pvData, 1, Length(pvData), pvBytes, 0);
{$ELSE}
  lvRawStr := UTF8Encode(pvData);
  Result := Length(lvRawStr);
  Move(PAnsiChar(lvRawStr)^, pvBytes[0], Result);
{$ENDIF}
end;

function StringToUtf8Bytes(const pvData: string; pvProcessEndByte: Boolean =
    false): TBytes;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.UTF8.GetBytes(pvData);
  if pvProcessEndByte then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) -1 ] := 0;
  end;
{$ELSE}
  lvRawStr := UTF8Encode(pvData);
  if pvProcessEndByte then
  begin
    SetLength(Result, Length(lvRawStr) + 1);
    Move(PAnsiChar(lvRawStr)^, Result[0], Length(lvRawStr));
    Result[Length(Result) -1 ] := 0;
  end else
  begin
    SetLength(Result, Length(lvRawStr));
    Move(PAnsiChar(lvRawStr)^, Result[0], Length(lvRawStr));
  end;
{$ENDIF}
end;

function Utf8BytesToString(pvBytes: TBytes; pvOffset: Integer): String;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
  l:Integer;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.UTF8.GetString(pvBytes, pvOffset, Length(pvBytes) - pvOffset);
{$ELSE}
  l := Length(pvBytes) - pvOffset;
  SetLength(lvRawStr, l);
  Move(pvBytes[pvOffset], PansiChar(lvRawStr)^, l);
  Result := UTF8Decode(lvRawStr);
{$ENDIF}
end;



function StringToBytes(pvData:String; pvBytes:TBytes): Integer;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.Default.GetBytes(pvData, 1, Length(pvData), pvBytes, 0);
{$ELSE}
  lvRawStr := pvData;
  Move(PAnsiChar(lvRawStr)^, pvBytes[0], Length(lvRawStr));
  Result := Length(lvRawStr);
{$ENDIF}
end;



function BytesToString(pvBytes: TBytes; pvOffset: Integer): String;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.Default.GetString(pvBytes, pvOffset, Length(pvBytes) - pvOffset);
{$ELSE}
  lvRawStr := StrPas(@pvBytes[pvOffset]);
  Result := lvRawStr;
{$ENDIF}
end;

function Utf8BufferToString(pvBuff: PByte; pvLen: Integer): string;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
  l:Cardinal;
{$ELSE}
var
  lvBytes:TBytes;
{$ENDIF}
begin
{$IFDEF UNICODE}
  SetLength(lvBytes, pvLen); 
  Move(pvBuff^, lvBytes[0], pvLen);
  Result := TEncoding.UTF8.GetString(lvBytes);
  //Result := TEncoding.UTF8.GetString(pvBytes, pvOffset, Length(pvBytes) - pvOffset);
{$ELSE}
  l := pvLen;
  SetLength(lvRawStr, l);
  Move(pvBuff^, PansiChar(lvRawStr)^, l);
  Result := UTF8Decode(lvRawStr);
{$ENDIF}
end;

function SpanPointer(const pvStart, pvEnd: PByte): Integer;
begin
  Result := Integer(pvEnd) - Integer(pvStart);
end;

function LeftUntil(var p: PChar; pvChars: TSysCharSet; var vLeftStr: string):
    Integer;
var
  lvPTemp: PChar;
  l:Integer;
  lvMatched: Byte;
begin
  lvMatched := 0;
  lvPTemp := p;
  while lvPTemp^ <> #0 do
  begin
    if CharInSet(lvPTemp^, pvChars) then
    begin            // 匹配到
      lvMatched := 1;
      Break;
    end else
      Inc(lvPTemp);
  end;
  if lvMatched = 0 then
  begin   // 没有匹配到
    Result := -1;
  end else
  begin   // 匹配到
    l := lvPTemp-P;
    SetLength(vLeftStr, l);
    if SizeOf(Char) = 1 then
    begin
      Move(P^, PChar(vLeftStr)^, l);
    end else
    begin
      l := l shl 1;
      Move(P^, PChar(vLeftStr)^, l);
    end;
    P := lvPTemp;  // 跳转到新位置
    Result := 0;
  end;
end;

function PickString(p: PChar; pvOffset, pvCount: Integer): String;
begin
  SetLength(Result, pvCount);
  Inc(p, pvOffset);
{$IFDEF UNICODE}
  Move(PChar(Result)^, P^, pvCount shl 1);
{$ELSE}
  Move(PChar(Result)^, P^, pvCount);
{$ENDIF}
end;

constructor TDStringBuilder.Create;
begin
  inherited Create;
  FLineBreak := Char(13) + Char(10);
end;

function TDStringBuilder.Append(c:Char): TDStringBuilder;
begin
  CheckNeedSize(1);
  FData[FPosition] := c;
  Inc(FPosition);
  Result := Self;
end;

function TDStringBuilder.Append(str:string): TDStringBuilder;
var
  l:Integer;
begin
  Result := Self;
  l := System.Length(str);
  if l = 0 then Exit;
  CheckNeedSize(l);
{$IFDEF UNICODE}
  Move(PChar(str)^, FData[FPosition], l shl 1);
{$ELSE}
  Move(PChar(str)^, FData[FPosition], l);
{$ENDIF}

  Inc(FPosition, l);

end;

function TDStringBuilder.Append(v: Boolean; UseBoolStrs: Boolean = True):
    TDStringBuilder;
begin
  Result := Append(BoolToStr(v, UseBoolStrs));
end;

function TDStringBuilder.Append(v:Integer): TDStringBuilder;
begin
  Result :=Append(IntToStr(v));
end;

function TDStringBuilder.Append(v:Double): TDStringBuilder;
begin
  Result := Append(FloatToStr(v));
end;

function TDStringBuilder.Append(str:string; pvLeftStr:string;
    pvRightStr:String): TDStringBuilder;
begin
  Result := Append(pvLeftStr).Append(str).Append(pvRightStr);
end;

function TDStringBuilder.AppendLine(str:string): TDStringBuilder;
begin
  Result := Append(Str).Append(FLineBreak);
end;

function TDStringBuilder.AppendQuoteStr(str:string): TDStringBuilder;
begin
  Result := Append('"').Append(str).Append('"');
end;

function TDStringBuilder.AppendSingleQuoteStr(str:string): TDStringBuilder;
begin
  Result := Append('''').Append(str).Append('''');
end;

procedure TDStringBuilder.CheckNeedSize(pvSize: LongInt);
var
  lvCapacity:LongInt;
begin
  if FPosition + pvSize > FCapacity then
  begin
    lvCapacity := (FPosition + pvSize + (BUFFER_BLOCK_SIZE - 1)) AND (not (BUFFER_BLOCK_SIZE - 1));
    FCapacity := lvCapacity;
    SetLength(FData, FCapacity);     
  end;
end;

procedure TDStringBuilder.Clear;
begin
  FPosition := 0;

  // modify by ymf
  // 2017-01-10 17:36:13
  FCapacity := 0;
  SetLength(FData, 0);
end;

function TDStringBuilder.GetLength: Integer;
begin
  Result := FPosition;
end;

function TDStringBuilder.ToString: string;
var
  l:Integer;
begin
  l := Length;
  SetLength(Result, l);
{$IFDEF UNICODE}
  Move(FData[0], PChar(Result)^, l shl 1);
{$ELSE}
  Move(FData[0], PChar(Result)^, l);
{$ENDIF}
end;

constructor TDBufferBuilder.Create;
begin
  inherited Create;
  FLineBreak := #13#10;
end;

function TDBufferBuilder.Append(const c: Char): TDBufferBuilder;
begin
{$IFDEF UNICODE}
  Result := AppendBuffer(@c, SizeOf(c));
//  CheckNeedSize(2);
//  Move(c, FData[FSize], 2);
//  Inc(FSize, 2);
//  Result := Self;
{$ELSE}
  Result := AppendBuffer(@c, SizeOf(c));
//  CheckNeedSize(1);
//  FData[FSize] := c;
//  Inc(FSize);
//  Result := Self;
{$ENDIF}

end;

function TDBufferBuilder.Append(str:string): TDBufferBuilder;
var
  l:Integer;
begin
  Result := Self;
  l := System.Length(str);
  if l = 0 then Exit;
{$IFDEF UNICODE}
  l := l shl 1;
{$ENDIF}
  Result := AppendBuffer(PByte(Str), l);
end;

function TDBufferBuilder.Append(v: Boolean; UseBoolStrs: Boolean = True):
    TDBufferBuilder;
begin
  Result := Append(BoolToStr(v, UseBoolStrs));
end;

function TDBufferBuilder.Append(v:Integer): TDBufferBuilder;
begin
  Result :=Append(IntToStr(v));
end;

function TDBufferBuilder.Append(v:Double): TDBufferBuilder;
begin
  Result := Append(FloatToStr(v));
end;

function TDBufferBuilder.Append(str:string; pvLeftStr:string;
    pvRightStr:String): TDBufferBuilder;
begin
  Result := Append(pvLeftStr).Append(str).Append(pvRightStr);
end;

function TDBufferBuilder.Append(const aByte:Byte): TDBufferBuilder;
begin
  Result := AppendBuffer(@aByte, 1);
end;

function TDBufferBuilder.Append(str: string; pvConvertToUtf8Bytes: Boolean):
    TDBufferBuilder;
var
  lvBytes:TBytes;
begin
  if pvConvertToUtf8Bytes then
  begin
    Result := Self;

    lvBytes := StringToUtf8Bytes(str);
    AppendBuffer(PByte(@lvBytes[0]), System.Length(lvBytes));
  end else
  begin
    Result := Append(str);
  end;

end;

function TDBufferBuilder.Append(const w: Word): TDBufferBuilder;
begin
  Result := AppendBuffer(@w, 2);
end;

function TDBufferBuilder.AppendBreakLineBytes: TDBufferBuilder;
begin
  if FBufferLocked then
  begin
    raise Exception.Create('Buffer Locked');
  end;
  CheckNeedSize(2);
  FData[FSize] := 13;
  FData[FSize +1 ] := 10;
  Inc(FSize, 2);
  FPosition := FSize;
  Result := Self;
end;

function TDBufferBuilder.AppendBuffer(pvBuffer:PByte; pvLength:Integer):
    TDBufferBuilder;
begin
  if FBufferLocked then
  begin
    raise Exception.Create('Buffer Locked');
  end;
  CheckNeedSize(pvLength);

  // 在最后添加
  Move(pvBuffer^, FData[FSize], pvLength);
  Inc(FSize, pvLength);
  // 移动Position
  FPosition := FSize;
  Result := Self;
end;

function TDBufferBuilder.AppendLine(str:string): TDBufferBuilder;
begin
  Result := Append(Str).Append(FLineBreak);
end;

function TDBufferBuilder.AppendQuoteStr(str:string): TDBufferBuilder;
begin
  Result := Append('"').Append(str).Append('"');
end;





function TDBufferBuilder.AppendRawStr(pvRawStr:RAWString): TDBufferBuilder;
begin
{$IFDEF MSWINDOWS}
  Result := AppendBuffer(PByte(pvRawStr), System.Length(pvRawStr));
{$ELSE}
  Result := AppendUtf8(pvRawStr);
{$ENDIF}
end;



function TDBufferBuilder.AppendSingleQuoteStr(str:string): TDBufferBuilder;
begin
  Result := Append('''').Append(str).Append('''');
end;

function TDBufferBuilder.AppendUtf8(str:String): TDBufferBuilder;
var
  lvBytes:TBytes;
begin 
  Result := Self;
  lvBytes := StringToUtf8Bytes(str);
  AppendBuffer(PByte(@lvBytes[0]), System.Length(lvBytes));
end;

procedure TDBufferBuilder.CheckNeedSize(pvSize: LongInt);
var
  lvCapacity:LongInt;
begin
  if FSize + pvSize > FCapacity then
  begin
    lvCapacity := (FSize + pvSize + (BUFFER_BLOCK_SIZE - 1)) AND (not (BUFFER_BLOCK_SIZE - 1));
    FCapacity := lvCapacity;
    SetLength(FData, FCapacity);
  end;
end;

procedure TDBufferBuilder.CheckNeedSize(pvOffset, pvSize: LongInt);
var
  lvCapacity:LongInt;
begin
  if pvOffset + pvSize > FCapacity then
  begin
    lvCapacity := (pvOffset + pvSize + (BUFFER_BLOCK_SIZE - 1)) AND (not (BUFFER_BLOCK_SIZE - 1));
    FCapacity := lvCapacity;
    SetLength(FData, FCapacity);
  end;
end;

procedure TDBufferBuilder.Clear;
begin
  FSize := 0;
  FPosition := 0;

  // modify by ymf
  // 2017-01-10 17:36:13
  FCapacity := 0;
  SetLength(FData, 0);
  
//  {$IFDEF MSWINDOWS}
//  {$IFDEF DEBUG}
//  ZeroMemory(@FData[0], FCapacity);
//  {$ENDIF}
//  {$ENDIF}
end;

function TDBufferBuilder.DecodeUTF8: string;
begin
{$IFDEF MSWINDOWS}
  Result := Utf8BufferToString(@FData[0], FSize);
{$ELSE}

  CheckNeedSize(2);
  FData[FSize] := 0;
  FData[FSize + 1] := 0;
  Result := TEncoding.UTF8.GetString(FData, 0, self.Length);
{$ENDIF}
end;

function TDBufferBuilder.ReArrange: TDBufferBuilder;
var
  lvOffset:LongInt;
begin
  lvOffset := FPosition;
  Move(FData[FPosition], FData[0], Remain);
  Result := Self;
  Dec(FSize, lvOffset);
  FPosition := 0;
end;

function TDBufferBuilder.GetLength: Integer;
begin
  Result := FSize;
end;

function TDBufferBuilder.GetLockBuffer(pvLength:Integer): PByte;
begin
  CheckNeedSize(pvLength);
  Result := @FData[FSize];
  FBufferLocked := True;
end;

function TDBufferBuilder.GetRemain: Integer;
begin
  Result := FSize - FPosition;
end;

procedure TDBufferBuilder.LoadFromFile(pvFileName:string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(pvFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;  
end;

procedure TDBufferBuilder.LoadFromStream(pvStream: TStream);
var
  Count: Longint;
begin
  pvStream.Position := 0;
  Count := pvStream.Size;
  SetSize(Count);
  if Count <> 0 then pvStream.ReadBuffer(FData[0], Count); 
end;

function TDBufferBuilder.Memory: PByte;
begin
  Result := @FData[0];
end;

function TDBufferBuilder.MemoryBuffer(const pvIndex: Integer): PByte;
begin
  Result := @FData[pvIndex];
end;

function TDBufferBuilder.PeekBuffer(pvBuffer:PByte; pvLength:Integer): Cardinal;
var
  l:Integer;
begin
  Result := 0;
  l := FSize - FPosition;
  if l = 0 then Exit;

  if l > pvLength then l := pvLength;
  Move(FData[FPosition], pvBuffer^, l);
  Result := l;
end;

function TDBufferBuilder.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FSize - FPosition;
  if Result = 0 then Exit;

  if Result > Count then Result := Count;
  Move(FData[FPosition], Buffer, Result);
  Inc(FPosition, Result);
//  if (FPosition >= 0) and (Count >= 0) then
//  begin
//    Result := FSize - FPosition;
//    if Result > 0 then
//    begin
//      if Result > Count then Result := Count;
//      Move(Pointer(Longint(@FData[0]) + FPosition)^, Buffer, Result);
//      Inc(FPosition, Result);
//      Exit;
//    end;
//  end;
//  Result := 0;
end;

function TDBufferBuilder.ReadBuffer(pvBuffer:PByte; pvLength:Integer): Cardinal;
var
  l:Integer;
begin
  Result := 0;
  l := FSize - FPosition;
  if l = 0 then Exit;

  if l > pvLength then l := pvLength;
  Move(FData[FPosition], pvBuffer^, l);
  Inc(FPosition, l);
  Result := l;
end;

function TDBufferBuilder.ReadByte(var vByte: Byte): Boolean;
begin
  Result := False;
  if Remain = 0 then Exit;

  vByte :=  FData[FPosition];
  Inc(FPosition);
  Result := True;
end;

function TDBufferBuilder.ReleaseLockBuffer(pvLength:Integer): TDBufferBuilder;
begin
  Inc(FSize, pvLength);
  Result := Self;
  FBufferLocked := False;
end;

procedure TDBufferBuilder.SaveToFile(pvFile:String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(pvFile, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDBufferBuilder.SaveToStream(pvStream:TStream);
begin
  if FSize <> 0 then pvStream.WriteBuffer(Memory^, FSize);
end;

function TDBufferBuilder.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  if FPosition > FSize then
  begin
    FPosition := FSize;
  end;
  Result := FPosition;
end;

procedure TDBufferBuilder.SetSize(NewSize: Longint);
var
  OldPosition: Longint;
begin
  OldPosition := FPosition;
  CheckNeedSize(0, NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then Seek(0, soFromEnd);
end;

function TDBufferBuilder.GetInstanceSize: Integer;
begin
  Result := FCapacity;
end;

function TDBufferBuilder.ToBytes: TBytes;
begin
  SetLength(Result, self.Length);
  Move(FData[0], Result[0], self.Length);
end;

function TDBufferBuilder.ToRAWString: RAWString;
begin
{$IFDEF MSWINDOWS}
  CheckNeedSize(1);
  FData[FSize] := 0;
  {$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
  Result := AnsiStrings.StrPas(PAnsiChar(@FData[0]));
  {$ELSE}
  Result := StrPas(PAnsiChar(@FData[0]));
  {$IFEND >=XE5}
{$ELSE}
  CheckNeedSize(2);
  FData[FSize] := 0;
  FData[FSize + 1] := 0; 
  Result := TEncoding.UTF8.GetString(FData, 0, self.Length);
{$ENDIF}
end;

function TDBufferBuilder.Write(const Buffer; Count: Longint): Integer;
begin
  if FBufferLocked then
  begin
    raise Exception.Create('Buffer Locked');
  end;
  CheckNeedSize(FPosition, Count);
  Move(Buffer, FData[FPosition], Count);
  Inc(FPosition, Count);
  if FPosition >= FSize then FSize := FPosition;
  Result := Count;
end;


function LoadStringFromUtf8NoBOMFile(pvFile:string): String;
var
  lvStream: TMemoryStream;
{$IFDEF UNICODE}
  lvBytes:TBytes;
{$ELSE}
  lvStr: AnsiString;
{$ENDIF}
begin
  if FileExists(pvFile) then
  begin
    lvStream := TMemoryStream.Create;
    try
      lvStream.LoadFromFile(pvFile);
      lvStream.Position := 0;
      {$IFDEF UNICODE}
      SetLength(lvBytes, lvStream.Size);
      lvStream.ReadBuffer(lvBytes[0], lvStream.Size);
      Result := TEncoding.UTF8.GetString(lvBytes);
      {$ELSE}
      SetLength(lvStr, lvStream.Size);
      lvStream.ReadBuffer(PAnsiChar(lvStr)^, lvStream.Size);
      Result := UTF8Decode(lvStr);
      {$ENDIF}
    finally
      lvStream.Free;
    end;
  end else
  begin
    Result := '';
  end;
end;

procedure WriteStringToUtf8NoBOMFile(pvFile, pvData: String);
var
  lvStream: TMemoryStream;
{$IFDEF UNICODE}
  lvBytes:TBytes;
{$ELSE}
  lvStr: AnsiString;
{$ENDIF}
begin
  lvStream := TMemoryStream.Create;
  try
    {$IFDEF UNICODE}
    lvBytes := TEncoding.UTF8.GetBytes(pvData);
    lvStream.WriteBuffer(lvBytes[0], Length(lvBytes));
    {$ELSE}
    lvStr := UTF8Encode(pvData);
    lvStream.WriteBuffer(PAnsiChar(lvStr)^, Length(lvStr));
    {$ENDIF}
    lvStream.SaveToFile(pvFile);
  finally
    lvStream.Free;
  end;  

end;

function ByteBufferToString(pvBuff:PByte; pvLen:Cardinal): string;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
  l:Cardinal;
{$ELSE}
var
  lvBytes:TBytes;
{$ENDIF}
begin
{$IFDEF UNICODE}
  SetLength(lvBytes, pvLen);
  Move(pvBuff^, lvBytes[0], pvLen);
  Result := TEncoding.Default.GetString(lvBytes);
{$ELSE}
  l := pvLen;
  SetLength(lvRawStr, l);
  Move(pvBuff^, PansiChar(lvRawStr)^, l);
  Result := lvRawStr;
{$ENDIF}
end;

function StringToBytes(pvData:string): TBytes;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.Default.GetBytes(pvData);
{$ELSE}
  lvRawStr := pvData;
  SetLength(Result, Length(lvRawStr) + 1);
  Move(PAnsiChar(lvRawStr)^, Result[0], Length(lvRawStr));
  Result[Length(Result) -1] := 0;
{$ENDIF}
end;

function GetTickCount: Cardinal;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.GetTickCount;
  {$ELSE}
  Result := TThread.GetTickCount;
  {$ENDIF}
end;

function GetCurrentThreadID: Cardinal;
begin
  {$IFDEF MSWINDOWS}
    Result := windows.GetCurrentThreadId;
  {$ELSE}
    Result := TThread.CurrentThread.ThreadID;
  {$ENDIF};
end;

function ObjectHexAddr(pvObj:TObject): String;
begin
  Result := IntToHex(IntPtr(pvObj), 2);
  //Result := Format('%.2x',[intPtr(pvObj)]));
end;

function ObjectIntStrAddr(pvObj:TObject): String;
begin
  Result := IntToStr(IntPtr(pvObj));
end;

function DateTimeString(pvDateTime:TDateTime): string;
begin
  Result := FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', pvDateTime);
end;

function NowString: String;
begin
  Result := DateTimeString(Now());
end;

function AnsiStringLength(pvBuff: Pointer; pvMaxLength: Integer = 0): Integer;
var
  lvBuf:PByte;
begin
  Result := 0;
  lvBuf := pvBuff;
  if pvMaxLength > 0 then
  begin
    while lvBuf^ <> 0 do
    begin
      Inc(Result);
      Inc(lvBuf);
      if Result > pvMaxLength then
      begin
        Result := -1;
        Exit;
      end;
    end;
  end else
  begin
    while lvBuf^ <> 0 do
    begin
      Inc(Result);
      Inc(lvBuf);
    end;
  end;
end;

function GetStrValueOfName(const pvStr, pvName: string; pvSplitChars,
    pvEndChars: TSysCharSet): string;
var
  lvPtr, lvSearchPtr:PChar;
  r :Integer;
begin
  lvPtr := PChar(pvStr);

  while True do
  begin
    lvSearchPtr := StrStrIgnoreCase(lvPtr, PChar(pvName));
    if lvSearchPtr = nil then
    begin
      Result := '';
      Exit;
    end;
    Inc(lvSearchPtr, Length(pvName));

    lvPtr := lvSearchPtr;
    r := SkipChars(lvPtr, pvSplitChars);
    if r = 0 then
    begin
      Continue;
    end else
    begin
      Break;
    end;
  end;

  if LeftUntil(lvPtr, pvEndChars, Result) = -1 then
  begin
    Result := lvPtr;
  end;
end;

function SkipUntilEx(var p:PChar; pvChars: TSysCharSet): Integer;
var
  ps, pe: PChar;
begin
  Result := -1;
  ps := p;
  pe := ps;
  while pe^ <> #0 do
  begin
    if CharInSet(pe^, pvChars) then
    begin
      p := pe;
      Result := 0;
      Break;
    end else
      Inc(pe);
  end;
  if Result = 0 then
    Result := pe - ps; 
end;

function NewPString(const s: string): PString;
var
  lvRVal:PString;
begin
  New(lvRVal);
  lvRVal^ := s;
  Result := lvRVal;
end;

function GetStringFromPString(const p:Pointer): string;
begin
  if p = nil then
  begin
    Result := STRING_EMPTY;
  end else
  begin
    Result := PString(p)^;
  end;
end;

function PosWStr(sub: DStringW; const s: DStringW): Integer;
begin
  Result := Pos(sub, s);
end;

function NewMapKeyString(const key:Integer; const s:string): PMAPKeyString;
begin
  New(Result);
  Result^.key := key;
  Result^.value := s;
end;

function HexToInt(const p:PChar; pvLength:Integer): Integer;
var
  ps: PChar;
  i: Integer;
begin
  Result := 0;
  ps := p; 
  
  for i := 0 to pvLength - 1 do
  begin
    Result := (Result shl 4) + HexValue(ps^);
    Inc(ps);
  end;
end;

function WideBufferToStringW(pvBuffer:Pointer; pvBufLength:Integer): DStringW;
begin
  SetLength(Result, pvBufLength shr 1);
  Move(pvBuffer^, PDCharW(Result)^, pvBufLength);
end;



initialization

{$IFDEF MSWINDOWS}

{$IFDEF UNICODE}
VCStrStrW := nil;
{$ELSE}
VCStrStr := nil;
{$ENDIF}
//VCMemCmp := nil;
hMsvcrtl := LoadLibrary('msvcrt.dll');
if hMsvcrtl <> 0 then
begin
  {$IFDEF UNICODE}
  VCStrStrW := TMSVCStrStrW(GetProcAddress(hMsvcrtl, 'wcsstr'));
  {$ELSE}
  VCStrStr := TMSVCStrStr(GetProcAddress(hMsvcrtl, 'strstr'));
  {$ENDIF}
  //VCMemCmp := TMSVCMemCmp(GetProcAddress(hMsvcrtl, 'memcmp'));
end;
{$ENDIF}

finalization

{$IFDEF MSWINDOWS}
if hMsvcrtl <> 0 then
  FreeLibrary(hMsvcrtl);
{$ENDIF}

end.
