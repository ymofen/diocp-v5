(*
 *	 Unit owner: d10.�����
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-03-05 12:53:38
 *     �޸�URLEncode��URLDecode��Anddriod��UNICODE�µ��쳣
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 ����
 *
 *   2015-04-02 12:52:43
 *     �޸�SplitStrings,�ָ������һ���ַ���û�м����bug  abcd&33&eef
 *       ��л(Xjumping  990669769)����bug

 *  ����SearchPointer�е�һ����bug(ֻ�Ƚ���ǰ��λ�ַ���ƥ����)
      2015-09-11 09:08:22
 *)

{$if CompilerVersion>= 28}    // XE7:28
  {$DEFINE USE_NetEncoding}
{$ifend}
 
unit utils_strings;

interface

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

type
{$IFDEF MSWINDOWS}
  RAWString = AnsiString;
{$ELSE}
  RAWString = String;
{$ENDIF}


{$IFDEF MSWINDOWS}  // Windowsƽ̨�������ʹ��AnsiString
  URLString = AnsiString;
  URLChar = AnsiChar;
{$ELSE}
  // andriod����ʹ��
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

    function ToString: string;
    property Length: Integer read GetLength;

    /// <summary>
    ///   ���з�: Ĭ��#13#10
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
    ///   д������
    /// </summary>
    function AppendBuffer(pvBuffer:PByte; pvLength:Integer): TDBufferBuilder;

    /// <summary>
    ///   ��ȡ����
    /// </summary>
    function ReadBuffer(pvBuffer:PByte; pvLength:Integer): Cardinal;

    function PeekBuffer(pvBuffer:PByte; pvLength:Integer): Cardinal;

    /// <summary>
    ///   ��ȡһ���ֽ�
    /// </summary>
    function ReadByte(var vByte: Byte): Boolean;

    /// <summary>
    ///   ��ǰ��ȡ��������һ��Buffer
    /// </summary>
    function GetLockBuffer(pvLength:Integer): PByte;

    /// <summary>
    ///    �ͷ����һ��������Buffer, ����д��ָ�����ȵ�����
    /// </summary>
    function ReleaseLockBuffer(pvLength:Integer): TDBufferBuilder;

    /// <summary>
    ///   ��������(���ƶ�����ָ��)
    /// </summary>
    function ToBytes: TBytes;

    function ToRAWString: RAWString;

    /// <summary>
    ///   �����ڴ�ָ��
    /// </summary>
    function Memory: PByte;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Integer; override;
    procedure SetSize(NewSize: Longint); override;

    /// <summary>
    ///   �������п�������
    /// </summary>
    function ReArrange: TDBufferBuilder;


    /// <summary>
    ///   �������ݳ���
    /// </summary>
    property Length: Integer read GetLength;

    /// <summary>
    ///   ���з�: Ĭ��#13#10
    /// </summary>
    property LineBreak: String read FLineBreak write FLineBreak;

    /// <summary>
    ///   ʣ�����ݳ���
    /// </summary>
    property Remain: Integer read GetRemain;




  end;


/// <summary>
///   �����ַ�
/// </summary>
/// <returns>
///   �����������ַ�
/// </returns>
/// <param name="p"> ��ʼ���λ�� </param>
/// <param name="pvChars"> ������Щ�ַ���ֹͣ��Ȼ�󷵻� </param>
function SkipUntil(var p:PChar; pvChars: TSysCharSet): Integer;




/// <summary>
///   �����ַ�
/// </summary>
/// <returns>
///   �����������ַ�����
/// </returns>
/// <param name="p"> Դ(�ַ���)λ�� </param>
/// <param name="pvChars"> (TSysCharSet) </param>
function SkipChars(var p:PChar; pvChars: TSysCharSet): Integer;

/// <summary>
///   �����ַ���
///   // p = pchar("abcabcefggg");
///   // ִ�к� p = "efgg"
///   // ���ؽ�� = 2 //2��abc
///   SkipStr(p, "abc");
///
/// </summary>
/// <returns>
///   �����������ַ�������
/// </returns>
/// <param name="P"> Դ�ַ������������������ </param>
/// <param name="pvSkipStr"> ��ͷ��Ҫ�������ַ� </param>
/// <param name="pvIgnoreCase"> ���Դ�Сд </param>
function SkipStr(var P:PChar; pvSkipStr: PChar; pvIgnoreCase: Boolean = true):
    Integer;


/// <summary>
///   ����Ƿ���pvStart��ͷ
/// </summary>
/// <returns> ���Ϊ�淵��true
/// </returns>
/// <param name="P"> (PChar) </param>
/// <param name="pvStart"> (PChar) </param>
/// <param name="pvIgnoreCase"> (Boolean) </param>
function StartWith(P:PChar; pvStart:PChar; pvIgnoreCase: Boolean = true):
    Boolean;


/// <summary>
///   ����߿�ʼ��ȡ�ַ�
/// </summary>
/// <returns>
///   ���ؽ�ȡ�����ַ���
///   û��ƥ�䵽�᷵�ؿ��ַ���
/// </returns>
/// <param name="p"> Դ(�ַ���)��ʼ��λ��, ƥ��ɹ��������pvSpliter���״γ���λ��, ���򲻻�����ƶ�</param>
/// <param name="pvChars"> (TSysCharSet) </param>
function LeftUntil(var p:PChar; pvChars: TSysCharSet): string; overload;

/// <summary>
///   ����߿�ʼ��ȡ�ַ�
/// </summary>
/// <param name="vLeftStr">��ȡ�����ַ���</param>
/// <returns>
///    0: ��ȡ�ɹ�(pͣ����pvChars���״γ��ֵ�λ��)
///   -1: ƥ��ʧ��(p�����ƶ�)
/// </returns>
/// <param name="p"> Դ(�ַ���)��ʼ��λ��, ƥ��ɹ��������pvChars���״γ���λ��, ���򲻻�����ƶ�</param>
function LeftUntil(var p: PChar; pvChars: TSysCharSet; var vLeftStr: string):
    Integer; overload;


/// <summary>
///   ����߿�ʼ��ȡ�ַ���
/// </summary>
/// <returns>
///   ���ؽ�ȡ�����ַ���
/// </returns>
/// <param name="p"> Դ(�ַ���), ƥ��ɹ��������pvSpliter���״γ���λ��, ���򲻻�����ƶ� </param>
/// <param name="pvSpliter"> �ָ�(�ַ���) </param>
function LeftUntilStr(var P: PChar; pvSpliter: PChar; pvIgnoreCase: Boolean =
    true): string;

/// <summary>
///   ����SpliterChars���ṩ���ַ������зָ��ַ��������뵽Strings��
///     * �����ַ�ǰ��Ŀո�
/// </summary>
/// <returns>
///   ���طָ�ĸ���
/// </returns>
/// <param name="s"> Դ�ַ��� </param>
/// <param name="pvStrings"> ��������ַ����б� </param>
/// <param name="pvSpliterChars"> �ָ��� </param>
function SplitStrings(s:String; pvStrings:TStrings; pvSpliterChars
    :TSysCharSet): Integer;


/// <summary>
///  ��һ���ַ����ָ��2���ַ���
///  splitStr("key=abcd", "=", s1, s2)
///  // s1=key, s2=abcd
/// </summary>
/// <returns> �ɹ�����true
/// </returns>
/// <param name="s"> Ҫ�ָ���ַ��� </param>
/// <param name="pvSpliterStr"> (string) </param>
/// <param name="s1"> (String) </param>
/// <param name="s2"> (String) </param>
function SplitStr(s:string; pvSpliterStr:string; var s1, s2:String): Boolean;

/// <summary>
///   URL���ݽ���,
///    Get��Post�����ݶ�������url����
/// </summary>
/// <returns>
///   ���ؽ�����URL����
/// </returns>
/// <param name="ASrc"> ԭʼ���� </param>
/// <param name="pvIsPostData"> Post��ԭʼ������ԭʼ�Ŀո񾭹�UrlEncode����+�� </param>
function URLDecode(const ASrc: URLString; pvIsPostData: Boolean = true):URLString;

/// <summary>
///  �����ݽ���URL����
/// </summary>
/// <returns>
///   ����URL����õ�����
/// </returns>
/// <param name="S"> ��Ҫ��������� </param>
/// <param name="pvIsPostData"> Post��ԭʼ������ԭʼ�Ŀո񾭹�UrlEncode����+�� </param>
function URLEncode(S: URLString; pvIsPostData: Boolean = true): URLString;


/// <summary>
///  ��Strings�и�����������ֵ
/// </summary>
/// <returns> String
/// </returns>
/// <param name="pvStrings"> (TStrings) </param>
/// <param name="pvName"> (string) </param>
/// <param name="pvSpliters"> ���ֺ�ֵ�ķָ�� </param>
function StringsValueOfName(pvStrings: TStrings; const pvName: string;
    pvSpliters: TSysCharSet; pvTrim: Boolean): String;


/// <summary>
///   ����PSub��P�г��ֵĵ�һ��λ��
///   ��ȷ����
///   ���PSubΪ���ַ���(#0, nil)��ֱ�ӷ���P
/// </summary>
/// <returns>
///   ����ҵ�, ���ص�һ���ַ���λ��
///   �Ҳ�������False
///   * ����qdac.qstrings
/// </returns>
/// <param name="P"> Ҫ��ʼ����(�ַ���) </param>
/// <param name="PSub"> Ҫ��(�ַ���) </param>
function StrStr(P:PChar; PSub:PChar): PChar;

/// <summary>
///   ����PSub��P�г��ֵĵ�һ��λ��
///   ���Դ�Сд
///   ���PSubΪ���ַ���(#0, nil)��ֱ�ӷ���P
/// </summary>
/// <returns>
///   ����ҵ�, ���ص�һ���ַ���λ��
///   �Ҳ�������nil
///   * ����qdac.qstrings
/// </returns>
/// <param name="P"> Ҫ��ʼ����(�ַ���) </param>
/// <param name="PSub"> Ҫ��(�ַ���) </param>
function StrStrIgnoreCase(P, PSub: PChar): PChar;


/// <summary>
///  �ַ�ת��д
///  * ����qdac.qstrings
/// </summary>
function UpperChar(c: Char): Char;

/// <summary>
///  aStr�Ƿ���Strs�б���
/// </summary>
/// <returns>
///   ������б��з���true
/// </returns>
/// <param name="pvStr"> sensors,1,3.1415926,1.1,1.2,1.3 </param>
/// <param name="pvStringList"> (array of string) </param>
function StrIndexOf(const pvStr: string; const pvStringList: array of string):
    Integer;

/// <summary>
///   ����PSub��P�г��ֵĵ�һ��λ��
/// </summary>
/// <returns>
///   ����ҵ�, ����ָ���һ��pvSub��λ��
///   �Ҳ������� Nil
/// </returns>
/// <param name="pvSource"> ���� </param>
/// <param name="pvSourceLen"> ���ݳ��� </param>
/// <param name="pvSub"> ���ҵ����� </param>
/// <param name="pvSubLen"> ���ҵ����ݳ��� </param>
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
///  ת���ַ�����Bytes
/// </summary>
function StringToUtf8Bytes(pvData:String; pvBytes:TBytes): Integer;overload;

/// <summary>
///
/// </summary>
function Utf8BytesToString(pvBytes: TBytes; pvOffset: Cardinal): String;

function Utf8BufferToString(pvBuff:PByte; pvLen:Cardinal): string;

function StringToUtf8Bytes(pvData:string): TBytes; overload;

function StringToBytes(pvData:String; pvBytes:TBytes): Integer; overload;

function StringToBytes(pvData:string): TBytes; overload;

function BytesToString(pvBytes:TBytes; pvOffset: Cardinal): String;
function ByteBufferToString(pvBuff:PByte; pvLen:Cardinal): string;

function SpanPointer(const pvStart, pvEnd: PByte): Integer;

function IsHexChar(c: Char): Boolean;

function HexValue(c: Char): Integer;

function HexChar(V: Byte): Char;

function PickString(p: PChar; pvOffset, pvCount: Integer): String;

/// <summary>
///  ��Utf8��BOM��ʽ���ļ��м����ַ���
/// </summary>
function LoadStringFromUtf8NoBOMFile(pvFile:string): String;

procedure WriteStringToUtf8NoBOMFile(pvFile, pvData: String);

/// <summary>
///   ת���ַ���,
///   copy from qdac.qstrings.pas
/// </summary>
function ParseNumeric(var S: PChar; var ANum: Extended): Boolean;
function ParseHex(var p: PChar; var Value: Int64): Integer; 
function ParseInt(var S: PChar; var ANum: Int64): Integer;



implementation



{$IFDEF MSWINDOWS}
type
  TMSVCStrStr = function(s1, s2: PAnsiChar): PAnsiChar; cdecl;
  TMSVCStrStrW = function(s1, s2: PWChar): PWChar; cdecl;
  TMSVCMemCmp = function(s1, s2: Pointer; len: Integer): Integer; cdecl;

var
  hMsvcrtl: HMODULE;
  VCStrStr: TMSVCStrStr;
{$IFDEF UNICODE}
  VCStrStrW: TMSVCStrStrW;
{$ENDIF}
//  VCMemCmp: TMSVCMemCmp;
{$ENDIF}

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
    begin            // ƥ�䵽
      lvMatched := 1;
      Break;
    end else
      Inc(lvPTemp);
  end;
  if lvMatched = 0 then
  begin   // û��ƥ�䵽
    Result := '';
  end else
  begin   // ƥ�䵽
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
    P := lvPTemp;  // ��ת����λ��
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
  // ����16���ƿ�ʼ�ַ�
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
      if ANum < 0 then // �����
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
      if S^ = '.' then // С������
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
    // �����հ�
    SkipChars(p, [' ']);
    lvValue := LeftUntil(P, pvSpliterChars);

    if lvValue = '' then
    begin
      if P^ <> #0 then
      begin  // ���һ���ַ�
        // ���ӵ��б���
        pvStrings.Add(P);
        inc(Result);
      end;
      Exit;
    end else
    begin
      // �����ָ���
      SkipChars(p, pvSpliterChars);

      // ���ӵ��б���
      pvStrings.Add(lvValue);
      inc(Result);
    end;
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
  SetLength(lvRawBytes, Length(ASrc));   // Ԥ������һ���ַ���������־
  lvSrcBytes := TEncoding.ANSI.GetBytes(ASrc);
  j := 0;  // ��0��ʼ
  i := 0;
  while i <= Length(ASrc) do
  begin
    if (pvIsPostData) and (lvSrcBytes[i] = 43) then   //43(+) �ű�ɿո�, Post��ԭʼ����������� �ո�ʱ���� +��
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
      Inc(i, 1);  // ������һ���ַ�.

    end;
    Inc(i);
    Inc(j);
  end;
  SetLength(lvRawBytes, j);
  Result := TEncoding.ANSI.GetString(lvRawBytes);
  {$ELSE}
  SetLength(Result, Length(ASrc));   // Ԥ������һ���ַ���������־
  j := 1;  // ��1��ʼ
  i := 1;
  while i <= Length(ASrc) do
  begin
    if (pvIsPostData) and (ASrc[i] = '+') then   // + �ű�ɿո�, Post��ԭʼ����������� �ո�ʱ���� +��
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
      Inc(i, 1);  // ������һ���ַ�.

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
  {$IFDEF UNICODE_URL}
  lvRawBytes:TBytes;
  {$ELSE}
  lvRawStr:AnsiString;
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
        begin     // Post��������ǿո���Ҫ����� +
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
        begin     // Post��������ǿո���Ҫ����� +
          Result := Result + '+';
        end else
        begin
          Result := Result + '%20';
        end
    else
      Result := Result + '%' + SysUtils.IntToHex(Ord(lvRawStr[i]), 2);
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

    // ��ȡ����
    lvName := LeftUntil(p, lvSpliters);

    if pvTrim then lvName := Trim(lvName);

    if CompareText(lvName, pvName) = 0 then
    begin
      // �����ָ���
      SkipChars(p, lvSpliters);

      // ��ȡֵ
      Result := P;

      // ��ȡֵ
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
        I := 1;     // �Ӻ���ڶ����ַ���ʼ�Ա�
        while PSub[I] <> #0 do
        begin
          if P[I] = PSub[I] then
            Inc(I)
          else
            Break;
        end;

        if PSub[I] = #0 then
        begin  // P1��P2�Ѿ�ƥ�䵽��ĩβ(ƥ��ɹ�)
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
    // ƥ��ʧ�ܲ��ƶ�P
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


        // ��ʱָ�룬�����ƶ�˳��Ƚ�ָ��
        lvTempP2 := lvTempP;
        Inc(lvTempP2);    // �ƶ����ڶ�λ(ǰһ���Ѿ������˱Ƚ�
        I := 1;           // ��ʼ��������(�Ӻ���ڶ����ַ���ʼ�Ա�)

        // ��ʱ�Ƚ��ַ�ָ��
        lvTempPSub2 := lvTempPSub;
        Inc(lvTempPSub2);  // �ƶ����ڶ�λ(ǰһ���Ѿ������˱Ƚ�

        while (I < pvSubLen) do
        begin
          if lvTempP2^ = lvTempPSub2^ then
          begin
            Inc(I);
            inc(lvTempP2);   // �ƶ�����һλ���бȽ�
            inc(lvTempPSub2);
          end else
            Break;
        end;

        if I = pvSubLen then
        begin  // P1��P2�Ѿ�ƥ�䵽��ĩβ(ƥ��ɹ�)
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
    lvSubUP := UpperCase(pvStart^);
    PSubUP := PChar(lvSubUP);
    if (P = nil) or (PSubUP = nil) then  Exit;
    
    if P^ = #0 then Exit;
    while PSubUP^ <> #0 do
    begin
      if UpperChar(P^) = PSubUP^ then
      begin
        Inc(P);
        Inc(PSubUP);
      end else
        Break;
    end;
    if PSubUP^ = #0 then  // �Ƚϵ����
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

  // ������ͷ�ķָ���
  SkipStr(pSource, pSpliter);

  lvTemp := LeftUntilStr(pSource, pSpliter);
  if lvTemp <> '' then
  begin
    Result := true;
    s1 := lvTemp;
    // ������ͷ�ķָ���
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

function Utf8BytesToString(pvBytes: TBytes; pvOffset: Cardinal): String;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
  l:Cardinal;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.UTF8.GetString(pvBytes, pvOffset, Length(pvBytes) - pvOffset);
{$ELSE}
  l := Cardinal(Length(pvBytes)) - pvOffset;
  SetLength(lvRawStr, l);
  Move(pvBytes[pvOffset], PansiChar(lvRawStr)^, l);
  Result := UTF8Decode(lvRawStr);
{$ENDIF}
end;

function StringToUtf8Bytes(pvData:string): TBytes; overload;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.UTF8.GetBytes(pvData);
{$ELSE}
  lvRawStr := UTF8Encode(pvData);
  SetLength(Result, Length(lvRawStr));
  Move(PAnsiChar(lvRawStr)^, Result[0], Length(lvRawStr));
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

function BytesToString(pvBytes:TBytes; pvOffset: Cardinal): String;
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

function Utf8BufferToString(pvBuff:PByte; pvLen:Cardinal): string;
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
    begin            // ƥ�䵽
      lvMatched := 1;
      Break;
    end else
      Inc(lvPTemp);
  end;
  if lvMatched = 0 then
  begin   // û��ƥ�䵽
    Result := -1;
  end else
  begin   // ƥ�䵽
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
    P := lvPTemp;  // ��ת����λ��
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

  // ���������
  Move(pvBuffer^, FData[FSize], pvLength);
  Inc(FSize, pvLength);
  // �ƶ�Position
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
  {$IFDEF MSWINDOWS}
  {$IFDEF DEBUG}
  ZeroMemory(@FData[0], FCapacity);
  {$ENDIF}
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
  SetLength(Result, Length(lvRawStr));
  Move(PAnsiChar(lvRawStr)^, Result[0], Length(lvRawStr));
{$ENDIF}
end;


initialization

{$IFDEF MSWINDOWS}
VCStrStr := nil;
{$IFDEF UNICODE}
VCStrStrW := nil;
{$ENDIF}
//VCMemCmp := nil;
hMsvcrtl := LoadLibrary('msvcrt.dll');
if hMsvcrtl <> 0 then
begin
  VCStrStr := TMSVCStrStr(GetProcAddress(hMsvcrtl, 'strstr'));
  {$IFDEF UNICODE}
  VCStrStrW := TMSVCStrStrW(GetProcAddress(hMsvcrtl, 'wcsstr'));
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