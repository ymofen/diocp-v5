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


unit utils_strings;


interface

{$IFNDEF DEBUG}     // INLINE���õ���
{$IF defined(FPC) or (RTLVersion>=18))}
  {$DEFINE HAVE_INLINE}
{$IFEND HAVE_INLINE}
{$ENDIF}

// ���е���
{.$UNDEF HAVE_INLINE}
{.$DEFINE DIOCP_DEBUG_HINT}

/// ���������sleep������cpu��
/// spinlock ����ʧ��ʱ��ִ��sleep
{$DEFINE SPINLOCK_SLEEP}

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
{$IFDEF POSIX}
    , Posix.String_, Posix.Time,Posix.SysTypes
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
{$IF (RTLVersion>=26)}
  STRING_EMPTY_W: String = '';
{$ELSE}
  STRING_EMPTY_W: WideString = '';
{$IFEND >=XE5}

type
  TDataProc = procedure(pvData:Pointer);
  TDataEvent = procedure(pvData:Pointer) of object;
  TStringEvent = procedure(pvSender:TObject; const str:string) of object;
  TBufferFuncEvent = function(pvSender:TObject; const Buffer; Count:Int64):Int64 of object;
  
  TExceptionNotifyEvent = procedure(pvSender: TObject; pvException: Exception;
      pvTag: Integer) of object;

{$IFDEF MSWINDOWS}
  RAWString = AnsiString;
{$ELSE}
  RAWString = String;
{$ENDIF}

  // ���ڴ洢�ַ���
  PMAPKeyString = ^MAPKeyString;
  MAPKeyString = record
    key:Integer;
    value: string;
  end; 


{$IF (RTLVersion>=26)}

{$ELSE}
  MarshaledAString = PAnsiChar;
{$IFEND >=XE5}


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

  TCharWArray = array of DCharW;
  
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
    procedure ClearContent;
    function Append(c:Char): TDStringBuilder;  overload;
    function Append(const str: string): TDStringBuilder; overload;
    function Append(const str, pvLeftStr, pvRightStr: string): TDStringBuilder;
        overload;
    function Append(v: Boolean; UseBoolStrs: Boolean = True): TDStringBuilder;
        overload;
    function Append(v:Integer): TDStringBuilder; overload;
    function Append(v:Double): TDStringBuilder; overload;
    function AppendQuoteStr(const str: string): TDStringBuilder;
    function AppendSingleQuoteStr(const str: string): TDStringBuilder;
    function AppendLine(const str: string): TDStringBuilder;

    function ToString: string;{$IFDEF UNICODE}override;{$ENDIF}
    property Length: Integer read GetLength;

    procedure SaveToFile(const pvFile: String);
    procedure SaveToStream(pvStream:TStream);

    /// <summary>
    ///   ���з�: Ĭ��#13#10
    /// </summary>
    property LineBreak: String read FLineBreak write FLineBreak;
  end;


  TDStreamAdapter = class(TStream)
  private
    FOnWrite: TBufferFuncEvent;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload;
        override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize: Longint); override;
  public
    property OnWrite: TBufferFuncEvent read FOnWrite write FOnWrite;
  end;


  TDBufferBuilder = class(TStream)
  private
    FData: TBytes;
    FPosition: Integer;
    FSize: Integer;
    FCapacity :Integer;
    FBufferLocked:Boolean;
    FLineBreak: String;
    FTagPtr: Pointer;

    procedure CheckNeedSize(pvSize: LongInt); overload;
    procedure CheckNeedSize(pvOffset, pvSize: LongInt); overload;
    function GetLength: Integer;
    function GetRemain: Integer;
  public
    constructor Create;
    procedure Clear;
    procedure DecBuf(n:Integer);  // ��������
    function Append(const aByte:Byte): TDBufferBuilder; overload;
    function Append(const w:Word):TDBufferBuilder; overload;
    function Append(const c: Char): TDBufferBuilder; overload;
    function Append(const str: string): TDBufferBuilder; overload;
    function Append(const str, pvLeftStr, pvRightStr: string): TDBufferBuilder;
        overload;
    function Append(v: Boolean; UseBoolStrs: Boolean = True): TDBufferBuilder;
        overload;
    function Append(v:Integer): TDBufferBuilder; overload;
    function Append(v:Double): TDBufferBuilder; overload;
    function AppendUtf8(const str: String): TDBufferBuilder;
    function AppendByte(const aByte:Byte): TDBufferBuilder;
    function AppendWord(const w:Word): TDBufferBuilder; 
    /// <summary>
    ///  �Ƽ��ø÷���
    /// </summary>
    function AppendStringAsUTF8(const str:DStringW): TDBufferBuilder;

    function AppendRawStr(const pvRawStr: RAWString): TDBufferBuilder;
    function AppendBreakLineBytes: TDBufferBuilder;
    function Append(const str: string; pvConvertToUtf8Bytes: Boolean):
        TDBufferBuilder; overload;

    function AppendQuoteStr(const str: string): TDBufferBuilder;
    function AppendSingleQuoteStr(const str: string): TDBufferBuilder;
    function AppendLine(const str: string): TDBufferBuilder;

    procedure LoadFromFile(const pvFileName: string);

    procedure LoadFromStream(pvStream: TStream); overload;
    procedure SaveToFile(const pvFile: String);

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

    function ReadData(var vDest; pvStart, pvLength: Integer): Integer;

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

    function DecodeUTF8: string;

    function ToString: String;

    function ToRAWString: RAWString;

    /// <summary>
    ///   �����ڴ�ָ��
    /// </summary>
    function Memory: PByte;

    /// <summary>
    ///  ����һ��ָ��
    ///  pvIndex�������±�(��0��ʼ)
    /// </summary>
    function MemoryBuffer(const pvIndex: Integer): PByte;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload;  override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize: Longint); override;

    /// <summary>
    ///   �������п�������
    /// </summary>
    function ReArrange: TDBufferBuilder;

    function GetInstanceSize: Integer;
    function Replace(const data; startIdx, l:Integer): TDBufferBuilder;




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


    property TagPtr: Pointer read FTagPtr write FTagPtr;




  end;


  TDStringWBuilder = class(TObject)
  private
    FBlockSize:Integer;
    FInitiCapacity:Integer;
    FData: TCharWArray;
    FPosition: Integer;
    FCapacity :Integer;
    FLineBreak: DStringW;
    procedure CheckNeedSize(pvSize: LongInt);
    procedure SetBlockSize(pvBlockSize:Integer);
    function GetLength: Integer;
  public
    constructor Create;overload;
    constructor Create(pvInitCapacity, pvBlockSize: Integer); overload;

    procedure Clear;
    procedure ClearContent;
    function DecChar(i: Word): TDStringWBuilder;
    function Append(c: DCharW): TDStringWBuilder; overload;
    function Append(const str: DStringW): TDStringWBuilder; overload;
    function Append(const str, pvLeftStr, pvRightStr: DStringW): TDStringWBuilder;
        overload;
    function Append(v: Boolean; UseBoolStrs: Boolean = True): TDStringWBuilder;
        overload;
    function Append(v:Integer): TDStringWBuilder; overload;
    function Append(v:Double): TDStringWBuilder; overload;
    function Append(SWB:TDStringWBuilder):TDStringWBuilder; overload;
    function AppendQuoteStr(const str: DStringW): TDStringWBuilder;
    function AppendSingleQuoteStr(const str: DStringW): TDStringWBuilder;
    function AppendLine(const str: DStringW): TDStringWBuilder;


    function ToString: DStringW;{$IFDEF UNICODE}override;{$ENDIF}
    function WStrPtr: PDCharW;
    property Length: Integer read GetLength;

    procedure SaveToFile(const pvFile: String);
    procedure SaveToStream(pvStream:TStream);

    /// <summary>
    ///   ���з�: Ĭ��#13#10
    /// </summary>
    property LineBreak: DStringW read FLineBreak write FLineBreak;
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
///     �����ַ�, û���ҵ����ƶ�P
/// </summary>
/// <returns>
///   û�ҵ�����-1
///   ����ҵ�, �򷵻��������ַ�
/// </returns>
/// <param name="p"> ��ʼ���λ�� </param>
/// <param name="pvChars"> ������Щ�ַ���ֹͣ��Ȼ�󷵻� </param>
function SkipUntilEx(var p:PChar; pvChars: TSysCharSet): Integer;



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
///   ����N���ַ�
/// </summary>
function SkipN(p: PChar; n: Integer): PChar;


/// <summary>
///   �����ַ�
/// </summary>
function StrSkipChars(const src :String; pvChars:TSysCharSet): String;


/// <summary>
///   ����߿�ʼ��ȡ�����ַ���
/// </summary>
function LeftStr(const s:string; count:Integer): String;
function LeftChar(const s:string): Char;
/// <summary>
///   ���ұ߿�ʼ��ȡ�����ַ���
/// </summary>
function RightStr(const s:string; count:Integer): String;

/// <summary>
///   �ұߵĵ�һ���ַ�
/// </summary>
function RightChar(const s:string): Char;


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
function SplitStrings(const s: String; pvStrings: TStrings; pvSpliterChars:
    TSysCharSet): Integer;

/// <summary>
///   ����SpliterChars���ṩ���ַ������зָ��ַ��������뵽Array of String��
/// </summary>
/// <returns>
///   ���طָ�ĸ���
/// </returns>
/// <param name="s"> Դ�ַ��� </param>
/// <param name="pvStrings"> ��������ַ����б� </param>
/// <param name="pvSpliterChars"> �ָ��� </param>
function SplitToArrayStr(const s: String; pvSpliterChars: TSysCharSet;
    pvSkipSpliterChars: Boolean = false): TArrayStrings;

function SplitNToArrayStr(const s: String; pvSpliterChars: TSysCharSet; pvMaxN:
    Integer; pvSkipSpliterChars: Boolean = false): TArrayStrings;

function JoinArrayStrings(const pvArrayStrings: TArrayStrings; const
    pvSplitStr: DStringW): DStringW;


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
function SplitStr(const s: string; pvSpliterStr: string; var s1, s2: String):
    Boolean;

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
function URLEncode(const S: URLString; pvIsPostData: Boolean = true): URLString;


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
///   > 0 �ҵ�
/// </summary>
function PosStr(const sub, s: string): Integer;

function PosWStr(const sub, s: DStringW): Integer;


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
///   �Ƚ�����Buff�Ƿ����
/// </summary>
/// <returns> 0: ���, r := p1-p2 </returns>
function CompareBuf(p1, p2: Pointer; len: Integer): Integer;

/// <param name="compareLen">-1��ȫ���Ƚ�, 0: �Ƚϵ�����һ������</param>
/// <returns> 0: ���, r := p1-p2 </returns>
function CompareStrIgnoreCase(const s1, s2:PChar; compareLen:Integer): Integer;
function CompareWStrIgnoreCase(const s1, s2:PDCharW; compareLen:Integer): Integer;

/// <summary>
///   ���N���ַ�
/// </summary>
function FillNChr(const ACount: Integer; AChr: Char = ' '): string;

/// <summary>
///   ����ַ���
///   StrAddPrefix('A01', 5, '0') = 00A01
/// </summary>
function StrAddPrefix(const s:string; pvTotalWidth:Integer; pvFillChar:Char):
    String;

/// <summary>
///   ����ַ���
///   StrAddSuffix('A01', 5, '0') = 00A01
/// </summary>
function StrAddSuffix(const s:string; pvTotalWidth:Integer; pvFillChar:Char):
    String;

/// <summary>
///   ȷ�����һ���ַ�ΪsuffixChar�������
/// </summary>
function CheckAddSuffix(const s: string; pvCheckChars: TSysCharSet; suffixChar:
    Char): string;


/// <summary>
///  �ַ�ת��д
///  * ����qdac.qstrings
/// </summary>
function UpperChar(c: Char): Char;

function UpperCharW(c: DCharW): DCharW;

/// <summary>
///  aStr�Ƿ���Strs�б���
/// </summary>
/// <returns>
///   �������б��е�λ�ã����򷵻�-1
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
function StringToUtf8Bytes(const pvData: String; pvBytes: TBytes): Integer;
    overload;
function StringToUtf8Bytes(const pvData: string; pvProcessEndByte: Boolean = false): TBytes; overload;


function StringWToUtf8Bytes(const Source: PDCharW; SourceChars:Cardinal;
    pvDest: Pointer; MaxDestBytes: Cardinal): Cardinal; overload;

function StringWToUtf8Bytes(const pvSourceData: DStringW): TBytes; overload;


/// <summary>
///
/// </summary>
function Utf8BytesToString(const pvBytes: TBytes; pvOffset: Integer): String;

function Utf8BufferToString(pvBuff: PByte; pvLen: Integer): string;



function WideBufferToStringW(pvBuffer:Pointer; pvBufLength:Integer): DStringW;

function StringToBytes(const pvData: String; pvBytes: TBytes): Integer;
    overload;

function StringToBytes(const pvData: string): TBytes; overload;

/// <summary>
///   ��ע��pvBytes���治��Ԥ���ַ���
/// </summary>
function BytesToString(const pvBytes: TBytes; pvOffset: Integer): String;
function ByteBufferToString(pvBuff:PByte; pvLen:Cardinal): string;

/// <summary>
///   ����AnsiString�ַ����ĳ���(��0����)
/// </summary>
/// <returns>
///   �����ַ������ȣ�
///    -1��ʧ��
/// </returns>
function AnsiStringLength(pvBuff: Pointer; pvMaxLength: Integer = 0): Integer;

function SpanPointer(const pvStart, pvEnd: PByte): Integer;

function IsHexChar(c: Char): Boolean;

function HexValue(const c: Char): Integer;

function HexChar(const V: Byte): Char;

function HexToInt(const p:PChar; pvLength:Integer): Integer;

function PickString(p: PChar; pvOffset, pvCount: Integer): String;

/// <summary>
///  ��Utf8��BOM��ʽ���ļ��м����ַ���
/// </summary>
function LoadStringFromUtf8NoBOMFile(pvFile:string): String;

procedure WriteStringToUtf8NoBOMFile(const pvFile: String; const pvData:
    DStringW);

/// <summary>
///   ת���ַ���,
///   copy from qdac.qstrings.pas
/// </summary>
function ParseNumeric(var S: PChar; var ANum: Extended): Boolean;
function ParseHex(var p: PChar; var Value: Int64): Integer;
function ParseInt(var S: PChar; var ANum: Int64): Integer;

function CheckStrAllIsAsciiChar(const s:string): Boolean;
function CheckStrAllIsLetterChar(const s:string): Boolean;

{$if CompilerVersion < 20}
function CharInSet(const C: Char; const CharSet: TSysCharSet): Boolean;
{$ifend}

function GetTickCount: Cardinal;

function GetCurrentThreadID: Cardinal;

function ObjectHexAddr(pvObj:TObject): String;
function ObjectIntStrAddr(pvObj:TObject): String;

function ToUtc(pvDateTime:TDateTime): TDateTime;

function DateTimeString(pvDateTime:TDateTime): string; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
function NowString: String; {$IFDEF HAVE_INLINE} inline;{$ENDIF}

function DateTimeStrToDateTime(const strDateTime:string):TDateTime;

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;

procedure SwapBuff(buf: Pointer; offset, len: Integer); overload;

/// <summary>
///   Ϊ�ַ����½�һ��PStringָ�룬����s������Ӧ��ϵ
/// </summary>
function NewPString(const s: string): PString;

function GetStringFromPString(const p:Pointer): string;

function NewPDStringW(const s:DStringW): PDStringW;

function GetDStringWFromPtr(const p:Pointer): DStringW;

function NewMapKeyString(const key:Integer; const s:string): PMAPKeyString;


procedure PrintDebugString(s:string); {$IFDEF HAVE_INLINE} inline;{$ENDIF}


function HashStr(const pvStrData:String): Integer;
function HashPtr(const p:Pointer;l:Integer): Integer;

function NewIDString: DStringW;

procedure BufferToHex(pvBuffer: Pointer; outText: PDCharW; BufSize: Integer);


function RandomVal(const max: Cardinal): Cardinal;
function StrNextNO(const S: string; Value: integer): string;


{$IF RTLVersion<24}
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AtomicIncrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AtomicDecrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
{$IFEND <XE5}



procedure SpinLock(var Target:Integer; var WaitCounter:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
procedure SpinLock(var Target:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
procedure SpinUnLock(var Target:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF}overload;

{$IF RTLVersion < 18}
function InterlockedIncrement(var Addend: Integer): Integer; stdcall; external kernel32 name 'InterlockedIncrement';
{$EXTERNALSYM InterlockedIncrement}
function InterlockedDecrement(var Addend: Integer): Integer; stdcall; external kernel32 name 'InterlockedDecrement';
{$EXTERNALSYM InterlockedDecrement}
function InterlockedExchange(var Target: Integer; Value: Integer): Integer; stdcall;external kernel32 name 'InterlockedExchange';
{$EXTERNALSYM InterlockedExchange}
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall;external kernel32 name 'InterlockedCompareExchange';
{$EXTERNALSYM InterlockedCompareExchange}

function InterlockedExchangeAdd(Addend: PLongint; Value: Longint): Longint; overload; external kernel32 name 'InterlockedExchangeAdd';
function InterlockedExchangeAdd(var Addend: Longint; Value: Longint): Longint; overload; external kernel32 name 'InterlockedExchangeAdd';

{$IFEND <D2007}


{$IFDEF MSWINDOWS}
//http://code.google.com/p/omnithreadlibrary/source/browse/branches/x64/OtlSync.pas?r=1071
function CAS64(const oldData, newData: int64; var destination): boolean;
function AtomicAdd64(var Target:Int64; n:Int64): Int64; 
function __InterlockedCompareExchange64(var Destination: Int64; Exchange: Int64; Comparand: Int64): Int64; stdcall;
{$ELSE}

{$ENDIF}

procedure FreeAsObjectProc(var pvData: Pointer);
procedure FreeAsDisposeProc(var pvData: Pointer);

/// <summary>
///   ����TStringsʹ��
/// </summary>
function CmpIntValForSortFun(List: TStringList; Index1, Index2: Integer):
    Integer;


var
  __app_root: string;
  __default_datetime_fmt:string;

implementation


procedure FreeAsObjectProc(var pvData: Pointer);
var
  lvObj:TObject;
begin
  lvObj := TObject(pvData);
  FreeAndNil(lvObj);
  pvData := nil; 
end;

procedure FreeAsDisposeProc(var pvData: Pointer);
begin
  Dispose(pvData);
end;




procedure SpinLock(var Target:Integer; var WaitCounter:Integer);
begin
  while AtomicCmpExchange(Target, 1, 0) <> 0 do
  begin
    AtomicIncrement(WaitCounter);
//    {$IFDEF MSWINDOWS}
//      SwitchToThread;
//    {$ELSE}
//      TThread.Yield;
//    {$ENDIF}
    {$IFDEF SPINLOCK_SLEEP}
    Sleep(1);    // 1 �Ա�0 (�߳�Խ�࣬�ٶ�Խƽ��)
    {$ENDIF}
  end;
end;

procedure SpinLock(var Target:Integer);
begin
  while AtomicCmpExchange(Target, 1, 0) <> 0 do
  begin
    {$IFDEF SPINLOCK_SLEEP}
    Sleep(1);    // 1 �Ա�0 (�߳�Խ�࣬�ٶ�Խƽ��)
    {$ENDIF}
  end;
end;


procedure SpinUnLock(var Target:Integer);
begin
  if AtomicCmpExchange(Target, 0, 1) <> 1 then
  begin
    Assert(False, 'SpinUnLock::AtomicCmpExchange(Target, 0, 1) <> 1');
  end;
end;



{$IF RTLVersion<24}
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedCompareExchange(Target, Value, Comparand);
{$ELSE}
  Result := TInterlocked.CompareExchange(Target, Value, Comparand);
{$ENDIF}
end;

function AtomicIncrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedIncrement(Target);
{$ELSE}
  Result := TInterlocked.Increment(Target);
{$ENDIF}
end;

function AtomicDecrement(var Target: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedDecrement(Target);
{$ELSE}
  Result := TInterlocked.Decrement(Target);
{$ENDIF}
end;

{$IFEND <XE5}



{$IFDEF MSWINDOWS}
type
  TMSVCStrStr = function(s1, s2: PAnsiChar): PAnsiChar; cdecl;
  TMSVCStrStrW = function(s1, s2: PWChar): PWChar; cdecl;
  TMSVCMemCmp = function(s1, s2: Pointer; len: Integer): Integer; cdecl;
  TMSInterlockedCompareExchange64 = function(var Val: Int64; Exchange, Compare: Int64): Int64; stdcall;


var
  hMsvcrtl: HMODULE;
  hKernel32: HMODULE;

{$IFDEF UNICODE}
  VCStrStrW: TMSVCStrStrW;
{$ELSE}
  VCStrStr: TMSVCStrStr;
{$ENDIF}
  VCMemCmp: TMSVCMemCmp;

  InterlockedCompareExchange64Locker:Integer;
  InterlockedCompareExchange64Func :TMSInterlockedCompareExchange64;
{$ENDIF}

var
  __DateFormat: TFormatSettings;


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

function __xp_InterlockedCompareExchange64(var Val: Int64; Exchange, Compare: Int64): Int64; stdcall; {$IFDEF HAVE_INLINE}inline;{$ENDIF HAVE_INLINE}
begin 
  SpinLock(InterlockedCompareExchange64Locker);
  Result := Val;
  if val = Compare  then
  begin
    Result := Compare;
    Val := Exchange;
  end;
  SpinUnLock(InterlockedCompareExchange64Locker);

end;

{$if CompilerVersion < 20}
function CharInSet(const C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ifend}

function IsHexChar(c: Char): Boolean;
begin
  Result := ((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or
    ((c >= 'A') and (c <= 'F'));
end;

function HexValue(const c: Char): Integer;
begin
  if (c >= '0') and (c <= '9') then
    Result := Ord(c) - Ord('0')
  else if (c >= 'a') and (c <= 'f') then
    Result := 10 + Ord(c) - Ord('a')
  else
    Result := 10 + Ord(c) - Ord('A');
end;

function HexChar(const V: Byte): Char;
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
  {$IFDEF MSWINDOWS}
  for i := 1 to l do
  {$ELSE}
  for i := Low(s) to high(s) do
  {$ENDIF}
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


function CmpIntValForSortFun(List: TStringList; Index1, Index2: Integer):
    Integer;
var
  v1, v2:Integer;
begin
  v1 := StrToInt64Def(List[index1], 0);
  v2 := StrToInt64Def(List[Index2], 0);
  Result := CompareValue(v1, v2);
//  if v1<v2 then
//    result:=-1
//  else if v2=v2 then
//    Result:=0
//  else
//    Result:=1;
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
    Result := STRING_EMPTY;
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


function SplitStrings(const s: String; pvStrings: TStrings; pvSpliterChars:
    TSysCharSet): Integer;
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
        // ��ӵ��б���
        pvStrings.Add(P);
        inc(Result);
      end;
      Exit;
    end else
    begin
      // �����ָ���
      SkipChars(p, pvSpliterChars);

      // ��ӵ��б���
      pvStrings.Add(lvValue);
      inc(Result);
    end;
  end;
end;

function SplitToArrayStr(const s: String; pvSpliterChars: TSysCharSet;
    pvSkipSpliterChars: Boolean = false): TArrayStrings;
var
  p:PChar;
  lvValue : String;
  l, idx, r:Integer;
  procedure checkLength();
  begin
    if idx <= l then
    begin
      if idx < 8 then l := 8
      else if idx < 64 then l := 64
      else if idx < 128 then l := 128
      else l := idx + 1024;           
      SetLength(Result, l);
    end;
  end;
begin
  p := PChar(s);
  l := 0;
  idx := 0;
  while True do
  begin
    // ������ͷ
    r := LeftUntil(P, pvSpliterChars, lvValue);


    if r = -1 then
    begin    // û��ƥ�䵽
      if P^ <> #0 then
      begin  // ���һ���ַ�
        // ��ӵ��б���
        SetLength(Result, idx + 1);
        Result[idx] := P;
        Inc(idx);
      end;
      Exit;
    end else
    begin
      // ƥ��ɹ�
      checkLength();
      Result[idx] := lvValue;
      Inc(idx);
      Inc(P);
    end;

    if (pvSkipSpliterChars) then  // �����ָ�����
      SkipChars(P, pvSpliterChars);
  end;

  SetLength(Result, idx + 1);
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
        s := Format('$%s%s', [URLChar(ASrc[i]), URLChar(ASrc[i+1])]);
        Result[j] := URLChar(StrToInt(s));
      except end;
      Inc(i, 1);  // ������һ���ַ�.

    end;
    Inc(i);
    Inc(j);
  end;
  SetLength(Result, j - 1);
  {$ENDIF}

end;




function URLEncode(const S: URLString; pvIsPostData: Boolean = true): URLString;
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
    if PSubUP^ = #0 then  // �Ƚϵ����
    begin
      Result := true;
    end;

  end else
  begin
    Result := CompareMem(P, pvStart, Length(pvStart));
  end;
end;

function SplitStr(const s: string; pvSpliterStr: string; var s1, s2: String):
    Boolean;
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

function StringToUtf8Bytes(const pvData: String; pvBytes: TBytes): Integer;
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
  if Result > Length(pvBytes) then Result := Length(pvBytes);
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

function Utf8BytesToString(const pvBytes: TBytes; pvOffset: Integer): String;
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



function StringToBytes(const pvData: String; pvBytes: TBytes): Integer;
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



function BytesToString(const pvBytes: TBytes; pvOffset: Integer): String;
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

//function Utf8BufferToString(pvBuff: PByte; pvLen: Integer): string;
//var
//  lvBytes:TBytes;
//  lvRawStr:AnsiString;
//  l:Cardinal;
//begin
//  l := pvLen;
//  SetLength(lvRawStr, l);
//  Move(pvBuff^, PansiChar(lvRawStr)^, l);
//  Result := UTF8Decode(lvRawStr);
//end;


function Utf8BufferToString(pvBuff: PByte; pvLen: Integer): string;
{$IFDEF MSWINDOWS}
var
  lvRawStr:AnsiString;
  l:Cardinal;
{$ELSE}
var
  lvBytes:TBytes;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  l := pvLen;
  SetLength(lvRawStr, l);
  Move(pvBuff^, PansiChar(lvRawStr)^, l);
  Result := UTF8Decode(lvRawStr);
{$ELSE}
  SetLength(lvBytes, pvLen);
  Move(pvBuff^, lvBytes[0], pvLen);
  // UNICODE ���׳��������쳣
  Result := TEncoding.UTF8.GetString(lvBytes);
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

function TDStringBuilder.Append(const str: string): TDStringBuilder;
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

function TDStringBuilder.Append(const str, pvLeftStr, pvRightStr: string):
    TDStringBuilder;
begin
  Result := Append(pvLeftStr).Append(str).Append(pvRightStr);
end;

function TDStringBuilder.AppendLine(const str: string): TDStringBuilder;
begin
  Result := Append(Str).Append(FLineBreak);
end;

function TDStringBuilder.AppendQuoteStr(const str: string): TDStringBuilder;
begin
  Result := Append('"').Append(str).Append('"');
end;

function TDStringBuilder.AppendSingleQuoteStr(const str: string):
    TDStringBuilder;
begin
  Result := Append('''').Append(str).Append('''');
end;

procedure TDStringBuilder.CheckNeedSize(pvSize: LongInt);
var
  lvCapacity:LongInt;
begin
  if (FPosition + pvSize) >= FCapacity then
  begin
    lvCapacity := (FPosition + pvSize + BUFFER_BLOCK_SIZE) AND (not (BUFFER_BLOCK_SIZE - 1));
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

procedure TDStringBuilder.ClearContent;
begin
  FPosition := 0;
  if FCapacity > 0 then
  begin
    FillChar(FData[0], FCapacity, 0);
  end;
end;

function TDStringBuilder.GetLength: Integer;
begin
  Result := FPosition;
end;

procedure TDStringBuilder.SaveToFile(const pvFile: String);
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

procedure TDStringBuilder.SaveToStream(pvStream:TStream);
var
  l:Integer;
begin
  l := self.Length;
{$IFDEF UNICODE}
  l := l shl 1;
{$ENDIF}

  if l <> 0 then pvStream.WriteBuffer(FData[0], l);
end;

function TDStringBuilder.ToString: string;
var
  l:Integer;
begin
  l := Length;
  SetLength(Result, l);
  if l > 0 then
  begin
  {$IFDEF UNICODE}
    Move(FData[0], PChar(Result)^, l shl 1);
  {$ELSE}
    Move(FData[0], PChar(Result)^, l);
  {$ENDIF}
  end;
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

function TDBufferBuilder.Append(const str: string): TDBufferBuilder;
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

function TDBufferBuilder.Append(const str, pvLeftStr, pvRightStr: string):
    TDBufferBuilder;
begin
  Result := Append(pvLeftStr).Append(str).Append(pvRightStr);
end;

function TDBufferBuilder.Append(const aByte:Byte): TDBufferBuilder;
begin
  Result := AppendBuffer(@aByte, 1);
end;


function TDBufferBuilder.Append(const str: string; pvConvertToUtf8Bytes:
    Boolean): TDBufferBuilder;
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

function TDBufferBuilder.AppendWord(const w:Word): TDBufferBuilder;
begin
  Result := AppendBuffer(@w, 2);
end;

function TDBufferBuilder.AppendByte(const aByte:Byte): TDBufferBuilder;
begin
  Result := AppendBuffer(@aByte, 1);
end;

function TDBufferBuilder.Replace(const data; startIdx, l:Integer):
    TDBufferBuilder;
var
  l1 :Integer;
begin
  if FBufferLocked then
  begin
    raise Exception.Create('Buffer Locked');
  end;
  if startIdx > FSize  then
  begin
    startIdx := FSize;
  end;
  
  l1 := (startIdx + l) - FSize;
  if l1 > 0 then
  begin
    CheckNeedSize(l1);
  end;
  Move(data, FData[startIdx], l);
  if l1 > 0 then
  begin
    Inc(FSize, l1);
  end;

end;

function TDBufferBuilder.AppendBreakLineBytes: TDBufferBuilder;
begin
  if FBufferLocked then
  begin
    raise Exception.Create('Buffer Locked');
  end;
  CheckNeedSize(2);
  Assert((FSize + 2) < self.FCapacity);
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
  if (FSize + pvLength) >= self.FCapacity then
  begin
   Assert((FSize + pvLength) < self.FCapacity);
  end;

  // ��������
  Move(pvBuffer^, FData[FSize], pvLength);
  Inc(FSize, pvLength);
  // �ƶ�Position
  FPosition := FSize;
  Result := Self;
end;

function TDBufferBuilder.AppendLine(const str: string): TDBufferBuilder;
begin
  Result := Append(Str).Append(FLineBreak);
end;

function TDBufferBuilder.AppendQuoteStr(const str: string): TDBufferBuilder;
begin
  Result := Append('"').Append(str).Append('"');
end;





function TDBufferBuilder.AppendRawStr(const pvRawStr: RAWString):
    TDBufferBuilder;
begin
{$IFDEF MSWINDOWS}
  Result := AppendBuffer(PByte(pvRawStr), System.Length(pvRawStr));
{$ELSE}
  Result := AppendUtf8(pvRawStr);
{$ENDIF}
end;



function TDBufferBuilder.AppendSingleQuoteStr(const str: string):
    TDBufferBuilder;
begin
  Result := Append('''').Append(str).Append('''');
end;

function TDBufferBuilder.AppendStringAsUTF8(const str:DStringW):
    TDBufferBuilder;
var
  l, l1, l2: Integer;
begin
  if FBufferLocked then
  begin
    raise Exception.Create('Buffer Locked');
  end;

  Result := Self;
  if System.Length(str) = 0 then Exit;
  l1 := System.Length(str);
  l2 := l1 shl 1 + l1;
  CheckNeedSize(l1);
  l := StringWToUtf8Bytes(PDCharW(str), l1, @FData[FSize], l2);
  Inc(FSize, l);
  // �ƶ�Position
  FPosition := FSize;
end;

function TDBufferBuilder.AppendUtf8(const str: String): TDBufferBuilder;
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
  if (FSize + pvSize) >= FCapacity then
  begin
    lvCapacity := (FSize + pvSize + BUFFER_BLOCK_SIZE) AND (not (BUFFER_BLOCK_SIZE - 1));
    FCapacity := lvCapacity;
    SetLength(FData, FCapacity);
  end;
end;

procedure TDBufferBuilder.CheckNeedSize(pvOffset, pvSize: LongInt);
var
  lvCapacity:LongInt;
begin
  if (pvOffset + pvSize) >= FCapacity then
  begin
    lvCapacity := (pvOffset + pvSize + BUFFER_BLOCK_SIZE) AND (not (BUFFER_BLOCK_SIZE - 1));
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

procedure TDBufferBuilder.DecBuf(n: Integer);
begin
  Dec(FSize, n);
end;

function TDBufferBuilder.DecodeUTF8: string;
begin
  if FSize =0 then
  begin
    Result := STRING_EMPTY;
    exit;
  end;
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

procedure TDBufferBuilder.LoadFromFile(const pvFileName: string);
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
  Assert(pvIndex < self.FCapacity);
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

procedure TDBufferBuilder.SaveToFile(const pvFile: String);
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

function TDBufferBuilder.ReadData(var vDest; pvStart, pvLength: Integer):
    Integer;
begin
  if pvStart >= self.FSize then
  begin
    Result := 0;
    exit;
  end;
  
  if pvStart + pvLength > self.FSize then
  begin
    pvLength := self.FSize - pvStart;
  end;
  Move(self.FData[pvStart], vDest, pvLength);
  Result := pvLength;
end;

function TDBufferBuilder.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize + Offset;
  end;
  if FPosition > FSize then
  begin
    FPosition := FSize;
  end;
  Result := FPosition;
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
  result := AnsiStrings.StrPas(PAnsiChar(@FData[0]));
  //Result := UTF8Decode(@FData[0]);
  //Result := TEncoding.Default.GetString(FData, 0, self.Length);
{$ENDIF}
end;

function TDBufferBuilder.ToString: String;
begin
  CheckNeedSize(2);
  FData[FSize] := 0;
  FData[FSize + 1] := 0; 
{$IFDEF MSWINDOWS}
  {$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
  TEncoding.Default.GetString(FData, 0, self.Length);
  {$ELSE}
  Result := StrPas(PAnsiChar(@FData[0]));
  {$IFEND >=XE5}
{$ELSE}  
  Result := TEncoding.Default.GetString(FData, 0, self.Length);
{$ENDIF}
end;

function TDBufferBuilder.Write(const Buffer; Count: Longint): Longint;
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

procedure WriteStringToUtf8NoBOMFile(const pvFile: String; const pvData:
    DStringW);
var
  lvStream: TMemoryStream;
  lvBytes:TBytes;

begin
  lvStream := TMemoryStream.Create;
  try
    lvBytes := StringWToUtf8Bytes(pvData);

    lvStream.Write(lvBytes[0], Length(lvBytes));

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

function StringToBytes(const pvData: string): TBytes;
{$IFNDEF UNICODE}
var
  lvRawStr:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := TEncoding.Default.GetBytes(pvData);
{$ELSE}
  // Ӧ�ñ���һ�²�ֵ�����\0
  lvRawStr := pvData;
  SetLength(Result, Length(lvRawStr));
  Move(PAnsiChar(lvRawStr)^, Result[0], Length(lvRawStr));
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


function DateTimeStrToDateTime(const strDateTime:string): TDateTime;
begin
  Result := SysUtils.StrToDateTime(strDateTime, __DateFormat);
end;

function DateTimeString(pvDateTime:TDateTime): string;
begin
  if pvDateTime = 0 then
  begin
    Result := '0000-00-00 00:00:00.000'
  end else
  begin
    Result := FormatDateTime(__default_datetime_fmt, pvDateTime);
  end;
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

function PosWStr(const sub, s: DStringW): Integer;
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

function StringWToUtf8Bytes(const Source: PDCharW; SourceChars: Cardinal;
    pvDest: Pointer; MaxDestBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
  lvDest:PByte;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if pvDest <> nil then
  begin
    lvDest := PByte(pvDest);
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        lvDest^ := (c); inc(lvDest);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        lvDest^ := ($E0 or (c shr 12)); inc(lvDest);
        lvDest^ := ($80 or ((c shr 6) and $3F));inc(lvDest);
        lvDest^ := ($80 or (c and $3F));inc(lvDest);
        inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        lvDest^ := ($C0 or (c shr 6)); inc(lvDest);
        lvDest^ := ($80 or (c and $3F)); inc(lvDest);
        Inc(count,2);
      end;
    end;
    Assert(count <= MaxDestBytes, '��Խ��Ŀ���,������(StringWToUtf8Bytes)');
  end
  else
  begin    // ֻ���㳤��
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count;
end;

function StringWToUtf8Bytes(const pvSourceData: DStringW): TBytes; overload;
var
  L, l1: Integer;
begin
  if length(pvSourceData) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  l1 := Length(pvSourceData);
  l1 := l1 shl 1 + l1;
  SetLength(Result, l1); // SetLength includes space for null terminator
  L := StringWToUtf8Bytes(PWideChar(pvSourceData), Length(pvSourceData), @Result[0], Length(Result));
  if L > 0 then
    SetLength(Result, L)   // ȥ�����0
  else
    SetLength(Result, 0);
end;

function NewPDStringW(const s:DStringW): PDStringW;
begin
  New(Result);
  Result^ := s;
end;

function GetDStringWFromPtr(const p:Pointer): DStringW;
begin
  if p = nil then
  begin
    Result := STRING_EMPTY;
  end else
  begin
    Result := PDStringW(p)^;
  end;
end;

function FillNChr(const ACount: Integer; AChr: Char = ' '): string;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  for i := 1 to ACount do
  begin
    Result[i] := AChr;
  end;
end;

function StrAddPrefix(const s:string; pvTotalWidth:Integer; pvFillChar:Char):
    String;
begin
  Result := FillNChr(pvTotalWidth - length(s), pvFillChar) + s;
end;

function SplitNToArrayStr(const s: String; pvSpliterChars: TSysCharSet; pvMaxN:
    Integer; pvSkipSpliterChars: Boolean = false): TArrayStrings;
var
  p:PChar;
  lvValue : String;
  idx, r:Integer;
begin
  if (pvMaxN <= 0) then
  begin
    Result := SplitToArrayStr(s, pvSpliterChars, pvSkipSpliterChars);
    Exit;
  end else if pvMaxN = 1 then
  begin
    SetLength(Result, 1);
    Result[0] := s;
    Exit;
  end;

  SetLength(Result, pvMaxN);

  p := PChar(s);
  idx := 0;
  while True do
  begin
    // ������ͷ
    r := LeftUntil(P, pvSpliterChars, lvValue);
    if r = -1 then
    begin    // û��ƥ�䵽
      if P^ <> #0 then
      begin  // ���һ���ַ�
        // ��ӵ��б���
        SetLength(Result, idx + 1);
        Result[idx] := P;
      end;
      Exit;
    end else
    begin
      Result[idx] := lvValue;
      Inc(idx);
      Inc(P);
    end;

    if (pvSkipSpliterChars) then  // �����ָ�����
      SkipChars(P, pvSpliterChars);

    if (idx + 1) = pvMaxN then
    begin
      Result[idx] := P;
      Exit;
    end;
  end;

  SetLength(Result, idx + 1);

end;

function StrSkipChars(const src :String; pvChars:TSysCharSet): String;
var
  lvPtr:PChar;
begin
  lvPtr := PChar(src);
  SkipChars(lvPtr, pvChars);
  Result := lvPtr;
end;

function RightStr(const s:string; count:Integer): String;
var
  lvPtr :PChar;
  l:Integer;
begin
  l := Length(s);
  if l < count then
  begin
    Result :=s ;
  end else
  begin
    lvPtr := PChar(s);
    Result := SkipN(lvPtr, l - count);
  end;
end;

function SkipN(p: PChar; n: Integer): PChar;
var
  i:Integer;
begin
  i := 0;
  while (p^ <> #0) and (i<n) do
  begin
    Inc(P);
    Inc(i);
  end;
  Result := P;   
end;

function LeftStr(const s:String; count:Integer): string;
begin
  Result := s;
  if Length(Result) > count then
  begin
    SetLength(Result, count);
  end;  
end;

function LeftChar(const s:string): Char;
begin
  Result := PChar(s)^;
end;

function PosStr(const sub, s: string): Integer;
begin
  Result := Pos(sub, s);
end;

function IncMinute(const AValue: TDateTime;
  const ANumberOfMinutes: Int64): TDateTime;
begin
  Result := ((AValue * MinsPerDay) + ANumberOfMinutes) / MinsPerDay;
end;

function ToUtc(pvDateTime:TDateTime): TDateTime;
{$IFDEF MSWINDOWS}
var
  pTime: _TIME_ZONE_INFORMATION;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetTimeZoneInformation(pTime);//��ȡʱ��
  Result := IncMinute(pvDateTime, pTime.Bias);
{$ENDIF}
end;

function StrAddSuffix(const s:string; pvTotalWidth:Integer; pvFillChar:Char):
    String;
begin
  Result :=s + FillNChr(pvTotalWidth - length(s), pvFillChar);
end;

function JoinArrayStrings(const pvArrayStrings: TArrayStrings; const
    pvSplitStr: DStringW): DStringW;
var
  i: Integer;
  lvSB:TDStringWBuilder;
begin
  lvSB := TDStringWBuilder.Create;
  try
    for i := 0 to length(pvArrayStrings) - 1 do
    begin
      if (i > 0) then lvSB.Append(pvSplitStr);
      lvSB.Append(pvArrayStrings[i]);

    end;
    Result := lvSB.ToString;
  finally
    lvSB.Free;
  end;
end;

function RightChar(const s:string): Char;
var
  lvPtr :PChar;
  l:Integer;
begin
  l := Length(s);
  if l = 0 then
  begin
    Result := #0;
  end else
  begin
    lvPtr := PChar(s);
    lvPtr := SkipN(lvPtr, l -1);
    Result := lvPtr^
  end;

end;

function CheckAddSuffix(const s: string; pvCheckChars: TSysCharSet; suffixChar:
    Char): string;
begin
  if RightChar(s) in pvCheckChars then
  begin
    Result := s;
  end else
  begin
    Result := s + suffixChar;
  end;
end;

function HashPtr(const p:Pointer;l:Integer): Integer;
var
  ps:PInteger;
  lr:Integer;
begin
  Result:=0;
  if l>0 then
  begin
    ps:=p;
    lr:=(l and $03);      //check length is multi 4
    l:=(l and $FFFFFFFC); //
    while l>0 do
    begin
      Result:=((Result shl 5) or (Result shr 27)) xor ps^;
      Inc(ps);
      Dec(l,4);
    end;
    if lr<>0 then
    begin
      l:=0;
      Move(ps^,l,lr);
      Result:=((Result shl 5) or (Result shr 27)) xor l;
    end;
  end;
end;

procedure BufferToHex(pvBuffer: Pointer; outText: PDCharW; BufSize: Integer);
const
  Convert: array[0..15] of DCharW = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  I: Integer;
  Buffer:PByte;
begin
  Buffer:=PByte(pvBuffer);

  for I := 0 to BufSize - 1 do
  begin
    outText[0] := Convert[Byte(Buffer^) shr 4];
    outText[1] := Convert[Byte(Buffer^) and $F];
    Inc(outText, 2);
    Inc(Buffer);
  end;
end;

function HashStr(const pvStrData:String): Integer;
begin
  Result := HashPtr(PChar(pvStrData), Length(pvStrData) * SizeOf(Char));
end;

function NewIDString: DStringW;
var
  lvGuid:TGUID;
begin
  CreateGUID(lvGuid);
  SetLength(Result, SizeOf(TGUID) * 2);
  BufferToHex(@lvGuid, PDCharW(Result), SizeOf(TGUID));
end;

function RandomVal(const max: Cardinal): Cardinal;
var
  lvGuid:TGUID;
begin
  CreateGUID(lvGuid);
  Result := lvGuid.D2 + lvGuid.D3 + lvGuid.D1;  //   Cardinal(HashPtr(@lvGuid, SizeOf(TGUID)));
  if max > 0 then
  begin
    Result := Result mod max;
  end;
end;

function FillChr(const ACount: Integer; AChr: Char = ' '): string;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  for i := 1 to ACount do
  begin
    Result[i] := AChr;
  end;
end;

function StrNextNO(const S: string; Value: integer): string;
var
  s2: string;
  s3: string;
  i: integer;
begin
  s2 := '';
  for i := length(s) downto 1 do
    if s[i] in ['0'..'9'] then
      s2 := s[i] + s2
    else
      break;
  if length(s2) < length(s) then
    s3 := Copy(s, 1, length(s) - length(s2))
  else
    s3 := '';
  s2 := IntToStr(StrToInt(s2) + value);
  if length(s2) + length(s3) < length(s) then
    s2 := FillChr(length(s) - length(s3) - length(s2), '0') + s2;
  Result := s3 + s2;
end;


{$IFDEF MSWINDOWS}

function CAS64(const oldData, newData: int64; var destination): boolean;
asm
{$IFNDEF CPUX64}
  push  edi
  push  ebx
  mov   edi, destination
  mov   ebx, low newData
  mov   ecx, high newData
  mov   eax, low oldData
  mov   edx, high oldData
  lock cmpxchg8b [edi]
  pop   ebx
  pop   edi
{$ELSE CPUX64}
  mov   rax, oldData
  lock cmpxchg [destination], newData
{$ENDIF ~CPUX64}
  setz  al
end; { CAS64 }





function __InterlockedCompareExchange64;
begin
  Result := InterlockedCompareExchange64Func(Destination, Exchange, Comparand);
end;

function AtomicAdd64(var Target:Int64; n:Int64): Int64;
var
  r, Old: Int64;
begin
  repeat
    Old := Target;
    Result := Old + n;
    r := InterlockedCompareExchange64Func(Target, Result, Old);
    if r = Old then
    begin
      break;
    end;
  until (1=1);
end;
{$ENDIF}

function CompareBuf(p1, p2: Pointer; len: Integer): Integer;
begin
  if len = 0 then
    Result := 0
  else
  begin
    {$IFDEF MSWINDOWS}
      Result := VCMemCmp(p1, p2, len)
    {$ELSE}
      Result := memcmp(p1, p2, len);
    {$ENDIF}
  end;
end;

function CompareStrIgnoreCase(const s1, s2:PChar; compareLen:Integer): Integer;
var
  lvP1, lvP2:PChar;
  j:Integer;
begin
  j := 0;
  Result := 0;
  lvP1 := s1;
  lvP2 := s2;
  while true do
  begin
    if (compareLen=0) and ((lvP1^ = #0) or (lvP2^ = #0)) then
    begin
      Break;
    end else if (j=compareLen) then
    begin
      Break;
    end else if (lvP1^=#0) and (lvP2^=#0) then
    begin
      break;
    end;

    Result := Ord(lvP1^)-Ord(lvP2^);
    if Result <> 0 then
      Result := Ord(UpCase(lvP1^)) - Ord(UpCase(lvP2^));

    if Result <> 0 then
    begin
      Break;
    end;
    inc(lvP1);
    Inc(lvP2);
    Inc(j);
  end;
end;

function CompareWStrIgnoreCase(const s1, s2:PDCharW; compareLen:Integer):
    Integer;
var
  lvP1, lvP2:PDCharW;
  j:Integer;
begin
  j := 0;
  Result := 0;  // Ĭ�����
  lvP1 := s1;
  lvP2 := s2;
  while true do
  begin
    if (compareLen=0) and ((lvP1^ = #0) or (lvP2^ = #0)) then
    begin
      Break;
    end else if (j=compareLen) then
    begin
      Break;
    end else if (lvP1^=#0) and (lvP2^=#0) then
    begin
      break;
    end;
    Result := Ord(lvP1^)-Ord(lvP2^);
    if Result <> 0 then
      Result := Ord(UpperCharW(lvP1^)) - Ord(UpperCharW(lvP2^));

    if Result <> 0 then
    begin
      Break;
    end;
    inc(lvP1);
    Inc(lvP2);
    Inc(j);
  end;

end;

function UpperCharW(c: DCharW): DCharW;
begin
  if (c >= #$61) and (c <= #$7A) then
    Result := DCharW(PWord(@c)^ xor $20)
  else
    Result := c;
end;

function CheckStrAllIsLetterChar(const s:string): Boolean;
var
  Ptr:PChar;
begin
  Result := False;
  Ptr := PChar(s);
  while Ptr^ <> #0 do
  begin
    Result := ((Ptr^ >= 'A') and (Ptr^ <= 'Z')) or ((Ptr^ >= 'a') and (Ptr^ <= 'z'));
    Inc(Ptr);
    if not Result then
    begin
      Exit;
    end;       
  end;

end;

function CheckStrAllIsAsciiChar(const s:string): Boolean;
var
  Ptr:PChar;
begin
  Result := False;
  Ptr := PChar(s);
  while Ptr^ <> #0 do
  begin
    // #20 Ϊ�ո�, #126 Ϊ~, �����˿ɶ���Ascii�ַ�
    Result := (Ptr^ >= #20) and (Ptr^ <= #126);
    Inc(Ptr);
    if not Result then
    begin
      Exit;
    end;       
  end;
end;





constructor TDStringWBuilder.Create;
begin
  inherited Create;
{$if CompilerVersion> 18}    // Delphi7 or later
  FLineBreak := DCharW(13) + DCharW(10);
{$else}
  FLineBreak := #13#10;
{$ifend}
  FInitiCapacity := 0;
  Clear;
  SetBlockSize(512);
end;

function TDStringWBuilder.Append(c: DCharW): TDStringWBuilder;
begin
  CheckNeedSize(1);
  FData[FPosition] := c;
  Inc(FPosition);
  Result := Self;
end;

function TDStringWBuilder.Append(const str: DStringW): TDStringWBuilder;
var
  l:Integer;
begin
  Result := Self;
  l := System.Length(str);
  if l = 0 then Exit;
  CheckNeedSize(l);

  Move(PDCharW(str)^, FData[FPosition], l shl 1);


  Inc(FPosition, l);

end;

function TDStringWBuilder.Append(v: Boolean; UseBoolStrs: Boolean = True):
    TDStringWBuilder;
begin
  Result := Append(BoolToStr(v, UseBoolStrs));
end;

function TDStringWBuilder.Append(v:Integer): TDStringWBuilder;
begin
  Result :=Append(IntToStr(v));
end;

function TDStringWBuilder.Append(v:Double): TDStringWBuilder;
begin
  Result := Append(FloatToStr(v));
end;

function TDStringWBuilder.Append(const str, pvLeftStr, pvRightStr: DStringW):
    TDStringWBuilder;
begin
  Result := Append(pvLeftStr).Append(str).Append(pvRightStr);
end;

function TDStringWBuilder.AppendLine(const str: DStringW): TDStringWBuilder;
begin
  Result := Append(Str).Append(FLineBreak);
end;

function TDStringWBuilder.AppendQuoteStr(const str: DStringW): TDStringWBuilder;
begin
  Result := Append('"').Append(str).Append('"');
end;

function TDStringWBuilder.AppendSingleQuoteStr(const str: DStringW):
    TDStringWBuilder;
begin
  Result := Append('''').Append(str).Append('''');
end;

procedure TDStringWBuilder.CheckNeedSize(pvSize: LongInt);
var
  lvCapacity:LongInt;
begin
  if FPosition + pvSize > FCapacity then
  begin
    // block�ı��� ���ǲ����sizeС
    lvCapacity := (FPosition + pvSize + (FBlockSize - 1)) AND (not (FBlockSize - 1));
    FCapacity := lvCapacity;
    SetLength(FData, FCapacity);     
  end;
end;

procedure TDStringWBuilder.Clear;
begin
  FPosition := 0;

  // modify by ymf
  // 2017-01-10 17:36:13
  FCapacity := FInitiCapacity;
  SetLength(FData, FCapacity);
end;

procedure TDStringWBuilder.ClearContent;
begin
  FPosition := 0;
  if FCapacity > 0 then
  begin
    FillChar(FData[0], FCapacity, 0);
  end;
end;

constructor TDStringWBuilder.Create(pvInitCapacity, pvBlockSize: Integer);
begin
  inherited Create;
{$if CompilerVersion> 18}    // Delphi7 or later
  FLineBreak := DCharW(13) + DCharW(10);
{$else}
  FLineBreak := #13#10;
{$ifend}
  FInitiCapacity := pvInitCapacity;

  Clear;
  if pvBlockSize = 0 then pvBlockSize := 128;
  SetBlockSize(pvBlockSize);
end;

function TDStringWBuilder.DecChar(i: Word): TDStringWBuilder;
begin
  if i > FPosition then
  begin
    FPosition := 0;
  end else
  begin
    Dec(FPosition, i);
  end;
  Result := Self;
end;

function TDStringWBuilder.GetLength: Integer;
begin
  Result := FPosition;
end;

procedure TDStringWBuilder.SaveToFile(const pvFile: String);
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

procedure TDStringWBuilder.SaveToStream(pvStream:TStream);
var
  l:Integer;
begin
  l := self.Length;
  l := l shl 1;

  if l <> 0 then pvStream.WriteBuffer(FData[0], l);
end;

procedure TDStringWBuilder.SetBlockSize(pvBlockSize: Integer);
var
  j, i:Integer;
begin
  j := pvBlockSize;
  i := 0;
  while j > 0 do
  begin
    j := j shr 1;
    Inc(i);
  end;
  Dec(i, 2);
  FBlockSize := 2 shl i;
end;

function TDStringWBuilder.ToString: DStringW;
var
  l:Integer;
begin
  l := Length;
  SetLength(Result, l);                
  Move(FData[0], PDCharW(Result)^, l shl 1); 
end;

function TDStringWBuilder.WStrPtr: PDCharW;
begin
  Result := @FData[0];
end;

function TDStringWBuilder.Append(SWB: TDStringWBuilder): TDStringWBuilder;
var
  l:Integer;
begin
  Result := Self;
  l := SWB.Length;
  if l = 0 then Exit;
  CheckNeedSize(l);

  Move(SWB.FData[0], FData[FPosition], l shl 1);


  Inc(FPosition, l);
end;

{ TDStreamAdapter }

function TDStreamAdapter.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TDStreamAdapter.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

function TDStreamAdapter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

procedure TDStreamAdapter.SetSize(NewSize: Longint);
begin
  inherited;

end;

function TDStreamAdapter.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(FOnWrite) then
  begin
    Result := FOnWrite(Self,Buffer,Count);
  end else
  begin
    Result := 0;
  end;
end;

initialization  
  __DateFormat.DateSeparator := '-';
  __DateFormat.TimeSeparator := ':';
  __DateFormat.ShortDateFormat := 'yyyy-MM-dd';
  __DateFormat.LongDateFormat := 'yyyy-MM-dd';
  __DateFormat.ShortTimeFormat := 'HH:nn:ss';
  __DateFormat.LongTimeFormat := 'HH:nn:ss';
  __app_root := ExtractFilePath(ParamStr(0));
  __default_datetime_fmt := 'yyyy-MM-dd hh:nn:ss.zzz';

  {$IFDEF MSWINDOWS}

  {$IFDEF UNICODE}
  VCStrStrW := nil;
  {$ELSE}
  VCStrStr := nil;
  {$ENDIF}
  VCMemCmp := nil;
  hMsvcrtl := LoadLibrary('msvcrt.dll');
  if hMsvcrtl <> 0 then
  begin
    {$IFDEF UNICODE}
    VCStrStrW := TMSVCStrStrW(GetProcAddress(hMsvcrtl, 'wcsstr'));
    {$ELSE}
    VCStrStr := TMSVCStrStr(GetProcAddress(hMsvcrtl, 'strstr'));
    {$ENDIF}
    VCMemCmp := TMSVCMemCmp(GetProcAddress(hMsvcrtl, 'memcmp'));
  end;

  hKernel32 := LoadLibrary(kernel32);
  if hKernel32 <> 0 then
  begin
    InterlockedCompareExchange64Func := TMSInterlockedCompareExchange64(GetProcAddress(hKernel32, 'InterlockedCompareExchange64'));
  end;

  if not Assigned(InterlockedCompareExchange64Func) then
    InterlockedCompareExchange64Func := @__xp_InterlockedCompareExchange64;

  {$ENDIF}

finalization

{$IFDEF MSWINDOWS}
if hMsvcrtl <> 0 then
  FreeLibrary(hMsvcrtl);

if hKernel32 <> 0 then
  FreeLibrary(hKernel32);
{$ENDIF}

end.
