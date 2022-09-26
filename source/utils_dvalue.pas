(*
 * ��Ȩ����:
      qdac.swish, d10.�����
 * �ο�qdac��qvalue����ʵ��
 *
 * 1. android����DValuelistʹ�� TList���г����쳣, ʹ��TList<TDValueObject>����
 *    2015-11-15 16:08:04(��лCP46����)
 * 2. SetArraySize, ��������Ŀռ���������ClearDValue������ͻ��ң������ڴ��������
 *����2016-06-17 15:27:49(��лJ����)
*)
unit utils_dvalue;

{$IF CompilerVersion>25}  // XE4(VER250)
  {$DEFINE HAVE_GENERICS}
{$IFEND}

{$IFNDEF NEXTGEN}

{$ENDIF}

{$IF (not Defined(NEXTGEN))}
  {$DEFINE HAVE_ASNI_STRING}
{$IFEND}


{$IFDEF DEBUG}
{$DEFINE CHECK_DVALUE}
{$ENDIF}

interface


uses classes, sysutils, variants,
{$IFDEF HAVE_GENERICS}
     System.Generics.Collections,
     System.Generics.Defaults,
{$ENDIF}
   utils_BufferPool,
   varutils, math, utils_base64, utils_strings;


type

  TDValueException = class(Exception);



  PInterface = ^IInterface;

  // XE5
  {$IF CompilerVersion<26}
  IntPtr=Integer;
  {$IFEND IntPtr}

  {$if CompilerVersion < 18} //before delphi 2007
  TBytes = array of Byte;
  {$ifend}

  // �ͷŶ���
  TObjectFreeAction = (faNone, faFree);

  // ָ���ͷŶ���
  TPtrReleaseAction = (praNone, praObjectFree, praDispose, praFreeMem);


  TDValueDataType = (vdtUnset, vdtNull, vdtBoolean, vdtSingle, vdtFloat,
    vdtInteger, vdtInt64, vdtUInt64, vdtCurrency, vdtGuid, vdtDateTime,
    vdtStringA,
    vdtString, vdtStringW, vdtStream, vdtInterface, vdtPtr, vdtObject, vdtArray);

  TDValueDataTypes = set of TDValueDataType;

  // �ڵ�����
  TDValueObjectType = (vntNull,        // û��ֵ
                     vntArray,       // �б�-����
                     vntObject,      // �б�-Key-Value
                     vntValue        // ֵ
                     );



  PDRawValue = ^TDRawValue;

  /// һ��ֵ����
  ///  �޸�ʱ��ҪҪ�޸�RawCopyFrom����
  TDRawInnerValue = record
    case Integer of
      0:
        (AsBoolean: Boolean);
      1:
        (AsFloat: Double);
      2:
        (AsInteger: Integer);
      3:
        (AsInt64: Int64);
      4:
        (AsUInt64: UInt64);
      5:
        (AsGuid: PGuid);
      6:
        (AsDateTime: TDateTime);
      7:
        (AsString: PString);
      {$IFDEF HAVE_ASNI_STRING}
      8:
        (AsStringA: PAnsiString);
      {$ENDIF}
      9:
        (AsStringW: PDStringW);
      15:
        (AsStream: Pointer);
      16:    // Array
        (
          ArrayLength: Integer;
          ArrayItemsEntry: PDRawValue;
        );
      17:
        (AsCurrency: Currency);
      18:
        (AsSingle: Single);
      20:
        (AsShort: Shortint);
      21:
        (AsByte: Byte);
      22:
        (AsSmallint: Smallint);
      23:
        (AsWord: Word);
      24:
        (AsExtend: Extended);
      25:
        (
          AsPointer: Pointer;
          PtrReleaseAction: TPtrReleaseAction;
        );
      27:
        (AsInterface: PInterface);
//      30:
//        (
//          ValueType: TDValueDataType;
//          Value: PDRawValue;
//        );
  end;

  TDRawValue = record
    Value: TDRawInnerValue;
    ValueType: TDValueDataType;
  end;
  TDRawValueArray = array of TDRawValue;

const
  TDValueObjectTypeStr: array[TDValueObjectType] of string = ('vntNull', 'vntArray', 'vntObject', 'vntValue');

  Path_SplitChars : TSysCharSet = ['.', '/' , '\'];


type
  TDValueItem = class;
  TDValue = class;
  TOnDValueNotifyEvent = procedure(Sender:TObject; pvVal:TDValue) of object;
  TDValueObject = class(TObject)
  private
    FName: String;
    FRawValue: TDRawValue;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Double;
    function GetAsInetger: Int64;
    function GetAsString: String;
    function GetDataType: TDValueDataType;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInetger(const Value: Int64);
    procedure SetAsString(const Value: String);
  public
    destructor Destroy; override;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsString: String read GetAsString write SetAsString;
    property AsInetger: Int64 read GetAsInetger write SetAsInetger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;

    property DataType: TDValueDataType read GetDataType;

    property Name: String read FName write FName;
  end;


  TDValueList = class(TObject)
  private
    {$IFDEF HAVE_GENERICS}
    FList: TList<TDValueObject>;
    {$ELSE}
    FList: TList;
    {$ENDIF}
    function GetCount: Integer;
    function GetItems(pvIndex: Integer): TDValueObject;
    function InnerAdd(const pvValueName: string): TDValueObject;
  public
    constructor Create();
    destructor Destroy; override;
    function Add(pvValueName:String): TDValueObject;

    function FindByName(const pvValueName: string): TDValueObject;

    function ParamByName(const pvValueName: String): TDValueObject;

    /// <summary>
    ///   ������������ڻ���д���,�������ֱ�ӷ���
    /// </summary>
    function ForceByName(const pvValueName: String): TDValueObject;

    /// <summary>
    ///   ������еĶ���
    /// </summary>
    procedure Clear;    
    
    property Count: Integer read GetCount;

    property Items[pvIndex: Integer]: TDValueObject read GetItems; default;
  end;


  /// <summary>
  ///   DValue�ڵ�
  /// </summary>
  TDValue = class(TObject)
  private
    {$IFDEF CHECK_DVALUE}
    ///���ڼ������Ƿ��⵽�ƻ�
    __objflag:byte;
    __CheckValue:array[0..7] of Byte;
    procedure __CheckValueOK;
    procedure __InitalizeCheckValue;
    {$ENDIF}
  private
    /// <summary>
    ///   ����޸�ʱ��
    /// </summary>
    FLastModify:Cardinal;
    procedure DoLastModify;
  private
    FName: TDValueItem;
    FValue: TDValueItem;
    FObjectType: TDValueObjectType;
    FParent: TDValue;
    FLastMsg:String;

    {$IFDEF HAVE_GENERICS}
    FChildren: TList<TDValue>;
    {$ELSE}
    FChildren: TList;
    {$ENDIF}
  private
    function GetCount: Integer;
    /// <summary>
    ///   �ͷ����е��Ӷ���
    ///   ����б�
    /// </summary>
    procedure ClearChildren();
    procedure CheckCreateChildren;
    procedure CreateName();
    procedure DeleteName();


    function GetItems(pvIndex: Integer): TDValue;

    /// <summary>
    ///   �������Ʋ����ӽڵ�
    /// </summary>
    function IndexOf(const pvName: string): Integer; overload;

    /// <summary>
    ///   �������Ʋ����ӽڵ�
    /// </summary>
    function IndexOf(pvName: Integer): Integer; overload;

    /// <summary>
    ///   ����·�����Ҷ�����������ڷ���nil
    /// </summary>
    /// <returns>
    ///   ������ڷ����ҵ��Ķ�����������ڷ���nil
    /// </returns>
    /// <param name="pvPath"> Ҫ���ҵ�·�� </param>
    /// <param name="vParent"> ������ҵ����󷵻��ҵ�����ĸ��ڵ� </param>
    /// <param name="vIndex"> ������ҵ�����,��ʾ�ڸ��ڵ��е�����ֵ </param>
    function InnerFindByPath(pvPath: string; var vParent:TDValue; var vIndex: Integer): TDValue;

  private
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    function GetAsFloat: Double;
    function GetAsInteger: Int64;
    function GetAsString: String;
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(const Value: Int64);
    procedure SetAsString(const Value: String);
    function GetAsObject: TObject;
    function GetAsStream: TMemoryStream;
    function GetAsUInt: UInt64;
    procedure SetAsUInt(const Value: UInt64);
    procedure CheckBeforeAddChild(pvType: TDValueObjectType);
    function GetAsInterface: IInterface;
    procedure SetAsInterface(const Value: IInterface);
    {$IF (not Defined(NEXTGEN))}
    function GetAsAnsiString: AnsiString;
    procedure SetAsAnsiString(const Value: AnsiString);
    {$IFEND}

    function GetAsStringW: DStringW;
    procedure SetAsStringW(const Value: DStringW);

    procedure SetAsDateTime(const Value: TDateTime);
    function GetAsDateTime: TDateTime;
  public
  {$IFDEF HAVE_GENERICS}
    procedure Sort(const Compare: IComparer<TDValue>);
  {$ELSE}
    procedure Sort(Compare: TListSortCompare);
  {$ENDIF}

    /// <summary>
    ///   �����ӽڵ��г���δ�޸ĵ��ӽڵ�
    /// </summary>
    function ClearLastModify(pvTimeOut:Cardinal = 30000): Integer;
    
    constructor Create(pvType: TDValueObjectType); overload;

    constructor Create; overload;

    destructor Destroy; override;


    function Clone(pvIgnoreValueTypes: TDValueDataTypes = [vdtInterface, vdtObject,
        vdtPtr]): TDValue;

    procedure CloneFrom(pvSource: TDValue; pvIgnoreValueTypes: TDValueDataTypes =
        [vdtInterface, vdtObject, vdtPtr]);

    procedure CloneValueFrom(pvSource: TDValue; pvIgnoreValueTypes:
        TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);

    procedure MergeValue(pvSource:TDValue; pvIgnoreValueTypes: TDValueDataTypes =
        [vdtInterface, vdtObject, vdtPtr]);

    /// <summary>
    ///   ���ýڵ�����, ����ת��ʱ�ᶪʧ����
    /// </summary>
    function CheckSetNodeType(pvType:TDValueObjectType): TDValue;

    function FindChild(const Key:string; pvKeyVal:string): TDValue;

    function FindByName(const pvName: String): TDValue; overload;

    function FindByName(pvName:Integer): TDValue; overload;

    function FindByPath(const pvPath: string): TDValue;

    function ItemByName(const pvName: string): TDValue;

    function ForceByName(const pvName: string): TDValue; overload;

    function ForceByName(pvName:Integer): TDValue; overload;

    function ForceByPath(const pvPath: String): TDValue;

    /// <summary>
    ///   ���ӽڵ�������ַ����б�
    /// </summary>
    function ToStrings(pvNameSpliter: String = '='; pvPreNameFix: string =
        STRING_EMPTY; pvValueDelimiter: string = sLineBreak): String;

    /// <summary>
    ///   ������Ϊһ���������һ���ӽڵ�
    ///     ���֮ǰ�����������ͣ����ᱻ���
    /// </summary>
    function AddArrayChild: TDValue; overload;

    /// <summary>
    ///  ��Ϊһ���Ӷ������
    /// </summary>
    procedure AddArrayChild(pvDValue: TDValue); overload;

    /// <summary>
    ///   ������Ϊһ��vntObject���һ���ӽڵ�
    ///     ���֮ǰ����vntObject���ͣ����ᱻ���
    /// </summary>
    function Add: TDValue; overload;
    function AsArray: TDValue;


    function Add(const pvName: String; pvType: TDValueObjectType): TDValue;
        overload;
    function Add(const pvName: String): TDValue; overload;
    function Add(const pvName, pvValue: string): TDValue; overload;
    function Add(const pvName: string; pvValue: Integer): TDValue; overload;
    function Add(const pvName: string; pvValue: Boolean): TDValue; overload;
    function Add(const pvName: string; pvValue: Double): TDValue; overload;

    /// <summary>
    ///  ���һ��DValue,���pvValueΪnull���������
    /// </summary>
    function Add(const pvName: string; pvValue: TDValue): TDValue; overload;

    function AddVar(const pvName: string; const pvValue: Variant): TDValue;


    /// <summary>
    ///   ֱ�Ӱ�, ӵ�иö������������
    /// </summary>
    procedure AttachDValue(const pvName: String; pvDValue: TDValue);

    /// <summary>
    ///   ���������Ƴ���һ���Ӷ���
    ///   (��Ӧ�Ķ��󽫱�����)
    ///   ��ʹ��Delete
    /// </summary>
    function RemoveByName(const pvName: String): Integer;

    function RemoveByPath(const pvPath: String): Boolean;

    /// <summary>
    ///   �Ӹ����н������Ƴ�������Ƴ��ɹ�, ����true, �ұ���������
    /// </summary>
    function RemoveFromParent: Boolean;

    /// <summary>
    ///   �Ӹ����н������Ƴ�������Ƴ��ɹ�, ����true�������ᱻ����
    /// </summary>
    function UnAttachFromParent: Boolean;


    function IndexDataOf(pvData:Pointer): Integer;

    /// <summary>
    ///   �ͷ����е��Ӷ���
    ///   ����б�
    /// </summary>
    procedure RemoveAll;

    /// <summary>
    ///   ����ֵ(���Ʋ��ᱻ���)
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   ��������ɾ����һ���Ӷ���(��Ӧ�Ķ��󽫱�����)
    /// </summary>
    procedure Delete(pvIndex:Integer); overload;

    /// <summary>
    ///   ���������Ƴ���һ���Ӷ���
    ///   (��Ӧ�Ķ��󽫱�����)
    /// </summary>
    function Delete(const pvName: String): Integer; overload;

    /// <summary>
    ///   ���������Ƴ�һ���Ӷ���(��Ӧ�Ķ��󲻻ᱻ����)
    /// </summary>
    function UnAttach(pvIndex:Integer): TDValue; overload;

    /// <summary>
    ///   ���������Ƴ�һ���Ӷ���(��Ӧ�Ķ��󲻻ᱻ����)
    /// </summary>
    function UnAttach(pvName:String): TDValue; overload;

    /// <summary>
    ///  ����Ŀ����
    /// </summary>
    property Count: Integer read GetCount;


    property Items[pvIndex: Integer]: TDValue read GetItems; default;

    /// <summary>
    ///   ��ֵ����
    /// </summary>
    property Name: TDValueItem read FName;

    /// <summary>
    ///   ��������
    /// </summary>
    property ObjectType: TDValueObjectType read FObjectType;

    /// <summary>
    ///   ���ڵ�
    /// </summary>
    property Parent: TDValue read FParent;

    /// <summary>
    ///   ֵ����
    /// </summary>
    property Value: TDValueItem read FValue;

  public
    /// <summary>
    ///   ͳ�����ݴ�С
    /// </summary>
    function SizeOf: Integer;

    procedure SetAsVariant(const pvValue: Variant);

    function GetStrValueByName(const pvName, pvDefault: string): String;
    function GetIntValueByName(pvName: String; pvDefault: Int64): Int64;
    function GetFloatValueByName(pvName: String; pvDefault: Double): Double;
    
    function GetValueByName(const pvName: String; pvDefault: Int64): Int64;
        overload;
    function GetValueByName(const pvName, pvDefault: string): String; overload;
    function GetValueByName(const pvName: String; pvDefault: Boolean): Boolean;
        overload;
    function GetValueByName(const pvName: String; pvDefault: Double): Double;
        overload;
    function GetValueByName(const pvName: String; pvDefault: TObject): TObject;
        overload;

    function GetValueByPath(const pvPath: string; pvDefault: string): string;
        overload;
    function GetValueByPath(const pvPath: string; pvDefault: Int64): Int64;
        overload;
    function GetValueByPath(const pvPath: string; pvDefault: Boolean): Boolean;
        overload;
    function GetValueByPath(const pvPath: string; pvDefault: Double): Double;
        overload;
    function GetValueByPath(const pvPath: string; pvDefault: TObject): TObject;
        overload;

    /// <summary>
    ///   ��������Base64���븳ֵ��Value
    /// </summary>
    procedure Base64LoadFromStream(pvInStream: TStream);

    procedure Base64SaveToStream(pvOutStream:TStream);

    procedure Base64LoadFromFile(pvFileName: String);

    procedure Base64SaveToFile(pvFileName: String);

    // ��Value�ķ��ʷ�װ, ����ֱ�ӷ���Value.AsXXXX
    procedure BindObject(pvObject: TObject; pvFreeAction: TObjectFreeAction =
        faFree);
    /// <summary>
    ///   �Ƿ���ֵ, ������ַ�����Ϊ�գ�Ҳ�᷵��true
    /// </summary>
    function IsEmpty: Boolean;
    function IsNull: Boolean;



    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsString: String read GetAsString write SetAsString;
    property AsStringW: DStringW read GetAsStringW write SetAsStringW;
    
    {$IF (not Defined(NEXTGEN))}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    {$IFEND}

    property AsInterface: IInterface read GetAsInterface write SetAsInterface;

    property AsInteger: Int64 read GetAsInteger write SetAsInteger;

    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;

    property AsUInt: UInt64 read GetAsUInt write SetAsUInt;

    property AsObject: TObject read GetAsObject;

    property AsStream: TMemoryStream read GetAsStream;
  end;

  TDValueItem = class(TObject)
  private
    {$IFDEF CHECK_DVALUE}
    __objflag:Byte;
    procedure __CheckValueOK;
   {$ENDIF}
  private
    FRawValue: TDRawValue;
    function GetItems(pvIndex: Integer): TDValueItem;
    function GetSize: Integer;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Double;
    function GetAsInteger: Int64;
    function GetAsInterface: IInterface;

    function GetAsString: String;
    function GetDataType: TDValueDataType;

    function GetAsObject: TObject;
    function GetAsStream: TMemoryStream;
    function GetAsUInt: UInt64;

    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(const Value: Int64);
    procedure SetAsInterface(const Value: IInterface);

    procedure SetAsString(const Value: String);
    procedure SetAsUInt(const Value: UInt64);
    function GetAsStringW: WideString;
    procedure SetAsStringW(const Value: WideString);

    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);

    {$IFNDEF NEXTGEN}
    function GetAsStringA: AnsiString;
    procedure SetAsStringA(const Value: AnsiString);
    {$ENDIF}
  public
    /// <summary>
    ///   ��ȡ���ݴ�С
    /// </summary>
    function SizeOf: Integer;

    procedure CloneFrom(pvSource: TDValueItem; pvIgnoreValueTypes: TDValueDataTypes
        = [vdtInterface, vdtObject, vdtPtr]);

    /// <summary>
    ///   ����Ϊ���鷽ʽͬʱ���������С
    ///    ���֮ǰ�������鷽ʽ�����ᱻ����
    ///    ������õĳߴ��֮ǰ��֮ǰ��ֵ���ᱻ����
    ///    ���С��֮ǰ�ĳߴ磬�����ֵ���ᱻ���
    /// </summary>
    procedure SetArraySize(const Value: Integer);

    constructor Create();

    destructor Destroy; override;

    /// <summary>
    ///  �Ƚ�����ֵ�Ƿ����
    /// </summary>
    function Equal(pvItem:TDValueItem): Boolean;

    /// <summary>
    ///   ���ֵ
    /// </summary>
    procedure Clear;


    property AsFloat: Double read GetAsFloat write SetAsFloat;

    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;

    property AsString: String read GetAsString write SetAsString;
    {$IFNDEF NEXTGEN}
    property AsStringA: AnsiString read GetAsStringA write SetAsStringA;
    {$ENDIF}
    
    property AsStringW:WideString read GetAsStringW write SetAsStringW;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsUInt: UInt64 read GetAsUInt write SetAsUInt;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TObject read GetAsObject;

    property AsStream: TMemoryStream read GetAsStream;



    property AsInterface: IInterface read GetAsInterface write SetAsInterface;

    procedure BindObject(pvObject: TObject; pvFreeAction: TObjectFreeAction =
        faFree);
        
    /// <summary>
    ///   �ַ������գ�����null
    /// </summary>
    function IsEmpty: Boolean;


    /// <summary>
    ///   ����������ȡ����
    /// </summary>
    property Items[pvIndex: Integer]: TDValueItem read GetItems; default;

    property Size: Integer read GetSize;

    property DataType: TDValueDataType read GetDataType;
  end;





 function CreateDValueObject: TDValue; overload;
 function CreateDValueObject(pvType: TDValueObjectType): TDValue; overload;
 procedure DisposeDValueObject(aVal:TDValue);

function CompareDValue(pvDValue1: PDRawValue; pvDValue2:PDRawValue):
    Integer;

function GetDValueSize(ADValue: PDRawValue): Integer;
function GetDValueItem(ADValue: PDRawValue; pvIndex: Integer): PDRawValue;

/// <summary>����DValue�ڲ�ռ�õ��ڴ�</summary>
/// <param name="ADValue"> (PDRawValue) </param>
procedure ClearDValue(ADValue:PDRawValue);
procedure CheckDValueSetType(ADValue:PDRawValue; AType: TDValueDataType);

procedure CheckDValueSetArrayLength(ADValue: PDRawValue; ALen: Integer);


procedure DValueSetAsString(ADValue: PDRawValue; const pvString: String);
function DValueGetAsString(ADValue:PDRawValue): string;

procedure DValueSetAsStringW(ADValue: PDRawValue; const pvString: DStringW);
function DValueGetAsStringW(ADValue:PDRawValue): DStringW;

{$IF (not Defined(NEXTGEN))}
procedure DValueSetAsStringA(ADValue:PDRawValue; pvString:AnsiString);
function DValueGetAsStringA(ADValue:PDRawValue): AnsiString;
{$IFEND}

procedure DValueBindPointerData(ADValue:PDRawValue; pvData:Pointer;
    pvReleaseAction:TPtrReleaseAction);
procedure DValueBindObjectData(ADValue:PDRawValue; pvData:TObject;
    pvReleaseAction:TPtrReleaseAction);
function DValueGetAsObject(ADValue:PDRawValue): TObject;

procedure DValueSetAsInterface(ADValue: PDRawValue; const pvValue:
    IInterface);
function DValueGetAsInterface(ADValue:PDRawValue): IInterface;

procedure DValueSetAsInt64(ADValue:PDRawValue; pvValue:Int64);
function DValueGetAsInt64(ADValue: PDRawValue): Int64;

procedure DValueSetAsUInt64(ADValue:PDRawValue; pvValue:UInt64);
function DValueGetAsUInt64(ADValue: PDRawValue): UInt64;


procedure DValueSetAsInteger(ADValue:PDRawValue; pvValue:Integer);
function DValueGetAsInteger(ADValue: PDRawValue): Integer;


procedure DValueSetAsFloat(ADValue:PDRawValue; pvValue:Double);
function DValueGetAsFloat(ADValue: PDRawValue): Double;

procedure DValueSetAsDateTime(ADValue:PDRawValue; pvValue:TDateTime);
function DValueGetAsDateTime(ADValue: PDRawValue): TDateTime;

procedure DValueSetAsBoolean(ADValue:PDRawValue; pvValue:Boolean);
function DValueGetAsBoolean(ADValue: PDRawValue): Boolean;

function DValueIsEmpty(ADValue:PDRawValue): Boolean;

procedure RawValueCopyFrom(pvSource, pvDest: PDRawValue; pvIgnoreValueTypes:
    TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);

function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): DStringW; overload;
function BinToHex(const ABytes: TBytes; ALowerCase: Boolean): DStringW; overload;

procedure FreeObject(AObject: TObject);

function GetDValueObjectCount: Integer;

function GetDValuePrintDebugInfo: String;








implementation

{$IFDEF DEBUG}
var
 __create_cnt:Integer;
 __destroy_cnt:Integer;
{$ENDIF}

var
  __DateTimeFormat:string;

resourcestring
  SValueNotArray = '��ǰֵ�����������ͣ��޷������鷽ʽ���ʡ�';
  SConvertError = '�޷��� %s ת��Ϊ %s ���͵�ֵ��';
  SUnsupportStreamSource = '�޷��� Variant ����ת��Ϊ����';
  SOutOfBound   = '����[%d]������Χ(0..%d)';

  SItemNotFound = '�Ҳ�����Ӧ����Ŀ:%s';
  SItemExists   = '��Ŀ[%s]�Ѿ�����,�����ظ����.';
  SNoNameNode   = '������[%s]�ڵ㲻��������';
  SNoValueNode  = '������[%s]�ڵ㲻����ֵ';

const
  DValueTypeName: array [TDValueDataType] of String = ('Unassigned', 'NULL',
    'Boolean', 'Single', 'Float', 'Integer', 'Int64', 'UInt64', 'Currency', 'Guid',
    'DateTime', 'AnsiString', 'String', 'StringW', 'Stream', 'Interface', 'Pointer', 'Object', 'Array');

procedure FreeObject(AObject: TObject);
begin
{$IFDEF AUTOREFCOUNT}
  AObject.DisposeOf;
{$ELSE}
  AObject.Free;
{$ENDIF}
end;

function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): DStringW;
const
  B2HConvert: array [0 .. 15] of DCharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  B2HConvertL: array [0 .. 15] of DCharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  pd: PDCharW;
  pb: PByte;
begin
  SetLength(Result, l shl 1);
  pd := PDCharW(Result);
  pb := p;
  if ALowerCase then
  begin
    while l > 0 do
    begin
      pd^ := B2HConvertL[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvertL[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end
  else
  begin
    while l > 0 do
    begin
      pd^ := B2HConvert[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvert[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end;
end;

function BinToHex(const ABytes: TBytes; ALowerCase: Boolean): DStringW;
begin
  Result := BinToHex(@ABytes[0], Length(ABytes), ALowerCase);
end;


function GetFirst(var strPtr: PChar; splitChars: TSysCharSet): string;
var
  oPtr:PChar;
  l:Cardinal;
begin
  oPtr := strPtr;
  Result := '';
  while True do
  begin
    if CharInSet(strPtr^ , splitChars) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
      {$IFDEF UNICODE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l shl 1);
      {$ELSE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l);
      {$ENDIF}
        break;
      end;
    end else if (strPtr^ = #0) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
      {$IFDEF UNICODE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l shl 1);
      {$ELSE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l);
      {$ENDIF}
      end;
      break;
    end;
    Inc(strPtr);
  end;
end;

function GetDValueSize(ADValue: PDRawValue): Integer;
var
  I: Integer;
begin
  Result := 0;
  case ADValue.ValueType of
    vdtBoolean:
      Result := 1;
    vdtSingle:
      Result := SizeOf(Single);
    vdtFloat:
      Result := SizeOf(Double);
    vdtInteger:
      Result := SizeOf(Integer);
    vdtInt64:
      Result := SizeOf(Int64);
    vdtUInt64:
      Result := SizeOf(UInt64);
    vdtCurrency:
      Result := SizeOf(Currency);
    vdtGuid:
      Result := SizeOf(TGuid);
    vdtDateTime:
      Result := SizeOf(TDateTime);
    vdtString:
    {$IFDEF UNICODE}
      Result := Length(ADValue.Value.AsString^) shl 1;
    {$ELSE}
      Result := Length(ADValue.Value.AsString^);
    {$ENDIF}
    vdtStringW:
      Result := Length(ADValue.Value.AsStringW^) shl 1;
    vdtStream:
      Result := TMemoryStream(ADValue.Value.AsStream).Size;
    vdtArray:
      begin
        Result := 0;
        for I := 0 to ADValue.Value.ArrayLength - 1 do
          Inc(Result, GetDValueSize(GetDVAlueItem(@ADValue, I)));
      end;
  end;
end;

function GetDValueItem(ADValue: PDRawValue; pvIndex: Integer): PDRawValue;
begin
  if ADValue.ValueType = vdtArray then
  begin
    if (pvIndex < 0) or (pvIndex >= ADValue.Value.ArrayLength) then
    begin
      raise TDValueException.CreateFmt(SOutOfBound, [pvIndex, ADValue.Value.ArrayLength - 1]);
    end;
    Result := PDRawValue(IntPtr(ADValue.Value.ArrayItemsEntry) + (SizeOf(TDRawValue) * pvIndex))
  end else
    raise Exception.Create(SValueNotArray);
end;

procedure ClearDValue(ADValue:PDRawValue);
  procedure ClearArray;
  var
    I: Integer;
  begin
    I := 0;
    while I < ADValue.Value.ArrayLength do
    begin
      ClearDValue(GetDValueItem(ADValue, I));
      Inc(I);
    end;
    FreeMem(ADValue.Value.ArrayItemsEntry);
  end;
  procedure ClearPointer();
  begin
    case ADValue.Value.PtrReleaseAction of
      praNone:;
      praObjectFree:
        begin
          FreeObject(TObject(ADValue.Value.AsPointer));
        end;
      praDispose:
        begin
          Dispose(ADValue.Value.AsPointer);
        end;
      praFreeMem:
        begin
          FreeMem(ADValue.Value.AsPointer);
        end;
    end;
    ADValue.Value.AsPointer := nil;
  end;

begin
  if ADValue.ValueType <> vdtUnset then
  begin
    case ADValue.ValueType of
      vdtGuid:
        Dispose(ADValue.Value.AsGuid);
      {$IFDEF HAVE_ASNI_STRING}
      vdtStringA:
        Dispose(ADValue.Value.AsStringA);
      {$ENDIF}
      vdtString:
        Dispose(ADValue.Value.AsString);
      vdtStringW:
        Dispose(ADValue.Value.AsStringW);
      vdtStream:
        FreeObject(TObject(ADValue.Value.AsStream));
      vdtInterface:
        begin
          ADValue.Value.AsInterface^ := nil;
          Dispose(ADValue.Value.AsInterface);
        end;
      vdtObject, vdtPtr:
        ClearPointer;
      vdtArray:
        ClearArray;
    end;
    ADValue.ValueType := vdtUnset;
  end;
end;

procedure CheckDValueSetType(ADValue:PDRawValue; AType: TDValueDataType);
var
  lvStream:TMemoryStream;
begin
  if ADValue.ValueType <> AType then
  begin
    ClearDValue(ADValue);
    case AType of
      vdtGuid:
        New(ADValue.Value.AsGuid);
      vdtString:
        New(ADValue.Value.AsString);
{$IFDEF HAVE_ASNI_STRING}
      vdtStringA:
        New(ADValue.Value.AsStringA);
{$ENDIF}
      vdtStringW:
        New(ADValue.Value.AsStringW);
      vdtInterface:
        New(ADValue.Value.AsInterface);
      vdtStream:
      begin
        lvStream := TMemoryStream.Create;
        ADValue.Value.AsStream := lvStream;
        {$IFDEF NEXTGEN}
        // �ƶ�ƽ̨��AData�ļ�����Ҫ���ӣ��Ա����Զ��ͷ�
        if lvStream <> nil then
        begin
          lvStream.__ObjAddRef;
        end;
        {$ENDIF}
      end;
      vdtArray:
        ADValue.Value.ArrayLength := 0;
    end;
    ADValue.ValueType := AType;
  end;
end;

procedure CheckDValueSetArrayLength(ADValue: PDRawValue; ALen: Integer);
var
  lvNewPtr:Pointer;
begin
  CheckDValueSetType(ADValue, vdtArray);
  if ALen > 0 then
  begin
    if ADValue.Value.ArrayLength = 0 then
    begin        // ԭ�г���Ϊ��
      GetMem(ADValue.Value.ArrayItemsEntry, SizeOf(TDRawValue) * ALen);

      // ��������Ŀռ�
      FillChar(ADValue.Value.ArrayItemsEntry^, SizeOf(TDRawValue) * ALen, 0);
      ADValue.Value.ArrayLength := ALen;
    end
    else
    begin
      if ALen > ADValue.Value.ArrayLength then
      begin
        ReallocMem(ADValue.Value.ArrayItemsEntry, SizeOf(TDRawValue) * ALen);

        lvNewPtr := Pointer(IntPtr(ADValue.Value.ArrayItemsEntry) + ADValue.Value.ArrayLength * SizeOf(TDRawValue));
        // ����������Ŀռ�
        FillChar(lvNewPtr^, SizeOf(TDRawValue) * (ALen - ADValue.Value.ArrayLength), 0);

        ADValue.Value.ArrayLength := ALen;
      end
      else
      begin
        while ADValue.Value.ArrayLength > ALen do
        begin
          ClearDValue(GetDValueItem(ADValue, ADValue.Value.ArrayLength - 1));
          Dec(ADValue.Value.ArrayLength);
        end;
      end;
    end;
  end;
end;

{$IF (not Defined(NEXTGEN))}
procedure DValueSetAsStringA(ADValue:PDRawValue; pvString:AnsiString);
begin
  CheckDValueSetType(ADValue, vdtStringA);
  ADValue.Value.AsStringA^ := pvString;
end;

function DValueGetAsStringA(ADValue:PDRawValue): AnsiString;
var
  lvHexStr:AnsiString;
  function DTToStr(ADValue: PDRawValue): AnsiString;
  begin
    if Trunc(ADValue.Value.AsFloat) = 0 then
      Result := FormatDateTime({$IF RTLVersion>=22} FormatSettings.{$IFEND}LongTimeFormat, ADValue.Value.AsDateTime)
    else if IsZero(ADValue.Value.AsFloat - Trunc(ADValue.Value.AsFloat)) then
      Result := FormatDateTime
        ({$IF RTLVersion>=22}FormatSettings.{$IFEND}LongDateFormat,
        ADValue.Value.AsDateTime)
    else
      Result := FormatDateTime
        ({$IF RTLVersion>=22}FormatSettings.{$IFEND}LongDateFormat + ' ' +
{$IF RTLVersion>=22}FormatSettings.{$IFEND}LongTimeFormat, ADValue.Value.AsDateTime);
  end;

begin
  case ADValue.ValueType of
{$IFDEF HAVE_ASNI_STRING}
    vdtStringA:
      Result := ADValue.Value.AsStringA^;
{$ENDIF}
    vdtString:
      Result := ADValue.Value.AsString^;
    vdtStringW:
      Result := ADValue.Value.AsStringW^;
    vdtUnset:
      Result := 'default';
    vdtNull:
      Result := 'null';
    vdtBoolean:
      Result := BoolToStr(ADValue.Value.AsBoolean, True);
    vdtSingle:
      Result := FloatToStr(ADValue.Value.AsSingle);
    vdtFloat:
      Result := FloatToStr(ADValue.Value.AsFloat);
    vdtInteger:
      Result := IntToStr(ADValue.Value.AsInteger);
    vdtInt64:
      Result := IntToStr(ADValue.Value.AsInt64);
    vdtCurrency:
      Result := CurrToStr(ADValue.Value.AsCurrency);
    vdtGuid:
      Result := GuidToString(ADValue.Value.AsGuid^);
    vdtDateTime:
      Result := DTToStr(ADValue);
    vdtObject:
      Result := Format('@@object[$%p]', [ADValue.Value.AsPointer]);
    vdtPtr:
      Result := Format('@@Ptr[$%p]', [ADValue.Value.AsPointer]);
    vdtInterface:
      Result := Format('@@Interface[$%p]', [ADValue.Value.AsInterface]);
    vdtStream:
      begin
        SetLength(lvHexStr, TMemoryStream(ADValue.Value.AsStream).Size * 2);
        lvHexStr := BinToHex(
          TMemoryStream(ADValue.Value.AsStream).Memory, TMemoryStream(ADValue.Value.AsStream).Size, False);

        Result := lvHexStr;
      end;
    vdtArray:
      Result := '@@Array';
  end;
end;
{$IFEND}

procedure DValueSetAsStringW(ADValue: PDRawValue; const pvString: DStringW);
begin
  CheckDValueSetType(ADValue, vdtStringW);
  ADValue.Value.AsStringW^ := pvString;
end;

function DValueGetAsStringW(ADValue:PDRawValue): DStringW;
var
  lvHexStr:DStringW;
  function DTToStr(ADValue: PDRawValue): DStringW;
  begin
    if Trunc(ADValue.Value.AsFloat) = 0 then
      Result := FormatDateTime({$IF RTLVersion>=22} FormatSettings.{$IFEND}LongTimeFormat, ADValue.Value.AsDateTime)
    else if IsZero(ADValue.Value.AsFloat - Trunc(ADValue.Value.AsFloat)) then
      Result := FormatDateTime
        ({$IF RTLVersion>=22}FormatSettings.{$IFEND}LongDateFormat,
        ADValue.Value.AsDateTime)
    else
      Result := FormatDateTime
        ({$IF RTLVersion>=22}FormatSettings.{$IFEND}LongDateFormat + ' ' +
{$IF RTLVersion>=22}FormatSettings.{$IFEND}LongTimeFormat, ADValue.Value.AsDateTime);
  end;

begin
  case ADValue.ValueType of
{$IF (not Defined(NEXTGEN))}
    vdtStringA:
      Result := ADValue.Value.AsStringA^;
{$IFEND}
    vdtString:
      Result := ADValue.Value.AsString^;
    vdtStringW:
      Result := ADValue.Value.AsStringW^;
    vdtUnset:
      Result := 'default';
    vdtNull:
      Result := 'null';
    vdtBoolean:
      Result := BoolToStr(ADValue.Value.AsBoolean, True);
    vdtSingle:
      Result := FloatToStr(ADValue.Value.AsSingle);
    vdtFloat:
      Result := FloatToStr(ADValue.Value.AsFloat);
    vdtInteger:
      Result := IntToStr(ADValue.Value.AsInteger);
    vdtInt64:
      Result := IntToStr(ADValue.Value.AsInt64);
    vdtCurrency:
      Result := CurrToStr(ADValue.Value.AsCurrency);
    vdtGuid:
      Result := GuidToString(ADValue.Value.AsGuid^);
    vdtDateTime:
      Result := DTToStr(ADValue);
    vdtObject:
      Result := Format('@@object[$%p]', [ADValue.Value.AsPointer]);
    vdtPtr:
      Result := Format('@@Ptr[$%p]', [ADValue.Value.AsPointer]);
    vdtInterface:
      Result := Format('@@Interface[$%p]', [ADValue.Value.AsInterface]);
    vdtStream:
      begin
        SetLength(lvHexStr, TMemoryStream(ADValue.Value.AsStream).Size * 2);
        lvHexStr := BinToHex(
          TMemoryStream(ADValue.Value.AsStream).Memory, TMemoryStream(ADValue.Value.AsStream).Size, False);

        Result := lvHexStr;
      end;
    vdtArray:
      Result := '@@Array';
  end;
end;

procedure DValueSetAsInt64(ADValue:PDRawValue; pvValue:Int64);
begin
  CheckDValueSetType(ADValue, vdtInt64);
  ADValue.Value.AsInt64 := pvValue;
end;

function DValueGetAsInteger(ADValue: PDRawValue): Integer;
begin
  case ADValue.ValueType of
    vdtInteger:
      Result := ADValue.Value.AsInteger;
    vdtInt64:
      Result := ADValue.Value.AsInt64;
    vdtUnset, vdtNull:
      Result := 0;
    vdtBoolean:
      Result := Integer(ADValue.Value.AsBoolean);
    vdtSingle:
      Result := Trunc(ADValue.Value.AsSingle);
    vdtFloat, vdtDateTime:
      Result := Trunc(ADValue.Value.AsFloat);
    vdtCurrency:
      Result := ADValue.Value.AsInt64 div 10000;
    vdtString:
      Result := StrToInt64(ADValue.Value.AsString^)
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      DValueTypeName[vdtInteger]]);
  end;
end;

function DValueGetAsInt64(ADValue: PDRawValue): Int64;
begin
  case ADValue.ValueType of
    vdtInt64:
      Result := ADValue.Value.AsInt64;
    vdtInteger:
      Result := ADValue.Value.AsInteger;
    vdtUnset, vdtNull:
      Result := 0;
    vdtBoolean:
      Result := Integer(ADValue.Value.AsBoolean);
    vdtSingle:
      Result := Trunc(ADValue.Value.AsSingle);
    vdtFloat, vdtDateTime:
      Result := Trunc(ADValue.Value.AsFloat);
    vdtCurrency:
      Result := ADValue.Value.AsInt64 div 10000;
    vdtString:
      Result := StrToInt64(ADValue.Value.AsString^)
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      DValueTypeName[vdtInt64]]);
  end;
end;

procedure DValueSetAsInteger(ADValue:PDRawValue; pvValue:Integer);
begin
  CheckDValueSetType(ADValue, vdtInteger);
  ADValue.Value.AsInt64 := pvValue;
  
end;

procedure DValueSetAsFloat(ADValue:PDRawValue; pvValue:Double);
begin
  CheckDValueSetType(ADValue, vdtFloat);
  ADValue.Value.AsFloat := pvValue;
end;

function DValueGetAsFloat(ADValue: PDRawValue): Double;
begin
  case ADValue.ValueType of
    vdtFloat, vdtDateTime:
      Result := ADValue.Value.AsFloat;
    vdtSingle:
      Result := ADValue.Value.AsSingle;
    vdtUnset, vdtNull:
      Result := 0;
    vdtBoolean:
      Result := Integer(ADValue.Value.AsBoolean);
    vdtInteger:
      Result := ADValue.Value.AsInteger;
    vdtInt64:
      Result := ADValue.Value.AsInt64;
    vdtCurrency:
      Result := ADValue.Value.AsCurrency;
    vdtString:
      Result := StrToFloat(ADValue.Value.AsString^)
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      DValueTypeName[vdtFloat]]);
  end;
end;

procedure DValueSetAsBoolean(ADValue:PDRawValue; pvValue:Boolean);
begin

  CheckDValueSetType(ADValue, vdtBoolean);
  ADValue.Value.AsBoolean := pvValue;
end;

function DValueGetAsBoolean(ADValue: PDRawValue): Boolean;
begin
  case ADValue.ValueType of
    vdtFloat, vdtDateTime:
      Result := not IsZero(ADValue.Value.AsFloat);
    vdtSingle:
      Result := not IsZero(ADValue.Value.AsSingle);
    vdtUnset, vdtNull:
      Result := false;
    vdtBoolean:
      Result := ADValue.Value.AsBoolean;
    vdtInteger:
      Result :=  ADValue.Value.AsInteger <> 0;
    vdtInt64:
      Result := ADValue.Value.AsInt64 <> 0;
    vdtCurrency:
      Result := not IsZero(ADValue.Value.AsCurrency);
    vdtString:
      Result := StrToBoolDef(ADValue.Value.AsString^, False)
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      DValueTypeName[vdtBoolean]]);
  end;
end;

function CompareDValue(pvDValue1: PDRawValue; pvDValue2:PDRawValue): Integer;
begin
  if pvDValue1.ValueType in [vdtInteger, vdtInt64] then
  begin
    Result := CompareValue(DValueGetAsInt64(pvDValue1), DValueGetAsInt64(pvDValue2));
  end else if pvDValue1.ValueType in [vdtSingle, vdtFloat] then
  begin
    Result := CompareValue(DValueGetAsFloat(pvDValue1), DValueGetAsFloat(pvDValue2));
  end else if pvDValue1.ValueType in [vdtBoolean] then
  begin
    Result := CompareValue(Ord(DValueGetAsBoolean(pvDValue1)), Ord(DValueGetAsBoolean(pvDValue2)));
  end else
  begin
    Result := CompareText(DValueGetAsString(pvDValue1), DValueGetAsString(pvDValue2));
  end;   
end;

procedure DValueSetAsString(ADValue: PDRawValue; const pvString: String);
begin
  CheckDValueSetType(ADValue, vdtString);
  ADValue.Value.AsString^ := pvString;
end;

function DValueGetAsString(ADValue:PDRawValue): string;
var
  lvHexStr:DStringW;
  function DTToStr(ADValue: PDRawValue): DStringW;
  begin
    if Trunc(ADValue.Value.AsFloat) = 0 then
      Result := FormatDateTime({$IF RTLVersion>=22} FormatSettings.{$IFEND}LongTimeFormat, ADValue.Value.AsDateTime)
    else if IsZero(ADValue.Value.AsFloat - Trunc(ADValue.Value.AsFloat)) then
      Result := FormatDateTime
        ({$IF RTLVersion>=22}FormatSettings.{$IFEND}LongDateFormat,
        ADValue.Value.AsDateTime)
    else
      Result := FormatDateTime
        ({$IF RTLVersion>=22}FormatSettings.{$IFEND}LongDateFormat + ' ' +
{$IF RTLVersion>=22}FormatSettings.{$IFEND}LongTimeFormat, ADValue.Value.AsDateTime);
  end;

begin
  case ADValue.ValueType of
{$IF (not Defined(NEXTGEN))}
    vdtStringA:
      Result := ADValue.Value.AsStringA^;
{$IFEND}  
    vdtString:
      Result := ADValue.Value.AsString^;
    vdtStringW:
      Result := ADValue.Value.AsStringW^;
    vdtUnset:
      Result := STRING_EMPTY;
    vdtNull:
      Result := STRING_EMPTY;
    vdtBoolean:
      Result := BoolToStr(ADValue.Value.AsBoolean, True);
    vdtSingle:
      Result := FloatToStr(ADValue.Value.AsSingle);
    vdtFloat:
      Result := FloatToStr(ADValue.Value.AsFloat);
    vdtInteger:
      Result := IntToStr(ADValue.Value.AsInteger);
    vdtInt64:
      Result := IntToStr(ADValue.Value.AsInt64);
    vdtCurrency:
      Result := CurrToStr(ADValue.Value.AsCurrency);
    vdtGuid:
      Result := GuidToString(ADValue.Value.AsGuid^);
    vdtDateTime:
      Result := DTToStr(ADValue);
    vdtStream:
      begin
        SetLength(lvHexStr, TMemoryStream(ADValue.Value.AsStream).Size * 2);
        lvHexStr := BinToHex(
          TMemoryStream(ADValue.Value.AsStream).Memory, TMemoryStream(ADValue.Value.AsStream).Size, False);

        Result := lvHexStr;
      end;
    vdtObject:
      Result := Format('@@object[$%p]', [ADValue.Value.AsPointer]);
    vdtPtr:
      Result := Format('@@Ptr[$%p]', [ADValue.Value.AsPointer]);
    vdtInterface:
      Result := Format('@@Interface[$%p]', [ADValue.Value.AsInterface]);
    vdtArray:
      Result := '@@Array';
  end;
end;

function DValueGetAsObject(ADValue:PDRawValue): TObject;
begin
  case ADValue.ValueType of
    vdtUnset, vdtNull:
      Result := nil;
    vdtObject:
      begin
        Result :=  TObject(ADValue.Value.AsPointer);
        {$IFDEF NEXTGEN}
        // �ƶ�ƽ̨��AData�ļ�����Ҫ���ӣ��Ա����Զ��ͷ�
        if Result <> nil then
        begin
          Result.__ObjAddRef;
        end;
        {$ENDIF}
      end;
    vdtPtr:
      case ADValue.Value.PtrReleaseAction of
        praNone, praObjectFree:  // ���ö��󣬻��߹����������ڵĶ���
          begin
            Result :=  TObject(ADValue.Value.AsPointer);
            {$IFDEF NEXTGEN}
            // �ƶ�ƽ̨��AData�ļ�����Ҫ���ӣ��Ա����Զ��ͷ�
            if Result <> nil then
            begin
              Result.__ObjAddRef;
            end;
            {$ENDIF}
          end;
        praDispose, praFreeMem:
          begin
            raise EConvertError.CreateFmt(SConvertError, ['memory pointer block',
              'Object']);
          end;
      else
        raise EConvertError.CreateFmt(SConvertError, ['unkown memory pointer block',
          'Object']);
      end;  
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      'Object']);
  end;
end;

procedure DValueSetAsInterface(ADValue: PDRawValue; const pvValue:
    IInterface);
begin
  if pvValue = nil then
  begin       // ���
    ClearDValue(ADValue);
  end else
  begin
    CheckDValueSetType(ADValue, vdtInterface);
    ADValue.Value.AsInterface^ := pvValue;
  end;
end;

function DValueGetAsInterface(ADValue:PDRawValue): IInterface;
var
  lvObj:TObject;
begin
  case ADValue.ValueType of
    vdtUnset, vdtNull:
      Result := nil;
    vdtInterface:
      Result :=  ADValue.Value.AsInterface^;
    vdtObject, vdtPtr:
      begin
        case ADValue.Value.PtrReleaseAction of
          praNone, praObjectFree:  // ���ö��󣬻��߹����������ڵĶ���
            begin
              lvObj :=TObject(ADValue.Value.AsPointer);
              {$IFDEF NEXTGEN}
              // �ƶ�ƽ̨��AData�ļ�����Ҫ���ӣ��Ա����Զ��ͷ�
              lvObj.__ObjAddRef;
              {$ENDIF}
              lvObj.GetInterface(IInterface, Result);
            end;
          praDispose, praFreeMem:
            begin
              raise EConvertError.CreateFmt(SConvertError, ['memory pointer block',
                'Interface']);
            end;
        else
          raise EConvertError.CreateFmt(SConvertError, ['unkown memory pointer block',
            'Interface']);
        end;
      end;  
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      DValueTypeName[vdtInterface]]);
  end;
end;



procedure DValueBindPointerData(ADValue:PDRawValue; pvData:Pointer;
    pvReleaseAction:TPtrReleaseAction);
begin
  if pvData = nil then
  begin       // ���
    ClearDValue(ADValue);
  end else
  begin
    CheckDValueSetType(ADValue, vdtPtr);
    ADValue.Value.AsPointer := pvData;
    ADValue.Value.PtrReleaseAction := pvReleaseAction;
  end;
end;

procedure DValueBindObjectData(ADValue:PDRawValue; pvData:TObject;
    pvReleaseAction:TPtrReleaseAction);
begin
  if pvData = nil then
  begin       // ���
    ClearDValue(ADValue);
  end else
  begin
    CheckDValueSetType(ADValue, vdtObject);
    ADValue.Value.AsPointer := pvData;
{$IFDEF NEXTGEN}
    // �ƶ�ƽ̨��AData�ļ�����Ҫ���ӣ��Ա����Զ��ͷ�
    pvData.__ObjAddRef;
{$ENDIF}
    ADValue.Value.PtrReleaseAction := pvReleaseAction;
  end;
end;

procedure RawValueCopyFrom(pvSource, pvDest: PDRawValue; pvIgnoreValueTypes:
    TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);
var
  i: Integer;
begin
  ClearDValue(pvDest);
  if pvSource.ValueType in pvIgnoreValueTypes then Exit;  
  if pvSource.ValueType <> vdtUnset then
  begin
    case pvSource.ValueType of
      vdtGuid:
        begin
          CheckDValueSetType(pvDest, vdtGuid);
          pvDest.Value.AsGuid^:= pvSource.Value.AsGuid^;
        end;
      vdtString:
        begin
          CheckDValueSetType(pvDest, vdtString);
          pvDest.Value.AsString^:= pvSource.Value.AsString^;
        end;
      vdtStringW:
        begin
          CheckDValueSetType(pvDest, vdtStringW);
          pvDest.Value.AsStringW^:= pvSource.Value.AsStringW^;
        end;
      vdtStream:
        begin
          CheckDValueSetType(pvDest, vdtStream);
          TMemoryStream(pvDest.Value.AsStream).LoadFromStream(
            TMemoryStream(pvSource.Value.AsStream));
        end;
      vdtInterface:
        begin
          CheckDValueSetType(pvDest, vdtInterface);
          pvDest.Value.AsInterface^:= pvSource.Value.AsInterface^;
        end;
      vdtObject:
        begin
          CheckDValueSetType(pvDest, vdtObject);
          pvDest.Value.AsPointer := pvSource.Value.AsPointer;
      {$IFDEF NEXTGEN}
          // �ƶ�ƽ̨��AData�ļ�����Ҫ���ӣ��Ա����Զ��ͷ�
          TObject(pvDest.Value.AsPointer).__ObjAddRef;
      {$ENDIF}
          pvDest.Value.PtrReleaseAction := praNone;   // �������ã������д���
        end;
      vdtPtr:
        begin
          CheckDValueSetType(pvDest, vdtPtr);
          pvDest.Value.AsPointer := pvSource.Value.AsPointer;
          pvDest.Value.PtrReleaseAction := praNone;   // �������ã������д���
        end;
      vdtArray:
        begin
          CheckDValueSetArrayLength(pvDest, pvSource.Value.ArrayLength);
          for i := 0 to pvSource.Value.ArrayLength -1 do
          begin
             RawValueCopyFrom(
                GetDValueItem(pvSource, i),
                GetDValueItem(pvDest, i));
          end;
        end;
    else
      begin
        pvDest.Value := pvSource.Value;
      end;
    end;
    pvDest.ValueType := pvSource.ValueType;
  end;
end;

procedure DValueSetAsUInt64(ADValue:PDRawValue; pvValue:UInt64);
begin
  CheckDValueSetType(ADValue, vdtUInt64);
  ADValue.Value.AsUInt64 := pvValue;
end;

function DValueGetAsUInt64(ADValue: PDRawValue): UInt64;
begin
  case ADValue.ValueType of
    vdtInt64:
      Result := UInt64(ADValue.Value.AsInt64);
    vdtUInt64:
      Result := UInt64(ADValue.Value.AsUInt64);
    vdtInteger:
      Result := UInt64(ADValue.Value.AsInteger);    
    vdtUnset, vdtNull:
      Result := 0;
    vdtBoolean:
      Result := UInt64(ADValue.Value.AsBoolean);
    vdtSingle:
      Result := UInt64(Trunc(ADValue.Value.AsSingle));
    vdtFloat, vdtDateTime:
      Result := UInt64(Trunc(ADValue.Value.AsFloat));
    vdtCurrency:
      Result := ADValue.Value.AsUInt64 div 10000;
    vdtString:
      Result := StrToInt64(ADValue.Value.AsString^)
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      DValueTypeName[vdtInt64]]);
  end;

end;

procedure DValueSetAsDateTime(ADValue:PDRawValue; pvValue:TDateTime);
begin
  CheckDValueSetType(ADValue, vdtDateTime);
  ADValue.Value.AsDateTime := pvValue;
end;

function DValueGetAsDateTime(ADValue: PDRawValue): TDateTime;
begin
  case ADValue.ValueType of
    vdtFloat, vdtDateTime:
      Result := ADValue.Value.AsDateTime;
    vdtSingle:
      Result := ADValue.Value.AsSingle;
    vdtUnset, vdtNull:
      Result := 0;
    vdtInteger:
      Result := ADValue.Value.AsInteger;
    vdtInt64:
      Result := ADValue.Value.AsInt64;
    vdtCurrency:
      Result := TDateTime(ADValue.Value.AsCurrency);
    vdtString:
      Result := StrToDateTime(ADValue.Value.AsString^)
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      DValueTypeName[vdtFloat]]);
  end;
end;

function CreateDValueObject: TDValue;
begin
  Result := TDValue.Create();
end;

procedure DisposeDValueObject(aVal:TDValue);
begin
  FreeObject(aVal);
end;

 function CreateDValueObject(pvType: TDValueObjectType): TDValue; overload;
 begin
   Result := TDValue.Create(pvType);
 end;

function GetDValueObjectCount: Integer;
begin
  {$IFDEF DEBUG}
  Result := __create_cnt - __destroy_cnt;
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

function GetDValuePrintDebugInfo: String;
begin
  {$IFDEF USE_DVALUE_POOL}
  if Assigned(__valPool) then
    Result := GetValPoolDebugInfo(__valPool)
  else
    Result := STRING_EMPTY;
  {$ELSE}
  Result := STRING_EMPTY;
  {$ENDIF}
end;

function DValueIsEmpty(ADValue:PDRawValue): Boolean;
begin
  if (ADValue.ValueType in [vdtUnset, vdtNull]) then
  begin
    Result := True;
  end else if (ADValue.ValueType in [vdtString]) then
  begin
    Result := Length(ADValue.Value.AsString^) = 0;
  end else if (ADValue.ValueType in [vdtStringW]) then
  begin
    Result := Length(ADValue.Value.AsStringW^) = 0;
  {$IFDEF HAVE_ASNI_STRING}
  end else if (ADValue.ValueType in [vdtStringA]) then
  begin
    Result := Length(ADValue.Value.AsStringA^) = 0;
 {$ENDIF}
  end else if (ADValue.ValueType in [vdtDateTime]) then
  begin
    Result := ADValue.Value.AsDateTime = 0;
  end else if (ADValue.ValueType in [vdtObject, vdtPtr]) then
  begin
    Result := ADValue.Value.AsPointer = nil;
  end else if (ADValue.ValueType in [vdtInterface]) then
  begin
    Result := ADValue.Value.AsInterface^ = nil;
  end else
  begin
    Result := false;
  end;
end;

destructor TDValueObject.Destroy;
begin
  ClearDValue(@FRawValue);
  inherited;
end;

function TDValueObject.GetAsBoolean: Boolean;
begin
  Result := DValueGetAsBoolean(@FRawValue);
end;

function TDValueObject.GetAsFloat: Double;
begin
  Result := DValueGetAsFloat(@FRawValue);
end;

function TDValueObject.GetAsInetger: Int64;
begin
  Result := DValueGetAsInt64(@FRawValue);
end;

function TDValueObject.GetAsString: String;
begin
  Result := DValueGetAsString(@FRawValue);
end;

function TDValueObject.GetDataType: TDValueDataType;
begin
  Result := FRawValue.ValueType;
end;

procedure TDValueObject.SetAsBoolean(const Value: Boolean);
begin
  DValueSetAsBoolean(@FRawValue, Value);
end;

procedure TDValueObject.SetAsFloat(const Value: Double);
begin
  DValueSetAsFloat(@FRawValue, Value);
end;

procedure TDValueObject.SetAsInetger(const Value: Int64);
begin
  DValueSetAsInt64(@FRawValue, Value);
end;

procedure TDValueObject.SetAsString(const Value: String);
begin
  DValueSetAsString(@FRawValue, Value);
end;

function TDValueList.Add(pvValueName:String): TDValueObject;
begin
  if FindByName(pvValueName) <> nil then
    raise Exception.CreateFmt(SItemExists, [pvValueName]);

  Result := InnerAdd(pvValueName);
end;

procedure TDValueList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    TObject(FList[i]).Free;
  end;
  FList.Clear;
end;

constructor TDValueList.Create;
begin
  inherited Create;
{$IFDEF HAVE_GENERICS}
  FList := TList<TDValueObject>.Create;
{$ELSE}
  FList := TList.Create;
{$ENDIF}

end;

destructor TDValueList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TDValueList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDValueList.GetItems(pvIndex: Integer): TDValueObject;
begin
  Result :=TDValueObject(FList[pvIndex]);
end;

function TDValueList.FindByName(const pvValueName: string): TDValueObject;
var
  i:Integer;
  lvItem:TDValueObject;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    lvItem := TDValueObject(FList[i]);
    if SameText(lvItem.Name, pvValueName)  then
    begin
      Result := lvItem;
      Break;    
    end;
  end;
end;

function TDValueList.ForceByName(const pvValueName: String): TDValueObject;
begin
  Result := FindByName(pvValueName);
  if Result = nil then Result := InnerAdd(pvValueName);
end;

function TDValueList.InnerAdd(const pvValueName: string): TDValueObject;
begin
  Result := TDValueObject.Create;
  Result.Name := pvValueName;
  FList.Add(Result);
end;

function TDValueList.ParamByName(const pvValueName: String): TDValueObject;
begin
  Result := FindByName(pvValueName);
  if Result = nil then
  begin
    Raise Exception.CreateFmt(SItemNotFound, [pvValueName]);
  end;
end;

procedure TDValue.ClearChildren;
var
  i: Integer;
begin
  if Assigned(FChildren) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      try
        FLastMsg := Format('��������[%d/%d]:%s', [i, FChildren.Count, TObject(FChildren[i]).ClassName]);
      except
        FLastMsg := Format('��������[%d/%d],�޷���ȡ����', [i, FChildren.Count]);
      end;
      TDValueItem(FChildren[i]).Free;
    end;
    FChildren.Clear;
  end;
end;

constructor TDValue.Create(pvType: TDValueObjectType);
begin
  inherited Create;
  {$IFDEF DEBUG}
  AtomicIncrement(__create_cnt);
  {$ENDIF}
  {$IFDEF CHECK_DVALUE}
  __InitalizeCheckValue;
  {$ENDIF}
  FObjectType := vntNull;
  CreateName;
  FValue := TDValueItem.Create;

  CheckSetNodeType(pvType);
end;

constructor TDValue.Create;
begin
  inherited;
  {$IFDEF DEBUG}
  AtomicIncrement(__create_cnt);
  {$ENDIF}
  {$IFDEF CHECK_DVALUE}
  __InitalizeCheckValue;
  {$ENDIF}
  FObjectType := vntNull;
  CreateName;
  FValue := TDValueItem.Create;
  CheckSetNodeType(vntObject);


end;

procedure TDValue.CreateName;
begin
  if not Assigned(FName) then FName := TDValueItem.Create;
end;

procedure TDValue.DeleteName;
begin
  if Assigned(FName) then
  begin
    FName.Free;
    FName := nil;
  end;    
end;

destructor TDValue.Destroy;
begin
  {$IFDEF DEBUG}
  AtomicIncrement(__destroy_cnt);
  {$ENDIF}

  if Assigned(FChildren) then
  begin
    ClearChildren();
    FChildren.Free;
    FChildren := nil;
  end;

  if Assigned(FValue) then FValue.Free;
  DeleteName;

  {$IFDEF CHECK_DVALUE}
  __CheckValueOK();
  FillChar(__checkvalue[0], length(__checkvalue), 0);
  __objflag := $A;
  {$ENDIF}  
  inherited;
end;

function TDValue.Add: TDValue;
begin
  CheckBeforeAddChild(vntObject);
  Result := TDValue.Create(vntValue);
  Result.FParent := Self;
  FChildren.Add(Result);
end;

function TDValue.Add(const pvName: String): TDValue;
begin
  CheckSetNodeType(vntObject);
  Result := TDValue.Create(vntValue);
  Result.FParent := Self;
  Result.FName.AsString := pvName;
  FChildren.Add(Result);
end;

function TDValue.Add(const pvName, pvValue: string): TDValue;
begin
  CheckSetNodeType(vntObject);
  Result := TDValue.Create(vntValue);
  Result.FParent := Self;
  Result.FName.AsString := pvName;
  Result.AsString := pvValue;
  FChildren.Add(Result);
end;

function TDValue.Add(const pvName: string; pvValue: Integer): TDValue;
begin
  CheckSetNodeType(vntObject);
  Result := TDValue.Create(vntValue);
  Result.FParent := Self;
  Result.FName.AsString := pvName;
  Result.AsInteger := pvValue;
  FChildren.Add(Result);
end;

function TDValue.Add(const pvName: string; pvValue: Boolean): TDValue;
begin
  CheckSetNodeType(vntObject);
  Result := TDValue.Create(vntValue);
  Result.FParent := Self;
  Result.FName.AsString := pvName;
  Result.AsBoolean := pvValue;
  FChildren.Add(Result);
end;

function TDValue.Add(const pvName: string; pvValue: Double): TDValue;
begin
  CheckSetNodeType(vntObject);
  Result := TDValue.Create(vntValue);
  Result.FParent := Self;
  Result.FName.AsString := pvName;
  Result.AsFloat := pvValue;
  FChildren.Add(Result);
end;

function TDValue.Add(const pvName: string; pvValue: TDValue): TDValue;
begin
  Result := nil;
  if pvValue = nil then Exit;
  CheckSetNodeType(vntObject);

  Result := TDValue.Create();
  Result.CloneValueFrom(pvValue);
  Result.FParent := Self;
  Result.FName.AsString := pvName;
  FChildren.Add(Result);  
end;


function TDValue.Add(const pvName: String; pvType: TDValueObjectType): TDValue;
begin
  CheckSetNodeType(vntObject);
  Result := TDValue.Create(pvType);
  Result.FParent := Self;
  Result.FName.AsString := pvName;
  FChildren.Add(Result);
end;

function TDValue.AddArrayChild: TDValue;
begin
  CheckSetNodeType(vntArray);
  Result := TDValue.Create(vntValue);
  Result.FParent := Self;
  FChildren.Add(Result);
end;

procedure TDValue.AddArrayChild(pvDValue: TDValue);
begin
  CheckSetNodeType(vntArray);
  pvDValue.FParent := Self;
  FChildren.Add(pvDValue);
end;

function TDValue.AddVar(const pvName: string; const pvValue: Variant): TDValue;
begin
  Result := Add(pvName);
  Result.SetAsVariant(pvValue);
end;

function TDValue.AsArray: TDValue;
begin
  CheckSetNodeType(vntArray);
  Result := self;
end;

procedure TDValue.AttachDValue(const pvName: String; pvDValue: TDValue);
begin
  CheckSetNodeType(vntObject);
  pvDValue.FParent := Self;
  pvDValue.FName.AsString := pvName;
  FChildren.Add(pvDValue);    
end;

procedure TDValue.Base64LoadFromFile(pvFileName: String);
var
  lvFileStream:TFileStream;
begin
  lvFileStream := TFileStream.Create(pvFileName, fmOpenRead or fmShareDenyWrite);
  try
    Base64LoadFromStream(lvFileStream);
  finally
    lvFileStream.Free;
  end;
end;

procedure TDValue.Base64LoadFromStream(pvInStream: TStream);
begin
  SetAsString(Base64Encode(pvInStream, 0));
end;

procedure TDValue.Base64SaveToFile(pvFileName: String);
var
  lvFileStream:TFileStream;
begin
  lvFileStream := TFileStream.Create(pvFileName, fmCreate);
  try
    Base64SaveToStream(lvFileStream);
  finally
    lvFileStream.Free;
  end;
end;

procedure TDValue.Base64SaveToStream(pvOutStream: TStream);
begin
  Base64Decode(GetAsString, pvOutStream);  
end;

procedure TDValue.BindObject(pvObject: TObject; pvFreeAction: TObjectFreeAction
    = faFree);
begin
  FValue.BindObject(pvObject, pvFreeAction);
end;

procedure TDValue.CheckBeforeAddChild(pvType: TDValueObjectType);
begin
  if not (FObjectType in [vntObject, vntArray]) then CheckSetNodeType(pvType);
end;

procedure TDValue.CheckCreateChildren;
begin
  if not Assigned(FChildren) then
  begin
    {$IFDEF HAVE_GENERICS}
      FChildren := TList<TDValue>.Create;
    {$ELSE}
      FChildren := TList.Create;
    {$ENDIF} 
  end;
end;

function TDValue.GetCount: Integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.Count
  else
  begin
    Result := 0;
  end;
end;

function TDValue.ItemByName(const pvName: string): TDValue;
begin
  Result := FindByName(pvName);
  if Result = nil then raise TDValueException.CreateFmt(SItemNotFound, [pvName]);
end;

function TDValue.CheckSetNodeType(pvType:TDValueObjectType): TDValue;
begin
  {$IFDEF CHECK_DVALUE}
  __CheckValueOK();
  {$ENDIF}
  DoLastModify;
  if pvType <> FObjectType then
  begin
    if not (FObjectType in [vntNull]) then
    begin
      ClearChildren;
    end;
    
    if pvType in [vntObject, vntArray] then
    begin
      CheckCreateChildren;
    end else if pvType = vntValue then
    begin 
      if not Assigned(FName) then FName := TDValueItem.Create;
      if not Assigned(FValue) then FValue := TDValueItem.Create;
    end;

    FObjectType := pvType;
  end;
  Result := Self;
end;

procedure TDValue.Clear;
var
  lvDebug:String;
begin
  {$IFDEF CHECK_DVALUE}
  __CheckValueOK();
  {$ENDIF}
  try
    FLastMsg := '';
    lvDebug := 'ClearChildren';
    ClearChildren;
    lvDebug := 'Value.Clear';
    FValue.Clear;
  except
    on e:Exception do
    begin
      raise Exception.CreateFmt('%s(%s):%s:%s', [e.ClassName, FLastMsg, lvDebug, e.Message]);
    end;
  end;
end;

function TDValue.ClearLastModify(pvTimeOut:Cardinal = 30000): Integer;
var
  i: Integer;
  lvNow:Cardinal;
begin
  Result := 0;
  lvNow := GetTickCount;
  for i := Count - 1 downto 0 do
  begin
    if tick_diff(Items[i].FLastModify, lvNow) >= pvTimeOut then
    begin
      Delete(i);
      Inc(Result);
    end;    
  end;
end;

function TDValue.Clone(pvIgnoreValueTypes: TDValueDataTypes = [vdtInterface,
    vdtObject, vdtPtr]): TDValue;
begin
  Result := TDValue.Create();
  try
    Result.CloneFrom(Self, pvIgnoreValueTypes);
  except
    Result.Free;
    raise;
  end;
end;

procedure TDValue.CloneFrom(pvSource: TDValue; pvIgnoreValueTypes:
    TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);
var
  i: Integer;
  lvDValue:TDValue;
begin
  self.Clear;
  CheckSetNodeType(pvSource.ObjectType);

  FName.CloneFrom(pvSource.FName);
  FValue.CloneFrom(pvSource.FValue, pvIgnoreValueTypes);
  for i := 0 to pvSource.Count -1 do
  begin
    lvDValue := TDValue.Create(vntValue);
    lvDValue.FParent := Self;
    FChildren.Add(lvDValue);
    lvDValue.CloneFrom(pvSource[i], pvIgnoreValueTypes);
  end;
end;

procedure TDValue.CloneValueFrom(pvSource: TDValue; pvIgnoreValueTypes:
    TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);
var
  i: Integer;
  lvDValue:TDValue;
begin
  ClearChildren;
  CheckSetNodeType(pvSource.ObjectType);

  FValue.CloneFrom(pvSource.FValue, pvIgnoreValueTypes);
  for i := 0 to pvSource.Count -1 do
  begin
    lvDValue := TDValue.Create(vntValue);
    lvDValue.FParent := Self;
    FChildren.Add(lvDValue);
    lvDValue.CloneFrom(pvSource[i], pvIgnoreValueTypes);
  end;

end;

procedure TDValue.MergeValue(pvSource:TDValue; pvIgnoreValueTypes:
    TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);
var
  i: Integer;
  lvItem:TDValue;
begin
  if (pvSource.FObjectType in [vntArray, vntValue]) then
  begin
    Self.CloneValueFrom(pvSource, pvIgnoreValueTypes);
  end else if (pvSource.FObjectType in [vntObject]) then
  begin
    CheckSetNodeType(pvSource.ObjectType);
    for i := 0 to pvSource.Count - 1 do
    begin
      lvItem := pvSource[i];
      if not (lvItem.Value.DataType  in pvIgnoreValueTypes) then
      begin
        Self.ForceByName(lvItem.Name.AsString).MergeValue(lvItem, pvIgnoreValueTypes);
      end;
    end;
  end else
  begin
    Self.CloneValueFrom(pvSource, pvIgnoreValueTypes);
  end;
end;


procedure TDValue.Delete(pvIndex:Integer);
begin
  TDValue(FChildren[pvIndex]).Free;
  FChildren.Delete(pvIndex);
end;

function TDValue.Delete(const pvName: String): Integer;
begin
  Result := IndexOf(pvName);
  if Result >= 0 then
  begin
    Delete(Result);
  end;
end;

function TDValue.FindByName(const pvName: String): TDValue;
var
  i:Integer;
begin
  i := IndexOf(pvName);
  if i = -1 then Result := nil else Result := Items[i];
end;

function TDValue.FindByName(pvName: Integer): TDValue;
var
  i:Integer;
begin
  i := IndexOf(pvName);
  if i = -1 then Result := nil else Result := Items[i];
end;

function TDValue.FindByPath(const pvPath: string): TDValue;
var
  lvParent:TDValue;
  j:Integer;
begin
  Result := InnerFindByPath(pvPath, lvParent, j);
end;

function TDValue.ForceByName(const pvName: string): TDValue;
begin
  Result := FindByName(pvName);
  if Result = nil then
  begin
    CheckSetNodeType(vntObject);
    Result := TDValue.Create(vntValue);
    Result.FName.AsString := pvName;
    Result.FParent := Self;
    FChildren.Add(Result);
  end;
end;

function TDValue.ForceByName(pvName:Integer): TDValue;
begin
  Result := FindByName(pvName);
  if Result = nil then
  begin
    CheckSetNodeType(vntObject);
    Result := TDValue.Create(vntValue);
    Result.FName.AsInteger := pvName;
    Result.FParent := Self;
    FChildren.Add(Result);
  end;
end;

function TDValue.ForceByPath(const pvPath: String): TDValue;
var
  lvName:string;
  s:string;
  sPtr:PChar;
  lvParent:TDValue;
begin
  Result := nil;
  s := pvPath;

  lvParent := Self;
  sPtr := PChar(s);
  while sPtr^ <> #0 do
  begin
    lvName := GetFirst(sPtr, Path_SplitChars);
    if lvName = '' then
    begin
      Break;
    end else
    begin
      if sPtr^ = #0 then
      begin           // end
        Result := lvParent.ForceByName(lvName);
      end else
      begin
        // find or create childrean
        lvParent := lvParent.ForceByName(lvName);
      end;
    end;
    if sPtr^ = #0 then Break;
    Inc(sPtr);
  end;
end;




function TDValue.GetAsBoolean: Boolean;
begin
  Result := FValue.GetAsBoolean;
end;

function TDValue.GetAsFloat: Double;
begin
  Result := FValue.GetAsFloat;
end;

function TDValue.GetAsInteger: Int64;
begin
  Result := FValue.GetAsInteger;
end;

function TDValue.GetAsInterface: IInterface;
begin
  Result := FValue.GetAsInterface;
end;

function TDValue.GetAsObject: TObject;
begin
  Result := FValue.GetAsObject;  
end;

function TDValue.GetAsStream: TMemoryStream;
begin
  Result := FValue.AsStream;
end;

function TDValue.GetAsString: String;
begin
  Result := FValue.GetAsString;
end;

function TDValue.GetAsStringW: DStringW;
begin
  Result := FValue.AsStringW
end;

function TDValue.GetAsUInt: UInt64;
begin
  Result := FValue.AsUInt;
end;

function TDValue.GetFloatValueByName(pvName: String; pvDefault: Double): Double;
var
  lvItem:TDValue;
begin
  lvItem := FindByName(pvName);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsFloat;
  end;
end;

function TDValue.GetValueByName(const pvName: String; pvDefault: Boolean):
    Boolean;
var
  lvItem:TDValue;
begin
  lvItem := FindByName(pvName);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsBoolean;
  end;
end;

function TDValue.GetIntValueByName(pvName: String; pvDefault: Int64): Int64;
var
  lvItem:TDValue;
begin
  lvItem := FindByName(pvName);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsInteger;
  end;
end;

function TDValue.GetItems(pvIndex: Integer): TDValue;
begin
  Result := TDValue(FChildren[pvIndex]);
end;

function TDValue.GetStrValueByName(const pvName, pvDefault: string): String;
var
  lvItem:TDValue;
begin
  lvItem := FindByName(pvName);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsString;
  end;
end;

function TDValue.GetValueByName(const pvName: String; pvDefault: Double):
    Double;
var
  lvItem:TDValue;
begin
  lvItem := FindByName(pvName);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsFloat;
  end;
end;

function TDValue.GetValueByPath(const pvPath: string; pvDefault: Int64): Int64;
var
  lvItem:TDValue;
begin
  lvItem := FindByPath(pvPath);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsInteger;
  end;

end;

function TDValue.GetValueByPath(const pvPath: string; pvDefault: Boolean):
    Boolean;
var
  lvItem:TDValue;
begin
  lvItem := FindByPath(pvPath);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsBoolean;
  end;

end;

function TDValue.GetValueByName(const pvName: String; pvDefault: Int64): Int64;
var
  lvItem:TDValue;
begin
  lvItem := FindByName(pvName);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsInteger;
  end;
end;

function TDValue.GetValueByName(const pvName, pvDefault: string): String;
var
  lvItem:TDValue;
begin
  lvItem := FindByName(pvName);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsString;
  end;


end;

function TDValue.GetValueByName(const pvName: String; pvDefault: TObject):
    TObject;
var
  lvItem:TDValue;
begin
  lvItem := FindByName(pvName);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsObject;
  end;
end;

function TDValue.GetValueByPath(const pvPath: string; pvDefault: Double):
    Double;
var
  lvItem:TDValue;
begin
  lvItem := FindByPath(pvPath);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsFloat;
  end;

end;

function TDValue.GetValueByPath(const pvPath: string; pvDefault: string):
    string;
var
  lvItem:TDValue;
begin
  lvItem := FindByPath(pvPath);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsString;
  end;
end;

function TDValue.GetValueByPath(const pvPath: string; pvDefault: TObject):
    TObject;
var
  lvItem:TDValue;
begin
  lvItem := FindByPath(pvPath);
  if lvItem = nil then
  begin
    Result := pvDefault;
  end else
  begin
    Result := lvItem.AsObject;
  end;
end;

function TDValue.IndexDataOf(pvData:Pointer): Integer;
var
  lvCount, j:Integer;
  lvItem:PDRawValue;
begin
  lvCount := Count;
  Result := -1;
  for j := 0 to lvCount - 1 do
  begin
    lvItem := @GetItems(j).FValue.FRawValue;
    if lvItem.ValueType = vdtPtr then
    begin
      if lvItem.Value.AsPointer = pvData then
      begin
        Result := j;
        Break;
      end;
    end else if lvItem.ValueType = vdtObject then
    begin
      if lvItem.Value.AsPointer = pvData then
      begin
        Result := j;
        Break;
      end;
    end;
  end;


end;

function TDValue.IndexOf(const pvName: string): Integer;
var
  i:Integer;
  s:string;
begin
  Result := -1;
  if Assigned(FChildren) then
    for i := 0 to FChildren.Count - 1 do
    begin
      s := Items[i].FName.AsString;
      if CompareText(s, pvName) = 0 then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TDValue.IndexOf(pvName: Integer): Integer;
var
  i:Integer;
begin
  Result := -1;
  if Assigned(FChildren) then
    for i := 0 to FChildren.Count - 1 do
    begin
      if Items[i].FName.DataType in [vdtInt64, vdtInteger] then
      begin
        if Items[i].FName.AsInteger = pvName then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
end;

function TDValue.InnerFindByPath(pvPath: string; var vParent:TDValue;
    var vIndex: Integer): TDValue;
var
  lvName:string;
  s:string;
  sPtr:PChar;
  lvTempObj, lvParent:TDValue;
  j:Integer;
begin
  s := pvPath;

  Result := nil;

  lvParent := Self;
  sPtr := PChar(s);
  while sPtr^ <> #0 do
  begin
    lvName := GetFirst(sPtr, ['.', '/','\']);
    if lvName = '' then
    begin
      Break;
    end else
    begin
      if sPtr^ = #0 then
      begin           // end
        j := lvParent.IndexOf(lvName);
        if j <> -1 then
        begin
          Result := lvParent.Items[j];
          vIndex := j;
          vParent := lvParent;
        end else
        begin
          Break;
        end;
      end else
      begin
        // find childrean
        lvTempObj := lvParent.FindByName(lvName);
        if lvTempObj = nil then
        begin
          Break;
        end else
        begin
          lvParent := lvTempObj;
        end;
      end;
    end;
    if sPtr^ = #0 then Break;
    Inc(sPtr);
  end;
end;


procedure TDValue.RemoveAll;
begin
  ClearChildren();
end;

function TDValue.RemoveByName(const pvName: String): Integer;
begin
  Result := IndexOf(pvName);
  if Result >= 0 then
  begin
    Delete(Result);
  end;
end;

function TDValue.RemoveByPath(const pvPath: String): Boolean;
var
  lvParent, lvDValue:TDValue;
  lvIndex:Integer;
begin
  Result := false;
  lvIndex := -1;
  InnerFindByPath(pvPath, lvParent, lvIndex);
  if lvIndex <> -1 then
  begin
    lvParent.Delete(lvIndex);
    Result := True;

    if lvParent.Count = 0 then
    begin
      lvDValue := lvParent;

      // 1.û���Ӽ�
      // 2.�����ڱ���
      while (lvDValue <> nil) and (lvDValue.Count <= 1) and (lvDValue <> Self)  do
      begin
        lvParent := lvDValue.Parent;
        lvDValue.RemoveFromParent;
        lvDValue := lvParent;
      end;
    end;
  end;
end;


function TDValue.RemoveFromParent: Boolean;
begin
  Result := UnAttachFromParent;
  if Result then Self.Free;
end;

{$IF (not Defined(NEXTGEN))}
function TDValue.GetAsAnsiString: AnsiString;
begin
  Result := FValue.GetAsStringA;
end;

procedure TDValue.SetAsAnsiString(const Value: AnsiString);
begin
  CheckSetNodeType(vntValue);
  FValue.SetAsStringA(Value);
end;
{$IFEND}

procedure TDValue.SetAsBoolean(const Value: Boolean);
begin
  CheckSetNodeType(vntValue);
  FValue.SetAsBoolean(Value);
end;

procedure TDValue.SetAsFloat(const Value: Double);
begin
  CheckSetNodeType(vntValue);
  FValue.SetAsFloat(Value);
end;

procedure TDValue.SetAsInteger(const Value: Int64);
begin
  CheckSetNodeType(vntValue);
  FValue.SetAsInteger(Value);
end;

procedure TDValue.SetAsInterface(const Value: IInterface);
begin
  CheckSetNodeType(vntValue);
  FValue.SetAsInterface(Value);
end;

procedure TDValue.SetAsString(const Value: String);
begin
  CheckSetNodeType(vntValue);
  FValue.SetAsString(Value);
end;

procedure TDValue.SetAsStringW(const Value: DStringW);
begin
  CheckSetNodeType(vntValue);
  FValue.AsStringW := Value;
end;

procedure TDValue.SetAsUInt(const Value: UInt64);
begin
  CheckSetNodeType(vntValue);
  FValue.SetAsUInt(Value);
end;

procedure TDValue.SetAsVariant(const pvValue: Variant);
var
  lvVarType:TVarType;
begin
  lvVarType := VarType(pvValue);
  case lvVarType of
    varSmallInt, varInteger, varShortInt: SetAsInteger(pvValue);
    varSingle, varDouble: SetAsFloat(pvValue);
    varDate: SetAsDateTime(pvValue);
    varBoolean: SetAsBoolean(pvValue);
  else
    SetAsString(pvValue);
  end;
end;

procedure TDValue.DoLastModify;
begin
  FLastModify := GetTickCount;
  if Parent <> nil then
  begin
    Parent.DoLastModify;
  end;
end;

function TDValue.FindChild(const Key:string; pvKeyVal:string): TDValue;
var
  i:Integer;
  s1 : string;
begin
  Result := nil;
  if Assigned(FChildren) then
    for i := 0 to FChildren.Count - 1 do
    begin
      s1 := Items[i].GetValueByName(Key, STRING_EMPTY);
      if CompareText(s1, pvKeyVal) = 0 then
      begin
        Result := Items[i];
        Break;
      end;
    end;
end;

function TDValue.GetAsDateTime: TDateTime;
begin
  Result := FValue.AsDateTime;
end;

function TDValue.IsEmpty: Boolean;
begin
  Result := FValue.IsEmpty; 
end;

function TDValue.IsNull: Boolean;
begin
  Result := FValue.FRawValue.ValueType in [vdtUnset, vdtNull];
end;

procedure TDValue.SetAsDateTime(const Value: TDateTime);
begin
  CheckSetNodeType(vntValue);
  FValue.SetAsDateTime(Value);
end;

function TDValue.SizeOf: Integer;
var
  i: Integer;
begin
  
  {$IFDEF UNICODE}
  Result := Length(FName.AsString) shl 1;
  {$ELSE}
  Result := Length(FName.AsString);
  {$ENDIF}
  if FObjectType in [vntArray, vntObject] then
  begin
    for i := 0 to self.Count - 1 do
    begin
      Inc(Result, self.Items[i].SizeOf);
    end;
  end else
  begin
    Inc(Result, self.Value.SizeOf);
  end;
  
end;

{$IFDEF HAVE_GENERICS}
procedure TDValue.Sort(const Compare: IComparer<TDValue>);
begin
  FChildren.Sort(Compare);
end;
{$ELSE}
procedure TDValue.Sort(Compare: TListSortCompare);
begin
  FChildren.Sort(Compare);
end;
{$ENDIF}



function TDValue.ToStrings(pvNameSpliter: String = '='; pvPreNameFix: string =
    STRING_EMPTY; pvValueDelimiter: string = sLineBreak): String;
var
  i: Integer;
begin
  Result := STRING_EMPTY;

  if self.ObjectType = vntArray then
  begin
    for i := 0 to Count - 1 do
    begin
      Result := Result + Items[i].AsString + pvValueDelimiter;
    end;
  end else
  begin
    for i := 0 to Count - 1 do
    begin
      Result := Result + pvPreNameFix + Items[i].Name.AsString + pvNameSpliter + Items[i].AsString + pvValueDelimiter;
    end;
  end;
end;

function TDValue.UnAttach(pvIndex:Integer): TDValue;
begin
  Result := TDValue(FChildren[pvIndex]);
  Result.FParent := nil;
  FChildren.Delete(pvIndex);
end;

function TDValue.UnAttach(pvName:String): TDValue;
var
  j:Integer;
begin
  j := IndexOf(pvName);
  if j <> -1 then
    Result := UnAttach(j)
  else
    Result := nil;
end;

function TDValue.UnAttachFromParent: Boolean;
var
  i:Integer;
begin
  Result := False;
  if Parent <> nil then
  begin
    i := Parent.FChildren.IndexOf(Self);
    if i > -1 then
    begin
      Parent.UnAttach(i);
      Result := True;
    end;
  end;
end;

{$IFDEF CHECK_DVALUE}
procedure TDValue.__CheckValueOK;
var
  lvTick1, lvTick2:PCardinal;
begin
  Assert(self<>nil, '������δ����');
  Assert(__objflag=$DA, '������δ����,�����Ѿ�����');

  
  lvTick1 := PCardinal(@__checkvalue[0]);
  lvTick2 := PCardinal(@__checkvalue[4]);
  Assert((lvTick1^ > 0) and (lvTick1^ = lvTick2^), '�����⵽�����Ѿ��ͷ�');
end;

procedure TDValue.__InitalizeCheckValue;
var
  lvTick1, lvTick2:PCardinal;
begin
  __objflag := $DA;
  lvTick1 := PCardinal(@__checkvalue[0]);
  lvTick2 := PCardinal(@__checkvalue[4]);
  lvTick1^ := GetTickCount;
  lvTick2^ := lvTick1^;
end;

{$ENDIF}

destructor TDValueItem.Destroy;
begin
  ClearDValue(@FRawValue);
{$IFDEF CHECK_DVALUE}
  __objflag := $A;
{$ENDIF}
  inherited;
end;

procedure TDValueItem.BindObject(pvObject: TObject; pvFreeAction:
    TObjectFreeAction = faFree);
begin
  case pvFreeAction of
    faNone: DValueBindObjectData(@FRawValue, pvObject, praNone);
    faFree: DValueBindObjectData(@FRawValue, pvObject, praObjectFree);
  end;
end;

procedure TDValueItem.Clear;
begin
  ClearDValue(@FRawValue);
end;

procedure TDValueItem.CloneFrom(pvSource: TDValueItem; pvIgnoreValueTypes:
    TDValueDataTypes = [vdtInterface, vdtObject, vdtPtr]);
begin
  RawValueCopyFrom(@pvSource.FRawValue, @FRawValue, pvIgnoreValueTypes);
end;

constructor TDValueItem.Create;
begin
{$IFDEF CHECK_DVALUE}
  __objflag := $DA;
{$ENDIF}

end;

function TDValueItem.Equal(pvItem:TDValueItem): Boolean;
begin
  Result := CompareDValue(@FRawValue, @pvItem.FRawValue) = 0;
end;

function TDValueItem.GetItems(pvIndex: Integer): TDValueItem;
var
  lvObj:TObject;
begin
  if DataType <> vdtArray then
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[DataType],
      DValueTypeName[vdtArray]]);




  lvObj := DValueGetAsObject(GetDValueItem(@FRawValue, pvIndex));
  Result := TDValueItem(lvObj);
end;

function TDValueItem.GetSize: Integer;
begin
  if FRawValue.ValueType <> vdtArray then Result := 0
  else Result := FRawValue.Value.ArrayLength;
end;

function TDValueItem.GetAsBoolean: Boolean;
begin
  Result := DValueGetAsBoolean(@FRawValue);
end;

function TDValueItem.GetAsDateTime: TDateTime;
begin
  Result := DValueGetAsDateTime(@FRawValue);
end;

procedure TDValueItem.SetAsDateTime(const Value: TDateTime);
begin
  DValueSetAsDateTime(@FRawValue, Value);
end;

function TDValueItem.GetAsFloat: Double;
begin
  Result := DValueGetAsFloat(@FRawValue);
end;

function TDValueItem.GetAsInteger: Int64;
begin
  Result := DValueGetAsInt64(@FRawValue);
end;

function TDValueItem.GetAsInterface: IInterface;
begin
  Result := DValueGetAsInterface(@FRawValue);
end;

function TDValueItem.GetAsObject: TObject;
begin
  Result := DValueGetAsObject(@FRawValue);
end;

function TDValueItem.GetAsStream: TMemoryStream;
begin
  CheckDValueSetType(@FRawValue, vdtStream);
  Result :=  TMemoryStream(FRawValue.Value.AsStream);
  {$IFDEF NEXTGEN}
  // �ƶ�ƽ̨��AData�ļ�����Ҫ���ӣ��Ա����Զ��ͷ�
  if Result <> nil then
  begin
    Result.__ObjAddRef;
  end;
  {$ENDIF} 
end;

function TDValueItem.GetAsString: String;
begin
  Result := DValueGetAsString(@FRawValue);
end;

{$IFNDEF NEXTGEN}
procedure TDValueItem.SetAsStringA(const Value: AnsiString);
begin
  DValueSetAsStringA(@FRawValue, Value);
end;    

function TDValueItem.GetAsStringA: AnsiString;
begin
  Result := DValueGetAsStringA(@FRawValue);
end;
{$ENDIF}

function TDValueItem.GetAsStringW: WideString;
begin
  Result := DValueGetAsStringW(@FRawValue);
end;

function TDValueItem.GetAsUInt: UInt64;
begin
  Result := DValueGetAsUInt64(@FRawValue);
end;

function TDValueItem.GetDataType: TDValueDataType;
begin
  Result := FRawValue.ValueType;
end;

function TDValueItem.IsEmpty: Boolean;
begin
  Result := DValueIsEmpty(@FRawValue);
end;

procedure TDValueItem.SetArraySize(const Value: Integer);
var
  lvOldSize:Integer;
  i, l: Integer;
  lvDValueItem:TDValueItem;
begin
  lvOldSize := GetSize;
  if lvOldSize <> Value then   // ԭ�гߴ����³ߴ��С��ͬ
  begin
    // �����µĳߴ��С�������С�ᴦ��ԭ�нڵ����������
    CheckDValueSetArrayLength(@FRawValue, Value);
    l := GetSize;
    if l > lvOldSize then
      for i := lvOldSize to l - 1 do
      begin
        lvDValueItem := TDValueItem.Create();
        // ����ItemΪTDValueItem����
        DValueBindPointerData(GetDValueItem(@FRawValue, i), lvDValueItem, praObjectFree);
      end;
  end;
end;

procedure TDValueItem.SetAsBoolean(const Value: Boolean);
begin
{$IFDEF CHECK_DVALUE}
  __CheckValueOK();
{$ENDIF}
  DValueSetAsBoolean(@FRawValue, Value);
end;


procedure TDValueItem.SetAsFloat(const Value: Double);
begin
{$IFDEF CHECK_DVALUE}
  __CheckValueOK();
{$ENDIF}

  DValueSetAsFloat(@FRawValue, Value);
end;

procedure TDValueItem.SetAsInteger(const Value: Int64);
begin
{$IFDEF CHECK_DVALUE}
  __CheckValueOK();
{$ENDIF}
  DValueSetAsInt64(@FRawValue, Value);
end;

procedure TDValueItem.SetAsInterface(const Value: IInterface);
begin
{$IFDEF CHECK_DVALUE}
  __CheckValueOK();
{$ENDIF}
  DValueSetAsInterface(@FRawValue, Value);
end;

procedure TDValueItem.SetAsString(const Value: String);
begin
{$IFDEF CHECK_DVALUE}
  __CheckValueOK();
{$ENDIF}
  DValueSetAsString(@FRawValue, Value);
end;


procedure TDValueItem.SetAsStringW(const Value: WideString);
begin
{$IFDEF CHECK_DVALUE}
  __CheckValueOK();
{$ENDIF}
  DValueSetAsStringW(@FRawValue, Value);

end;

procedure TDValueItem.SetAsUInt(const Value: UInt64);
begin
  DValueSetAsUInt64(@FRawValue, Value);
end;

function TDValueItem.SizeOf: Integer;
begin
  Result := GetDValueSize(@FRawValue);
end;

{$IFDEF CHECK_DVALUE}
procedure TDValueItem.__CheckValueOK;
begin
   Assert(self<>nil, '������δ����');
   Assert(__objflag=$DA, '������δ����,�����Ѿ�����');
end;
{$ENDIF}

initialization
  __DateTimeFormat := 'yyyy-MM-dd hh:nn:ss.zzz';


end.
