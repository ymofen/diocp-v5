(*
 * 版权所有:
      qdac.swish, d10.天地弦
 * 参考qdac中qvalue进行实现
 *
 * 1. android下面DValuelist使用 TList会有出现异常, 使用TList<TDValueObject>正常
 *    2015-11-15 16:08:04(感谢CP46反馈)
*)
unit utils_dvalue;

{$IF CompilerVersion>25}  // XE4(VER250)
  {$DEFINE HAVE_GENERICS}
{$IFEND}

interface


uses classes, sysutils, variants,
{$IFDEF HAVE_GENERICS}
     System.Generics.Collections,
{$ENDIF}
     varutils, math;


type

  TDValueException = class(Exception);

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


  PInterface = ^IInterface;

  // XE5
  {$IF CompilerVersion<26}
  IntPtr=Integer;
  {$IFEND IntPtr}

  {$if CompilerVersion < 18} //before delphi 2007
  TBytes = array of Byte;
  {$ifend}

  // 释放动作
  TObjectFreeAction = (faNone, faFree);

  // 指针释放动作
  TPtrReleaseAction = (praNone, praObjectFree, praDispose, praFreeMem);


  TDValueDataType = (vdtUnset, vdtNull, vdtBoolean, vdtSingle, vdtFloat,
    vdtInteger, vdtInt64, vdtCurrency, vdtGuid, vdtDateTime,
    vdtString, vdtStringW, vdtStream, vdtInterface, vdtPtr, vdtObject, vdtArray);

  TDValueDataTypes = set of TDValueDataType;

  // 节点类型
  TDValueObjectType = (vntNull,        // 没有值
                     vntArray,       // 列表-数组
                     vntObject,      // 列表-Key-Value
                     vntValue        // 值
                     );



  PDRawValue = ^TDRawValue;

  /// 一个值对象
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
      5:
        (AsGuid: PGuid);
      6:
        (AsDateTime: TDateTime);
      7:
        (AsString: PString);
      9:
        (AsStringW: PDStringW);
      15:
        (AsStream: Pointer);
      16:    // Array
        (
          ArrayLength: Cardinal;
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
    function InnerAdd(pvValueName:string): TDValueObject;
  public
    constructor Create();
    destructor Destroy; override;
    function Add(pvValueName:String): TDValueObject;

    function FindByName(pvValueName:string): TDValueObject;

    function ParamByName(pvValueName:String): TDValueObject;

    /// <summary>
    ///   如果参数不存在会进行创建,如果存在直接返回
    /// </summary>
    function ForceByName(pvValueName: String): TDValueObject;

    /// <summary>
    ///   清空所有的对象
    /// </summary>
    procedure Clear;    
    
    property Count: Integer read GetCount;

    property Items[pvIndex: Integer]: TDValueObject read GetItems; default;
  end;


  /// <summary>
  ///   DValue节点
  /// </summary>
  TDValue = class(TObject)
  private
    FName: TDValueItem;
    FValue: TDValueItem;
    FObjectType: TDValueObjectType;
    FParent: TDValue;

    {$IFDEF HAVE_GENERICS}
    FChildren: TList<TDValue>;
    {$ELSE}
    FChildren: TList;
    {$ENDIF}
  private
    function GetCount: Integer;
    /// <summary>
    ///   释放所有的子对象
    ///   清空列表
    /// </summary>
    procedure ClearChildren();
    procedure CheckCreateChildren;
    procedure CreateName();
    procedure DeleteName();


    function GetItems(pvIndex: Integer): TDValue;

    /// <summary>
    ///   根据名称查找子节点
    /// </summary>
    function IndexOf(pvName: string): Integer; overload;

    /// <summary>
    ///   根据名称查找子节点
    /// </summary>
    function IndexOf(pvName: Integer): Integer; overload;

    /// <summary>
    ///   根据路径查找对象，如果不存在返回nil
    /// </summary>
    /// <returns>
    ///   如果存在返回找到的对象，如果不存在返回nil
    /// </returns>
    /// <param name="pvPath"> 要查找的路径 </param>
    /// <param name="vParent"> 如果查找到对象返回找到对象的父节点 </param>
    /// <param name="vIndex"> 如果查找到对象,表示在父节点中的索引值 </param>
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
  public
    constructor Create(pvType: TDValueObjectType); overload;

    constructor Create; overload;

    destructor Destroy; override;

    /// <summary>
    ///   设置节点类型, 类型转换时会丢失数据
    /// </summary>
    procedure CheckSetNodeType(pvType:TDValueObjectType);

    function FindByName(pvName:String): TDValue; overload;

    function FindByName(pvName:Integer): TDValue; overload;

    function FindByPath(pvPath:string): TDValue;

    function ItemByName(pvName:string): TDValue;

    function ForceByName(pvName:string): TDValue; overload;

    function ForceByName(pvName:Integer): TDValue; overload;

    function ForceByPath(pvPath:String): TDValue;

    /// <summary>
    ///   将子节点整理成字符串列表
    /// </summary>
    function ToStrings(pvNameSpliter: String = '='; pvPreNameFix: string = '';
        pvValueDelimiter: string = sLineBreak): String;

    /// <summary>
    ///   本身作为一个数组添加一个子节点
    ///     如果之前不是数组类型，将会被清除
    /// </summary>
    function AddArrayChild: TDValue;

    /// <summary>
    ///   本身作为一个vntObject添加一个子节点
    ///     如果之前不是vntObject类型，将会被清除
    /// </summary>
    function Add: TDValue; overload;

    function Add(pvName:String): TDValue; overload;

    /// <summary>
    ///   根据名称移除掉一个子对象
    /// </summary>
    function RemoveByName(pvName:String): Integer;

    function IndexDataOf(pvData:Pointer): Integer;

    /// <summary>
    ///   释放所有的子对象
    ///   清空列表
    /// </summary>
    procedure RemoveAll;

    /// <summary>
    ///   清理值
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   根据索引删除掉一个子对象
    /// </summary>
    procedure Delete(pvIndex:Integer);

    /// <summary>
    ///  子项目数量
    /// </summary>
    property Count: Integer read GetCount;


    property Items[pvIndex: Integer]: TDValue read GetItems; default;

    /// <summary>
    ///   键值对象
    /// </summary>
    property Name: TDValueItem read FName;

    /// <summary>
    ///   对象类型
    /// </summary>
    property ObjectType: TDValueObjectType read FObjectType;

    /// <summary>
    ///   父节点
    /// </summary>
    property Parent: TDValue read FParent;

    /// <summary>
    ///   值对象
    /// </summary>
    property Value: TDValueItem read FValue;

  public
    function GetStrValueByName(pvName:string; pvDefault:string): String;
    function GetIntValueByName(pvName: String; pvDefault: Int64): Int64;

    function GetValueByName(pvName: String; pvDefault: Int64): Int64;overload;
    function GetValueByName(pvName:string; pvDefault:string): String;overload;
    function GetValueByName(pvName:String; pvDefault:Boolean): Boolean; overload;
    function GetValueByName(pvName:String; pvDefault:Double): Double; overload;

    function GetValueByPath(pvPath:string; pvDefault:string): string; overload;
    function GetValueByPath(pvPath: string; pvDefault: Int64): Int64; overload;
    function GetValueByPath(pvPath:string; pvDefault:Boolean): Boolean; overload;
    function GetValueByPath(pvPath:string; pvDefault:Double): Double; overload;

    // 对Value的访问封装, 可以直接访问Value.AsXXXX
    procedure BindObject(pvObject: TObject; pvFreeAction: TObjectFreeAction =
        faFree);
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsString: String read GetAsString write SetAsString;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsObject: TObject read GetAsObject;

    property AsStream: TMemoryStream read GetAsStream;

  end;

  TDValueItem = class(TObject)
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

    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(const Value: Int64);
    procedure SetAsInterface(const Value: IInterface);

    procedure SetAsString(const Value: String);
  public
    /// <summary>
    ///   设置为数组方式同时设置数组大小
    ///    如果之前不是数组方式，将会被清理
    ///    如果设置的尺寸比之前大，之前的值将会被保留
    ///    如果小于之前的尺寸，后面的值将会被清空
    /// </summary>
    procedure SetArraySize(const Value: Integer);

    destructor Destroy; override;

    /// <summary>
    ///  比较两个值是否相等
    /// </summary>
    function Equal(pvItem:TDValueItem): Boolean;

    /// <summary>
    ///   清空值
    /// </summary>
    procedure Clear;


    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsString: String read GetAsString write SetAsString;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TObject read GetAsObject;

    property AsStream: TMemoryStream read GetAsStream;



    property AsInterface: IInterface read GetAsInterface write SetAsInterface;

    procedure BindObject(pvObject: TObject; pvFreeAction: TObjectFreeAction =
        faFree);

    /// <summary>
    ///   根据索引获取对象
    /// </summary>
    property Items[pvIndex: Integer]: TDValueItem read GetItems; default;

    property Size: Integer read GetSize;

    property DataType: TDValueDataType read GetDataType;
  end;







function CompareDValue(pvDValue1: PDRawValue; pvDValue2:PDRawValue):
    Integer;

function GetDValueSize(ADValue: PDRawValue): Integer;
function GetDValueItem(ADValue: PDRawValue; pvIndex: Integer): PDRawValue;

/// <summary>清理DValue内部占用的内存</summary>
/// <param name="ADValue"> (PDRawValue) </param>
procedure ClearDValue(ADValue:PDRawValue);
procedure CheckDValueSetType(ADValue:PDRawValue; AType: TDValueDataType);

procedure CheckDValueSetArrayLength(ADValue: PDRawValue; ALen: Integer);

procedure DValueSetAsString(ADValue:PDRawValue; pvString:String);
function DValueGetAsString(ADValue:PDRawValue): string;

procedure DValueSetAsStringW(ADValue:PDRawValue; pvString:DStringW);
function DValueGetAsStringW(ADValue:PDRawValue): DStringW;

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

procedure DValueSetAsInteger(ADValue:PDRawValue; pvValue:Integer);
function DValueGetAsInteger(ADValue: PDRawValue): Integer;


procedure DValueSetAsFloat(ADValue:PDRawValue; pvValue:Double);
function DValueGetAsFloat(ADValue: PDRawValue): Double;


procedure DValueSetAsBoolean(ADValue:PDRawValue; pvValue:Boolean);
function DValueGetAsBoolean(ADValue: PDRawValue): Boolean;

function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): DStringW; overload;
function BinToHex(const ABytes: TBytes; ALowerCase: Boolean): DStringW; overload;

procedure FreeObject(AObject: TObject);

implementation

resourcestring
  SValueNotArray = '当前值不是数组类型，无法按数组方式访问。';
  SConvertError = '无法将 %s 转换为 %s 类型的值。';
  SUnsupportStreamSource = '无法将 Variant 类型转换为流。';
  SOutOfBound   = '访问[%d]超出范围(0..%d)';

  SItemNotFound = '找不到对应的项目:%s';
  SItemExists   = '项目[%s]已经存在,不能重复添加.';
  SNoNameNode   = '该类型[%s]节点不保护名字';
  SNoValueNode  = '该类型[%s]节点不包含值';

const
  DValueTypeName: array [TDValueDataType] of String = ('Unassigned', 'NULL',
    'Boolean', 'Single', 'Float', 'Integer', 'Int64', 'Currency', 'Guid',
    'DateTime', 'String', 'StringW', 'Stream', 'Interface', 'Pointer', 'Object', 'Array');

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
    if (strPtr^ in splitChars) then
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
    I: Cardinal;
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
      vdtString:
        Dispose(ADValue.Value.AsString);
      vdtStringW:
        Dispose(ADValue.Value.AsStringW);
      vdtStream:
        FreeObject(TObject(ADValue.Value.AsStream));
      vdtInterface:
        Dispose(ADValue.Value.AsInterface);
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
      vdtStringW:
        New(ADValue.Value.AsStringW);
      vdtInterface:
        New(ADValue.Value.AsInterface);
      vdtStream:
      begin
        lvStream := TMemoryStream.Create;
        ADValue.Value.AsStream := lvStream;
        {$IFDEF NEXTGEN}
        // 移动平台下AData的计数需要增加，以避免自动释放
        if Result <> nil then
        begin
          Result.__ObjAddRef;
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
begin
  CheckDValueSetType(ADValue, vdtArray);
  if ALen > 0 then
  begin
    if ADValue.Value.ArrayLength = 0 then
    begin        // 原有长度为空
      GetMem(ADValue.Value.ArrayItemsEntry, SizeOf(TDRawValue) * ALen);
      ADValue.Value.ArrayLength := ALen;
    end
    else
    begin
      if Cardinal(ALen) > ADValue.Value.ArrayLength then
      begin
        ReallocMem(ADValue.Value.ArrayItemsEntry, SizeOf(TDRawValue) * ALen);
        ADValue.Value.ArrayLength := ALen;
      end
      else
      begin
        while ADValue.Value.ArrayLength > Cardinal(ALen) do
        begin
          ClearDValue(GetDValueItem(ADValue, ADValue.Value.ArrayLength - 1));
          Dec(ADValue.Value.ArrayLength);
        end;
      end;
    end;
  end;
end;

procedure DValueSetAsStringW(ADValue:PDRawValue; pvString:DStringW);
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

procedure DValueSetAsString(ADValue:PDRawValue; pvString:String);
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
    vdtString:
      Result := ADValue.Value.AsString^;
    vdtStringW:
      Result := ADValue.Value.AsStringW^;
    vdtUnset:
      Result := '';
    vdtNull:
      Result := '';
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
        // 移动平台下AData的计数需要增加，以避免自动释放
        if Result <> nil then
        begin
          Result.__ObjAddRef;
        end;
        {$ENDIF}
      end;
    vdtPtr:
      case ADValue.Value.PtrReleaseAction of
        praNone, praObjectFree:  // 引用对象，或者管理生命周期的对象
          begin
            Result :=  TObject(ADValue.Value.AsPointer);
            {$IFDEF NEXTGEN}
            // 移动平台下AData的计数需要增加，以避免自动释放
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
  begin       // 清空
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
          praNone, praObjectFree:  // 引用对象，或者管理生命周期的对象
            begin
              lvObj :=TObject(ADValue.Value.AsPointer);
              {$IFDEF NEXTGEN}
              // 移动平台下AData的计数需要增加，以避免自动释放
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
  begin       // 清空
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
  begin       // 清空
    ClearDValue(ADValue);
  end else
  begin
    CheckDValueSetType(ADValue, vdtObject);
    ADValue.Value.AsPointer := pvData;
{$IFDEF NEXTGEN}
    // 移动平台下AData的计数需要增加，以避免自动释放
    pvData.__ObjAddRef;
{$ENDIF}
    ADValue.Value.PtrReleaseAction := pvReleaseAction;
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

function TDValueList.FindByName(pvValueName:string): TDValueObject;
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

function TDValueList.ForceByName(pvValueName: String): TDValueObject;
begin
  Result := FindByName(pvValueName);
  if Result = nil then Result := InnerAdd(pvValueName);
end;

function TDValueList.InnerAdd(pvValueName:string): TDValueObject;
begin
  Result := TDValueObject.Create;
  Result.Name := pvValueName;
  FList.Add(Result);
end;

function TDValueList.ParamByName(pvValueName:String): TDValueObject;
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
      TDValueItem(FChildren[i]).Free;
    end;
    FChildren.Clear;
  end;
end;

constructor TDValue.Create(pvType: TDValueObjectType);
begin
  inherited Create;
  FObjectType := vntNull;
  CreateName;
  FValue := TDValueItem.Create;

  CheckSetNodeType(pvType);
end;

constructor TDValue.Create;
begin
  inherited;
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
  if Assigned(FChildren) then
  begin
    ClearChildren();
    FChildren.Free;
    FChildren := nil;
  end;

  if Assigned(FValue) then FValue.Free;
  DeleteName;
  inherited;
end;

function TDValue.Add: TDValue;
begin
  CheckSetNodeType(vntObject);
  Result := TDValue.Create(vntValue);
  Result.FParent := Self;
  FChildren.Add(Result);
end;

function TDValue.Add(pvName:String): TDValue;
begin
  CheckSetNodeType(vntObject);
  Result := TDValue.Create(vntValue);
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

procedure TDValue.BindObject(pvObject: TObject; pvFreeAction: TObjectFreeAction
    = faFree);
begin
  FValue.BindObject(pvObject, pvFreeAction);
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

function TDValue.ItemByName(pvName:string): TDValue;
begin
  Result := FindByName(pvName);
  if Result = nil then raise TDValueException.CreateFmt(SItemNotFound, [pvName]);
end;

procedure TDValue.CheckSetNodeType(pvType:TDValueObjectType);
begin
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
end;

procedure TDValue.Clear;
begin
  ClearChildren;
  FValue.Clear;
end;

procedure TDValue.Delete(pvIndex:Integer);
begin
  TDValueItem(FChildren[pvIndex]).Free;
  FChildren.Delete(pvIndex);
end;

function TDValue.FindByName(pvName:String): TDValue;
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

function TDValue.FindByPath(pvPath:string): TDValue;
var
  lvParent:TDValue;
  j:Integer;
begin
  Result := InnerFindByPath(pvPath, lvParent, j);
end;

function TDValue.ForceByName(pvName:string): TDValue;
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

function TDValue.ForceByPath(pvPath:String): TDValue;
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

function TDValue.GetValueByName(pvName:String; pvDefault:Boolean): Boolean;
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

function TDValue.GetStrValueByName(pvName:string; pvDefault:string): String;
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

function TDValue.GetValueByName(pvName:String; pvDefault:Double): Double;
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

function TDValue.GetValueByPath(pvPath: string; pvDefault: Int64): Int64;
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

function TDValue.GetValueByPath(pvPath: string; pvDefault: Boolean): Boolean;
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

function TDValue.GetValueByName(pvName: String; pvDefault: Int64): Int64;
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

function TDValue.GetValueByName(pvName, pvDefault: string): String;
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

function TDValue.GetValueByPath(pvPath: string; pvDefault: Double): Double;
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

function TDValue.GetValueByPath(pvPath:string; pvDefault:string): string;
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
    end;
  end;


end;

function TDValue.IndexOf(pvName: string): Integer;
var
  i:Integer;
begin
  Result := -1;
  if Assigned(FChildren) then   
    for i := 0 to FChildren.Count - 1 do
    begin
      if CompareText(Items[i].FName.AsString, pvName) = 0 then
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

function TDValue.RemoveByName(pvName:String): Integer;
begin

  Result := IndexOf(pvName);
  if Result >= 0 then
  begin
    Delete(Result);
  end;
end;

procedure TDValue.SetAsBoolean(const Value: Boolean);
begin
  FValue.SetAsBoolean(Value);
end;

procedure TDValue.SetAsFloat(const Value: Double);
begin
  FValue.SetAsFloat(Value);
end;

procedure TDValue.SetAsInteger(const Value: Int64);
begin
  FValue.SetAsInteger(Value);
end;

procedure TDValue.SetAsString(const Value: String);
begin
  FValue.SetAsString(Value);
end;

function TDValue.ToStrings(pvNameSpliter: String = '='; pvPreNameFix: string =
    ''; pvValueDelimiter: string = sLineBreak): String;
var
  i: Integer;
begin
  Result := '';

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

destructor TDValueItem.Destroy;
begin
  ClearDValue(@FRawValue);
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
  // TODO -cMM: TDValueItem.GetAsInterface default body inserted
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
  // 移动平台下AData的计数需要增加，以避免自动释放
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

function TDValueItem.GetDataType: TDValueDataType;
begin
  Result := FRawValue.ValueType;
end;

procedure TDValueItem.SetArraySize(const Value: Integer);
var
  lvOldSize:Integer;
  i, l: Integer;
  lvDValueItem:TDValueItem;
begin
  lvOldSize := GetSize;
  if lvOldSize <> Value then   // 原有尺寸与新尺寸大小不同
  begin
    // 设置新的尺寸大小，如果缩小会处理原有节点的数据清理
    CheckDValueSetArrayLength(@FRawValue, Value);
    l := GetSize;
    if l > lvOldSize then
      for i := lvOldSize to l - 1 do
      begin
        lvDValueItem := TDValueItem.Create();
        // 设置Item为TDValueItem对象
        DValueBindPointerData(GetDValueItem(@FRawValue, i), lvDValueItem, praObjectFree);
      end;
  end;
end;

procedure TDValueItem.SetAsBoolean(const Value: Boolean);
begin
  DValueSetAsBoolean(@FRawValue, Value);
end;

procedure TDValueItem.SetAsFloat(const Value: Double);
begin
  DValueSetAsFloat(@FRawValue, Value);
end;

procedure TDValueItem.SetAsInteger(const Value: Int64);
begin
  DValueSetAsInt64(@FRawValue, Value);
end;

procedure TDValueItem.SetAsInterface(const Value: IInterface);
begin
  DValueSetAsInterface(@FRawValue, Value);
end;

procedure TDValueItem.SetAsString(const Value: String);
begin
  DValueSetAsString(@FRawValue, Value);
end;

end.
