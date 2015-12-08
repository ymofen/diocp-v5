(*
 * 版权所有:
      qdac.swish, d10.天地弦
 * 参考qdac中qvalue进行实现
 *
 * 1. android下面DValuelist使用 TList会有出现异常, 使用TList<TDValueObject>正常
 *    2015-11-15 16:08:04(感谢CP46反馈)
*)
unit utils_DValue;

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
  
{$IFDEF UNICODE}
  DStringW = UnicodeString;
{$ELSE}
  DStringW = WideString;
{$ENDIF UNICODE}
  DCharW = WideChar;
  PDCharW = PWideChar;
  PDStringW = ^DStringW;
  PInterface = ^IInterface;

  // XE5
  {$IF CompilerVersion<26}
  IntPtr=Integer;
  {$IFEND IntPtr}

  {$if CompilerVersion < 18} //before delphi 2007
  TBytes = array of Byte;
  {$ifend}

  TDValueDataType = (vdtUnset, vdtNull, vdtBoolean, vdtSingle, vdtFloat,
    vdtInteger, vdtInt64, vdtCurrency, vdtGuid, vdtDateTime,
    vdtString, vdtStringW, vdtStream, vdtInterface, vdtReferObject, vdtOwnerObject, vdtArray);

  // 节点类型
  TDValueNodeType = (vntNull,        // 没有值
                     vntArray,       // 列表-数组
                     vntObject,      // 列表-Key-Value
                     vntValue        // 值
                     );



  PDValueRecord = ^TDValueRecord;

  /// 一个值对象
  TDValueData = record
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
          ArrayItemsEntry: PDValueRecord;
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
        (AsPointer: Pointer);
      26:
        (AsInterface: PInterface);
      30:
        (
          ValueType: TDValueDataType;
          Value: PDValueRecord;
        );
  end;

  TDValueRecord = record
    Value: TDValueData;
    ValueType: TDValueDataType;
  end;
  TDValues = array of TDValueRecord;

const
  TDValueNodeTypeStr: array[TDValueNodeType] of string = ('vntNull', 'vntArray', 'vntObject', 'vntValue');

  Path_SplitChars : TSysCharSet = ['.', '/' , '\'];


type
  TDValueItem = class;
  TDValueNode = class;
  TDValueObject = class(TObject)
  private
    FName: String;
    FRawValue: TDValueRecord;
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
  TDValueNode = class(TObject)
  private
    FName: TDValueItem;
    FValue: TDValueItem;
    FNodeType: TDValueNodeType;
    FParent: TDValueNode;

    {$IFDEF HAVE_GENERICS}
    FChildren: TList<TDValueNode>;
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
    function GetItems(Index: Integer): TDValueNode;
    /// <summary>
    ///   根据名称查找子节点
    /// </summary>
    function IndexOf(pvName: string): Integer;

    /// <summary>
    ///   根据路径查找对象，如果不存在返回nil
    /// </summary>
    /// <returns>
    ///   如果存在返回找到的对象，如果不存在返回nil
    /// </returns>
    /// <param name="pvPath"> 要查找的路径 </param>
    /// <param name="vParent"> 如果查找到对象返回找到对象的父节点 </param>
    /// <param name="vIndex"> 如果查找到对象,表示在父节点中的索引值 </param>
    function InnerFindByPath(pvPath: string; var vParent:TDValueNode; var vIndex:
        Integer): TDValueNode;
  public
    constructor Create(pvType: TDValueNodeType);
    
    destructor Destroy; override;

    /// <summary>
    ///   设置节点类型, 类型转换时会丢失数据
    /// </summary>
    procedure CheckSetNodeType(pvType:TDValueNodeType);

    function FindByName(pvName:String): TDValueNode;

    function FindByPath(pvPath:string): TDValueNode;

    function ItemByName(pvName:string): TDValueNode;

    function ForceByName(pvName:string): TDValueNode;

    function ForceByPath(pvPath:String): TDValueNode;

    /// <summary>
    ///   本身作为一个数组添加一个子节点
    ///     如果之前不是数组类型，将会被清除
    /// </summary>
    function AddArrayChild: TDValueNode;

    /// <summary>
    ///   根据名称移除掉一个子对象
    /// </summary>
    function RemoveByName(pvName:String): Integer;

    /// <summary>
    ///   释放所有的子对象
    ///   清空列表
    /// </summary>
    procedure RemoveAll;

    /// <summary>
    ///   根据索引删除掉一个子对象
    /// </summary>
    procedure Delete(pvIndex:Integer);

    property Count: Integer read GetCount;

    
    property Items[Index: Integer]: TDValueNode read GetItems; default;

    /// <summary>
    ///   键值对象
    /// </summary>
    property Name: TDValueItem read FName;

    /// <summary>
    ///   父节点
    /// </summary>
    property Parent: TDValueNode read FParent;

    /// <summary>
    ///   值对象
    /// </summary>
    property Value: TDValueItem read FValue;
  end;

  TDValueItem = class(TObject)
  private
    FRawValue: TDValueRecord;
    function GetArrayItem(pvIndex: Integer): TDValueItem;
    function GetArraySize: Integer;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Double;
    function GetAsInetger: Int64;
    function GetAsInterface: IInterface;

    function GetAsString: String;
    function GetDataType: TDValueDataType;
    procedure SetArraySize(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInetger(const Value: Int64);
    procedure SetAsInterface(const Value: IInterface);

    procedure SetAsString(const Value: String);

  public
    destructor Destroy; override;
    function Equal(pvItem:TDValueItem): Boolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsString: String read GetAsString write SetAsString;
    property AsInetger: Int64 read GetAsInetger write SetAsInetger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;

    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    function GetAsObject: TObject;
    procedure SetAsOwnerObject(const Value: TObject);
    procedure SetReferObject(const value:TObject);

    property ArrayItem[pvIndex: Integer]: TDValueItem read GetArrayItem;
    property ArraySize: Integer read GetArraySize write SetArraySize;




    property DataType: TDValueDataType read GetDataType;
  end;







function CompareDValue(pvDValue1: PDValueRecord; pvDValue2:PDValueRecord):
    Integer;

function GetDValueSize(ADValue: PDValueRecord): Integer;
function GetDValueItem(ADValue: PDValueRecord; pvIndex: Integer): PDValueRecord;

/// <summary>清理DValue内部占用的内存</summary>
/// <param name="ADValue"> (PDValueRecord) </param>
procedure ClearDValue(ADValue:PDValueRecord);
procedure CheckDValueSetType(ADValue:PDValueRecord; AType: TDValueDataType);

procedure CheckDValueSetArrayLength(ADValue: PDValueRecord; ALen: Integer);

procedure DValueSetAsString(ADValue:PDValueRecord; pvString:String);
function DValueGetAsString(ADValue:PDValueRecord): string;

procedure DValueSetAsStringW(ADValue:PDValueRecord; pvString:DStringW);
function DValueGetAsStringW(ADValue:PDValueRecord): DStringW;

procedure DValueSetAsReferObject(ADValue:PDValueRecord; pvData:TObject);
procedure DValueSetAsOwnerObject(ADValue:PDValueRecord; pvObject:TObject);
function DValueGetAsObject(ADValue:PDValueRecord): TObject;

procedure DValueSetAsInterface(ADValue: PDValueRecord; const pvValue:
    IInterface);
function DValueGetAsInterface(ADValue:PDValueRecord): IInterface;

procedure DValueSetAsInt64(ADValue:PDValueRecord; pvValue:Int64);
function DValueGetAsInt64(ADValue: PDValueRecord): Int64;

procedure DValueSetAsInteger(ADValue:PDValueRecord; pvValue:Integer);
function DValueGetAsInteger(ADValue: PDValueRecord): Integer;


procedure DValueSetAsFloat(ADValue:PDValueRecord; pvValue:Double);
function DValueGetAsFloat(ADValue: PDValueRecord): Double;


procedure DValueSetAsBoolean(ADValue:PDValueRecord; pvValue:Boolean);
function DValueGetAsBoolean(ADValue: PDValueRecord): Boolean;

function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): DStringW; overload;
function BinToHex(const ABytes: TBytes; ALowerCase: Boolean): DStringW; overload;

implementation

resourcestring
  SValueNotArray = '当前值不是数组类型，无法按数组方式访问。';
  SConvertError = '无法将 %s 转换为 %s 类型的值。';
  SUnsupportStreamSource = '无法将 Variant 类型转换为流。';

  SItemNotFound = '找不到对应的项目:%s';
  SItemExists   = '项目[%s]已经存在,不能重复添加.';
  SNoNameNode   = '该类型[%s]节点不保护名字';
  SNoValueNode  = '该类型[%s]节点不包含值';

const
  DValueTypeName: array [TDValueDataType] of String = ('Unassigned', 'NULL',
    'Boolean', 'Single', 'Float', 'Integer', 'Int64', 'Currency', 'Guid',
    'DateTime', 'String', 'StringW', 'Stream', 'Interface', 'ReferObject', 'OwnerObject', 'Array');


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

function GetDValueSize(ADValue: PDValueRecord): Integer;
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

function GetDValueItem(ADValue: PDValueRecord; pvIndex: Integer): PDValueRecord;
begin
  if ADValue.ValueType = vdtArray then
    Result := PDValueRecord(IntPtr(ADValue.Value.ArrayItemsEntry) + (SizeOf(TDValueRecord) * pvIndex))
  else
    raise Exception.Create(SValueNotArray);
end;

procedure ClearDValue(ADValue:PDValueRecord);
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
        FreeAndNil(ADValue.Value.AsStream);
      vdtInterface:
        Dispose(ADValue.Value.AsInterface);
      vdtOwnerObject:
        TObject(ADValue.Value.AsPointer).Free;
      vdtArray:
        ClearArray;
    end;
    ADValue.ValueType := vdtUnset;
  end;
end;

procedure CheckDValueSetType(ADValue:PDValueRecord; AType: TDValueDataType);
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
        ADValue.Value.AsStream := TMemoryStream.Create;
      vdtArray:
        ADValue.Value.ArrayLength := 0;
    end;
    ADValue.ValueType := AType;
  end;
end;

procedure CheckDValueSetArrayLength(ADValue: PDValueRecord; ALen: Integer);
begin
  CheckDValueSetType(ADValue, vdtArray);
  if ALen > 0 then
  begin
    if ADValue.Value.ArrayLength = 0 then
    begin
      GetMem(ADValue.Value.ArrayItemsEntry, SizeOf(TDValueRecord) * ALen);
      ADValue.Value.ArrayLength := ALen;
    end
    else
    begin
      if Cardinal(ALen) > ADValue.Value.ArrayLength then
      begin
        ReallocMem(ADValue.Value.ArrayItemsEntry, SizeOf(TDValueRecord) * ALen);
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

procedure DValueSetAsStringW(ADValue:PDValueRecord; pvString:DStringW);
begin
  CheckDValueSetType(ADValue, vdtStringW);
  ADValue.Value.AsStringW^ := pvString;
end;

function DValueGetAsStringW(ADValue:PDValueRecord): DStringW;
var
  lvHexStr:DStringW;
  function DTToStr(ADValue: PDValueRecord): DStringW;
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

procedure DValueSetAsInt64(ADValue:PDValueRecord; pvValue:Int64);
begin
  CheckDValueSetType(ADValue, vdtInt64);
  ADValue.Value.AsInt64 := pvValue;
end;

function DValueGetAsInteger(ADValue: PDValueRecord): Integer;
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

function DValueGetAsInt64(ADValue: PDValueRecord): Int64;
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

procedure DValueSetAsInteger(ADValue:PDValueRecord; pvValue:Integer);
begin
  CheckDValueSetType(ADValue, vdtInteger);
  ADValue.Value.AsInt64 := pvValue;
  
end;

procedure DValueSetAsFloat(ADValue:PDValueRecord; pvValue:Double);
begin
  CheckDValueSetType(ADValue, vdtFloat);
  ADValue.Value.AsFloat := pvValue;
end;

function DValueGetAsFloat(ADValue: PDValueRecord): Double;
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

procedure DValueSetAsBoolean(ADValue:PDValueRecord; pvValue:Boolean);
begin
  CheckDValueSetType(ADValue, vdtBoolean);
  ADValue.Value.AsBoolean := pvValue;
end;

function DValueGetAsBoolean(ADValue: PDValueRecord): Boolean;
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

function CompareDValue(pvDValue1: PDValueRecord; pvDValue2:PDValueRecord): Integer;
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

procedure DValueSetAsString(ADValue:PDValueRecord; pvString:String);
begin
  CheckDValueSetType(ADValue, vdtString);
  ADValue.Value.AsString^ := pvString;
end;

function DValueGetAsString(ADValue:PDValueRecord): string;
var
  lvHexStr:DStringW;
  function DTToStr(ADValue: PDValueRecord): DStringW;
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
    vdtArray:
      Result := '@@Array';
  end;
end;

procedure DValueSetAsOwnerObject(ADValue:PDValueRecord; pvObject:TObject);
begin
  if pvObject = nil then
  begin       // 清空
    ClearDValue(ADValue);
  end else
  begin
    CheckDValueSetType(ADValue, vdtOwnerObject);
    ADValue.Value.AsPointer := pvObject;
  end;
end;

function DValueGetAsObject(ADValue:PDValueRecord): TObject;
begin
  case ADValue.ValueType of
    vdtUnset, vdtNull:
      Result := nil;
    vdtOwnerObject, vdtReferObject:
      Result :=  TObject(ADValue.Value.AsPointer);
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      'Object']);
  end;
end;

procedure DValueSetAsInterface(ADValue: PDValueRecord; const pvValue:
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

function DValueGetAsInterface(ADValue:PDValueRecord): IInterface;
begin

  case ADValue.ValueType of
    vdtUnset, vdtNull:
      Result := nil;
    vdtInterface:
      Result :=  ADValue.Value.AsInterface^;
    vdtOwnerObject, vdtReferObject:
      TObject(ADValue.Value.AsPointer).GetInterface(IInterface, Result);
  else
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[ADValue.ValueType],
      DValueTypeName[vdtInterface]]);
  end;
end;



procedure DValueSetAsReferObject(ADValue:PDValueRecord; pvData:TObject);
begin
  if pvData = nil then
  begin       // 清空
    ClearDValue(ADValue);
  end else
  begin
    CheckDValueSetType(ADValue, vdtReferObject);
    ADValue.Value.AsPointer := pvData;
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

procedure TDValueNode.ClearChildren;
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

constructor TDValueNode.Create(pvType: TDValueNodeType);
begin
  inherited Create;
  FNodeType := vntNull;
  CreateName;
  FValue := TDValueItem.Create;
  
  CheckSetNodeType(pvType);
end;

procedure TDValueNode.CreateName;
begin
  if not Assigned(FName) then FName := TDValueItem.Create;
end;

procedure TDValueNode.DeleteName;
begin
  if Assigned(FName) then
  begin
    FName.Free;
    FName := nil;
  end;    
end;

destructor TDValueNode.Destroy;
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

function TDValueNode.AddArrayChild: TDValueNode;
begin
  CheckSetNodeType(vntArray);
  Result := TDValueNode.Create(vntValue);
  Result.FParent := Self;
  FChildren.Add(Result);
end;

procedure TDValueNode.CheckCreateChildren;
begin
  if not Assigned(FChildren) then
  begin
    {$IFDEF HAVE_GENERICS}
      FChildren := TList<TDValueNode>.Create;
    {$ELSE}
      FChildren := TList.Create;
    {$ENDIF} 
  end;
end;

function TDValueNode.GetCount: Integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.Count
  else
  begin
    Result := 0;
  end;
end;

function TDValueNode.ItemByName(pvName:string): TDValueNode;
begin
  Result := FindByName(pvName);
  if Result = nil then raise TDValueException.CreateFmt(SItemNotFound, [pvName]);
end;

procedure TDValueNode.CheckSetNodeType(pvType:TDValueNodeType);
begin
  if pvType <> FNodeType then
  begin
    if not (FNodeType in [vntNull]) then
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

    FNodeType := pvType;
  end;
end;

procedure TDValueNode.Delete(pvIndex:Integer);
begin
  TDValueItem(FChildren[pvIndex]).Free;
  FChildren.Delete(pvIndex);
end;

function TDValueNode.FindByName(pvName:String): TDValueNode;
var
  i:Integer;
begin
  i := IndexOf(pvName);
  if i = -1 then Result := nil else Result := Items[i];
end;

function TDValueNode.FindByPath(pvPath:string): TDValueNode;
var
  lvParent:TDValueNode;
  j:Integer;
begin
  Result := InnerFindByPath(pvPath, lvParent, j);
end;

function TDValueNode.ForceByName(pvName:string): TDValueNode;
begin
  Result := FindByName(pvName);
  if Result = nil then
  begin
    CheckSetNodeType(vntObject);
    Result := TDValueNode.Create(vntValue);
    Result.FName.AsString := pvName;
    Result.FParent := Self;
    FChildren.Add(Result);
  end;
end;

function TDValueNode.ForceByPath(pvPath:String): TDValueNode;
var
  lvName:string;
  s:string;
  sPtr:PChar;
  lvParent:TDValueNode;
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

function TDValueNode.GetItems(Index: Integer): TDValueNode;
begin
  Result := TDValueNode(FChildren[Index]);
end;

function TDValueNode.IndexOf(pvName: string): Integer;
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

function TDValueNode.InnerFindByPath(pvPath: string; var vParent:TDValueNode;
    var vIndex: Integer): TDValueNode;
var
  lvName:string;
  s:string;
  sPtr:PChar;
  lvTempObj, lvParent:TDValueNode;
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

procedure TDValueNode.RemoveAll;
begin
  ClearChildren();
end;

function TDValueNode.RemoveByName(pvName:String): Integer;
begin

  Result := IndexOf(pvName);
  if Result >= 0 then
  begin
    Delete(Result);
  end;
end;

destructor TDValueItem.Destroy;
begin
  ClearDValue(@FRawValue);
  inherited;
end;

function TDValueItem.Equal(pvItem:TDValueItem): Boolean;
begin
  Result := CompareDValue(@FRawValue, @pvItem.FRawValue) = 0;
end;

function TDValueItem.GetArrayItem(pvIndex: Integer): TDValueItem;
var
  lvObj:TObject;
begin
  if DataType <> vdtArray then
    raise EConvertError.CreateFmt(SConvertError, [DValueTypeName[DataType],
      DValueTypeName[vdtArray]]);

  lvObj := DValueGetAsObject(GetDValueItem(@FRawValue, pvIndex));
  Result := TDValueItem(lvObj);
end;

function TDValueItem.GetArraySize: Integer;
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

function TDValueItem.GetAsInetger: Int64;
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
  lvOldSize := GetArraySize;
  CheckDValueSetArrayLength(@FRawValue, Value);
  l := GetArraySize;
  if l > lvOldSize then
    for i := lvOldSize to l - 1 do
    begin
      lvDValueItem := TDValueItem.Create();
      // 设置Item为TDValueItem对象
      DValueSetAsOwnerObject(GetDValueItem(@FRawValue, i), lvDValueItem);
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

procedure TDValueItem.SetAsInetger(const Value: Int64);
begin
  DValueSetAsInt64(@FRawValue, Value);
end;

procedure TDValueItem.SetAsInterface(const Value: IInterface);
begin
  DValueSetAsInterface(@FRawValue, Value);
end;

procedure TDValueItem.SetAsOwnerObject(const Value: TObject);
begin
  DValueSetAsOwnerObject(@FRawValue, Value);
end;

procedure TDValueItem.SetAsString(const Value: String);
begin
  DValueSetAsString(@FRawValue, Value);
end;

procedure TDValueItem.SetReferObject(const value:TObject);
begin
  DValueSetAsReferObject(@FRawValue, Value);
end;

end.
