(*
 *	 Unit owner: d10.天地弦,qdac.swish
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)

unit utils_hashs;

interface

uses
  SysUtils, SyncObjs, Classes;


type
// 25:XE5
{$IF CompilerVersion<=25}
  IntPtr = Integer;
{$IFEND}

  EDHashTableException = Class(Exception);
  /// <summary>
  ///   hash value type
  /// </summary>
  TDHashValueType = Cardinal;

  PDHashData=^TDHashData;
  TDHashData=record
    Key        : String;      // Data Key    
    Next       : PDHashData;      // next value
    Data       : Pointer;         // data
    Hash       : TDHashValueType; // data hash value
  end;

  TDBuckets =array of PDHashData;

{$IFDEF UNICODE}
  TOnDataCompare = reference to function(P1,P2:Pointer): Integer;
  TOnDHashDataNotify = reference to procedure(pvData:PDHashData);
  TOnDHashDataNotifyEx = reference to procedure(pvData: PDHashData; pvParamData: Pointer);
  TOnDataNotify = reference to procedure(pvData:Pointer);
{$ELSE}
  TOnDataCompare = function(P1,P2:Pointer): Integer of object;
  TOnDHashDataNotify = procedure(pvData:PDHashData) of object;
  TOnDHashDataNotifyEx = procedure(pvData: PDHashData; pvParamData: Pointer) of object;
  TOnDataNotify = procedure(pvData:Pointer) of object;
{$ENDIF}
  TPointerNotifyProc = procedure(const sender:Pointer; const v:Pointer);


  TDHashTable = class(TObject)
  private
    FOnCompare: TOnDataCompare;

    FBucketSize: Cardinal;

    FCount:Integer;

    FBuckets:TDBuckets;
    FOnDelete: TOnDataNotify;


    procedure DoDelete(AHash:TDHashValueType; AData:Pointer);

    procedure CreateHashData(var vData: PDHashData);
    function GetBuckets(AIndex: Cardinal): PDHashData;


    procedure ReleaseHashData(var vData: PDHashData);

    function InnerCompare(pvData1, pvData2:Pointer): Integer;

    procedure SetOnCompare(const Value: TOnDataCompare);
    
    procedure SetValues(pvHashValue: Cardinal; const Value: Pointer);
    function GetValues(pvHashValue: Cardinal): Pointer;
  private
    function GetValueMap(const pvKey: String): Pointer;
    procedure SetBucketAutoSize(const Value: Boolean);
    procedure SetValueMap(const pvKey: String; const Value: Pointer);
  protected
    FBucketAutoSize: Boolean;
  public
    constructor Create(pvBucketSize: Cardinal = 1361);

    destructor Destroy; override;

    /// <summary>
    ///    循环每一个数据进行回调
    /// </summary>
    procedure ForEach(pvCallback:TOnDHashDataNotify);overload;

    /// <summary>
    ///    循环每一个数据进行回调
    /// </summary>
    procedure ForEach(pvCallback:TPointerNotifyProc); overload;



    /// <summary>
    ///   循环每一个数据, 带一个扩展的参数数据进行回调
    /// </summary>
    procedure ForEach(pvCallback:TOnDHashDataNotifyEx; pvParamData:Pointer); overload;

    /// <summary>
    ///   循环每一个数据, 带一个扩展的参数数据进行回调
    /// </summary>
    procedure ForEach(pvCallback:TPointerNotifyProc; pvParamData:Pointer); overload;

    /// <summary>
    ///  add AData
    /// </summary>
    procedure Add(pvHashValue: TDHashValueType; pvData: Pointer);

    /// <summary>
    ///  set key->value
    /// </summary>
    procedure SetData(pvHashValue: TDHashValueType; pvData: Pointer);

    /// <summary>
    ///   find first item by hashValue
    /// </summary>
    function FindFirst(pvHashValue:TDHashValueType): PDHashData;


    /// <summary>
    ///   find first data by hashValue
    /// </summary>
    function FindFirstData(pvHashValue:TDHashValueType): Pointer;

    /// <summary>
    ///   clear all data
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   delete frist element by hashValue
    /// </summary>
    function DeleteFirst(pvHashValue: TDHashValueType; pvData: Pointer): Boolean; overload;

    /// <summary>
    ///   delete frist element by hashValue
    /// </summary>
    function DeleteFirst(pvHashValue: TDHashValueType): Boolean; overload;


    /// <summary>
    ///   exists?
    /// </summary>
    function Exists(pvHashValue: TDHashValueType; pvData: Pointer): Boolean;overload;

    /// <summary>
    ///   exists?
    /// </summary>
    function Exists(pvHashValue: TDHashValueType): Boolean; overload;
  public
    /// <summary>
    ///   remove data by strKey
    /// </summary>
    function Remove(pvKey:string):Boolean;
  public


    /// <summary>
    ///   resize bucket length
    /// </summary>
    procedure SetBucketSize(pvBucketSize:Integer);

    procedure FreeAllDataAsObject();
    procedure DisposeAllDataAsPointer;

    /// <summary>
    ///   将所有数据写入到List中
    /// </summary>
    procedure GetDatas(pvList:TList);

    procedure GetKeyList(pvList:TStrings);

    procedure GetEntrySetList(pvList:TList);

    /// <summary>
    ///  进行一个赋值，如果键值存在，则不进行赋值， 返回false
    /// </summary>
    function TrySetValue(const pvKey: String; const Value: Pointer): Boolean;

    property Buckets[AIndex: Cardinal]: PDHashData read GetBuckets;

    property BucketSize: Cardinal read FBucketSize;

    property Count: Integer read FCount;

    property OnDelete: TOnDataNotify read FOnDelete write FOnDelete;

    property OnCompare: TOnDataCompare read FOnCompare write SetOnCompare;

    /// 是否自动调整桶大小
    property BucketAutoSize: Boolean read FBucketAutoSize write SetBucketAutoSize;

    property ValueMap[const pvKey:String]: Pointer read GetValueMap write SetValueMap;

    property Values[pvHashValue: Cardinal]: Pointer read GetValues write SetValues; default;

    
  end;

  TDHashTableSafe = class(TDHashTable)
  private
    FLocker: TCriticalSection;
  public
    constructor Create(pvBucketSize: Cardinal = 1361);
    destructor Destroy; override;

    procedure Lock();

    procedure UnLock;

  end;


// copy from qdac
function hashOf(const pvStrData:String): Integer; overload;
function hashOf(const p:Pointer;l:Integer): Integer; overload;
procedure FreeObject(AObject: TObject);

implementation

resourcestring
  SHashTableIndexError = 'Buckets index out of bounds (%d)';

procedure FreeObject(AObject: TObject);
begin
{$IFDEF AUTOREFCOUNT}
  AObject.DisposeOf;
{$ELSE}
  AObject.Free;
{$ENDIF}
end;

function hashOf(const p:Pointer;l:Integer): Integer; overload;
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

function hashOf(const pvStrData:String): Integer; overload;
begin
  Result := hashOf(PChar(pvStrData), Length(pvStrData) * SizeOf(Char));
end;

procedure TDHashTable.Clear;
var
  I:Integer;
  lvBucket: PDHashData;
begin
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      FBuckets[I]:=lvBucket.Next;

      DoDelete(lvBucket.Hash, lvBucket.Data);
      ReleaseHashData(lvBucket);

      lvBucket:=FBuckets[I];
    end;
  end;
  FCount:=0;
end;

constructor TDHashTable.Create(pvBucketSize: Cardinal = 1361);
begin
  inherited Create;
  FBucketAutoSize := pvBucketSize = 0;
  SetBucketSize(pvBucketSize);
  FOnCompare := InnerCompare;
end;

procedure TDHashTable.Add(pvHashValue: TDHashValueType; pvData: Pointer);
var
  lvIndex :Cardinal;
  lvBucket:PDHashData;
begin
  CreateHashData(lvBucket);

  lvBucket.Data:=pvData;
  lvBucket.Hash:=pvHashValue;

  lvIndex := pvHashValue mod FBucketSize;
  lvBucket.Next:=FBuckets[lvIndex];

  FBuckets[lvIndex]:=lvBucket;

  Inc(FCount);

  if FBucketAutoSize and ((FCount div Length(FBuckets)) > 3) then
    SetBucketSize(0);
end;

function TDHashTable.InnerCompare(pvData1, pvData2:Pointer): Integer;
begin           
  Result := IntPtr(pvData1) - IntPtr(pvData2);
end;

procedure TDHashTable.CreateHashData(var vData: PDHashData);
begin
  New(vData);
end;

function TDHashTable.DeleteFirst(pvHashValue: TDHashValueType): Boolean;
var
  lvIndex:Cardinal;
  lvCurrData, lvPrior:PDHashData;
begin
  Result := False;
  lvIndex:=pvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];
  lvPrior:=nil;

  while Assigned(lvCurrData) do
  begin
    if lvCurrData.Hash = pvHashValue then
    begin
      if Assigned(lvPrior) then
        lvPrior.Next := lvCurrData.Next
      else
        FBuckets[lvIndex]:= lvCurrData.Next;

      DoDelete(lvCurrData.Hash, lvCurrData.Data);

      ReleaseHashData(lvCurrData);
      Dec(FCount);
      Result := true;
      Break;
    end else
    begin
      lvPrior:= lvCurrData;
      lvCurrData:=lvPrior.Next;
    end;
  end;
end;

destructor TDHashTable.Destroy;
begin
  Clear;
  inherited;
end;

procedure TDHashTable.DoDelete(AHash:TDHashValueType; AData:Pointer);
begin
  if Assigned(FOnDelete) then
    FOnDelete(AData);

end;

function TDHashTable.Exists(pvHashValue: TDHashValueType; pvData: Pointer):
    Boolean;
var
  lvIndex:Cardinal;
  lvCurrData:PDHashData;
begin
  Result := False;
  lvIndex:=pvHashValue mod FBucketSize;

  lvCurrData:=FBuckets[lvIndex];
  while Assigned(lvCurrData) do
  begin
    //first compare hash value
    if lvCurrData.Hash = pvHashValue then
      if FOnCompare(pvData, lvCurrData.Data) = 0 then
      begin
        Result := true;
        Break;
      end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

function TDHashTable.FindFirst(pvHashValue:TDHashValueType): PDHashData;
var
  lvIndex:Cardinal;
  lvCurrData:PDHashData;
begin
  Result := nil;
  lvIndex:=pvHashValue mod FBucketSize;

  lvCurrData:=FBuckets[lvIndex];
  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if lvCurrData.Hash = pvHashValue then
    begin
      Result := lvCurrData;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

function TDHashTable.FindFirstData(pvHashValue:TDHashValueType): Pointer;
var
  lvIndex:Cardinal;
  lvCurrData:PDHashData;
begin
  Result := nil;
  lvIndex:=pvHashValue mod FBucketSize;

  lvCurrData:=FBuckets[lvIndex];
  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if lvCurrData.Hash = pvHashValue then
    begin
      Result := lvCurrData.Data;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

procedure TDHashTable.ForEach(pvCallback: TOnDHashDataNotifyEx;
  pvParamData: Pointer);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  Assert(Assigned(pvCallback));
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      pvCallback(lvBucket, pvParamData);
      lvBucket:=lvBucket.Next;
    end;
  end;
end;

procedure TDHashTable.ForEach(pvCallback:TOnDHashDataNotify);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  Assert(Assigned(pvCallback));
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      pvCallback(lvBucket);
      lvBucket:=lvBucket.Next;
    end;
  end;
end;

procedure TDHashTable.FreeAllDataAsObject;
var
  I:Integer;
  lvBucket: PDHashData;
begin
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      FreeObject(TObject(lvBucket.Data));
      lvBucket:=lvBucket.Next;
    end;
  end;
end;

procedure TDHashTable.DisposeAllDataAsPointer;
var
  I:Integer;
  lvBucket: PDHashData;
begin
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      if lvBucket.Data <> nil then
      begin
        Dispose(lvBucket.Data);
        lvBucket.Data :=nil;
      end;
      lvBucket:=lvBucket.Next;
    end;
  end;
end;

function TDHashTable.GetBuckets(AIndex: Cardinal): PDHashData;
begin
  if (AIndex>=FBucketSize) then
  begin
    raise EDHashTableException.CreateFmt(SHashTableIndexError, [AIndex]);
  end;

  Result := FBuckets[AIndex];
end;

procedure TDHashTable.ReleaseHashData(var vData: PDHashData);
begin
  Dispose(vData);
end;

function TDHashTable.Remove(pvKey: string): Boolean;
var
  lvIndex, lvHashValue:Cardinal;
  lvCurrData, lvPrior:PDHashData;
begin
  Result := False;
  lvHashValue := hashOf(LowerCase(pvKey));
  lvIndex:=lvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];
  lvPrior:=nil;

  while Assigned(lvCurrData) do
  begin
    if (lvCurrData.Hash = lvHashValue) and SameText(pvKey, lvCurrData.Key) then
    begin
      if Assigned(lvPrior) then
        lvPrior.Next := lvCurrData.Next
      else
        FBuckets[lvIndex]:= lvCurrData.Next;

      DoDelete(lvCurrData.Hash, lvCurrData.Data);

      ReleaseHashData(lvCurrData);
      Dec(FCount);
      Result := true;
      Break;
    end else
    begin
      lvPrior:= lvCurrData;
      lvCurrData:=lvPrior.Next;
    end;
  end;  
end;

function TDHashTable.DeleteFirst(pvHashValue: TDHashValueType; pvData:
    Pointer): Boolean;
var
  lvIndex:Cardinal;
  lvCurrData, lvPrior:PDHashData;
begin
  Result := False;
  lvIndex:=pvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];
  lvPrior:=nil;
  
  while Assigned(lvCurrData) do
  begin
    if FOnCompare(pvData, lvCurrData.Data) = 0 then
    begin
      if Assigned(lvPrior) then
        lvPrior.Next := lvCurrData.Next
      else
        FBuckets[lvIndex]:= lvCurrData.Next;

      DoDelete(lvCurrData.Hash, lvCurrData.Data);
      
      ReleaseHashData(lvCurrData);
      Dec(FCount);
      Result := true;
      Break;
    end else
    begin
      lvPrior:= lvCurrData;
      lvCurrData:=lvPrior.Next;
    end;
  end;   
end;



function TDHashTable.Exists(pvHashValue: TDHashValueType): Boolean;
var
  lvIndex:Cardinal;
  lvCurrData:PDHashData;
begin
  Result := False;
  lvIndex:=pvHashValue mod FBucketSize;

  lvCurrData:=FBuckets[lvIndex];
  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if lvCurrData.Hash = pvHashValue then
    begin
      Result := true;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

procedure TDHashTable.ForEach(pvCallback:TPointerNotifyProc);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  Assert(Assigned(pvCallback));
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      pvCallback(lvBucket, lvBucket.Data);
      lvBucket:=lvBucket.Next;
    end;
  end;
end;

procedure TDHashTable.GetDatas(pvList:TList);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      pvList.Add(lvBucket.Data);
      lvBucket:=lvBucket.Next;
    end;
  end;
end;

procedure TDHashTable.GetKeyList(pvList:TStrings);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      pvList.Add(lvBucket.Key);
      lvBucket:=lvBucket.Next;
    end;
  end;
end;

function TDHashTable.GetValueMap(const pvKey: String): Pointer;
var
  lvCurrData:PDHashData;
  lvIndex, lvHashValue:Cardinal;
  lvDataKey  : String;
begin
  Result := nil;
  lvDataKey   := LowerCase(pvKey);
  lvHashValue := hashOf(lvDataKey);
  
  lvIndex:=lvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];

  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if (lvCurrData.Hash = lvHashValue) and (SameText(lvDataKey, lvCurrData.Key)) then
    begin
      Result := lvCurrData.Data;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;
end;

function TDHashTable.GetValues(pvHashValue: Cardinal): Pointer;
begin
  Result := FindFirstData(pvHashValue);
end;

procedure TDHashTable.SetBucketAutoSize(const Value: Boolean);
begin
  if FBucketAutoSize <> Value then
  begin
    FBucketAutoSize := Value;
    if BucketAutoSize then
    begin
      if (FCount div Length(FBuckets)) > 3 then
        SetBucketSize(0);
    end;
  end;
end;

procedure TDHashTable.SetBucketSize(pvBucketSize:Integer);
const
  //default bucket size
  BucketNormalSize:array[0..27] of Integer=(
    17,37,79,163,331,673, 1361, 2729, 5471,10949,21911,43853,87719,175447,350899,
    701819,1403641,2807303,5614657,11229331,22458671,44917381,89834777,
    179669557,359339171,718678369,1437356741,2147483647
    );
var
  lvIndex, lvBucketSize:Cardinal;
  I :Integer;
  lvHash  : TDHashValueType;
  lvOldBuckets: TDBuckets;
  lvData, lvNext: PDHashData;
begin
  lvBucketSize := pvBucketSize;
  if lvBucketSize=0 then
  begin
    for i:=0 to 27 do
    begin
      if BucketNormalSize[i] > FCount then
      begin
        lvBucketSize:= BucketNormalSize[i];
        Break;
      end;
    end;

    if lvBucketSize=0 then  // max size
      lvBucketSize:= BucketNormalSize[27];
    if lvBucketSize = FBucketSize then Exit;
  end;

  if FBucketSize <> lvBucketSize then
  begin   // bucket size changed

    // save old arrange
    lvOldBuckets := FBuckets;

    // new bucket size
    FBucketSize := lvBucketSize;
    SetLength(FBuckets, FBucketSize);

    // empty
    for I := 0 to FBucketSize - 1 do FBuckets[I]:=nil;

    
    // rearrange element
    for I := 0 to High(lvOldBuckets) do
    begin        
      lvData:=lvOldBuckets[I];
      while lvData<>nil do
      begin
        lvHash := lvData.Hash;
        lvIndex := lvHash mod FBucketSize;
        lvNext := lvData.Next;

        lvData.Next := FBuckets[lvIndex];
        FBuckets[lvIndex]:=lvData;
        lvData := lvNext;
      end;
    end;
  end;
end;

procedure TDHashTable.SetData(pvHashValue: TDHashValueType; pvData: Pointer);
var
  lvPData, lvBucket, lvCurrData:PDHashData;
  lvIndex:Cardinal;
begin
  lvPData := nil;
  lvIndex:=pvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];

  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if lvCurrData.Hash = pvHashValue then
    begin
      lvPData := lvCurrData;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;

  // found
  if lvPData <> nil then
  begin
    lvPData.Data := pvData;
  end else
  begin  // add
    CreateHashData(lvBucket);
    lvBucket.Data:=pvData;
    lvBucket.Hash:=pvHashValue;
    
    lvBucket.Next:=FBuckets[lvIndex];
    FBuckets[lvIndex]:=lvBucket;

    Inc(FCount);
  end;   
end;

procedure TDHashTable.SetOnCompare(const Value: TOnDataCompare);
begin
  if not Assigned(Value) then
    FOnCompare := InnerCompare
  else
    FOnCompare := Value;
end;

procedure TDHashTable.SetValueMap(const pvKey: String; const Value: Pointer);
var
  lvPData, lvBucket, lvCurrData:PDHashData;
  lvIndex, lvHashValue:Cardinal;
  lvDataKey  : String;
begin
  lvPData := nil;
  lvDataKey   := LowerCase(pvKey);
  lvHashValue := hashOf(lvDataKey);
  
  lvIndex:=lvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];

  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if (lvCurrData.Hash = lvHashValue) and (SameText(lvDataKey, lvCurrData.Key)) then
    begin
      lvPData := lvCurrData;
      Break;
    end;
    lvCurrData:=lvCurrData.Next;
  end;

  // found
  if lvPData <> nil then
  begin
    lvPData.Data := Value;
  end else
  begin  // add
    CreateHashData(lvBucket);
    lvBucket.Data:=Value;
    lvBucket.Hash:=lvHashValue;
    lvBucket.Key := pvKey;
    
    lvBucket.Next:=FBuckets[lvIndex];
    FBuckets[lvIndex]:=lvBucket;

    Inc(FCount);
  end;    
end;

procedure TDHashTable.SetValues(pvHashValue: Cardinal; const Value: Pointer);
begin
  SetData(pvHashValue, Value);
end;

constructor TDHashTableSafe.Create(pvBucketSize: Cardinal = 1361);
begin
  inherited Create(pvBucketSize);
  FLocker := TCriticalSection.Create();
end;

destructor TDHashTableSafe.Destroy;
begin
  FreeAndNil(FLocker);
  inherited Destroy;
end;


procedure TDHashTableSafe.Lock;
begin
  FLocker.Enter;
end;

procedure TDHashTableSafe.UnLock;
begin
  FLocker.Leave;
end;

procedure TDHashTable.ForEach(pvCallback: TPointerNotifyProc;
  pvParamData: Pointer);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  Assert(Assigned(pvCallback));
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      pvCallback(lvBucket, pvParamData);
      lvBucket:=lvBucket.Next;
    end;
  end;  
end;

procedure TDHashTable.GetEntrySetList(pvList:TList);
var
  I:Integer;
  lvBucket: PDHashData;
begin
  for I := 0 to High(FBuckets) do
  begin
    lvBucket := FBuckets[I];
    while lvBucket<>nil do
    begin
      pvList.Add(lvBucket);
      lvBucket:=lvBucket.Next;
    end;
  end;

end;

function TDHashTable.TrySetValue(const pvKey: String; const Value: Pointer):
    Boolean;
var
  lvPData, lvBucket, lvCurrData:PDHashData;
  lvIndex, lvHashValue:Cardinal;
  lvDataKey  : String;
begin
  Result := False;
  lvPData := nil;
  lvDataKey   := LowerCase(pvKey);
  lvHashValue := hashOf(lvDataKey);
  
  lvIndex:=lvHashValue mod FBucketSize;
  lvCurrData:=FBuckets[lvIndex];

  while Assigned(lvCurrData) do
  begin
    //compare hash value
    if (lvCurrData.Hash = lvHashValue) and (SameText(lvDataKey, lvCurrData.Key)) then
    begin
      lvPData := lvCurrData;
      Exit;
    end;
    lvCurrData:=lvCurrData.Next;
  end;


  // add
  CreateHashData(lvBucket);
  lvBucket.Data:=Value;
  lvBucket.Hash:=lvHashValue;
  lvBucket.Key := pvKey;

  lvBucket.Next:=FBuckets[lvIndex];
  FBuckets[lvIndex]:=lvBucket;

  Inc(FCount);
  Result := True;
end;

end.
