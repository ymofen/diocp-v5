(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-07-23 15:13:44
 *     修改Clear清空方式,去掉确保FHead不为nil的方式
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *   2016-02-15 22:39:17
 *     添加DataReleaseAction属性，Clear自动清理Data数据
 *
 *)
 
unit utils_queues;

interface

uses
  SyncObjs, SysUtils;

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

{.$DEFINE USE_QUEUE_POOL}

type
  TDataReleaseAction = (raNone, raObjectFree, raDispose, raFreeMem);

  PQueueData = ^TQueueData;
  TQueueData = record
    Data: Pointer;
    AsObject:TObject;
    Next: PQueueData;
    ReleaseAction: TDataReleaseAction;
  end;



  TBaseQueue = class(TObject)
  private
    FName: String;
    FLocker: TCriticalSection;
    FCount: Integer;
    FHead: PQueueData;
    FTail: PQueueData;

    {$IFDEF DEBUG_ON}
    FPopCounter:Integer;
    FPushCounter:Integer;
    {$ENDIF}

    function InnerDeQueue: PQueueData;
    procedure InnerAddToTail(AData: PQueueData);
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   清空所有数据
    /// </summary>
    procedure Clear;
    
    function IsEmpty: Boolean;

    function Size: Integer;

    function DeQueue: Pointer; overload;
    function DeQueue(var outPointer:Pointer):Boolean;overload;

    function DeQueueObject: TObject;

    /// <summary>
    ///  入队列
    /// </summary>
    procedure EnQueue(AData: Pointer; pvReleaseAction: TDataReleaseAction =
        raNone); overload;

    /// <summary>
    ///  入队列(队列, android平台需要_ObjAddRef
    /// </summary>
    procedure EnQueueObject(AData: TObject; pvReleaseAction: TDataReleaseAction =
        raNone); overload;
    /// <summary>
    ///  invoke Only Data Pointer is TObject
    /// </summary>
    procedure FreeDataObject;

    /// <summary>
    ///   dispose all data
    /// </summary>
    procedure DisposeAllData;

    property Name: String read FName write FName;

  end;

  TSafeQueue = class(TBaseQueue);

  /// <summary>
  ///   without lock
  /// </summary>
  TSimpleQueue = class(TObject)
  private
    FName: String;
    FCount: Integer;
    FHead: PQueueData;
    FTail: PQueueData;

    {$IFDEF DEBUG_ON}
    FPopCounter:Integer;
    FPushCounter:Integer;
    {$ENDIF}


    function InnerPop: PQueueData;
    procedure InnerAddToTail(AData: PQueueData);
  public
    constructor Create;
    destructor Destroy; override;
    function IsEmpty: Boolean;

    function Size: Integer;

    function DeQueue: Pointer; overload;
    function DeQueue(var outPointer:Pointer): Boolean; overload;

    /// <summary>
    ///   清空所有数据
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   add to tail
    /// </summary>
    procedure EnQueue(AData: Pointer; pvReleaseAction: TDataReleaseAction = raNone);

    /// <summary>
    ///  invoke Only Data Pointer is TObject
    /// </summary>
    procedure FreeDataObject;

    /// <summary>
    ///   dispose all data
    /// </summary>
    procedure DisposeAllData;

    property Name: String read FName write FName;  
  end;


function IsDebugMode: Boolean;

implementation


type
  /// <summary>
  ///  reference TJobPool in qdac3 
  /// </summary>
  TQueueDataPool = class
  protected
    FFirst: PQueueData;
    FCount: Integer;
    FSize: Integer;
    FLocker: TCriticalSection;

    {$IFDEF DEBUG_ON}
    FPopCounter:Integer;
    FPushCounter:Integer;
    {$ENDIF}
  public
    constructor Create(AMaxSize: Integer = 2048); overload;
    destructor Destroy; override;
    procedure Push(pvQueueData: PQueueData);
    function Pop: PQueueData;
    property Count: Integer read FCount;
    property Size: Integer read FSize write FSize;
  end;

var
  // data pool of PQueueData
  queueDataPool :TQueueDataPool;

function IsDebugMode: Boolean;
begin
{$IFDEF MSWINDOWS}
{$warn symbol_platform off}
  Result := Boolean(DebugHook);
{$warn symbol_platform on}
{$ELSE}
  Result := false;
{$ENDIF}
end;

procedure __ReleaseQueueData(pvQueueData:PQueueData);
begin
//  try

    if pvQueueData = nil then Exit;
    if pvQueueData.ReleaseAction = raNone then Exit;


    if pvQueueData.AsObject <> nil then
    begin
      case pvQueueData.ReleaseAction of
        raObjectFree : pvQueueData.AsObject.Free;
      end;
    end else
    begin
      if pvQueueData.Data = nil then Exit;

      case pvQueueData.ReleaseAction of
        raObjectFree : TObject(pvQueueData.Data).Free;
        raFreeMem : FreeMem(pvQueueData.Data);
        raDispose : Dispose(pvQueueData.Data);
      end;
    end;
//  except
//  end;
  pvQueueData.Data := nil;
end;

constructor TBaseQueue.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
  FHead := nil;
  FTail := nil;
  FCount := 0;
  FName := 'utils_queues';
end;

destructor TBaseQueue.Destroy;
begin
  Clear;
  {$IFDEF DEBUG_ON}
  if IsDebugMode then
    Assert(FPopCounter = FPushCounter,
      Format('[%s]PopCounter(%d) <> PushCounter(%d)', [FName, FPopCounter, FPushCounter])
      );
  {$ENDIF}


  FLocker.Free;
  inherited Destroy;
end;

procedure TBaseQueue.DisposeAllData;
var
  lvData:Pointer;
begin
  while True do
  begin
    lvData := nil;
    if DeQueue(lvData) then
    begin
      if lvData = nil then
      begin
        lvData := nil;
      end else
      begin
        Dispose(lvData);
      end;
    end else
    begin
      Break;
    end;
  end;
end;

{ TBaseQueue }

//procedure TBaseQueue.AddToHead(AData: Pointer);
//var
//  lvTemp:PQueueData;
//begin
//  lvTemp := queueDataPool.Pop;
//  lvTemp.Data := AData;
//  InnerAddToHead(lvTemp);
//end;

procedure TBaseQueue.Clear;
var
  ANext: PQueueData;
begin
  FLocker.Enter;
  try
    while FHead <> nil do
    begin
      ANext := FHead.Next;
      __ReleaseQueueData(FHead);
    {$IFDEF DEBUG_ON}
      Inc(FPopCounter);
    {$ENDIF}
      {$IFDEF USE_QUEUE_POOL}
      queueDataPool.Push(FHead);
      {$ELSE}
      Dispose(FHead);
      {$ENDIF} 
      FHead := ANext;
    end;
    FHead := nil;
    FTail := nil;
    FCount := 0;
  finally
    FLocker.Leave;
  end;
end;

procedure TBaseQueue.FreeDataObject;
var
  lvData:Pointer;
begin
  while True do
  begin
    lvData := nil;
    if DeQueue(lvData) then
    begin
      if lvData = nil then
      begin
        lvData := nil;
      end else
      begin
        TObject(lvData).Free;
      end;
    end else
    begin
      Break;
    end;
  end;
end;

function TBaseQueue.IsEmpty: Boolean;
begin
  Result := (FHead = nil);
end;

function TBaseQueue.DeQueue: Pointer;
var
  lvTemp:PQueueData;
begin
  Result := nil;
  lvTemp := InnerDeQueue;
  if lvTemp <> nil then
  begin
    Result := lvTemp.Data;
    {$IFDEF USE_QUEUE_POOL}
    queueDataPool.Push(lvTemp);
    {$ELSE}
    Dispose(lvTemp);
    {$ENDIF} 
  end;
end;

function TBaseQueue.DeQueue(var outPointer: Pointer): Boolean;
var
  lvTemp:PQueueData;
begin
  Result := false;
  lvTemp := InnerDeQueue;
  if lvTemp <> nil then
  begin
    outPointer := lvTemp.Data;
    {$IFDEF USE_QUEUE_POOL}
    queueDataPool.Push(lvTemp);
    {$ELSE}
    Dispose(lvTemp);
    {$ENDIF}
    Result := true;
  end;
end;

function TBaseQueue.DeQueueObject: TObject;
var
  lvTemp:PQueueData;
begin
  Result := nil;
  lvTemp := InnerDeQueue;
  if lvTemp <> nil then
  begin
    Result := lvTemp.AsObject;
    lvTemp.AsObject := nil;
    {$IFDEF USE_QUEUE_POOL}
    queueDataPool.Push(lvTemp);
    {$ELSE}
    Dispose(lvTemp);
    {$ENDIF}
  end;
end;

procedure TBaseQueue.EnQueue(AData: Pointer; pvReleaseAction:
    TDataReleaseAction = raNone);
var
  lvTemp:PQueueData;
begin
{$IFDEF USE_QUEUE_POOL}
  lvTemp := queueDataPool.Pop;
{$ELSE}
  New(lvTemp);
{$ENDIF}
  lvTemp.Data := AData;
  lvTemp.AsObject := nil;
  lvTemp.ReleaseAction := pvReleaseAction;
  InnerAddToTail(lvTemp);
end;

procedure TBaseQueue.EnQueueObject(AData: TObject; pvReleaseAction:
    TDataReleaseAction = raNone);
var
  lvTemp:PQueueData;
begin
{$IFDEF USE_QUEUE_POOL}
  lvTemp := queueDataPool.Pop;
{$ELSE}
  New(lvTemp);
{$ENDIF}
  lvTemp.Data := nil;
  lvTemp.AsObject := AData;
  lvTemp.ReleaseAction := pvReleaseAction;
  InnerAddToTail(lvTemp);
end;

function TBaseQueue.Size: Integer;
begin
  Result := FCount;
end;

function TBaseQueue.InnerDeQueue: PQueueData;
begin
  FLocker.Enter;
  try
    Result := FHead;
    if Result <> nil then
    begin
      FHead := Result.Next;
      
      if FHead = nil then FTail := nil;

      Dec(FCount);

    {$IFDEF DEBUG_ON}
      Inc(FPopCounter);
    {$ENDIF}
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TBaseQueue.InnerAddToTail(AData: PQueueData);
begin
  AData.Next := nil;
  FLocker.Enter;
  try
    if FTail = nil then
      FHead := AData
    else
    begin
      FTail.Next := AData;
    end;

    FTail := AData;
    Inc(FCount);

    {$IFDEF DEBUG_ON}
    Inc(FPushCounter);
    {$ENDIF}

  finally
    FLocker.Leave;
  end;
end;

{ TQueueDataPool }

constructor TQueueDataPool.Create(AMaxSize: Integer = 2048);
var
  i: Integer;
  lvBlock:PQueueData;
begin
  inherited Create;
  FSize := AMaxSize;
  FLocker := TCriticalSection.Create;


  // 预先分配好，避免与其他内存块一起分配(内存堆积)
  for i := 0 to AMaxSize - 1 do
  begin
    New(lvBlock);
    Push(lvBlock);
  end;
  
  {$IFDEF DEBUG_ON}
  FPushCounter := 0;
  FPopCounter := 0;
  {$ENDIF}
end;

destructor TQueueDataPool.Destroy;
var
  lvData: PQueueData;
begin
  {$IFDEF DEBUG_ON}
  if IsDebugMode then
  begin
    Assert(FPopCounter = FPushCounter, ('PopCounter <> PushCounter'));
  end;
  {$ENDIF}

  FLocker.Enter;
  while FFirst <> nil do
  begin
    lvData := FFirst.Next;
    Dispose(FFirst);
    FFirst := lvData;
  end;
  FLocker.Free;
  inherited;
end;

function TQueueDataPool.Pop: PQueueData;
begin
  FLocker.Enter;
  try
    Result := FFirst;
    if Result <> nil then
    begin
      FFirst := Result.Next;
      Dec(FCount);
    end;
    {$IFDEF DEBUG_ON}
      Inc(FPopCounter);
    {$ENDIF}
  finally
    FLocker.Leave;
  end;


  if Result = nil then
  begin
    New(Result);
  end;
  Result.Data := nil;
  Result.Next := nil;
end;

procedure TQueueDataPool.Push(pvQueueData: PQueueData);
var
  ADoFree: Boolean;
begin
  Assert(pvQueueData <> nil);
  FLocker.Enter;
  try
    ADoFree := (FCount >= FSize);
    if not ADoFree then
    begin
      pvQueueData.Next := FFirst;
      FFirst := pvQueueData;
      Inc(FCount);
    end;
    {$IFDEF DEBUG_ON}
    Inc(FPushCounter);
    {$ENDIF}
  finally
    FLocker.Leave;
  end;


  if ADoFree then
  begin
    Dispose(pvQueueData);
  end;
end; 

constructor TSimpleQueue.Create;
begin
  inherited Create;
  FHead := nil;
  FTail := nil;
  FCount := 0;
  FName := 'simpleQueue';
end;

destructor TSimpleQueue.Destroy;
begin
  {$IFDEF DEBUG_ON}
  if IsDebugMode then
    Assert(FPopCounter = FPushCounter, ('[' + FName + ']PopCounter <> PushCounter'));
  {$ENDIF}

  Clear;
  inherited Destroy;
end;

procedure TSimpleQueue.DisposeAllData;
var
  lvData:Pointer;
begin
  while True do
  begin
    lvData := nil;
    if DeQueue(lvData) then
    begin
      if lvData = nil then
      begin
        lvData := nil;
      end else
      begin
        Dispose(lvData);
      end;
    end else
    begin
      Break;
    end;
  end;
end;

{ TSimpleQueue }

//procedure TSimpleQueue.AddToHead(AData: Pointer);
//var
//  lvTemp:PQueueData;
//begin
//  lvTemp := queueDataPool.Pop;
//  lvTemp.Data := AData;
//  InnerAddToHead(lvTemp);
//end;

procedure TSimpleQueue.Clear;
var
  ANext: PQueueData;
begin
  while FHead <> nil do
  begin
    ANext := FHead.Next;
    __ReleaseQueueData(FHead);

    {$IFDEF USE_QUEUE_POOL}
    queueDataPool.Push(FHead);
    {$ELSE}
    Dispose(FHead);
    {$ENDIF}
    FHead := ANext;
  end;
  FTail := nil;
  FHead := nil;
  FCount := 0; 
end;

procedure TSimpleQueue.FreeDataObject;
var
  lvData:Pointer;
begin
  while True do
  begin
    lvData := nil;
    if DeQueue(lvData) then
    begin
      if lvData = nil then
      begin
        lvData := nil;
      end else
      begin
        TObject(lvData).Free;
      end;
    end else
    begin
      Break;
    end;
  end;
end;

function TSimpleQueue.IsEmpty: Boolean;
begin
  Result := (FHead = nil);
end;

function TSimpleQueue.DeQueue: Pointer;
var
  lvTemp:PQueueData;
begin
  Result := nil;
  lvTemp := InnerPop;
  if lvTemp <> nil then
  begin
    Result := lvTemp.Data;
    {$IFDEF USE_QUEUE_POOL}
    lvTemp.Data := nil;
    queueDataPool.Push(lvTemp);
    {$ELSE}
    Dispose(lvTemp);
    {$ENDIF}

  end;
end;

function TSimpleQueue.DeQueue(var outPointer:Pointer): Boolean;
var
  lvTemp:PQueueData;
begin
  Result := false;
  lvTemp := InnerPop;
  if lvTemp <> nil then
  begin
    outPointer := lvTemp.Data;
    {$IFDEF USE_QUEUE_POOL}
    queueDataPool.Push(lvTemp);
    {$ELSE}
    Dispose(lvTemp);
    {$ENDIF}
    Result := true;
  end;
end;

procedure TSimpleQueue.EnQueue(AData: Pointer; pvReleaseAction:
    TDataReleaseAction = raNone);
var
  lvTemp:PQueueData;
begin
{$IFDEF USE_QUEUE_POOL}
  lvTemp := queueDataPool.Pop;
{$ELSE}
  New(lvTemp);
{$ENDIF}
  lvTemp.Data := AData;
  lvTemp.AsObject := nil;
  lvTemp.ReleaseAction := pvReleaseAction;
  InnerAddToTail(lvTemp);
end;

function TSimpleQueue.Size: Integer;
begin
  Result := FCount;
end;

function TSimpleQueue.InnerPop: PQueueData;
begin
  Result := FHead;
  if Result <> nil then
  begin
    FHead := Result.Next;

    if FHead = nil then FTail := nil;

    Dec(FCount);

  {$IFDEF DEBUG_ON}
    Inc(FPopCounter);
  {$ENDIF}
  end;
end;

procedure TSimpleQueue.InnerAddToTail(AData: PQueueData);
begin
  AData.Next := nil;
  if FTail = nil then
    FHead := AData
  else
  begin
    FTail.Next := AData;
  end;

  FTail := AData;
  Inc(FCount);

  {$IFDEF DEBUG_ON}
  Inc(FPushCounter);
  {$ENDIF}

end;


initialization
  queueDataPool := TQueueDataPool.Create(819200);

finalization
  queueDataPool.Free;


end.
