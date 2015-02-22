(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
 
unit utils.queues;

interface

uses
  SyncObjs;

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

type
  PQueueData = ^TQueueData;
  TQueueData = record
    Data: Pointer;
    Next: PQueueData;
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

    /// <summary>
    ///   清空所有数据
    /// </summary>
    procedure clear;
    function InnerDeQueue: PQueueData;
    procedure InnerAddToTail(AData: PQueueData);
    procedure InnerAddToHead(AData: PQueueData);
  public
    constructor Create;
    destructor Destroy; override;
    function IsEmpty: Boolean;

    function Size: Integer;

    function DeQueue: Pointer; overload;
    function DeQueue(var outPointer:Pointer):Boolean;overload;

    /// <summary>
    ///  入队列
    /// </summary>
    procedure EnQueue(AData: Pointer);

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

    /// <summary>
    ///   清空所有数据
    /// </summary>
    procedure Clear;
    function InnerPop: PQueueData;
    procedure InnerAddToTail(AData: PQueueData);
    procedure InnerAddToHead(AData: PQueueData);
  public
    constructor Create;
    destructor Destroy; override;
    function IsEmpty: Boolean;

    function Size: Integer;

    function DeQueue: Pointer; overload;
    function DeQueue(var outPointer:Pointer): Boolean; overload;

    /// <summary>
    ///   add to tail
    /// </summary>
    procedure EnQueue(AData: Pointer);

//    /// <summary>
//    ///   add to head
//    /// </summary>
//    procedure AddToHead(AData: Pointer);

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

constructor TBaseQueue.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
  FHead := nil;
  FTail := nil;
  FCount := 0;
  FName := 'utils.queues';
end;

destructor TBaseQueue.Destroy;
begin
  {$IFDEF DEBUG_ON}
  if IsDebugMode then
    Assert(FPopCounter = FPushCounter, ('[' + FName + ']PopCounter <> PushCounter'));

  {$ENDIF}

  Clear;
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

procedure TBaseQueue.clear;
var
  ANext: PQueueData;
begin
  FLocker.Enter;
  try
    if FHead = nil then Exit;

    while FHead.Next <> nil do
    begin
      ANext := FHead.Next;     
      
      queueDataPool.Push(FHead);
      FHead := ANext;
    end;

    FCount := 0;
  finally
    FLocker.Leave;
  end;
end;

procedure TBaseQueue.freeDataObject;
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
  Result := (FHead.next = nil);
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
    queueDataPool.Push(lvTemp);
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
    queueDataPool.Push(lvTemp);
    Result := true;
  end;
end;

procedure TBaseQueue.EnQueue(AData: Pointer);
var
  lvTemp:PQueueData;
begin
  lvTemp := queueDataPool.Pop;
  lvTemp.Data := AData;
  InnerAddToTail(lvTemp);
end;

function TBaseQueue.Size: Integer;
begin
  Result := FCount;
end;

procedure TBaseQueue.InnerAddToHead(AData: PQueueData);
begin
  FLocker.Enter;
  try
    AData.Next := FHead;
    FHead := AData;
    if FTail = nil then FTail := FHead;

    Inc(FCount);

    {$IFDEF DEBUG_ON}
      Inc(FPushCounter);
    {$ENDIF}
  finally
    FLocker.Leave;
  end;
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
begin
  inherited Create;
  FSize := AMaxSize;
  FLocker := TCriticalSection.Create;
end;

destructor TQueueDataPool.Destroy;
var
  lvData: PQueueData;
begin
  {$IFDEF DEBUG_ON}
  if IsDebugMode then
    Assert(FPopCounter = FPushCounter, ('PopCounter <> PushCounter'));
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
  Result := FFirst;
  if Result <> nil then
  begin
    FFirst := Result.Next;
    Dec(FCount);
  end;
  {$IFDEF DEBUG_ON}
    Inc(FPopCounter);
  {$ENDIF}
  FLocker.Leave;

  if Result = nil then
    GetMem(Result, SizeOf(TQueueData));
  Result.Data := nil;
  Result.Next := nil;
end;

procedure TQueueDataPool.Push(pvQueueData: PQueueData);
var
  ADoFree: Boolean;
begin
  Assert(pvQueueData <> nil);

  FLocker.Enter;
  ADoFree := (FCount = FSize);
  if not ADoFree then
  begin
    pvQueueData.Next := FFirst;
    FFirst := pvQueueData;
    Inc(FCount);
  end;
  {$IFDEF DEBUG_ON}
    Inc(FPushCounter);
  {$ENDIF}
  FLocker.Leave;
  
  if ADoFree then
  begin
    FreeMem(pvQueueData);
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
  if FHead = nil then Exit;

  while FHead.Next <> nil do
  begin
    ANext := FHead.Next;

    queueDataPool.Push(FHead);
    FHead := ANext;
  end;

  FCount := 0;

end;

procedure TSimpleQueue.freeDataObject;
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
  Result := (FHead.next = nil);
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
    queueDataPool.Push(lvTemp);
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
    queueDataPool.Push(lvTemp);
    Result := true;
  end;
end;

procedure TSimpleQueue.EnQueue(AData: Pointer);
var
  lvTemp:PQueueData;
begin
  lvTemp := queueDataPool.Pop;
  lvTemp.Data := AData;
  InnerAddToTail(lvTemp);
end;

function TSimpleQueue.Size: Integer;
begin
  Result := FCount;
end;

procedure TSimpleQueue.InnerAddToHead(AData: PQueueData);
begin
  AData.Next := FHead;
  FHead := AData;
  if FTail = nil then FTail := FHead;
  Inc(FCount);

  {$IFDEF DEBUG_ON}
    Inc(FPushCounter);
  {$ENDIF}

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
  queueDataPool := TQueueDataPool.Create(10240);

finalization
  queueDataPool.Free;


end.
