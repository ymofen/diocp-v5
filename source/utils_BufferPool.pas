(*
 * 内存池单元
 *   内存块通过引用计数，归还到池
 *
*)

unit utils_BufferPool;

interface

{$DEFINE USE_SPINLOCK}

/// 如果不进行sleep会跑满cpu。
/// spinlock 交换失败时会执行sleep
{$DEFINE SPINLOCK_SLEEP}

/// 使用内存池
{$DEFINE USE_MEM_POOL}

uses
  SyncObjs, SysUtils, Classes
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}

  {$ENDIF}, Math;

{$IFNDEF DEBUG}     // INLINE不好调试
{$IF defined(FPC) or (RTLVersion>=18))}
  {$DEFINE HAVE_INLINE}
{$IFEND HAVE_INLINE}
{$ENDIF}

// 进行调试
{.$UNDEF HAVE_INLINE}
{.$DEFINE DIOCP_DEBUG_HINT}

{$IFDEF DIOCP_HIGH_SPEED}
  {$UNDEF DIOCP_DEBUG}
{$ENDIF}


const
  block_flag :Word = $1DFB;

{$IFDEF DEBUG}
  protect_size = 8;
{$ELSE}
  // 如果增加 则初始化时要进行填充
  protect_size = 0;
{$ENDIF}

{$IFDEF DIOCP_DEBUG_HINT}
  BLOCK_DEBUG_HINT_LENGTH = 128;
{$ENDIF}

type
 
  PBufferPool = ^ TBufferPool;
  PBufferBlock = ^TBufferBlock;
  
  TBufferPool = record
    FBlockSize: Integer;
    FHead:PBufferBlock;
    FGet:Integer;
    FPut:Integer;
    FSize:Integer;
    FAddRef:Integer;
    FReleaseRef:Integer;

    FPoolSize:Integer;

    {$IFDEF USE_SPINLOCK}
    FSpinLock:Integer;
    FLockWaitCounter: Integer;
    {$ELSE}
    FLocker:TCriticalSection;
    {$ENDIF}

    FName:String;

  end;



  TBufferBlock = record
    flag: Word;

    refcounter :Integer;
    next: PBufferBlock;
    owner: PBufferPool;
    data: Pointer;
    data_free_type:Byte; // 0

    {$IFDEF DIOCP_DEBUG_HINT}
    __debug_lock:Integer;
    __debug_hint:array[0..BLOCK_DEBUG_HINT_LENGTH -1] of Char;
    __debug_hint_pos:Integer;
    {$ENDIF}
    __debug_flag:Byte;

  end;


  TBufferNotifyEvent = procedure(pvSender: TObject; pvBuffer: Pointer; pvLength:
      Integer) of object;
  TBlockBuffer = class(TObject)
  private
    {$IFDEF USE_SPINLOCK}
    FSpinLock:Integer;
    FLockWaitCounter: Integer;
    {$ELSE}
    FLocker:TCriticalSection;
    {$ENDIF}
    FBlockSize: Integer;
    FThreadID: THandle;
    FSize:Integer;
    FPosition:Integer;
    FBuffer: Pointer;
    FBufferPtr:PByte;
    FBufferPool: PBufferPool;
    FOnBufferWrite: TBufferNotifyEvent;
    procedure CheckBlockBuffer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
  public
    constructor Create(ABufferPool: PBufferPool);
    procedure SetBufferPool(ABufferPool: PBufferPool);
    procedure Append(pvBuffer:Pointer; pvLength:Integer);{$IFDEF HAVE_INLINE} inline;{$ENDIF}
    destructor Destroy; override;

    procedure Lock();
    procedure UnLock();
    procedure CheckThreadIn;
    procedure CheckThreadOut;
    procedure CheckThreadNone;
    procedure CheckIsCurrentThread;
    procedure FlushBuffer();
    procedure ClearBuffer();
    property OnBufferWrite: TBufferNotifyEvent read FOnBufferWrite write FOnBufferWrite;
  end;

const
  BLOCK_HEAD_SIZE = SizeOf(TBufferBlock);

  FREE_TYPE_NONE = 0;
  FREE_TYPE_FREEMEM = 1;
  FREE_TYPE_DISPOSE = 2;
  FREE_TYPE_OBJECTFREE = 3;
  FREE_TYPE_OBJECTNONE = 4;


function NewBufferPool(pvBlockSize: Integer = 1024; pvPoolSize:Integer = 0):
    PBufferPool;
procedure FreeBufferPool(buffPool:PBufferPool);
procedure ClearBufferPool(buffPool:PBufferPool);

function GetBuffer(ABuffPool:PBufferPool): PByte;{$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;

// 获取一块内存, 可以通过AddRef和ReleaseRef进行引用计数释放
function GetBuffer(pvSize:Integer): Pointer;{$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;

procedure FreeBuffer(const pvBuffer:PByte; const pvHint: string; pvReleaseAttachDataAtEnd:Boolean=True);overload; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
procedure FreeBuffer(const pvBuffer:PByte; pvReleaseAttachDataAtEnd:Boolean=True);overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}

/// <summary>
///   对内存块进行引用计数
/// </summary>
function AddRef(const pvBuffer:PByte; const pvHint: string): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AddRef(const pvBuffer:PByte): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}

/// <summary>
///   减少对内存块的引用
///   为0时，释放data数据
/// </summary>
function ReleaseRef(const pvBuffer: PByte; const pvHint: string): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function ReleaseRef(const pvBuffer: PByte): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function ReleaseRef(const pvBuffer: Pointer; pvReleaseAttachDataAtEnd: Boolean;
    const pvHint: string): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}


/// <summary>
///   附加一个数据
/// </summary>
procedure AttachData(pvBuffer, pvData: Pointer; pvFreeType: Byte);

/// <summary>
///   获取附加的数据
///   0:成功
///   1:没有
/// </summary>
function GetAttachData(pvBuffer: Pointer; var X: Pointer): Integer;


function GetAttachDataAsObject(pvBuffer:Pointer): TObject;

/// <summary>
///  检测池中内存块越界情况
/// </summary>
function CheckBufferBounds(ABuffPool:PBufferPool): Integer;

/// <summary>
///   检测单个内存块是否越界
/// </summary>
function CheckBlockBufferBounds(pvBuffer: Pointer): Integer;

{$IF RTLVersion<24}
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AtomicIncrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AtomicDecrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
{$IFEND <XE5}



//procedure SpinLock(var Target:Integer; var WaitCounter:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
//procedure SpinLock(var Target:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
//procedure SpinUnLock(var Target:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF}overload;


{$if CompilerVersion < 18} //before delphi 2007
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall; external kernel32 name 'InterlockedCompareExchange';
{$EXTERNALSYM InterlockedCompareExchange}
{$ifend}

procedure FreeObject(AObject: TObject); {$IFDEF HAVE_INLINE} inline;{$ENDIF}

procedure PrintDebugString(s:string); {$IFDEF HAVE_INLINE} inline;{$ENDIF}

function GetBufferPoolDebugInfo(ABuffPool:PBufferPool): string;




implementation

const
  STRING_EMPTY:String = '';



{$IFDEF DIOCP_DEBUG}
var
  __debug_dna:Integer;
{$ENDIF}






procedure PushABlockBuffer(const pvBufBlock: PBufferBlock; const pvOwner:
    PBufferPool); {$IFDEF HAVE_INLINE} inline;{$ENDIF}
var
  lvBuffer :PBufferBlock;
begin
  lvBuffer := pvOwner.FHead;
  pvBufBlock.next := lvBuffer;
  pvOwner.FHead := pvBufBlock;
end;

procedure CheckIntializePool(const pvBufPool:PBufferPool); {$IFDEF HAVE_INLINE} inline;{$ENDIF}
var
  i: Integer;
  lvBuffer:PBufferBlock;
begin
  if pvBufPool.FPoolSize = 0 then Exit;

  for i := 0 to pvBufPool.FPoolSize -1 do
  begin
    // + 2保护边界(可以检测内存越界写入)
    GetMem(lvBuffer, BLOCK_HEAD_SIZE + pvBufPool.FBlockSize + protect_size);
    {$IFDEF DEBUG}
    FillChar(lvBuffer^, BLOCK_HEAD_SIZE + pvBufPool.FBlockSize + protect_size, 0);
    {$ELSE}
    FillChar(lvBuffer^, BLOCK_HEAD_SIZE, 0);
    {$ENDIF}
    lvBuffer.owner := pvBufPool;
    lvBuffer.flag := block_flag;
    lvBuffer.__debug_flag := 0;

    {$IFDEF DIOCP_DEBUG_HINT}
    lvBuffer.__debug_lock := 0;
    lvBuffer.__debug_hint_pos := 0;
    {$ENDIF}

    PushABlockBuffer(lvBuffer, pvBufPool);


    AtomicIncrement(pvBufPool.FSize);
  end;
end;

procedure FreeObject(AObject: TObject);
begin
{$IFDEF AUTOREFCOUNT}
  AObject.DisposeOf;
{$ELSE}
  AObject.Free;
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

{$IFDEF DIOCP_DEBUG_HINT}
procedure InnerAddBlockHint(const pvBlock:PBufferBlock; const pvHint:string); {$IFDEF HAVE_INLINE} inline;{$ENDIF}
var
  lvPtr:PChar;
  l:Integer;
  lvStr:string;
begin
  if Length(pvHint) = 0 then Exit;
  lvPtr := PChar(@pvBlock.__debug_hint[0]);
  SpinLock(pvBlock.__debug_lock);
  try
    if ((pvBlock.__debug_hint_pos + 1 + 2) >= BLOCK_DEBUG_HINT_LENGTH) then
    begin
      pvBlock.__debug_hint_pos := 0;
    end;

    if pvBlock.__debug_hint_pos > 0 then
    begin
      Inc(lvPtr, pvBlock.__debug_hint_pos);
      lvPtr^ := #13;
      Inc(lvPtr);
      lvPtr^ := #10;
      Inc(lvPtr);
      Inc(pvBlock.__debug_hint_pos, 2);
    end;

    lvStr := IntToStr(pvBlock.refcounter) + ':';
    l := Length(lvStr);
    Move(PChar(lvStr)^, lvPtr^, l);
    Inc(lvPtr, l);
    Inc(pvBlock.__debug_hint_pos, l);

    l := Length(pvHint);
    if l > (BLOCK_DEBUG_HINT_LENGTH - pvBlock.__debug_hint_pos) - 1  then
    begin
      l := (BLOCK_DEBUG_HINT_LENGTH - pvBlock.__debug_hint_pos) - 1;
    end;
    Move(PChar(pvHint)^, lvPtr^, l);
    Inc(lvPtr, l);
    Inc(pvBlock.__debug_hint_pos, l);

    lvPtr^ := #0;
  finally
    SpinUnLock(pvBlock.__debug_lock);
  end;
end;
{$ENDIF}

procedure ReleaseAttachData(pvBlock:PBufferBlock); {$IFDEF HAVE_INLINE} inline;{$ENDIF}
begin
  if pvBlock.data <> nil then
  begin
    case pvBlock.data_free_type of
      0: ;
      1: FreeMem(pvBlock.data);
      2: Dispose(pvBlock.data);
      3: FreeObject(pvBlock.data);
{$IFDEF AUTOREFCOUNT}
      FREE_TYPE_OBJECTNONE: TObject(pvBlock.data).__ObjRelease;
{$ENDIF}

    else
      begin
        if pvBlock.owner <> nil then
        begin
          Assert(False, Format('BufferBlock[%s] unkown data free type:%d', [pvBlock.owner.FName, pvBlock.data_free_type]));
        end else
        begin
          Assert(False, Format('BufferBlock unkown data free type:%d', [pvBlock.data_free_type]));
        end;
      end;
    end;
    pvBlock.data := nil;
  end;   
end;

procedure SpinLock(var Target:Integer; var WaitCounter:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
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
    Sleep(1);    // 1 对比0 (线程越多，速度越平均)
    {$ENDIF}
  end;
end;

procedure SpinLock(var Target:Integer);{$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
begin
  while AtomicCmpExchange(Target, 1, 0) <> 0 do
  begin
    {$IFDEF SPINLOCK_SLEEP}
    Sleep(1);    // 1 对比0 (线程越多，速度越平均)
    {$ENDIF}
  end;
end;


procedure SpinUnLock(var Target:Integer);{$IFDEF HAVE_INLINE} inline;{$ENDIF}
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

{$IFDEF DEBUG}
/// <summary>
///   检测一块内存是否有越界情况
///   false 有越界清空
/// </summary>
function CheckBufferBlockBounds(ABlock: PBufferBlock): Boolean;
var
  lvBuffer:PByte;
  i:Integer;
begin
  Result := True;
  lvBuffer:= PByte(ABlock);
  Inc(lvBuffer, BLOCK_HEAD_SIZE + ABlock.owner.FBlockSize);

  for I := 0 to protect_size - 1 do
  begin
    if lvBuffer^ <> 0 then
    begin
      Result := False;
      Break;
    end;
    Inc(lvBuffer);
  end;

end;
{$ENDIF}

function GetBuffer(ABuffPool:PBufferPool): PByte;
var
  lvBuffer:PBufferBlock;
begin

  {$IFDEF USE_MEM_POOL}
  if ABuffPool.FPoolSize > 0 then
  begin
    {$IFDEF USE_SPINLOCK}
    SpinLock(ABuffPool.FSpinLock, ABuffPool.FLockWaitCounter);
    {$ELSE}
    ABuffPool.FLocker.Enter;
    {$ENDIF}

    // 获取一个节点
    lvBuffer := PBufferBlock(ABuffPool.FHead);
    if lvBuffer <> nil then ABuffPool.FHead := lvBuffer.next;


    {$IFDEF USE_SPINLOCK}
    SpinUnLock(ABuffPool.FSpinLock);
    {$ELSE}
    ABuffPool.FLocker.Leave;
    {$ENDIF}
  end else
  begin
    lvBuffer := nil;
  end;
  {$ELSE}
  lvBuffer := nil;
  {$ENDIF}


  if lvBuffer = nil then
  begin
    // + 2保护边界(可以检测内存越界写入)
    GetMem(Result, BLOCK_HEAD_SIZE + ABuffPool.FBlockSize + protect_size);
    {$IFDEF DEBUG}
    FillChar(Result^, BLOCK_HEAD_SIZE + ABuffPool.FBlockSize + protect_size, 0);
    {$ELSE}
    FillChar(Result^, BLOCK_HEAD_SIZE, 0);
    {$ENDIF}
    lvBuffer := PBufferBlock(Result);
    lvBuffer.owner := ABuffPool;
    lvBuffer.flag := block_flag;
    lvBuffer.__debug_flag := 0;

    {$IFDEF DIOCP_DEBUG_HINT}
    lvBuffer.__debug_lock := 0;
    lvBuffer.__debug_hint_pos := 0;
    {$ENDIF}


    AtomicIncrement(ABuffPool.FSize);
  end else
  begin
    Result := PByte(lvBuffer);
    Assert(lvBuffer.__debug_flag = 0, '多线程抢占， 混乱');
  end;

  lvBuffer.__debug_flag := 1;

  {$IFDEF DIOCP_DEBUG_HINT}
  InnerAddBlockHint(lvBuffer, '* GetBuffer');
  {$ENDIF}

  Inc(Result, BLOCK_HEAD_SIZE);
  AtomicIncrement(ABuffPool.FGet);
end;

/// <summary>
///  释放内存块到Owner的列表中
/// </summary>
procedure InnerFreeBuffer(pvBufBlock: PBufferBlock; const pvHint: string);{$IFDEF HAVE_INLINE} inline;{$ENDIF}
var
  lvBuffer:PBufferBlock;
  lvOwner:PBufferPool;
begin
  if pvBufBlock.__debug_flag = 0 then
  begin
    Assert(pvBufBlock.__debug_flag <> 0, '多次归还');
  end;
  pvBufBlock.__debug_flag := 0;

  {$IFDEF DIOCP_DEBUG_HINT}
  InnerAddBlockHint(pvBufBlock, '# FreeBuff' + pvHint);
  {$ENDIF}

  lvOwner := pvBufBlock.owner;
  if lvOwner = nil then
  begin
    ReleaseAttachData(pvBufBlock);
    FreeMem(pvBufBlock);
  end else
  begin
    {$IFDEF USE_MEM_POOL}
    if lvOwner.FPoolSize > 0 then
    begin
      {$IFDEF USE_SPINLOCK}
      SpinLock(lvOwner.FSpinLock, lvOwner.FLockWaitCounter);
      {$ELSE}
      lvOwner.FLocker.Enter;
      {$ENDIF}
      PushABlockBuffer(pvBufBlock, lvOwner);
  //    lvBuffer := lvOwner.FHead;
  //    pvBufBlock.next := lvBuffer;
  //    lvOwner.FHead := pvBufBlock;
      {$IFDEF USE_SPINLOCK}
      SpinUnLock(lvOwner.FSpinLock);
      {$ELSE}
      lvOwner.FLocker.Leave;
      {$ENDIF}
    end else
    begin
      ReleaseAttachData(pvBufBlock);
      FreeMem(pvBufBlock);
      AtomicDecrement(lvOwner.FSize);
    end;
    {$ELSE}
    ReleaseAttachData(pvBufBlock);
    FreeMem(pvBufBlock);
    AtomicDecrement(lvOwner.FSize);
    {$ENDIF}
    AtomicIncrement(lvOwner.FPut);
  end;
end;

function AddRef(const pvBuffer:PByte): Integer;
begin
  Result := AddRef(pvBuffer, '');
end;

function AddRef(const pvBuffer:PByte; const pvHint: string): Integer;
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock;
begin
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_HEAD_SIZE);
  lvBlock := PBufferBlock(lvBuffer);

  if lvBlock.__debug_flag = 0 then
  begin
    Assert(lvBlock.__debug_flag <> 0, '已经归还, 不能进行AddRef');
  end;

  Assert(lvBlock.flag = block_flag, 'Invalid DBufferBlock');
  Result := AtomicIncrement(lvBlock.refcounter);
  if lvBlock.owner <> nil then
  begin
    AtomicIncrement(lvBlock.owner.FAddRef);
  end;

  {$IFDEF DIOCP_DEBUG_HINT}
  InnerAddBlockHint(lvBlock, pvHint);
  {$ENDIF}
  
  // 不加会被优化掉(DX10)
  Assert(Result > 0, 'error');
end;

function ReleaseRef(const pvBuffer: PByte): Integer;
begin
  Result := ReleaseRef(pvBuffer, True, '');
end;

function ReleaseRef(const pvBuffer: PByte; const pvHint: string): Integer;
begin
  Result := ReleaseRef(pvBuffer, True, pvHint);
end;

function ReleaseRef(const pvBuffer: Pointer; pvReleaseAttachDataAtEnd: Boolean;
    const pvHint: string): Integer;
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock;
begin
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_HEAD_SIZE);
  lvBlock := PBufferBlock(lvBuffer);
  if lvBlock.flag <>  block_flag then
  begin
    Assert(lvBlock.flag = block_flag, 'Invalid DBufferBlock');
  end;
  Result := AtomicDecrement(lvBlock.refcounter);
  if lvBlock.owner <> nil then
  begin
    AtomicIncrement(lvBlock.owner.FReleaseRef);
  end;

  {$IFDEF DIOCP_DEBUG_HINT}
  InnerAddBlockHint(lvBlock, pvHint);
  {$ENDIF}

  //if lvBlock.refcounter = 0 then
  if Result = 0 then  
  begin
    if pvReleaseAttachDataAtEnd then
      ReleaseAttachData(lvBlock);
    InnerFreeBuffer(lvBlock, pvHint);
  end else if Result < 0 then
  begin          // error(不能小于0，如果小于0，则出现了严重问题)
    Assert(Result >= 0, Format('DBuffer error release ref:%d', [lvBlock.refcounter]));
  end;
end;

function NewBufferPool(pvBlockSize: Integer = 1024; pvPoolSize:Integer = 0):
    PBufferPool;
begin
  New(Result);
  Result.FBlockSize := pvBlockSize;
  Result.FHead := nil;
  {$IFDEF USE_SPINLOCK}
  Result.FSpinLock := 0;
  Result.FLockWaitCounter := 0;
  {$ELSE}
  Result.FLocker := TCriticalSection.Create;
  {$ENDIF}

  Result.FGet := 0;
  Result.FSize := 0;
  Result.FPut := 0;
  Result.FAddRef := 0;
  Result.FReleaseRef :=0;
  Result.FPoolSize := pvPoolSize;

  CheckIntializePool(Result);

end;

procedure FreeBufferPool(buffPool:PBufferPool);
var
  lvBlock, lvNext:PBufferBlock;
begin
  Assert(buffPool.FGet = buffPool.FPut,
    Format('BufferPool-%s Leak, get:%d, put:%d', [buffPool.FName, buffPool.FGet, buffPool.FPut]));

  lvBlock := buffPool.FHead;
  while lvBlock <> nil do
  begin
    lvNext := lvBlock.next;
    ReleaseAttachData(lvBlock);
    FreeMem(lvBlock);
    lvBlock := lvNext;
  end;
  {$IFDEF USE_SPINLOCK}
  ;
  {$ELSE}
  buffPool.FLocker.Free;
  {$ENDIF}

  Dispose(buffPool);
end;

function CheckBufferBounds(ABuffPool:PBufferPool): Integer;
{$IFDEF DEBUG}
var
  lvBlock:PBufferBlock;
{$ENDIF}
begin
  {$IFNDEF DEBUG}
  Result := -1;
  {$ELSE}
  if protect_size = 0 then
  begin   // 没有保护边界的大小
    Result := -1;
    Exit;
  end;
  Result := 0;
  {$IFDEF USE_SPINLOCK}
  SpinLock(ABuffPool.FSpinLock, ABuffPool.FLockWaitCounter);
  {$ELSE}
  ABuffPool.FLocker.Enter;
  {$ENDIF}
  lvBlock := ABuffPool.FHead;
  while lvBlock <> nil do
  begin
    if not CheckBufferBlockBounds(lvBlock) then Inc(Result);

    lvBlock := lvBlock.next;
  end;
  {$IFDEF USE_SPINLOCK}
  SpinUnLock(ABuffPool.FSpinLock);
  {$ELSE}
  ABuffPool.FLocker.Leave;
  {$ENDIF}
  {$ENDIF}
end;

procedure AttachData(pvBuffer, pvData: Pointer; pvFreeType: Byte);
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock;
begin
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_HEAD_SIZE);
  lvBlock := PBufferBlock(lvBuffer);
  Assert(lvBlock.flag = block_flag, 'invalid DBufferBlock');

  ReleaseAttachData(lvBlock);

  lvBlock.data := pvData;
  lvBlock.data_free_type := pvFreeType;

{$IFDEF AUTOREFCOUNT}
  if pvFreeType in [FREE_TYPE_OBJECTFREE, FREE_TYPE_OBJECTNONE] then
    TObject(pvData).__ObjAddRef();
{$ENDIF}
end;

function GetAttachData(pvBuffer: Pointer; var X: Pointer): Integer;
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock;
begin
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_HEAD_SIZE);
  lvBlock := PBufferBlock(lvBuffer);
  Assert(lvBlock.flag = block_flag, 'invalid DBufferBlock');

  if lvBlock.data <> nil then
  begin
    X := lvBlock.data;
    Result := 0;
  end else
  begin
    Result := -1;
  end;
end;

function GetAttachDataAsObject(pvBuffer:Pointer): TObject;
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock;
begin
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_HEAD_SIZE);
  lvBlock := PBufferBlock(lvBuffer);
  Assert(lvBlock.flag = block_flag, 'invalid DBufferBlock');

  if lvBlock.data <> nil then
  begin
    Result :=TObject(lvBlock.data);
  end else
  begin
    Result := nil;
  end;
end;


procedure FreeBuffer(const pvBuffer:PByte; pvReleaseAttachDataAtEnd:Boolean=True);overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
begin
  FreeBuffer(pvBuffer, '', pvReleaseAttachDataAtEnd);
end;

procedure FreeBuffer(const pvBuffer:PByte; const pvHint: string; pvReleaseAttachDataAtEnd:Boolean);
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock;
begin
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_HEAD_SIZE);
  lvBlock := PBufferBlock(lvBuffer);
  Assert(lvBlock.flag = block_flag, 'invalid DBufferBlock');
  {$IFDEF DEBUG}
  Assert(lvBlock.refcounter = 0, Format('DBufferBlock:: buffer is in use, refcount:%d', [lvBlock.refcounter]));
  {$ENDIF}
  if pvReleaseAttachDataAtEnd then
    ReleaseAttachData(lvBlock);
  InnerFreeBuffer(lvBlock, pvHint);
end;

function CheckBlockBufferBounds(pvBuffer: Pointer): Integer;
{$IFDEF DEBUG}
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock;
  ABuffPool:PBufferPool;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_HEAD_SIZE);
  lvBlock := PBufferBlock(lvBuffer);
  Assert(lvBlock.flag = block_flag, 'invalid DBufferBlock');

  if protect_size = 0 then
  begin   // 没有保护边界的大小
    Result := -1;
    Exit;
  end;
  Result := 0;
  ABuffPool := lvBlock.owner;
  Assert(ABuffPool <> nil);
  {$IFDEF USE_SPINLOCK}
  SpinLock(ABuffPool.FSpinLock, ABuffPool.FLockWaitCounter);
  {$ELSE}
  ABuffPool.FLocker.Enter;
  {$ENDIF}
  if not CheckBufferBlockBounds(lvBlock) then Result := 1;  
  {$IFDEF USE_SPINLOCK}
  SpinUnLock(ABuffPool.FSpinLock);
  {$ELSE}
  ABuffPool.FLocker.Leave;
  {$ENDIF}
  {$ELSE}
  Result := -1;
  {$ENDIF}
end;

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

procedure ClearBufferPool(buffPool:PBufferPool);
var
  lvBlock, lvNext:PBufferBlock;
begin
  Assert(buffPool.FGet = buffPool.FPut,
    Format('DBuffer-%s Leak, get:%d, put:%d', [buffPool.FName, buffPool.FGet, buffPool.FPut]));
  {$IFDEF USE_SPINLOCK}
  SpinLock(buffPool.FSpinLock, buffPool.FLockWaitCounter);
  {$ELSE}
  buffPool.FLocker.Enter;
  {$ENDIF}
  
  lvBlock := buffPool.FHead;
  while lvBlock <> nil do
  begin
    lvNext := lvBlock.next;
    ReleaseAttachData(lvBlock);
    FreeMem(lvBlock);
    lvBlock := lvNext;
  end;
  
  buffPool.FHead := nil;
  buffPool.FSize := 0;
  {$IFDEF USE_SPINLOCK}
  SpinUnLock(buffPool.FSpinLock);
  {$ELSE}
  buffPool.FLocker.Leave;
  {$ENDIF}


end;

function GetBufferPoolDebugInfo(ABuffPool:PBufferPool): string;
begin
  Result := Format('name:%s, get:%d, put:%d, addRef:%d, releaseRef:%d, size:%d', [ABuffPool.FName, ABuffPool.FGet, ABuffPool.FPut, ABuffPool.FAddRef, ABuffPool.FReleaseRef, ABuffPool.FSize]);;
end;

function GetBuffer(pvSize:Integer): Pointer;
var
  rval:PByte;
  lvBuffer:PBufferBlock;
begin
  // + 2保护边界(可以检测内存越界写入)
  GetMem(rval, BLOCK_HEAD_SIZE + pvSize + protect_size);
  {$IFDEF DEBUG}
  FillChar(rval^, BLOCK_HEAD_SIZE + pvSize + protect_size, 0);
  {$ELSE}
  FillChar(rval^, BLOCK_HEAD_SIZE, 0);
  {$ENDIF}
  lvBuffer := PBufferBlock(rval);
  lvBuffer.owner := nil;
  lvBuffer.flag := block_flag;
  lvBuffer.__debug_flag := 1;

  {$IFDEF DIOCP_DEBUG_HINT}
  lvBuffer.__debug_lock := 0;
  lvBuffer.__debug_hint_pos := 0;
  {$ENDIF}

  Inc(rval, BLOCK_HEAD_SIZE);
  Result := rval; 
end;





procedure TBlockBuffer.Append(pvBuffer: Pointer; pvLength: Integer);
var
  l, r:Integer;
  lvBuff:PByte;
begin  
  lvBuff := PByte(pvBuffer);
  r := pvLength;
  while r > 0 do
  begin
    CheckBlockBuffer;
    if FPosition + r > FBlockSize then l := FBlockSize - FPosition else l := r;

    Move(lvBuff^, FBufferPtr^, l);
    Dec(r, l);
    Inc(lvBuff, l);
    Inc(FBufferPtr, l);
    Inc(FPosition, l);
    Inc(FSize, l);
    if FPosition = FBlockSize then
    begin
      FlushBuffer;
    end else if FPosition > FBlockSize then
    begin            // 越界
      Assert(false, Format('TBlockBuffer.Append bug :: pos:%d, block:%d', [FPosition, FBlockSize]));
    end;
  end;
end;

constructor TBlockBuffer.Create(ABufferPool: PBufferPool);
begin
  inherited Create;
  SetBufferPool(ABufferPool);
  {$IFDEF USE_SPINLOCK}
  FSpinLock := 0;
  FLockWaitCounter := 0;
  {$ELSE}
  FLocker := TCriticalSection.Create;
  {$ENDIF}
end;

destructor TBlockBuffer.Destroy;
begin
  FlushBuffer;

  {$IFDEF USE_SPINLOCK}
  ;
  {$ELSE}
  FLocker.Free;
  {$ENDIF}

  inherited Destroy;
end;

procedure TBlockBuffer.FlushBuffer;
{$IF Defined(DIOCP_DEBUG) or Defined(DEBUG)}
var
  r, n:Integer;
{$IFEND}
begin
  if FBuffer = nil then Exit;
  try
    if (Assigned(FOnBufferWrite) and (FSize > 0)) then
    begin
      {$IFDEF DIOCP_DEBUG}n := AtomicIncrement(__debug_dna){$ENDIF};
      {$IFDEF DIOCP_DEBUG}r := {$ENDIF}AddRef(FBuffer{$IFDEF DIOCP_DEBUG}, Format('+ FlushBuffer(%d)', [n]){$ENDIF});    // 避免事件中没有使用引用计数，不释放buf
      try
        {$IFNDEF BIG_CONCURRENT}
        {$IFDEF DIOCP_DEBUG}PrintDebugString(Format('+ FlushBuffer %2x: %d', [Cardinal(FBuffer), r]));{$ENDIF}
        {$ENDIF}
        FOnBufferWrite(self, FBuffer, FSize);
      finally
        ReleaseRef(FBuffer{$IFDEF DIOCP_DEBUG}, Format('- FlushBuffer(%d)', [n]){$ENDIF});
      end;
    end else
    begin
      if FBuffer <> nil then FreeBuffer(FBuffer{$IFDEF DIOCP_DEBUG}, 'FlushBuffer - 2'{$ENDIF});
    end;
  finally
    FBuffer := nil;
  end;
end;

procedure TBlockBuffer.Lock;
begin
  {$IFDEF USE_SPINLOCK}
  SpinLock(FSpinLock, FLockWaitCounter);
  {$ELSE}
  FLocker.Enter;
  {$ENDIF}
end;

procedure TBlockBuffer.SetBufferPool(ABufferPool: PBufferPool);
begin
  FBufferPool := ABufferPool;
  if FBufferPool <> nil then
  begin
    FBlockSize := FBufferPool.FBlockSize;
  end else
  begin
    FBlockSize := 0;
  end;
  FBuffer := nil;
end;

procedure TBlockBuffer.UnLock;
begin
  {$IFDEF USE_SPINLOCK}
  SpinUnLock(FSpinLock);
  {$ELSE}
  ABuffPool.FLocker.Leave;
  {$ENDIF}
end;

procedure TBlockBuffer.CheckBlockBuffer;
begin
  if FBuffer = nil then
  begin
    FBuffer := GetBuffer(FBufferPool);
    FBufferPtr := PByte(FBuffer);
    FPosition := 0;
    FSize := 0;
  end;
  
end;

procedure TBlockBuffer.CheckIsCurrentThread;
begin
  if (FThreadID <> 0) and (FThreadID <> GetCurrentThreadID) then
  begin
    raise Exception.CreateFmt('(%d,%d)当前对象已经被其他线程正在使用',
       [GetCurrentThreadID, FThreadID]);
  end;
end;

procedure TBlockBuffer.CheckThreadIn;
begin
  if FThreadID <> 0 then
  begin
    raise Exception.CreateFmt('(%d,%d)当前对象已经被其他线程正在使用',
       [GetCurrentThreadID, FThreadID]);
  end;
  FThreadID := GetCurrentThreadID;
end;

procedure TBlockBuffer.CheckThreadNone;
begin
  if FThreadID <> 0 then
  begin
    raise Exception.CreateFmt('(%d,%d)当前对象已经被其他线程正在使用',
       [GetCurrentThreadID, FThreadID]);
  end;  
end;

procedure TBlockBuffer.CheckThreadOut;
begin
  FThreadID := 0;  
end;






procedure TBlockBuffer.ClearBuffer;
begin
  if FBuffer = nil then Exit;
  try                        
    if FBuffer <> nil then FreeBuffer(FBuffer, 'ClearBuffer');
  finally
    FBuffer := nil;
  end;  
end;

end.
