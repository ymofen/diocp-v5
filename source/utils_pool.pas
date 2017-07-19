unit utils_pool;
(*
 * 对象池单元
 *   内存块通过引用计数，归还到池
 *
*)

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
  STRING_EMPTY:String = '';

{$IFDEF DEBUG}
  protect_size = 8;
{$ELSE}
  // 如果增加 则初始化时要进行填充
  protect_size = 0;
{$ENDIF}

{$IFDEF DIOCP_DEBUG_HINT}
  BLOCK_DEBUG_HINT_LENGTH = 512;
{$ENDIF}

type
  PValPool = ^ TValPool;
  PValPoolBlock = ^TValPoolBlock;

  TValBlockProc = procedure(const pvValBlock:PValPoolBlock);

  TValPool = record
    FBlockSize: Integer;
    FHead:PValPoolBlock;
    FGet:Integer;
    FPut:Integer;
    FSize:Integer;
    FAddRef:Integer;
    FReleaseRef:Integer;

    FPoolSize:Integer;

    // 新建
    FNewProc:TValBlockProc;

    // 规划清理
    FCleanUpProc: TValBlockProc;

    // 释放
    FDisposeProc: TValBlockProc;

    {$IFDEF USE_SPINLOCK}
    FSpinLock:Integer;
    FLockWaitCounter: Integer;
    {$ELSE}
    FLocker:TCriticalSection;
    {$ENDIF}

    FName:String;

  end;



  TValPoolBlock = record
    flag: Word;

    refcounter :Integer;
    next: PValPoolBlock;
    owner: PValPool;
    data: Pointer;

    {$IFDEF DIOCP_DEBUG_HINT}
    __debug_lock:Integer;
    __debug_hint:array[0..BLOCK_DEBUG_HINT_LENGTH -1] of Char;
    __debug_hint_pos:Integer;
    {$ENDIF}
    __debug_flag:Byte;

  end;

const
  VAL_BLOCK_SIZE = SizeOf(TValPoolBlock);



function NewValPool: PValPool;
procedure FreeValPool(pvValPool: PValPool);
procedure ClearValPool(pvValPool: PValPool);

procedure CheckIntializeValPool(AValPool: PValPool);

function GetValBlockFromPool(AValPool: PValPool): PValPoolBlock;

/// <summary>
///   对内存块进行引用计数
/// </summary>
function AddValRef(const pvValBlock: PValPoolBlock; const pvHint: string):
    Integer; overload;
function AddValRef(const pvValBlock: PValPoolBlock): Integer; overload;

/// <summary>
///   减少对内存块的引用
///   为0时，释放data数据
/// </summary>
function ReleaseValPoolRef(const pvValBlock: PValPoolBlock): Integer; overload;
function ReleaseValPoolRef(const pvValBlock: PValPoolBlock; const pvHint:
    string): Integer; overload;



/// <summary>
///  检测池中内存块越界情况
/// </summary>
function CheckBufferBounds(ABuffPool:PValPool): Integer;

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

procedure SpinLock(var Target:Integer; var WaitCounter:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
procedure SpinLock(var Target:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
procedure SpinUnLock(var Target:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF}overload;




{$if CompilerVersion < 18} //before delphi 2007
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall; external kernel32 name 'InterlockedCompareExchange';
{$EXTERNALSYM InterlockedCompareExchange}
{$ifend}

procedure PrintDebugString(s:string); {$IFDEF HAVE_INLINE} inline;{$ENDIF}

function GetValPoolDebugInfo(ABuffPool:PValPool): string;






implementation

{$IFDEF DIOCP_DEBUG}
var
  __debug_dna:Integer;
{$ENDIF}

function NewValPoolBlock(pvValPool:PValPool):PValPoolBlock;
var
  lvBuffer:Pointer;
begin
    // + 2保护边界(可以检测内存越界写入)
  GetMem(lvBuffer, VAL_BLOCK_SIZE + protect_size);
  {$IFDEF DEBUG}
  FillChar(lvBuffer^, VAL_BLOCK_SIZE + protect_size, 0);
  {$ELSE}
  FillChar(lvBuffer^, VAL_BLOCK_SIZE, 0);
  {$ENDIF}
  result := PValPoolBlock(lvBuffer);
  result.owner := pvValPool;
  result.flag := block_flag;
  result.data := nil;
  result.__debug_flag := 0;

  {$IFDEF DIOCP_DEBUG_HINT}
  result.__debug_lock := 0;
  result.__debug_hint_pos := 0;
  {$ENDIF}
  if Assigned(pvValPool.FNewProc) then
  begin
    pvValPool.FNewProc(Result);
  end;

end;


procedure PushABlockBuffer(const pvBufBlock: PValPoolBlock; const pvOwner:
    PValPool); {$IFDEF HAVE_INLINE} inline;{$ENDIF}
var
  lvBuffer :PValPoolBlock;
begin
  lvBuffer := pvOwner.FHead;
  pvBufBlock.next := lvBuffer;
  pvOwner.FHead := pvBufBlock;
end;

procedure CheckIntializeValPool(AValPool: PValPool);
var
  i: Integer;
  lvBuffer:PValPoolBlock;
begin
  for i := 0 to AValPool.FPoolSize -1 do
  begin
    lvBuffer := NewValPoolBlock(AValPool);

    PushABlockBuffer(lvBuffer, AValPool);

    AtomicIncrement(AValPool.FSize);
  end;
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
procedure InnerAddBlockHint(const pvBlock:PValPoolBlock; const pvHint:string); {$IFDEF HAVE_INLINE} inline;{$ENDIF}
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
    Sleep(1);    // 1 对比0 (线程越多，速度越平均)
    {$ENDIF}
  end;
end;

procedure SpinLock(var Target:Integer);
begin
  while AtomicCmpExchange(Target, 1, 0) <> 0 do
  begin
    {$IFDEF SPINLOCK_SLEEP}
    Sleep(1);    // 1 对比0 (线程越多，速度越平均)
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

{$IFDEF DEBUG}
/// <summary>
///   检测一块内存是否有越界情况
///   false 有越界清空
/// </summary>
function CheckBufferBlockBounds(ABlock: PValPoolBlock): Boolean;
var
  lvBuffer:PByte;
  i:Integer;
begin
  Result := True;
  lvBuffer:= PByte(ABlock);
  Inc(lvBuffer, VAL_BLOCK_SIZE + ABlock.owner.FBlockSize);

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

function GetValBlockFromPool(AValPool: PValPool): PValPoolBlock;
var
  lvBuffer:PValPoolBlock;
begin
  {$IFDEF USE_MEM_POOL}
  if AValPool.FPoolSize > 0 then
  begin
    {$IFDEF USE_SPINLOCK}
    SpinLock(AValPool.FSpinLock, AValPool.FLockWaitCounter);
    {$ELSE}
    AValPool.FLocker.Enter;
    {$ENDIF}

    // 获取一个节点
    lvBuffer := PValPoolBlock(AValPool.FHead);
    if lvBuffer <> nil then AValPool.FHead := lvBuffer.next;


    {$IFDEF USE_SPINLOCK}
    SpinUnLock(AValPool.FSpinLock);
    {$ELSE}
    AValPool.FLocker.Leave;
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
    Result := NewValPoolBlock(AValPool);
    AtomicIncrement(AValPool.FSize);
  end else
  begin
    Result := lvBuffer;
    Assert(lvBuffer.__debug_flag = 0, '多线程抢占， 混乱');
  end;

  lvBuffer.__debug_flag := 1;

  {$IFDEF DIOCP_DEBUG_HINT}
  InnerAddBlockHint(lvBuffer, '* GetValBlockFromPool');
  {$ENDIF}

  AtomicIncrement(AValPool.FGet);
end;

/// <summary>
///  释放内存块到Owner的列表中
/// </summary>
procedure InnerFreeBuffer(pvBufBlock: PValPoolBlock; const pvHint: string);{$IFDEF HAVE_INLINE} inline;{$ENDIF}
var
  lvOwner:PValPool;
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
  {$IFDEF USE_MEM_POOL}
  if lvOwner.FPoolSize > 0 then
  begin
    {$IFDEF USE_SPINLOCK}
    SpinLock(lvOwner.FSpinLock, lvOwner.FLockWaitCounter);
    {$ELSE}
    lvOwner.FLocker.Enter;
    {$ENDIF}
    if Assigned(lvOwner.FCleanUpProc) then
    begin
      lvOwner.FCleanUpProc(pvBufBlock);
    end;
    PushABlockBuffer(pvBufBlock, lvOwner);
    {$IFDEF USE_SPINLOCK}
    SpinUnLock(lvOwner.FSpinLock);
    {$ELSE}
    lvOwner.FLocker.Leave;
    {$ENDIF}
  end else
  begin
    if Assigned(lvOwner.FCleanUpProc) then
    begin
      lvOwner.FCleanUpProc(pvBufBlock);
    end;
    if Assigned(lvOwner.FDisposeProc) then
    begin
      lvOwner.FDisposeProc(pvBufBlock);
    end;
    FreeMem(pvBufBlock);
    AtomicDecrement(lvOwner.FSize);
  end;
  {$ELSE}

  if Assigned(lvOwner.FDisposeProc) then
  begin
    lvOwner.FDisposeProc(pvBufBlock);
  end;
  FreeMem(pvBufBlock);
  AtomicDecrement(lvOwner.FSize);
  {$ENDIF}
  AtomicIncrement(lvOwner.FPut);
end;

function AddValRef(const pvValBlock: PValPoolBlock): Integer;
begin
  Result := AddValRef(pvValBlock, STRING_EMPTY);
end;

function AddValRef(const pvValBlock: PValPoolBlock; const pvHint: string):
    Integer;
var
  lvBlock:PValPoolBlock;
begin
  lvBlock := PValPoolBlock(pvValBlock);

  if lvBlock.__debug_flag = 0 then
  begin
    Assert(lvBlock.__debug_flag <> 0, '已经归还, 不能进行AddRef');
  end;

  Assert(lvBlock.flag = block_flag, 'Invalid DBufferBlock');
  Result := AtomicIncrement(lvBlock.refcounter);
  AtomicIncrement(lvBlock.owner.FAddRef);

  {$IFDEF DIOCP_DEBUG_HINT}
  InnerAddBlockHint(lvBlock, pvHint);
  {$ENDIF}

  // 不加会被优化掉(DX10)
  Assert(Result > 0, 'error');
end;

function ReleaseValPoolRef(const pvValBlock: PValPoolBlock): Integer;
begin
  Result := ReleaseValPoolRef(pvValBlock, STRING_EMPTY);
end;


function ReleaseValPoolRef(const pvValBlock: PValPoolBlock; const pvHint:
    string): Integer;
var
  lvBlock:PValPoolBlock;
begin
  lvBlock := pvValBlock;
  if lvBlock.flag <>  block_flag then
  begin
    Assert(lvBlock.flag = block_flag, 'Invalid DBufferBlock');
  end;
  Result := AtomicDecrement(lvBlock.refcounter);
  AtomicIncrement(lvBlock.owner.FReleaseRef);

  {$IFDEF DIOCP_DEBUG_HINT}
  InnerAddBlockHint(lvBlock, pvHint);
  {$ENDIF}

  //if lvBlock.refcounter = 0 then
  if Result = 0 then
  begin
    InnerFreeBuffer(lvBlock, pvHint);
  end else if Result < 0 then
  begin          // error(不能小于0，如果小于0，则出现了严重问题)
    Assert(Result >= 0, Format('DBuffer error release ref:%d', [lvBlock.refcounter]));
  end;
end;

function NewValPool: PValPool;
begin
  New(Result);
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

end;

procedure FreeValPool(pvValPool: PValPool);
var
  lvBlock, lvNext:PValPoolBlock;
begin
  Assert(pvValPool.FGet = pvValPool.FPut,
    Format('ValPool-%s Leak, get:%d, put:%d', [pvValPool.FName, pvValPool.FGet, pvValPool.FPut]));

  lvBlock := pvValPool.FHead;
  while lvBlock <> nil do
  begin
    lvNext := lvBlock.next;
    if Assigned(pvValPool.FDisposeProc) then
    begin
      pvValPool.FDisposeProc(lvBlock);
    end;
    FreeMem(lvBlock);
    lvBlock := lvNext;
  end;
  {$IFDEF USE_SPINLOCK}
  ;
  {$ELSE}
  pvValPool.FLocker.Free;
  {$ENDIF}

  Dispose(pvValPool);
end;

function CheckBufferBounds(ABuffPool:PValPool): Integer;
{$IFDEF DEBUG}
var
  lvBlock:PValPoolBlock;
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


function CheckBlockBufferBounds(pvBuffer: Pointer): Integer;
{$IFDEF DEBUG}
var
  lvBuffer:PByte;
  lvBlock:PValPoolBlock;
  ABuffPool:PValPool;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  lvBuffer := pvBuffer;
  Dec(lvBuffer, VAL_BLOCK_SIZE);
  lvBlock := PValPoolBlock(lvBuffer);
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

procedure ClearValPool(pvValPool: PValPool);
var
  lvBlock, lvNext:PValPoolBlock;
begin
  Assert(pvValPool.FGet = pvValPool.FPut,
    Format('ValPool-%s Leak, get:%d, put:%d', [pvValPool.FName, pvValPool.FGet, pvValPool.FPut]));
  {$IFDEF USE_SPINLOCK}
  SpinLock(pvValPool.FSpinLock, pvValPool.FLockWaitCounter);
  {$ELSE}
  pvValPool.FLocker.Enter;
  {$ENDIF}

  lvBlock := pvValPool.FHead;
  while lvBlock <> nil do
  begin
    lvNext := lvBlock.next;
    if Assigned(pvValPool.FDisposeProc) then
    begin
      pvValPool.FDisposeProc(lvBlock);
    end;
    FreeMem(lvBlock);
    lvBlock := lvNext;
  end;

  pvValPool.FHead := nil;
  pvValPool.FSize := 0;
  {$IFDEF USE_SPINLOCK}
  SpinUnLock(pvValPool.FSpinLock);
  {$ELSE}
  pvValPool.FLocker.Leave;
  {$ENDIF}


end;

function GetValPoolDebugInfo(ABuffPool:PValPool): string;
begin
  Result := Format('name:%s, get:%d, put:%d, AddRef:%d, ReleaseRef:%d, size:%d', [ABuffPool.FName, ABuffPool.FGet, ABuffPool.FPut, ABuffPool.FAddRef, ABuffPool.FReleaseRef, ABuffPool.FSize]);;
end;










end.
