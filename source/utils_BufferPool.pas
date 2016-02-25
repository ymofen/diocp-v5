(*
 * 内存池单元
 *   内存块通过引用计数，归还到池
 *
*)

unit utils_BufferPool;

interface

uses
  SyncObjs
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}

  {$ENDIF};

const
  block_flag :Word = $1DFB;

{$IFDEF DEBUG}
  protect_size = 8;
{$ELSE}
  protect_size = 0;
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
    FLocker:TCriticalSection;

  end;



  TBufferBlock = packed record
    flag: Word;
    refcounter :Integer;
    next: PBufferBlock;
    owner: PBufferPool;
  end;

const
  BLOCK_SIZE = SizeOf(TBufferBlock);

function NewBufferPool(pvBlockSize: Integer = 1024): PBufferPool;
procedure FreeBufferPool(buffPool:PBufferPool);

function GetBuffer(ABuffPool:PBufferPool): PByte;

function AddRef(pvBuffer:PByte): Integer;
function ReleaseRef(pvBuffer:PByte): Integer;

/// <summary>
///  检测池中内存块越界情况
/// </summary>
function CheckBufferBounds(ABuffPool:PBufferPool): Integer;

implementation



{$IF RTLVersion<24}
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedCompareExchange(Target, Value, Comparand);
{$ELSE}
  Result := TInterlocked.CompareExchange(Target, Value, Comparand);
{$ENDIF}
end;

function AtomicInc(var Target: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedIncrement(Target);
{$ELSE}
  Result := TInterlocked.Increment(Target);
{$ENDIF}
end;

function AtomicDec(var Target: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := InterlockedDecrement(Target);
{$ELSE}
  Result := TInterlocked.Decrement(Target);
{$ENDIF}
end;

{$IFEND <XE5}

/// <summary>
///   检测一块内存是否有越界情况
/// </summary>
function CheckBufferBlockBounds(ABlock: PBufferBlock): Boolean;
var
  lvBuffer:PByte;
  i:Integer;
begin
  Result := True;
  lvBuffer:= PByte(ABlock);
  Inc(lvBuffer, BLOCK_SIZE + ABlock.owner.FBlockSize);
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

function GetBuffer(ABuffPool:PBufferPool): PByte;
var
  lvBuffer:PBufferBlock;
begin
  ABuffPool.FLocker.Enter;
  lvBuffer := PBufferBlock(ABuffPool.FHead);
  if lvBuffer <> nil then ABuffPool.FHead := lvBuffer.next;
  ABuffPool.FLocker.Leave;

  if lvBuffer = nil then
  begin
    // + 2保护边界(可以检测内存越界写入)
    GetMem(Result, BLOCK_SIZE + ABuffPool.FBlockSize + protect_size);
    FillChar(Result^, BLOCK_SIZE + ABuffPool.FBlockSize + protect_size, 0);
    lvBuffer := PBufferBlock(Result);
    lvBuffer.owner := ABuffPool;
    lvBuffer.flag := block_flag;
    AtomicInc(ABuffPool.FSize);
  end else
  begin
    Result := PByte(lvBuffer);
  end;     

  Inc(Result, BLOCK_SIZE);
  AtomicInc(ABuffPool.FGet);
end;

procedure FreeBuffer(pvBufBlock:PBufferBlock);
var
  lvBuffer:PBufferBlock;
  lvOwner:PBufferPool;
begin
  lvOwner := pvBufBlock.owner;

  lvOwner.FLocker.Enter;
  lvBuffer := lvOwner.FHead;
  pvBufBlock.next := lvBuffer;
  lvOwner.FHead := pvBufBlock;
  lvOwner.FLocker.Leave;
  AtomicInc(lvOwner.FPut);
end;



function AddRef(pvBuffer:PByte): Integer;
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock; 
begin
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_SIZE);
  lvBlock := PBufferBlock(lvBuffer);
  Assert(lvBlock.flag = block_flag, 'invalid DBufferBlock');
  Result := AtomicInc(lvBlock.refcounter);
  AtomicInc(lvBlock.owner.FAddRef);
end;

function ReleaseRef(pvBuffer:PByte): Integer;
var
  lvBuffer:PByte;
  lvBlock:PBufferBlock; 
begin
  lvBuffer := pvBuffer;
  Dec(lvBuffer, BLOCK_SIZE);
  lvBlock := PBufferBlock(lvBuffer);
  Assert(lvBlock.flag = block_flag, 'invalid DBufferBlock');
  Result := AtomicDec(lvBlock.refcounter);
  AtomicInc(lvBlock.owner.FReleaseRef);
  if Result = 0 then
  begin
    FreeBuffer(lvBlock);
  end else
  begin
    Assert(Result > 0, 'DBuffer error release');
  end;

end;

function NewBufferPool(pvBlockSize: Integer = 1024): PBufferPool;
begin
  New(Result);
  Result.FBlockSize := pvBlockSize;
  Result.FHead := nil;
  Result.FLocker := TCriticalSection.Create;
  Result.FGet := 0;
  Result.FSize := 0;
  Result.FPut := 0;
  Result.FAddRef := 0;
  Result.FReleaseRef :=0;
  
end;

procedure FreeBufferPool(buffPool:PBufferPool);
var
  lvBlock, lvNext:PBufferBlock;
begin
  Assert(buffPool.FGet = buffPool.FPut, 'DBuffer Leak');

  lvBlock := buffPool.FHead;
  while lvBlock <> nil do
  begin
    lvNext := lvBlock.next;
    FreeMem(lvBlock);
    lvBlock := lvNext;
  end;
  buffPool.FLocker.Free;
  Dispose(buffPool);
end;

function CheckBufferBounds(ABuffPool:PBufferPool): Integer;
var
  lvBlock, lvNext:PBufferBlock;  
begin
  Result := 0;
  if protect_size = 0 then
  begin   // 没有保护边界的大小
    Result := -1;
    Exit;
  end;
  ABuffPool.FLocker.Enter;
  lvBlock := ABuffPool.FHead;
  while lvBlock <> nil do
  begin
    if not CheckBufferBlockBounds(lvBlock) then Inc(Result);

    lvBlock := lvBlock.next;
  end;
  ABuffPool.FLocker.Leave;
end;


end.
