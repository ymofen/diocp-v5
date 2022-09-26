(*
 * �ڴ�ص�Ԫ
 *   �ڴ��ͨ�����ü������黹����
 *
*)

unit utils_BufferPool;

interface

{$DEFINE USE_SPINLOCK}

/// ���������sleep������cpu�� spinlock�Ƶ�utils_strings��
/// spinlock ����ʧ��ʱ��ִ��sleep
// {$DEFINE SPINLOCK_SLEEP}

/// ʹ���ڴ��
{$DEFINE USE_MEM_POOL}

uses
  SyncObjs, SysUtils, Classes, utils_strings
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}

  {$ENDIF}, Math;

{$IFNDEF DEBUG}     // INLINE���õ���
{$IF defined(FPC) or (RTLVersion>=18))}
  {$DEFINE HAVE_INLINE}
{$IFEND HAVE_INLINE}
{$ENDIF}


// ���е���
{.$UNDEF HAVE_INLINE}
{.$DEFINE DIOCP_DEBUG_HINT}

{$IFDEF DIOCP_HIGH_SPEED}
  {$UNDEF DIOCP_DEBUG}
{$ENDIF}

{$IFDEF DIOCP_DEBUG}
{$UNDEF HAVE_INLINE}
{$ENDIF}



const
  block_flag :Word = $1DFB;
  free_block_flag: Word = $1DFE;

{$IFDEF DEBUG}
  protect_size = 8;
{$ELSE}
  // ������� ���ʼ��ʱҪ�������
  protect_size = 0;
{$ENDIF}

{$IFDEF DIOCP_DEBUG_HINT}
  BLOCK_DEBUG_HINT_LENGTH = 128;
{$ENDIF}

type
 
  PBufferPool = ^ TBufferPool;
  PBufferBlock = ^TBufferBlock;
  PBlockLinked = ^TBlockLinked;

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
    data_free_proc:TDataProc; // �ͷź���

    {$IFDEF DIOCP_DEBUG_HINT}
    __debug_lock:Integer;
    __debug_hint:array[0..BLOCK_DEBUG_HINT_LENGTH -1] of Char;
    __debug_hint_pos:Integer;
    {$ENDIF}
    __debug_flag:Byte;

  end;

  // ��Ҫ��֤
  TBlockLinked = record
    FHead:PBufferBlock;
    FTail:PBufferBlock;
    FSize:Integer;
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



function NewBufferPool(pvBlockSize: Integer = 1024; pvPoolSize:Integer = 0):
    PBufferPool;
procedure FreeBufferPool(buffPool:PBufferPool);
procedure ClearBufferPool(buffPool:PBufferPool);

function GetBuffer(ABuffPool:PBufferPool): PByte;{$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;

// ��ȡһ���ڴ�, ����ͨ��AddRef��ReleaseRef�������ü����ͷ�
function GetBuffer(pvSize:Integer): Pointer;{$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;


function Ptr2BuffBlock(pvBuff:Pointer): PBufferBlock;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function BuffBlock2Ptr(pvBlock:PBufferBlock): Pointer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}

/// <summary>
///   0:�ɹ�
///  -1: ʧ��
/// </summary>
procedure EnQueue2BlockLinked(pvBlock:PBufferBlock; pvLinked:PBlockLinked);{$IFDEF HAVE_INLINE} inline;{$ENDIF}

/// <summary>
///   nil:��ȡʧ��
/// </summary>
function DeQueueFromLinked(pvLinked:PBlockLinked): PBufferBlock;{$IFDEF HAVE_INLINE} inline;{$ENDIF}

procedure FreeBuffer(const pvBuffer:PByte; const pvHint: string; pvReleaseAttachDataAtEnd:Boolean=True);overload; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
procedure FreeBuffer(const pvBuffer:PByte; pvReleaseAttachDataAtEnd:Boolean=True);overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}

/// <summary>
///   ���ڴ��������ü���
/// </summary>
function AddRef(const pvBuffer:PByte; const pvHint: string): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AddRef(const pvBuffer:PByte): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}

/// <summary>
///   ���ٶ��ڴ�������
///   Ϊ0ʱ���ͷ�data����
/// </summary>
function ReleaseRef(const pvBuffer: PByte; const pvHint: string): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function ReleaseRef(const pvBuffer: PByte): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function ReleaseRef(const pvBuffer: Pointer; pvReleaseAttachDataAtEnd: Boolean;
    const pvHint: string): Integer;overload;{$IFDEF HAVE_INLINE} inline;{$ENDIF}


/// <summary>
///   ����һ������
/// </summary>
procedure AttachData(pvBuffer, pvData: Pointer; pvFreeProc: TDataProc);

/// <summary>
///   ��ȡ���ӵ�����
///   0:�ɹ�
///   1:û��
/// </summary>
function GetAttachData(pvBuffer: Pointer; var X: Pointer): Integer;


function GetAttachDataAsObject(pvBuffer:Pointer): TObject;

/// <summary>
///  �������ڴ��Խ�����
/// </summary>
function CheckBufferBounds(ABuffPool:PBufferPool): Integer;

/// <summary>
///   ��ⵥ���ڴ���Ƿ�Խ��
/// </summary>
function CheckBlockBufferBounds(pvBuffer: Pointer): Integer;

//{$IF RTLVersion<24}
//function AtomicCmpExchange(var Target: Integer; Value: Integer;
//  Comparand: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
//function AtomicIncrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
//function AtomicDecrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
//{$IFEND <XE5}
//
//
//
////procedure SpinLock(var Target:Integer; var WaitCounter:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
////procedure SpinLock(var Target:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
////procedure SpinUnLock(var Target:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF}overload;
//
//
//{$if CompilerVersion < 18} //before delphi 2007
//function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall; external kernel32 name 'InterlockedCompareExchange';
//{$EXTERNALSYM InterlockedCompareExchange}
//{$ifend}

procedure FreeObject(AObject: TObject); {$IFDEF HAVE_INLINE} inline;{$ENDIF}

procedure PrintDebugString(s:string); {$IFDEF HAVE_INLINE} inline;{$ENDIF}

function GetBufferPoolDebugInfo(ABuffPool:PBufferPool): string;


procedure FreePtrAsObjectProc(pvData:Pointer);




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
    // + 2�����߽�(���Լ���ڴ�Խ��д��)
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
    if Assigned(pvBlock.data_free_proc) then
    begin
      pvBlock.data_free_proc(pvBlock.data);
    end;
    pvBlock.data := nil;
  end;
end;
//
//procedure SpinLock(var Target:Integer; var WaitCounter:Integer); {$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
//begin
//  while AtomicCmpExchange(Target, 1, 0) <> 0 do
//  begin
//    AtomicIncrement(WaitCounter);
////    {$IFDEF MSWINDOWS}
////      SwitchToThread;
////    {$ELSE}
////      TThread.Yield;
////    {$ENDIF}
//    {$IFDEF SPINLOCK_SLEEP}
//    Sleep(1);    // 1 �Ա�0 (�߳�Խ�࣬�ٶ�Խƽ��)
//    {$ENDIF}
//  end;
//end;
//
//procedure SpinLock(var Target:Integer);{$IFDEF HAVE_INLINE} inline;{$ENDIF} overload;
//begin
//  while AtomicCmpExchange(Target, 1, 0) <> 0 do
//  begin
//    {$IFDEF SPINLOCK_SLEEP}
//    Sleep(1);    // 1 �Ա�0 (�߳�Խ�࣬�ٶ�Խƽ��)
//    {$ENDIF}
//  end;
//end;
//
//
//procedure SpinUnLock(var Target:Integer);{$IFDEF HAVE_INLINE} inline;{$ENDIF}
//begin
//  if AtomicCmpExchange(Target, 0, 1) <> 1 then
//  begin
//    Assert(False, 'SpinUnLock::AtomicCmpExchange(Target, 0, 1) <> 1');
//  end;
//end;
//
//
//
//{$IF RTLVersion<24}
//function AtomicCmpExchange(var Target: Integer; Value: Integer;
//  Comparand: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
//begin
//{$IFDEF MSWINDOWS}
//  Result := InterlockedCompareExchange(Target, Value, Comparand);
//{$ELSE}
//  Result := TInterlocked.CompareExchange(Target, Value, Comparand);
//{$ENDIF}
//end;
//
//function AtomicIncrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
//begin
//{$IFDEF MSWINDOWS}
//  Result := InterlockedIncrement(Target);
//{$ELSE}
//  Result := TInterlocked.Increment(Target);
//{$ENDIF}
//end;
//
//function AtomicDecrement(var Target: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
//begin
//{$IFDEF MSWINDOWS}
//  Result := InterlockedDecrement(Target);
//{$ELSE}
//  Result := TInterlocked.Decrement(Target);
//{$ENDIF}
//end;
//
//{$IFEND <XE5}

{$IFDEF DEBUG}
/// <summary>
///   ���һ���ڴ��Ƿ���Խ�����
///   false ��Խ�����
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

    // ��ȡһ���ڵ�
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
    // + 2�����߽�(���Լ���ڴ�Խ��д��)
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
    Assert(lvBuffer.__debug_flag = 0, '���߳���ռ�� ����');
  end;

  lvBuffer.__debug_flag := 1;
  lvBuffer.flag := block_flag;

  {$IFDEF DIOCP_DEBUG_HINT}
  InnerAddBlockHint(lvBuffer, '* GetBuffer');
  {$ENDIF}

  Inc(Result, BLOCK_HEAD_SIZE);
  AtomicIncrement(ABuffPool.FGet);
end;

/// <summary>
///  �ͷ��ڴ�鵽Owner���б���
/// </summary>
procedure InnerFreeBuffer(pvBufBlock: PBufferBlock; const pvHint: string);{$IFDEF HAVE_INLINE} inline;{$ENDIF}
var
  lvOwner:PBufferPool;
begin
  if pvBufBlock.__debug_flag = 0 then
  begin
    Assert(pvBufBlock.__debug_flag <> 0, '��ι黹');
  end;
  pvBufBlock.__debug_flag := 0;
  pvBufBlock.flag := free_block_flag;

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
      ReleaseAttachData(pvBufBlock);
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
    Assert(lvBlock.__debug_flag <> 0, '�Ѿ��黹, ���ܽ���AddRef');
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
  
  // ���ӻᱻ�Ż���(DX10)
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
  begin          // error(����С��0�����С��0�����������������)
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
  begin   // û�б����߽�Ĵ�С
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

procedure AttachData(pvBuffer, pvData: Pointer; pvFreeProc: TDataProc);
var
  lvBlock:PBufferBlock;
begin
  lvBlock := Ptr2BuffBlock(pvBuffer);

  ReleaseAttachData(lvBlock);

  lvBlock.data := pvData;
  lvBlock.data_free_proc := pvFreeProc;
end;

function GetAttachData(pvBuffer: Pointer; var X: Pointer): Integer;
var
  lvBlock:PBufferBlock;
begin
  lvBlock := Ptr2BuffBlock(pvBuffer);

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
  lvBlock:PBufferBlock;
begin
  lvBlock := Ptr2BuffBlock(pvBuffer);

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
  begin   // û�б����߽�Ĵ�С
    Result := -1;
    Exit;
  end;
  Result := 0;
  ABuffPool := lvBlock.owner;
  if ABuffPool <> nil then
  begin
    {$IFDEF USE_SPINLOCK}
    SpinLock(ABuffPool.FSpinLock, ABuffPool.FLockWaitCounter);
    {$ELSE}
    ABuffPool.FLocker.Enter;
    {$ENDIF}
  end;
  if not CheckBufferBlockBounds(lvBlock) then Result := 1;
  if ABuffPool <> nil then
  begin
    {$IFDEF USE_SPINLOCK}
    SpinUnLock(ABuffPool.FSpinLock);
    {$ELSE}
    ABuffPool.FLocker.Leave;
    {$ENDIF}
  end;
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
  // + 2�����߽�(���Լ���ڴ�Խ��д��)
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

function Ptr2BuffBlock(pvBuff:Pointer): PBufferBlock;
var
  lvPtr:PByte;
begin
  lvPtr :=PByte(pvBuff);
  Dec(lvPtr, BLOCK_HEAD_SIZE);
  Result := PBufferBlock(lvPtr);

  {$IFDEF DEBUG}
  if Result.flag <>  block_flag then
  begin
    Assert(Result.flag = block_flag, 'Invalid DBufferBlock');
  end;
  {$ENDIF}
end;

function BuffBlock2Ptr(pvBlock:PBufferBlock): Pointer;
var
  lvPtr:PByte;
begin
  lvPtr := PByte(pvBlock);
  Inc(lvPtr, BLOCK_HEAD_SIZE);
  Result := lvPtr;
end;

procedure EnQueue2BlockLinked(pvBlock:PBufferBlock; pvLinked:PBlockLinked);
begin
  if pvLinked.FTail = nil then
    pvLinked.FHead := pvBlock
  else
  begin
    pvLinked.FTail.Next := pvBlock;
  end;

  pvLinked.FTail := pvBlock;
  Inc(pvLinked.FSize);
end;

function DeQueueFromLinked(pvLinked:PBlockLinked): PBufferBlock;
begin
  Result := pvLinked.FHead;
  if Result <> nil then
  begin
    pvLinked.FHead := Result.Next;

    if pvLinked.FHead = nil then pvLinked.FTail := nil;

    Dec(pvLinked.FSize);
  end;
end;

procedure FreePtrAsObjectProc(pvData:Pointer);
begin
  TObject(pvData).Free;
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
      FlushBuffer; // Buffer = nil;
    end else if FPosition > FBlockSize then
    begin            // Խ��
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
{$IF Defined(DIOCP_DEBUG)}
var
  r, n:Integer;
  lvDebugStr:string;
{$IFEND}
var
  lvBuffer:PByte; 
begin
  lvBuffer := FBuffer;
  if lvBuffer = nil then Exit;
  try
    if (Assigned(FOnBufferWrite) and (FSize > 0)) then
    begin
      {$IFDEF DIOCP_DEBUG}n := AtomicIncrement(__debug_dna){$ENDIF};
      {$IFDEF DIOCP_DEBUG}r := {$ENDIF}AddRef(lvBuffer{$IFDEF DIOCP_DEBUG}, Format('+ FlushBuffer(%d)', [n]){$ENDIF});    // �����¼���û��ʹ�����ü��������ͷ�buf
      try
        {$IFNDEF BIG_CONCURRENT}
        //{$IFDEF DIOCP_DEBUG}PrintDebugString(Format('+ FlushBuffer %2x: %d', [Cardinal(FBuffer), r]));{$ENDIF}
        {$ENDIF}
        FOnBufferWrite(self, lvBuffer, FSize);
      finally
        {$IFDEF DIOCP_DEBUG}
        lvDebugStr := Format('- FlushBuffer(n:%d)(r:%d)', [n, r]);
        ReleaseRef(lvBuffer, lvDebugStr);
        {$ELSE}
        ReleaseRef(lvBuffer);
        {$ENDIF}

      end;
    end else
    begin
      FreeBuffer(lvBuffer{$IFDEF DIOCP_DEBUG}, 'FlushBuffer - 2'{$ENDIF});
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
    raise Exception.CreateFmt('(%d,%d)��ǰ�����Ѿ��������߳�����ʹ��',
       [GetCurrentThreadID, FThreadID]);
  end;
end;

procedure TBlockBuffer.CheckThreadIn;
begin
  if FThreadID <> 0 then
  begin
    raise Exception.CreateFmt('(%d,%d)��ǰ�����Ѿ��������߳�����ʹ��',
       [GetCurrentThreadID, FThreadID]);
  end;
  FThreadID := GetCurrentThreadID;
end;

procedure TBlockBuffer.CheckThreadNone;
begin
  if FThreadID <> 0 then
  begin
    raise Exception.CreateFmt('(%d,%d)��ǰ�����Ѿ��������߳�����ʹ��',
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
