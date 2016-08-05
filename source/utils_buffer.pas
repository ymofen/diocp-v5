(*
 *	 Unit owner: 不得闲, d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *
 *   1. 2015年7月17日 15:01:12
 *      TBufferLink添加函数
 *     /// <summary>
 *    ///  从当前位置开始搜索Buffer
 *    ///   返回搜索到subBuf的起始位置
 *    ///   如果搜索不到返回-1
 *    /// </summary>
 *    function SearchBuffer(subBuf:PAnsiChar; subBufLen:Cardinal): Integer;
 *
 *  2 . 修正TDiocpExTcpServer和TDiocpExTcpClient的一处解码严重bug(谢谢 沈阳-想  14667479反馈和提供有缺陷的DEMO)
 *     修正了SearchBuffer中用于搜索数据的函数SearchPointer bug，
 *     由于没有赋值j = pvStartIndex导致判断 j < sourcelen时，会出现超越内存块搜寻。导致搜寻了无效的数据。
 *     2015-08-28 14:06:14

 * 3. 修正SearchBuffer中的一严重bug(只比较了前两位字符的匹配性)
      2015-09-11 09:08:22

 *
 *)

unit utils_buffer;

/// 2014年2月26日 11:33:38
///  杨茂丰
///  inline
//{$IF defined(FPC) or defined(VER170) or defined(VER180) or defined(VER190) or defined(VER200) or defined(VER210)}
//  {$DEFINE HAVE_INLINE}
//{$IFEND}

{$if CompilerVersion > 18}
  {$DEFINE HAVE_INLINE}
{$ifend}
interface

uses
  Windows, SyncObjs, SysUtils, Classes;

// 25:XE5
{$IF CompilerVersion<=25}
type
     NativeUInt = Cardinal;
     IntPtr = Cardinal;
{$ifend}

type
  TDxMemBlockType = (MB_Small,MB_Normal,MB_Big,MB_SpBig,MB_Large,MB_SPLarge,MB_Max,MB_MaxBlock); //内存块模式
  PMemoryBlock = ^TMemoryBlock;
  TMemoryBlock = record
    Memory: Pointer;
    Next: PMemoryBlock;
    Prev: PMemoryBlock;
    NextEx: PMemoryBlock;
    PrevEx: PMemoryBlock;
    BlockType: TDxMemBlockType;
    DataLen: NativeUInt;
  end;

  //内存池
  TDxMemoryPool = class
  private
    FCs: TCriticalSection;
    FBlockSize: Integer;
    FMaxFreeBlocks: Integer;

    FUseHead: PMemoryBlock; //使用的节点的皆点头
    FUseLast: PMemoryBlock; //最后一个

    FUnUseHead: PMemoryBlock; //未使用的皆点头
    FUnUseLast: PMemoryBlock;

    FUseCount: Integer;
    FFreeCount: Integer;
    FBlockType: TDxMemBlockType;
  protected
    function InnerCreateBlock: PMemoryBlock;
  public
    constructor Create(BlockSize: Integer;InitCount: Integer;BlockType: TDxMemBlockType;MaxFreeBlocks: Integer=50);
    destructor Destroy;override;
    procedure Lock; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
    procedure Unlock; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
    function GetMemory(Zero: Boolean): Pointer;deprecated;
    procedure FreeMemory(P: Pointer);deprecated;

    function GetMemoryBlock: PMemoryBlock;
    procedure FreeMemoryBlock(p: PMemoryBlock);
    procedure Clear;
  end;

  TBufferLink = class;

  //依附于内存池的内存流对象
  TDxMemoryStream = class(TStream)
  private
    FHead: PMemoryBlock;
    FLast: PMemoryBlock;
    FCurBlock: PMemoryBlock;
    FCurBlockPos: Integer;

    FMarkBlock: PMemoryBlock;
    FMarkBlokPos: Integer;
    FSize, FPosition,FCapacity: Longint;
    FMemBlockType: TDxMemBlockType;
    FMemBlockCount: Integer;
    procedure SetMemBlockType(const Value: TDxMemBlockType);
    function GetBlockSize: DWORD;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(const MemType: TDxMemBlockType = MB_Small);
    destructor Destroy;override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SwapStreamLink(Stream: TDxMemoryStream);
    procedure SetSize(NewSize: Longint); override;
    procedure SaveCurBlock;
    procedure RestoreCurBlock;
    procedure LinkToBufferList(BufList: TBufferLink);
    procedure ReadToBufferList(BufList: TBufferLink;Len: Integer);
    procedure SwapBufferBlock(Stream: TDxMemoryStream); //交换内部连接
    procedure LoadFromBufferList(BufList: TBufferLink;const DataLen: Integer);
    procedure WriteStream(Stream: TStream;Len: Integer);
    procedure ReadStream(Stream: TStream;len: Integer);
    procedure Clear;
    property Head: PMemoryBlock read FHead;
    property Last: PMemoryBlock read FLast;
    property BlockSize: DWORD read GetBlockSize;
    property MemBlockType: TDxMemBlockType read FMemBlockType write SetMemBlockType; //小内存流
  end;


  /// <summary>
  ///   内部分配内存时是使用的内存池
  /// </summary>
  TBufferLink = class
  private
    FHead: PMemoryBlock; //头部
    FLast: PMemoryBlock;  //最后一个可用的内存块位
    FRead: PMemoryBlock;  //当前读到的Buffer
    FReadPosition: Cardinal; //当前读到的Buffer位置
    FMark: PMemoryBlock; //标记的内存块
    FMarkPosition: Cardinal; //标记的内存块位置
    function InnerReadBuf(const pvBufRecord: PMemoryBlock; pvStartPostion: Cardinal;
        buf: PAnsiChar; len: Cardinal): Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure markReaderIndex;
    procedure restoreReaderIndex;
    procedure AddBuffer(buf:PAnsiChar; len:Cardinal);
    procedure AddMemBlockLink(MemBlockLink: PMemoryBlock);
    function readBuffer(const buf: PAnsiChar; len: Cardinal): Cardinal;

    /// <summary>
    ///   跳过一段
    /// </summary>
    function Skip(len:Cardinal): Cardinal;

    /// <summary>
    ///  从当前位置开始搜索Buffer
    ///   返回搜索到subBuf的起始位置到当前读取位置直接的字符个数
    ///   如果搜索不到返回-1
    ///   searchbuffer('12', '1234') = 0;
    /// </summary>
    function SearchBuffer(subBuf:PAnsiChar; subBufLen:Cardinal): Integer;


    procedure clearBuffer;
    procedure clearHaveReadBuffer;

    function validCount: Integer;
    function GetCurBlock: PMemoryBlock;
    function FirstBlock: PMemoryBlock;
    procedure RemoveBlock(Block: PMemoryBlock;const FreeBlock: Boolean = False);
  end;

  TDxObjectPool = class;
  TOnCreateObject = procedure(Pool: TDxObjectPool;var Obj: TObject) of Object;
  TDxObjectPool = class
  private
    FUses,FUnUses: TList;
    FObjClass: TClass;
    FMaxObjCount: Integer;
    FLocker: TCriticalSection;
    FOnCreateObject: TOnCreateObject;
  public
    constructor Create(AMaxCount: Integer);overload;
    constructor Create(AMaxCount: Integer;ObjClass: TClass);overload;
    destructor Destroy;override;
    function GetObject: TObject;
    procedure FreeObject(Obj: TObject);
    property OnCreateObject: TOnCreateObject read FOnCreateObject write FOnCreateObject;
  end;

  //环形缓冲流
  TDxRingStream = class(TStream)
  private
    FReadBlock: PMemoryBlock; //读取的块
    FReadBlockPos: Integer;
    FMemBlockType: TDxMemBlockType;

    FWriteBlock: PMemoryBlock;
    FWriteBlockPos: Integer;
    FSize, FCapacity: Longint;
    FMemBlockCount: Integer;
    FHead: PMemoryBlock;
    FLast: PMemoryBlock;
    FReadPosition: Integer;
    FWritePosition: Integer;
    FMarkRead,FMarkWrite: PMemoryBlock; //标记的内存块
    FMarkReadPosition,FMarkWritePosition: Cardinal; //标记的内存块位置
    procedure SetReadPosition(const Value: Integer);
    procedure SetWritePostion(const Value: Integer);
    function SeekReadWrite(Offset: Longint; Origin: TSeekOrigin;SeekRead: Boolean): Longint;
    function GetCanWriteSize: Integer;
    function GetDataSize: Integer;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(const RingBufferSize: DWORD; const MemType: TDxMemBlockType = MB_Small);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);


    procedure markReaderIndex;
    procedure restoreReaderIndex;

    procedure markWriterIndex;
    procedure restoreWriterIndex;

    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize: Longint); override;
    property ReadPosition: Integer read FReadPosition write SetReadPosition;
    property WritePosition: Integer read FWritePosition write SetWritePostion;
    property CanWriteSize: Integer read GetCanWriteSize; //能写入的数据长度
    property DataSize: Integer read GetDataSize; //能读取的数据长度
  end;

const
  MAX_SEND_BLOCK_SIZE: Integer= 1460*44;

procedure FreeMemBlock(Block: PMemoryBlock);
function GetMemBlock(BlockType: TDxMemBlockType): PMemoryBlock;
function CalcMemType(MemSize: Integer): TDxMemBlockType;
implementation


var
  NormalMPool: TDxMemoryPool = nil;
  SmallMPool: TDxMemoryPool = nil; //小内存块内存池
  BigMPool: TDxMemoryPool = nil; //大内存块内存池
  SuperBigMPool: TDxMemoryPool = nil; //超级大内存块内存池
  LargeMPool: TDxMemoryPool = nil;
  SPLargeMPool: TDxMemoryPool = nil;
  MaxMPool: TDxMemoryPool = nil;
  FMaxBlockMPool: TDxMemoryPool = nil;


/// <summary>
///   来自于utils_strings
///   查找PSub在P中出现的第一个位置
/// </summary>
/// <returns>
///   如果找到, 返回指向第一个pvSub的位置
///   找不到返回 Nil
/// </returns>
/// <param name="pvSource"> 数据 </param>
/// <param name="pvSourceLen"> 数据长度 </param>
/// <param name="pvStartIndex"> 开始位置0开始 </param>
/// <param name="pvSub"> 查找的数据 </param>
/// <param name="pvSubLen"> 查找的数据长度 </param>
function SearchPointer(pvSource: Pointer; pvSourceLen, pvStartIndex: Integer;
    pvSub: Pointer; pvSubLen: Integer): Pointer;
var
  I, j: Integer;
  lvTempP, lvTempPSub, lvTempP2, lvTempPSub2:PAnsiChar;
begin
  if (pvSub = nil) then
    Result := nil
  else
  begin
    Result := nil;
    j := pvStartIndex;
    lvTempP := PAnsiChar(pvSource);
    Inc(lvTempP, pvStartIndex);

    lvTempPSub := PAnsiChar(pvSub);
    while j<pvSourceLen do
    begin
      if lvTempP^ = lvTempPSub^ then
      begin


        // 临时指针，避免移动顺序比较指针
        lvTempP2 := lvTempP;
        Inc(lvTempP2);    // 移动到第二位(前一个已经进行了比较
        I := 1;           // 初始化计数器(从后面第二个字符开始对比)

        // 临时比较字符指针
        lvTempPSub2 := lvTempPSub;
        Inc(lvTempPSub2);  // 移动到第二位(前一个已经进行了比较

        while (I < pvSubLen) do
        begin
          if lvTempP2^ = lvTempPSub2^ then
          begin
            Inc(I);
            inc(lvTempP2);   // 移动到下一位进行比较
            inc(lvTempPSub2);
          end else
            Break;
        end;

        if I = pvSubLen then
        begin  // P1和P2已经匹配到了末尾(匹配成功)
          Result := lvTempP;
          Break;
        end;
      end;
      Inc(lvTempP);
      inc(j);
    end;
  end;
end;


function CalcMemType(MemSize: Integer): TDxMemBlockType;
begin
  if MemSize > 1024 * 128 then
    Result := MB_Max
  else if MemSize >= MAX_SEND_BLOCK_SIZE then
    Result := MB_MaxBlock
  else if MemSize > 4096*4  then
    Result := MB_SPLarge
  else if MemSize > 2048 then
    Result := MB_Large
  else if MemSize > 640 then
    Result := MB_Big
  else if MemSize > 256 then
    Result := MB_Normal
  else Result := MB_Small;
end;


function SuperMemoryPool: TDxMemoryPool;
begin
  if SuperBigMPool = nil then
    SuperBigMPool := TDxMemoryPool.Create(2048,30,MB_SpBig,100);
  Result := SuperBigMPool;
end;

function MaxMemoryPool: TDxMemoryPool;
begin
  if MaxMPool = nil then
   MaxMPool := TDxMemoryPool.Create(1024 * 128,30,MB_Max,100);
  Result := MaxMPool;
end;

function LargeMemoryPool: TDxMemoryPool;
begin
  if LargeMPool = nil then
    LargeMPool := TDxMemoryPool.Create(4096,30,MB_Large,100);
  Result := LargeMPool;
end;

function SuperLargeMemoryPool: TDxMemoryPool;
begin
  if SPLargeMPool = nil then
    SPLargeMPool := TDxMemoryPool.Create(4096*4,30,MB_SpLarge,100); //16KB
  Result := SPLargeMPool;
end;

function MaxBlockMPool: TDxMemoryPool;
begin
  if FMaxBlockMPool = nil then
    FMaxBlockMPool := TDxMemoryPool.Create(MAX_SEND_BLOCK_SIZE,30,MB_MaxBlock,100);
  Result := FMaxBlockMPool;
end;

function MemoryPool: TDxMemoryPool;
begin
  if NormalMPool = nil then
    NormalMPool := TDxMemoryPool.Create(640,30,MB_Normal,100);
  Result := NormalMPool;
end;

function SmallMemoryPool: TDxMemoryPool;
begin
  if SmallMPool = nil then
    SmallMPool := TDxMemoryPool.Create(128,30,MB_Small,100); //小内存块
  Result := SmallMPool;
end;

function BigMemoryPool: TDxMemoryPool;
begin
  if BigMPool = nil then
    BigMPool := TDxMemoryPool.Create(1024,30,MB_Big,100); //大内存块
  Result := BigMPool;
end;


function GetMemBlock(BlockType: TDxMemBlockType): PMemoryBlock;
begin
  case BlockType of
    MB_Small: Result := SmallMemoryPool.GetMemoryBlock;
    MB_Normal: Result := MemoryPool.GetMemoryBlock;
    MB_Big: Result := BigMemoryPool.GetMemoryBlock;
    MB_SpBig: Result := SuperMemoryPool.GetMemoryBlock;
    MB_Large: Result := LargeMemoryPool.GetMemoryBlock;
    MB_SPLarge: Result := SuperLargeMemoryPool.GetMemoryBlock;
    MB_Max: Result := MaxMemoryPool.GetMemoryBlock;
    MB_MaxBlock: Result := MaxBlockMPool.GetMemoryBlock;
  else Result := nil;
  end;
end;

{ TDxMemoryPool }

procedure TDxMemoryPool.Clear;
var
  P,tmp: PMemoryBlock;
begin
  Lock;
  try
    p := FUseHead;
    while p <> nil do
    begin
      tmp := p^.Next;
      FreeMem(p^.Memory);
      FreeMem(p);
      p := tmp;
    end;

    p := FUnUseHead;
    while p <> nil do
    begin
      tmp := p^.Next;
      FreeMem(p^.Memory,FBlockSize);
      FreeMem(p,SizeOf(TMemoryBlock));
      p := tmp;
    end;
    FUnUseHead := nil;
    FUnUseLast := nil;
    FUseHead := nil;
    FUseLast := nil;
  finally
    Unlock;
  end;
end;

constructor TDxMemoryPool.Create(BlockSize, InitCount: integer;BlockType: TDxMemBlockType;MaxFreeBlocks: Integer = 50);
var
  p: PMemoryBlock;
begin
  FBlockType := BlockType;
  // 块大小以64字节对齐，这样的执行效率最高
  if (BlockSize mod 64 = 0) then
    FBlockSize := BlockSize
  else
    FBlockSize := (BlockSize div 64) * 64 + 64;

  FMaxFreeBlocks := MaxFreeBlocks;
  FCS := TCriticalSection.Create;

  FMaxFreeBlocks := MaxFreeBlocks;
  FUseCount := 0;
  FFreeCount := InitCount;
  while InitCount > 0 do
  begin
    GetMem(p,SizeOf(TMemoryBlock));
    GetMem(p^.Memory,FBlockSize);
    p^.BlockType := FBlockType;
    if FUnUseHead = nil then
    begin
      FUnUseHead := p;
      FUnUseHead^.Next := nil;
      FUnUseHead.Prev := nil;
      FUnUseLast := FUnUseHead;
    end
    else
    begin
      p.Prev := FUnUseLast;
      p.Next := nil;
      FUnUseLast.Next := p;
      FUnUseLast := p;
    end;
    Dec(InitCount);
  end;
end;

destructor TDxMemoryPool.Destroy;
begin
  Clear;
  FCS.Free;
  inherited;
end;

procedure TDxMemoryPool.FreeMemory(P: Pointer);
var
  pBlock: PMemoryBlock;
begin
  if P = nil then Exit;
  Lock;
  try
    pBlock := FUseHead;
    while true do
    begin
      if PBlock^.Memory = p then
      begin
        //先判定是否是头
        if pBlock = FUseHead then
        begin
          FUseHead := pBlock^.Next;
          if FUseHead <> nil then
            FUseHead^.Prev := nil;
        end;
        if PBlock^.Prev <> nil then
          PBlock^.Prev^.Next := PBlock^.Next;
        if FUseLast = pBlock then
          FUseLast := FUseLast^.Prev;
        if FUnUseHead = nil then
        begin
          FUnUseHead := pBlock;
          pBlock^.Prev := nil;
          FUnUseLast := pBlock;
        end
        else if FFreeCount + 1 < FMaxFreeBlocks then
        begin
          pBlock^.Prev := FUnUseLast;
          FUnUseLast^.Next := pBlock;
          FUnUseLast := pBlock;
        end
        else //直接释放
        begin
          FreeMem(PBlock^.Memory);
          FreeMem(pBlock);
          pBlock := nil;
        end;
        if pBlock <> nil then
        begin
          Inc(FFreeCount);
          PBlock^.Next := nil;
        end;
        Dec(FUseCount);
        if FUseLast = nil then
          FUseHead := nil;
        Break;
      end;
      pBlock := pBlock^.Next;
      if pBlock = nil then
        break;
    end;
  finally
    Unlock;
  end;
end;

procedure TDxMemoryPool.FreeMemoryBlock(p: PMemoryBlock);
begin
  if p = nil then
    Exit;
  Lock;
  try
    if p^.PrevEx <> nil then
      p^.PrevEx^.NextEx := p^.NextEx;//连接指向下一个
    if p^.NextEx <> nil then
      p^.NextEx^.PrevEx := p^.PrevEx;
    //从Use列表中断开,Use列表指向P的节点的下一个
    if p^.Prev <> nil then
      p^.Prev^.Next := p^.Next;
    if p^.Next <> nil then
        p^.Next^.Prev := p^.Prev;

    p^.NextEx := nil;
    p^.PrevEx := nil;
    if FUseLast = p then
       FUseLast := FUseLast^.Prev;
    if FUnUseHead = nil then
    begin
      FUnUseHead := p;
      FUnUseHead^.Prev := nil;
      FUnUseLast := p;
    end
    else if FFreeCount + 1 <= FMaxFreeBlocks then
    begin
      p^.Prev := FUnUseLast;
      FUnUseLast^.Next := p;
      FUnUseLast := p;
    end
    else
    begin
      FreeMem(P^.Memory);
      FreeMem(P);
      p := nil;
    end;
    Dec(FUseCount);
    if p <> nil then
    begin
      Inc(FFreeCount);
      p^.Next := nil;
    end;
    if FUseLast = nil then
      FUseHead := nil;
  finally
    Unlock;
  end;
end;

function TDxMemoryPool.GetMemory(Zero: Boolean): Pointer;
var
  p: PMemoryBlock;
begin
  Lock;
  try
    if FUnUseHead = nil then
    begin
      GetMem(p,SizeOf(TMemoryBlock));
      GetMem(p^.Memory,FBlockSize);
      P^.BlockType := FBlockType;
    end
    else //直接从链表中取
    begin
      p := FUnUseHead;
      FUnUseHead := FUnUseHead.Next;
      Dec(FFreeCount);
    end;
    Inc(FUseCount);
    p.Next := nil;
    if FUseHead = nil then
    begin
      FUseHead := p;
      p.Prev := nil;
      FUseLast := p;
    end
    else
    begin
      p.Prev := FUseLast;
      FUseLast.Next := p;
      FUseLast := p;
    end;
    Result := p^.Memory;
    if Zero then
      ZeroMemory(Result,FBlockSize);
  finally
    Unlock;
  end;
end;

function TDxMemoryPool.GetMemoryBlock: PMemoryBlock;
begin
  Lock;
  try
    if FUnUseHead = nil then
    begin
      GetMem(Result,SizeOf(TMemoryBlock));
      GetMem(Result^.Memory,FBlockSize);
      Result^.BlockType := FBlockType;
    end
    else //直接从链表中取
    begin
      Result := FUnUseHead;
      FUnUseHead := FUnUseHead.Next;
      Dec(FFreeCount);
    end;
    Inc(FUseCount);
    Result.Next := nil;
    result.NextEx := nil;
    result.PrevEx := nil;
    if FUseHead = nil then
    begin
      FUseHead := Result;
      Result.Prev := nil;
      FUseLast := FUseHead;
    end
    else
    begin
      Result.Prev := FUseLast;
      FUseLast.Next := Result;
      FUseLast := Result;
    end;
  finally
    Unlock;
  end;
end;

function TDxMemoryPool.InnerCreateBlock: PMemoryBlock;
begin
   GetMem(Result,SizeOf(TMemoryBlock));
   GetMem(result^.Memory,FBlockSize);
   Result^.BlockType := FBlockType;
end;

procedure TDxMemoryPool.Lock;
begin
  FCs.Enter;
end;

procedure TDxMemoryPool.Unlock;
begin
  FCs.Leave;
end;

{ TDxMemoryStream }

procedure TDxMemoryStream.Clear;
begin
  SetSize(0);
end;

constructor TDxMemoryStream.Create(const MemType: TDxMemBlockType = MB_Small);
begin
  inherited Create;
  FMemBlockType := MemType;
end;

destructor TDxMemoryStream.Destroy;
begin
  SetSize(0);
  inherited;
end;

function TDxMemoryStream.GetBlockSize: DWORD;
begin
  case FMemBlockType of
    MB_Small: Result := SmallMemoryPool.FBlockSize;
    MB_Normal: Result := MemoryPool.FBlockSize;
    MB_Big: Result := BigMemoryPool.FBlockSize;
    MB_SpBig: Result := SuperMemoryPool.FBlockSize;
    MB_Large: Result := LargeMemoryPool.FBlockSize;
    MB_SPLarge: Result := SuperLargeMemoryPool.FBlockSize;
    MB_Max: Result := MaxMemoryPool.FBlockSize;
    MB_MaxBlock: Result := MaxBlockMPool.FBlockSize;
  else Result := 0;
  end;
end;

function TDxMemoryStream.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TDxMemoryStream.LinkToBufferList(BufList: TBufferLink);
var
  tmp: PMemoryBlock;
  BSize: integer;
begin
  if FHead <> nil then
  begin
    if BufList.FHead = nil then
      BufList.FHead := FHead
    else
    begin
      FHead.PrevEx := BufList.FLast;
      BufList.FLast^.NextEx := FHead;
    end;
    BufList.FLast := FLast;
    tmp := FHead;
    case FMemBlockType of
    MB_Small: BSize := SmallMemoryPool.FBlockSize;
    MB_Normal: BSize := MemoryPool.FBlockSize;
    MB_SpBig: BSize := SuperMemoryPool.FBlockSize;
    MB_Large: BSize := LargeMemoryPool.FBlockSize;
    MB_SPLarge: BSize := SuperLargeMemoryPool.FBlockSize;
    MB_Max: BSize := MaxMemoryPool.FBlockSize;
    MB_MaxBlock: BSize := MaxBlockMPool.FBlockSize;
    else BSize := BigMemoryPool.FBlockSize;
    end;
    while tmp <> FLast do
    begin
      tmp^.DataLen := BSize;
      tmp := tmp^.NextEx;
    end;
    FLast^.DataLen := BSize - (FCapacity - FSize);

    //连接到连接缓存区，就相当于本流清空
    FHead := nil;
    FLast := nil;
    FMarkBlokPos := 0;
    FPosition := 0;
    FMarkBlock := nil;
    FSize := 0;
    FCapacity := 0;
    FMemBlockCount := 0;
    FCurBlockPos := 0;
    FCurBlock := nil;
  end;
end;

procedure TDxMemoryStream.LoadFromBufferList(BufList: TBufferLink;const DataLen: Integer);
var
  Windex: Integer;
  tmpBlock: PMemoryBlock;
  MPool: TDxMemoryPool;
begin
  SetSize(DataLen);
  if FSize <> 0 then
  begin
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    tmpBlock := FHead;
    for Windex := 0 to FMemBlockCount - 1 do
    begin
      if tmpBlock = nil then
        Break;
      if Windex = FMemBlockCount - 1 then
        BufList.ReadBuffer(tmpBlock^.Memory,MPool.FBlockSize - (FCapacity - FSize))
      else BufList.readBuffer(tmpBlock^.Memory,MPool.FBlockSize);
      tmpBlock := tmpBlock^.NextEx;
    end;
    FPosition := 0;
    FCurBlock := FHead;
    FCurBlockPos := 0;
  end;
end;

procedure TDxMemoryStream.LoadFromFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TDxMemoryStream.LoadFromStream(Stream: TStream);
var
  Windex: Integer;
  tmpBlock: PMemoryBlock;
  MPool: TDxMemoryPool;
begin
  Stream.Position := 0;
  SetSize(Stream.Size);
  if FSize <> 0 then
  begin
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    tmpBlock := FHead;
    for Windex := 0 to FMemBlockCount - 1 do
    begin
      if Windex = FMemBlockCount - 1 then
      begin
        Stream.ReadBuffer(tmpBlock^.Memory^,MPool.FBlockSize - (FCapacity - FSize));
      end
      else Stream.ReadBuffer(tmpBlock^.Memory^,MPool.FBlockSize);
      tmpBlock := tmpBlock^.NextEx;
    end;
    FPosition := 0;
    FCurBlock := FHead;
    FCurBlockPos := 0;
  end;
end;

function TDxMemoryStream.Read(var Buffer; Count: Integer): Longint;
var
  p: PAnsiChar;
  MPool: TDxMemoryPool;
  pBuf: Pointer;
begin
  if FCurBlock = nil then
    Result := 0
  else
  begin
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    if FPosition + Count > FSize then
      Count := FSize - FPosition;
    if Count = 0 then
    begin
      result := 0;
      Exit;
    end;

    if FCurBlockPos = MPool.FBlockSize then
    begin
      FCurBlock := FCurBlock^.NextEx;
      FCurBlockPos := 0;
    end;

    if FCurBlock = nil then
    begin
      result := 0;
      Exit;
    end;

    Result := Count;
    p := @Buffer;
    //先读取当前块剩下的区域
    pBuf := Pointer(NativeUInt(FCurBlock^.Memory) + DWORD(FCurBlockPos));
    if Count <= MPool.FBlockSize - FCurBlockPos then //足够写了
    begin
      Move(PBuf^,buffer,Count);
      Inc(FPosition,Count);
      inc(FCurBlockPos,Count);
      Count := 0;
    end
    else
    begin
      Move(PBuf^,Buffer,MPool.FBlockSize - FCurBlockPos);
      Dec(Count,MPool.FBlockSize - FCurBlockPos);
      Inc(FPosition,MPool.FBlockSize - FCurBlockPos);
      Inc(p,MPool.FBlockSize - FCurBlockPos);
      FCurBlock := FCurBlock^.NextEx;
      FCurBlockPos := 0;
    end;
    if FCurBlockPos = MPool.FBlockSize then
    begin
      FCurBlock := FCurBlock^.NextEx;
      FCurBlockPos := 0;
    end;
    while Count > 0 do
    begin
      if Count > MPool.FBlockSize then
      begin
        Move(FCurBlock^.Memory^,p^,MPool.FBlockSize);
        Dec(Count,MPool.FBlockSize);
        FCurBlockPos := MPool.FBlockSize;
        Inc(FPosition,MPool.FBlockSize);
        Inc(p,MPool.FBlockSize);
      end
      else
      begin
        Move(FCurBlock^.Memory^,p^,Count);
        Inc(FPosition,Count);
        Inc(p,Count);
        FCurBlockPos := Count;
        Count := 0;
      end;
      if FCurBlockPos = MPool.FBlockSize then
      begin
        FCurBlock := FCurBlock^.NextEx;
        FCurBlockPos := 0;
      end;
    end;
  end;
end;

procedure TDxMemoryStream.ReadStream(Stream: TStream; len: Integer);
var
  MPool: TDxMemoryPool;
  pBuf: Pointer;
begin
  if FCurBlock <> nil then
  begin
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    if FPosition + Len > FSize then
      Len := FSize - FPosition;
    //先读取当前块剩下的区域
    pBuf := Pointer(NativeUInt(FCurBlock^.Memory) + DWORD(FCurBlockPos));
    if Len <= MPool.FBlockSize - FCurBlockPos then //足够写了
    begin
      Stream.WriteBuffer(PBuf^,Len);
      Inc(FPosition,Len);
      inc(FCurBlockPos,Len);
      Len := 0;
    end
    else
    begin
      Stream.WriteBuffer(PBuf^,MPool.FBlockSize - FCurBlockPos);
      Dec(Len,MPool.FBlockSize - FCurBlockPos);
      Inc(FPosition,MPool.FBlockSize - FCurBlockPos);
      FCurBlock := FCurBlock^.NextEx;
      FCurBlockPos := 0;
    end;

    while Len > 0 do
    begin
      if Len > MPool.FBlockSize then
      begin
        Stream.WriteBuffer(FCurBlock^.Memory^,MPool.FBlockSize);
        Dec(Len,MPool.FBlockSize);
        FCurBlockPos := MPool.FBlockSize;
        Inc(FPosition,MPool.FBlockSize);
      end
      else
      begin
        Stream.WriteBuffer(FCurBlock^.Memory^,Len);
        Inc(FPosition,Len);
        FCurBlockPos := Len;
        Len := 0;
      end;
      if FCurBlockPos = MPool.FBlockSize then
      begin
        FCurBlock := FCurBlock^.NextEx;
        FCurBlockPos := 0;
      end;
    end;
  end;
end;

procedure TDxMemoryStream.ReadToBufferList(BufList: TBufferLink; Len: Integer);
var
  MPool: TDxMemoryPool;
  pBuf: Pointer;
begin
  if FCurBlock <> nil then
  begin
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    if FPosition + Len > FSize then
      Len := FSize - FPosition;
    //先读取当前块剩下的区域
    pBuf := Pointer(NativeUInt(FCurBlock^.Memory) + DWORD(FCurBlockPos));
    if Len <= MPool.FBlockSize - FCurBlockPos then //足够写了
    begin
      BufList.AddBuffer(PBuf,Len);
      Inc(FPosition,Len);
      inc(FCurBlockPos,Len);
      Len := 0;
    end
    else
    begin
      BufList.AddBuffer(PBuf,MPool.FBlockSize - FCurBlockPos);
      Dec(Len,MPool.FBlockSize - FCurBlockPos);
      Inc(FPosition,MPool.FBlockSize - FCurBlockPos);
      FCurBlock := FCurBlock^.NextEx;
      FCurBlockPos := 0;

      if FCurBlock = FLast then
      begin
        if Len > MPool.FBlockSize then//最后一块，并且超过了块大小
          SetSize(FPosition + Len)
        else if FPosition + Len > FSize then
          FSize := FPosition + Len;
      end;
    end;

    while Len > 0 do
    begin
      if Len > MPool.FBlockSize then
      begin
        BufList.AddBuffer(FCurBlock^.Memory,MPool.FBlockSize);
        Dec(Len,MPool.FBlockSize);
        FCurBlockPos := MPool.FBlockSize;
        Inc(FPosition,MPool.FBlockSize);
      end
      else
      begin
        BufList.AddBuffer(FCurBlock^.Memory,Len);
        Inc(FPosition,Len);
        FCurBlockPos := Len;
        Len := 0;
      end;
      if FCurBlockPos = MPool.FBlockSize then
      begin
        FCurBlock := FCurBlock^.NextEx;
        FCurBlockPos := 0;
      end;
    end;
  end;
end;

procedure TDxMemoryStream.RestoreCurBlock;
begin
  FCurBlock := FMarkBlock;
  FCurBlockPos := FMarkBlokPos;
end;

procedure TDxMemoryStream.SaveCurBlock;
begin
  FMarkBlock := FCurBlock;
  FMarkBlokPos := FCurBlockPos;
end;

procedure TDxMemoryStream.SaveToFile(const FileName: string);
var
  FStream: TFileStream;
begin
  if FHead <> nil then
  begin
    FStream := TFileStream.Create(FileName,fmCreate);
    try
      SaveToStream(FStream);
    finally
      FStream.Free;
    end;
  end;
end;

procedure TDxMemoryStream.SaveToStream(Stream: TStream);
var
  tmp: PMemoryBlock;
  MPool: TDxMemoryPool;
begin
  if FHead <> nil then
  begin
    case FMemBlockType of
    MB_Small: MPool := SmallMemoryPool;
    MB_Normal: MPool := MemoryPool;
    MB_SpBig: MPool := SuperMemoryPool;
    MB_Large: MPool := LargeMemoryPool;
    MB_SPLarge: MPool := SuperLargeMemoryPool;
    MB_Max: MPool := MaxMemoryPool;
    MB_MaxBlock: MPool := MaxBlockMPool;
    else MPool := BigMemoryPool;
    end;
    tmp := FHead;
    while tmp <> nil do
    begin
      if tmp <> FLast then
        Stream.WriteBuffer(tmp^.Memory^,MPool.FBlockSize)
      else Stream.WriteBuffer(tmp^.Memory^,MPool.FBlockSize - FCapacity + FSize);
      tmp := tmp^.NextEx;
    end;
  end;
end;

function TDxMemoryStream.Seek(Offset: Integer; Origin: Word): Longint;
var
  MPool: TDxMemoryPool;
  BIndex: integer;
  FLastSize: Integer;
begin
  if FHead <> nil then
  begin
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    case TSeekOrigin(Origin) of
    soBeginning:
      begin
        if Offset < 0 then
          Offset := 0;
        if Offset <= FSize then
        begin
          FCurBlockPos := Offset mod MPool.FBlockSize;
          BIndex := Offset div MPool.FBlockSize;
          FCurBlock := FHead;
          FPosition := FCurBlockPos;
          if BIndex > 0 then
          repeat
            Inc(FPosition,MPool.FBlockSize);
            Dec(Bindex);
            FCurBlock := FCurBlock^.NextEx;
          until (BIndex = 0) or (FCurBlock = nil);
        end
        else
        begin
          FCurBlock := FLast;
          FCurBlockPos := FCapacity - FSize;
          FPosition := Size;
        end;
      end;
    soCurrent:
      begin
        if Offset > 0 then
        begin
          BIndex := (FCurBlockPos + Offset) div MPool.FBlockSize;
          Inc(FPosition,MPool.FBlockSize - FCurBlockPos);
          FCurBlockPos := (FCurBlockPos + Offset) mod MPool.FBlockSize;
          Inc(FPosition,FCurBlockPos);
          while BIndex > 0 do
          begin
            FCurBlock := FCurBlock^.NextEx;
            Dec(BIndex);
            if BIndex > 0 then
              Inc(FPosition,MPool.FBlockSize);
          end;
        end
        else
        begin
          if Offset + FCurBlockPos >= 0 then
          begin
            Inc(FPosition,offset);
            Inc(FCurBlockPos,offset);
          end
          else
          begin
            FCurBlock := FCurBlock^.PrevEx;
            if FCurBlock = nil then
            begin
              FCurBlockPos := 0;
              FCurBlock := FHead;
            end
            else
            begin
              inc(offset,FCurBlockPos);
              Dec(FPosition,FCurBlockPos);
              FCurBlockPos := MPool.FBlockSize;
              Seek(Offset,soCurrent);
            end;
          end;
        end;
      end;
    soEnd:
      begin
        if Offset < 0 then
          Offset := -offset;
        if Offset < FSize then
        begin
          FLastSize := MPool.FBlockSize - FCapacity + FSize; //最后一个大小
          if Offset <= FLastSize then
          begin
            FCurBlockPos := FLastSize - Offset;
            FCurBlock := FLast;
            FPosition := FSize - Offset;
          end
          else
          begin
            FCurBlock := FLast^.PrevEx;
            Dec(Offset,FLastSize);
            FPosition := FSize - FLastSize;
            BIndex := Offset div MPool.FBlockSize;
            Offset := Offset mod MPool.FBlockSize;
            FCurBlockPos := MPool.FBlockSize - Offset;
            Dec(FPosition,MPool.FBlockSize - FCurBlockPos);
            while BIndex > 0 do
            begin
              FCurBlock := FCurBlock^.PrevEx;
              Dec(BIndex);
              Dec(FPosition,MPool.FBlockSize);
            end;
          end;
        end
        else
        begin
          FPosition := 0;
          FCurBlockPos := 0;
          FCurBlock := FHead;
        end;
      end;
    end;
    Result := FPosition;
  end
  else Result := 0;
end;

procedure TDxMemoryStream.SetMemBlockType(const Value: TDxMemBlockType);
begin
  if FMemBlockType <> Value then
  begin
    Clear;
    FMemBlockType := Value;
  end;
end;

procedure TDxMemoryStream.SetSize(NewSize: Integer);
var
  CurCount: integer;
  mBlock: PMemoryBlock;
  MPool: TDxMemoryPool;
begin
  if FSize <> NewSize then
  begin
    FSize := NewSize;
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    CurCount := FCapacity div MPool.FBlockSize;
    FMemBlockCount := NewSize div MPool.FBlockSize;
    if NewSize mod MPool.FBlockSize <> 0 then
      Inc(FMemBlockCount);
    if CurCount <> FMemBlockCount then
    begin
      while CurCount > FMemBlockCount do
      begin //内存回收
        mBlock := FLast;
        FLast := FLast^.PrevEx;
        MPool.FreeMemoryBlock(mBlock);
        Dec(CurCount);
      end;
      while CurCount < FMemBlockCount do
      begin
        mBlock := MPool.GetMemoryBlock;
        mBlock.NextEx := nil;
        mBlock.PrevEx := nil;
        if FHead = nil then
        begin
          FHead := mBlock;
          FLast := mBlock;
          FHead.NextEx := nil;
          FHead.PrevEx := nil;
        end
        else
        begin
          FLast^.NextEx := mBlock;
          mBlock^.PrevEx := FLast;
          FLast := mBlock;
        end;
        Inc(CurCount);
      end;
      FCapacity := MPool.FBlockSize * FMemBlockCount;
    end;
    if NewSize = 0 then
    begin
      FPosition := 0;
      FCurBlock := nil;
      FHead := nil;
      FLast := nil;
      FMarkBlokPos := 0;
      FMarkBlock := nil;
      FCurBlockPos := 0;
    end
    else
    begin
      if FPosition > NewSize then
        Position := NewSize;
      if FCurBlock = nil then
      begin
        FCurBlock := FHead;
        FCurBlockPos := 0;
      end;
    end;
  end;
end;


procedure TDxMemoryStream.SwapBufferBlock(Stream: TDxMemoryStream);
begin

end;

procedure TDxMemoryStream.SwapStreamLink(Stream: TDxMemoryStream);
var
  OldHead,OldLast: PMemoryBlock;
  OldSize,OldBlockCount: DWORD;
  OldMtype: TDxMemBlockType;
  OldFCapacity: LongInt;
begin
  FMarkBlokPos := 0;
  FMarkBlock := nil;

  OldBlockCount := FMemBlockCount;
  FMemBlockCount := Stream.FMemBlockCount;

  OldMtype := FMemBlockType;
  FMemBlockType := Stream.FMemBlockType;

  OldFCapacity := FCapacity;
  FCapacity := Stream.FCapacity;

  OldHead := FHead;
  FHead := Stream.FHead;

  OldLast := FLast;
  FLast := Stream.FLast;

  OldSize := FSize;
  FSize := Stream.Size;

  FCurBlock := nil;
  FCurBlockPos := 0;
  FPosition := 0;

  Stream.FHead := OldHead;
  Stream.FLast := OldLast;
  Stream.FMarkBlock := nil;
  Stream.FMarkBlokPos := 0;
  Stream.FMarkBlock := nil;
  Stream.FCurBlock := nil;
  Stream.FCurBlockPos := 0;
  Stream.FMemBlockCount := OldBlockCount;
  Stream.FCapacity := OldFCapacity;
  Stream.FPosition := 0;
  Stream.FSize := OldSize;
  Stream.FMemBlockType := OldMtype;
end;

function TDxMemoryStream.Write(const Buffer; Count: Integer): Longint;
var
  MPool: TDxMemoryPool;
  pBuf: Pointer;
  tmpBuf: PByte;
begin
  case FMemBlockType of
    MB_Small: MPool := SmallMemoryPool;
    MB_Normal: MPool := MemoryPool;
    MB_SpBig: MPool := SuperMemoryPool;
    MB_Large: MPool := LargeMemoryPool;
    MB_SPLarge: MPool := SuperLargeMemoryPool;
    MB_Max: MPool := MaxMemoryPool;
    MB_MaxBlock: MPool := MaxBlockMPool;
    else MPool := BigMemoryPool;
  end;
  if FCurBlock = nil then
    SetSize(Count)
  else if FCurBlock = FLast then
  begin
    if FCurBlockPos + Count > MPool.FBlockSize then
      SetSize(FSize + Count)
    else if FPosition + Count > FSize then
      Inc(FSize,Count);
  end;
  if FCurBlockPos = MPool.FBlockSize then
  begin
    FCurBlock := FCurBlock^.NextEx;
    FCurBlockPos := 0;
  end;
  Result := Count;
  tmpBuf := @Buffer;
  //先写满当前未写满的内存块空间
  pBuf := Pointer(NativeUInt(FCurBlock^.Memory) + DWORD(FCurBlockPos));
  if Count <= MPool.FBlockSize - FCurBlockPos then //足够写了
  begin
    Move(Buffer,pBuf^,Count);
    inc(FCurBlockPos,Count);
    Inc(FPosition,Count);
    Count := 0;
  end
  else
  begin
    Move(Buffer,PBuf^,MPool.FBlockSize - FCurBlockPos);
    Dec(Count,MPool.FBlockSize - FCurBlockPos);
    Inc(tmpBuf,MPool.FBlockSize - FCurBlockPos);

    Inc(FPosition,MPool.FBlockSize - FCurBlockPos);
    FCurBlock := FCurBlock^.NextEx;
    FCurBlockPos := 0;
    if FCurBlock = FLast then
    begin
      if Count > MPool.FBlockSize then//最后一块，并且超过了块大小
        SetSize(FPosition + Count)
      else if FPosition + Count > FSize then
        FSize := FPosition + Count;
    end;
  end;
  while Count > 0 do
  begin
    if Count > MPool.FBlockSize then
    begin
      Move(tmpBuf^,FCurBlock^.Memory^,MPool.FBlockSize);
      Inc(tmpBuf,MPool.FBlockSize);
      Dec(Count,MPool.FBlockSize);
      FCurBlockPos := MPool.FBlockSize;
      Inc(FPosition,MPool.FBlockSize);
    end
    else
    begin
      Move(tmpBuf^,FCurBlock^.Memory^,Count);
      Inc(FPosition,Count);
      Inc(tmpBuf,Count);
      FCurBlockPos := Count;
      Count := 0;
    end;
    if FCurBlockPos = MPool.FBlockSize then
    begin
      FCurBlock := FCurBlock^.NextEx;
      FCurBlockPos := 0;
      if FCurBlock = FLast then
      begin
        if Count > MPool.FBlockSize then//最后一块，并且超过了块大小
          SetSize(FPosition + Count)
        else if FPosition + Count > FSize then
          FSize := FPosition + Count;
      end;
    end;
  end;
  if FCurBlock = nil then //为空，表示所有数据全部刚刚写完，需要重新分配一个内存
  begin
    FCurBlockPos := MPool.FBlockSize;
    FCurBlock := FLast;
  end;
end;

procedure TDxMemoryStream.WriteStream(Stream: TStream; Len: Integer);
var
  MPool: TDxMemoryPool;
  pBuf: Pointer;
begin
  case FMemBlockType of
    MB_Small: MPool := SmallMemoryPool;
    MB_Normal: MPool := MemoryPool;
    MB_SpBig: MPool := SuperMemoryPool;
    MB_Large: MPool := LargeMemoryPool;
    MB_SPLarge: MPool := SuperLargeMemoryPool;
    MB_Max: MPool := MaxMemoryPool;
    MB_MaxBlock: MPool := MaxBlockMPool;
    else MPool := BigMemoryPool;
  end;
  if FCurBlock = nil then
    SetSize(Len)
  else if FCurBlock = FLast then
  begin
    if FCurBlockPos + Len > MPool.FBlockSize then
      SetSize(Len+FSize)
    else if FPosition + Len > FSize then
      Inc(FSize,Len);
  end;
  //先写满当前未写满的内存块空间
  pBuf := Pointer(NativeUInt(FCurBlock^.Memory) + DWORD(FCurBlockPos));
  if Len <= MPool.FBlockSize - FCurBlockPos then //足够写了
  begin
    Stream.ReadBuffer(PBuf^,Len);
    inc(FCurBlockPos,Len);
    Inc(FPosition,Len);
    Len := 0;
  end
  else
  begin
    Stream.ReadBuffer(PBuf^,MPool.FBlockSize - FCurBlockPos);
    Dec(Len,MPool.FBlockSize - FCurBlockPos);

    Inc(FPosition,MPool.FBlockSize - FCurBlockPos);
    FCurBlock := FCurBlock^.NextEx;
    FCurBlockPos := 0;
    if FCurBlock = FLast then
    begin
      if Len > MPool.FBlockSize then//最后一块了，并且超过了块大小
        SetSize(FPosition + Len)
      else if FPosition + Len > FSize then
        FSize := FPosition + Len;
    end;
  end;
  while Len > 0 do
  begin
    if Len > MPool.FBlockSize then
    begin
      Stream.ReadBuffer(FCurBlock^.Memory^,MPool.FBlockSize);
      Dec(Len,MPool.FBlockSize);
      FCurBlockPos := MPool.FBlockSize;
      Inc(FPosition,MPool.FBlockSize);
    end
    else
    begin
      Stream.ReadBuffer(FCurBlock^.Memory^,Len);
      Inc(FPosition,Len);
      FCurBlockPos := Len;
      Len := 0;
    end;
    if FCurBlockPos = MPool.FBlockSize then
    begin
      FCurBlock := FCurBlock^.NextEx;
      FCurBlockPos := 0;
      if FCurBlock = FLast then
      begin
        if Len > MPool.FBlockSize then//最后一块了，并且超过了块大小
          SetSize(FPosition + Len)
        else if FPosition + Len > FSize then
          FSize := FPosition + Len;
      end;
    end;
  end;
end;

{ TBufferLink }

procedure TBufferLink.AddBuffer(buf: PAnsiChar; len: Cardinal);
var
  MemBlock: PMemoryBlock;
  BlockSize,WSize: NativeUInt;
  pBuf: PAnsiChar;
  MPool: TDxMemoryPool;
begin
  if FLast <> nil then
  begin
    //先写入空白内容数据
    case FLast^.BlockType of
    MB_Small: BlockSize := SmallMemoryPool.FBlockSize;
    MB_Normal: BlockSize := MemoryPool.FBlockSize;
    MB_SpBig: BlockSize := SuperMemoryPool.FBlockSize;
    MB_Large: BlockSize := LargeMemoryPool.FBlockSize;
    MB_SPLarge: BlockSize := SuperLargeMemoryPool.FBlockSize;
    MB_Max: BlockSize := MaxMemoryPool.FBlockSize;
    MB_MaxBlock: BlockSize := MaxBlockMPool.FBlockSize;
    else BlockSize := BigMemoryPool.FBlockSize;
    end;

    if (FLast^.DataLen <> BlockSize) then
    begin
      WSize := BlockSize - FLast^.DataLen;
      pBuf := FLast^.Memory;
      Inc(PBuf,FLast^.DataLen);
      if WSize >= len then
      begin
        Move(buf^,PBuf^,len);
        Inc(FLast^.DataLen,len);
        len := 0;
      end
      else
      begin
        Move(buf^,PBuf^,WSize);
        Dec(len,WSize);
        Inc(buf,WSize);
        FLast^.DataLen := BlockSize;
      end;
    end;
  end;
  if len = 0 then
    Exit;

  if len <= DWORD(SmallMemoryPool.FBlockSize) then
    MPool := SmallMemoryPool
  else if len <= DWORD(MemoryPool.FBlockSize) then
    MPool := MemoryPool
  else if len <= DWORD(BigMemoryPool.FBlockSize) then
    MPool := BigMemoryPool
  else if len <= DWORD(SuperMemoryPool.FBlockSize) then
    MPool := SuperMemoryPool
  else if len <= DWORD(LargeMemoryPool.FBlockSize) then
    MPool := LargeMemoryPool
  else if len <= DWORD(SuperLargeMemoryPool.FBlockSize) then
    MPool := SuperLargeMemoryPool
  else if len <= MaxBlockMPool.FBlockSize then
    MPool := MaxBlockMPool
  else
    MPool := nil;


  if MPool <> nil then
  begin
    MemBlock := MPool.GetMemoryBlock;
    MemBlock^.DataLen := len;
    MemBlock^.NextEx := nil;
    MemBlock^.PrevEx := nil;
    Move(buf^,MemBlock^.Memory^,len);
    if FHead = nil then
      FHead := MemBlock;
    if FLast = nil then
      FLast := MemBlock
    else
    begin
      FLast^.NextEx := MemBlock;
      MemBlock^.PrevEx := FLast;
      FLast := MemBlock;
    end;
  end
  else
  begin
    MemBlock := MaxMemoryPool.GetMemoryBlock;
    MemBlock^.NextEx := nil;
    MemBlock^.PrevEx := nil;
    if MaxMemoryPool.FBlockSize <= Len then
    begin
      MemBlock^.DataLen := MaxMemoryPool.FBlockSize;
      Move(buf^,MemBlock^.Memory^,MemBlock^.DataLen)
    end
    else
    begin
      MemBlock^.DataLen := Len;
      Move(buf^,MemBlock^.Memory^,len);
    end;
    if FHead = nil then
      FHead := MemBlock;
    if FLast = nil then
      FLast := MemBlock
    else
    begin
      FLast^.NextEx := MemBlock;
      MemBlock^.PrevEx := FLast;
      FLast := MemBlock;
    end;
    Dec(len,MemBlock^.DataLen);
    Inc(buf,MemBlock^.DataLen);
    if Len > 0 then
      AddBuffer(buf,len);
  end;
end;

procedure TBufferLink.AddMemBlockLink(MemBlockLink: PMemoryBlock);
begin
  MemBlockLink^.NextEx := nil;
  MemBlockLink^.PrevEx := nil;
  if FHead = nil then
    FHead := MemBlockLink;
  if FLast = nil then
    FLast := MemBlockLink
  else
  begin
    FLast^.NextEx := MemBlockLink;
    MemBlockLink^.PrevEx := FLast;
    FLast := MemBlockLink;
  end;
end;

procedure TBufferLink.clearBuffer;
var
  lvBuf, lvFreeBuf: PMemoryBlock;
begin
  lvBuf := FLast;
  while lvBuf <> nil do
  begin
    lvFreeBuf := lvBuf;
    lvBuf := lvBuf.PrevEx;
    case lvFreeBuf^.BlockType of
    MB_Small: SmallMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_Normal: MemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_SpBig: SuperMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_Large: LargeMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_SPLarge: SuperLargeMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_Max: MaxMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_MaxBlock: MaxBlockMPool.FreeMemoryBlock(lvFreeBuf);
    else BigMemoryPool.FreeMemoryBlock(lvFreeBuf);
    end;
  end;
  FHead := nil;
  FLast := nil;
  FRead := nil;
  FReadPosition := 0;
  FMark := nil;
  FMarkPosition := 0;
end;

procedure TBufferLink.clearHaveReadBuffer;
var
  lvBuf, lvFreeBuf: PMemoryBlock;
begin
  if FRead = nil then exit;
    lvBuf := FRead.PrevEx;
  while lvBuf <> nil do
  begin
    lvFreeBuf :=lvBuf;
    lvBuf := lvBuf.PrevEx;
    case lvFreeBuf^.BlockType of
    MB_Small: SmallMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_Normal: MemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_SpBig: SuperMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_Large: LargeMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_SPLarge: SuperLargeMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_Max: MaxMemoryPool.FreeMemoryBlock(lvFreeBuf);
    MB_MaxBlock: MaxBlockMPool.FreeMemoryBlock(lvFreeBuf);
    else BigMemoryPool.FreeMemoryBlock(lvFreeBuf);
    end;
  end;
  //之前都被清理
  FRead.PrevEx := nil;
  FHead := FRead;
end;

constructor TBufferLink.Create;
begin
  inherited Create;
  FReadPosition := 0;
end;

destructor TBufferLink.Destroy;
begin
  clearBuffer;
  inherited;
end;

function TBufferLink.FirstBlock: PMemoryBlock;
begin
  Result := FHead;
end;

function TBufferLink.GetCurBlock: PMemoryBlock;
begin
  Result := FRead;
  if Result = nil then
    Result := FHead;
end;

function TBufferLink.InnerReadBuf(const pvBufRecord: PMemoryBlock;
  pvStartPostion: Cardinal; buf: PAnsiChar; len: Cardinal): Cardinal;
var
  lvValidCount:Cardinal;
begin
  Result := 0;
  if pvBufRecord <> nil then
  begin
    lvValidCount := pvBufRecord.Datalen - pvStartPostion;//本块内存还剩下的内存数据
    if lvValidCount <= 0 then
      Result := 0
    else
    begin
      if len <= lvValidCount then //数据全部在本块内存中获取到
      begin
        CopyMemory(buf, Pointer(NativeUInt(pvBufRecord.Memory) + pvStartPostion), len);
        Result := len;
      end
      else
      begin
        CopyMemory(buf, Pointer(NativeUInt(pvBufRecord.Memory) + pvStartPostion), lvValidCount);
        Result := lvValidCount; //本块内存中获取的内存数据不够，返回本块读取的剩下的数据，需要到下一块内存继续读取
      end;
    end;
  end;
end;

function TBufferLink.readBuffer(const buf: PAnsiChar;
  len: Cardinal): Cardinal;
var
  lvBuf: PMemoryBlock;
  lvPosition, l, lvReadCount, lvRemain:Cardinal;
begin
  lvReadCount := 0;
  lvBuf := FRead;
  lvPosition := FReadPosition;
  if lvBuf = nil then
  begin
    lvBuf := FHead;
    lvPosition := 0;
  end;

  if lvBuf <> nil then
  begin
    lvRemain := len;
    while lvBuf <> nil do
    begin
      l := InnerReadBuf(lvBuf, lvPosition, Pointer(NativeUInt(buf) + lvReadCount), lvRemain);
      if l = lvRemain then
      begin
        //读完
        inc(lvReadCount, l);
        Inc(lvPosition, l);
        FReadPosition := lvPosition;
        FRead := lvBuf;
        Break;
      end
      else if l < lvRemain then  //读取的比需要读的长度小
      begin
        lvRemain := lvRemain - l;
        inc(lvReadCount, l);
        Inc(lvPosition, l);
        FReadPosition := lvPosition;
        FRead := lvBuf;
        lvBuf := lvBuf.NextEx;
        if lvBuf <> nil then   //读下一个
        begin
          FRead := lvBuf;
          FReadPosition := 0;
          lvPosition := 0;
        end;
      end;
    end;
    Result := lvReadCount;
  end
  else Result := 0;
end;

procedure TBufferLink.RemoveBlock(Block: PMemoryBlock;const FreeBlock: Boolean = False);
begin
  if Block^.NextEx <> nil then
    Block^.NextEx^.PrevEx := Block^.PrevEx;
  if Block^.PrevEx <> nil then
    Block^.PrevEx^.NextEx := Block^.NextEx;
  if Block = FHead then
    FHead := Block^.NextEx;
  if Block = FLast then
    FLast := Block^.PrevEx;
  if FreeBlock then
    FreeMemBlock(Block);
end;

procedure TBufferLink.restoreReaderIndex;
begin
  FRead := FMark;
  FReadPosition := FMarkPosition;
end;

procedure TBufferLink.markReaderIndex;
begin
  FMark := FRead;
  FMarkPosition := FReadPosition;
end;

function TBufferLink.SearchBuffer(subBuf:PAnsiChar; subBufLen:Cardinal):
    Integer;
var
  lvBufBlock: PMemoryBlock;
  LCompLen,HasCompLen:Cardinal;
  SearchStart,SubPointer,search1,LLast: PAnsiChar;
  LPosition,BlockSize,LResult,HasRead:Integer;
  FHasFound: Boolean;
  BlockIndex: Integer;
begin
  Result := -1;
  lvBufBlock := FRead;
  if lvBufBlock = nil then
  begin   // 从头开始
    lvBufBlock := FHead;
  end;
  BlockIndex := 0;
  HasRead := 0;
  if lvBufBlock <> nil then
  begin
    FHasFound := False;
    SubPointer := subBuf;
    Result := FReadPosition;
    LPosition := FReadPosition;
    HasCompLen := 0;
    while lvBufBlock <> nil do
    begin
      case lvBufBlock.BlockType of
      MB_Small: BlockSize := SmallMemoryPool.FBlockSize;
      MB_Normal: BlockSize := MemoryPool.FBlockSize;
      MB_SpBig: BlockSize := SuperMemoryPool.FBlockSize;
      MB_Large: BlockSize := LargeMemoryPool.FBlockSize;
      MB_SPLarge: BlockSize := SuperLargeMemoryPool.FBlockSize;
      MB_Max: BlockSize := MaxMemoryPool.FBlockSize;
      MB_MaxBlock: BlockSize := MaxBlockMPool.FBlockSize;
      else BlockSize := BigMemoryPool.FBlockSize;
      end;
      SearchStart := PAnsiChar(lvBufBlock.Memory);
      LLast := SearchStart;
      Inc(LLast,BlockSize);
      inc(SearchStart,LPosition);
      while LPosition < BlockSize do
      begin
        if SearchStart^ = SubPointer^ then //匹配到，匹配后面的
        begin
          LCompLen := 1 + HasCompLen;
          search1 := SearchStart;
          if BlockIndex = 0 then
            LResult := Integer(search1)- Integer(PAnsiChar(lvBufBlock.Memory)) - FReadPosition
          else LResult := Integer(search1)- Integer(PAnsiChar(lvBufBlock.Memory));
          Inc(search1);
          Inc(SubPointer);
          while LCompLen < subBufLen do
          begin
            if Integer(search1) = integer(LLast) then //已经匹配到末尾
            begin
              HasCompLen := LCompLen;
              Break;
            end
            else if search1^ = subPointer^ then
            begin
              Inc(search1);
              Inc(SubPointer);
              Inc(LCompLen);
              Inc(LResult);
            end
            else
            begin
              if Integer(search1) <> integer(LLast) then
              begin
                SubPointer := subBuf;
                HasCompLen := 0;
              end
              else HasCompLen := LCompLen;
              Break;
            end;
          end;
          FHasFound := LCompLen = subBufLen;
          if FHasFound then
          begin
            Result := HasRead + LResult - subBufLen + 1;
            Break;
          end;
        end;
        Inc(LPosition);
        Inc(SearchStart);
      end;
      if FHasFound then
         Break
      else Result := -1; // 感谢[杭州]军  444356747反馈
      if BlockIndex = 0 then
        HasRead := BlockSize - FReadPosition
      else Inc(HasRead,BlockSize);
      Inc(BlockIndex);
      lvBufBlock := lvBufBlock^.NextEx;
      LPosition := 0;
      //
      {lvPRes := SearchPointer(lvBufBlock.Memory, lvBufBlock.DataLen, lvPosition, subBuf, subBufLen);
      if lvPRes <> nil then
      begin
        x := PAnsiChar(lvPRes) - PAnsiChar(lvBufBlock.Memory) - lvPosition;
        Result := l + x;
        Break;
      end
      else
      begin
        l := l + lvBufBlock.DataLen - lvPosition;
        // 查找下一块
        lvBufBlock := lvBufBlock.NextEx;
        lvPosition := 0;
      end;}
    end;
  end;
end;

function TBufferLink.validCount: Integer;
var
  lvNext: PMemoryBlock;
begin
  Result := 0;
  if FRead = nil then
    lvNext:= FHead
  else
  begin
    Result := FRead.DataLen - FReadPosition;
    lvNext := FRead.NextEx;
  end;
  while lvNext <> nil do
  begin
    Inc(Result, lvNext.DataLen);
    lvNext := lvNext.NextEx;
  end;
end;

function TBufferLink.Skip(len: Cardinal): Cardinal;
var
  lvBuf: PMemoryBlock;
  lvPosition, l, lvReadCount, lvRemain:Cardinal;
begin
  lvReadCount := 0;
  lvBuf := FRead;
  lvPosition := FReadPosition;
  if lvBuf = nil then
  begin
    lvBuf := FHead;
    lvPosition := 0;
  end;

  if lvBuf <> nil then
  begin
    lvRemain := len;
    while lvBuf <> nil do
    begin
      l := lvBuf.Datalen - lvPosition;//本块内存还剩下的内存数据;
      if l >= lvRemain then
      begin
        //读完
        inc(lvReadCount, lvRemain);
        Inc(lvPosition, lvRemain);
        FReadPosition := lvPosition;
        FRead := lvBuf;
        Break;
      end
      else if l < lvRemain then  //读取的比需要读的长度小
      begin
        lvRemain := lvRemain - l;
        inc(lvReadCount, l);
        Inc(lvPosition, l);
        FReadPosition := lvPosition;
        FRead := lvBuf;
        lvBuf := lvBuf.NextEx;
        if lvBuf <> nil then   //读下一个
        begin
          FRead := lvBuf;
          FReadPosition := 0;
          lvPosition := 0;
        end;
      end;
    end;
    Result := lvReadCount;
  end
  else Result := 0;
end;

{ TDxObjectPool }

constructor TDxObjectPool.Create(AMaxCount: Integer);
begin
  FUses := TList.Create;
  FUnUses := TList.Create;
  FMaxObjCount := AMaxCount;
  FLocker := TCriticalSection.Create;
end;

constructor TDxObjectPool.Create(AMaxCount: Integer; ObjClass: TClass);
begin
  Create(AMaxCount);
  FObjClass := ObjClass;
end;

destructor TDxObjectPool.Destroy;
begin
  FLocker.Free;
  while FUses.Count > 0 do
  begin
    TObject(FUses[FUses.Count - 1]).Free;
    FUses.Delete(FUses.Count - 1);
  end;

  while FUnUses.Count > 0 do
  begin
    TObject(FUnUses[FUnUses.Count - 1]).Free;
    FUnUses.Delete(FUnUses.Count - 1);
  end;
  FUses.Free;
  FUnUses.Free;
  inherited;
end;

procedure TDxObjectPool.FreeObject(Obj: TObject);
begin
  FLocker.Enter;
  try
    FUses.Remove(obj);
    if FUnUses.Count + 1 > FMaxObjCount then
      obj.Free
    else FUnUses.Add(Obj);
  finally
    FLocker.Leave;
  end;
end;

function TDxObjectPool.GetObject: TObject;
begin
  FLocker.Enter;
  try
    if FUnUses.Count = 0 then
    begin
      if Assigned(FOnCreateObject) then
        FOnCreateObject(self,Result)
      else if FObjClass.InheritsFrom(TComponent) then
        Result := TComponentClass(FObjClass).Create(nil)
      else Result := FObjClass.Create;
      if Result <> nil then
        FUses.Add(Result);
    end
    else
    begin
      Result := TObject(FUnUses[FUnUses.Count - 1]);
      FUnUses.Delete(FUnUses.Count - 1);
      FUses.Add(Result)
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure FreeObjPool;
begin
  if NormalMPool <> nil then
    NormalMPool.Free;
  if SmallMPool <> nil then
    SmallMPool.Free;
  if BigMPool <> nil then
    BigMPool.Free;
  if SuperBigMPool <> nil then
    SuperBigMPool.Free;
  if LargeMPool <> nil then
    LargeMPool.Free;
  if SPLargeMPool <> nil then
    SPLargeMPool.Free;
  if MaxMPool <> nil then
    MaxMPool.Free;
  if FMaxBlockMPool <>nil then
    FMaxBlockMPool.Free;
end;

{ TDxRingStream }

constructor TDxRingStream.Create(const RingBufferSize: DWORD;
  const MemType: TDxMemBlockType);
begin
  inherited Create;
  FMemBlockType := MemType;
  if RingBufferSize <> 0 then
    SetSize(RingBufferSize);
end;

function TDxRingStream.GetCanWriteSize: Integer;
begin
  if FWritePosition > FReadPosition then
    Result := FSize - FWritePosition + FReadPosition
  else if FWritePosition < FReadPosition then
    Result := FReadPosition - FWritePosition
  else if FReadPosition = 0 then
    Result := FSize
  else Result := 0;
end;

function TDxRingStream.GetDataSize: Integer;
begin
  if FWritePosition > FReadPosition then
    Result := FWritePosition - FReadPosition
  else if FWritePosition < FReadPosition then
    Result := FWritePosition + FSize - FReadPosition
  else if FWritePosition = 0 then
    Result := 0
  else Result := FSize;
end;

function TDxRingStream.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TDxRingStream.markReaderIndex;
begin
  FMarkRead := FReadBlock;
  FMarkReadPosition := FReadBlockPos;
end;

procedure TDxRingStream.markWriterIndex;
begin
  FMarkWrite := FWriteBlock;
  FMarkWritePosition := FWriteBlockPos;
end;

function TDxRingStream.Read(var Buffer; Count: Integer): Longint;
var
  p: PAnsiChar;
  MPool: TDxMemoryPool;
  pBuf: Pointer;
  CanReadSize: Integer;
begin
  if FReadBlock = nil then
    Result := 0
  else
  begin
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    CanReadSize := DataSize;
    if Count > CanReadSize then
      Count := CanReadSize;
    if Count = 0 then
    begin
      result := 0;
      Exit;
    end;

    if FReadBlockPos = MPool.FBlockSize then
    begin
      FReadBlock := FReadBlock^.NextEx;
      FReadBlockPos := 0;
    end;

    if FReadBlock = nil then
    begin
      result := 0;
      Exit;
    end;

    Result := Count;
    p := @Buffer;
    //先读取当前块剩下的区域
    pBuf := Pointer(NativeUInt(FReadBlock^.Memory) + DWORD(FReadBlockPos));
    if Count <= MPool.FBlockSize - FReadBlockPos then //足够写了
    begin
      Move(PBuf^,buffer,Count);
      Inc(FReadPosition,Count);
      inc(FReadBlockPos,Count);
      Count := 0;
    end
    else
    begin
      Move(PBuf^,Buffer,MPool.FBlockSize - FReadBlockPos);
      Dec(Count,MPool.FBlockSize - FReadBlockPos);
      Inc(FReadPosition,MPool.FBlockSize - FReadBlockPos);
      Inc(p,MPool.FBlockSize - FReadBlockPos);
      FReadBlock := FReadBlock^.NextEx;
      FReadBlockPos := 0;
    end;
    if FReadBlockPos = MPool.FBlockSize then
    begin
      FReadBlock := FReadBlock^.NextEx;
      FReadBlockPos := 0;
    end;
    while Count > 0 do
    begin
      if Count > MPool.FBlockSize then
      begin
        Move(FReadBlock^.Memory^,p^,MPool.FBlockSize);
        Dec(Count,MPool.FBlockSize);
        FReadBlockPos := MPool.FBlockSize;
        Inc(FReadPosition,MPool.FBlockSize);
        Inc(p,MPool.FBlockSize);
      end
      else
      begin
        Move(FReadBlock^.Memory^,p^,Count);
        Inc(FReadPosition,Count);
        Inc(p,Count);
        FReadBlockPos := Count;
        Count := 0;
      end;
      if FReadBlockPos = MPool.FBlockSize then
      begin
        FReadBlock := FReadBlock^.NextEx;
        FReadBlockPos := 0;
      end;
    end;
  end;
end;

procedure TDxRingStream.ReadBuffer(var Buffer; Count: Integer);
begin
  read(buffer,Count);
end;

procedure TDxRingStream.restoreReaderIndex;
begin
  FReadBlock := FMarkRead;
  FReadBlockPos := FMarkReadPosition;
end;

procedure TDxRingStream.restoreWriterIndex;
begin
  FWriteBlock := FMarkWrite;
  FWriteBlockPos := FMarkWritePosition;
end;

function TDxRingStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := 0;
end;

function TDxRingStream.SeekReadWrite(Offset: Integer; Origin: TSeekOrigin;
  SeekRead: Boolean): Longint;
var
  MPool: TDxMemoryPool;
  BIndex: integer;
  FLastSize: Integer;
begin
  if FHead <> nil then
  begin
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    case TSeekOrigin(Origin) of
    soBeginning:
      begin
        if Offset < 0 then
          Offset := 0;
        if Offset <= FSize then
        begin
          BIndex := Offset div MPool.FBlockSize;
          if SeekRead then
          begin
            FReadBlockPos := Offset mod MPool.FBlockSize;
            FReadBlock := FHead;
            FReadPosition := FReadBlockPos;
          end
          else
          begin
            FWriteBlockPos := Offset mod MPool.FBlockSize;
            FWriteBlock := FHead;
            FWritePosition := FWriteBlockPos;
          end;
          if BIndex > 0 then
          begin
            if SeekRead then
            repeat
              Inc(FReadPosition,MPool.FBlockSize);
              Dec(Bindex);
              FReadBlock := FReadBlock^.NextEx;
            until (BIndex = 0) or (FReadBlock = nil)
            else
            repeat
              Inc(FWritePosition,MPool.FBlockSize);
              Dec(Bindex);
              FWriteBlock := FWriteBlock^.NextEx;
            until (BIndex = 0) or (FWriteBlock = nil)
          end;
        end
        else
        begin
          if SeekRead then
          begin
            FReadBlock := FLast;
            FReadBlockPos := FCapacity - FSize;
            FReadPosition := Size;
          end
          else
          begin
            FWriteBlock := FLast;
            FWriteBlockPos := FCapacity - FSize;
            FWritePosition := Size;
          end;
        end;
      end;
    soCurrent:
      begin
        if Offset > 0 then
        begin
          if SeekRead then
          begin
            BIndex := (FReadBlockPos + Offset) div MPool.FBlockSize;
            Inc(FReadPosition,MPool.FBlockSize - FReadBlockPos);
            FReadBlockPos := (FReadBlockPos + Offset) mod MPool.FBlockSize;
            Inc(FReadPosition,FReadBlockPos);
            while BIndex > 0 do
            begin
              FReadBlock := FReadBlock^.NextEx;
              Dec(BIndex);
              if BIndex > 0 then
                Inc(FReadPosition,MPool.FBlockSize);
            end;
          end
          else
          begin
            BIndex := (FWriteBlockPos + Offset) div MPool.FBlockSize;
            Inc(FWritePosition,MPool.FBlockSize - FWriteBlockPos);
            FWriteBlockPos := (FWriteBlockPos + Offset) mod MPool.FBlockSize;
            Inc(FWritePosition,FWriteBlockPos);
            while BIndex > 0 do
            begin
              FWRiteBlock := FWRiteBlock^.NextEx;
              Dec(BIndex);
              if BIndex > 0 then
                Inc(FWritePosition,MPool.FBlockSize);
            end;
          end;
        end
        else
        begin
          if SeekRead then
          begin
            if Offset + FReadBlockPos >= 0 then
            begin
              Inc(FReadPosition,offset);
              Inc(FReadBlockPos,offset);
            end
            else
            begin
              FReadBlock := FReadBlock^.PrevEx;
              if FReadBlock = nil then
              begin
                FReadBlockPos := 0;
                FReadBlock := FHead;
              end
              else
              begin
                inc(offset,FReadBlockPos);
                Dec(FReadPosition,FReadBlockPos);
                FReadBlockPos := MPool.FBlockSize;
                SeekReadWrite(Offset,soCurrent,SeekRead);
              end;
            end;
          end
          else
          begin
            if Offset + FWriteBlockPos >= 0 then
            begin
              Inc(FWritePosition,offset);
              Inc(FWriteBlockPos,offset);
            end
            else
            begin
              FWriteBlock := FWriteBlock^.PrevEx;
              if FWriteBlock = nil then
              begin
                FWriteBlockPos := 0;
                FWriteBlock := FHead;
              end
              else
              begin
                inc(offset,FWriteBlockPos);
                Dec(FWritePosition,FWriteBlockPos);
                FWriteBlockPos := MPool.FBlockSize;
                SeekReadWrite(Offset,soCurrent,SeekRead);
              end;
            end;
          end;
        end;
      end;
    soEnd:
      begin
        if Offset < 0 then
          Offset := -offset;
        if Offset < FSize then
        begin
          FLastSize := MPool.FBlockSize - FCapacity + FSize; //最后一个大小
          if Offset <= FLastSize then
          begin
            if SeekRead then
            begin
              FReadBlockPos := FLastSize - Offset;
              FReadBlock := FLast;
              FReadPosition := FSize - Offset;
            end
            else
            begin
              FWriteBlockPos := FLastSize - Offset;
              FWriteBlock := FLast;
              FWritePosition := FSize - Offset;
            end;
          end
          else
          begin
            Dec(Offset,FLastSize);
            if SeekRead then
            begin
              FReadBlock := FLast^.PrevEx;
              FReadPosition := FSize - FLastSize;
            end
            else
            begin
              FWriteBlock := FLast^.PrevEx;
              FWritePosition := FSize - FLastSize;
            end;
            BIndex := Offset div MPool.FBlockSize;
            Offset := Offset mod MPool.FBlockSize;
            if SeekRead then
            begin
              FReadBlockPos := MPool.FBlockSize - Offset;
              Dec(FReadPosition,MPool.FBlockSize - FReadBlockPos);
              while BIndex > 0 do
              begin
                FReadBlock := FReadBlock^.PrevEx;
                Dec(BIndex);
                Dec(FReadPosition,MPool.FBlockSize);
              end;
            end
            else
            begin
              FWriteBlockPos := MPool.FBlockSize - Offset;
              Dec(FWritePosition,MPool.FBlockSize - FWriteBlockPos);
              while BIndex > 0 do
              begin
                FWriteBlock := FWriteBlock^.PrevEx;
                Dec(BIndex);
                Dec(FWritePosition,MPool.FBlockSize);
              end;
            end;
          end;
        end
        else if SeekRead then
        begin
          FReadPosition := 0;
          FReadBlockPos := 0;
          FReadBlock := FHead;
        end
        else
        begin
          FWritePosition := 0;
          FWriteBlockPos := 0;
          FWriteBlock := FHead;
        end;
      end;
    end;
    if SeekRead then
      Result := FReadPosition
    else Result := FWritePosition;
  end
  else Result := 0;
end;

procedure TDxRingStream.SetReadPosition(const Value: Integer);
begin
  SeekReadWrite(Value, soBeginning,true);
end;

procedure TDxRingStream.SetSize(NewSize: Integer);
var
  CurCount: integer;
  mBlock: PMemoryBlock;
  MPool: TDxMemoryPool;
begin
  if FSize <> NewSize then
  begin
    FSize := NewSize;
    case FMemBlockType of
      MB_Small: MPool := SmallMemoryPool;
      MB_Normal: MPool := MemoryPool;
      MB_SpBig: MPool := SuperMemoryPool;
      MB_Large: MPool := LargeMemoryPool;
      MB_SPLarge: MPool := SuperLargeMemoryPool;
      MB_Max: MPool := MaxMemoryPool;
      MB_MaxBlock: MPool := MaxBlockMPool;
      else MPool := BigMemoryPool;
    end;
    CurCount := FCapacity div MPool.FBlockSize;
    FMemBlockCount := NewSize div MPool.FBlockSize;
    if NewSize mod MPool.FBlockSize <> 0 then
      Inc(FMemBlockCount);
    if CurCount <> FMemBlockCount then
    begin
      while CurCount > FMemBlockCount do
      begin //内存回收
        mBlock := FLast;
        FLast := FLast^.PrevEx;
        MPool.FreeMemoryBlock(mBlock);
        Dec(CurCount);
      end;
      while CurCount < FMemBlockCount do
      begin
        mBlock := MPool.GetMemoryBlock;
        mBlock.NextEx := nil;
        mBlock.PrevEx := nil;
        if FHead = nil then
        begin
          FHead := mBlock;
          FLast := mBlock;
          FHead.NextEx := nil;
          FHead.PrevEx := nil;
        end
        else
        begin
          FLast^.NextEx := mBlock;
          mBlock^.PrevEx := FLast;
          FLast := mBlock;
        end;
        Inc(CurCount);
      end;
      FCapacity := MPool.FBlockSize * FMemBlockCount;
      if FLast <> nil then //指向头部形成一个环形
        FLast.NextEx := FHead;
    end;
    if NewSize = 0 then
    begin
      FReadBlock := nil;
      FWriteBlock := nil;
      FHead := nil;
      FLast := nil;
      FWriteBlockPos := 0;
      FReadBlockPos := 0;
    end
    else
    begin
      if FReadPosition > NewSize then
        ReadPosition := NewSize;
      if FReadBlock = nil then
      begin
        FReadBlock := FHead;
        FReadBlockPos := 0;
        FReadPosition := 0;
      end;

      if FWritePosition > NewSize then
        WritePosition := NewSize;
      if FWriteBlock = nil then
      begin
        FWriteBlock := FHead;
        FWriteBlockPos := 0;
        FWritePosition := 0;
      end;
    end;
  end;
end;

procedure TDxRingStream.SetWritePostion(const Value: Integer);
begin
  SeekReadWrite(Value, soBeginning,False);
end;

function TDxRingStream.Write(const Buffer; Count: Integer): Longint;
var
  MPool: TDxMemoryPool;
  pBuf: Pointer;
  tmpBuf: PByte;
  CSize: Integer;
begin
  CSize := CanWriteSize;
  if CSize = 0 then
  begin
    Result := 0;
    Exit;
  end
  else if Count > CSize then
    Count := CSize;
  case FMemBlockType of
    MB_Small: MPool := SmallMemoryPool;
    MB_Normal: MPool := MemoryPool;
    MB_SpBig: MPool := SuperMemoryPool;
    MB_Large: MPool := LargeMemoryPool;
    MB_SPLarge: MPool := SuperLargeMemoryPool;
    MB_Max: MPool := MaxMemoryPool;
    MB_MaxBlock: MPool := MaxBlockMPool;
    else MPool := BigMemoryPool;
  end;

  if FWriteBlockPos = MPool.FBlockSize then
  begin
    FWriteBlock := FWriteBlock^.NextEx;
    FWriteBlockPos := 0;
  end;

  Result := Count;
  tmpBuf := @Buffer;
  //先写满当前未写满的内存块空间
  pBuf := Pointer(NativeUInt(FWriteBlock^.Memory) + DWORD(FWriteBlockPos));
  if Count <= MPool.FBlockSize - FWriteBlockPos then //足够写了
  begin
    Move(Buffer,pBuf^,Count);
    inc(FWriteBlockPos,Count);
    Inc(FWritePosition,Count);
    Count := 0;
  end
  else
  begin
    Move(Buffer,PBuf^,MPool.FBlockSize - FWriteBlockPos);
    Dec(Count,MPool.FBlockSize - FWriteBlockPos);
    Inc(tmpBuf,MPool.FBlockSize - FWriteBlockPos);

    Inc(FWritePosition,MPool.FBlockSize - FWriteBlockPos);
    FWriteBlock := FWriteBlock^.NextEx;
    FWriteBlockPos := 0;
  end;
  while Count > 0 do
  begin
    if Count > MPool.FBlockSize then
    begin
      Move(tmpBuf^,FWriteBlock^.Memory^,MPool.FBlockSize);
      Inc(tmpBuf,MPool.FBlockSize);
      Dec(Count,MPool.FBlockSize);
      FWriteBlockPos := MPool.FBlockSize;
      Inc(FWritePosition,MPool.FBlockSize);
    end
    else
    begin
      Move(tmpBuf^,FWriteBlock^.Memory^,Count);
      Inc(FWritePosition,Count);
      Inc(tmpBuf,Count);
      FWriteBlockPos := Count;
      Count := 0;
    end;
    if FWriteBlockPos = MPool.FBlockSize then
    begin
      FWriteBlock := FWriteBlock^.NextEx;
      FWriteBlockPos := 0;
    end;
  end;
end;

procedure TDxRingStream.WriteBuffer(const Buffer; Count: Integer);
begin
  Write(Buffer,Count)
end;


procedure FreeMemBlock(Block: PMemoryBlock);
begin
   case Block^.BlockType of
    MB_Small: SmallMemoryPool.FreeMemoryBlock(Block);
    MB_Normal: MemoryPool.FreeMemoryBlock(Block);
    MB_SpBig: SuperMemoryPool.FreeMemoryBlock(Block);
    MB_Large: LargeMemoryPool.FreeMemoryBlock(Block);
    MB_SPLarge: SuperLargeMemoryPool.FreeMemoryBlock(Block);
    MB_Max: MaxMemoryPool.FreeMemoryBlock(Block);
    MB_MaxBlock: MaxBlockMPool.FreeMemoryBlock(Block);
   else BigMemoryPool.FreeMemoryBlock(Block);
   end;
end;

initialization

finalization
  FreeObjPool;

end.
