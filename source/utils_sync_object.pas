unit utils_sync_object;

interface

uses
  Classes, SyncObjs, SysUtils
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}

  {$ENDIF}
  , utils_strings;

type
  /// <summary>
  ///   通过引用计数来管理对象和自身的生命周期
  /// </summary>
  TObjectRef = class(TObject)
  private
    FFlag:Integer;
    FObject: TObject;
    FRefCount:Integer;
    procedure DoDestroy;
  public
    /// <summary>
    ///   会进行一次引用，引用计数为1
    /// </summary>
    constructor Create(AObject: TObject);
    procedure CheckBeginRefObject;
    function BeginRefObject: Boolean;
    function ReleaseRefObject: Integer;
    property RefObject: TObject read FObject;
    property RefCount: Integer read FRefCount;
  end;

  TMREWLock = class(TObject)
  private
    FLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockForRead();
    procedure UnLockForRead();

    procedure LockForWrite();
    procedure UnLockForWrite();
  end;


procedure ASyncExecute(const pvCallBack: TNotifyEvent; const pvSender: TObject);


{$IF RTLVersion<24}
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AtomicIncrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
function AtomicDecrement(var Target: Integer): Integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
{$IFEND <XE5}

implementation

type
  TASyncWorker = class(TThread)
  private
    FData: Pointer;
    FDataObj: TObject;
    FDataTag: Integer;
    FOnNotifyEvent: TNotifyEvent;
    procedure SetDataObj(const Value: TObject);

  public
    constructor Create;
    procedure Execute; override;
    property Data: Pointer read FData write FData;
    property DataObj: TObject read FDataObj write SetDataObj;
    property DataTag: Integer read FDataTag write FDataTag;

    property Terminated;     
  end;


{$IF RTLVersion < 18}

function InterlockedIncrement(var Addend: Integer): Integer; stdcall; external kernel32 name 'InterlockedIncrement';
{$EXTERNALSYM InterlockedIncrement}
function InterlockedDecrement(var Addend: Integer): Integer; stdcall; external kernel32 name 'InterlockedDecrement';
{$EXTERNALSYM InterlockedDecrement}
function InterlockedExchange(var Target: Integer; Value: Integer): Integer; stdcall;external kernel32 name 'InterlockedExchange';
{$EXTERNALSYM InterlockedExchange}
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall;external kernel32 name 'InterlockedCompareExchange';
{$EXTERNALSYM InterlockedCompareExchange}

function InterlockedExchangeAdd(Addend: PLongint; Value: Longint): Longint; overload; external kernel32 name 'InterlockedExchangeAdd';
function InterlockedExchangeAdd(var Addend: Longint; Value: Longint): Longint; overload; external kernel32 name 'InterlockedExchangeAdd';
{$IFEND <D2007}


function TObjectRef.BeginRefObject: Boolean;
begin
  AtomicIncrement(FRefCount);

  Result := FRefCount > 0;
end;

constructor TObjectRef.Create(AObject: TObject);
begin
  inherited Create;
  FFlag := 0;
  FObject := AObject;
  FRefCount := 1;
  FObject := AObject;
end;

procedure TObjectRef.CheckBeginRefObject;
begin
  Assert(BeginRefObject, 'BeginRefObject fail');
end;

procedure TObjectRef.DoDestroy;
begin
  try
    Self.FObject.Free;
  except
  end;

  try
    self.Free;
  except
  end;
end;

function TObjectRef.ReleaseRefObject: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    PrintDebugString('TObjectRef.ReleaseRefObject->DoDestroy');
    DoDestroy;
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


procedure ASyncExecute(const pvCallBack: TNotifyEvent; const pvSender: TObject);
var
  lvWorker:TASyncWorker;
begin
  lvWorker := TASyncWorker.Create();
  lvWorker.FOnNotifyEvent := pvCallBack;
  lvWorker.DataObj := pvSender;
  {$IFDEF UNICODE}
  lvWorker.Start;
  {$ELSE}
  lvWorker.Resume;
  {$ENDIF}

end;

constructor TMREWLock.Create;
begin
  inherited Create;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create();
end;

destructor TMREWLock.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

{ TMREWLock }

procedure TMREWLock.LockForRead;
begin
  FLock.BeginRead;
end;

procedure TMREWLock.LockForWrite;
var
  r:boolean;
begin
  r := FLock.BeginWrite;
  Assert(r, 'lock write fail');
end;

procedure TMREWLock.UnLockForRead;
begin
  FLock.EndRead;
end;

procedure TMREWLock.UnLockForWrite;
begin
  FLock.EndWrite;
end;

constructor TASyncWorker.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TASyncWorker.Execute;
begin
  if Assigned(FOnNotifyEvent) then
  begin
    FOnNotifyEvent(self.FDataObj);
  end;
end;

procedure TASyncWorker.SetDataObj(const Value: TObject);
begin
  FDataObj := Value;
end;

end.
