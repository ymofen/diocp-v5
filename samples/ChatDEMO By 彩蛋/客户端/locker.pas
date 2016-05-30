unit locker;

interface

uses
  SyncObjs, utils_locker;

type
  TLocker = class(TCriticalSection)
  private
    FName,
    FEnterInfo: string;
    function GetEnterCount: Integer;
  public
    constructor Create(const AName: String = '');
    destructor Destroy; override;

    procedure Lock(const ADebugInfo: String = '');
    procedure UnLock;
    //
    property EnterCount: Integer read GetEnterCount;
    property Name: String read FName write FName;
  end;

implementation

{ TLocker }

constructor TLocker.Create(const AName: String);
begin
  inherited Create;
  FName := AName;
end;

destructor TLocker.Destroy;
begin
  inherited Destroy;
end;

function TLocker.GetEnterCount: Integer;
begin
  Result := FSection.RecursionCount;
end;

procedure TLocker.Lock(const ADebugInfo: String);
begin
  Enter;
  FEnterInfo := ADebugInfo
end;

procedure TLocker.UnLock;
begin
  Leave;
  FEnterInfo := '';
end;

end.
