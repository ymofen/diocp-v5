(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-02-22 08:29:43
 *     DIOCP-V5 发布
 *
 *)
unit utils_locker;

interface

{$DEFINE USECriticalSection}

uses
  {$IFDEF USECriticalSection}
    SyncObjs,
  {$ELSE}
    Windows,
  {$ENDIF}
  SysUtils, Classes;

type
  {$IFDEF USECriticalSection}
    TIocpLocker = class(TCriticalSection)
  {$ELSE}
    TIocpLocker = class(TObject)
  {$ENDIF}
  private
    FEnterINfo: string;
    FTryEnterInfo: String;
    FName: String;

  {$IFDEF USECriticalSection}
  {$ELSE}
    FSection: TRTLCriticalSection;
  {$ENDIF}

    function GetEnterCount: Integer;
  public
    constructor Create(pvName: String = '');
    destructor Destroy; override;

    procedure lock(const pvDebugInfo: String = '');
    procedure unLock;

    property EnterCount: Integer read GetEnterCount;

    property EnterINfo: string read FEnterINfo;

    property TryEnterInfo: String read FTryEnterInfo;

    function getDebugINfo():String;

    property Name: String read FName write FName;

  end;

implementation

constructor TIocpLocker.Create(pvName: String = '');
begin
  inherited Create;
  {$IFDEF USECriticalSection}

  {$ELSE}
    InitializeCriticalSection(FSection);
  {$ENDIF}
  Name := pvName;
end;

destructor TIocpLocker.Destroy;
begin
  {$IFDEF USECriticalSection}

  {$ELSE}
    DeleteCriticalSection(FSection);
  {$ENDIF}
  inherited Destroy;
end;

function TIocpLocker.getDebugINfo: String;
begin
  Result := Format('%s: busycount:%d, try:%s, enter:%s', [self.FName, GetEnterCount, FTryEnterInfo, FEnterInfo]);
end;

function TIocpLocker.GetEnterCount: Integer;
begin
  {$IFDEF USECriticalSection}
     Result := FSection.RecursionCount;
  {$ELSE}
     Result := FSection.RecursionCount;
  {$ENDIF}
end;

procedure TIocpLocker.lock(const pvDebugInfo: String = '');
begin
// unsafe
//  FTryEnterInfo := pvDebugInfo;
  {$IFDEF USECriticalSection}
     Enter;
  {$ELSE}
     EnterCriticalSection(FSection);
  {$ENDIF}
  FEnterINfo := pvDebugInfo;
end;

procedure TIocpLocker.unLock;
begin
  {$IFDEF USECriticalSection}
     Leave;
  {$ELSE}
     LeaveCriticalSection(FSection);
  {$ENDIF}
end;

end.
