unit CHATHandler;

interface

uses
  SimpleMsgPack, diocp.session, diocp_tcp_server;

type
  TChatSession = class(TSessionItem)
  private
    FContext: TIocpClientContext;
    FOwnerTcpServer: TDiocpTcpServer;
    FData: TSimpleMsgPack;
    FState: Integer;
    FUserID: String;
    FVerified: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///   对应连接
    /// </summary>
    property Context: TIocpClientContext read FContext write FContext;
    property Data: TSimpleMsgPack read FData;
    /// <summary>
    ///   状态 (0,离线, 1:在线, 2:隐身)
    /// </summary>
    property State: Integer read FState write FState;
    property UserID: String read FUserID write FUserID;
    /// <summary>
    ///   验证状态
    /// </summary>
    property Verified: Boolean read FVerified write FVerified;
    property OwnerTcpServer: TDiocpTcpServer read FOwnerTcpServer write FOwnerTcpServer;
  end;

/// <summary>procedure CHATExecute
/// </summary>
/// <param name="pvCMDObject"> (TSimpleMsgPack) </param>
var
  ChatSessions: TSessions;

implementation

uses
  utils_safeLogger, SysUtils, ComObj,diocp_coder_tcpServer,
  Classes;

constructor TChatSession.Create;
begin
  inherited;
  FData := TSimpleMsgPack.Create;
  FOwnerTcpServer := nil;
end;


destructor TChatSession.Destroy;
begin
   FData.Free;
   inherited;
end;

initialization
  ChatSessions := TSessions.Create(TChatSession);

finalization
  ChatSessions.Free;

end.
