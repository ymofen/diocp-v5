unit diocp.ex.SimpleMsgPackSession;

interface

uses
  diocp_ex_httpServer, SimpleMsgPack;


type
  TDiocpSimpleMsgPackSession = class(TDiocpHttpSession)
  private
    FMsgPack: TSimpleMsgPack;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MsgPack: TSimpleMsgPack read FMsgPack;


  end;

implementation

constructor TDiocpSimpleMsgPackSession.Create;
begin
  inherited Create;
  FMsgPack := TSimpleMsgPack.Create();
end;

destructor TDiocpSimpleMsgPackSession.Destroy;
begin
  FMsgPack.Free;
  inherited Destroy;
end;

end.
