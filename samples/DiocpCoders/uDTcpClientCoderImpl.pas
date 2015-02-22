(*
   unit owner: d10.ymofen

*)
unit uDTcpClientCoderImpl;



interface

uses
  uICoderSocket, DTcpClient;

type
  TDTcpClientCoderImpl = class(TInterfacedObject, ICoderSocket)
  private
    FReconnect: Boolean;
    FTcpClient: TDTcpClient;
  protected
    function sendBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;
    function recvBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;    
    procedure closeSocket; stdcall;
  public
    constructor Create(ATcpClient: TDTcpClient; pvReconnect: Boolean = true);
    destructor Destroy; override;
  end;

implementation

constructor TDTcpClientCoderImpl.Create(ATcpClient: TDTcpClient; pvReconnect:
    Boolean = true);
begin
  inherited Create;
  FTcpClient := ATcpClient;
  FReconnect := pvReconnect;
end;

destructor TDTcpClientCoderImpl.Destroy;
begin
  inherited Destroy;
end;

{ TDTcpClientCoderImpl }

procedure TDTcpClientCoderImpl.closeSocket;
begin
  FTcpClient.Disconnect;
end;

function TDTcpClientCoderImpl.recvBuf(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if not FTcpClient.Active then FTcpClient.connect;
    try
      FTcpClient.recv(buf, len);
      Result := len;
    except
      FTcpClient.Disconnect;
      raise;
    end;
  end else
  begin
     FTcpClient.recv(buf, len);
     Result := len;
  end;

end;

function TDTcpClientCoderImpl.sendBuf(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if not FTcpClient.Active then FTcpClient.connect;
    try
      Result := FTcpClient.sendBuffer(buf, len);
    except
      FTcpClient.Disconnect;
      raise;
    end;
  end else
  begin
    Result := FTcpClient.sendBuffer(buf, len);
  end;
end;

end.
