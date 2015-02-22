(*
   unit owner: d10.ymofen

   + add reconnect param in constructor
     2014-10-11 22:18:18
*)
unit uRawTcpClientCoderImpl;

interface

uses
  uICoderSocket, RawTcpClient;

type
  TRawTcpClientCoderImpl = class(TInterfacedObject, ICoderSocket)
  private
    FReconnect: Boolean;
    FTcpClient: TRawTcpClient;
  protected
    function sendBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;
    function recvBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;    
    procedure closeSocket; stdcall;
  public
    constructor Create(ATcpClient: TRawTcpClient; pvReconnect: Boolean = true);
    destructor Destroy; override;
  end;

implementation

constructor TRawTcpClientCoderImpl.Create(ATcpClient: TRawTcpClient;
    pvReconnect: Boolean = true);
begin
  inherited Create;
  FTcpClient := ATcpClient;
  FReconnect := pvReconnect;
end;

destructor TRawTcpClientCoderImpl.Destroy;
begin
  inherited Destroy;
end;

{ TRawTcpClientCoderImpl }

procedure TRawTcpClientCoderImpl.closeSocket;
begin
  FTcpClient.Disconnect;
end;

function TRawTcpClientCoderImpl.recvBuf(buf: Pointer; len: Cardinal): Cardinal;
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

function TRawTcpClientCoderImpl.sendBuf(buf: Pointer; len: Cardinal): Cardinal;
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
