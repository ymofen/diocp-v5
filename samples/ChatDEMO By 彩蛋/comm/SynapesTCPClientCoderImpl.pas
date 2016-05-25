(*
   unit owner: Sehe  105957021,
       d10.ymofen –ﬁ∏¥Ω” ’bug

+ 2015-03-10 12:59:22
*)
unit SynapesTCPClientCoderImpl;

interface

uses
  uICoderSocket, blcksock, synsock;

type
  TSynapesTCPClientCoderImpl = class(TInterfacedObject, ICoderSocket)
  private
    FReconnect: Boolean;
    FTcpClient: TTCPBlockSocket;
    FHost: String;
    FPort: String;
  protected
    function sendBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;
    function recvBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;    
    procedure closeSocket; stdcall;
  public
    constructor Create(ATcpClient: TTCPBlockSocket; pvReconnect: Boolean = true; Host: string = '127.0.0.1'; Port: String = '211');
    destructor Destroy; override;
    //property Host: String read FHost write FHost;
    //property Port: String read FPort write FPort;
  end;

implementation

constructor TSynapesTCPClientCoderImpl.Create(ATcpClient: TTCPBlockSocket;
    pvReconnect: Boolean = true; Host: string = '127.0.0.1'; Port: String = '211');
begin
  inherited Create;
  FTcpClient := ATcpClient;
  FReconnect := pvReconnect;
  FHost := Host;
  FPort := Port;
end;

destructor TSynapesTCPClientCoderImpl.Destroy;
begin
  inherited Destroy;
end;

{ TSynapesTCPClientCoderImpl }

procedure TSynapesTCPClientCoderImpl.closeSocket;
begin
  FTcpClient.CloseSocket;
end;

function TSynapesTCPClientCoderImpl.recvBuf(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if (FTcpClient.Socket = INVALID_SOCKET) then FTcpClient.connect(FHost, FPort);
    try
      // modify by d10
      // 2015-03-10 12:59:00
      Result := FTcpClient.RecvBuffer(buf, len);
      // Result := len;
    except
      FTcpClient.CloseSocket;
      raise;
    end;
  end else
  begin
     Result := FTcpClient.RecvBuffer(buf, len);

  end;

end;

function TSynapesTCPClientCoderImpl.sendBuf(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if FTcpClient.Socket = INVALID_SOCKET then FTcpClient.connect(FHost, FPort);
    try
      Result := FTcpClient.sendBuffer(buf, len);
    except
      FTcpClient.CloseSocket;
      raise;
    end;
  end else
  begin
    Result := FTcpClient.sendBuffer(buf, len);
  end;
end;

end.
