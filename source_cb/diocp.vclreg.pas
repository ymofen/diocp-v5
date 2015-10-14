unit diocp.vclreg;

interface

uses
  diocp.tcp.server, diocp.tcp.client, diocp.tcp.blockClient,
  diocp.coder.tcpServer, diocp.coder.tcpClient,
  Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DIOCP', [TDiocpTcpServer, TDiocpCoderTcpServer
                              , TDiocpTcpClient, TDiocpCoderTcpClient
                              , TDiocpBlockTcpClient]);
end;

end.
