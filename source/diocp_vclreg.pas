unit diocp_vclreg;

interface

uses
  diocp_tcp_server, diocp_tcp_client, diocp_tcp_blockClient, diocp_ex_httpClient, 
  diocp_coder_tcpServer, diocp_coder_tcpClient, diocp_ex_httpServer,
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
