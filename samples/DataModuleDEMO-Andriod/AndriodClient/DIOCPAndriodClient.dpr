program DIOCPAndriodClient;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDTcpClientCoderImpl in '..\..\diocpCoders\uDTcpClientCoderImpl.pas',
  uICoderSocket in '..\..\diocpCoders\uICoderSocket.pas',
  uRemoteServerDIOCPImpl in 'service\uRemoteServerDIOCPImpl.pas',
  uIRemoteServer in 'interface\uIRemoteServer.pas',
  SimpleMsgPack in '..\Common\SimpleMsgPack.pas',
  uStreamCoderSocket in '..\..\diocpCoders\uStreamCoderSocket.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
