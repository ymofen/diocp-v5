program RawTcpClientDemo;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uICoderSocket in '..\..\diocpCoders\uICoderSocket.pas',
  uStreamCoderSocket in '..\..\diocpCoders\uStreamCoderSocket.pas',
  uRawTcpClientCoderImpl in '..\..\diocpCoders\uRawTcpClientCoderImpl.pas',
  uZipTools in '..\..\diocpCoders\uZipTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
