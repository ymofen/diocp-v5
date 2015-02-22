program diocpTcpClientDemo;



uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uFMMonitor in 'Frames\uFMMonitor.pas',
  uRunTimeINfoTools in 'Frames\uRunTimeINfoTools.pas';

{$R *.res}

begin
  {$IF CompilerVersion> 18}
  ReportMemoryLeaksOnShutdown := true;
  {$IFEND}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
