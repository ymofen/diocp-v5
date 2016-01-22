program DiocpStreamServer;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  DiocpStreamClientContext in 'DiocpStreamClientContext.pas',
  uFMMonitor in '..\..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\..\Common\Frames\uRunTimeINfoTools.pas',
  DiocpStreamProtocol in 'DiocpStreamProtocol.pas';

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
