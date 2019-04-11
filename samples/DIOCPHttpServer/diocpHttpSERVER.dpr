program diocpHttpSERVER;

uses
  FastMM4,
  FastMM4Messages,
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uFMMonitor in '..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\Common\Frames\uRunTimeINfoTools.pas';

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
