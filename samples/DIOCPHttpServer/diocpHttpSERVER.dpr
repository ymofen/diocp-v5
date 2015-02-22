program diocpHttpSERVER;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uFMMonitor in '..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\Common\Frames\uRunTimeINfoTools.pas',
  superobject in 'superobject.pas';

{$R *.res}

begin
  {$IF CompilerVersion> 18}
  ReportMemoryLeaksOnShutdown := DebugHook = 1;
  {$IFEND}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
