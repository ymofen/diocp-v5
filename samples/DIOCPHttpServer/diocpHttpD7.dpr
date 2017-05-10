program diocpHttpD7;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uFMMonitor in '..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\Common\Frames\uRunTimeINfoTools.pas',
  utils_websocket in '..\..\source\utils_websocket.pas';

{$R *.res}

begin
  {$IF CompilerVersion> 18}
  ReportMemoryLeaksOnShutdown := true;
  {$IFEND}
  Application.Initialize;
  {$IF CompilerVersion> 18}
  Application.MainFormOnTaskbar := True;
  {$IFEND}

  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
