program diocpHttpSERVER;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uFMMonitor in '..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\Common\Frames\uRunTimeINfoTools.pas',
  superobject in 'superobject.pas',
  diocp.ex.SimpleMsgPackSession in 'diocp.ex.SimpleMsgPackSession.pas',
  SimpleMsgPack in 'SimpleMsgPack.pas',
  utils_websocket in '..\..\source\utils_websocket.pas';

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
