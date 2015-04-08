program CHATSERVER;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDIOCPStreamCoder in '..\..\diocpCoders\uDIOCPStreamCoder.pas',
  uMyClientContext in 'uMyClientContext.pas',
  uFMMonitor in '..\..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\..\Common\Frames\uRunTimeINfoTools.pas',
  diocp.session in 'diocp.session.pas',
  CHATHandler in 'CHATHandler.pas',
  SimpleMsgPack in '..\COMMON\SimpleMsgPack.pas';

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
