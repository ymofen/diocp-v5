program DiocpJSONServer;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  diocp_ex_strObjectCoder in '..\..\DiocpCoders\diocp_ex_strObjectCoder.pas',
  uMyClientContext in 'uMyClientContext.pas',
  uZipTools in '..\..\diocpCoders\uZipTools.pas',
  uFMMonitor in '..\..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\..\Common\Frames\uRunTimeINfoTools.pas';

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
