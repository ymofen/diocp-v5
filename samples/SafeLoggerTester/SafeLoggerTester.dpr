program SafeLoggerTester;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  SingleLogFileAppender4SafeLogger in 'SingleLogFileAppender4SafeLogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
