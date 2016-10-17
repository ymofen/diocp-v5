program prjDiocpYunLogger;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {Form1},
  utils_yunlogger in '..\..\source\utils_yunlogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
