program diocp_utils_demo;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {Form1},
  utils_router in '..\..\source\utils_router.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
