program wsclientTester;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {Form1},
  diocp_ex_websocketclient in '..\..\..\source\diocp_ex_websocketclient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
