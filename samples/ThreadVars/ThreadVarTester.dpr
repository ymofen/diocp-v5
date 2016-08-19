program ThreadVarTester;

uses
  //ScaleMM2,
  QMM,
  Forms,
  ufrmMain in 'ufrmMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := true;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
