program BufferPoolTester;

uses
  //ScaleMM2,
  Forms,
  ufrmMain in 'ufrmMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := true;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
