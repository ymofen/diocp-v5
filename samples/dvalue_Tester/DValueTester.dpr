program DValueTester;

uses
  DateSetting,
  Forms,
  ufrmMain in 'ufrmMain.pas' {Form1},
  utils_textfile in 'utils_textfile.pas';

{$R *.res}

begin
  Application.UpdateFormatSettings := False;
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := true;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
