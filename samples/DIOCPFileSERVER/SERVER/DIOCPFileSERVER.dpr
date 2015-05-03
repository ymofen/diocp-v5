program DIOCPFileSERVER;

uses
  FastMM4,
  FastMM4Messages,
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uMyClientContext in 'Service\uMyClientContext.pas',
  SimpleMsgPack in '..\Common\SimpleMsgPack.pas',
  uFileOperaHandler in 'Service\uFileOperaHandler.pas',
  uFMMonitor in '..\..\Common\Frames\uFMMonitor.pas' {FMMonitor: TFrame},
  uRunTimeINfoTools in '..\..\Common\Frames\uRunTimeINfoTools.pas',
  uDIOCPStreamCoder in '..\..\DiocpCoders\uDIOCPStreamCoder.pas',
  uCRCTools in '..\..\DiocpCoders\uCRCTools.pas',
  uZipTools in '..\..\DiocpCoders\uZipTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
