unit frm_Server;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus,
  diocp_coder_tcpServer;

type
  TForm5 = class(TForm)
    redtLog: TRichEdit;
    mmain: TMainMenu;
    mniN1: TMenuItem;
    mniStart: TMenuItem;
    mniN3: TMenuItem;
    mniN4: TMenuItem;
    mniN5: TMenuItem;
    mniStop: TMenuItem;
    tmrKeepAlive: TTimer;
    procedure mniStartClick(Sender: TObject);
    procedure mniN3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mniStopClick(Sender: TObject);
    procedure tmrKeepAliveTimer(Sender: TObject);
  private
    { Private declarations }
    FTcpServer: TDiocpCoderTcpServer;
    procedure RefreshState;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  uDIOCPStreamCoder, uMyClientContext, CHATHandler, utils_safeLogger;

{$R *.dfm}

procedure TForm5.FormCreate(Sender: TObject);
begin
  FTcpServer := TDiocpCoderTcpServer.Create(Self);
  FTcpServer.CreateDataMonitor;
  FTcpServer.WorkerCount := 3;
  // register decoder and encoder class
  FTcpServer.RegisterCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  FTcpServer.RegisterContextClass(TMyClientContext);

  sfLogger.setAppender(TStringsAppender.Create(redtLog.Lines));
  sfLogger.AppendInMainThread := true;
end;

procedure TForm5.mniN3Click(Sender: TObject);
begin
  FTcpServer.DisconnectAll;
end;

procedure TForm5.mniStartClick(Sender: TObject);
begin
  FTcpServer.Port := 60544;
  FTcpServer.Active := true;
  RefreshState;
end;

procedure TForm5.mniStopClick(Sender: TObject);
begin
  FTcpServer.SafeStop;
  RefreshState;
end;

procedure TForm5.RefreshState;
begin
  mniStart.Enabled := not FTcpServer.Active;
  mniStop.Enabled := FTcpServer.Active;
end;

procedure TForm5.tmrKeepAliveTimer(Sender: TObject);
begin
  FTcpServer.KickOut(20000);
  ChatSessions.KickOut(20000);
end;

end.
