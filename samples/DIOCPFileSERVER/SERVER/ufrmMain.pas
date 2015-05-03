unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, diocp.coder.tcpServer, uDIOCPStreamCoder, ExtCtrls,
  ComObj, ComCtrls, uMyClientContext;

type
  TfrmMain = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    actlstMain: TActionList;
    actOpen: TAction;
    actStop: TAction;
    actPushMsg: TAction;
    pgcMain: TPageControl;
    tsMoniter: TTabSheet;
    pnlMonitor: TPanel;
    pnlTop: TPanel;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
  private
    { Private declarations }
    FTcpServer: TDiocpCoderTcpServer;
    procedure refreshState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, uRunTimeINfoTools;

{$R *.dfm}



constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TDiocpCoderTcpServer.Create(Self);
  //FTcpServer.KeepAlive := true;
  FTcpServer.createDataMonitor;
  // register decoder and encoder class
  FTcpServer.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);

  FTcpServer.registerContextClass(TMyClientContext);
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
  FTcpServer.LogicWorkerNeedCoInitialize := true;
end;

destructor TfrmMain.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmMain.refreshState;
begin
  if FTcpServer.Active then
  begin
    btnOpen.Action := actStop;
  end else
  begin
    btnOpen.Action := actOpen;
  end;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  FTcpServer.Port := StrToInt(edtPort.Text);
  FTcpServer.Active := true;
  refreshState;
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

end.
