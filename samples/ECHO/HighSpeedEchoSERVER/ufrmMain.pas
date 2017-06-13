unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, diocp_tcp_server, StdCtrls, ActnList, ExtCtrls;

type
  TfrmMain = class(TForm)
    btnAction: TButton;
    actlstMain: TActionList;
    actStart: TAction;
    actStop: TAction;
    edtPort: TEdit;
    tmrInfo: TTimer;
    edtWorkCount: TEdit;
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure tmrInfoTimer(Sender: TObject);
  private
    { Private declarations }
    FTcpSvr: TDiocpTcpServer;

    procedure OnRecv(pvClientContext:TIocpClientContext; buf:Pointer; len:cardinal;
        errCode:Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpSvr := TDiocpTcpServer.Create(nil);
end;

destructor TfrmMain.Destroy;
begin
  FTcpSvr.SafeStop;
  FreeAndNil(FTcpSvr);
  inherited Destroy;
end;

procedure TfrmMain.actStartExecute(Sender: TObject);
begin

  FTcpSvr.Port := StrToInt(edtPort.Text);
  FTcpSvr.OnDataReceived := OnRecv;
  FTcpSvr.UseObjectPool := False;
  FTcpSvr.WorkerCount := StrToInt(edtWorkCount.Text);
  FTcpSvr.Open;
  btnAction.Action := actStop;
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpSvr.SafeStop;
  btnAction.Action := actStart;
end;

procedure TfrmMain.OnRecv(pvClientContext:TIocpClientContext; buf:Pointer;
    len:cardinal; errCode:Integer);
begin
  pvClientContext.PostWSASendRequest(buf, len);
end;

procedure TfrmMain.tmrInfoTimer(Sender: TObject);
begin
  self.Caption := Format('DiocpEchoSvr(%d):%d', [FTcpSvr.Port, FTcpSvr.ClientCount]);
end;

end.
