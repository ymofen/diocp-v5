unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, diocp_ex_http_common, diocp_ex_websocketclient,
  utils_websocket, utils_safeLogger, utils_dtimewheel;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    pnlClient: TPanel;
    mmoRecv: TMemo;
    pnlSend: TPanel;
    mmoSend: TMemo;
    pnlSendRight: TPanel;
    btnConnect: TButton;
    edtWsUrl: TEdit;
    btnSend: TButton;
    btnDisconnect: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
    FWsClient:TDiocpWebSocketContext;

    procedure OnShakeHand(Sender:TObject);
    procedure OnDisconnected(Sender:TObject);
    procedure OnRecv(Sender:TObject);

    procedure WsSendPing(pvTimeWheel:TDTimeWheel; pvUserData:Pointer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

uses
  utils_strings;

{$R *.dfm}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DoFinalizeWebSocketClient;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoInitializeWebSocketClient;
  sfLogger.setAppender(TStringsAppender.Create(mmoRecv.Lines));
  sfLogger.AppendInMainThread := True;
  FWsClient := NewWsClient();
  FWsClient.Masked := true;
  FWsClient.HttpOrigin := '';

  FWsClient.OnRecv := OnRecv;
  FWsClient.OnDisconnectedEvent := OnDisconnected;
  FWsClient.OnShakeHand := OnShakeHand;
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  //FWsClient.HeaderBuilder.SetHeader('subscribeid', '1007');
  FWsClient.Open(edtWsUrl.Text);
end;

procedure TForm1.btnDisconnectClick(Sender: TObject);
begin
  FWsClient.Close();
end;

procedure TForm1.btnSendClick(Sender: TObject);
var
  s:String;
  lvBytes:TBytes;
begin
  s := mmoSend.Text;
  lvBytes := StringToUtf8Bytes(s);

  FWsClient.SendBuffer(@lvBytes[0], Length(lvBytes), OPT_TEXT);
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  InitialDtw(1000);
  Dtw.AddTask(5000, WsSendPing, nil, 0, nil)
end;

procedure TForm1.OnDisconnected(Sender: TObject);
begin
  sfLogger.logMessage('on disconnected');   
end;

procedure TForm1.OnRecv(Sender:TObject);
begin
  mmoRecv.Lines.Add(FWsClient.WebSocketContentBuffer.DecodeUTF8);
end;

procedure TForm1.OnShakeHand(Sender:TObject);
begin
  mmoRecv.Lines.Add(FWsClient.HttpBuffer.HeaderBuilder.ToRAWString);
end;

procedure TForm1.WsSendPing(pvTimeWheel:TDTimeWheel; pvUserData:Pointer);
begin
  if FWsClient.Active then
    FWsClient.CheckSendPing(5000);

end;

end.
