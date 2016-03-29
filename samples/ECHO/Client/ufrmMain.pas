unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, diocp.tcp.client,
  utils.safeLogger, ComCtrls, diocp.sockets, ExtCtrls;

type
  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    tsMonitor: TTabSheet;
    mmoRecvMessage: TMemo;
    tsOperator: TTabSheet;
    mmoData: TMemo;
    btnFill1K: TButton;
    btnSendObject: TButton;
    pnlTop: TPanel;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnClose: TButton;
    btnCreate: TButton;
    edtCount: TEdit;
    chkSendData: TCheckBox;
    chkRecvEcho: TCheckBox;
    chkRecvOnLog: TCheckBox;
    btnClear: TButton;
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnFill1KClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
    procedure chkRecvEchoClick(Sender: TObject);
    procedure chkRecvOnLogClick(Sender: TObject);
    procedure chkSendDataClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSendDataOnConnected:Boolean;
    FSendDataOnRecv:Boolean;
    FRecvOnLog:Boolean;
    { Private declarations }
    FIocpClientSocket: TDiocpTcpClient;

    procedure OnContextConnected(pvContext: TDiocpCustomContext);

    procedure OnRecvdBuffer(pvContext: TDiocpCustomContext; buf: Pointer; len:
        cardinal; pvErrorCode: Integer);

    procedure WriteHistory;

    procedure ReadHistory;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, utils_dvalue, utils_DValue_JSON;
{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FSendDataOnRecv := chkRecvEcho.Checked;
  FRecvOnLog := chkRecvOnLog.Checked;
  sfLogger.setAppender(TStringsAppender.Create(mmoRecvMessage.Lines));
  sfLogger.AppendInMainThread := true;

  FIocpClientSocket := TDiocpTcpClient.Create(Self);
  FIocpClientSocket.createDataMonitor;
  FIocpClientSocket.OnContextConnected := OnContextConnected;
  FIocpClientSocket.OnReceivedBuffer := OnRecvdBuffer;
  TFMMonitor.createAsChild(tsMonitor, FIocpClientSocket);

  ReadHistory;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSendDataOnConnected := true;
end;

destructor TfrmMain.Destroy;
begin
  FIocpClientSocket.Close;
  FIocpClientSocket.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  mmoRecvMessage.Clear;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FIocpClientSocket.Count-1 do
  begin
    FIocpClientSocket.Items[i].AutoReConnect := false;
    FIocpClientSocket.Items[i].Close; 
  end;
  FIocpClientSocket.WaitForContext(30000);
  FIocpClientSocket.ClearContexts;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
var
  lvClient:TIocpRemoteContext;
begin
  FIocpClientSocket.open;

  lvClient := FIocpClientSocket.Add;
  lvClient.Host := edtHost.Text;
  lvClient.Port := StrToInt(edtPort.Text);
  lvClient.AutoReConnect := true;
  lvClient.ConnectASync;



  mmoRecvMessage.Clear;

  mmoRecvMessage.Lines.Add('start to recv...');
end;

procedure TfrmMain.btnCreateClick(Sender: TObject);

var
  lvClient:TIocpRemoteContext;
  i:Integer;

begin
  FIocpClientSocket.open;

  for i := 1 to StrToInt(edtCount.Text) do
  begin
    lvClient := FIocpClientSocket.Add;
    lvClient.Host := edtHost.Text;
    lvClient.Port := StrToInt(edtPort.Text);
    lvClient.AutoReConnect := true;
    lvClient.connectASync;
  end;

end;

procedure TfrmMain.btnFill1KClick(Sender: TObject);
var
  s:AnsiString;
begin
  SetLength(s, 1024);
  FillChar(PAnsiChar(s)^, 1024, 'a');
  mmoData.Lines.Text :=  s;
end;

procedure TfrmMain.btnSendObjectClick(Sender: TObject);
var
  s:AnsiString;
  i: Integer;
begin  
  s := mmoData.Lines.Text;

  for i := 0 to FIocpClientSocket.Count - 1 do
  begin
    FIocpClientSocket.Items[i].PostWSASendRequest(PAnsiChar(s), Length(s));
  end;
end;

procedure TfrmMain.chkRecvEchoClick(Sender: TObject);
begin
  FSendDataOnRecv := chkRecvEcho.Checked;
end;

procedure TfrmMain.chkRecvOnLogClick(Sender: TObject);
begin
  FRecvOnLog := chkRecvOnLog.Checked;
end;

procedure TfrmMain.chkSendDataClick(Sender: TObject);
begin
  FSendDataOnConnected := chkSendData.Checked;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteHistory;
end;

procedure TfrmMain.OnContextConnected(pvContext: TDiocpCustomContext);
var
  s:AnsiString;
begin
  if FSendDataOnConnected then
  begin
    s := mmoData.Lines.Text;

    pvContext.PostWSASendRequest(PAnsiChar(s), Length(s));
  end;

end;

procedure TfrmMain.OnRecvdBuffer(pvContext: TDiocpCustomContext; buf: Pointer;
    len: cardinal; pvErrorCode: Integer);
var
  lvStr:AnsiString;
begin
  if len = 0 then
  begin
    sfLogger.logMessage('recv err zero');
  end;
  if pvErrorCode = 0 then
  begin
    if FSendDataOnRecv then
    begin
      Sleep(0);
      pvContext.PostWSASendRequest(buf, len);
    end;
    if FRecvOnLog then
    begin
      SetLength(lvStr, len);
      Move(buf^, PAnsiChar(lvStr)^, len);
      sfLogger.logMessage(lvStr);
    end;
  end else
  begin
    sfLogger.logMessage('recv err:%d', [pvErrorCode]);
  end;
end;

procedure TfrmMain.ReadHistory;
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create();
  JSONParseFromUtf8NoBOMFile(ChangeFileExt(ParamStr(0), '.history.json'), lvDVAlue);
  edtHost.Text := lvDValue.ForceByName('host').AsString;
  edtPort.Text := lvDValue.ForceByName('port').AsString;
  mmoData.Lines.Text := lvDValue.ForceByName('sendText').AsString;
  chkRecvEcho.Checked := lvDValue.ForceByName('chk_recvecho').AsBoolean;
  chkRecvOnLog.Checked := lvDValue.ForceByName('chk_recvonlog').AsBoolean;
  chkSendData.Checked := lvDValue.ForceByName('chk_send_onconnected').AsBoolean;

  lvDValue.Free;  
end;

procedure TfrmMain.WriteHistory;
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create();
  lvDValue.ForceByName('host').AsString := edtHost.Text;
  lvDValue.ForceByName('port').AsString := edtPort.Text;
  lvDValue.ForceByName('sendText').AsString := mmoData.Lines.Text;
  lvDValue.ForceByName('chk_recvecho').AsBoolean := chkRecvEcho.Checked;
  lvDValue.ForceByName('chk_recvonlog').AsBoolean := chkRecvOnLog.Checked;
  lvDValue.ForceByName('chk_send_onconnected').AsBoolean := chkSendData.Checked;
  JSONWriteToUtf8NoBOMFile(ChangeFileExt(ParamStr(0), '.history.json'), lvDVAlue);
  lvDValue.Free;
end;

end.
