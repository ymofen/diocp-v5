unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, diocp.coder.tcpClient,
  utils.safeLogger,
  diocp.task, diocp.sockets, diocp.tcp.client;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendObject: TButton;
    mmoData: TMemo;
    Button1: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FDiocpContext: TDiocpExRemoteContext;

    FDiocpTcpClient: TDiocpTcpClient;

    procedure OnDisconnected(pvContext: TDiocpCustomContext);
    procedure OnRecvBuffer(pvContext: TDiocpCustomContext; buf: Pointer; len:
        cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  utils.buffer;


{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
var
  s1, s2:AnsiString;
begin
  inherited;
  sfLogger.setAppender(TStringsAppender.Create(mmoRecvMessage.Lines));
  sfLogger.AppendInMainThread := true;
  TStringsAppender(sfLogger.Appender).MaxLines := 5000;
  FDiocpTcpClient := TDiocpTcpClient.Create(Self);
  FDiocpTcpClient.RegisterContextClass(TDiocpExRemoteContext);
  FDiocpContext :=TDiocpExRemoteContext(FDiocpTcpClient.Add);

  s1 := '#';
  s2 := '!';
  FDiocpContext.SetStart(PAnsiChar(s1), length(s1));
  FDiocpContext.SetEnd(PAnsiChar(s2), length(s2));

  FDiocpContext.OnBufferAction := OnRecvBuffer;


  FDiocpTcpClient.OnContextDisconnected := OnDisconnected;
end;

destructor TfrmMain.Destroy;
begin
  sfLogger.Enable := false;
  FDiocpTcpClient.DisconnectAll;
  FDiocpTcpClient.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  FDiocpTcpClient.open;

  if FDiocpContext.Active then
  begin
    sfLogger.logMessage('already connected...');
    Exit;
  end;
  FDiocpContext.Host := edtHost.Text;
  FDiocpContext.Port := StrToInt(edtPort.Text);
  FDiocpContext.Connect;

  mmoRecvMessage.Clear;

  mmoRecvMessage.Lines.Add('start to recv...');
end;

procedure TfrmMain.btnSendObjectClick(Sender: TObject);
var
  s:AnsiString;
begin
  s := mmoData.Lines.Text;
  FDiocpContext.PostWSASendRequest(PAnsiChar(s), Length(s), True);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  lvBuffer:TBufferLink;
  s, s1, s2:AnsiString;
  j:Integer;
begin
  lvBuffer:=TBufferLink.Create;
  s := mmoData.Lines.Text;
  lvBuffer.AddBuffer(PAnsiChar(s), Length(s));

  while lvBuffer.validCount > 0 do
  begin
    s1 := '#';
    j := lvBuffer.SearchBuffer(PAnsiChar(s1), length(s1));
    if j = -1 then
    begin
      sfLogger.logMessage('start is none...');
      break;
    end;
    lvBuffer.Skip(j + length(s1));

    s1 := '!';
    j := lvBuffer.SearchBuffer(PAnsiChar(s1), length(s1));
    if j = -1 then
    begin
      sfLogger.logMessage('end is none...');
      break;
    end;

    setLength(s2, j);
    lvBuffer.readBuffer(PAnsiChar(s2), j);
    sfLogger.logMessage(s2);

    lvBuffer.Skip(length(s1));
  end;

  lvBuffer.Free;

  



end;

procedure TfrmMain.OnDisconnected(pvContext: TDiocpCustomContext);
begin
  if csDestroying in ComponentState then
  begin
    exit;
  end;

  sfLogger.logMessage('disconnected');
end;

procedure TfrmMain.OnRecvBuffer(pvContext: TDiocpCustomContext; buf: Pointer;
    len: cardinal);
var
  s:AnsiString;
begin
  SetLength(s, len);
  Move(buf^, pansichar(s)^, len);
  sfLogger.logMessage(s);
end;

end.
