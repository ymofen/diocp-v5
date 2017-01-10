unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, diocp_ex_coder_tcpclient,
  utils_safeLogger,
  diocp_task, diocp_sockets, diocp_tcp_client, diocp_ex_StreamCoder;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendObject: TButton;
    mmoData: TMemo;
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
  private
    { Private declarations }
    FDiocpContext: TIocpCoderRemoteContext;

    FCoderTcpClient: TDiocpCoderTcpClient;

    procedure OnRecvObject(const pvObject: Pointer);

    procedure OnDisconnected(pvContext: TDiocpCustomContext);
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation


{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  sfLogger.setAppender(TStringsAppender.Create(mmoRecvMessage.Lines));
  sfLogger.AppendInMainThread := true;
  FCoderTcpClient := TDiocpCoderTcpClient.Create(Self);
  FDiocpContext :=TIocpCoderRemoteContext(FCoderTcpClient.Add);

  FDiocpContext.RegisterCoderExchangeClass(TDiocpStreamCoderExchange);
  FDiocpContext.RegisterCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  FDiocpContext.OnContextAction := OnRecvObject;
  FCoderTcpClient.OnContextDisconnected := OnDisconnected;


end;

destructor TfrmMain.Destroy;
begin
  sfLogger.Enable := false;
  FCoderTcpClient.DisconnectAll;
  FCoderTcpClient.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  FCoderTcpClient.open;

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
  lvStream:TMemoryStream;
  s:AnsiString;
begin
  lvStream := TMemoryStream.Create;
  try
   // lvStream.LoadFromFile('C:\1.txt');
    s := mmoData.Lines.Text;
    lvStream.Write(s[1], Length(s));

    lvStream.Position := 0;

    //send stream object
    FDiocpContext.writeObject(lvStream);
  finally
    lvStream.Free;
  end;

end;

procedure TfrmMain.OnDisconnected(pvContext: TDiocpCustomContext);
begin
  if csDestroying in ComponentState then
  begin
    exit;
  end;

  sfLogger.logMessage('disconnected');
end;

procedure TfrmMain.OnRecvObject(const pvObject: Pointer);
var
  s:AnsiString;
  lvStream:TMemoryStream;
begin
  lvStream := TMemoryStream(pvObject);
  SetLength(s, lvStream.Size);
  lvStream.Position := 0;
  lvStream.Read(s[1], lvStream.Size);

  sfLogger.logMessage('recv msg from server:' + sLineBreak + '    ' + s);
  //sfLogger.logMessage('');
 end;

end.
