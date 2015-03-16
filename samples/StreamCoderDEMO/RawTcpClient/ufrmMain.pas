unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, diocp.task,
  uStreamCoderSocket, uRawTcpClientCoderImpl, utils.safeLogger,
  diocp.tcp.blockClient;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnSendObject: TButton;
    btnReConnect: TButton;
    mmoData: TMemo;
    procedure btnReConnectClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
  private
    { Private declarations }
    FTcpClient:TDiocpBlockTcpClient;


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
  FTcpClient := TDiocpBlockTcpClient.Create(Self);

end;

destructor TfrmMain.Destroy;
begin
  FTcpClient.Disconnect;
  
  inherited Destroy;
end;

procedure TfrmMain.btnReConnectClick(Sender: TObject);
begin
  FTcpClient.Disconnect;
  btnConnect.Click;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  if FTcpClient.Active then
  begin
    sfLogger.logMessage('already connected...');
    Exit;
  end;
  FTcpClient.Host := edtHost.Text;
  FTcpClient.Port := StrToInt(edtPort.Text);
  FTcpClient.ConnectTimeOut(10000);

  mmoRecvMessage.Clear;   
  mmoRecvMessage.Lines.Add('connected...');
end;

procedure TfrmMain.btnSendObjectClick(Sender: TObject);
var
  i: Integer;
  lvStream:TMemoryStream;
  s:AnsiString;
begin
  lvStream := TMemoryStream.Create;
  try
    //lvStream.LoadFromFile('C:\1.txt');
    s := mmoData.Lines.Text;
    lvStream.Write(s[1], Length(s));

    lvStream.Position := 0;

    TStreamCoderSocket.SendObject(TRawTcpClientCoderImpl.Create(FTcpClient), lvStream);

    lvStream.Clear;

    // recv
    TStreamCoderSocket.RecvObject(TRawTcpClientCoderImpl.Create(FTcpClient), lvStream);

    SetLength(s, lvStream.Size);
    lvStream.Position := 0;
    lvStream.Read(s[1], lvStream.Size);

    sfLogger.logMessage('recv msg from server:' + sLineBreak + '    ' + s);
  finally
    lvStream.Free;
  end;

end;

end.
