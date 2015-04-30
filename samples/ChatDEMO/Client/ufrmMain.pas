unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, diocp.coder.tcpClient,
  utils.safeLogger,
  uDIOCPDxStreamCoder, diocp.task, diocp.sockets, diocp.tcp.client, ExtCtrls,
  SimpleMsgPack;

type
  TfrmMain = class(TForm)
    mmoRecvMessage: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    pnlOperator: TPanel;
    pnlSendArea: TPanel;
    btnSendObject: TButton;
    mmoData: TMemo;
    btnLogin: TButton;
    cbbName: TComboBox;
    lstUsers: TListBox;
    tmrHeart: TTimer;
    procedure btnConnectClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
    procedure tmrHeartTimer(Sender: TObject);
  private
    FUserID:String;

    FCMDObject: TSimpleMsgPack;
    FCMDStream: TMemoryStream;

    FDiocpContext: TIocpCoderRemoteContext;

    FCoderTcpClient: TDiocpCoderTcpClient;

    procedure OnRecvObject(pvObject:TObject);

    procedure OnDisconnected(pvContext: TDiocpCustomContext);

    procedure DoHeart;

    procedure SendCMDObject(pvCMDObject: TSimpleMsgPack);

    procedure StartHeart();
  public

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

  FDiocpContext.RegisterCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  FDiocpContext.OnContextAction := OnRecvObject;
  FCoderTcpClient.OnContextDisconnected := OnDisconnected;


  FCMDObject := TSimpleMsgPack.Create();
  FCMDStream := TMemoryStream.Create;
end;

destructor TfrmMain.Destroy;
begin
  FreeAndNil(FCMDObject);
  sfLogger.Enable := false;
  FCoderTcpClient.DisconnectAll;
  FCoderTcpClient.Free;
  FCMDStream.Free;
  inherited Destroy;
end;

procedure TfrmMain.DoHeart;
begin
  FCMDObject.Clear;
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 0;
  SendCMDObject(FCMDObject);
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  FCoderTcpClient.open;

  if FDiocpContext.Active then
  begin
    sfLogger.logMessage('已经连接到服务器');
    Exit;
  end;
  FDiocpContext.Host := edtHost.Text;
  FDiocpContext.Port := StrToInt(edtPort.Text);
  FDiocpContext.Connect;

  sfLogger.logMessage('与服务器建立连接成功, 请进行登陆');
  
end;

procedure TfrmMain.btnLoginClick(Sender: TObject);
begin
  FCMDObject.Clear;
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 11;
  FCMDObject.ForcePathObject('requestID').AsString := 'login';
  FCMDObject.ForcePathObject('params.userid').AsString := cbbName.Text;
  SendCMDObject(FCMDObject);


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

procedure TfrmMain.OnRecvObject(pvObject: TObject);
var
  s:AnsiString;
  lvStream:TMemoryStream;
  lvCMDObject:TSimpleMsgPack;
begin
  lvStream := TMemoryStream(pvObject);
  lvCMDObject:= TSimpleMsgPack.Create;
  lvCMDObject.DecodeFromStream(lvStream);
  if lvCMDObject.ForcePathObject('requestID').AsString = 'login' then
  begin
    if lvCMDObject.ForcePathObject('result.code').AsInteger = -1 then
    begin
      sfLogger.logMessage(lvCMDObject.ForcePathObject('result.msg').AsString);
    end else
    begin
      sfLogger.logMessage('登陆成功...');
      iocpTaskManager.PostATask(StartHeart, true);
    end;                                   
  end;
 end;

procedure TfrmMain.SendCMDObject(pvCMDObject: TSimpleMsgPack);
var
  lvCMDStream:TMemoryStream;
begin
  lvCMDStream := TMemoryStream.Create;
  try
    pvCMDObject.EncodeToStream(lvCMDStream);
    FDiocpContext.WriteObject(lvCMDStream);
  finally
    lvCMDStream.Free;
  end;
end;

procedure TfrmMain.StartHeart;
begin
  tmrHeart.Enabled := true;
end;

procedure TfrmMain.tmrHeartTimer(Sender: TObject);
begin
  DoHeart;
end;

end.
