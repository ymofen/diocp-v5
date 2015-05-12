unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, diocp.coder.tcpClient,
  utils.safeLogger,  uDIOCPDxStreamCoder, diocp.task, diocp.sockets,
   diocp.tcp.client, ExtCtrls,  SimpleMsgPack;

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
    Button2: TButton;
    btn1: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
    procedure tmrHeartTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn1Click(Sender: TObject);
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
    procedure UpdataUser;
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

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
FCoderTcpClient.DisconnectAll;
end;

procedure TfrmMain.btn1Click(Sender: TObject);
begin
 FCoderTcpClient.DisconnectAll;
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
begin
  FCMDObject.Clear;
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 5;
  FCMDObject.ForcePathObject('requestID').AsString := 'messageID';
  FCMDObject.ForcePathObject('params.msg').AsString := mmoData.Lines.Text;
  SendCMDObject(FCMDObject);
end;
// 请求包:
///   {
/// 	   "cmdIndex": 3,
/// 	   "requestID":"xxx-xx-xx-xx",  // 请求的ID, 不重复的字符串，回应时, 会带回去
/// 	   "params":
/// 	    {
/// 		    "page":1,               // 查询页码(显示第几页数据)
/// 		  }
/// 	}
///
procedure TfrmMain.Button1Click(Sender: TObject);
begin
  FCMDObject.Clear;
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 3;
  FCMDObject.ForcePathObject('requestID').AsString := cbbName.Text;
  FCMDObject.ForcePathObject('params.page').AsString :='1';
  SendCMDObject(FCMDObject);
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 5;
 // FCMDObject.ForcePathObject('userid').AsString := cbbName.Text;
  FCMDObject.ForcePathObject('params.userid').AsString := lstUsers.Items[lstUsers.ItemIndex];
  FCMDObject.ForcePathObject('params.msg').AsString := mmoData.Lines.Text;
  SendCMDObject(FCMDObject);
end;

procedure TfrmMain.OnDisconnected(pvContext: TDiocpCustomContext);
begin
  if csDestroying in ComponentState then
  begin
    exit;
  end;

  sfLogger.logMessage('与服务器断开连接...');
end;

procedure TfrmMain.OnRecvObject(pvObject: TObject);
var
  s:AnsiString;
  lvStream:TMemoryStream;
  lvCMDObject:TSimpleMsgPack;
  lvItem, lvList:TSimpleMsgPack;
  UserNum,I:Integer;
begin
  lvStream := TMemoryStream(pvObject);
  lvCMDObject:= TSimpleMsgPack.Create;
  try
    lvCMDObject.DecodeFromStream(lvStream);

    // 异常信息
    if lvCMDObject.ForcePathObject('result.code').AsInteger = -1 then
    begin
      sfLogger.logMessage(lvCMDObject.ForcePathObject('result.msg').AsString);
    end;

    if lvCMDObject.ForcePathObject('requestID').AsString = 'login' then
    begin
      UpdataUser;
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
      begin
        sfLogger.logMessage('登陆成功...');
        iocpTaskManager.PostATask(StartHeart, true);
      end;
    end else if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 21 then
    begin
       UpdataUser;
      if lvCMDObject.ForcePathObject('type').AsInteger = 0 then
      begin
        sfLogger.logMessage(Format('用户[%s]已经下线!', [lvCMDObject.ForcePathObject('userid').AsString]));
      end else
      begin
        sfLogger.logMessage(Format('用户[%s]上线!', [lvCMDObject.ForcePathObject('userid').AsString]));
      end;
    end else if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 6 then
    begin
      sfLogger.logMessage(Format('用户[%s]私聊对你说:%s',[lvCMDObject.ForcePathObject('userid').AsString, lvCMDObject.ForcePathObject('msg').AsString]));
    end else if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 5 then
    begin
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
      begin
        sfLogger.logMessage(Format('你说了:%s',
          [lvCMDObject.ForcePathObject('params.msg').AsString]));
      end;       
    end else if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 3 then
    begin
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
      begin
          UserNum:=lvCMDObject.ForcePathObject('list').Count;
          for I := 0 to UserNum-1 do
          begin
          lstUsers.Clear;
          lstUsers.Items.Add(lvCMDObject.ForcePathObject('list').Items[i].ForcePathObject('userid').AsString);
          end;
//         ShowMessage(lvCMDObject.ForcePathObject('list').Items[0].ForcePathObject('userid').AsString);
//         ShowMessage(lvCMDObject.ForcePathObject('list').Items[1].ForcePathObject('userid').AsString);
      end;
    end else if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 6 then
    begin
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
      begin
         mmoData.Lines.Add(lvCMDObject.ForcePathObject('userid').AsString+'说：'+

                lvCMDObject.ForcePathObject('msg').AsString );
//         ShowMessage(lvCMDObject.ForcePathObject('list').Items[0].ForcePathObject('userid').AsString);
//         ShowMessage(lvCMDObject.ForcePathObject('list').Items[1].ForcePathObject('userid').AsString);
      end;
    end;
  finally
    lvCMDObject.Free;
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

procedure TfrmMain.UpdataUser; //更新在线列表
begin
        FCMDObject.Clear;
        FCMDObject.ForcePathObject('cmdIndex').AsInteger := 3;
        FCMDObject.ForcePathObject('requestID').AsString := cbbName.Text;
        FCMDObject.ForcePathObject('params.page').AsString :='1';
        SendCMDObject(FCMDObject);
end;

end.
