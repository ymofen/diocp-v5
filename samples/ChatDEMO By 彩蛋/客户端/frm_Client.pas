unit frm_Client;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  diocp_coder_tcpClient, SimpleMsgPack, diocp_sockets, diocp_task;

type
  TForm6 = class(TForm)
    btn1: TButton;
    lstUsers: TListBox;
    mmoMsg: TMemo;
    tmrKeepAlive: TTimer;
    btn3: TButton;
    btn4: TButton;
    edtUserID: TEdit;
    edtMsg: TEdit;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrKeepAliveTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure edtUserIDChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FUserID: string;
    FCoderTcpClient: TDiocpCoderTcpClient;
    FDiocpContext: TIocpCoderRemoteContext;
    FCMDObject: TSimpleMsgPack;
    FCMDStream: TMemoryStream;
    //
    procedure SendCMDObject(pvCMDObject: TSimpleMsgPack);
    procedure OnRecvObject(pvObject:TObject);
    procedure OnDisconnected(pvContext: TDiocpCustomContext);
    procedure KeepAlive;
    procedure UpdataUser;  // 获取在线用户列表
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses
  uDIOCPStreamCoder, utils_safeLogger;

{$R *.dfm}

procedure TForm6.btn1Click(Sender: TObject);
begin
  if edtUserID.Text = '' then
  begin
    ShowMessage('请输入你的姓名！');
    Exit;
  end;
  // 连接
  FCoderTcpClient.open;
  if FDiocpContext.Active then
  begin
    //sfLogger.logMessage('已经连接到服务器');
    Exit;
  end;
  FDiocpContext.Host := '192.168.1.10';
  FDiocpContext.Port := 60544;
  FDiocpContext.Connect;
  //sfLogger.logMessage('与服务器建立连接成功, 请进行登陆');
  // 上线
  if FDiocpContext.Active then
  begin
    FCMDObject.Clear;
    FCMDObject.ForcePathObject('cmdIndex').AsInteger := 11;
    FCMDObject.ForcePathObject('requestID').AsString := 'login';
    FCMDObject.ForcePathObject('params.userid').AsString := FUserID;
    SendCMDObject(FCMDObject);
  end;
end;

procedure TForm6.btn3Click(Sender: TObject);
begin
  FCMDObject.Clear;
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 5;
  FCMDObject.ForcePathObject('requestID').AsString := 'messageID';
  FCMDObject.ForcePathObject('params.msg').AsString := edtMsg.Text;
  SendCMDObject(FCMDObject);
end;

procedure TForm6.btn4Click(Sender: TObject);
begin
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 5;
  //FCMDObject.ForcePathObject('userid').AsString := cbbName.Text;
  FCMDObject.ForcePathObject('params.userid').AsString := lstUsers.Items[lstUsers.ItemIndex];
  FCMDObject.ForcePathObject('params.msg').AsString := edtMsg.Text;
  SendCMDObject(FCMDObject);
end;

procedure TForm6.edtUserIDChange(Sender: TObject);
begin
  FUserID := edtUserID.Text;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  sfLogger.setAppender(TStringsAppender.Create(mmoMsg.Lines));
  sfLogger.AppendInMainThread := true;

  FCoderTcpClient := TDiocpCoderTcpClient.Create(Self);
  FCoderTcpClient.OnContextDisconnected := OnDisconnected;

  FDiocpContext := TIocpCoderRemoteContext(FCoderTcpClient.Add);
  FDiocpContext.RegisterCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);
  FDiocpContext.OnContextAction := OnRecvObject;

  FCMDObject := TSimpleMsgPack.Create();
  FCMDStream := TMemoryStream.Create;
end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCMDObject);
  sfLogger.Enable := false;
  FCoderTcpClient.DisconnectAll;
  FCoderTcpClient.Free;
  FCMDStream.Free;
end;

procedure TForm6.FormShow(Sender: TObject);
begin
  FUserID := edtUserID.Text;
end;

procedure TForm6.OnDisconnected(pvContext: TDiocpCustomContext);
begin
  //sfLogger.logMessage('与服务器断开连接...');
end;

procedure TForm6.OnRecvObject(pvObject: TObject);
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
      sfLogger.logMessage(lvCMDObject.ForcePathObject('result.msg').AsString);

    if lvCMDObject.ForcePathObject('requestID').AsString = 'login' then
    begin
      UpdataUser;
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
      begin
        sfLogger.logMessage('登陆成功...');
        iocpTaskManager.PostATask(KeepAlive, true);
      end;
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 21 then
    begin
       UpdataUser;
      if lvCMDObject.ForcePathObject('type').AsInteger = 0 then
      begin
        sfLogger.logMessage(Format('用户[%s]已经下线!', [lvCMDObject.ForcePathObject('userid').AsString]));
      end
      else
      begin
        sfLogger.logMessage(Format('用户[%s]上线!', [lvCMDObject.ForcePathObject('userid').AsString]));
      end;
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 6 then
    begin
      sfLogger.logMessage(Format('用户[%s]私聊对你说:%s',[lvCMDObject.ForcePathObject('userid').AsString, lvCMDObject.ForcePathObject('msg').AsString]));
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 5 then
    begin
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
      begin
        sfLogger.logMessage(Format('你说了:%s',
          [lvCMDObject.ForcePathObject('params.msg').AsString]));
      end;
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 3 then  // 用户列表
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
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 6 then
    begin
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
      begin
         mmoMsg.Lines.Add(lvCMDObject.ForcePathObject('userid').AsString+'说：'+
           lvCMDObject.ForcePathObject('msg').AsString );
//         ShowMessage(lvCMDObject.ForcePathObject('list').Items[0].ForcePathObject('userid').AsString);
//         ShowMessage(lvCMDObject.ForcePathObject('list').Items[1].ForcePathObject('userid').AsString);
      end;
    end;
  finally
    lvCMDObject.Free;
  end;
end;

procedure TForm6.SendCMDObject(pvCMDObject: TSimpleMsgPack);
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

procedure TForm6.tmrKeepAliveTimer(Sender: TObject);
begin
  FCMDObject.Clear;
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 0;
  SendCMDObject(FCMDObject);
end;

procedure TForm6.KeepAlive;
begin
  tmrKeepAlive.Enabled := true;
end;

procedure TForm6.UpdataUser;
begin
  FCMDObject.Clear;
  FCMDObject.ForcePathObject('cmdIndex').AsInteger := 3;
  FCMDObject.ForcePathObject('requestID').AsString := FUserID;
  FCMDObject.ForcePathObject('params.page').AsString := '1';
  SendCMDObject(FCMDObject);
end;

end.
