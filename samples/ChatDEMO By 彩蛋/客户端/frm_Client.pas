unit frm_Client;

interface

uses
  SysUtils, Classes, Controls, Forms, ComCtrls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, diocp_task, SimpleMsgPack, locker, diocp_sockets;

type
  TfrmClient = class(TForm)
    tmrKeepAlive: TTimer;
    lvUser: TListView;
    pnl1: TPanel;
    pnl2: TPanel;
    btnSendMsg: TButton;
    edtMsg: TEdit;
    mmoMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrKeepAliveTimer(Sender: TObject);
    procedure btnSendMsgClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    /// <summary>
    /// UI同步临界区对象
    /// </summary>
    FUILocker: TLocker;

    /// <summary>
    /// 心跳包
    /// </summary>
    procedure KeepAliveEx;

    /// <summary>
    /// 客户端Contextg事件
    /// </summary>
    /// <param name="AObject">消息流</param>
    procedure OnContextActionEx(AObject: TObject);

    /// <summary>
    /// 刷新用户列表
    /// </summary>
    /// <param name="AMsgPack">消息包</param>
    procedure RefreshUserList(const AMsgPack: TSimpleMsgPack);

    /// <summary>
    /// 显示获取到的离线消息
    /// </summary>
    /// <param name="AMsgPack">离线消息的消息包</param>
    procedure SetOfflineMsg(const AMsgPack: TSimpleMsgPack);

    /// <summary>
    /// 客户端断开连接
    /// </summary>
    /// <param name="AContext">客户端Context</param>
    procedure OnDisConnected(AContext: TDiocpCustomContext);
  public
    { Public declarations }
  end;

var
  frmClient: TfrmClient;

implementation

uses
  ClientIocpOper, utils_safeLogger;

{$R *.dfm}

procedure TfrmClient.btnSendMsgClick(Sender: TObject);
var
  vMsgToID: string;
begin
  if DiocpContext.Active then
  begin
    if edtMsg.Text <> '' then  // 消息不为空
    begin
      if lvUser.ItemIndex < 1 then  // 发给所有人
        vMsgToID := ''
      else  // 发给指定接收人
        vMsgToID := lvUser.Items[lvUser.ItemIndex].Caption;
      CMD_SendMsg(vMsgToID, edtMsg.Text);  // 请求发送消息
    end;
  end
  else
  begin
    ShowMessage('服务端关闭，请重新登录！');
  end;
end;

procedure TfrmClient.FormCreate(Sender: TObject);
begin
  FUILocker := TLocker.Create('界面操作锁');  // 创建UI同步临界区对象
  // 设置异步日志记录相关参数
  sfLogger.setAppender(TStringsAppender.Create(mmoMsg.Lines));
  sfLogger.AppendInMainThread := True;
  IocpTaskManager.PostATask(KeepAliveEx, True);  // 添加一个任务并在主线程执行

  DiocpContext.LockContext('客户端初始化', Self);
  try
    DiocpContext.OnContextAction := OnContextActionEx;  // 客户端上下文事件
    DiocpContext.OnDisconnectedEvent := OnDisConnected;
  finally
    DiocpContext.UnLockContext('客户端初始化', Self);
  end;
end;

procedure TfrmClient.FormDestroy(Sender: TObject);
begin
  sfLogger.Enable := False;
  FUILocker.Free;
end;

procedure TfrmClient.FormShow(Sender: TObject);
begin
  CMD_UpdataUsers(CurUserID);  // 请求所有用户列表
  CMD_OfflineMessage(CurUserID);  // 请求
end;

procedure TfrmClient.KeepAliveEx;
begin
  tmrKeepAlive.Enabled := true;
end;

procedure TfrmClient.OnContextActionEx(AObject: TObject);
var
  lvStream:TMemoryStream;
  lvCMDObject: TSimpleMsgPack;
  lvItem, lvList:TSimpleMsgPack;
begin
  lvStream := TMemoryStream(AObject);
  lvCMDObject:= TSimpleMsgPack.Create;
  try
    lvCMDObject.DecodeFromStream(lvStream);
    // 异常信息
    if lvCMDObject.ForcePathObject('result.code').AsInteger = -1 then
      sfLogger.logMessage(lvCMDObject.ForcePathObject('result.msg').AsString);

    if lvCMDObject.ForcePathObject('requestID').AsString = 'login' then  // 登录请求的返回数据
    begin
      CMD_UpdataUsers(CurUserID);
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then  // 发出的请求服务端执行成功
      begin
        sfLogger.logMessage('登录成功...');
        IocpTaskManager.PostATask(KeepAliveEx, True);
      end;
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 21 then  // 服务端有人上线的通知
    begin
      CMD_UpdataUsers(CurUserID);
      if lvCMDObject.ForcePathObject('type').AsInteger = 0 then
        sfLogger.logMessage(Format('[%s]下线!', [lvCMDObject.ForcePathObject('userid').AsString]))
      else
        sfLogger.logMessage(Format('[%s]上线!', [lvCMDObject.ForcePathObject('userid').AsString]));
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 6 then  // 消息是发送人对所有人发送（公告）
      sfLogger.logMessage(Format('[%s]:%s', [lvCMDObject.ForcePathObject('userid').AsString,
        lvCMDObject.ForcePathObject('msg').AsString]))
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 5 then  // 有人发送私聊信息
    begin
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
      begin
        sfLogger.logMessage(Format('%s', [lvCMDObject.ForcePathObject('params.msg').AsString]));
      end;
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 7 then  // 从服务端获取到离线期间积累的消息
    begin
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
        SetOfflineMsg(lvCMDObject);
    end
    else
    if lvCMDObject.ForcePathObject('cmdIndex').AsInteger = 3 then  // 返回所有用户列表信息
    begin
      if lvCMDObject.ForcePathObject('result.code').AsInteger = 0 then
        RefreshUserList(lvCMDObject);
    end;
  finally
    lvCMDObject.Free;
  end;
end;

procedure TfrmClient.OnDisConnected(AContext: TDiocpCustomContext);
begin
  ShowMessage('服务端关闭，请重新登录！');
end;

procedure TfrmClient.RefreshUserList(const AMsgPack: TSimpleMsgPack);
var
  vUserCount, i: Integer;
  vListItem: TListItem;
begin
  FUILocker.Lock('更新用户列表');
  try
    vUserCount := AMsgPack.ForcePathObject('list').Count;  // 用户数
    lvUser.Clear;
    if vUserCount = 0 then Exit;
    vListItem := lvUser.Items.Add;  // 先添加一个所有人的数据便于向所有人发送消息
    vListItem.Caption := '所有人';
    // 添加各用户
    lvUser.Items.BeginUpdate;
    try
      for i := 0 to vUserCount - 1 do
      begin
        vListItem := lvUser.Items.Add;
        vListItem.Caption := AMsgPack.ForcePathObject('list').Items[i].ForcePathObject('user.id').AsString;  // 用户ID
        vListItem.SubItems.Add(AMsgPack.ForcePathObject('list').Items[i].ForcePathObject('user.name').AsString);  // 用户名
        if AMsgPack.ForcePathObject('list').Items[i].ForcePathObject('user.state').AsInteger = 0 then  // 不在线
          vListItem.SubItems.Add('离线')
        else
          vListItem.SubItems.Add('在线');
      end;
    finally
      lvUser.Items.EndUpdate;
    end;
  finally
    FUILocker.UnLock;
  end;
end;

procedure TfrmClient.SetOfflineMsg(const AMsgPack: TSimpleMsgPack);
var
  vMsgCount, i: Integer;
  vListItem: TListItem;
begin
  FUILocker.Lock('获取离线消息');
  try
    vMsgCount := AMsgPack.ForcePathObject('list').Count;  // 获取和我相关的离线消息数
    if vMsgCount = 0 then Exit;
    mmoMsg.Lines.BeginUpdate;
    try
      for i := 0 to vMsgCount - 1 do
      begin
        mmoMsg.Lines.Add(Format('[%s] %s:%s',  // 显示各离线消息
          [FormatDateTime('YYYY-MM-DD hh:mm:ss', AMsgPack.ForcePathObject('list').Items[i].ForcePathObject('dt').AsDateTime),
          AMsgPack.ForcePathObject('list').Items[i].ForcePathObject('from').AsString,
          AMsgPack.ForcePathObject('list').Items[i].ForcePathObject('msg').AsString]));
      end;
    finally
      mmoMsg.Lines.EndUpdate;
    end;
  finally
    FUILocker.UnLock;
  end;
end;

procedure TfrmClient.tmrKeepAliveTimer(Sender: TObject);
begin
  CMD_KeepAlive;  // 心跳包
end;

end.
