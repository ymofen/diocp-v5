unit frm_Server;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Menus, StdCtrls, Generics.Collections,
  diocp_coder_tcpServer, diocp_tcp_server, uMyClientContext, diocp.session,
  SimpleMsgPack, utils_locker;

type
  TForm5 = class(TForm)
    mmain: TMainMenu;
    mniN1: TMenuItem;
    mniStart: TMenuItem;
    mniN3: TMenuItem;
    mniStop: TMenuItem;
    tmrKeepAlive: TTimer;
    pgc: TPageControl;
    tsState: TTabSheet;
    tsMsg: TTabSheet;
    lvUser: TListView;
    statCtl: TStatusBar;
    mmoMsg: TMemo;
    pnl1: TPanel;
    edtMsg: TEdit;
    btnSend: TButton;
    btn1: TButton;
    procedure mniStartClick(Sender: TObject);
    procedure mniN3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mniStopClick(Sender: TObject);
    procedure tmrKeepAliveTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    /// <summary>
    /// UI操作锁
    /// </summary>
    FUILocker: TIocpLocker;

    /// <summary>
    /// IOCPServer
    /// </summary>
    FTcpServer: TDiocpCoderTcpServer;

    /// <summary>
    /// 离线信息列表
    /// </summary>
    FOfflineMsgs: TObjectList<TOfflineInfo>;

    /// <summary>
    /// 刷新界面显示的数据
    /// </summary>
    procedure RefreshUIState;

    /// <summary>
    /// 刷新用户
    /// </summary>
    procedure RefreshClientList;

    /// <summary>
    /// 有新用户上线
    /// </summary>
    /// <param name="AUserID">上线用户的UserID</param>
    procedure ContextLogin(const AUserID: string);

    /// <summary>
    /// 有用户下线
    /// </summary>
    /// <param name="AUserID"></param>
    procedure ContextLogout(const AUserID: string);

    /// <summary>
    /// 客户端请求获取所有用户事件
    /// </summary>
    /// <param name="AMsgPackFrom">请求消息</param>
    procedure GetAllUserEvent(const AMsgPackFrom: TSimpleMsgPack);

    /// <summary>
    /// 服务端记录一条离线消息
    /// </summary>
    /// <param name="AToUserID">接收人UserID</param>
    /// <param name="AFromUserID">发送人UserID</param>
    /// <param name="AMsg">请求消息</param>
    procedure SetOffLineMsgEvent(const AToUserID, AFromUserID, AMsg: string);

    /// <summary>
    /// 返回指定人的离线消息
    /// </summary>
    /// <param name="AMsgPackFrom">请求消息</param>
    procedure GetOfflineMsgEvent(const AMsgPackFrom: TSimpleMsgPack);

    /// <summary>
    /// 发送公告
    /// </summary>
    /// <param name="AMsg">公告消息</param>
    procedure SendAnnouncement(const AMsg: string);
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  uDIOCPStreamCoder, utils_safeLogger, uFMMonitor, frm_dm;

{$R *.dfm}

procedure TForm5.btnSendClick(Sender: TObject);
begin
  SendAnnouncement(edtMsg.Text);  // 发送公告给所有人
end;

procedure TForm5.ContextLogin(const AUserID: string);
begin
  RefreshClientList;  // 刷新用户列表UI
end;

procedure TForm5.ContextLogout(const AUserID: string);
begin
  RefreshClientList; // 刷新用户列表UI
end;

procedure TForm5.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FTcpServer.Active then
  begin
    ShowMessage('请先停止服务端的运行，再退出！');
    CanClose := False;
  end;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  pgc.ActivePageIndex := 0;
  FUILocker := TIocpLocker.Create('界面异步操作锁');
  FOfflineMsgs := TObjectList<TOfflineInfo>.Create;

  FTcpServer := TDiocpCoderTcpServer.Create(Self);
  FTcpServer.CreateDataMonitor;  // 创建监视器(如不创建TFMMonitor创建后不能获取监控数据)
  FTcpServer.WorkerCount := 3;
  // register decoder and encoder class
  FTcpServer.RegisterCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);  // 注册加解密流类
  FTcpServer.RegisterContextClass(TMyClientContext);  // 注册客户端Context
  //FTcpServer.OnContextDisconnected := OnContextNotifyEvent;
  // 设置日志记录
  sfLogger.setAppender(TStringsAppender.Create(mmoMsg.Lines));
  sfLogger.AppendInMainThread := true;

  TFMMonitor.CreateAsChild(tsState, FTcpServer);  // 创建服务端运行监控
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  FTcpServer.Free;
  FOfflineMsgs.Free;
  FUILocker.Free;
end;

procedure TForm5.FormShow(Sender: TObject);
begin
  RefreshUIState;
end;

procedure TForm5.GetAllUserEvent(const AMsgPackFrom: TSimpleMsgPack);
var
  i: Integer;
  vMsgList, lvItem: TSimpleMsgPack;
  vReqUserID: string;
  vPageNo: Integer;
begin
  FUILocker.Lock('获取所有用户');
  try
    vReqUserID := AMsgPackFrom.ForcePathObject('requestID').AsString;  // 请求人的UserID
    vPageNo := AMsgPackFrom.ForcePathObject('params.page').AsInteger;  // 请求第几页数据
    vMsgList := AMsgPackFrom.ForcePathObject('list');  // 请求返回数据存放的列表
    // 将所有的用户返回到请求
    for i := 0 to lvUser.Items.Count - 1 do
    begin
      if lvUser.Items[i].Caption <> vReqUserID then
      begin
        lvItem := vMsgList.AddArrayChild;
        lvItem.ForcePathObject('user.id').AsString := lvUser.Items[i].Caption;
        lvItem.ForcePathObject('user.name').AsString := lvUser.Items[i].SubItems[0];
        if lvUser.Items[i].SubItems[2] = '在线' then
          lvItem.ForcePathObject('user.state').AsInteger := 1
        else
          lvItem.ForcePathObject('user.state').AsInteger := 0;
      end;
    end;
  finally
    FUILocker.UnLock;
  end;
end;

procedure TForm5.mniN3Click(Sender: TObject);
begin
  FTcpServer.DisconnectAll;
end;

procedure TForm5.mniStartClick(Sender: TObject);
begin
  FTcpServer.Port := 60544;
  FTcpServer.Active := true;
  // 赋值相关事件
  OnContextLogin := ContextLogin;
  OnContextLogout := ContextLogout;
  OnGetAllUser := GetAllUserEvent;
  OnSetOfflineMsg := SetOffLineMsgEvent;
  OnGetOfflineMsg := GetOfflineMsgEvent;

  RefreshUIState;
  RefreshClientList;
end;

procedure TForm5.mniStopClick(Sender: TObject);
begin
  FTcpServer.SafeStop;
  RefreshUIState;
end;

procedure TForm5.SetOffLineMsgEvent(const AToUserID, AFromUserID, AMsg: string);
var
  vOfflineInfo: TOfflineInfo;
begin
  // 离线信息
  vOfflineInfo := TOfflineInfo.Create;
  vOfflineInfo.DT := Now;  // 离线留言时间
  vOfflineInfo.ToUID := AToUserID;  // 离线消息接收者
  vOfflineInfo.FromUID := AFromUserID;  // 离线消息发送者
  vOfflineInfo.Msg := AMsg;  // 离线消息内容
  FOfflineMsgs.Add(vOfflineInfo);
end;

procedure TForm5.RefreshClientList;
var
  i, vInLineCount: Integer;
  vListItem: TListItem;
  vSession: TChatSession;
begin
  FUILocker.Lock('更新在线客户端');
  //FTcpServer.GetOnlineContextList(vContexts);
  try
    lvUser.Clear;
    vInLineCount := 0;

    if not FTcpServer.Active then Exit;

    dm.GetAllUser;  // 获取所有注册的用户
    if dm.qryTemp.RecordCount = 0 then Exit;
    // 用户信息更新到界面控件
    lvUser.Items.BeginUpdate;
    try
      dm.qryTemp.First;
      while not dm.qryTemp.Eof do
      begin
        vListItem := lvUser.Items.Add;
        vListItem.Data := nil;
        vListItem.Caption := dm.qryTemp.FieldByName('UserID').AsString;
        vListItem.SubItems.Add(dm.qryTemp.FieldByName('UserName').AsString);
        vListItem.SubItems.Add('');
        vListItem.SubItems.Add('');

        dm.qryTemp.Next;
      end;
      // 根据当前在线用户，修改用户控件上各用户的在线状态
      for i := 0 to lvUser.Items.Count - 1 do
      begin
        vSession := TChatSession(ChatSessions.FindSession(lvUser.Items[i].Caption));
        if vSession <> nil then
        begin
          if vSession.State = 1 then  // 在线
          begin
            lvUser.Items[i].SubItems[1] := vSession.Context.RemoteAddr + ':'
              + IntToStr(vSession.Context.RemotePort);
            lvUser.Items[i].SubItems[2] := '在线';
            Inc(vInLineCount);
          end
          else
          begin
            lvUser.Items[i].SubItems[1] := '';
            lvUser.Items[i].SubItems[2] := '离线';
          end;
        end
        else
        begin
          lvUser.Items[i].SubItems[1] := '';
          lvUser.Items[i].SubItems[2] := '离线';
        end;
      end;

      statCtl.Panels[0].Text := '共 ' + IntToStr(dm.qryTemp.RecordCount)
        + ' 人 在线 ' + IntToStr(vInLineCount);
    finally
      lvUser.Items.EndUpdate;
    end;
  finally
    FUILocker.UnLock;
  end;
end;

procedure TForm5.RefreshUIState;
begin
  mniStart.Enabled := not FTcpServer.Active;
  mniStop.Enabled := FTcpServer.Active;
end;

procedure TForm5.SendAnnouncement(const AMsg: string);
var
  vMS: TMemoryStream;
  i: Integer;
  vOnlineContextList: TList;
  vContext: TIOCPCoderClientContext;
  vMsgPack: TSimpleMsgPack;
begin
  FTcpServer.Locker.Lock('公告');
  try
    vMS := TMemoryStream.Create;
    vOnlineContextList := TList.Create;
    try
      vMsgPack := TSimpleMsgPack.Create;
      try
        // 组织公告消息和内容
        vMsgPack.ForcePathObject('cmdIndex').AsInteger := 5;
        vMsgPack.ForcePathObject('requestID').AsString := 'messageID';
        vMsgPack.ForcePathObject('params.userid').AsString := 'admin';
        vMsgPack.ForcePathObject('params.msg').AsString := AMsg;
        vMsgPack.EncodeToStream(vMS);

        FTcpServer.GetOnlineContextList(vOnlineContextList);  // 获取所有在线用户(可自行更改为获取所有用户，在线直接发送，不在线的保留到离线消息列表中)
        // 发送消息到每一个客户
        for i := 0 to vOnlineContextList.Count - 1 do
        begin
          vContext := TIOCPCoderClientContext(vOnlineContextList[i]);
          vContext.LockContext('推送信息', nil);
          try
            vMS.Position := 0;
            vContext.WriteObject(vMS);
          finally
            vContext.UnLockContext('推送信息', nil);
          end;
        end;
      finally
        vMsgPack.Free;
      end;
    finally
      vMS.Free;
      vOnlineContextList.Free;
    end;
  finally
    FTcpServer.Locker.UnLock;
  end;
end;

procedure TForm5.GetOfflineMsgEvent(const AMsgPackFrom: TSimpleMsgPack);
var
  i: Integer;
  vMsgList, vMsgPack: TSimpleMsgPack;
  vUserID: string;
begin
  vUserID := AMsgPackFrom.ForcePathObject('requestID').AsString;
  if vUserID = '' then Exit;
  vMsgList := AMsgPackFrom.ForcePathObject('list');
  for i := FOfflineMsgs.Count - 1 downto 0 do  // 倒序遍历所有离线消息
  begin
    if FOfflineMsgs[i].ToUID = vUserID then  // 是发送给当前用户的消息
    begin
      vMsgPack := vMsgList.AddArrayChild;
      vMsgPack.ForcePathObject('dt').AsDateTime := FOfflineMsgs[i].DT;
      vMsgPack.ForcePathObject('from').AsString := FOfflineMsgs[i].FromUID;
      vMsgPack.ForcePathObject('msg').AsString := FOfflineMsgs[i].Msg;

      FOfflineMsgs.Delete(i);
    end;
  end;

end;

procedure TForm5.tmrKeepAliveTimer(Sender: TObject);
begin
  FTcpServer.KickOut(20000);
  ChatSessions.KickOut(20000);
  RefreshClientList;
end;

end.
