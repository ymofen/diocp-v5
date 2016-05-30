unit uMyClientContext;

interface

uses
  diocp_coder_tcpServer, SysUtils, Classes, Windows, Math, SimpleMsgPack,
  diocp_tcp_server, diocp.session;

type
  TOfflineInfo = class
  private
    FFromUID,
    FToUID,
    FMsg: string;
    FDT: TDateTime;
  public
    property FromUID: string read FFromUID write FFromUID;
    property ToUID: string read FToUID write FToUID;
    property Msg: string read FMsg write FMsg;
    property DT: TDateTime read FDT write FDT;
  end;

  TChatSession = class(TSessionItem)
  private
    /// <summary>
    /// 会话对应的连接Context
    /// </summary>
    FContext: TIocpClientContext;

    /// <summary>
    /// 会话每次请求的消息包
    /// </summary>
    FMsgPack: TSimpleMsgPack;

    /// <summary>
    /// 会话当前状态
    /// </summary>
    FState: Integer;

    /// <summary>
    /// 会话对应的用户UserID
    /// </summary>
    FUserID: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    /// 对应的连接Context
    /// </summary>
    property Context: TIocpClientContext read FContext write FContext;
    property MsgPack: TSimpleMsgPack read FMsgPack;
    /// <summary>
    ///   状态 (0,离线, 1:在线, 2:隐身)
    /// </summary>
    property State: Integer read FState write FState;
    property UserID: string read FUserID write FUserID;
  end;

  /// <summary>
  /// 用户上线事件
  /// </summary>
  TContextLoginNotify = procedure(const AUserID: string) of object;

  /// <summary>
  /// 用户下线事件
  /// </summary>
  TContextLogoutNotify = procedure(const AUserID: string) of object;

  /// <summary>
  /// 用户发送离线消息事件
  /// </summary>
  TSetOfflineMsgNotify = procedure (const AToUserID, AFromUserID, AMsg: string) of object;

  /// <summary>
  /// 用户获取离线消息事件
  /// </summary>
  TGetOfflineMsgNotify = procedure(const AMsgPack: TSimpleMsgPack) of object;

  /// <summary>
  /// 获取所有用户事件
  /// </summary>
  TGetAllUserNotify = procedure (const AMsgPack: TSimpleMsgPack) of object;

  TMyClientContext = class(TIOCPCoderClientContext)
  private
    /// <summary>
    /// 客户端有数据发送到服务端
    /// </summary>
    /// <param name="pvCMDObject"></param>
    procedure ChatExecute(pvCMDObject: TSimpleMsgPack);

    /// <summary>
    /// 发送消息
    /// </summary>
    /// <param name="pvCMDObject">消息包</param>
    /// <param name="pvContext">客户端Context</param>
    procedure SendMsgPack(AMsgPack: TSimpleMsgPack; pvContext: TObject);

    /// <summary>
    /// 分发消息到所有在线用户
    /// </summary>
    /// <param name="AMsgPack">消息包</param>
    procedure DispatchMsgPackToAll(AMsgPack: TSimpleMsgPack);

    /// <summary>
    /// 心跳包
    /// </summary>
    /// <param name="AMsgPack">心跳消息包</param>
    procedure ExecuteHeart(AMsgPack: TSimpleMsgPack);

    /// <summary>
    /// 获取所有在线用户
    /// </summary>
    /// <param name="AMsgPack">消息包</param>
    procedure ExecuteAllUser(AMsgPack: TSimpleMsgPack);

    /// <summary>
    /// 用户上线
    /// </summary>
    /// <param name="AMsgPack">上线消息包</param>
    procedure ExecuteLogin(AMsgPack: TSimpleMsgPack);

    /// <summary>
    /// 发送消息给指定用户
    /// </summary>
    /// <param name="AMsgPackFrom">消息包</param>
    procedure ExecuteSendMessage(AMsgPackFrom: TSimpleMsgPack);

    /// <summary>
    /// 获取离线消息
    /// </summary>
    /// <param name="AMsgPackFrom">消息包</param>
    procedure ExecuteOfflineMessage(AMsgPackFrom: TSimpleMsgPack);
  protected
    procedure OnDisconnected; override;
    procedure OnConnected; override;
  public
    /// <summary>
    /// 处理客户端传来的数据
    /// </summary>
    procedure DoContextAction(const pvObject: TObject); override;
  end;

var
  ChatSessions: TSessions;
  OnContextLogin: TContextLoginNotify;
  OnContextLogout: TContextLogoutNotify;
  OnGetAllUser: TGetAllUserNotify;
  OnSetOffLineMsg: TSetOfflineMsgNotify;
  OnGetOfflineMsg: TGetOfflineMsgNotify;

implementation

uses
  utils_safeLogger;

procedure TMyClientContext.ChatExecute(pvCMDObject: TSimpleMsgPack);
var
  lvCMDIndex:Integer;
begin
  lvCMDIndex := pvCMDObject.ForcePathObject('cmdIndex').AsInteger;
  case lvCMDIndex of
    0: ExecuteHeart(pvCMDObject);        // 心跳
    3: ExecuteAllUser(pvCMDObject);     // 获取所有用户
    5: ExecuteSendMessage(pvCMDObject);  // 发送消息
    7: ExecuteOfflineMessage(pvCMDObject);  // 获取离线消息
    11: ExecuteLogin(pvCMDObject);       // 登陆/上线
  else
    begin
      raise exception.CreateFmt('未知的命令[%d]', [lvCMDIndex]);
    end;
  end;
end;

procedure TMyClientContext.DispatchMsgPackToAll(AMsgPack: TSimpleMsgPack);
var
  lvMS:TMemoryStream;
  i:Integer;
  lvList:TList;
  lvContext:TIOCPCoderClientContext;
begin
  lvMS := TMemoryStream.Create;
  lvList := TList.Create;
  try
    AMsgPack.EncodeToStream(lvMS);
    lvMS.Position := 0;
    // 通知所有在线的客户端有人上线或下线等行为
    Self.Owner.GetOnlineContextList(lvList);
    for i := 0 to lvList.Count - 1 do
    begin
      lvContext := TIOCPCoderClientContext(lvList[i]);
      if lvContext <> Self then
      begin
        lvContext.LockContext('推送信息', nil);
        try
          lvContext.WriteObject(lvMS);
        finally
          lvContext.UnLockContext('推送信息', nil);
        end;
      end;
    end;
  finally
    lvMS.Free;
    lvList.Free;
  end;
end;

procedure TMyClientContext.DoContextAction(const pvObject: TObject);
var
  lvCMDObj: TSimpleMsgPack;
begin
  // 此方法已经在 TIOCPCoderClientContext.DoExecuteRequest 中处理了线程同步了
  lvCMDObj := TSimpleMsgPack.Create;
  try
    try
      TMemoryStream(pvObject).Position := 0;
      lvCMDObj.DecodeFromStream(TMemoryStream(pvObject));  // 解密消息

      ChatExecute(lvCMDObj);  // 根据消息协议类型由对应的事件处理

      // 通知客户端本次调用是否成功
      if lvCMDObj.O['cmdIndex'] <> nil then  // 或 lvCMDObj.ForcePathObject('cmdIndex').AsString <> ''
      begin
        if lvCMDObj.O['result.code'] = nil then
          lvCMDObj.I['result.code'] := 0;
      end;
    except
      on E:Exception do
      begin
        lvCMDObj.ForcePathObject('result.code').AsInteger := -1;
        lvCMDObj.ForcePathObject('result.msg').AsString := e.Message;
        sfLogger.logMessage('处理逻辑出现异常:'+ e.Message);
        {$IFDEF CONSOLE}
        writeln('处理逻辑出现异常:'+ e.Message);
        {$ENDIF}
      end;
    end;

    if lvCMDObj.O['cmdIndex'] <> nil then
    begin
      TMemoryStream(pvObject).Clear;
      lvCMDObj.EncodeToStream(TMemoryStream(pvObject));  // 加密消息
      TMemoryStream(pvObject).Position := 0;
      WriteObject(pvObject);  // 添加到SendingQueue回写对象
    end;
  finally
    lvCMDObj.Free;
  end;
end;

procedure TMyClientContext.ExecuteAllUser(AMsgPack: TSimpleMsgPack);
var
  lvSession: TChatSession;
begin
  if Self.LockContext('执行登陆', nil) then
  try
    lvSession := TChatSession(Self.Data);
    if lvSession = nil then
    begin
      raise Exception.Create('未登陆用户!');
    end;

    if Assigned(OnGetAllUser) then
      OnGetAllUser(AMsgPack);
  finally
    Self.UnLockContext('执行登陆', nil);
  end;
end;

procedure TMyClientContext.ExecuteHeart(AMsgPack: TSimpleMsgPack);
var
  lvSession:TChatSession;
begin
  if Self.LockContext('执行心跳', nil) then
  try
    lvSession := TChatSession(Self.Data);
    if lvSession = nil then
    begin
      AMsgPack.ForcePathObject('result.msg').AsString := '尚未登陆...';
      AMsgPack.ForcePathObject('result.code').AsInteger := -1;
      Exit;
    end;

    if lvSession.Context <> Self then
    begin
      AMsgPack.ForcePathObject('result.msg').AsString := '你的帐号已经在其他客户端进行登陆...';
      AMsgPack.ForcePathObject('result.code').AsInteger := -1;
      Exit;
    end;

    {sfLogger.logMessage(lvSession.UserID + ' 心跳包');

    if lvSession.Verified then
    begin
      if lvSession.State = 0 then
      begin  // 在线
        lvSession.State := 1;
      end;
    end;}

    lvSession.DoActivity();

    AMsgPack.Clear;  // 清理，不需要返回客户端
  finally
    Self.UnLockContext('执行心跳', nil);
  end;
end;

procedure TMyClientContext.ExecuteLogin(AMsgPack: TSimpleMsgPack);
var
  lvSession: TChatSession;
  lvSQL, vUserPaw, lvUserID: String;
  lvCMDObject:TSimpleMsgPack;
begin
  lvUserID := AMsgPack.ForcePathObject('user.id').AsString;  // 登录人UserID
  vUserPaw := AMsgPack.ForcePathObject('user.paw').AsString;  // 登录人密码
  if lvUserID = '' then
  begin
    raise Exception.Create('缺少指定用户ID');
  end;

  if ChatSessions.FindSession(lvUserID) <> nil then
  begin
    raise Exception.Create('用户已经登陆!');
  end;

  if Self.LockContext('执行登陆', nil) then
  try
    lvSession := TChatSession(ChatSessions.CheckSession(lvUserID));  // 查找会话，如果没有则创建
    lvSession.Context := Self;

    lvSession.UserID := lvUserID;
    lvSession.State := 1;  // 在线
    Self.Data := lvSession;  // 建立关联关系

    sfLogger.logMessage(lvUserID + ' 上线[' + RemoteAddr + ':' + IntToStr(RemotePort) + ']');

    // 上线通知推送
    lvCMDObject := TSimpleMsgPack.Create;
    try
      lvCMDObject.ForcePathObject('cmdIndex').AsInteger := 21;  // 有人上线
      lvCMDObject.ForcePathObject('userid').AsString := lvUserID;
      lvCMDObject.ForcePathObject('type').AsInteger := 1;  // 上线
      if Self <> nil then
        DispatchMsgPackToAll(lvCMDObject);  // 推送消息，通知所有在线的客户端有人上线了
    finally
      lvCMDObject.Free;
    end;
  finally
    Self.UnLockContext('执行登陆', nil);
  end;

  if Assigned(OnContextLogin) then
    OnContextLogin(lvUserID);  // 上线
end;

procedure TMyClientContext.ExecuteOfflineMessage(AMsgPackFrom: TSimpleMsgPack);
var
  lvSession: TChatSession;
begin
  if Self.LockContext('获取离线消息', nil) then
  try
    lvSession := TChatSession(Self.Data);  // 得到会话
    if lvSession = nil then
      raise Exception.Create('未登陆用户!');

    if Assigned(OnGetOfflineMsg) then
      OnGetOfflineMsg(AMsgPackFrom);
  finally
    Self.UnLockContext('获取离线消息', nil);
  end;
end;

procedure TMyClientContext.ExecuteSendMessage(AMsgPackFrom: TSimpleMsgPack);
var
  vFromSession, vToSession: TChatSession;
  vFromUserID, vToUserID: string;
  vMsgPackTo: TSimpleMsgPack;
  vToContext: TIocpClientContext;
begin
  if Self.LockContext('发送信息', nil) then
  try
    vFromSession := TChatSession(Self.Data);  // 得到当前Context对应的会话
    if vFromSession = nil then
      raise Exception.Create('用户未登陆！');

    // 接收用户ID <不指定发送给所有用户>
    vToUserID := AMsgPackFrom.ForcePathObject('params.userid').AsString;
    vFromUserID := vFromSession.UserID;  // 请求ID

    if vToUserID = '' then  // 发送给所有用户
    begin
      vMsgPackTo := TSimpleMsgPack.Create;
      try
        vMsgPackTo.ForcePathObject('cmdIndex').AsInteger := 6;
        vMsgPackTo.ForcePathObject('userid').AsString := vFromUserID;
        vMsgPackTo.ForcePathObject('requestID').AsString := AMsgPackFrom.ForcePathObject('requestID').AsString;
        vMsgPackTo.ForcePathObject('msg').AsString := AMsgPackFrom.ForcePathObject('params.msg').AsString;
        DispatchMsgPackToAll(vMsgPackTo);

        sfLogger.logMessage(vFromUserID + ' 对 所有人：' + AMsgPackFrom.ForcePathObject('params.msg').AsString);
      finally
        vMsgPackTo.Free;
      end;
    end
    else  // 发送到指定用户
    begin
      vToSession := TChatSession(ChatSessions.FindSession(vToUserID));
      if vToSession <> nil then  // 接收用户在线
      begin
        vToContext := vToSession.Context;
        if vToContext <> nil then
        begin
          if vToContext.LockContext('推送信息', nil) then
          begin
            try  // 组织消息包
              vMsgPackTo := TSimpleMsgPack.Create;
              try
                vMsgPackTo.ForcePathObject('cmdIndex').AsInteger := 6;
                vMsgPackTo.ForcePathObject('userid').AsString := vFromUserID;
                vMsgPackTo.ForcePathObject('requestID').AsString :=
                  AMsgPackFrom.ForcePathObject('requestID').AsString;
                vMsgPackTo.ForcePathObject('msg').AsString :=
                  AMsgPackFrom.ForcePathObject('params.msg').AsString;

                SendMsgPack(vMsgPackTo, vToContext);

                sfLogger.logMessage(vFromUserID + ' 对' + vToUserID + '：'
                  + AMsgPackFrom.ForcePathObject('params.msg').AsString);
              finally
                vMsgPackTo.Free;
              end;
            finally
              vToContext.UnLockContext('推送信息', nil);
            end;
          end;
        end;
      end
      else  // 接收用户不在线
      begin
        AMsgPackFrom.ForcePathObject('result.code').AsInteger := -1;
        AMsgPackFrom.ForcePathObject('result.msg').AsString := '对方不在线，已留言！';
        if Assigned(OnSetOfflineMsg) then  // 消息添加到离线消息列表
          OnSetOfflineMsg(vToUserID, vFromUserID, AMsgPackFrom.ForcePathObject('params.msg').AsString);
      end;
    end;
  finally
    Self.UnLockContext('发送信息', nil);
  end;
end;

procedure TMyClientContext.OnConnected;
begin
  //sfLogger.logMessage(RemoteAddr + ':' + IntToStr(RemotePort) + ' 连接');
end;

procedure TMyClientContext.OnDisconnected;
var
  lvCMDObject: TSimpleMsgPack;
  lvSession: TChatSession;
  vUserID: string;
begin
  lvSession := TChatSession(Self.Data);
  lvCMDObject := TSimpleMsgPack.Create;
  try
    lvCMDObject.ForcePathObject('cmdIndex').AsInteger := 21;
    lvCMDObject.ForcePathObject('userid').AsString := lvSession.UserID;
    lvCMDObject.ForcePathObject('type').AsInteger := 0;  // 离线
    //if (lvSession.OwnerTcpServer <> nil) and (Self <> nil) then  // 推送消息
    DispatchMsgPackToAll(lvCMDObject);  // 通知所有在线人，我下线了

    vUserID := lvSession.UserID;
    sfLogger.logMessage(vUserID + ' 下线[' + RemoteAddr + ':' + IntToStr(RemotePort) + ']');
    ChatSessions.RemoveSession(vUserID);  // 移除下线客户端对应的会话
    if Assigned(OnContextLogout) then
      OnContextLogout(vUserID);  // 下线事件
  finally
    lvCMDObject.Free;
  end;
end;

procedure TMyClientContext.SendMsgPack(AMsgPack: TSimpleMsgPack; pvContext: TObject);
var
  lvMS:TMemoryStream;
begin
  lvMS := TMemoryStream.Create;
  try
    AMsgPack.EncodeToStream(lvMS);
    lvMS.Position := 0;
    TIOCPCoderClientContext(pvContext).WriteObject(lvMS);
  finally
    lvMS.Free;
  end;
end;

{ TChatSession }

constructor TChatSession.Create;
begin
  inherited;
  FMsgPack := TSimpleMsgPack.Create;
end;

destructor TChatSession.Destroy;
begin
  FMsgPack.Free;
  inherited;
end;

initialization
  ChatSessions := TSessions.Create(TChatSession);

finalization
  ChatSessions.Free;

end.


