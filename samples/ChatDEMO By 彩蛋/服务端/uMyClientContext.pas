unit uMyClientContext;

interface

uses
  diocp_coder_tcpServer, SysUtils, Classes, Windows, Math, SimpleMsgPack,
  diocp_tcp_server, diocp.session;

type
  TChatSession = class(TSessionItem)
  private
    FContext: TIocpClientContext;
    //FOwnerTcpServer: TDiocpTcpServer;
    FData: TSimpleMsgPack;
    FState: Integer;
    FUserID: String;
    FVerified: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///   对应连接
    /// </summary>
    property Context: TIocpClientContext read FContext write FContext;
    property Data: TSimpleMsgPack read FData;
    /// <summary>
    ///   状态 (0,离线, 1:在线, 2:隐身)
    /// </summary>
    property State: Integer read FState write FState;
    property UserID: String read FUserID write FUserID;
    /// <summary>
    ///   验证状态
    /// </summary>
    property Verified: Boolean read FVerified write FVerified;
    //property OwnerTcpServer: TDiocpTcpServer read FOwnerTcpServer write FOwnerTcpServer;
  end;

  TMyClientContext = class(TIOCPCoderClientContext)
  private
    procedure ChatExecute(pvCMDObject: TSimpleMsgPack);

    procedure SendCMDObject(pvCMDObject: TSimpleMsgPack; pvContext: TObject);  // 发送消息
    procedure DispatchCMDObject(pvCMDObject: TSimpleMsgPack);  // 广播
    procedure ExecuteHeart(pvCMDObject: TSimpleMsgPack);  // 心跳包
    procedure ExecuteAllUsers(pvCMDObject: TSimpleMsgPack);  // 获取所有在线用户
    procedure ExecuteLogin(pvCMDObject: TSimpleMsgPack);  // 用户上线
    procedure ExecuteSendMessage(pvCMDObject: TSimpleMsgPack);  // 发送消息
  protected
    procedure OnDisconnected; override;
    procedure OnConnected; override;
  public
    /// <summary>
    /// 数据处理
    /// </summary>
    /// <param name="pvObject"> (TObject) </param>
    procedure DoContextAction(const pvObject: TObject); override;
  end;

implementation

uses
  utils_safeLogger;

var
  ChatSessions: TSessions;

/// <summary>
///   进行广播
/// </summary>
procedure TMyClientContext.ChatExecute(pvCMDObject: TSimpleMsgPack);
var
  lvCMDIndex:Integer;
begin
  lvCMDIndex := pvCMDObject.ForcePathObject('cmdIndex').AsInteger;
  case lvCMDIndex of
    0: ExecuteHeart(pvCMDObject);        // 心跳
    3: ExecuteAllUsers(pvCMDObject);     // 获取所有用户
    5: ExecuteSendMessage(pvCMDObject);  // 发送消息
    11: ExecuteLogin(pvCMDObject);       // 登陆/上线
  else
    begin
      raise exception.CreateFmt('未知的命令[%d]', [lvCMDIndex]);
    end;
  end;
end;

procedure TMyClientContext.DispatchCMDObject(pvCMDObject: TSimpleMsgPack);
var
  lvMS:TMemoryStream;
  i:Integer;
  lvList:TList;
  lvContext:TIOCPCoderClientContext;
begin
  lvMS := TMemoryStream.Create;
  lvList := TList.Create;
  try
    pvCMDObject.EncodeToStream(lvMS);
    lvMS.Position := 0;

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
  lvCMDObj := TSimpleMsgPack.Create;
  try
    try
      TMemoryStream(pvObject).Position := 0;
      lvCMDObj.DecodeFromStream(TMemoryStream(pvObject));

      ChatExecute(lvCMDObj);

      if lvCMDObj.O['cmdIndex'] <> nil then
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
      lvCMDObj.EncodeToStream(TMemoryStream(pvObject));
      TMemoryStream(pvObject).Position := 0;
      WriteObject(pvObject);  // 添加到SendingQueue回写对象
    end;
  finally
    lvCMDObj.Free;
  end;
end;

/// <summary>
/// 列出所有用户
///
/// </summary>
/// <param name="pvCMDObject">
///
///   请求包:
///   {
/// 	   "cmdIndex": 3,
/// 	   "requestID":"xxx-xx-xx-xx",  // 请求的ID, 不重复的字符串，回应时, 会带回去
/// 	   "params":
/// 	    {
/// 		    "page":1,               // 查询页码(显示第几页数据)
/// 		  }
/// 	}
///
/// 	响应包:
///   {
/// 	   "cmdIndex": 3,
/// 	   "requestID":"xxx-xx-xx-xx",  // 请求的ID, 不重复的字符串，回应时, 会带回去
///   	 "result":
/// 	    {
/// 		   "code":0,           // 返回码, 0:成功, -1:失败
/// 		   "msg":"错误信息"
/// 	    },
/// 	   "list":
///       [
/// 	      {"userid":"邮箱或者电话", "nickname":"昵称", "imageIndex":0, "state":0},
/// // state:状态(0,离线, 1:在线, 2:隐身)
/// 		    {"userid":"邮箱或者电话", "nickname":"昵称", "imageIndex":0, "state":0}
/// 	    ]
/// 	}
/// </param>
procedure TMyClientContext.ExecuteAllUsers(pvCMDObject: TSimpleMsgPack);
var
  lvSession, lvTempSession: TChatSession;
  i: Integer;
  lvItem, lvList:TSimpleMsgPack;
  lvSessions:TList;
begin
  if Self.LockContext('执行登陆', nil) then
  try
    lvSession := TChatSession(Self.Data);
    if lvSession = nil then
    begin
      raise Exception.Create('未登陆用户!');
    end;

    lvSessions := TList.Create;
    try
      // 获取所有在线连接上下文
      ChatSessions.GetSessionList(lvSessions);

      lvList := pvCMDObject.ForcePathObject('list');
      for i := 0 to lvSessions.Count - 1 do
      begin
        lvTempSession := TChatSession(lvSessions[i]);
        if lvTempSession <> lvSession then  // 不是当前请求数据的用户
        begin
          if lvTempSession.State = 1 then  // 在线
          begin
            lvItem := lvList.AddArrayChild;
            lvItem.Add('userid', lvTempSession.UserID);
          end;
        end;
      end;
    finally
      lvSessions.Free;
    end;
  finally
    Self.UnLockContext('执行登陆', nil);
  end;
end;

/// <summary> 进行心跳
/// </summary>
/// <param name="pvCMDObject">
///   {
///      "cmdIndex": 0,
///   }
/// </param>
procedure TMyClientContext.ExecuteHeart(pvCMDObject: TSimpleMsgPack);
var
  lvSession:TChatSession;
begin
  if Self.LockContext('执行心跳', nil) then
  try
    lvSession := TChatSession(Self.Data);
    if lvSession = nil then
    begin
      pvCMDObject.ForcePathObject('result.msg').AsString := '尚未登陆...';
      pvCMDObject.ForcePathObject('result.code').AsInteger := -1;
      Exit;
    end;

    {if lvSession.Context <> Self then
    begin
      pvCMDObject.ForcePathObject('result.msg').AsString := '你的帐号已经在其他客户端进行登陆...';
      pvCMDObject.ForcePathObject('result.code').AsInteger := -1;
      Exit;
    end;}

    //sfLogger.logMessage(lvSession.UserID + ' 心跳包');

    if lvSession.Verified then
    begin
      if lvSession.State = 0 then
      begin  // 在线
        lvSession.State := 1;
      end;
    end;

    lvSession.DoActivity();

    // 清理，不需要返回客户端
    pvCMDObject.Clear;
  finally
    Self.UnLockContext('执行心跳', nil);
  end;
end;

/// <summary>
///  用户登陆
/// </summary>
/// <param name="pvCMDObject">
/// 登陆:
///    请求包:
///     {
///       "cmdIndex": 11,
///       "requestID":"xxx-xx-xx-xx",  // 请求的ID, 不重复的字符串，回应时, 会带回去
///       "params":
///       {
///         "userid":"邮箱或者电话",      // 接收信息ID
///         "pass":"xxx",                 // 密码base 64位密码
///       }
///     }
///
/// 	响应包:
///     {
///       "cmdIndex": 11,
///       "requestID":"xxx-xx-xx-xx",  // 请求的ID, 不重复的字符串，回应时, 会带回去
///       "result":
///       {
///         "code":0,           // 返回码, 0:成功, -1:失败, 1:离线请求
///         "msg":"错误信息"
///       }
///     }
///  </param>
procedure TMyClientContext.ExecuteLogin(pvCMDObject: TSimpleMsgPack);
var
  lvSession:TChatSession;
  lvSQL, lvPass, lvUserID:String;
  lvCMDObject:TSimpleMsgPack;
begin
  lvUserID := pvCMDObject.ForcePathObject('params.userid').AsString;
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
    //lvSession.OwnerTcpServer := Self.Owner;

    lvSession.UserID := lvUserID;
    lvSession.State := 1;  // 在线
    lvSession.Verified := true;  // 已经验证
    Self.Data := lvSession;  // 建立关联关系

    sfLogger.logMessage(lvUserID + ' 上线[' + RemoteAddr + ':' + IntToStr(RemotePort) + ']');

    // 上线通知推送
    lvCMDObject := TSimpleMsgPack.Create;
    try
      lvCMDObject.ForcePathObject('cmdIndex').AsInteger := 21;
      lvCMDObject.ForcePathObject('userid').AsString := lvUserID;
      lvCMDObject.ForcePathObject('type').AsInteger := 1;  // 上线
      if Self <> nil then
      begin   // 推送消息
        DispatchCMDObject(lvCMDObject);
      end;
    finally
      lvCMDObject.Free;
    end;
  finally
    Self.UnLockContext('执行登陆', nil);
  end;
end;

/// <summary>
/// 发送信息
/// </summary>
/// <param name="pvCMDObject">
/// 发送信息:
///   请求包:
///   {
///     "cmdIndex": 5,
///     "requestID":"xxx-xx-xx-xx",  // 请求的ID, 不重复的字符串，回应时, 会带回去
///     "params":
///     {
///       "userid":"邮箱或者电话",      // 接收信息ID
///       "msg":"要发送的文字"
/// 		}
///
/// 	}
///
/// 	响应包:
///   {
///     "cmdIndex": 5,
///     "requestID":"xxx-xx-xx-xx",  // 请求的ID, 不重复的字符串，回应时, 会带回去
///     "result":
///     {
///       "code":0,           // 返回码, 0:成功, -1:失败, 1:离线信息
///       "msg":"错误信息"
///     }
///   }
///  </param>
procedure TMyClientContext.ExecuteSendMessage(pvCMDObject: TSimpleMsgPack);
var
  lvSession, lvSession2:TChatSession;
  lvSent:Boolean;
  lvSQL, lvPass, lvUserID, lvUserID2:String;
  lvItem, lvList, lvSendCMDObject:TSimpleMsgPack;
  lvSendContext: TIocpClientContext;
begin
  lvSent := false;
  if Self.LockContext('发送信息', nil) then
  try
    lvSession := TChatSession(Self.Data);
    if lvSession = nil then
    begin
      raise Exception.Create('未登陆用户!');
    end;

    // 接收用户ID <不指定发送给所有用户>
    lvUserID2 := pvCMDObject.ForcePathObject('params.userid').AsString;

    // 请求ID
    lvUserID := lvSession.UserID;

    if lvUserID2 = '' then
    begin    // 发送给所有用户
      lvSendCMDObject := TSimpleMsgPack.Create;
      try
        lvSendCMDObject.ForcePathObject('cmdIndex').AsInteger := 6;
        lvSendCMDObject.ForcePathObject('userid').AsString := lvUserID;
        lvSendCMDObject.ForcePathObject('requestID').AsString := pvCMDObject.ForcePathObject('requestID').AsString;
        lvSendCMDObject.ForcePathObject('msg').AsString :=
          pvCMDObject.ForcePathObject('params.msg').AsString;
        DispatchCMDObject(lvSendCMDObject);

        sfLogger.logMessage(lvUserID + ' 所有人：' + pvCMDObject.ForcePathObject('params.msg').AsString);
      finally
        lvSendCMDObject.Free;
      end;
    end
    else
    begin
      // 接收用户
      lvSession2 := TChatSession(ChatSessions.FindSession(lvUserID2));

      if lvSession2 <> nil then
      begin
        lvSendContext := lvSession2.Context;
        if lvSendContext <> nil then
        begin
          if lvSendContext.LockContext('推送信息', nil) then
          begin
            try
              lvSendCMDObject := TSimpleMsgPack.Create;
              try
                lvSendCMDObject.ForcePathObject('cmdIndex').AsInteger := 6;
                lvSendCMDObject.ForcePathObject('userid').AsString := lvUserID;
                lvSendCMDObject.ForcePathObject('requestID').AsString := pvCMDObject.ForcePathObject('requestID').AsString;
                lvSendCMDObject.ForcePathObject('msg').AsString :=
                  pvCMDObject.ForcePathObject('params.msg').AsString;

                SendCMDObject(lvSendCMDObject, lvSendContext);
                lvSent := true;

                sfLogger.logMessage(lvUserID + ' ' + lvUserID2 + '：' + pvCMDObject.ForcePathObject('params.msg').AsString);
              finally
                lvSendCMDObject.Free;
              end;
            finally
              lvSendContext.UnLockContext('推送信息', nil);
            end;
          end;
        end;
      end;

      if not lvSent then
      begin
        // 离线信息
        pvCMDObject.ForcePathObject('result.code').AsInteger := -1;
        pvCMDObject.ForcePathObject('result.msg').AsString := '用户不在线';
      end;
    end;
  finally
    Self.UnLockContext('执行登陆', nil);
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
begin
  lvSession := TChatSession(Self.Data);
  lvCMDObject := TSimpleMsgPack.Create;
  try
    lvCMDObject.ForcePathObject('cmdIndex').AsInteger := 21;
    lvCMDObject.ForcePathObject('userid').AsString := lvSession.UserID;
    lvCMDObject.ForcePathObject('type').AsInteger := 0;  // 离线
    //if (lvSession.OwnerTcpServer <> nil) and (Self <> nil) then  // 推送消息
    DispatchCMDObject(lvCMDObject);

    sfLogger.logMessage(lvSession.UserID + ' 下线[' + RemoteAddr + ':' + IntToStr(RemotePort) + ']');
    ChatSessions.RemoveSession(lvSession.UserID);
  finally
    lvCMDObject.Free;
  end;
end;

procedure TMyClientContext.SendCMDObject(pvCMDObject: TSimpleMsgPack; pvContext: TObject);
var
  lvMS:TMemoryStream;
begin
  lvMS := TMemoryStream.Create;
  try
    pvCMDObject.EncodeToStream(lvMS);
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
  FData := TSimpleMsgPack.Create;
end;

destructor TChatSession.Destroy;
begin
  FData.Free;
  inherited;
end;

initialization
  ChatSessions := TSessions.Create(TChatSession);

finalization
  ChatSessions.Free;

end.


