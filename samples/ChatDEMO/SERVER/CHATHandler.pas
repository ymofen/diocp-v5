unit CHATHandler;

interface

uses
  SimpleMsgPack, diocp.session, diocp.tcp.server;

type
  TCHATSession = class(TSessionItem)
  private
    FContext: TIocpClientContext;
    FData: TSimpleMsgPack;
    FState: Integer;
    FUserID: String;
    FUserKey: string;
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
    property UserKey: string read FUserKey write FUserKey;

    

    /// <summary>
    ///   验证状态
    /// </summary>
    property Verified: Boolean read FVerified write FVerified;
        
  end;

/// <summary>procedure CHATExecute
/// </summary>
/// <param name="pvCMDObject"> (TSimpleMsgPack) </param>
procedure CHATExecute(pvCMDObject: TSimpleMsgPack; pvContext:
    TIocpClientContext);


var
  ChatSessions:TSessions;

implementation

uses
  utils.safeLogger, SysUtils, ComObj, utils.base64, diocp.coder.tcpServer, Uni,
  Classes;

procedure SendCMDObject(pvCMDObject:TSimpleMsgPack; pvContext: TObject);
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


/// <summary>
///   进行广播
/// </summary>
procedure DispatchCMDObject(pvCMDObject: TSimpleMsgPack; pvOwnerTcpServer:
    TDiocpTcpServer; pvIgnoreContext: TIOCPCoderClientContext);
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

    pvOwnerTcpServer.GetOnlineContextList(lvList);
    for i := 0 to lvList.Count - 1 do
    begin
      lvContext := TIOCPCoderClientContext(lvList[i]);
      if lvContext <> pvIgnoreContext then
      begin
        lvContext.WriteObject(lvMS);
      end;
    end;
  finally
    lvMS.Free;
  end;

end;

/// <summary> 进行心跳
/// </summary>
/// <param name="pvCMDObject">
///   {
///      "cmdIndex": 0,
///   }
/// </param>
procedure ExecuteHeart(pvCMDObject: TSimpleMsgPack; pvContext: TObject);
var
  lvSession:TCHATSession;
  lvContext:TIocpClientContext;
begin

  lvContext := TIocpClientContext(pvContext);
  if lvContext.LockContext('执行心跳', nil) then
  try  
    lvSession := TCHATSession(lvContext.Data);
    if lvSession = nil then
    begin
      pvCMDObject.ForcePathObject('result.msg').AsString := '尚未登陆...';
      pvCMDObject.ForcePathObject('result.code').AsInteger := -1;
      exit;
    end;

    if lvSession.Context <> pvContext then
    begin
      pvCMDObject.ForcePathObject('result.msg').AsString := '你的帐号已经在其他客户端进行登陆...';
      pvCMDObject.ForcePathObject('result.code').AsInteger := -1;
      exit;
    end;
    if lvSession.FVerified then
    begin
      if lvSession.FState = 0 then
      begin  // 在线
        lvSession.FState := 1;
      end;
    end;

    lvSession.DoActivity();

    // 清理，不需要返回客户端
    pvCMDObject.Clear;

  finally
    lvContext.UnLockContext('执行心跳', nil);
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
procedure ExecuteAllUsers(pvCMDObject: TSimpleMsgPack; pvContext: TIocpClientContext);
var
  lvSession, lvTempSession:TCHATSession;
  lvContext:TIocpClientContext;
  i: Integer;
var
  lvItem, lvList:TSimpleMsgPack;
  lvSessions:TList;
begin
  lvContext := TIocpClientContext(pvContext);
  if lvContext.LockContext('执行登陆', nil) then
  try
    lvSession := TCHATSession(lvContext.Data);
    if lvSession = nil then
    begin
      raise Exception.Create('未登陆用户!');
    end;

    lvSessions := TList.Create;
    try
      /// 获取所有在线连接上下文
      CHATSessions.GetSessionList(lvSessions);

      lvList := pvCMDObject.ForcePathObject('list');
      for i := 0 to lvSessions.Count - 1 do
      begin
        lvTempSession := TCHATSession(lvSessions[i]);
        if lvTempSession <> lvSession then
        begin   // 不是请求的当前用户
          // 在线
          if lvTempSession.State = 1 then
          begin
            lvItem := lvList.AddArrayChild;
            lvItem.Add('userid', lvTempSession.UserID);
            /// ....
          end;
        end;

      end;
    finally
      lvSessions.Free;
    end;


  finally
    lvContext.UnLockContext('执行登陆', nil);
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
procedure ExecuteLogin(pvCMDObject: TSimpleMsgPack; pvContext: TIocpClientContext);
var
  lvSession:TCHATSession;
  lvContext:TIocpClientContext;
var
  lvSQL, lvPass, lvUserID:String;
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

  lvContext := TIocpClientContext(pvContext);
  if lvContext.LockContext('执行登陆', nil) then
  try

    lvSession := TCHATSession(ChatSessions.CheckSession(lvUserID));
    lvSession.FContext := lvContext;
    
    // 在线
    lvSession.FState := 1;

    // 已经验证
    lvSession.Verified := true;

    /// 建立关联关系
    lvContext.Data := lvSession;


  finally
    lvContext.UnLockContext('执行登陆', nil);
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
procedure ExecuteSendMessage(pvCMDObject: TSimpleMsgPack; pvContext:
    TIocpClientContext);
var
  lvSession, lvSession2:TCHATSession;
  lvContext:TIocpClientContext;
  lvSent:Boolean;
var
  lvSQL, lvPass, lvUserID, lvUserID2:String;
var
  lvItem, lvList, lvSendCMDObject:TSimpleMsgPack;
  lvSendContext:TIocpClientContext;

var
  lvQuery1, lvQuery2:TUniQuery;
begin
  lvSent := false;
  lvContext := TIocpClientContext(pvContext);
  if lvContext.LockContext('发送信息', nil) then
  try
    lvSession := TCHATSession(lvContext.Data);
    if lvSession = nil then
    begin
      raise Exception.Create('未登陆用户!');
    end;


      // 接收用户ID
      lvUserID2 := pvCMDObject.ForcePathObject('params.userid').AsString;

      // 请求ID
      lvUserID := lvSession.UserID;

      // 接收用户
      lvSession2 := TCHATSession(ChatSessions.FindSession(lvUserID2));

      if lvSession2 <> nil then
      begin
        lvSendContext := lvSession2.FContext;
        if lvSendContext <> nil then
        begin
          if lvSendContext.LockContext('推送信息', nil) then
          begin
            try
              lvSendCMDObject := TSimpleMsgPack.Create;
              try
                lvSendCMDObject.ForcePathObject('cmdIndex').AsInteger := 6;
                lvSendCMDObject.ForcePathObject('userid').AsString := lvUserID;
                lvSendCMDObject.ForcePathObject('requestID').AsString :=
                  pvCMDObject.ForcePathObject('requestID').AsString;
                lvSendCMDObject.ForcePathObject('msg').AsString :=
                  pvCMDObject.ForcePathObject('params.msg').AsString;
                TIOCPCoderClientContext(lvSendContext).WriteObject(lvSendCMDObject);
                lvSent := true;
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

  finally
    lvContext.UnLockContext('执行登陆', nil);
  end;
end;



procedure CHATExecute(pvCMDObject: TSimpleMsgPack; pvContext:
    TIocpClientContext);
var
  lvCMDIndex:Integer;
begin
  lvCMDIndex := pvCMDObject.ForcePathObject('cmdIndex').AsInteger;
  case lvCMDIndex of
    0: ExecuteHeart(pvCMDObject, pvContext);               // 心跳
    3: ExecuteAllUsers(pvCMDObject, pvContext);            // 所有用户
    5: ExecuteSendMessage(pvCMDObject, pvContext);         // 发送消息
    11: ExecuteLogin(pvCMDObject, pvContext);              // 登陆
  else
    begin
      raise exception.CreateFmt('未知的命令[%d]', [lvCMDIndex]);
    end;
  end;
end;

constructor TCHATSession.Create;
begin
  inherited;
  FData := TSimpleMsgPack.Create;
end;


destructor TCHATSession.Destroy;
begin
   FData.Free;
   inherited;
end;

initialization
  ChatSessions := TSessions.Create(TCHATSession);

finalization
  ChatSessions.Free;

end.
