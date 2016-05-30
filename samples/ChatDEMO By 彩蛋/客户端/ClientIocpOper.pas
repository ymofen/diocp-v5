unit ClientIocpOper;

interface

uses
  Classes, diocp_coder_tcpClient, SimpleMsgPack, uDIOCPStreamCoder;

  /// <summary>
  /// 请求登录
  /// </summary>
  /// <param name="AUserID">账号</param>
  /// <param name="APaw">密码</param>
  procedure CMD_Login(const AUserID, APaw: string);

  /// <summary>
  /// 请求发送消息
  /// </summary>
  /// <param name="AUserID">消息接收人ID(空表示发送给所有人)</param>
  /// <param name="AMsg">消息内容</param>
  procedure CMD_SendMsg(const AUserID, AMsg: string);

  /// <summary>
  /// 请求心跳包
  /// </summary>
  procedure CMD_KeepAlive;

  /// <summary>
  /// 请求用户列表
  /// </summary>
  procedure CMD_UpdataUsers(const AUserID: string);

  /// <summary>
  /// 请求离线消息
  /// </summary>
  /// <param name="AUserID">请求者ID</param>
  procedure CMD_OfflineMessage(const AUserID: string);

  /// <summary>
  /// 初始化客户端使用的对象
  /// </summary>
  procedure IniClientObject;

  /// <summary>
  /// 销毁客户端创建的对象
  /// </summary>
  procedure UnIniClientObject;

var
  CurUserID: string;
  CoderTcpClient: TDiocpCoderTcpClient;
  DiocpContext: TIocpCoderRemoteContext;
  //
implementation

uses SysUtils;

var
  CMDObject: TSimpleMsgPack;
  CMDStream: TMemoryStream;

procedure SendCMDObject(pvCMDObject: TSimpleMsgPack);
var
  lvCMDStream:TMemoryStream;
begin
  lvCMDStream := TMemoryStream.Create;
  try
    pvCMDObject.EncodeToStream(lvCMDStream);  // 加密消息
    DiocpContext.WriteObject(lvCMDStream);
  finally
    lvCMDStream.Free;
  end;
end;

procedure CMD_UpdataUsers(const AUserID: string);
begin
  CMDObject.Clear;
  CMDObject.ForcePathObject('cmdIndex').AsInteger := 3;
  CMDObject.ForcePathObject('requestID').AsString := AUserID;
  CMDObject.ForcePathObject('params.page').AsInteger := 1;
  SendCMDObject(CMDObject);
end;

procedure CMD_OfflineMessage(const AUserID: string);
begin
  CMDObject.Clear;
  CMDObject.ForcePathObject('cmdIndex').AsInteger := 7;
  CMDObject.ForcePathObject('requestID').AsString := AUserID;
  SendCMDObject(CMDObject);
end;

procedure CMD_SendMsg(const AUserID, AMsg: string);
begin
  if AMsg <> '' then
  begin
    CMDObject.Clear;
    CMDObject.ForcePathObject('cmdIndex').AsInteger := 5;
    CMDObject.ForcePathObject('requestID').AsString := 'messageID';
    CMDObject.ForcePathObject('params.userid').AsString := AUserID;
    CMDObject.ForcePathObject('params.msg').AsString := AMsg;
    SendCMDObject(CMDObject);
  end;
end;

procedure CMD_Login(const AUserID, APaw: string);
begin
  // 连接
  CoderTcpClient.open;
  if DiocpContext.Active then Exit;
  DiocpContext.Connect;
  //sfLogger.logMessage('与服务器建立连接成功, 请进行登陆');
  // 上线
  if DiocpContext.Active then  // 连接成功，请求登陆
  begin
    CMDObject.Clear;
    CMDObject.ForcePathObject('cmdIndex').AsInteger := 11;
    CMDObject.ForcePathObject('requestID').AsString := 'login';
    CMDObject.ForcePathObject('user.id').AsString := AUserID;
    CMDObject.ForcePathObject('user.paw').AsString := APaw;
    SendCMDObject(CMDObject);
  end;
end;

procedure CMD_KeepAlive;
begin
  CMDObject.Clear;
  CMDObject.ForcePathObject('cmdIndex').AsInteger := 0;
  SendCMDObject(CMDObject);
end;

procedure IniClientObject;
begin
  CoderTcpClient := TDiocpCoderTcpClient.Create(nil);

  DiocpContext := TIocpCoderRemoteContext(CoderTcpClient.Add);
  DiocpContext.RegisterCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);

  CMDObject := TSimpleMsgPack.Create;
  CMDStream := TMemoryStream.Create;
end;

procedure UnIniClientObject;
begin
  FreeAndNil(CMDObject);
  CoderTcpClient.DisconnectAll;
  CoderTcpClient.Free;
  CMDStream.Free;
end;

initialization
  IniClientObject;

finalization
  UnIniClientObject;

end.
