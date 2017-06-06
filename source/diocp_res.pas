unit diocp_res;

interface



const
  BytePerKB = 1024;
  BytePerMB = BytePerKB * 1024;
  BytePerGB = BytePerMB * 1024;

type
  TWorkDoneCallBack = procedure(pvData:Pointer; pvCode:Integer) of object;


resourcestring
  /// iocpTcpServer 日志
  strRecvZero      = '[%d]接收到0字节的数据,该连接将断开!';
  strRecvError     = '[%d]响应接收请求时出现了错误。错误代码:%d!';
  strRecvEngineOff = '[%d]响应接收请求时发现IOCP服务关闭';
  strRecvPostError = '[%d]投递接收请求时出现了错误。错误代码:%d!'; //'TIocpRecvRequest.PostRequest Error:%d'
  strRecvResponseErr='[%d]-[%d]响应接收请求时发现请求的引用计数器异常:%d,应该为0';

  strSendEngineOff = '[%d]响应发送数据请求时发现IOCP服务关闭';
  strSendSizeErr   = '[%d]响应发送数据请求时请求发送的字节[%d]与成功发送[%d]的字节不一致';
  strSendErr       = '[%d]响应发送数据请求时出现了错误。错误代码:%d!';
  strSendPostError = '[%d]投递发送数据请求时出现了错误。错误代码:%d';
  strSendZero      = '[%d]投递发送请求数据时遇到0长度数据。进行关闭处理';
  strWSACloseRequest      = '处理投递发送请求数据包时,发现异步关闭请求(Request.Tag = -1)。进行关闭处理!';
  strWSACloseRequestEx = '主动断开连接请求!';
  strSendPushFail  = '[%d]投递发送请求数据包超出队列允许的最大长度[%d/%d]。';

  strDoConnectedError  = '[%d]高等级错误:触发DoConnected事件时,发现连接状态(Active)已经为true';    //  on DoConnected event is already actived


  strFuncFail      = '[%d]执行[%s]失败: %s';

  strRequestDisconnectFileID = '请求断开日志';


  strBindingIocpError = '[%d]绑定到IOCP句柄时出现了异常, 错误代码:%d, (%s)';
  strAcceptExError = '[%d]投递AcceptEx时出现了异常, 错误代码:%d, (%s)';

  strPushFail      = '[%d]压入到待发送队列失败, 队列信息: %d/%d';

  strOnRecvBufferException = '[%d]响应OnRecvBuffer时出现了异常:%s。';
  strOnResponseException = '[%d]响应%s时出现了异常:%s。';

  strConnectTimeOut = '建立连接超时(%s:%d)';

  strEngineIsOff = '[%s]IOCP引擎未启动,请确保IOCP引擎开启!';

  strListenFail  = '侦听(%s:%d)时出现异常:%s';

  strHttpServerStateInfo = 'Session对象:%d, 未处理请求队列: %d, 请求对象(num/out/back):%d/%d/%d, 响应内存块池(num*size/put/get):%d*%d/%d/%d';



  //strContextCloseErr = '


  /// =========== iocpTcpServer 状态信息============
  strState_Active      = '服务状态: 开启';
  strState_MonitorNull = '没有创建监控器';
  strState_ObjectNull  = '没有监控对象';    //'iocp server is null'
  strState_Off         = '服务状态: 关闭';
  strRecv_SizeInfo     = '接收数据: %s';
  strSend_SizeInfo     = '发送数据: %s';
  strRecv_PostInfo     = '接收信息: 投递:%d, 回应:%d, 剩余:%d 速度(每秒处理个数):%d';  //post:%d, response:%d, remain:%d
  strSend_Info         = '发送信息: 投递:%d, 回应:%d, 剩余:%d 速度(每秒处理个数):%d';  //post:%d, response:%d, remain:%d
  strSendQueue_Info    = '发送队列: 压入/弹出/完成/终止:%d, %d, %d, %d';//push/pop/complted/abort:%d, %d, %d, %d
  strSendRequest_Info  = '发送对象: 创建:%d, 借出:%d, 还回:%d';  //'create:%d, out:%d, return:%d'
  strAcceptEx_Info     = 'AcceptEx: 投递:%d, 回应:%d';      //'post:%d, response:%d'
  strSocketHandle_Info = '套接字句柄: 创建:%d, 销毁:%d';  //'create:%d, destroy:%d'
  strContext_Info      = '连接对象: 创建:%d, 借出:%d, 还回:%d';  //'create:%d, out:%d, return:%d'
  strOnline_Info       = '在线信息: %d(max.%d)';
  strWorkers_Info      = '工作线程: %d';
  strRunTime_Info      = '运行信息: %s';
  /// =========== 服务端状态信息============

implementation

end.
