diocp5
======


## 快速开始  

	从那里得到:
	 git更新(推荐同步更新)
	  1.https://github.com/ymofen/diocp-v5
	  2.https://gitee.com/ymofen/diocp-v5
	最新版zip下载
	  https://codeload.github.com/ymofen/diocp-v5/zip/master
    
	FAQ文档:
	  http://note.youdao.com/noteshare?id=2b40e5d99f39a57512d739f0a5a8a3ee


	设置Delphi环境变量
	  DIOCP5_HOME=E:\workspace\diocp-v5

	搜索路径
	  $(DIOCP5_HOME)\Source


## FAQ
### Q:同时启动两个Diocp服务, 为什么一个处理逻辑慢, 会影响到另外一个服务也变慢
	A: Diocp底层默认共享同一个IocpEngine, 多个Diocp服务都是由同一个Iocp引擎驱动。可以用下面的办法进行优化
	  1>. 每个DiocpTcpServer可以单独设置IocpEngine. 
	      代码：FTcpServer.BindDiocpEngine(TIocpEngine.Create, true);
	  2>. 加大默认IocpEngine工作线程, IocpEngine默认的工作线程数量:cpu核数* 2-1
	  3>. 如果逻辑代码比较复杂，可以吧逻辑处理投递到另外的线程中执行。
	  4>. Http服务逻辑处理是由diocpTask进行逻辑处理的，diocpTask默认的工作线程数量: cpu * 2 -1，
	      可以引用diocp_task.pas, 在初始化app时进行设定 iocpTaskManager.setWorkerCount(50);
	      可以在APP编译条件中指定QDAC_QWorker, 切换使用qworkers线程池
        
## 注意的一些事情
### HTTP协议
#### 关于ResponseStream, ResponesAFile
如果使用这两个过程响应Http, Diocp会分块发送数据，直达发送所有的数据发送完成，然后进行判断是否需要关闭连接。所以您不需要进行其他的响应(SendResponse), 也不需要执行ResponseEnd，也不能提前把Stream进行释放，Diocp的Http会接管处理他。


## 目录说明:
	samples                        下面是各种DEMO
	source                         目录下面是源代码
  
	可以先从下面的DEMO中了解diocp的工作原理
	  samples\ECHO
	  samples\simple
	  samples\StringDEMO



## 关于文档帮助
	DIOCP QQ 群: 638127021  
	DIOCP官方社区: www.diocp.org


## 关于捐助：

	DIOCP5遵循BSD协议，你可以任意的用于商业项目和自由的项目中而不用通知我，
	如果你觉得DIOCP5对你有帮助而你刚好又想对DIOCP5进行捐助,请联系作者，或者直接进行捐助：

	捐助的支付宝：
	账号：ymofen@diocp.org
	户名: 杨茂丰


   