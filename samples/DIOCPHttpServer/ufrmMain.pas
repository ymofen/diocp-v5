unit ufrmMain;

interface

{$DEFINE JSON}

{$IFDEF JSON}
  {$DEFINE USE_SuperObject}
  {$DEFINE USE_QJSON}
{$ENDIF}



uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ExtCtrls
  {$IFDEF USE_SuperObject}, superobject{$ENDIF}
  , utils.safeLogger, StrUtils,
  ComCtrls, diocp.ex.httpServer, System.Actions
  ;

type
  TfrmMain = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    actlstMain: TActionList;
    actOpen: TAction;
    actStop: TAction;
    btnDisconectAll: TButton;
    pgcMain: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    mmoLog: TMemo;
    pnlMonitor: TPanel;
    btnGetWorkerState: TButton;
    btnFindContext: TButton;
    pnlTop: TPanel;
    tmrHeart: TTimer;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure tmrHeartTimer(Sender: TObject);
  private
    iCounter:Integer;
    FTcpServer: TDiocpHttpServer;
    procedure refreshState;

    procedure OnHttpSvrRequest(pvRequest:TDiocpHttpRequest);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, diocp.core.engine;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TDiocpHttpServer.Create(Self);
  FTcpServer.Name := 'HttpSVR';
  FTcpServer.SetMaxSendingQueueSize(10000);
  FTcpServer.createDataMonitor;
  FTcpServer.OnDiocpHttpRequest := OnHttpSvrRequest;
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
  
  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;
end;

procedure TfrmMain.OnHttpSvrRequest(pvRequest:TDiocpHttpRequest);
var
  lvJSon:ISuperObject;
  s:String;
  lvRawData:AnsiString;
begin
  // Context Type                        返回的是UTF-8的编码
  pvRequest.Response.ContentType := 'text/html; charset=utf-8';

  // 解码Post数据参数
  {$IFDEF UNICODE}
  pvRequest.DecodePostDataParam(nil);
  pvRequest.DecodeURLParam(nil);
  {$ELSE}
  pvRequest.DecodePostDataParam(false);
  pvRequest.DecodeURLParam(false);
  {$ENDIF}

  // 输出客户端IP信息
  pvRequest.Response.WriteString(Format('<div>ip:%s:%d</div><br>', [pvRequest.Connection.RemoteAddr,
    pvRequest.Connection.RemotePort]));

  pvRequest.Response.WriteString('请求方法:' + pvRequest.RequestMethod);
  pvRequest.Response.WriteString('<br>');
  pvRequest.Response.WriteString('=======================================<br>');

  if pvRequest.RequestURI = '/diocp-v5' then
  begin  // 输出diocp运行信息
    pvRequest.Response.WriteString('DIOCP运行信息<br>');
    s := FTcpServer.GetStateInfo;
    s := ReplaceText(s, sLineBreak, '<br>');
    pvRequest.Response.WriteString(s);
    pvRequest.Response.WriteString('<br>');

    pvRequest.Response.WriteString('IOCP线程信息<br>');
    s := FTcpServer.IocpEngine.GetStateINfo;
    s := ReplaceText(s, sLineBreak, '<br>');
    pvRequest.Response.WriteString(s);
  end else if pvRequest.RequestURI = '/redirect' then
  begin                                       //重新定向
    s := pvRequest.GetRequestParam('url');
    if s = '' then
    begin
      pvRequest.Response.WriteString('重定向例子:<a href="/redirect?url=http://www.diocp.org">' +
       Format('http://%s:%s/redirect?url=http://www.diocp.org',[pvRequest.RequestHostName, pvRequest.RequestHostPort]) + '</a>');
    end else
    begin
      pvRequest.Response.RedirectURL(s);
      pvRequest.CloseContext;
      Exit;
    end;
  end else if pvRequest.RequestURI = '/input' then
  begin  // 输出diocp运行信息
    pvRequest.Response.WriteString('DIOCP HTTP 表单提交测试<br>');
    pvRequest.Response.WriteString('<form id="form1" name="form1" method="post" action="/post?param1=''汉字''&time=' + DateTimeToStr(Now()) +'">');
    pvRequest.Response.WriteString('<table width="50%" border="1" align="center">');
    pvRequest.Response.WriteString('<tr><td width="35%">请输入你的名字:</td>');
    pvRequest.Response.WriteString('<td width="35%"><input name="a" type="text" value="DIOCP-V5" /></td></tr>');
    pvRequest.Response.WriteString('<tr><td width="35%">请输入你的爱好:</td>');
    pvRequest.Response.WriteString('<td width="35%"><input name="b" type="text" value="LOL英雄联盟" /></td></tr>');
    pvRequest.Response.WriteString('<tr><td width="35%">操作:</td>');
    pvRequest.Response.WriteString('<td width="35%"><input type="submit" name="Submit" value="提交"/></td></tr>');
    pvRequest.Response.WriteString('</table></form>');
  end else
  begin

    // 回写数据
    pvRequest.Response.WriteString('北京时间:' + DateTimeToStr(Now()) + '<br>');
    pvRequest.Response.WriteString('<a href="http://www.diocp.org">DIOCP/MyBean官方社区</a><br>');
    pvRequest.Response.WriteString('<a href="/diocp-v5">查看diocp运行信息</a><br>');
    pvRequest.Response.WriteString('<a href="/input">表单提交测试</a><br>');
    pvRequest.Response.WriteString('<a href="/redirect">重新定向</a><br>');
    pvRequest.Response.WriteString('<br>');

    pvRequest.Response.WriteString('<div>');

    // 获取头信息
    s := pvRequest.RequestHeader.Text;
    s := ReplaceText(s, sLineBreak, '<br>');
    pvRequest.Response.WriteString('头信息<br>');
    pvRequest.Response.WriteString('请求Url:' + pvRequest.RequestUrl + '<br>');


    pvRequest.Response.WriteString(s);
    pvRequest.Response.WriteString('<br>');
    pvRequest.Response.WriteString('参数信息<br>');
    pvRequest.Response.WriteString('=======================================<br>');
    pvRequest.Response.WriteString('  关于URL的编码问题说明<br>');
    pvRequest.Response.WriteString('URI, IE和其他浏览器进行了URLEncode编码和UTF8编码，所以后台进行了统一处理<br>');
    pvRequest.Response.WriteString('URL中的参数, IE为进行任何编码, 其他浏览器(FireFox和360极速浏览器)进行了URLEncode编码和UTF8编码<br>');
    pvRequest.Response.WriteString('   所以后台参数中只进行了URLDecode的解码，需要开发时进行去单独处理<br>');
    pvRequest.Response.WriteString('*****************************************<br>');


    pvRequest.Response.WriteString(Format('原始URL数据:%s<br>', [pvRequest.RequestRawURL]));
    pvRequest.Response.WriteString(Format('原始数据长度:%d<br>', [pvRequest.RawPostData.Size]));
    pvRequest.Response.WriteString(Format('context-length:%d<br>', [pvRequest.ContextLength]));

    SetLength(lvRawData,pvRequest.RawPostData.Size);
    pvRequest.RawPostData.Position := 0;
    pvRequest.RawPostData.Read(PByte(lvRawData)^, pvRequest.RawPostData.Size);
    pvRequest.Response.WriteString('原始数据:');
    pvRequest.Response.WriteString(lvRawData);
    pvRequest.Response.WriteString('<br>=======================================<br>');

    pvRequest.Response.WriteString('<br>');
    pvRequest.Response.WriteString(Format('解码参数信息(参数数量:%d)<br>', [pvRequest.RequestParamsList.Count]));
    pvRequest.Response.WriteString(pvRequest.RequestParamsList.Text);

    if pvRequest.RequestParamsList.Count > 0 then
    begin
      pvRequest.Response.WriteString('<br>第一个参数:' + pvRequest.GetRequestParam(pvRequest.RequestParamsList.Names[0]));
    end;
    pvRequest.Response.WriteString('<br>获取a参数的值:' + pvRequest.GetRequestParam('a'));

    pvRequest.Response.WriteString('<br>');
    pvRequest.Response.WriteString('=======================================<br>'); 


    // 返回json
    lvJSon := SO();
    lvJSon.S['title'] := 'DIOCP3 Http 服务演示';
    lvJSon.S['author'] := 'D10.天地弦';
    lvJSon.S['date'] := DateTimeToStr(Now());
    s := lvJSon.AsJSon(True, False);
    s := ReplaceText(s, sLineBreak, '<br>');
    pvRequest.Response.WriteString(s);
    pvRequest.Response.WriteString('</div>');
  end;

  // 应答完毕，发送会客户端
  pvRequest.ResponseEnd;

  pvRequest.CloseContext;
end;

destructor TfrmMain.Destroy;
begin
  FTcpServer.SafeStop();
  inherited Destroy;
end;

procedure TfrmMain.refreshState;
begin
  if FTcpServer.Active then
  begin
    btnOpen.Action := actStop;
  end else
  begin
    btnOpen.Action := actOpen;
  end;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  FTcpServer.Port := StrToInt(edtPort.Text);
  FTcpServer.Active := true;
  refreshState;
  tmrHeart.Enabled := true;
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

procedure TfrmMain.btnDisconectAllClick(Sender: TObject);
begin
  FTcpServer.DisConnectAll();
end;

procedure TfrmMain.btnFindContextClick(Sender: TObject);
var
  lvList:TList;
  i:Integer;
begin
  lvList := TList.Create;
  try
    FTcpServer.getOnlineContextList(lvList);
    for i:=0 to lvList.Count -1 do
    begin
      FTcpServer.findContext(TDiocpHttpClientContext(lvList[i]).SocketHandle);
    end;
  finally
    lvList.Free;
  end;

end;

procedure TfrmMain.btnGetWorkerStateClick(Sender: TObject);
begin
  //ShowMessage(FTcpServer.IocpEngine.getWorkerStateInfo(0));

end;

procedure TfrmMain.tmrHeartTimer(Sender: TObject);
begin
  FTcpServer.KickOut();
end;



end.
