unit ufrmMain;

interface

{$DEFINE JSON}

{$IFDEF JSON}
  {$DEFINE USE_QJSON}
{$ENDIF}



uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ExtCtrls
  , utils_safeLogger, StrUtils,
  ComCtrls, diocp_ex_httpServer, diocp_ex_http_common, utils_byteTools,
  utils_dvalue_json, System.Actions;

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
    tsLog: TTabSheet;
    mmoLog: TMemo;
    pnlMonitor: TPanel;
    btnGetWorkerState: TButton;
    btnFindContext: TButton;
    pnlTop: TPanel;
    tmrHeart: TTimer;
    tsTester: TTabSheet;
    tsURLCode: TTabSheet;
    mmoURLInput: TMemo;
    mmoURLOutput: TMemo;
    btnURLDecode: TButton;
    btnURLEncode: TButton;
    btnCompress: TButton;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnCompressClick(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnURLDecodeClick(Sender: TObject);
    procedure btnURLEncodeClick(Sender: TObject);
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
  uFMMonitor, diocp_core_engine, utils_strings, diocp.ex.SimpleMsgPackSession,
  utils_dvalue;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TDiocpHttpServer.Create(Self);
  FTcpServer.Name := 'HttpSVR';
  FTcpServer.SetMaxSendingQueueSize(10000);
  FTcpServer.createDataMonitor;
  FTcpServer.OnDiocpHttpRequest := OnHttpSvrRequest;
  FTcpServer.RegisterSessionClass(TDiocpSimpleMsgPackSession);
  FTcpServer.WorkerCount := 5;
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
  
  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;
end;

procedure TfrmMain.OnHttpSvrRequest(pvRequest:TDiocpHttpRequest);
var
  s:String;
  lvRawData:AnsiString;
  lvSession:TDiocpSimpleMsgPackSession;
  procedure WriteLoginForm();
  begin
    pvRequest.Response.WriteString('你还没有进行登陆，登陆成功后可以看到更多的演示(本功能借助服务端Session完成)<br>');
    pvRequest.Response.WriteString('<a href="/login">点击进行登陆</a><br>');
  end;

  procedure WriteLogOutUrl();
  begin
    pvRequest.Response.WriteString('<a href="/logout">点击进行注销登陆</a><br>');
  end;

  procedure WriteNormalPage();
  begin     
    if pvRequest.GetCookie('diocp_cookie') = '' then
    begin
      pvRequest.Response.AddCookie('diocp_cookie', '这是一个diocp的cookie演示,Cookie会传递到客户端去');
    end else
    begin
      pvRequest.Response.WriteString('客户端Cookie读取演示:' + pvRequest.GetCookie('diocp_cookie'));
    end;

    // 回写数据
    pvRequest.Response.WriteString('北京时间:' + DateTimeToStr(Now()) + '<br>');
    pvRequest.Response.WriteString('<a href="http://www.diocp.org">DIOCP/MyBean官方社区</a><br>');
    pvRequest.Response.WriteString('<a href="/diocp-v5">查看diocp运行信息</a><br>');
    pvRequest.Response.WriteString('<a href="/input">表单提交测试</a><br>');
    pvRequest.Response.WriteString('<a href="/redirect">重新定向</a><br>');
    pvRequest.Response.WriteString('<a href="/json">请求Json数据</a><br>');

    pvRequest.Response.WriteString('<br>');

    pvRequest.Response.WriteString('<div>');

    // 获取头信息
    s := pvRequest.RequestRawHeaderString;
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
    pvRequest.Response.WriteString(Format('原始数据长度:%d<br>', [pvRequest.ContentDataLength]));
    pvRequest.Response.WriteString(Format('context-length:%d<br>', [pvRequest.ContextLength]));


    lvRawData := pvRequest.ContentAsString;
    pvRequest.Response.WriteString('原始数据:');
    pvRequest.Response.WriteString(lvRawData);
    pvRequest.Response.WriteString('<br>=======================================<br>');

    pvRequest.Response.WriteString('<br>');
    pvRequest.Response.WriteString(Format('解码参数信息(参数数量:%d)<br>', [pvRequest.RequestParamsList.Count]));

    lvRawData :=pvRequest.RequestParamsList.ToStrings();
    pvRequest.Response.WriteString(lvRawData);
    pvRequest.Response.WriteString('<br>');
//
    if pvRequest.RequestParamsList.Count > 0 then
    begin
      pvRequest.Response.WriteString('<br>第一个参数:' +
        pvRequest.GetRequestParam(pvRequest.RequestParamsList[0].Name.AsString));
    end;
    pvRequest.Response.WriteString('<br>获取b参数的原值:' +pvRequest.GetRequestParam('b'));
    //pvRequest.Response.WriteString('<br>获取b参数的Utf8解码:' +Utf8Decode(pvRequest.GetRequestParam('b')));

    pvRequest.Response.WriteString('<br>');
    pvRequest.Response.WriteString('=======================================<br>');



    pvRequest.Response.WriteString('</div>');
  end;

var
  lvBytes:TBytes;
  lvDValue:TDValue;

begin
  if pvRequest.RequestURI = '/json' then
  begin
    pvRequest.Response.ContentType := 'text/json';
    lvDValue := TDValue.Create;
    lvDValue.ForceByName('title').AsString := 'DIOCP-V5 Http 服务演示';
    lvDValue.ForceByName('author').AsString := 'D10.天地弦';
    lvDValue.ForceByName('time').AsString := DateTimeToStr(Now());
    s := JSONEncode(lvDValue);
    lvDValue.Free;
    pvRequest.Response.WriteString(s);
    pvRequest.ResponseEnd;
    Exit;
  end;

  if pvRequest.RequestURI = '/CHUNKED' then
  begin
    // Context Type                        返回的是UTF-8的编码
    pvRequest.Response.ContentType := 'text/html; charset=utf-8';
    pvRequest.Response.SetChunkedStart;
    pvRequest.Response.SetChunkedUtf8('Chunked编码测试1<BR>');
    pvRequest.Response.SetChunkedUtf8('================================<BR>');
    pvRequest.Response.ChunkedFlush;
    
    pvRequest.Response.SetChunkedUtf8('Chunked编码测试2<BR>');
    pvRequest.Response.SetChunkedUtf8('================================<BR>');
    pvRequest.Response.SetChunkedEnd;
    pvRequest.Response.ChunkedFlush;
    pvRequest.CloseContext;

    Exit;
  end;

//  if pvRequest.RequestURI = '/favicon.ico' then
//  begin
//    // Context Type                        返回的是UTF-8的编码
//    pvRequest.Response.ContentType := 'application/oct stream;
//    pvRequest.Response.SetChunkedStart;
//    pvRequest.Response.SetChunkedUtf8('Chunked编码测试1<BR>');
//    pvRequest.Response.SetChunkedUtf8('================================<BR>');
//    pvRequest.Response.ChunkedFlush;
//
//    pvRequest.Response.SetChunkedUtf8('Chunked编码测试2<BR>');
//    pvRequest.Response.SetChunkedUtf8('================================<BR>');
//    pvRequest.Response.SetChunkedEnd;
//    pvRequest.Response.ChunkedFlush;
//    pvRequest.CloseContext;
//
//    Exit;
//  end;

  // Context Type                        返回的是UTF-8的编码
  pvRequest.Response.ContentType := 'text/html; charset=utf-8';

  // 解码Post数据参数
  {$IFDEF UNICODE}
  pvRequest.DecodePostDataParam(nil);
  pvRequest.DecodeURLParam(nil);
  {$ELSE}
  // 使用UTF8方式解码
  pvRequest.DecodePostDataParam(True);
  pvRequest.DecodeURLParam(True);
  {$ENDIF}



//  SetLength(lvBytes, pvRequest.RawPostData.Size);
//  pvRequest.RawPostData.Position := 0;
//  pvRequest.RawPostData.Read(lvBytes[0], pvRequest.RawPostData.Size);
//  s :=  TEncoding.UTF8.GetString(lvBytes);
//  sfLogger.logMessage(s);


  //sfLogger.logMessage(UTF8Decode(pvRequest.GetRequestParam('param')));

  // 输出客户端IP信息
  pvRequest.Response.WriteString(Format('<div>ip:%s:%d</div><br>', [pvRequest.Connection.RemoteAddr,
    pvRequest.Connection.RemotePort]));

  pvRequest.Response.WriteString('请求方法:' + pvRequest.RequestMethod);
  pvRequest.Response.WriteString('<br>');
  WriteLogOutUrl();
  pvRequest.Response.WriteString('=======================================<br>');


  lvSession := TDiocpSimpleMsgPackSession(pvRequest.GetSession);
  if pvRequest.RequestURI = '/login' then
  begin
    lvSession.MsgPack.B['login'] := true;
    pvRequest.Response.WriteString('登陆成功<br>');
    WriteNormalPage();
  end else if (not lvSession.MsgPack.B['login']) then
  begin
    WriteLoginForm();
  end else if pvRequest.RequestURI = '/diocp-v5' then
  begin  // 输出diocp运行信息
    Sleep(1000);
    pvRequest.Response.WriteString('DIOCP运行信息<br>');
    s := FTcpServer.GetStateInfo;
    s := ReplaceText(s, sLineBreak, '<br>');
    pvRequest.Response.WriteString(s);
    pvRequest.Response.WriteString('<br>');

    pvRequest.Response.WriteString('IOCP线程信息<br>');
    s := FTcpServer.IocpEngine.GetStateINfo;
    s := ReplaceText(s, sLineBreak, '<br>');
    pvRequest.Response.WriteString(s);
  end else if pvRequest.RequestURI = '/logout' then
  begin
    lvSession.MsgPack.B['login'] := false;
    WriteLoginForm();
  end else if pvRequest.RequestURI = '/redirect' then
  begin                                       //重新定向
    s := pvRequest.GetRequestParam('url');
    if s = '' then
    begin
      pvRequest.Response.WriteString('重定向例子:<a href="/redirect?url=http://www.diocp.org">' +
       '/redirect?url=http://www.diocp.org</a>');
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
    WriteNormalPage();
  end;


//  if Pos('deflate',pvRequest.Header.GetValueByName('Accept-Encoding', '')) >= 0 then
//  begin
//    pvRequest.Response.Header.ForceByName('Content-Encoding').AsString := 'deflate';
//    pvRequest.Response.DeflateCompressContent;
//  end else if Pos('gzip', pvRequest.Header.GetValueByName('Accept-Encoding', '')) >= 0 then
//  begin
//    pvRequest.Response.Header.ForceByName('Content-Encoding').AsString := 'gzip';
//    pvRequest.Response.GZipContent;
//  end;

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

procedure TfrmMain.btnCompressClick(Sender: TObject);
var
  lvBuilder:TDBufferBuilder;
begin
  lvBuilder := TDBufferBuilder.Create;
  lvBuilder.Clear;
  lvBuilder.AppendUtf8('0000');
  GZCompressBufferBuilder(lvBuilder);
  sfLogger.logMessage(TByteTools.varToHexString(lvBuilder.Memory^, lvBuilder.Length));

  lvBuilder.Clear;
  lvBuilder.AppendUtf8('0000');
  DeflateCompressBufferBuilder(lvBuilder);
  sfLogger.logMessage(TByteTools.varToHexString(lvBuilder.Memory^, lvBuilder.Length));

  lvBuilder.Clear;
  lvBuilder.AppendUtf8('111');
  GZCompressBufferBuilder(lvBuilder);
  sfLogger.logMessage(TByteTools.varToHexString(lvBuilder.Memory^, lvBuilder.Length));

  lvBuilder.Clear;
  lvBuilder.AppendUtf8('111');
  DeflateCompressBufferBuilder(lvBuilder);
  sfLogger.logMessage(TByteTools.varToHexString(lvBuilder.Memory^, lvBuilder.Length));

  //GZDecompressBufferBuilder(lvBuilder);
  //ShowMessage(lvBuilder.ToRAWString);
  lvBuilder.Free;

  pgcMain.ActivePage := tsLog;

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

procedure TfrmMain.btnURLDecodeClick(Sender: TObject);
begin
  mmoURLOutput.Lines.Text := diocp_ex_http_common.URLDecode(mmoURLInput.Lines.Text);
end;

procedure TfrmMain.btnURLEncodeClick(Sender: TObject);
begin
 // showMessage(Format('%d', [Low(String)]));
  mmoURLOutput.Lines.Text := diocp_ex_http_common.URLEncode(mmoURLInput.Lines.Text);
end;

procedure TfrmMain.tmrHeartTimer(Sender: TObject);
begin
  FTcpServer.KickOut();
end;



end.
