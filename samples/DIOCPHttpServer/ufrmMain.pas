unit ufrmMain;

interface

{$DEFINE JSON}

{$IFDEF JSON}
  {$DEFINE USE_QJSON}
{$ENDIF}

{$I 'diocp.inc'}


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ExtCtrls
  , utils_safeLogger, StrUtils, 
  ComCtrls, diocp_ex_httpServer, diocp_ex_http_common, utils_byteTools,
  utils_dvalue_json, utils_BufferPool, diocp_tcp_server, uRunTimeINfoTools,
  diocp_task;

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
    btnInfo: TButton;
    mmoLog: TMemo;
    chkUseSession: TCheckBox;
    chkUsePool: TCheckBox;
    chkIPV6: TCheckBox;
    chkRecord2File: TCheckBox;
    tsWebSocket: TTabSheet;
    mmoWebSocketData: TMemo;
    btnWebSocketPush: TButton;
    tmrWebSocketPing: TTimer;
    pnlHTTPInfo: TPanel;
    lblHttpInfo: TLabel;
    tmrInfoRefresh: TTimer;
    edtWorkCount: TEdit;
    chkWebSocketEcho: TCheckBox;
    btnParseRange: TButton;
    chkAccessControl: TCheckBox;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnCompressClick(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnParseRangeClick(Sender: TObject);
    procedure btnURLDecodeClick(Sender: TObject);
    procedure btnURLEncodeClick(Sender: TObject);
    procedure btnWebSocketPushClick(Sender: TObject);
    procedure chkUseSessionClick(Sender: TObject);
    procedure tmrHeartTimer(Sender: TObject);
    procedure tmrInfoRefreshTimer(Sender: TObject);
    procedure tmrWebSocketPingTimer(Sender: TObject);
  private
    iCounter:Integer;
    FChkSession:Boolean;
    FTcpServer: TDiocpHttpServer;
    function DoLoadFile(pvRequest:TDiocpHttpRequest): Boolean;
    procedure refreshState;

    procedure OnHttpSvrRequest(pvRequest:TDiocpHttpRequest);

    procedure OnHttpSvrTestRequest(pvRequest:TDiocpHttpRequest);

    function WebSocketPush(const pvData: string; pvExceptContext:
        TDiocpHttpClientContext): Integer;

    function GetWebSocketCounter(pvExceptContext:TDiocpHttpClientContext): Integer;

    procedure OnResposeStreamDone(pvData:Pointer; pvCode:Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, diocp_core_engine, utils_strings, 
  utils_dvalue;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TDiocpHttpServer.Create(nil);
  FTcpServer.Name := 'HttpSVR';
  FTcpServer.SetMaxSendingQueueSize(10000);
  FTcpServer.createDataMonitor;
  FTcpServer.OnDiocpHttpRequest := OnHttpSvrRequest;
  FTcpServer.WorkerCount := 5;
  CheckCreateRequestPoolForTask(20000);
  //FTcpServer.UseAsyncRecvQueue := true;
  //StartDiocpLogicWorker(0);
{$IFDEF DIOCP_HIGH_SPEED}
{$ELSE}
  FTcpServer.DisableSession := true;
{$ENDIF}
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
  
  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;
  //sfLogger.LogFilter := [lgvError, lgvWarning];

{$if CompilerVersion>= 18}
  //ShowMessage('Hello');
{$ifend}

{$IFDEF USE_ZLIBExGZ}
  //HELLO;
{$ENDIF}
end;


procedure TfrmMain.OnHttpSvrRequest(pvRequest:TDiocpHttpRequest);
var
  s:String;
  lvRawData:AnsiString;
  lvSession:TDiocpHttpDValueSession;
  procedure WriteLoginForm();
  begin
    pvRequest.Response.WriteString('�㻹û�н��е�½����½�ɹ�����Կ����������ʾ(�����ܽ��������Session���)<br>');
    pvRequest.Response.WriteString('<a href="/login">������е�½</a><br>');
  end;

  procedure WriteLogOutUrl();
  begin
    pvRequest.Response.WriteString('<a href="/logout">�������ע����½</a><br>');
  end;

  procedure WriteNormalPage();
  begin     
    if pvRequest.GetCookie('diocp_cookie') = '' then
    begin
      pvRequest.Response.AddCookie('diocp_cookie', '����һ��diocp��cookie��ʾ,Cookie�ᴫ�ݵ��ͻ���ȥ');
    end else
    begin
      pvRequest.Response.WriteString('�ͻ���Cookie��ȡ��ʾ:' + pvRequest.GetCookie('diocp_cookie'));
    end;

    // ��д����
    pvRequest.Response.WriteString('����ʱ��:' + DateTimeToStr(Now()) + '<br>');
    pvRequest.Response.WriteString('<a href="http://www.diocp.org">DIOCP/MyBean�ٷ�����</a><br>');
    pvRequest.Response.WriteString('<a href="/diocp-v5">�鿴diocp������Ϣ</a><br>');
    pvRequest.Response.WriteString('<a href="/input">���ύ����</a><br>');
    pvRequest.Response.WriteString('<a href="/redirect">���¶���</a><br>');
    pvRequest.Response.WriteString('<a href="/json">����Json����</a><br>');

    pvRequest.Response.WriteString('<br>');

    pvRequest.Response.WriteString('<div>');

    // ��ȡͷ��Ϣ
    s := pvRequest.RequestRawHeaderString;

    s := StringReplace(s, sLineBreak, '<br>', [rfReplaceAll]);
    pvRequest.Response.WriteString('ͷ��Ϣ<br>');
    pvRequest.Response.WriteString('����Url:' + pvRequest.RequestUrl + '<br>');


    pvRequest.Response.WriteString(s);
    pvRequest.Response.WriteString('<br>');
    pvRequest.Response.WriteString('������Ϣ<br>');
    pvRequest.Response.WriteString('=======================================<br>');
    pvRequest.Response.WriteString('  ����URL�ı�������˵��<br>');
    pvRequest.Response.WriteString('URI, IE�����������������URLEncode�����UTF8���룬���Ժ�̨������ͳһ����<br>');
    pvRequest.Response.WriteString('URL�еĲ���, IEδ�����κα���, ���������(FireFox��360���������)������URLEncode�����UTF8����<br>');
    pvRequest.Response.WriteString('   ���Ժ�̨������ֻ������URLDecode�Ľ��룬��Ҫ����ʱ����ȥ��������<br>');
    pvRequest.Response.WriteString('*****************************************<br>');


    pvRequest.Response.WriteString(Format('ԭʼURL����:%s<br>', [pvRequest.RequestRawURL]));
    pvRequest.Response.WriteString(Format('ԭʼ���ݳ���:%d<br>', [pvRequest.ContentDataLength]));
    pvRequest.Response.WriteString(Format('content-length:%d<br>', [pvRequest.ContentLength]));


    lvRawData := pvRequest.ContentAsString;
    pvRequest.Response.WriteString('ԭʼ����:');
    pvRequest.Response.WriteString(lvRawData);
    pvRequest.Response.WriteString('<br>=======================================<br>');

    pvRequest.Response.WriteString('<br>');
    pvRequest.Response.WriteString(Format('���������Ϣ(��������:%d)<br>', [pvRequest.RequestParamsList.Count]));

    lvRawData :=pvRequest.RequestParamsList.ToStrings();
    pvRequest.Response.WriteString(lvRawData);
    pvRequest.Response.WriteString('<br>');
//
    if pvRequest.RequestParamsList.Count > 0 then
    begin
      pvRequest.Response.WriteString('<br>��һ������:' +
        pvRequest.GetRequestParam(pvRequest.RequestParamsList[0].Name.AsString));
    end;
    pvRequest.Response.WriteString('<br>��ȡb������ԭֵ:' +pvRequest.GetRequestParam('b'));
    //pvRequest.Response.WriteString('<br>��ȡb������Utf8����:' +Utf8Decode(pvRequest.GetRequestParam('b')));

    pvRequest.Response.WriteString('<br>');
    pvRequest.Response.WriteString('=======================================<br>');



    pvRequest.Response.WriteString('</div>');
  end;

var
  lvBytes:TBytes;
  lvDValue:TDValue;
  lvUpgrade, lvNowString :String;
  n:Integer;
begin
  //Randomize;
  //Sleep(Random(2000));
  if pvRequest.RequestURI = '/hello' then
  begin
    pvRequest.Response.ResponseCode := 200;
    pvRequest.Response.WriteString('hello world');
    pvRequest.SendResponse();
    pvRequest.DoResponseEnd;
    Exit;
  end;
  try
    if chkRecord2File.Checked then
    begin
      lvNowString := FormatDateTime('MMddhhnnsszzz', Now);
      pvRequest.SaveToFile(Format('DiocpHttpRequest_%d_%s.req', [pvRequest.Connection.SocketHandle, lvNowString]));
    end;




    if pvRequest.CheckIsWebSocketRequest then
    begin    // ����Ƿ�ΪWebSocket�Ľ�������

      // ��ӦWebSocket����
      pvRequest.ResponseForWebSocketShake;

      // ��������ΪWebSocket����
      pvRequest.Connection.ContextType := Context_Type_WebSocket;

      s := Format('��ӭ����WebSocketЭ��%s�������Ѿ�����:%s, ��ǰ�����û�:%d%s  <a href="http://www.diocp.org" target="_bank">�����Ի�����DIOCP�ṩ</a>',
        ['<br>', TRunTimeINfoTools.GetRunTimeINfo, self.GetWebSocketCounter(nil), '<br>']);

      pvRequest.Connection.PostWebSocketData(s, true);
      Exit;
    end;

    if pvRequest.Connection.ContextType = Context_Type_WebSocket then
    begin   // �������ΪWebSocket����
      // ���յ���WebSocket������
  //    s := TByteTools.varToHexString(pvRequest.InnerWebSocketFrame.Buffer.Memory^, pvRequest.InnerWebSocketFrame.Buffer.Length);
  //    sfLogger.logMessage(s);

      try
        s := pvRequest.WebSocketContentBuffer.DecodeUTF8;

        // ��ȡ�ַ�������
        s := Format('����:%s:%d����Ϣ:%s',
            [pvRequest.Connection.RemoteAddr,
            pvRequest.Connection.RemotePort,
            s]);
      except
        on e:Exception do
        begin
          s := '����ʧ��:' + e.Message;
          // �����ַ������ͻ���
          pvRequest.Connection.PostWebSocketData(s, true);
          Exit;
        end;
      end;


      if chkWebSocketEcho.Checked then
      begin
        // �����ַ������ͻ���
        pvRequest.Connection.PostWebSocketData(s, true);
      end else
      begin 

        if Length(s) > 1024 then
        begin
          s := Format('������Ϣ(%d)����1024,�������Ѿ��յ���,���ǲ�����ת����', [n]);

          // �����ַ������ͻ���
          pvRequest.Connection.PostWebSocketData(s, true);
        end else
        begin
          //sfLogger.logMessage(s);

          n := WebSocketPush(s, pvRequest.Connection);

          s := Format('������Ϣ�Ѿ��㲥��%d���ն�', [n]);

          // �����ַ������ͻ���
          pvRequest.Connection.PostWebSocketData(s, true);
        end;
      end;
      Exit;
    end;

    if chkAccessControl.Checked then
    begin       // �������
      pvRequest.Response.Header.ForceByName('Access-Control-Allow-Origin').AsString := '*';
      pvRequest.Response.Header.ForceByName('Access-Control-Allow-Methods').AsString := 'POST, GET, OPTIONS, DELETE';
      pvRequest.Response.Header.ForceByName('Access-Control-Allow-Headers').AsString := 'x-requested-with,content-type';
    end;
  
    if DoLoadFile(pvRequest) then Exit;
  
    if pvRequest.RequestURI = '/weixin' then
    begin
      //pvRequest.DecodeURLParam(nil);
      s := pvRequest.GetRequestParam('echostr');
      pvRequest.Response.WriteString(s);
      pvRequest.SendResponse;
      pvRequest.DoResponseEnd;
      Exit;
    end;
                      


    if pvRequest.RequestURI = '/json' then
    begin
      pvRequest.Response.ContentType := 'text/json';
      //s := '{"ab":"1111111111111111111111111111111111111111111111111111111111111111111"}';
      lvDValue := TDValue.Create;
      lvDValue.ForceByName('title').AsString := 'DIOCP-V5 Http ������ʾ';
      lvDValue.ForceByName('author').AsString := 'D10.�����';
      lvDValue.ForceByName('time').AsString := DateTimeToStr(Now());
      s := JSONEncode(lvDValue);
      lvDValue.Free;
      pvRequest.Response.WriteString(s);
      pvRequest.SendResponse;
      pvRequest.DoResponseEnd;
      Exit;
    end;

    if pvRequest.RequestURI = '/CHUNKED' then
    begin
      // Context Type                        ���ص���UTF-8�ı���
      pvRequest.Response.ContentType := 'text/html; charset=utf-8';
      pvRequest.Response.SetChunkedStart;
      pvRequest.Response.SetChunkedUtf8('Chunked�������1<BR>');
      pvRequest.Response.SetChunkedUtf8('================================<BR>');
      pvRequest.Response.ChunkedFlush;
    
      pvRequest.Response.SetChunkedUtf8('Chunked�������2<BR>');
      pvRequest.Response.SetChunkedUtf8('================================<BR>');
      pvRequest.Response.SetChunkedEnd;
      pvRequest.Response.ChunkedFlush;
      pvRequest.CloseContext;

      Exit;
    end;

  //  if pvRequest.RequestURI = '/favicon.ico' then
  //  begin
  //    // Context Type                        
  //    pvRequest.Response.ContentType := 'application/oct stream;
  //    pvRequest.Response.SetChunkedStart;
  //    pvRequest.Response.SetChunkedUtf8('Chunked�������1<BR>');
  //    pvRequest.Response.SetChunkedUtf8('================================<BR>');
  //    pvRequest.Response.ChunkedFlush;
  //
  //    pvRequest.Response.SetChunkedUtf8('Chunked�������2<BR>');
  //    pvRequest.Response.SetChunkedUtf8('================================<BR>');
  //    pvRequest.Response.SetChunkedEnd;
  //    pvRequest.Response.ChunkedFlush;
  //    pvRequest.CloseContext;
  //
  //    Exit;
  //  end;

    // Context Type                        ���ص���UTF-8�ı���
    pvRequest.Response.ContentType := 'text/html; charset=utf-8';

    // ����Post���ݲ���
    {$IFDEF UNICODE}
    pvRequest.DecodePostDataParam(nil);
    pvRequest.DecodeURLParam(nil);
    {$ELSE}
    // ʹ��UTF8��ʽ����
    pvRequest.DecodePostDataParam(True);
    pvRequest.DecodeURLParam(True);
    {$ENDIF}


    // ����ͻ���IP��Ϣ
    pvRequest.Response.WriteString(Format('<div>ip:%s:%d</div><br>', [pvRequest.Connection.RemoteAddr,
      pvRequest.Connection.RemotePort]));

    pvRequest.Response.WriteString('���󷽷�:' + pvRequest.RequestMethod);
    pvRequest.Response.WriteString('<br>');
    WriteLogOutUrl();
    pvRequest.Response.WriteString('=======================================<br>');

    lvSession := nil;
    if FChkSession then
    begin 
      lvSession :=TDiocpHttpDValueSession(pvRequest.GetSession);
      if pvRequest.RequestURI = '/login' then
      begin
        lvSession.DValues.ForceByName('login').AsBoolean := true;
        pvRequest.Response.WriteString('��½�ɹ�<br>');
        WriteNormalPage();
      end else if (not lvSession.DValues.ForceByName('login').AsBoolean) then
      begin
        WriteLoginForm();
      end;
    end;
    if pvRequest.RequestURI = '/diocp-v5' then
    begin  // ���diocp������Ϣ
      Sleep(1000);
      pvRequest.Response.WriteString('DIOCP������Ϣ<br>');
      s := FTcpServer.GetStateInfo;
      s := StringReplace(s, sLineBreak, '<br>', [rfReplaceAll]);
      pvRequest.Response.WriteString(s);
      pvRequest.Response.WriteString('<br>');

      pvRequest.Response.WriteString('IOCP�߳���Ϣ<br>');
      s := FTcpServer.IocpEngine.GetStateINfo;
      s := StringReplace(s, sLineBreak, '<br>', [rfReplaceAll]);
      pvRequest.Response.WriteString(s);
    end else if pvRequest.RequestURI = '/logout' then
    begin
      if lvSession <> nil then
      begin
        lvSession.DValues.ForceByName('login').AsBoolean := false;
      end;
      WriteLoginForm();
    end else if pvRequest.RequestURI = '/redirect' then
    begin                                       //���¶���
      s := pvRequest.GetRequestParam('url');
      if s = '' then
      begin
        pvRequest.Response.WriteString('�ض�������:<a href="/redirect?url=http://www.diocp.org">' +
         '/redirect?url=http://www.diocp.org</a>');
      end else
      begin
        pvRequest.Response.RedirectURL(s);
        pvRequest.CloseContext;
        Exit;
      end;
    end else if pvRequest.RequestURI = '/input' then
    begin  // ���diocp������Ϣ
      pvRequest.Response.WriteString('DIOCP HTTP ���ύ����<br>');
      pvRequest.Response.WriteString('<form id="form1" name="form1" method="post" action="/post?param1=''����''&time=' + DateTimeToStr(Now()) +'">');
      pvRequest.Response.WriteString('<table width="50%" border="1" align="center">');
      pvRequest.Response.WriteString('<tr><td width="35%">�������������:</td>');
      pvRequest.Response.WriteString('<td width="35%"><input name="a" type="text" value="DIOCP-V5" /></td></tr>');
      pvRequest.Response.WriteString('<tr><td width="35%">��������İ���:</td>');
      pvRequest.Response.WriteString('<td width="35%"><input name="b" type="text" value="LOLӢ������" /></td></tr>');
      pvRequest.Response.WriteString('<tr><td width="35%">����:</td>');
      pvRequest.Response.WriteString('<td width="35%"><input type="submit" name="Submit" value="�ύ"/></td></tr>');
      pvRequest.Response.WriteString('</table></form>');
    end else
    begin
      WriteNormalPage();
    end;

    // Ӧ����ϣ����ͻ�ͻ���
    pvRequest.ResponseEnd;
  finally
    if chkRecord2File.Checked then
    begin
      WriteStringToUtf8NoBOMFile(
        Format('DiocpHttpRequest_%d_%s.resp', [pvRequest.Connection.SocketHandle, lvNowString]),
        pvRequest.Response.GetResponseHeaderAsString);
    end;
  end;
end;

procedure TfrmMain.OnHttpSvrTestRequest(pvRequest: TDiocpHttpRequest);
var
  lvDValue:TDValue;
  s:string;
begin
  if pvRequest.RequestURI = '/json' then
  begin
    pvRequest.Response.ContentType := 'text/json';
    s := '{"ab":"1111111111111111111111111111111111111111111111111111111111111111111"}';
//    lvDValue := TDValue.Create;
//    lvDValue.ForceByName('title').AsString := 'DIOCP-V5 Http ������ʾ';
//    lvDValue.ForceByName('author').AsString := 'D10.�����';
//    lvDValue.ForceByName('time').AsString := DateTimeToStr(Now());
//    s := JSONEncode(lvDValue);
//   lvDValue.Free;
    pvRequest.Response.WriteString(s);
    pvRequest.SendResponse;
    pvRequest.DoResponseEnd;
  end;  
end;

destructor TfrmMain.Destroy;
begin
  FTcpServer.SafeStop();
  FTcpServer.Free;
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

  chkUsePool.Enabled := not FTcpServer.Active;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  FTcpServer.Port := StrToInt(edtPort.Text);
  FTcpServer.WorkerCount := StrToInt(edtWorkCount.Text);
  iocpTaskManager.setWorkerCount(FTcpServer.WorkerCount);
  FTcpServer.UseObjectPool := chkUsePool.Checked;
  FTcpServer.Listeners.ClearObjects;
  if chkIPV6.Checked then
  begin
    FTcpServer.Listeners.Bind('0.0.0.0', StrToInt(edtPort.Text), IP_V6, TDiocpHttpClientContext);
    ShowMessage('��������: http://[����ipv6��ַ]:8081/');
  end;
  FTcpServer.Active := true;
  FTcpServer.DisableSession := not chkUseSession.Checked;
  refreshState;
  tmrHeart.Enabled := true;
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

procedure TfrmMain.btnInfoClick(Sender: TObject);
var
  s:string;
  r:Integer;
begin
  s :=Format('get:%d, put:%d, addRef:%d, releaseRef:%d, size:%d',
    [FTcpServer.BlockBufferPool.FGet, FTcpServer.BlockBufferPool.FPut, FTcpServer.BlockBufferPool.FAddRef,
    FTcpServer.BlockBufferPool.FReleaseRef, FTcpServer.BlockBufferPool.FSize]);
  r := CheckBufferBounds(FTcpServer.BlockBufferPool);
  s := s + sLineBreak + Format('���й���:%d���ڴ��, ����[%d]���ڴ��д��Խ������', [FTcpServer.BlockBufferPool.FSize, r]);
  sfLogger.logMessage(s);
  sfLogger.logMessage(FTcpServer.GetPrintDebugInfo);

  sfLogger.logMessage(Format('log:%d', [__logCounter]));

  
end;

procedure TfrmMain.btnCompressClick(Sender: TObject);
var
  lvBuilder:TDBufferBuilder;
begin
  {$IFDEF USE_ZLIBExGZ}
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
  {$ELSE}
  raise Exception.Create('δ����ZlibEx');
  {$ENDIF}

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

procedure TfrmMain.btnParseRangeClick(Sender: TObject);
var
  s:String;
  lvRange:THttpRange;
begin
  //��ʾͷ500���ֽڣ�bytes=0-499
  //������ʾ�ڶ���500�ֽڣ�bytes=500-999
  //������ʾ���500���ֽڣ�bytes=-500
  //������ʾ500�ֽ��Ժ�ķ�Χ��bytes=500-  
  //������һ�������һ���ֽڣ�bytes=0-0,-1
  //����ͬʱָ��������Χ��bytes=500-600,601-999
  lvRange := THttpRange.Create;
  try
    s := 'bytes=500-999';
    lvRange.ParseRange(s);
    ShowMessage(Format('%d, %d', [lvRange.IndexOf(0).VStart, lvRange.IndexOf(0).VEnd]));
  finally
    lvRange.Free;
  end;
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

procedure TfrmMain.btnWebSocketPushClick(Sender: TObject);
var
  lvList:TList;
  i: Integer;
  lvContext:TDiocpHttpClientContext;
begin
  lvList := TList.Create;
  try
    FTcpServer.GetOnlineContextList(lvList);

    for i := 0 to lvList.Count - 1 do
    begin
       lvContext := TDiocpHttpClientContext(lvList[i]);
       if lvContext.LockContext('websocket', lvContext) then
       try
         if lvContext.ContextType = Context_Type_WebSocket then
         begin
           lvContext.PostWebSocketData(mmoWebSocketData.Lines.Text, True);       
         end;
       finally
         lvContext.UnLockContext('websocket', lvContext);
       end;
    end;
  finally
    lvList.Free;
  end;
end;

procedure TfrmMain.chkUseSessionClick(Sender: TObject);
begin
  FChkSession := chkUseSession.Checked;
end;

function TfrmMain.DoLoadFile(pvRequest:TDiocpHttpRequest): Boolean;
var
  lvDefaultFile:string;
  lvExt, lvstr:string;
begin
  pvRequest.DecodeURLParam(True);
  lvDefaultFile := pvRequest.URLParams.GetValueByName('downfile', STRING_EMPTY);
  if Length(lvDefaultFile) > 0 then
  begin
    lvDefaultFile := StringReplace(lvDefaultFile, '/', '\', [rfReplaceAll]);
    pvRequest.Response.ContentType := GetContentTypeFromFileExt(lvExt, 'application/stream');
    pvRequest.Response.SetResponseFileName(ExtractFileName(lvDefaultFile));
    pvRequest.ResponseAFile(lvDefaultFile);
    Result := True;
    Exit;
  end;
  if not FileExists(lvDefaultFile) then
  begin
    lvDefaultFile := pvRequest.RequestURI;
    if not FileExists(lvDefaultFile) then
      lvDefaultFile := ExtractFilePath(ParamStr(0)) + '\webroot\' + pvRequest.RequestURI;
  end;

  if FileExists(lvDefaultFile) then
  begin
    pvRequest.Response.ClearContent;
    lvExt :=LowerCase(ExtractFileExt(lvDefaultFile));

    pvRequest.Response.ContentType := GetContentTypeFromFileExt(lvExt, 'application/stream');
    //pvRequest.Response.SetResponseFileName('x.file');
    //pvRequest.ResponseAFile(lvDefaultFile);
 //   pvRequest.ResponseAStream(TFileStream.Create(lvDefaultFile, fmOpenRead), OnResposeStreamDone);
    pvRequest.Response.LoadFromFile(lvDefaultFile);

    if (StrIndexOf(lvExt, ['.js', '.html', '.htm', '.css']) <> -1)
      and
       (PosWStr('gzip', pvRequest.RequestAcceptEncoding) > 0) then  // ����ѹ��
    begin
      if pvRequest.ResponseAFileETag(lvDefaultFile) then
      begin      // ����ͷ��ETag�� �������false���Ѿ���Ӧ
        pvRequest.Response.LoadFromFile(lvDefaultFile);
        pvRequest.Response.Header.ForceByName('Content-Encoding').AsString := 'gzip';
        lvstr := pvRequest.Response.Header.GetValueByName('Content-Encoding', '');
        if Pos('zlib', lvstr) > 0 then
        begin
          pvRequest.Response.ZLibContent;
        end else if Pos('lzo', lvstr) > 0 then
        begin
          pvRequest.Response.LZOCompressContent;
        end else if Pos('gzip', lvstr) > 0 then
        begin
          pvRequest.Response.GZipContent;
        end else if Pos('deflate', lvstr) > 0 then
        begin
          pvRequest.Response.DeflateCompressContent;
        end;
        pvRequest.SendResponse();
        pvRequest.DoResponseEnd;
      end else
      begin
        // ETag �Զ�����pvRequest
      end;

    end else
    begin
      pvRequest.ResponseAFileEx(lvDefaultFile);
    end;




    Result := True;
  end else
  begin
    Result := False;
  end;

//  pvRequest.SendResponse();
//  pvRequest.DoResponseEnd();
end;

function TfrmMain.GetWebSocketCounter(pvExceptContext:TDiocpHttpClientContext):
    Integer;
var
  lvList:TList;
  i: Integer;
  lvContext:TDiocpHttpClientContext;
begin
  lvList := TList.Create;
  try
    FTcpServer.GetOnlineContextList(lvList);
    Result := 0;

    for i := 0 to lvList.Count - 1 do
    begin
       lvContext := TDiocpHttpClientContext(lvList[i]);
       if lvContext <> pvExceptContext then
       begin
         if lvContext.LockContext('websocket', lvContext) then
         try
           if lvContext.ContextType = Context_Type_WebSocket then
           begin
             inc(Result);
           end;
         finally
           lvContext.UnLockContext('websocket', lvContext);
         end;
       end;
    end;
  finally
    lvList.Free;
  end;
end;

procedure TfrmMain.OnResposeStreamDone(pvData:Pointer; pvCode:Integer);
begin
  sfLogger.logMessage('�������������:%d', [TStream(pvData).Size]);
  TStream(pvData).Free;                                                   
  
end;

procedure TfrmMain.tmrHeartTimer(Sender: TObject);
begin
  FTcpServer.KickOut();
  FTcpServer.CheckSessionTimeOut;
end;

procedure TfrmMain.tmrInfoRefreshTimer(Sender: TObject);
begin
  //if FTcpServer.Active then
  begin
    lblHttpInfo.Caption := FTcpServer.GetPrintDebugInfo;
  end;
end;

procedure TfrmMain.tmrWebSocketPingTimer(Sender: TObject);
begin
  FTcpServer.WebSocketSendPing;
end;

function TfrmMain.WebSocketPush(const pvData: string; pvExceptContext:
    TDiocpHttpClientContext): Integer;
var
  lvList:TList;
  i: Integer;
  lvContext:TDiocpHttpClientContext;
begin
  lvList := TList.Create;
  try
    FTcpServer.GetOnlineContextList(lvList);
    Result := 0;

    for i := 0 to lvList.Count - 1 do
    begin
       lvContext := TDiocpHttpClientContext(lvList[i]);
       if lvContext <> pvExceptContext then
       begin
         if lvContext.LockContext('websocket', lvContext) then
         try
           if lvContext.ContextType = Context_Type_WebSocket then
           begin
             lvContext.PostWebSocketData(pvData, True);
             inc(Result);
           end;
         finally
           lvContext.UnLockContext('websocket', lvContext);
         end;
       end;
    end;
  finally
    lvList.Free;
  end;
  
end;



end.
