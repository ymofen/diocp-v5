unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, diocp.tcp.client,
  utils.safeLogger, ComCtrls, diocp.sockets, ExtCtrls;

type
  TEchoContext = class(TIocpRemoteContext)
    FMaxTick:Cardinal;
    FStartTime:TDateTime;
    FLastTick:Cardinal;
  end;

  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    tsMonitor: TTabSheet;
    mmoRecvMessage: TMemo;
    tsOperator: TTabSheet;
    mmoData: TMemo;
    btnFill1K: TButton;
    btnSendObject: TButton;
    pnlTop: TPanel;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnClose: TButton;
    btnCreate: TButton;
    edtCount: TEdit;
    chkSendData: TCheckBox;
    chkRecvEcho: TCheckBox;
    chkRecvOnLog: TCheckBox;
    btnClear: TButton;
    chkHex: TCheckBox;
    chkCheckHeart: TCheckBox;
    btnSaveHistory: TButton;
    tmrCheckHeart: TTimer;
    chkLogRecvTime: TCheckBox;
    procedure btnClearClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnFill1KClick(Sender: TObject);
    procedure btnSaveHistoryClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
    procedure chkCheckHeartClick(Sender: TObject);
    procedure chkHexClick(Sender: TObject);
    procedure chkLogRecvTimeClick(Sender: TObject);
    procedure chkRecvEchoClick(Sender: TObject);
    procedure chkRecvOnLogClick(Sender: TObject);
    procedure chkSendDataClick(Sender: TObject);
    procedure tmrCheckHeartTimer(Sender: TObject);
  private
    FSendDataOnConnected:Boolean;
    FSendDataOnRecv:Boolean;
    FLogRecvInfo:Boolean;
    FRecvOnLog:Boolean;
    FConvertHex:Boolean;
    FFileLogger:TSafeLogger;
    FIocpClientSocket: TDiocpTcpClient;

    procedure DoSend(pvConentxt: TDiocpCustomContext; s: AnsiString);

    procedure OnContextConnected(pvContext: TDiocpCustomContext);

    procedure OnRecvdBuffer(pvContext: TDiocpCustomContext; buf: Pointer; len:
        cardinal; pvErrorCode: Integer);

    procedure WriteHistory;

    procedure ReadHistory;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, utils_dvalue, utils_DValue_JSON, utils.byteTools;
{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FFileLogger := TSafeLogger.Create;
  FFileLogger.setAppender(TLogFileAppender.Create(False), true);
  FSendDataOnRecv := chkRecvEcho.Checked;
  FRecvOnLog := chkRecvOnLog.Checked;
  FConvertHex := chkHex.Checked;
  sfLogger.setAppender(TStringsAppender.Create(mmoRecvMessage.Lines));
  sfLogger.AppendInMainThread := true;

  FIocpClientSocket := TDiocpTcpClient.Create(Self);
  FIocpClientSocket.createDataMonitor;
  FIocpClientSocket.OnContextConnected := OnContextConnected;
  FIocpClientSocket.OnReceivedBuffer := OnRecvdBuffer;
  FIocpClientSocket.RegisterContextClass(TEchoContext);
  TFMMonitor.createAsChild(tsMonitor, FIocpClientSocket);

  ReadHistory;

end;

destructor TfrmMain.Destroy;
begin
  FIocpClientSocket.Close;
  FIocpClientSocket.Free;
  FFileLogger.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  mmoRecvMessage.Clear;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FIocpClientSocket.Count-1 do
  begin
    FIocpClientSocket.Items[i].AutoReConnect := false;
    FIocpClientSocket.Items[i].Close; 
  end;
  FIocpClientSocket.WaitForContext(30000);
  FIocpClientSocket.ClearContexts;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
var
  lvClient:TIocpRemoteContext;
begin
  FIocpClientSocket.open;

  lvClient := FIocpClientSocket.Add;
  lvClient.Host := edtHost.Text;
  lvClient.Port := StrToInt(edtPort.Text);
  lvClient.AutoReConnect := true;
  lvClient.ConnectASync;



  mmoRecvMessage.Clear;

  mmoRecvMessage.Lines.Add('start to recv...');
end;

procedure TfrmMain.btnCreateClick(Sender: TObject);

var
  lvClient:TIocpRemoteContext;
  i:Integer;

begin
  FIocpClientSocket.open;

  for i := 1 to StrToInt(edtCount.Text) do
  begin
    lvClient := FIocpClientSocket.Add;
    lvClient.Host := edtHost.Text;
    lvClient.Port := StrToInt(edtPort.Text);
    lvClient.AutoReConnect := true;
    lvClient.connectASync;
  end;

end;

procedure TfrmMain.btnFill1KClick(Sender: TObject);
var
  s:AnsiString;
begin
  SetLength(s, 1024);
  FillChar(PAnsiChar(s)^, 1024, 'a');
  mmoData.Lines.Text :=  s;
end;

procedure TfrmMain.btnSaveHistoryClick(Sender: TObject);
begin
  WriteHistory;
end;

procedure TfrmMain.btnSendObjectClick(Sender: TObject);
var
  i, l: Integer;
  lvBytes:TBytes;
  s:AnsiString;
begin
  s := mmoData.Lines.Text;
  for i := 0 to FIocpClientSocket.Count - 1 do
  begin
    DoSend(FIocpClientSocket.Items[i], s);
  end;
end;

procedure TfrmMain.chkCheckHeartClick(Sender: TObject);
begin
  ;
end;

procedure TfrmMain.chkHexClick(Sender: TObject);
var
  s:AnsiString;
  l:Integer;
  lvBytes:TBytes;
begin
  FConvertHex := chkHex.Checked;
  if chkHex.Tag = 1 then Exit;

  s := mmoData.Lines.Text;


  if FConvertHex then
  begin
    mmoData.Lines.Text := TByteTools.varToHexString(PAnsiChar(s)^, Length(s));
  end else
  begin
    s := StringReplace(s, ' ', '', [rfReplaceAll]);
    s := StringReplace(s, #10, '', [rfReplaceAll]);
    s := StringReplace(s, #13, '', [rfReplaceAll]);
    l := Length(s);
    SetLength(lvBytes, l);
    FillChar(lvBytes[0], l, 0);
    l := TByteTools.HexToBin(s, @lvBytes[0]);
    mmoData.Lines.Text := StrPas(@lvBytes[0]);
  end;
end;

procedure TfrmMain.chkLogRecvTimeClick(Sender: TObject);
begin
  FLogRecvInfo := chkLogRecvTime.Checked;
end;

procedure TfrmMain.chkRecvEchoClick(Sender: TObject);
begin
  FSendDataOnRecv := chkRecvEcho.Checked;
end;

procedure TfrmMain.chkRecvOnLogClick(Sender: TObject);
begin
  FRecvOnLog := chkRecvOnLog.Checked;
end;

procedure TfrmMain.chkSendDataClick(Sender: TObject);
begin
  FSendDataOnConnected := chkSendData.Checked;
end;

procedure TfrmMain.DoSend(pvConentxt: TDiocpCustomContext; s: AnsiString);
var
  i, l: Integer;
  lvBytes:TBytes;
begin
  //s := mmoData.Lines.Text;
  if FConvertHex then
  begin
    s := StringReplace(s, ' ', '', [rfReplaceAll]);
    s := StringReplace(s, #10, '', [rfReplaceAll]);
    s := StringReplace(s, #13, '', [rfReplaceAll]);
    l := Length(s);
    SetLength(lvBytes, l);
    FillChar(lvBytes[0], l, 0);
    l := TByteTools.HexToBin(s, @lvBytes[0]);
  end;

  if FConvertHex then
  begin
    pvConentxt.PostWSASendRequest(@lvBytes[0], l);
  end else
  begin
    pvConentxt.PostWSASendRequest(PAnsiChar(s), Length(s));
  end;

end;

procedure TfrmMain.OnContextConnected(pvContext: TDiocpCustomContext);
var
  s:AnsiString;
begin
  TEchoContext(pvContext).FStartTime := Now();
  TEchoContext(pvContext).FLastTick := GetTickCount;
  TEchoContext(pvContext).FMaxTick := 0;

  s := mmoData.Lines.Text;
  if FSendDataOnConnected then
  begin
    DoSend(pvContext, s);
  end;

end;

procedure TfrmMain.OnRecvdBuffer(pvContext: TDiocpCustomContext; buf: Pointer;
    len: cardinal; pvErrorCode: Integer);
var
  lvStr:AnsiString;
  lvContext:TEchoContext;
  lvFmt:String;
  lvTick:Cardinal;
begin
  lvContext := TEchoContext(pvContext);
  if FLogRecvInfo then
  begin
    lvTick := GetTickCount;
    lvFmt := Format('[%d], t: %s, data:%d, delay:%d',
      [lvContext.SocketHandle,
       FormatDateTime('yyyy-MM-dd hh:nn:ss', Now()),
       len,
       lvTick - TEchoContext(pvContext).FLastTick
      ]);


    FFileLogger.logMessage(lvFmt, '连接数据信息');
    TEchoContext(pvContext).FLastTick := lvTick;
    TEchoContext(pvContext).FMaxTick := 0;
  end;
  if len = 0 then
  begin
    sfLogger.logMessage('recv err zero');
  end;
  if pvErrorCode = 0 then
  begin
    if FSendDataOnRecv then
    begin
      Sleep(0);
      pvContext.PostWSASendRequest(buf, len);
    end;
    if FRecvOnLog then
    begin
      SetLength(lvStr, len);
      Move(buf^, PAnsiChar(lvStr)^, len);
      sfLogger.logMessage(lvStr);
    end;
  end else
  begin
    sfLogger.logMessage('recv err:%d', [pvErrorCode]);
  end;
end;

procedure TfrmMain.ReadHistory;
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create();
  JSONParseFromUtf8NoBOMFile(ChangeFileExt(ParamStr(0), '.history.json'), lvDVAlue);
  edtHost.Text := lvDValue.ForceByName('host').AsString;
  edtPort.Text := lvDValue.ForceByName('port').AsString;
  mmoData.Lines.Text := lvDValue.ForceByName('sendText').AsString;
  chkRecvEcho.Checked := lvDValue.ForceByName('chk_recvecho').AsBoolean;
  chkRecvOnLog.Checked := lvDValue.ForceByName('chk_recvonlog').AsBoolean;
  chkSendData.Checked := lvDValue.ForceByName('chk_send_onconnected').AsBoolean;

  chkHex.Tag := 1;
  chkHex.Checked := lvDValue.ForceByName('chk_send_hex').AsBoolean;
  chkHex.Tag := 0;

  chkCheckHeart.Checked := lvDValue.ForceByName('chk_checkheart').AsBoolean;
  chkLogRecvTime.Checked := lvDValue.ForceByName('chk_LogRecvInfo').AsBoolean;
  lvDValue.Free;

  FSendDataOnConnected := chkSendData.Checked;
  FRecvOnLog := chkRecvOnLog.Checked;
  FSendDataOnRecv := chkRecvEcho.Checked;
  FConvertHex := chkHex.Checked;
  FLogRecvInfo := chkLogRecvTime.Checked;
end;

procedure TfrmMain.tmrCheckHeartTimer(Sender: TObject);
begin
  if chkCheckHeart.Checked then
    self.FIocpClientSocket.KickOut(30000);
end;

procedure TfrmMain.WriteHistory;
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create();
  lvDValue.ForceByName('host').AsString := edtHost.Text;
  lvDValue.ForceByName('port').AsString := edtPort.Text;
  lvDValue.ForceByName('sendText').AsString := mmoData.Lines.Text;
  lvDValue.ForceByName('chk_recvecho').AsBoolean := chkRecvEcho.Checked;
  lvDValue.ForceByName('chk_recvonlog').AsBoolean := chkRecvOnLog.Checked;
  lvDValue.ForceByName('chk_send_onconnected').AsBoolean := chkSendData.Checked;
  lvDValue.ForceByName('chk_send_hex').AsBoolean := chkHex.Checked;
  lvDValue.ForceByName('chk_checkheart').AsBoolean := chkCheckHeart.Checked;
  lvDValue.ForceByName('chk_LogRecvInfo').AsBoolean := chkLogRecvTime.Checked;
  JSONWriteToUtf8NoBOMFile(ChangeFileExt(ParamStr(0), '.history.json'), lvDVAlue);
  lvDValue.Free;
end;

end.
