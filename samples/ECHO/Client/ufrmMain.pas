unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, diocp_tcp_client,
  utils_safeLogger, ComCtrls, diocp_sockets, ExtCtrls, utils_async,
  utils_BufferPool, utils_fileWriter, diocp_tcp_blockClient, Registry,
  utils_strings;

type
  TEchoContext = class(TIocpRemoteContext)
    FObjectID:Integer;
    FMaxTick:Cardinal;
    FStartTime:TDateTime;
    FLastTick:Cardinal;
    FLastSendTick:Cardinal;
    FFileWritter: TSingleFileWriter;
    FNextDisconnect:Cardinal;
  public
    procedure OnDisconnected; override;
    procedure DoRandomDisconnect;
  public
    destructor Destroy; override;
    procedure WriteRecvData(pvBuf:Pointer; pvLength:Integer);
  end;

  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    tsMonitor: TTabSheet;
    mmoRecvMessage: TMemo;
    tsOperator: TTabSheet;
    pnlTop: TPanel;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnClose: TButton;
    btnCreate: TButton;
    edtCount: TEdit;
    chkRecvEcho: TCheckBox;
    chkRecvOnLog: TCheckBox;
    btnClear: TButton;
    chkHex: TCheckBox;
    chkCheckHeart: TCheckBox;
    btnSaveHistory: TButton;
    tmrCheckHeart: TTimer;
    chkLogRecvTime: TCheckBox;
    pnlOpera_Top: TPanel;
    btnSendObject: TButton;
    btnFill1K: TButton;
    pnlOpera_Send: TPanel;
    mmoData: TMemo;
    tsEvent: TTabSheet;
    mmoIntervalData: TMemo;
    pnlIntervalTop: TPanel;
    edtInterval: TEdit;
    btnSetInterval: TButton;
    grpOnConnected: TGroupBox;
    grpInterval: TGroupBox;
    chkSendData: TCheckBox;
    mmoOnConnected: TMemo;
    chkIntervalSendData: TCheckBox;
    chkSaveData: TCheckBox;
    btnEcho: TButton;
    mmoOperaLog: TMemo;
    chkAutoReconnect: TCheckBox;
    chkRandomDisconnect: TCheckBox;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    pnlLog: TPanel;
    spllog: TSplitter;
    btnReadConfig: TButton;
    btnInfo: TButton;
    btnBreakConnectNumLimit: TButton;
    procedure btnBreakConnectNumLimitClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnEchoClick(Sender: TObject);
    procedure btnFill1KClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnReadConfigClick(Sender: TObject);
    procedure btnSaveHistoryClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
    procedure btnSetIntervalClick(Sender: TObject);
    procedure chkAutoReconnectClick(Sender: TObject);
    procedure chkCheckHeartClick(Sender: TObject);
    procedure chkHexClick(Sender: TObject);
    procedure chkIntervalSendDataClick(Sender: TObject);
    procedure chkLogRecvTimeClick(Sender: TObject);
    procedure chkRandomDisconnectClick(Sender: TObject);
    procedure chkRecvEchoClick(Sender: TObject);
    procedure chkRecvOnLogClick(Sender: TObject);
    procedure chkSaveDataClick(Sender: TObject);
    procedure chkSendDataClick(Sender: TObject);
    procedure tmrCheckHeartTimer(Sender: TObject);
  private
    FBlockTcp:TDiocpBlockTcpClient;
    FSpinLock:Integer;
    
    FSendDataOnConnected:Boolean;
    FSendDataOnRecv:Boolean;
    FLogRecvInfo:Boolean;
    FRecvOnLog:Boolean;
    FRecvOnSaveToFile:Boolean;
    FConvertHex:Boolean;
    FRandomeDisconnect:Boolean;
    FAutoReconnect:Boolean;


    FASyncInvoker:TASyncInvoker;
    FSendInterval: Cardinal;
    FSendDataOnInterval:Boolean;

    FFileLogger:TSafeLogger;
    FIocpClientSocket: TDiocpTcpClient;

    procedure DoSend(pvConentxt: TDiocpCustomContext; s: AnsiString);

    procedure OnContextConnected(pvContext: TDiocpCustomContext);

    procedure OnRecvdBuffer(pvContext: TDiocpCustomContext; buf: Pointer; len:
        cardinal; pvErrorCode: Integer);

    procedure OnASyncWork(pvASyncWorker:TASyncWorker);

    procedure OnASyncCycle(pvContext:TDiocpCustomContext);

    procedure WriteHistory(pvFileName: string);

    procedure ReadHistory(pvFileName: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, utils_dvalue, utils_DValue_JSON, utils_byteTools;
{$R *.dfm}

{ TfrmMain }

var
  __SN:Integer;
  __defaultFile:string;



constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  __defaultFile := ChangeFileExt(ParamStr(0), '.history.json');
  FASyncInvoker := TASyncInvoker.Create;
  FASyncInvoker.Start(OnASyncWork);
  FFileLogger := TSafeLogger.Create;
  FFileLogger.setAppender(TLogFileAppender.Create(False), true);
  FSendDataOnRecv := chkRecvEcho.Checked;
  FRecvOnLog := chkRecvOnLog.Checked;
  FRecvOnSaveToFile := chkSaveData.Checked;
  FConvertHex := chkHex.Checked;
  sfLogger.setAppender(TStringsAppender.Create(mmoRecvMessage.Lines));
  sfLogger.AppendInMainThread := true;

  FIocpClientSocket := TDiocpTcpClient.Create(Self);
  FIocpClientSocket.createDataMonitor;
  FIocpClientSocket.OnContextConnected := OnContextConnected;
  FIocpClientSocket.OnReceivedBuffer := OnRecvdBuffer;
  FIocpClientSocket.RegisterContextClass(TEchoContext);
  FIocpClientSocket.DisableAutoConnect := True;
  TFMMonitor.createAsChild(tsMonitor, FIocpClientSocket);

  ReadHistory(__defaultFile);

end;

destructor TfrmMain.Destroy;
begin
  FASyncInvoker.Terminate;
  FASyncInvoker.WaitForStop;
  FASyncInvoker.Free;
  FIocpClientSocket.Close;
  FIocpClientSocket.Free;
  FFileLogger.Free;
  inherited Destroy;
end;

procedure RegUpdateMaxUserPort();
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters', True) then
    begin
      try 
        if (not reg.ValueExists('MaxUserPort')) or (reg.ReadInteger('MaxUserPort') <> 65534) then
        begin
          reg.WriteInteger('MaxUserPort', 65534);
          reg.WriteInteger('MaxHashTableSize', 65536);
          reg.WriteInteger('MaxFreeTcbs', 16000);
          reg.WriteInteger('TcpTimedWaitDelay', 5);
        end;
      except
      end;
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure TfrmMain.btnBreakConnectNumLimitClick(Sender: TObject);
begin
  RegUpdateMaxUserPort;
  ShowMessage('连接数更新成功，可能需要重启系统');
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  mmoRecvMessage.Clear;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
var
  i: Integer;
begin
  SpinLock(FSpinLock);
  try
    for i := 0 to FIocpClientSocket.Count-1 do
    begin
      FIocpClientSocket.Items[i].AutoReConnect := false;
      FIocpClientSocket.Items[i].Close;
    end;
    FIocpClientSocket.WaitForContext(30000);
    FIocpClientSocket.ClearContexts;
  finally
    SpinUnLock(FSpinLock);
  end;

  Self.Caption := 'diocpv5 echo client[stop]';
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
var
  lvClient:TIocpRemoteContext;
begin
  SpinLock(FSpinLock);
  try
    FIocpClientSocket.Open; 
    lvClient := FIocpClientSocket.Add;
    lvClient.Host := edtHost.Text;
    lvClient.Port := StrToInt(edtPort.Text);
    lvClient.ConnectASync;
    lvClient.OnASyncCycle := OnASyncCycle;
  finally
    SpinUnLock(FSpinLock);
  end;


  mmoRecvMessage.Clear;

  mmoRecvMessage.Lines.Add('start to recv...');
end;

procedure TfrmMain.btnCreateClick(Sender: TObject);
var
  lvClient:TIocpRemoteContext;
  i:Integer;
begin
  chkRecvOnLog.Checked := StrToInt(edtCount.Text) < 10;
  SpinLock(FSpinLock);
  try
    FIocpClientSocket.Open;

    for i := 1 to StrToInt(edtCount.Text) do
    begin
      lvClient := FIocpClientSocket.Add;
      lvClient.Host := edtHost.Text;
      lvClient.Port := StrToInt(edtPort.Text);
      lvClient.OnASyncCycle := OnASyncCycle;
      lvClient.connectASync;
    end;

    Self.Caption := Format('diocpv5 echo client[running:%d]', [StrToInt(edtCount.Text)]);
  finally
    SpinUnLock(FSpinLock);
  end;
end;

procedure TfrmMain.btnEchoClick(Sender: TObject);
var
  t:Cardinal;
  s:AnsiString;
  l:Integer;
  lvRecv:TBytes;
begin
  if FBlockTcp = nil then
  begin
    FBlockTcp := TDiocpBlockTcpClient.Create(self);
  end;
  if FBlockTcp.Host <> edtHost.Text then
  begin
    FBlockTcp.Disconnect;
    FBlockTcp.Host := edtHost.Text;
  end;

  if FBlockTcp.Port <> StrToInt(edtPort.Text) then
  begin
    FBlockTcp.Disconnect;
    FBlockTcp.Port := StrToInt(edtPort.Text);
  end;

  if not FBlockTcp.Active then
  begin
    t := GetTickCount;
    FBlockTcp.Connect;
    t := GetTickCount - t;
    mmoOperaLog.Lines.Add(Format('连接耗时:%d ms', [t]));
  end;
  s := mmoData.Lines.Text;
  l := Length(s);
  if l = 0 then Exit;
  t := GetTickCount;
  FBlockTcp.SendBuffer(PAnsiChar(s), Length(s));
  t := GetTickCount - t;
  mmoOperaLog.Lines.Add(Format('send耗时:%d ms, len:%d', [t, l]));

  SetLength(lvRecv, l);
  t := GetTickCount;
  FBlockTcp.recv(@lvRecv[0], l);
  t := GetTickCount - t;
  mmoOperaLog.Lines.Add(Format('recv耗时:%d ms, 接收数据长度:%d', [t, l]));
end;

procedure TfrmMain.btnFill1KClick(Sender: TObject);
var
  s:AnsiString;
begin
  SetLength(s, 1024);
  FillChar(PAnsiChar(s)^, 1024, 'a');
  mmoData.Lines.Text :=  s;
end;

procedure TfrmMain.btnInfoClick(Sender: TObject);
begin
  mmoOperaLog.Lines.Add(Format('剩余未处理log对象:%d', [__logCounter])) ;
end;

procedure TfrmMain.btnReadConfigClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    ReadHistory(dlgOpen.FileName);
  end;
end;

procedure TfrmMain.btnSaveHistoryClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    WriteHistory(dlgSave.FileName);
  end;
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

procedure TfrmMain.btnSetIntervalClick(Sender: TObject);
var
  lvInterval:Integer;
begin
  lvInterval := StrToIntDef(edtInterval.Text, 0) * 1000;
  if lvInterval <=0 then raise Exception.Create('必须设定大于0的值');
  FSendInterval := lvInterval;
end;

procedure TfrmMain.chkAutoReconnectClick(Sender: TObject);
begin
  FAutoReconnect := chkAutoReconnect.Checked;
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
    mmoData.Lines.Text := StrPas(PAnsiChar(@lvBytes[0]));
  end;

  s := mmoOnConnected.Lines.Text;
  if FConvertHex then
  begin
    mmoOnConnected.Lines.Text := TByteTools.varToHexString(PAnsiChar(s)^, Length(s));
  end else
  begin
    s := StringReplace(s, ' ', '', [rfReplaceAll]);
    s := StringReplace(s, #10, '', [rfReplaceAll]);
    s := StringReplace(s, #13, '', [rfReplaceAll]);
    l := Length(s);
    SetLength(lvBytes, l);
    FillChar(lvBytes[0], l, 0);
    l := TByteTools.HexToBin(s, @lvBytes[0]);
    mmoOnConnected.Lines.Text := StrPas(PAnsiChar(@lvBytes[0]));
  end;

  s := mmoIntervalData.Lines.Text;
  if FConvertHex then
  begin
    mmoIntervalData.Lines.Text := TByteTools.varToHexString(PAnsiChar(s)^, Length(s));
  end else
  begin
    s := StringReplace(s, ' ', '', [rfReplaceAll]);
    s := StringReplace(s, #10, '', [rfReplaceAll]);
    s := StringReplace(s, #13, '', [rfReplaceAll]);
    l := Length(s);
    SetLength(lvBytes, l);
    FillChar(lvBytes[0], l, 0);
    l := TByteTools.HexToBin(s, @lvBytes[0]);
    mmoIntervalData.Lines.Text := StrPas(PAnsiChar(@lvBytes[0]));
  end;
end;

procedure TfrmMain.chkIntervalSendDataClick(Sender: TObject);
begin
  FSendDataOnInterval := chkIntervalSendData.Checked;
end;

procedure TfrmMain.chkLogRecvTimeClick(Sender: TObject);
begin
  FLogRecvInfo := chkLogRecvTime.Checked;
end;

procedure TfrmMain.chkRandomDisconnectClick(Sender: TObject);
begin
  FRandomeDisconnect := chkRandomDisconnect.Checked;
end;

procedure TfrmMain.chkRecvEchoClick(Sender: TObject);
begin
  FSendDataOnRecv := chkRecvEcho.Checked;
end;

procedure TfrmMain.chkRecvOnLogClick(Sender: TObject);
begin
  FRecvOnLog := chkRecvOnLog.Checked;
end;

procedure TfrmMain.chkSaveDataClick(Sender: TObject);
begin
  FRecvOnSaveToFile := chkSaveData.Checked;
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

procedure TfrmMain.OnASyncCycle(pvContext: TDiocpCustomContext);
begin
  if FAutoReconnect then
  begin
    if pvContext.CheckActivityTimeOut(10000) then
      TIocpRemoteContext(pvContext).CheckDoReConnect;
  end;

  if FRandomeDisconnect then
  begin
    TEchoContext(pvContext).DoRandomDisconnect;
  end;
  
end;

procedure TfrmMain.OnASyncWork(pvASyncWorker:TASyncWorker);
var
  i, l: Integer;
  lvBytes:TBytes;
  s:AnsiString;
  lvEchoClient:TEchoContext;
begin
  while not FASyncInvoker.Terminated do
  begin
    if (FSendInterval > 0) and FSendDataOnInterval then
    begin
      s := mmoIntervalData.Lines.Text;
      if s <> '' then
      begin
        SpinLock(FSpinLock);
        try
          for i := 0 to FIocpClientSocket.Count - 1 do
          begin
            if FASyncInvoker.Terminated then Exit;
            
            lvEchoClient := TEchoContext(FIocpClientSocket.Items[i]);
            if lvEchoClient.Active then
            begin
              if tick_diff(lvEchoClient.FLastSendTick, GetTickCount) > FSendInterval  then
              begin
                DoSend(lvEchoClient, s);
                lvEchoClient.FLastSendTick := GetTickCount;
              end;
            end;
          end;
        finally
          SpinUnLock(FSpinLock);
        end;
      end;
    end;

    FASyncInvoker.WaitForSleep(1000);
  end;
end;

procedure TfrmMain.OnContextConnected(pvContext: TDiocpCustomContext);
var
  s:AnsiString;
begin
 

  TEchoContext(pvContext).FStartTime := Now();
  TEchoContext(pvContext).FLastTick := GetTickCount;
  TEchoContext(pvContext).FMaxTick := 0;

  s := mmoOnConnected.Lines.Text;
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
      lvStr := TByteTools.BufShowAsString(buf, len);
      sfLogger.logMessage(lvStr);
    end;
    if FRecvOnSaveToFile then
    begin
      TEchoContext(pvContext).WriteRecvData(buf, len);
    end;
  end else
  begin
    sfLogger.logMessage('recv err:%d', [pvErrorCode]);
  end;
end;

procedure TfrmMain.ReadHistory(pvFileName: string);
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create();
  JSONParseFromUtf8NoBOMFile(pvFileName, lvDVAlue);
  edtHost.Text := lvDValue.GetValueByName('host', '127.0.0.1');
  edtPort.Text := lvDValue.GetValueByName('port', '9983');;
  mmoData.Lines.Text := lvDValue.ForceByName('sendText').AsString;
  chkRecvEcho.Checked := lvDValue.ForceByName('chk_recvecho').AsBoolean;
  chkSaveData.Checked := lvDValue.ForceByName('chk_saveonrecv').AsBoolean;
  chkRecvOnLog.Checked := lvDValue.ForceByName('chk_recvonlog').AsBoolean;
  chkSendData.Checked := lvDValue.ForceByName('chk_send_onconnected').AsBoolean;


  chkHex.Tag := 1;
  chkHex.Checked := lvDValue.ForceByName('chk_send_hex').AsBoolean;
  chkHex.Tag := 0;

  chkCheckHeart.Checked := lvDValue.ForceByName('chk_checkheart').AsBoolean;
  chkLogRecvTime.Checked := lvDValue.ForceByName('chk_LogRecvInfo').AsBoolean;

  chkIntervalSendData.Checked := lvDValue.ForceByName('chk_send_oninterval').AsBoolean;
  chkAutoReconnect.Checked := lvDValue.ForceByName('chk_autoconnect').AsBoolean;
  chkRandomDisconnect.Checked := lvDValue.ForceByName('chk_randomdisonnect').AsBoolean;

  edtInterval.Text := IntToStr(lvDValue.ForceByName('send_interval').AsInteger);
  mmoIntervalData.Lines.Text := lvDValue.ForceByName('send_interval_data').AsString;
  mmoOnConnected.Lines.Text := lvDValue.ForceByName('send_onconnected_data').AsString;
  edtCount.Text := lvDValue.ForceByName('client_num').AsString;
  
  lvDValue.Free;

  FSendDataOnConnected := chkSendData.Checked;
  FRecvOnLog := chkRecvOnLog.Checked;
  FRecvOnSaveToFile := chkSaveData.Checked;
  FSendDataOnRecv := chkRecvEcho.Checked;
  FConvertHex := chkHex.Checked;
  FLogRecvInfo := chkLogRecvTime.Checked;
  FRandomeDisconnect := chkRandomDisconnect.Checked;
  FAutoReconnect := chkAutoReconnect.Checked;
  FSendInterval := StrToIntDef(edtInterval.Text, 0) * 1000;
end;

procedure TfrmMain.tmrCheckHeartTimer(Sender: TObject);
begin
  if chkCheckHeart.Checked then
  begin
    SpinLock(FSpinLock);
    try
      self.FIocpClientSocket.KickOut(30000);
    finally
      SpinUnLock(FSpinLock);
    end;

  end;
end;

procedure TfrmMain.WriteHistory(pvFileName: string);
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create();
  lvDValue.ForceByName('host').AsString := edtHost.Text;
  lvDValue.ForceByName('port').AsString := edtPort.Text;
  lvDValue.ForceByName('client_num').AsString := edtCount.Text;
  lvDValue.ForceByName('sendText').AsString := mmoData.Lines.Text;
  lvDValue.ForceByName('chk_recvecho').AsBoolean := chkRecvEcho.Checked;
  lvDValue.ForceByName('chk_saveonrecv').AsBoolean := chkSaveData.Checked;
  lvDValue.ForceByName('chk_recvonlog').AsBoolean := chkRecvOnLog.Checked;
  lvDValue.ForceByName('chk_send_onconnected').AsBoolean := chkSendData.Checked;
  lvDValue.ForceByName('chk_send_hex').AsBoolean := chkHex.Checked;
  lvDValue.ForceByName('chk_checkheart').AsBoolean := chkCheckHeart.Checked;
  lvDValue.ForceByName('chk_LogRecvInfo').AsBoolean := chkLogRecvTime.Checked;
  lvDValue.ForceByName('chk_send_oninterval').AsBoolean := chkIntervalSendData.Checked;
  lvDValue.ForceByName('chk_autoconnect').AsBoolean := chkAutoReconnect.Checked;
  lvDValue.ForceByName('chk_randomdisonnect').AsBoolean := chkRandomDisconnect.Checked;

  lvDValue.ForceByName('send_interval').AsInteger := StrToIntDef(edtInterval.Text, 0);
  lvDValue.ForceByName('send_interval_data').AsString := mmoIntervalData.Lines.Text;
  lvDValue.ForceByName('send_onconnected_data').AsString := mmoOnConnected.Lines.Text;

  JSONWriteToUtf8NoBOMFile(pvFileName, lvDVAlue);
  lvDValue.Free;
end;

destructor TEchoContext.Destroy;
begin
  if FFileWritter <> nil then
  begin
    FreeAndNil(FFileWritter);
  end;
  inherited Destroy;
end;

procedure TEchoContext.DoRandomDisconnect;
  procedure doRandom();
  var
    t:Integer;
  begin
    Randomize;
    t :=  Random(60);
    // 60秒随机断线一次
    FNextDisconnect := GetTickCount + (t * 1000);
  end;
begin
  if FNextDisconnect = 0 then doRandom;

  if tick_diff(FNextDisconnect, GetTickCount) > 0 then
  begin
    self.RequestDisconnect('随机断线', Self);
    doRandom;
  end;
end;

procedure TEchoContext.OnDisconnected;
begin
  if FFileWritter <> nil then
  begin
    FFileWritter.Flush;
  end;
  inherited;
end;

procedure TEchoContext.WriteRecvData(pvBuf:Pointer; pvLength:Integer);
begin
  if FObjectID = 0 then
  begin
    FObjectID := InterlockedIncrement(__SN);
  end;
  if FFileWritter = nil then
  begin
    FFileWritter := TSingleFileWriter.Create;
    FFileWritter.FilePreFix := Format('recv_%d_', [FObjectID]);
  end;
  FFileWritter.WriteBuffer(pvBuf, pvLength);
end;

end.
