unit ufrmMain;

interface



uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, diocp_tcp_server, ExtCtrls,
  ComCtrls, utils_safeLogger, utils_BufferPool, utils_fileWriter, utils_async;

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
    btnPostWSAClose: TButton;
    btnReOpenTest: TButton;
    tmrKickOut: TTimer;
    tmrTest: TTimer;
    tmrInfo: TTimer;
    chkLogDetails: TCheckBox;
    tsOperator: TTabSheet;
    mmoPushData: TMemo;
    btnPushToAll: TButton;
    actPushToAll: TAction;
    btnPoolInfo: TButton;
    edtThread: TEdit;
    chkEcho: TCheckBox;
    chkShowInMemo: TCheckBox;
    chkSaveToFile: TCheckBox;
    chkUseContextPool: TCheckBox;
    chkUseBufferPool: TCheckBox;
    btnASyncPush: TButton;
    btnFill4K: TButton;
    btnTest: TButton;
    btnASyncStop: TButton;
    tsOpt: TTabSheet;
    edtSendSize: TEdit;
    chkSendForerver: TCheckBox;
    edtMaxSendSize: TEdit;
    btnStartSend: TButton;
    chkNagle: TCheckBox;
    edtSendBufLen: TEdit;
    chkChangeSendBufSize: TCheckBox;
    chkSleepOnRecvSend: TCheckBox;
    edtRecvSendSleep: TEdit;
    procedure actOpenExecute(Sender: TObject);
    procedure actPushToAllExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnASyncPushClick(Sender: TObject);
    procedure btnASyncStopClick(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnFill4KClick(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnPoolInfoClick(Sender: TObject);
    procedure btnPostWSACloseClick(Sender: TObject);
    procedure btnReOpenTestClick(Sender: TObject);
    procedure btnStartSendClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure chkChangeSendBufSizeClick(Sender: TObject);
    procedure chkEchoClick(Sender: TObject);
    procedure chkLogDetailsClick(Sender: TObject);
    procedure chkSaveToFileClick(Sender: TObject);
    procedure chkSendForerverClick(Sender: TObject);
    procedure chkShowInMemoClick(Sender: TObject);
    procedure chkSleepOnRecvSendClick(Sender: TObject);
    procedure chkUseBufferPoolClick(Sender: TObject);
    procedure edtMaxSendSizeKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure edtSendSizeKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure tmrInfoTimer(Sender: TObject);
    procedure tmrKickOutTimer(Sender: TObject);
    procedure tmrTestTimer(Sender: TObject);
  private
    FIsStop:Boolean;
    iCounter:Integer;
    FChkUseBufferPool:Boolean;
    FChkEcho:Boolean;
    FChkShowInMemo:Boolean;
    FChkSaveToFile:Boolean;
    FChkSendForEver:Boolean;
    FChkSendWithThread:Boolean;
    FChkSleepOnRecvSend:Boolean;
    FSleepOnRSInterval:Integer;
    FSendBlockSize:Integer;
    FSendQueueSize:Integer;
    FTcpServer: TDiocpTcpServer;
    FPool:PBufferPool;
    FAsync: TASyncInvoker;
    function InnerDoPostSend(pvContext: TIocpClientContext): Boolean;
    procedure ReadState;
    procedure RefreshState;
    procedure OnRecvBuffer(pvClientContext:TIocpClientContext; buf:Pointer;
        len:cardinal; errCode:Integer);

    procedure OnSendBufferCompleted(pvContext: TIocpClientContext; pvBuff: Pointer;
        len: Cardinal; pvBufferTag: Integer; pvTagData: Pointer; pvErrorCode:
        Integer);

    procedure OnAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer; var
        vAllowAccept: Boolean);
    procedure OnDisconnected(pvClientContext: TIocpClientContext);

    procedure OnASyncWorker(pvSender:TObject);

    procedure OnASyncDo(pvASyncWorker: TASyncWorker);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, diocp_core_engine, diocp_core_rawWinSocket, utils_strings;

{$R *.dfm}

type
  TCrackTcpSvr = class(TDiocpTcpServer);

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;
  FAsync := TASyncInvoker.Create();
  FTcpServer := TDiocpTcpServer.Create(Self);
  FTcpServer.Name := 'iocpSVR';
  FTcpServer.OnDataReceived := self.OnRecvBuffer;
  FTcpServer.OnContextAccept := OnAccept;
  FTcpServer.createDataMonitor;
  FTcpServer.OnSendBufferCompleted := OnSendBufferCompleted;
  FTcpServer.OnContextDisconnected := OnDisconnected;
  FPool := NewBufferPool(FTcpServer.WSARecvBufferSize);
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
  ReadState;

end;

destructor TfrmMain.Destroy;
begin
  FIsStop := true;
  FAsync.Terminate;
  FAsync.WaitForStop;
  FTcpServer.SafeStop; 
  FreeBufferPool(FPool);
  FTcpServer.Free;
  FAsync.Free;

  inherited Destroy;
end;

procedure TfrmMain.RefreshState;
begin
  if FTcpServer.Active then
  begin
    btnOpen.Action := actStop;
  end else
  begin
    btnOpen.Action := actOpen;
  end;
  chkUseContextPool.Enabled := not FTcpServer.Active;
  edtPort.Enabled := not FTcpServer.Active;
  edtThread.Enabled := not FTcpServer.Active;
  chkNagle.Enabled := not FTcpServer.Active;
  chkChangeSendBufSize.Enabled := not FTcpServer.Active;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  FTcpServer.WorkerCount := StrToInt(edtThread.Text);
  FTcpServer.Port := StrToInt(edtPort.Text);
  FTcpServer.OnDataReceived := self.OnRecvBuffer;
  FTcpServer.UseObjectPool := chkUseContextPool.Checked;
  if chkNagle.Checked then
  begin
    FTcpServer.NoDelayOption := False;
  end else
  begin
    FTcpServer.NoDelayOption := True;
  end;

  FSendBlockSize := StrToInt(edtSendSize.Text);
  FSendQueueSize := StrToInt(edtMaxSendSize.Text);
  if chkChangeSendBufSize.Checked then
  begin
    FTcpServer.SendBufCacheSize :=StrToInt(edtSendBufLen.Text);
  end;
  FTcpServer.Active := true;



  
  FIsStop := false;
  FAsync.Start(Self.OnASyncDo);
  RefreshState;
end;

procedure TfrmMain.actPushToAllExecute(Sender: TObject);
var
  ansiStr:AnsiString;
var
  lvList:TList;
  i:Integer;
  lvContext:TIocpClientContext;
begin
  ansiStr := mmoPushData.Lines.Text;
  lvList := TList.Create;
  try
    FTcpServer.getOnlineContextList(lvList);
    for i:=0 to lvList.Count -1 do
    begin
      lvContext := TIocpClientContext(lvList[i]);
      lvContext.PostWSASendRequest(PAnsiChar(ansiStr), Length(ansiStr));
    end;
  finally
    lvList.Free;
  end;
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FIsStop := true;
  FTcpServer.SafeStop;
  FAsync.Terminate;
  FAsync.WaitForStop;
  RefreshState;
end;

procedure TfrmMain.btnASyncPushClick(Sender: TObject);
begin
  btnASyncPush.Tag := 1;
  ASyncExecute(OnASyncWorker, nil);
end;

procedure TfrmMain.btnASyncStopClick(Sender: TObject);
begin
  btnASyncPush.Tag := 0;
end;

procedure TfrmMain.btnDisconectAllClick(Sender: TObject);
begin
  FTcpServer.DisconnectAll;
end;

procedure TfrmMain.btnFill4KClick(Sender: TObject);
var
  s:string;
begin
  SetLength(s, 4096);
  FillChar(PChar(s)^, 4096, '1');
  mmoPushData.Lines.Text := s;
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
      FTcpServer.findContext(TIocpClientContext(lvList[i]).SocketHandle);
    end;
  finally
    lvList.Free;
  end;

end;

procedure TfrmMain.btnGetWorkerStateClick(Sender: TObject);
begin
  ShowMessage(FTcpServer.IocpEngine.getWorkerStateInfo(0));
end;

procedure TfrmMain.btnPoolInfoClick(Sender: TObject);
var
  s:string;
  r:Integer;
begin
  if FPool = nil then Exit;
  s :=Format('get:%d, put:%d, addRef:%d, releaseRef:%d, size:%d',
    [FPool.FGet, FPool.FPut, FPool.FAddRef, FPool.FReleaseRef, FPool.FSize]);
  r := CheckBufferBounds(FPool);
  s := s + sLineBreak + Format('池中共有:%d个内存块, 可能[%d]个内存块写入越界的情况', [FPool.FSize, r]);
  ShowMessage(s);
end;

procedure TfrmMain.btnPostWSACloseClick(Sender: TObject);
var
  lvList:TList;
  i:Integer;
begin
  lvList := TList.Create;
  try
    FTcpServer.getOnlineContextList(lvList);
    for i:=0 to lvList.Count -1 do
    begin
      TIocpClientContext(lvList[i]).PostWSACloseRequest();
    end;
  finally
    lvList.Free;
  end;

end;

procedure TfrmMain.btnReOpenTestClick(Sender: TObject);
begin
  FTcpServer.logMessage('DoHeartBeatChcek', 'DEBUG', lgvDebug);
  tmrTest.Enabled := not tmrTest.Enabled;
end;

procedure TfrmMain.btnStartSendClick(Sender: TObject);
var
  ansiStr:AnsiString;
var
  lvList:TList;
  i:Integer;
  lvContext:TIocpClientContext;
begin
  FSendBlockSize := StrToInt(edtSendSize.Text);
  FSendQueueSize := StrToInt(edtMaxSendSize.Text);
  ansiStr := mmoPushData.Lines.Text;
  lvList := TList.Create;
  try
    FTcpServer.getOnlineContextList(lvList);
    for i:=0 to lvList.Count -1 do
    begin
      lvContext := TIocpClientContext(lvList[i]);
      InnerDoPostSend(lvContext);
    end;
  finally
    lvList.Free;
  end;

end;

procedure TfrmMain.btnTestClick(Sender: TObject);
var
  lvRequest:TIocpSendRequest;
  i, r32: Integer;
  lvList:TList;
  lvBuf:Pointer;
  ansiStr:AnsiString;
  i64, r64:Int64;
begin
  i := 0;
  i64 := 0;

  r32 := AtomicIncrement(i);

  r64 := AtomicAdd64(i64, 2);

  ShowMessage(format('r32:%d(%d), r64:%d, i64:%d', [r32, i, r64, i64]));
  ansiStr := mmoPushData.Lines.Text;
  lvList := TList.Create;
  try
    for i := 0 to 102400 - 1 do
    begin
      lvRequest := TCrackTcpSvr(FTcpServer).GetSendRequest;
      GetMem(lvBuf, Length(ansiStr));
      lvRequest.SetBuffer(lvBuf, Length(ansiStr), dtFreeMem);

      lvList.Add(lvRequest);
    end;

    for i := 0 to lvList.Count - 1 do
    begin
      TCrackTcpSvr(FTcpServer).ReleaseSendRequest(TIocpSendRequest(lvList[i]){$IFDEF DIOCP_DEBUG},nil{$ENDIF});
    end;

    TCrackTcpSvr(FTcpServer).DoCleanUpSendRequest;
  finally
    lvList.Free;
  end;

end;

procedure TfrmMain.chkChangeSendBufSizeClick(Sender: TObject);
begin
  edtSendBufLen.Enabled := chkChangeSendBufSize.Checked;
end;



procedure TfrmMain.chkLogDetailsClick(Sender: TObject);
begin
  if chkLogDetails.Checked then
  begin
    FTcpServer.Logger.LogFilter := LogAllLevels;
  end else
  begin
    FTcpServer.Logger.LogFilter := [lgvError];     // 只记录致命错误
  end;
end;

procedure TfrmMain.chkEchoClick(Sender: TObject);
begin
  ReadState;
end;

procedure TfrmMain.chkSaveToFileClick(Sender: TObject);
begin
  ReadState;
end;

procedure TfrmMain.chkSendForerverClick(Sender: TObject);
begin
  FChkSendForEver := chkSendForerver.Checked;
  edtSendSize.Enabled := FChkSendForEver;
  if FChkSendForEver then
  begin
    FSendBlockSize := StrToInt(edtSendSize.Text);
    FSendQueueSize := StrToInt(edtMaxSendSize.Text);
  end;
end;

procedure TfrmMain.chkShowInMemoClick(Sender: TObject);
begin
  ReadState;
end;

procedure TfrmMain.chkSleepOnRecvSendClick(Sender: TObject);
begin
  FChkSleepOnRecvSend := chkSleepOnRecvSend.Checked;
  FSleepOnRSInterval := StrToInt(edtRecvSendSleep.Text);
end;

procedure TfrmMain.chkUseBufferPoolClick(Sender: TObject);
begin
  ReadState;
end;

procedure TfrmMain.edtMaxSendSizeKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if Key = 13 then
  begin
    FSendQueueSize := StrToInt(edtMaxSendSize.Text);
  end;
end;

procedure TfrmMain.edtSendSizeKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if Key = 13 then
  begin
    FSendBlockSize := StrToInt(edtSendSize.Text);
  end;
end;

function TfrmMain.InnerDoPostSend(pvContext: TIocpClientContext): Boolean;
var
  lvBuf: Pointer;
  lvTime: AnsiString;
begin
  Result := True;
  if FChkUseBufferPool then
  begin
    lvBuf := GetBuffer(FSendBlockSize);
    FillChar(lvBuf^, FSendBlockSize, Ord('='));


    lvTime := NowString;
    if FSendBlockSize > Length(lvTime) then
    begin
      Move(PAnsiChar(lvTime)^, lvBuf^, Length(lvTime));
    end;
    AddRef(lvBuf);
    if not pvContext.PostWSASendRequest(lvBuf, FSendBlockSize, dtNone, 1) then
    begin
      ReleaseRef(lvBuf);
      Result := false;
    end;
  end else
  begin
    GetMem(lvBuf, FSendBlockSize);
    FillChar(lvBuf^, FSendBlockSize, Ord('='));


    lvTime := NowString;
    if FSendBlockSize > Length(lvTime) then
    begin
      Move(PAnsiChar(lvTime)^, lvBuf^, Length(lvTime));
    end;

    Result := pvContext.PostWSASendRequest(lvBuf, FSendBlockSize, dtFreeMem, 0);
  end;
end;

procedure TfrmMain.OnAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer;
    var vAllowAccept: Boolean);
begin
//  if pvAddr = '127.0.0.1' then
//    vAllowAccept := false;

end;

procedure TfrmMain.OnASyncDo(pvASyncWorker: TASyncWorker);
begin
  while not FAsync.Terminated do
  begin
//    lvList := TList.Create;
//    try
//      FTcpServer.getOnlineContextList(lvList);
//      for i:=0 to lvList.Count -1 do
//      begin
//        lvContext := TIocpClientContext(lvList[i]);
//        lvContext.PostWSASendRequest(PAnsiChar(ansiStr), Length(ansiStr));
//      end;
//    finally
//      lvList.Free;
//    end;
     Sleep(100);
  end;
end;

procedure TfrmMain.OnASyncWorker(pvSender:TObject);
var
  ansiStr:AnsiString;
var
  lvList:TList;
  i:Integer;
  lvContext:TIocpClientContext;
begin
  ansiStr := mmoPushData.Lines.Text;
  while self.FTcpServer.Active and (btnASyncPush.Tag = 1) do
  begin
    lvList := TList.Create;
    try
      FTcpServer.getOnlineContextList(lvList);
      for i:=0 to lvList.Count -1 do
      begin
        lvContext := TIocpClientContext(lvList[i]);
        lvContext.PostWSASendRequest(PAnsiChar(ansiStr), Length(ansiStr));
      end;
    finally
      lvList.Free;
    end;
    Sleep(1);
  end;
end;

procedure TfrmMain.OnDisconnected(pvClientContext: TIocpClientContext);
begin
  if chkLogDetails.Checked then
  begin
    sfLogger.logMessage(Format('%s:%d-%s', [pvClientContext.RemoteAddr, pvClientContext.RemotePort, pvClientContext.DisconnectedReason]));
  end;
  if pvClientContext.Data <> nil then
  begin
    //TSingleFileWriter(pvClientContext.Data).Flush;
    TObject(pvClientContext.Data).Free;
    pvClientContext.Data := nil;
  end;
end;

procedure TfrmMain.OnRecvBuffer(pvClientContext:TIocpClientContext;
    buf:Pointer; len:cardinal; errCode:Integer);
var
  j, i, r:Integer;
  s:AnsiString;
  lvBuff:PByte;
  lvFileWriter:TSingleFileWriter;
begin
  if FChkSleepOnRecvSend then
  begin
    Sleep(FSleepOnRSInterval);
  end;
  
    if FChkShowInMemo then
    begin
      //   如果客户端发送的为字符串，可以用下面代码进行显示
      SetLength(s, len);
      Move(buf^, s[1], len);
      sfLogger.logMessage(s);
    end;
    if FChkEcho then
    begin
      if FChkUseBufferPool then
      begin   
        lvBuff := GetBuffer(FPool);
        Assert(len <= FPool.FBlockSize, 'err');        

        Move(buf^, lvBuff^, len);
        
        {$IFNDEF SPEED_TEST}
        r := CheckBlockBufferBounds(lvBuff);
        if r = 1 then
        begin
          //r := CheckBufferBounds(lvBuff);
          Assert(false, Format('r:%d, len:%d', [r, len]));
        end;
        {$ENDIF}

        //
        AddRef(lvBuff); 
        if not pvClientContext.PostWSASendRequest(lvBuff, len, dtNone, 1) then
        begin
          ReleaseRef(lvBuff);
        end;
      end else
      begin
        pvClientContext.PostWSASendRequest(buf, len);      
      end;
    end;

    if chkSaveToFile.Checked then
    begin
      lvFileWriter := TSingleFileWriter(pvClientContext.Data);
      if lvFileWriter = nil then
      begin
        lvFileWriter := TSingleFileWriter.Create;
        pvClientContext.Data := lvFileWriter;
        lvFileWriter.FilePreFix := Format('RECV_%d', [pvClientContext.SocketHandle]);
        lvFileWriter.FilePerSize := 1024 * 1024 * 100;
      end;

      lvFileWriter.WriteBuffer(buf, len);
    end;
end;

procedure TfrmMain.OnSendBufferCompleted(pvContext: TIocpClientContext; pvBuff:
    Pointer; len: Cardinal; pvBufferTag: Integer; pvTagData: Pointer;
    pvErrorCode: Integer);
{$IFNDEF SPEED_TEST}
var
  r: Integer;
{$ENDIF}

var
  lvBuf:Pointer;
  lvTime:AnsiString;
begin
  if FChkSleepOnRecvSend then
  begin
    Sleep(FSleepOnRSInterval);
  end;
  
  if pvBufferTag = 1 then
  begin
    {$IFNDEF SPEED_TEST}
    r := CheckBlockBufferBounds(pvBuff);
    if r = 1 then
    begin
      Assert(false, Format('r:%d, len:%d', [r, len]));
    end;
    {$ENDIF}
    ReleaseRef(pvBuff);
  end;

  if pvErrorCode = 0 then
  begin
    if FChkSendForEver then
    begin
      {$IFDEF DIRECT_SEND}
      if FSendQueueSize > 0 then
      begin        
        while (pvContext.SendQueueSize < FSendQueueSize) and (not FIsStop) do
        begin
          if not InnerDoPostSend(pvContext) then
          begin
            Break;  // 投递失败了
          end; 
        end;
      end else
      {$ENDIF}
      begin
        InnerDoPostSend(pvContext);
      end;
    end;
    
  end;
end;

procedure TfrmMain.ReadState;
begin
  FChkEcho := chkEcho.Checked;
  FChkShowInMemo := chkShowInMemo.Checked;
  FChkUseBufferPool := chkUseBufferPool.Checked;
  FChkSaveToFile := chkSaveToFile.Checked;
end;

procedure TfrmMain.tmrInfoTimer(Sender: TObject);
begin
  //self.Caption := Format('DIOCP 测试:%d, %d', [__DebugWSACreateCounter, __DebugWSACloseCounter]);
end;

procedure TfrmMain.tmrKickOutTimer(Sender: TObject);
begin
  FTcpServer.KickOut(30000);
end;

procedure TfrmMain.tmrTestTimer(Sender: TObject);
begin
  actStop.Execute;


  Application.ProcessMessages;



  actOpen.Execute;

end;

end.
