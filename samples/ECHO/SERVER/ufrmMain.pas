unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, diocp_tcp_server, ExtCtrls,
  ComCtrls, utils_safeLogger, utils_BufferPool, utils_fileWriter, System.Actions;

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
    procedure actOpenExecute(Sender: TObject);
    procedure actPushToAllExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnPoolInfoClick(Sender: TObject);
    procedure btnPostWSACloseClick(Sender: TObject);
    procedure btnReOpenTestClick(Sender: TObject);
    procedure chkLogDetailsClick(Sender: TObject);
    procedure tmrInfoTimer(Sender: TObject);
    procedure tmrKickOutTimer(Sender: TObject);
    procedure tmrTestTimer(Sender: TObject);
  private
    iCounter:Integer;
    FTcpServer: TDiocpTcpServer;
    FPool:PBufferPool;
    procedure RefreshState;
    procedure OnRecvBuffer(pvClientContext:TIocpClientContext; buf:Pointer;
        len:cardinal; errCode:Integer);

    procedure OnSendBufferCompleted(pvContext: TIocpClientContext; pvBuff: Pointer;
        len: Cardinal; pvBufferTag, pvErrorCode: Integer);

    procedure OnAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer; var
        vAllowAccept: Boolean);
    procedure OnDisconnected(pvClientContext: TIocpClientContext);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, diocp_core_engine, diocp_core_rawWinSocket;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;

  FTcpServer := TDiocpTcpServer.Create(Self);
  FTcpServer.Name := 'iocpSVR';
  FTcpServer.OnDataReceived := self.OnRecvBuffer;
  FTcpServer.OnContextAccept := OnAccept;
  FTcpServer.createDataMonitor;
  FTcpServer.OnSendBufferCompleted := OnSendBufferCompleted;
  FTcpServer.OnContextDisconnected := OnDisconnected;
  FPool := NewBufferPool(FTcpServer.WSARecvBufferSize);
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
end;

destructor TfrmMain.Destroy;
begin
  FreeBufferPool(FPool);
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
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  FTcpServer.WorkerCount := StrToInt(edtThread.Text);
  FTcpServer.Port := StrToInt(edtPort.Text);
  FTcpServer.OnDataReceived := self.OnRecvBuffer;
  FTcpServer.Active := true;
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
  FTcpServer.DisconnectAll;
  FTcpServer.SafeStop;
  RefreshState;
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

procedure TfrmMain.OnAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer;
    var vAllowAccept: Boolean);
begin
//  if pvAddr = '127.0.0.1' then
//    vAllowAccept := false;

end;

procedure TfrmMain.OnDisconnected(pvClientContext: TIocpClientContext);
begin
  if pvClientContext.Data <> nil then
  begin
    TObject(pvClientContext.Data).Free;
    pvClientContext.Data := nil;
  end;
end;

procedure TfrmMain.OnRecvBuffer(pvClientContext:TIocpClientContext;
    buf:Pointer; len:cardinal; errCode:Integer);
var
  j, i:Integer;
  s:AnsiString;
  lvBuff:PByte;
  lvFileWriter:TSingleFileWriter;
begin
  if errCode = 0 then
  begin
    if chkShowInMemo.Checked then
    begin
      //   如果客户端发送的为字符串，可以用下面代码进行显示
      SetLength(s, len);
      Move(buf^, s[1], len);
      sfLogger.logMessage(s);
    end;
    if chkEcho.Checked then
    begin

      lvBuff := GetBuffer(FPool);

      Move(buf^, lvBuff^, len);

      //
      AddRef(lvBuff);


      pvClientContext.PostWSASendRequest(lvBuff, len, dtNone, 1);
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

  end else
  begin
    pvClientContext.RequestDisconnect;
  end;
end;

procedure TfrmMain.OnSendBufferCompleted(pvContext: TIocpClientContext; pvBuff:
    Pointer; len: Cardinal; pvBufferTag, pvErrorCode: Integer);
begin
  if pvBufferTag = 1 then
    ReleaseRef(pvBuff);
end;

procedure TfrmMain.tmrInfoTimer(Sender: TObject);
begin
  self.Caption := Format('DIOCP 测试:%d, %d', [__DebugWSACreateCounter, __DebugWSACloseCounter]);
end;

procedure TfrmMain.tmrKickOutTimer(Sender: TObject);
begin
  FTcpServer.KickOut(30000);
end;

procedure TfrmMain.tmrTestTimer(Sender: TObject);
begin
  actStop.Execute;

  FTcpServer.IocpAcceptorMgr.MinRequest := 1000;
  FTcpServer.IocpAcceptorMgr.MaxRequest := 2000;

  Application.ProcessMessages;



  actOpen.Execute;

end;

end.
