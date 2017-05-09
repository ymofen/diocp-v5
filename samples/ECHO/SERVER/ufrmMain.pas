unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, diocp_tcp_server, ExtCtrls,
  ComCtrls, utils_safeLogger, utils_BufferPool, utils_fileWriter;

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
    procedure actOpenExecute(Sender: TObject);
    procedure actPushToAllExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnPoolInfoClick(Sender: TObject);
    procedure btnPostWSACloseClick(Sender: TObject);
    procedure btnReOpenTestClick(Sender: TObject);
    procedure chkEchoClick(Sender: TObject);
    procedure chkLogDetailsClick(Sender: TObject);
    procedure chkSaveToFileClick(Sender: TObject);
    procedure chkShowInMemoClick(Sender: TObject);
    procedure chkUseBufferPoolClick(Sender: TObject);
    procedure tmrInfoTimer(Sender: TObject);
    procedure tmrKickOutTimer(Sender: TObject);
    procedure tmrTestTimer(Sender: TObject);
  private
    iCounter:Integer;
    FChkUseBufferPool:Boolean;
    FChkEcho:Boolean;
    FChkShowInMemo:Boolean;
    FChkSaveToFile:Boolean;
    FTcpServer: TDiocpTcpServer;
    FPool:PBufferPool;
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
  ReadState;
end;

destructor TfrmMain.Destroy;
begin
  FTcpServer.SafeStop;
  FreeBufferPool(FPool);
  FTcpServer.Free;
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
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  FTcpServer.WorkerCount := StrToInt(edtThread.Text);
  FTcpServer.Port := StrToInt(edtPort.Text);
  FTcpServer.OnDataReceived := self.OnRecvBuffer;
  FTcpServer.UseObjectPool := chkUseContextPool.Checked;
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
  FTcpServer.SafeStop;
  RefreshState;
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

procedure TfrmMain.chkEchoClick(Sender: TObject);
begin
  ReadState;
end;

procedure TfrmMain.chkSaveToFileClick(Sender: TObject);
begin
  ReadState;
end;

procedure TfrmMain.chkShowInMemoClick(Sender: TObject);
begin
  ReadState;
end;

procedure TfrmMain.chkUseBufferPoolClick(Sender: TObject);
begin
  ReadState;
end;

procedure TfrmMain.OnAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer;
    var vAllowAccept: Boolean);
begin
//  if pvAddr = '127.0.0.1' then
//    vAllowAccept := false;

end;

procedure TfrmMain.OnDisconnected(pvClientContext: TIocpClientContext);
begin
  sfLogger.logMessage(Format('%s:%d-%s', [pvClientContext.RemoteAddr, pvClientContext.RemotePort, pvClientContext.DisconnectedReason]));
  if pvClientContext.Data <> nil then
  begin
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

    if FChkShowInMemo then
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
begin
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
  self.Caption := Format('DIOCP 测试:%d, %d', [__DebugWSACreateCounter, __DebugWSACloseCounter]);
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
