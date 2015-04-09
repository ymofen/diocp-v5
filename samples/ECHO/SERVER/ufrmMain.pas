unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, diocp.tcp.server, ExtCtrls,
  ComCtrls, utils.safeLogger;

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
    Button1: TButton;
    tmrKickOut: TTimer;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnPostWSACloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tmrKickOutTimer(Sender: TObject);
  private
    iCounter:Integer;
    FTcpServer: TDiocpTcpServer;
    procedure refreshState;
    procedure OnRecvBuffer(pvClientContext:TIocpClientContext; buf:Pointer;
        len:cardinal; errCode:Integer);
    procedure OnAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer; var
        vAllowAccept: Boolean);
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

  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;

  FTcpServer := TDiocpTcpServer.Create(Self);
  FTcpServer.Name := 'iocpSVR';
  FTcpServer.OnDataReceived := self.OnRecvBuffer;
  FTcpServer.OnContextAccept := OnAccept;
  FTcpServer.createDataMonitor;
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
end;

destructor TfrmMain.Destroy;
begin
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
  FTcpServer.OnDataReceived := self.OnRecvBuffer;
  FTcpServer.Active := true;
  refreshState;
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

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  FTcpServer.logMessage('DoHeartBeatChcek', 'DEBUG', lgvDebug);
end;

procedure TfrmMain.OnAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer;
    var vAllowAccept: Boolean);
begin
//  if pvAddr = '127.0.0.1' then
//    vAllowAccept := false;

end;

procedure TfrmMain.OnRecvBuffer(pvClientContext:TIocpClientContext;
    buf:Pointer; len:cardinal; errCode:Integer);
var
  j, i:Integer;
  s:AnsiString;
begin
  if errCode = 0 then
  begin
// 如果客户端发送的为字符串，可以用下面代码进行显示
//    SetLength(s, len);
//    Move(buf^, s[1], len);
//    sfLogger.logMessage(s);
    pvClientContext.PostWSASendRequest(buf, len);

  end else
  begin
    pvClientContext.RequestDisconnect;
  end;
end;

procedure TfrmMain.tmrKickOutTimer(Sender: TObject);
begin
  FTcpServer.KickOut(30000);
end;

end.
