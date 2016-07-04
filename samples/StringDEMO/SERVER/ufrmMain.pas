unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, diocp_tcp_server, ExtCtrls,
  ComCtrls, utils_safeLogger, diocp_ex_server;

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
    tmrKickOut: TTimer;
    chkLogDetails: TCheckBox;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnPostWSACloseClick(Sender: TObject);
    procedure chkLogDetailsClick(Sender: TObject);
    procedure tmrKickOutTimer(Sender: TObject);
  private
    iCounter:Integer;
    FTcpServer: TDiocpStringTcpServer;
    procedure RefreshState;

    procedure OnContextStringAction(pvContext: TDiocpStringContext; pvDataString:
        String);

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
  uFMMonitor, diocp_core_engine, diocp_core_rawWinSocket;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;
  TStringsAppender(sfLogger.Appender).MaxLines := 5000;
  
  FTcpServer := TDiocpStringTcpServer.Create(Self);
  FTcpServer.Name := 'iocpSVR';
  FTcpServer.OnContextStringAction := OnContextStringAction;
  FTcpServer.OnContextAccept := OnAccept;
  FTcpServer.createDataMonitor;
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
end;

destructor TfrmMain.Destroy;
begin
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
  FTcpServer.Port := StrToInt(edtPort.Text);
  
  // 设置最大数据包长度
  FTcpServer.SetMaxDataLen(1024);

  // 设置开始字符串
  FTcpServer.SetPackStartStr('#');

  // 设置结束字符串
  FTcpServer.SetPackEndStr('!');  

  FTcpServer.Active := true;
  
  RefreshState;
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
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
    FTcpServer.GetOnlineContextList(lvList);
    for i:=0 to lvList.Count -1 do
    begin
      FTcpServer.FindContext(TIocpClientContext(lvList[i]).SocketHandle);
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
    FTcpServer.GetOnlineContextList(lvList);
    for i:=0 to lvList.Count -1 do
    begin
      TIocpClientContext(lvList[i]).PostWSACloseRequest();
    end;
  finally
    lvList.Free;
  end;

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

procedure TfrmMain.OnContextStringAction(pvContext: TDiocpStringContext;
    pvDataString: String);
begin
  sfLogger.logMessage(pvDataString);

  // 可以把一个数据直接写回去
  pvContext.WriteAnsiString(pvDataString);
  //Sleep(1);
end;

procedure TfrmMain.tmrKickOutTimer(Sender: TObject);
begin
  FTcpServer.KickOut(30000);
end;

end.
