unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, diocp.coder.tcpServer, diocp.tcp.server, ExtCtrls,
  ComObj, ComCtrls, utils.safeLogger;

type
  TfrmMain = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    actlstMain: TActionList;
    actOpen: TAction;
    actStop: TAction;
    actPushMsg: TAction;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    pnlMonitor: TPanel;
    mmoLog: TMemo;
    pnlTop: TPanel;
    btnDisconnectAll: TButton;
    actDisconnectAll: TAction;
    tsTest: TTabSheet;
    btnBufferTester: TButton;
    edtMsg: TEdit;
    btnPushMsg: TButton;
    tmrHeart: TTimer;
    procedure actDisconnectAllExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actPushMsgExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure tmrHeartTimer(Sender: TObject);
  private
    { Private declarations }
    FTcpServer: TDiocpCoderTcpServer;
    procedure refreshState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uFMMonitor, uDIOCPStreamCoder, uMyClientContext, CHATHandler;

{$R *.dfm}



constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TDiocpCoderTcpServer.Create(Self);
  FTcpServer.createDataMonitor;

  FTcpServer.WorkerCount := 3;
  // register decoder and encoder class
  FTcpServer.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);

  FTcpServer.registerContextClass(TMyClientContext);
  
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);

  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := true;
end;

destructor TfrmMain.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmMain.actDisconnectAllExecute(Sender: TObject);
begin
  FTcpServer.DisconnectAll;
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
end;

procedure TfrmMain.actPushMsgExecute(Sender: TObject);
var
  lvList:TList;
  i: Integer;
  lvStream:TMemoryStream;
  s:AnsiString;
begin
  lvList := TList.Create;
  try
    lvStream := TMemoryStream.Create;
    try
      s := edtMsg.Text;
      lvStream.Write(s[1], Length(s));

      // get all client context to List
      FTcpServer.getOnlineContextList(lvList);


      for i := 0 to lvList.Count-1 do
      begin
        //send stream object directly
        TIOCPCoderClientContext(lvList[i]).writeObject(lvStream);
      end;
    finally
      lvStream.Free;
    end;
  finally
    lvList.Free;
  end;

end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

procedure TfrmMain.tmrHeartTimer(Sender: TObject);
begin
  FTcpServer.KickOut(20000);
  ChatSessions.KickOut(20000);

end;

end.
