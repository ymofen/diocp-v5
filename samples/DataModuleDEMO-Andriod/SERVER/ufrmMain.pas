unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, diocp.coder.tcpServer, ExtCtrls,
  ComObj, ComCtrls, uMyClientContext,utils.zipTools;

type
  TfrmMain = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    actlstMain: TActionList;
    actOpen: TAction;
    actStop: TAction;
    actPushMsg: TAction;
    pgcMain: TPageControl;
    tsMoniter: TTabSheet;
    pnlMonitor: TPanel;
    Button1: TButton;
    procedure actOpenExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  uFMMonitor, uDIOCPStreamCoder, uRunTimeINfoTools, diocp.task;

{$R *.dfm}



constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTcpServer := TDiocpCoderTcpServer.Create(Self);
  //FTcpServer.KeepAlive := true;
  FTcpServer.createDataMonitor;
  // register decoder and encoder class
  FTcpServer.registerCoderClass(TIOCPStreamDecoder, TIOCPStreamEncoder);

  FTcpServer.registerContextClass(TMyClientContext);
  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
  FTcpServer.LogicWorkerNeedCoInitialize := true;
end;

destructor TfrmMain.Destroy;
begin
  iocpTaskManager.Enable := false;
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
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  FTcpServer.safeStop;
  refreshState;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  lvStream, lvStream2:TMemoryStream;
  s:String;
begin
  lvStream := TMemoryStream.Create;
  lvStream2 := TMemoryStream.Create;
  try
    s := '0123456789';
    lvStream.Write(s[1], Length(s) * SizeOf(Char));
    TZipTools.ZipStream(lvStream, lvStream);

    TZipTools.UnZipStream(lvStream, lvStream);

    ShowMessage(IntToStr(lvStream.Size));


  finally
    lvStream.Free;
    lvStream2.Free;
  end;

end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FTcpServer.SafeStop;
end;

end.
