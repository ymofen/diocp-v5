unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, iocpLogger, diocp.task, diocp.tcp.blockClient,
  uStreamCoderSocket, uRawTcpClientCoderImpl, Grids, DBGrids,
  uIRemoteServer, uRemoteServerDIOCPImpl, DB, DBClient;

type
  TfrmMain = class(TForm)
    mmoSQL: TMemo;
    btnConnect: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    DBGrid1: TDBGrid;
    btnOpen: TButton;
    cdsMain: TClientDataSet;
    dsMain: TDataSource;
    procedure btnConnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    { Private declarations }
    FRemoteSvrObj:TRemoteServerDIOCPImpl;
    FRemoteSvr:IRemoteServer;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation





{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FRemoteSvrObj := TRemoteServerDIOCPImpl.Create;
  FRemoteSvr := FRemoteSvrObj;
end;

destructor TfrmMain.Destroy;
begin
  FRemoteSvr := nil;
  inherited Destroy;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  FRemoteSvrObj.setHost(edtHost.Text);
  FRemoteSvrObj.setPort(StrToInt(edtPort.Text));
  FRemoteSvrObj.Open();
  ShowMessage('open succ!');
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
var
  vData:OleVariant;
  l : Cardinal;
begin
  vData := mmoSQL.Lines.Text;

  l := GetTickCount;
  if FRemoteSvr.Execute(1, vData) then
  begin
    self.cdsMain.Data := vData;
    Self.Caption := Format('query: count:%d, time:%d',
      [self.cdsMain.RecordCount, GetTickCount - l]);
  end;
end;

end.
