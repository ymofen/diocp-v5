unit ufrmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  Data.DB, Datasnap.DBClient, FMX.Layouts, FMX.Grid, FMX.StdCtrls, FMX.Edit,
  uIRemoteServer, uRemoteServerDIOCPImpl, FMX.Memo, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.DBScope, Data.Bind.Grid;

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    dsMain: TDataSource;
    cdsMain: TClientDataSet;
    mmoSQL: TMemo;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    BindSourceDB1: TBindSourceDB;
    StringGrid1: TStringGrid;
    pnlTopSet: TPanel;
    edtPort: TEdit;
    edtHost: TEdit;
    btnConnect: TButton;
    btnOpen: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    { Private declarations }
    FRemoteSvrObj:TRemoteServerDIOCPImpl;
    FRemoteSvr:IRemoteServer;
  public
    constructor Create(AOwner: TComponent); override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

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
  if FRemoteSvr.Execute(1, vData) then
  begin
    self.cdsMain.Data := vData;
  end;
end;



constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FRemoteSvrObj := TRemoteServerDIOCPImpl.Create;
  FRemoteSvr := FRemoteSvrObj;
end;

end.
