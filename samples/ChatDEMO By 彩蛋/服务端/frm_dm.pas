unit frm_dm;

interface

uses
  Classes, DB, ADODB;

type
  Tdm = class(TDataModule)
    conn: TADOConnection;
    qryTemp: TADOQuery;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetAllUser: Integer;
  end;

var
  dm: Tdm;

implementation

{$R *.dfm}

{ Tdm }

procedure Tdm.DataModuleCreate(Sender: TObject);
begin
  conn.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=tm.mdb;Persist Security Info=False';
end;

function Tdm.GetAllUser: Integer;
begin
  with qryTemp do
  begin
    Close;
    SQL.Text := 'SELECT userid, username From Users';
    Open;
  end;
  Result := qryTemp.RecordCount;
end;

end.
