unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, utils_yunlogger, StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    btnWriterLog: TButton;
    btnViewLog: TButton;
    edtAccessToken: TEdit;
    edtLogContent: TEdit;
    procedure btnWriterLogClick(Sender: TObject);
    procedure btnViewLogClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnWriterLogClick(Sender: TObject);
begin
  yunLogger.LogMessage(edtAccessToken.Text, edtLogContent.Text, 'dss-client', 'message');
end;

procedure TForm1.btnViewLogClick(Sender: TObject);
var
  s:String;
begin
  s := 'http://123.232.98.202:9007/api/querylog?accesstoken=' + edtAccessToken.Text;
  ShellExecute(0, 'open', PChar(s), nil, nil, SW_SHOWNORMAL);
end;

end.
