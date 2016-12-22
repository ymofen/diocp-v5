unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, utils_yunlogger, StdCtrls, ShellAPI, ExtCtrls, utils_diocpYun,
  utils_dvalue, utils_strings;

type
  TForm1 = class(TForm)
    btnWriterLog: TButton;
    btnViewLog: TButton;
    edtAccessToken: TEdit;
    edtLogContent: TEdit;
    tmrLog: TTimer;
    btnStartTimer: TButton;
    btnSetValue: TButton;
    edtPath: TEdit;
    edtValue: TEdit;
    btnViewStorage: TButton;
    btnSetJSON: TButton;
    btnSetHost: TButton;
    edtHost: TEdit;
    btnWriteLog1000: TButton;
    btnSetValue1000: TButton;
    procedure btnSetHostClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSetValueClick(Sender: TObject);
    procedure btnStartTimerClick(Sender: TObject);
    procedure btnViewLogClick(Sender: TObject);
    procedure btnViewStorageClick(Sender: TObject);
    procedure btnSetJSONClick(Sender: TObject);
    procedure btnSetValue1000Click(Sender: TObject);
    procedure btnWriteLog1000Click(Sender: TObject);
    procedure btnWriterLogClick(Sender: TObject);
    procedure tmrLogTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
var
  __sn:Integer;


procedure TForm1.btnSetHostClick(Sender: TObject);
begin
  yunStorage.SetYunServer(Format('http://%s/api/yunStorageWriter', [edtHost.Text]));
  yunLogger.SetYunServer(Format('http://%s/api/yunlogger', [edtHost.Text]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  yunStorage.SetYunServer('http://127.0.0.1:32000/api/yunStorageWriter');
end;

procedure TForm1.btnSetValueClick(Sender: TObject);
begin
  yunStorage.SetValue(edtAccessToken.Text, edtPath.Text, edtValue.Text);

end;

procedure TForm1.btnStartTimerClick(Sender: TObject);
begin
  __sn := 0;
  tmrLog.Enabled := True;
end;

procedure TForm1.btnViewLogClick(Sender: TObject);
var
  s:String;
begin
  s := Format('http://%s/api/querylog?accesstoken=%s', [edtHost.Text, edtAccessToken.Text]);
  ShellExecute(0, 'open', PChar(s), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.btnViewStorageClick(Sender: TObject);
var
  s:String;
begin
  s := Format('http://%s/api/yunStorageQuery?accesstoken=%s&path=%s', [edtHost.Text, edtAccessToken.Text, edtPath.Text]);
  ShellExecute(0, 'open', PChar(s), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.btnSetJSONClick(Sender: TObject);
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create;
  lvDValue.Add('value', edtValue.Text);
  lvDValue.Add('time', NowString);
  yunStorage.SetJSON(edtAccessToken.Text, edtPath.Text, lvDValue);
  lvDValue.Free;
end;

procedure TForm1.btnSetValue1000Click(Sender: TObject);
var
  i: Integer;
  lvDValue:TDValue;
begin
  for i := 0 to 1000 - 1 do
  begin
    lvDValue := TDValue.Create;
    lvDValue.Add('value', edtValue.Text);
    lvDValue.Add('time', NowString);
    lvDValue.Add('sn', i);
    yunStorage.SetJSON(edtAccessToken.Text, edtPath.Text, lvDValue);
    lvDValue.Free;
  end;
end;

procedure TForm1.btnWriteLog1000Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 1000 - 1 do
  begin
    Inc(__sn);
    yunLogger.LogMessage(edtAccessToken.Text, Format('sn:%d, %s',[__sn, edtLogContent.Text]), 'dss-client', 'message');
  end;
end;

procedure TForm1.btnWriterLogClick(Sender: TObject);
begin
  yunLogger.LogMessage(edtAccessToken.Text, Format('sn:%d, %s',[__sn, edtLogContent.Text]), 'dss-client', 'message');
end;

procedure TForm1.tmrLogTimer(Sender: TObject);
begin
  Randomize;
  tmrLog.Enabled := false;
  Inc(__sn);
  yunLogger.LogMessage(edtAccessToken.Text, Format('sn:%d, %s',[__sn, edtLogContent.Text]), 'dss-client', 'message');
  tmrLog.Interval := (Random(60) + 1) * 1000;
  tmrLog.Enabled := True;

  self.Caption := Format('timer:%d', [tmrLog.Interval]);
end;

end.
