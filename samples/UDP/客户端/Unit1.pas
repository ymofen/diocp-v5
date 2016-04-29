unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,diocp_udp, utils_safeLogger;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FDiocpUdp: TDiocpUdp;
  public
    { Public declarations }
    procedure OnRecv(pvReqeust:TDiocpUdpRecvRequest);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
procedure TForm1.OnRecv(pvReqeust:TDiocpUdpRecvRequest);
var
  s:AnsiString;
begin
    s := PAnsiChar(pvReqeust.RecvBuffer);
    s[pvReqeust.RecvBufferLen + 1] := #0;
    sfLogger.logMessage(s);
    Edit2.Text:=pvReqeust.RemoteAddr;
    Edit3.Text:=IntToStr(pvReqeust.RemotePort);
end;  

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FDiocpUdp.Active then
  begin
    FDiocpUdp.Stop();
    Button1.Caption := '点击开启';
  end else
  begin
    FDiocpUdp.DefaultListener.Port := StrToInt(Edit1.Text);
    FDiocpUdp.Start();
    Button1.Caption := '点击关闭';
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s:AnsiString;
  lvBuffer:Pointer;
begin
    s := '客户端发送的消息';
    lvBuffer := PAnsiChar(s);
    FDiocpUdp.WSASendTo(Edit2.Text,StrToInt(Edit3.Text),lvBuffer,Length(s),true);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  sfLogger.setAppender(TStringsAppender.Create(Memo1.Lines));
  sfLogger.AppendInMainThread := true;
  FDiocpUdp := TDiocpUdp.Create(Self);
  FDiocpUdp.OnRecv := OnRecv;
end;

end.
