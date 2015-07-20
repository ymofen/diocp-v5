unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, diocp.core.rawWinSocket, StdCtrls, diocp.winapi.winsock2, diocp.udp, utils.safeLogger,
  ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    pgcMain: TPageControl;
    TabSheet1: TTabSheet;
    mmoOutput: TMemo;
    pnlTop: TPanel;
    btnStart: TButton;
    edtPort: TEdit;
    tsSend: TTabSheet;
    btnSend: TButton;
    edtRemoteHost: TEdit;
    edtRemotePort: TEdit;
    procedure btnSendClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
    FRawSocket:TRawSocket;
    FDiocpUdp: TDiocpUdp;
  public
    constructor Create(AOwner: TComponent); override;
    procedure OnRecv(pvReqeust:TDiocpUdpRecvRequest);


  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  sfLogger.setAppender(TStringsAppender.Create(mmoOutput.Lines));
  sfLogger.AppendInMainThread := true;
  FDiocpUdp := TDiocpUdp.Create(Self);
  FDiocpUdp.OnRecv := OnRecv;  
end;

procedure TForm1.btnSendClick(Sender: TObject);
var
  lvRawSocket:TRawSocket;
  s :AnsiString;
  lvBuffer:PAnsiChar;
  lvSocketAddr: diocp.winapi.winsock2.TSockAddrIn;

  i, socketlen, r: Integer;

begin
  lvRawSocket := TRawSocket.Create();
  lvRawSocket.CreateUdpOverlappedSocket();
  socketlen := SizeOf(TSockAddrIn);

  s := edtRemoteHost.Text;



  lvSocketAddr.sin_family := AF_INET;
  lvSocketAddr.sin_addr.S_addr := inet_addr(PAnsiChar(s));
  lvSocketAddr.sin_port := htons(StrToInt(edtRemotePort.Text));


  for i := 0 to 10 - 1 do
  begin
    s := IntToStr(i) + '.abc';
    lvBuffer := PAnsiChar(s);
    r := sendto(lvRawSocket.SocketHandle, lvBuffer^, Length(s), 0, @lvSocketAddr, socketlen);
    if r = -1 then
    begin
      RaiseLastOSError;
    end;
  end;
  ;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  FDiocpUdp.DefaultListener.Port := StrToInt(edtPort.Text);
  FDiocpUdp.Start();
end;

procedure TForm1.OnRecv(pvReqeust:TDiocpUdpRecvRequest);
var
  s:AnsiString;
begin
  s := PAnsiChar(pvReqeust.RecvBuffer);
  s[pvReqeust.RecvBufferLen + 1] := #0;
  sfLogger.logMessage(s);
  Sleep(1);
end;

end.
