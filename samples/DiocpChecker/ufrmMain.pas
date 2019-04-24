unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, diocp_core_engine, diocp_core_rawWinSocket,
  diocp_sockets, utils_safeLogger, diocp_winapi_winsock2,
  diocp_sockets_utils, diocp_tcp_server, utils_async;

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    mmoInfo: TMemo;
    btnRefresh: TButton;
    btnStart: TButton;
    btnCreateClientSocket: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    btnPostRecvRequest: TButton;
    btnCloseSocket: TButton;
    chkShutDown: TCheckBox;
    btnCancelIoEx: TButton;
    btnShutDown: TButton;
    procedure btnCancelIoExClick(Sender: TObject);
    procedure btnCloseIOHandleClick(Sender: TObject);
    procedure btnCreateClientSocketClick(Sender: TObject);
    procedure btnPostRecvRequestClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnCloseSocketClick(Sender: TObject);
    procedure btnShutDownClick(Sender: TObject);
  private
    { Private declarations }
    FRawSocket: TRawSocket;
    FCacnelIoExRequest:TIocpRequest;
    FRecvRequest:TIocpRequest;
    FRecvBuffer: diocp_winapi_winsock2.TWsaBuf;
    FBuff:array[0..4096-1] of AnsiChar;
    FNumberOfBytesRecvd:Cardinal;
    FRecvdFlag: Cardinal;
    procedure OnRecvResponse(pvSender:TObject);
    procedure OnCancelIoExResponse(pvSender:TObject);
    procedure InnerPostRecvRequest(Sender: TObject);
    procedure InnerPostCacnelIoExRequest(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  sfLogger.setAppender(TStringsAppender.Create(mmoInfo.Lines));
  sfLogger.AppendInMainThread := True;
  FRecvRequest := TIocpRequest.Create;
  FRecvRequest.OnResponse := OnRecvResponse;
  FRecvBuffer.len := Length(FBuff);
  FRecvBuffer.buf := @FBuff[0];

  FCacnelIoExRequest := TIocpRequest.Create;
  FCacnelIoExRequest.OnResponse := OnCancelIoExResponse;
end;


destructor TfrmMain.Destroy;
begin
  FRecvRequest.Free;
  FCacnelIoExRequest.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnCancelIoExClick(Sender: TObject);
begin
  FCacnelIoExRequest.CheckThreadIn;
  ASyncExecute(InnerPostCacnelIoExRequest, nil);
end;

procedure TfrmMain.btnCloseIOHandleClick(Sender: TObject);
begin
  __defaultDiocpEngine.IocpCore.DoFinalize;
end;

procedure TfrmMain.btnCreateClientSocketClick(Sender: TObject);
begin
  if FRawSocket <> nil then
    raise Exception.Create('raw socket already create');
  FRawSocket := TRawSocket.Create;
  try
    FRawSocket.CreateTcpOverlappedSocket;
    __defaultDiocpEngine.IocpCore.Bind2IOCPHandle(FRawSocket.SocketHandle, ULONG_PTR(FRawSocket));
    if not FRawSocket.Connect(edtHost.Text, StrToInt(edtPort.Text)) then
    begin
      RaiseLastOSError;
    end;
  except
    //__defaultDiocpEngine.IocpCore.
    FRawSocket.Free;
    FRawSocket := nil;
    raise;
  end;
end;

procedure TfrmMain.btnPostRecvRequestClick(Sender: TObject);
begin
  FRecvRequest.CheckThreadIn;
  ASyncExecute(InnerPostRecvRequest, nil);
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  mmoInfo.Clear;
  if __defaultDiocpEngine <> nil then
  begin
    mmoInfo.Lines.Add(Format('diocp engine worker num:%d', [__defaultDiocpEngine.WorkerCount]));
  end else
  begin
    mmoInfo.Lines.Add('diocp engine is null');
  end;  
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  StartDiocpEngine;
  __defaultDiocpEngine.CheckStart;     
end;

procedure TfrmMain.btnCloseSocketClick(Sender: TObject);
begin
  FRawSocket.Close(chkShutDown.Checked);
end;

procedure TfrmMain.btnShutDownClick(Sender: TObject);
begin
  FRawSocket.ShutDown();
end;

procedure TfrmMain.InnerPostCacnelIoExRequest(Sender: TObject);
var
  err:Integer;
begin
  //FRecvRequest.CancelIoEx(FRawSocket.SocketHandle);
  if not DiocpCancelIoEx(FRawSocket.SocketHandle, nil) then
  begin
    err := GetLastError;
    sfLogger.logMessage('post cancelioex error:%d, desc:%s',[err, SysErrorMessage(err)]);
    FCacnelIoExRequest.CheckThreadOut;
  end;
end;

procedure TfrmMain.InnerPostRecvRequest(Sender: TObject);
var
  lvRet:Integer;
begin
  lvRet := diocp_winapi_winsock2.WSARecv(FRawSocket.SocketHandle,
     @FRecvBuffer,
     1,
     FNumberOfBytesRecvd,
     FRecvdFlag,
     LPWSAOVERLAPPED(FRecvRequest.OverlappedPtr),   // d7 need to cast
     nil
     );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    if lvRet <> WSA_IO_PENDING then
    begin
      sfLogger.logMessage('post recv error:%d',[lvRet]);
      FRecvRequest.CheckThreadOut;
    end;
  end;
end;

procedure TfrmMain.OnCancelIoExResponse(pvSender:TObject);
begin
  sfLogger.logMessage(Format('投递的CancelIoEx请求已经响应,Error:%d, byteSize:%d', [FCacnelIoExRequest.ErrorCode, FCacnelIoExRequest.BytesTransferred]));
  FCacnelIoExRequest.CheckThreadOut;
end;

procedure TfrmMain.OnRecvResponse(pvSender:TObject);
begin
  sfLogger.logMessage(Format('投递的接收请求已经响应,Error:%d, byteSize:%d', [FRecvRequest.ErrorCode, FRecvRequest.BytesTransferred]));
  FRecvRequest.CheckThreadOut;
end;



end.
