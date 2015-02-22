unit uFMMonitor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, diocp.sockets, StdCtrls, ExtCtrls, uRunTimeINfoTools;

type
  TFMMonitor = class(TFrame)
    Label1: TLabel;
    tmrReader: TTimer;
    lblsvrState: TLabel;
    Label2: TLabel;
    lblPostRecvINfo: TLabel;
    Label3: TLabel;
    lblSend: TLabel;
    Label4: TLabel;
    lblAcceptEx: TLabel;
    lblOnlineCounter: TLabel;
    Label5: TLabel;
    lblRunTimeINfo: TLabel;
    Label6: TLabel;
    lblWorkerCount: TLabel;
    Label7: TLabel;
    lblRecvdSize: TLabel;
    lblSentSize: TLabel;
    lblSendQueue: TLabel;
    Label8: TLabel;
    procedure lblWorkerCountClick(Sender: TObject);
    procedure tmrReaderTimer(Sender: TObject);
    procedure refreshState;
  private
    FIocpSocket: TDiocpCustom;

    { Private declarations }
  public
    class function createAsChild(pvParent: TWinControl; pvDiocpCustom:
        TDiocpCustom): TFMMonitor;

    property IocpSocket: TDiocpCustom read FIocpSocket write FIocpSocket;
  end;

implementation

{$R *.dfm}

class function TFMMonitor.createAsChild(pvParent: TWinControl; pvDiocpCustom:
    TDiocpCustom): TFMMonitor;
begin
  Result := TFMMonitor.Create(pvParent.Owner);
  Result.Parent := pvParent;
  Result.Align := alClient;
  Result.IocpSocket := pvDiocpCustom;
  Result.tmrReader.Enabled := True;
  Result.refreshState;   
end;

procedure TFMMonitor.lblWorkerCountClick(Sender: TObject);
begin
  if IocpSocket <> nil then
  begin
    ShowMessage(IocpSocket.IocpEngine.getStateINfo);
  end;
end;

procedure TFMMonitor.tmrReaderTimer(Sender: TObject);
begin
  refreshState;
end;

procedure TFMMonitor.refreshState;
begin
  if FIocpSocket = nil then
  begin
    lblsvrState.Caption := 'iocp server is null';
    exit;
  end;

  if FIocpSocket.DataMoniter = nil then
  begin
    lblsvrState.Caption := 'monitor is null';
    exit;
  end;

  if FIocpSocket.Active then
  begin
    lblsvrState.Caption := 'running';
  end else
  begin
    lblsvrState.Caption := 'stop';
  end;


  lblPostRecvINfo.Caption :=   Format('post:%d, response:%d',
     [
       FIocpSocket.DataMoniter.PostWSARecvCounter,
       FIocpSocket.DataMoniter.ResponseWSARecvCounter
     ]
    );

  lblRecvdSize.Caption := TRunTimeINfoTools.transByteSize(FIocpSocket.DataMoniter.RecvSize);


//  Format('post:%d, response:%d, recvd:%d',
//     [
//       FIocpSocket.DataMoniter.PostWSARecvCounter,
//       FIocpSocket.DataMoniter.ResponseWSARecvCounter,
//       FIocpSocket.DataMoniter.RecvSize
//     ]
//    );

  lblSend.Caption := Format('post:%d, response:%u',
     [
       FIocpSocket.DataMoniter.PostWSASendCounter,
       FIocpSocket.DataMoniter.ResponseWSASendCounter
     ]
    );
    
  lblSendQueue.Caption := Format('push/pop/complted:%d, %d, %d',
     [
       FIocpSocket.DataMoniter.PushSendQueueCounter,
       FIocpSocket.DataMoniter.PostSendObjectCounter,
       FIocpSocket.DataMoniter.ResponseSendObjectCounter
     ]
    );
  lblSentSize.Caption := TRunTimeINfoTools.transByteSize(FIocpSocket.DataMoniter.SentSize);


  lblAcceptEx.Caption := Format('post:%d, response:%d',
     [
       FIocpSocket.DataMoniter.PostWSAAcceptExCounter,
       FIocpSocket.DataMoniter.ResponseWSAAcceptExCounter
     ]
    );

  lblOnlineCounter.Caption := Format('%d', [FIocpSocket.OnlineContextCount]);

  lblWorkerCount.Caption := Format('%d', [FIocpSocket.WorkerCount]);

  lblRunTimeINfo.Caption :=TRunTimeINfoTools.getRunTimeINfo;


end;

end.
