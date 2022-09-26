unit uFMDiocpCltMonitor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, diocp_sockets, StdCtrls, ExtCtrls, uRunTimeINfoTools;

type
  TFMDiocpCltMonitor = class(TFrame)
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
    lblDisconnect: TLabel;
    lblDisconnectCounter: TLabel;
    procedure lblWorkerCountClick(Sender: TObject);
    procedure tmrReaderTimer(Sender: TObject);
    procedure refreshState;
  private
    FIocpSocket: TDiocpCustom;

    { Private declarations }
  public
    class function createAsChild(pvParent: TWinControl; pvDiocpCustom:
        TDiocpCustom): TFMDiocpCltMonitor;

    property IocpSocket: TDiocpCustom read FIocpSocket write FIocpSocket;
  end;

implementation



{$R *.dfm}

resourcestring
  strRecv_PostInfo     = 'Ͷ��:%d, ��Ӧ:%d, ʣ��:%d �ٶ�:%d ��/��, %s';  //post:%d, response:%d, remain:%d
  strSend_Info         = 'Ͷ��:%d, ��Ӧ:%d, ʣ��:%d �ٶ�:%d ��/��';  //post:%d, response:%d, remain:%d

class function TFMDiocpCltMonitor.createAsChild(pvParent: TWinControl;
    pvDiocpCustom: TDiocpCustom): TFMDiocpCltMonitor;
begin
  Result := TFMDiocpCltMonitor.Create(pvParent.Owner);
  Result.Parent := pvParent;
  Result.Align := alClient;
  Result.IocpSocket := pvDiocpCustom;
  Result.tmrReader.Enabled := True;
  Result.refreshState;   
end;

procedure TFMDiocpCltMonitor.lblWorkerCountClick(Sender: TObject);
begin
  if IocpSocket <> nil then
  begin
    ShowMessage(IocpSocket.IocpEngine.getStateINfo);
  end;
end;

procedure TFMDiocpCltMonitor.tmrReaderTimer(Sender: TObject);
begin
  refreshState;
end;

procedure TFMDiocpCltMonitor.refreshState;
var
  str:string;
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

  // ͳ���ٶ���Ϣ
  FIocpSocket.DataMoniter.SpeedCalcuEnd();

  // ��ʼ��¼
  FIocpSocket.DataMoniter.SpeedCalcuStart();


//  lblPostRecvINfo.Caption :=   Format('post:%d, response:%d',
//     [
//       FIocpSocket.DataMoniter.PostWSARecvCounter,
//       FIocpSocket.DataMoniter.ResponseWSARecvCounter
//     ]
//    );

  if FIocpSocket.DataMoniter.Speed_Recv < 1024 then
  begin
    str := Format('%d B/��', [FIocpSocket.DataMoniter.Speed_Recv]);
  end else if FIocpSocket.DataMoniter.Speed_Recv < 1024 * 1024 then
  begin
    str := Format('%.2f KB/��', [FIocpSocket.DataMoniter.Speed_Recv / 1024]);
  end else
  begin
    str := Format('%.2f MB/��', [FIocpSocket.DataMoniter.Speed_Recv / (1024 * 1024.00)]);           
  end;
  

  lblPostRecvINfo.Caption :=   Format(strRecv_PostInfo,
     [
       FIocpSocket.DataMoniter.PostWSARecvCounter,
       FIocpSocket.DataMoniter.ResponseWSARecvCounter,
       FIocpSocket.DataMoniter.PostWSARecvCounter -
       FIocpSocket.DataMoniter.ResponseWSARecvCounter,
       FIocpSocket.DataMoniter.Speed_WSARecvResponse,
       str
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

//  lblSend.Caption := Format('post:%d, response:%u',
//     [
//       FIocpSocket.DataMoniter.PostWSASendCounter,
//       FIocpSocket.DataMoniter.ResponseWSASendCounter
//     ]
//    );

  lblSend.Caption := Format(strSend_Info,
     [
       FIocpSocket.DataMoniter.PostWSASendCounter,
       FIocpSocket.DataMoniter.ResponseWSASendCounter,
       FIocpSocket.DataMoniter.PostWSASendCounter - FIocpSocket.DataMoniter.ResponseWSASendCounter,
       FIocpSocket.DataMoniter.Speed_WSASendResponse
     ]
    );
    
    
  lblSendQueue.Caption := Format('create/push/pop/complted:%d, %d, %d, %d',
     [
       FIocpSocket.DataMoniter.SendRequestCreateCounter,
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
  
  lblDisconnectCounter.Caption := Format('%d', [FIocpSocket.DataMoniter.DisconnectCounter]);

  lblWorkerCount.Caption := Format('%d', [FIocpSocket.WorkerCount]);

  lblRunTimeINfo.Caption :=TRunTimeINfoTools.getRunTimeINfo;


end;

end.
