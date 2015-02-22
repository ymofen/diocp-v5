unit uFMMonitor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, diocp.tcp.server, StdCtrls, ExtCtrls, uRunTimeINfoTools;

type
  TFMMonitor = class(TFrame)
    lblServerStateCaption: TLabel;
    tmrReader: TTimer;
    lblsvrState: TLabel;
    lblRecvCaption: TLabel;
    lblPostRecvINfo: TLabel;
    lblSendCaption: TLabel;
    lblSend: TLabel;
    lblAcceptExCaption: TLabel;
    lblAcceptEx: TLabel;
    lblOnlineCounter: TLabel;
    lblOnlineCaption: TLabel;
    lblRunTimeINfo: TLabel;
    lblWorkersCaption: TLabel;
    lblWorkerCount: TLabel;
    lblRunTimeCaption: TLabel;
    lblRecvdSize: TLabel;
    lblSentSize: TLabel;
    lblSendQueue: TLabel;
    lblSendingQueueCaption: TLabel;
    lblSocketHandle: TLabel;
    lblSocketHandleCaption: TLabel;
    lblContextInfo: TLabel;
    lblContextInfoCaption: TLabel;
    lblSendRequest: TLabel;
    lblSendRequestCaption: TLabel;
    procedure lblRecvCaptionDblClick(Sender: TObject);
    procedure lblWorkerCountClick(Sender: TObject);
    procedure tmrReaderTimer(Sender: TObject);
    procedure refreshState;
  private
    FIocpTcpServer: TDiocpTcpServer;
    procedure Translate();
  public
    class function CreateAsChild(pvParent: TWinControl; pvIOCPTcpServer:
        TDiocpTcpServer): TFMMonitor;
    property IocpTcpServer: TDiocpTcpServer read FIocpTcpServer write FIocpTcpServer;
  end;

implementation

{$R *.dfm}

resourcestring
  strState_Caption = '服务状态';
  strRecv_Caption  = '接收信息';
  strSend_Caption  = '发送信息';
  strSendQueue_Caption    = '发送队列';
  strSendRequest_Caption  = '发送请求对象';
  strSocketHandle_Caption = '套接字句柄';
  strAcceptEx_Caption     = 'AcceptEx信息';
  strContext_Caption      = '连接信息';
  strOnline_Caption       = '在线信息';
  strWorkers_Caption      = '工作线程';
  strRunTime_Caption      = '运行信息';
  

  strState_Active      = '开启';
  strState_MonitorNull = '没有创建监控器';
  strState_ObjectNull  = '没有监控对象';    //'iocp server is null'
  strState_Off         = '关闭';
  strRecv_PostInfo     = '投递:%d, 回应:%d, 剩余:%d';  //post:%d, response:%d, remain:%d
  strSend_Info         = '投递:%d, 回应:%d, 剩余:%d';  //post:%d, response:%d, remain:%d
  strSendQueue_Info    = '压入/弹出/完成/终止:%d, %d, %d, %d';//push/pop/complted/abort:%d, %d, %d, %d
  strSendRequest_Info  = '创建:%d, 借出:%d, 还回:%d';  //'create:%d, out:%d, return:%d'
  strAcceptEx_Info     = '投递:%d, 回应:%d';      //'post:%d, response:%d'
  strSocketHandle_Info = '创建:%d, 销毁:%d';  //'create:%d, destroy:%d'
  strContext_Info      = '创建:%d, 借出:%d, 还回:%d';  //'create:%d, out:%d, return:%d'

class function TFMMonitor.CreateAsChild(pvParent: TWinControl; pvIOCPTcpServer:
    TDiocpTcpServer): TFMMonitor;
begin
  Result := TFMMonitor.Create(pvParent.Owner);
  Result.Translate;
  Result.Parent := pvParent;
  Result.Align := alClient;
  Result.IocpTcpServer := pvIOCPTcpServer;
  Result.tmrReader.Enabled := True;
  Result.refreshState;   
end;

procedure TFMMonitor.lblRecvCaptionDblClick(Sender: TObject);
begin
  FIocpTcpServer.DataMoniter.Clear;
end;

procedure TFMMonitor.lblWorkerCountClick(Sender: TObject);
begin
  if IocpTcpServer <> nil then
  begin
    ShowMessage(IocpTcpServer.IocpEngine.getStateINfo);
  end;
end;

procedure TFMMonitor.tmrReaderTimer(Sender: TObject);
begin
  refreshState;
end;

procedure TFMMonitor.Translate;
begin
  lblServerStateCaption.Caption := strState_Caption;
  lblRecvCaption.Caption := strRecv_Caption;
  lblSendCaption.Caption := strSend_Caption;
  lblSendingQueueCaption.Caption := strSendQueue_Caption;
  lblSendRequestCaption.Caption := strSendRequest_Caption;
  lblRunTimeCaption.Caption := strRunTime_Caption;
  lblAcceptExCaption.Caption := strAcceptEx_Caption;
  lblOnlineCaption.Caption := strOnline_Caption;
  lblSocketHandleCaption.Caption := strSocketHandle_Caption;
  lblContextInfoCaption.Caption := strContext_Caption;
  lblWorkersCaption.Caption := strWorkers_Caption;
end;

procedure TFMMonitor.refreshState;
begin
  if FIocpTcpServer = nil then
  begin
    lblsvrState.Caption := strState_ObjectNull;
    exit;
  end;

  if FIocpTcpServer.DataMoniter = nil then
  begin
    lblsvrState.Caption := strState_MonitorNull;
    exit;
  end;

  if FIocpTcpServer.Active then
  begin
    lblsvrState.Caption := strState_Active;
  end else
  begin
    lblsvrState.Caption := strState_Off;
  end;


  lblPostRecvINfo.Caption :=   Format(strRecv_PostInfo,
     [
       FIocpTcpServer.DataMoniter.PostWSARecvCounter,
       FIocpTcpServer.DataMoniter.ResponseWSARecvCounter,
       FIocpTcpServer.DataMoniter.PostWSARecvCounter -
       FIocpTcpServer.DataMoniter.ResponseWSARecvCounter
     ]
    );

  lblRecvdSize.Caption := TRunTimeINfoTools.TransByteSize(FIocpTcpServer.DataMoniter.RecvSize);


//  Format('post:%d, response:%d, recvd:%d',
//     [
//       FIocpTcpServer.DataMoniter.PostWSARecvCounter,
//       FIocpTcpServer.DataMoniter.ResponseWSARecvCounter,
//       FIocpTcpServer.DataMoniter.RecvSize
//     ]
//    );

  lblSend.Caption := Format(strSend_Info,
     [
       FIocpTcpServer.DataMoniter.PostWSASendCounter,
       FIocpTcpServer.DataMoniter.ResponseWSASendCounter,
       FIocpTcpServer.DataMoniter.PostWSASendCounter - FIocpTcpServer.DataMoniter.ResponseWSASendCounter
     ]
    );

  lblSendRequest.Caption := Format(strSendRequest_Info,
     [
       FIocpTcpServer.DataMoniter.SendRequestCreateCounter,
       FIocpTcpServer.DataMoniter.SendRequestOutCounter,
       FIocpTcpServer.DataMoniter.SendRequestReturnCounter
     ]
    );

  lblSendQueue.Caption := Format(strSendQueue_Info,
     [
       FIocpTcpServer.DataMoniter.PushSendQueueCounter,
       FIocpTcpServer.DataMoniter.PostSendObjectCounter,
       FIocpTcpServer.DataMoniter.ResponseSendObjectCounter,
       FIocpTcpServer.DataMoniter.SendRequestAbortCounter
     ]
    );
  lblSentSize.Caption := TRunTimeINfoTools.transByteSize(FIocpTcpServer.DataMoniter.SentSize);


  lblAcceptEx.Caption := Format(strAcceptEx_Info,
     [
       FIocpTcpServer.DataMoniter.PostWSAAcceptExCounter,
       FIocpTcpServer.DataMoniter.ResponseWSAAcceptExCounter
     ]
    );

  lblSocketHandle.Caption := Format(strSocketHandle_Info,
     [
       FIocpTcpServer.DataMoniter.HandleCreateCounter,
       FIocpTcpServer.DataMoniter.HandleDestroyCounter
     ]
    );

  lblContextInfo.Caption := Format(strContext_Info,
     [
       FIocpTcpServer.DataMoniter.ContextCreateCounter,
       FIocpTcpServer.DataMoniter.ContextOutCounter,
       FIocpTcpServer.DataMoniter.ContextReturnCounter


     ]
    );

  lblOnlineCounter.Caption := Format('%d', [FIocpTcpServer.ClientCount]);
  
  lblWorkerCount.Caption := Format('%d', [FIocpTcpServer.WorkerCount]);


  lblRunTimeINfo.Caption :=TRunTimeINfoTools.GetRunTimeINfo;


end;

end.
