(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   
 *)
 
unit diocp.udp;

interface

uses
  diocp.core.engine, Classes, SysUtils, diocp.core.rawWinSocket, diocp.winapi.winsock2, SyncObjs, Windows;

type
  TDiocpUdpListener = class;
  TDiocpUdp = class;

  TRequestState = (rsNone, rsPosting, rsResponding);
  
  TDiocpUdpSendRequest = class(TIocpRequest)
  public

  end;

  TDiocpUdpRecvRequest = class(TIocpRequest)
  private
    FWSARecvdFlag     : Cardinal;
    FInnerBuffer      : diocp.winapi.winsock2.TWsaBuf;
    FListener         : TDiocpUdpListener;
    FWSARecvBuffer    : diocp.winapi.winsock2.TWsaBuf;
    FWSARecvFrom      : diocp.winapi.winsock2.TSockAddrIn;
    FWSAFromLen       : Integer;
  private
    function GetRecvBufferLen: Integer;
    function GetRecvBuffer: PAnsiChar;
    function GetRemoteAddr: String;
    function GetRemotePort: Integer;
  protected
    FRequestState: TRequestState;
    procedure HandleResponse; override;
  public

    function PostRequest(pvBlockSize:Integer): Boolean; overload;

    /// <summary>
    ///
    /// </summary>
    function PostRequest(pvBuffer: PAnsiChar; pvBufferLen: Cardinal): Boolean;
        overload;

    /// <summary>
    ///   所属的Listener
    /// </summary>
    property Listener: TDiocpUdpListener read FListener write FListener;

    /// <summary>
    ///   接收到的数据
    /// </summary>
    property RecvBuffer: PAnsiChar read GetRecvBuffer;

    /// <summary>
    ///   接收到的数据长度
    /// </summary>
    property RecvBufferLen: Integer read GetRecvBufferLen;
    
    property RemoteAddr: String read GetRemoteAddr;
    property RemotePort: Integer read GetRemotePort;

    

    
        
    /// <summary>
    ///   请求状态
    /// </summary>
    property RequestState: TRequestState read FRequestState;
  end;

  TDiocpUdpListener = class(TObject)
  private
    FReleaseEvent: TEvent;

    /// <summary>
    ///  正在请求工作的个数
    /// </summary>
    FRequestingCounter: Integer;
    FEnable: Boolean;
    FRecvRequestList: TList;
    FHost: String;
    FOwner: TDiocpUdp;
    FRawSocket: TRawSocket;
    FPort: Integer;
    procedure DecRequestingCounter;
    /// <summary>
    ///   触发接收事件
    /// </summary>
    procedure DoRecv(pvRequest:TDiocpUdpRecvRequest);
    procedure PostRecvRequest(pvRequestNum, pvBlockSize: Integer);

    procedure ClearRecvRequest();
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start();
    procedure Stop();
    
    /// <summary>
    ///   为False时不再重复投递
    /// </summary>
    property Enable: Boolean read FEnable write FEnable;
    property Host: String read FHost write FHost;
    property Owner: TDiocpUdp read FOwner write FOwner;
    property Port: Integer read FPort write FPort;

  end;

  TDiocpUdpRecvRequestEvent = procedure(pvRecvRequest:TDiocpUdpRecvRequest) of object;
  TDiocpUdp = class(TComponent)
  private
    FActive: Boolean;
    FDefaultListener: TDiocpUdpListener;
    FIocpEngine: TIocpEngine;
    FOnRecv: TDiocpUdpRecvRequestEvent;
    /// <summary>
    ///   触发接收事件
    /// </summary>
    procedure DoRecv(pvRequest:TDiocpUdpRecvRequest);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Stop();
    procedure Start();
    property Active: Boolean read FActive;
    
    property DefaultListener: TDiocpUdpListener read FDefaultListener;

    property OnRecv: TDiocpUdpRecvRequestEvent read FOnRecv write FOnRecv;
  end;

implementation

constructor TDiocpUdp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIocpEngine := TIocpEngine.Create();
  FDefaultListener := TDiocpUdpListener.Create;
  FDefaultListener.Owner := Self;
end;

destructor TDiocpUdp.Destroy;
begin
  Stop();
  FDefaultListener.Free;
  FreeAndNil(FIocpEngine);
  inherited Destroy;
end;

procedure TDiocpUdp.DoRecv(pvRequest:TDiocpUdpRecvRequest);
begin
  if Assigned(FOnRecv) then
  begin
    FOnRecv(pvRequest);
  end;
end;

procedure TDiocpUdp.Start;
begin
  FIocpEngine.Start();
  FDefaultListener.Start();
  FIocpEngine.IocpCore.Bind2IOCPHandle(FDefaultListener.FRawSocket.SocketHandle, 0);
  FDefaultListener.PostRecvRequest(10, 1024 * 4);
  
  FActive := true;
end;

procedure TDiocpUdp.Stop;
begin
  FActive := False;
  FDefaultListener.Stop();
  FIocpEngine.SafeStop(); 
end;

procedure TDiocpUdpListener.ClearRecvRequest;
var
  i: Integer;
  lvRecvRequest:TDiocpUdpRecvRequest;
begin
  if FRecvRequestList.Count > 0 then
  begin
    FReleaseEvent.WaitFor(INFINITE);
    for i := 0 to FRecvRequestList.Count - 1 do
    begin
      lvRecvRequest := TDiocpUdpRecvRequest(FRecvRequestList[i]);
      lvRecvRequest.Free;
    end;
  end;
end;

constructor TDiocpUdpListener.Create;
begin
  inherited Create;
  FRawSocket := TRawSocket.Create();
  FRecvRequestList := TList.Create();
  FReleaseEvent := TEvent.Create(nil, True, True, '');   
end;

destructor TDiocpUdpListener.Destroy;
begin
  FreeAndNil(FRawSocket);
  FRecvRequestList.Free;
  FReleaseEvent.Free;
  inherited Destroy;
end;

procedure TDiocpUdpListener.DecRequestingCounter;
begin
  if InterlockedDecrement(FRequestingCounter) = 0 then
  begin
    FReleaseEvent.SetEvent();
  end;
end;

procedure TDiocpUdpListener.DoRecv(pvRequest:TDiocpUdpRecvRequest);
begin
  FOwner.DoRecv(pvRequest);
  if FEnable then
  begin
    if pvRequest.PostRequest(pvRequest.FWSARecvBuffer.buf, pvRequest.FWSARecvBuffer.len) then
    begin
      // 重新成功了请求
    end else
    begin
      // 投递失败
      pvRequest.FRequestState := rsNone;
      DecRequestingCounter();
    end;
  end else
  begin
    pvRequest.FRequestState := rsNone;
    DecRequestingCounter();
  end;
end;

procedure TDiocpUdpListener.PostRecvRequest(pvRequestNum, pvBlockSize: Integer);
var
  i: Integer;
  lvRequest:TDiocpUdpRecvRequest;
begin
  for i := 1 to pvRequestNum do
  begin
    lvRequest := TDiocpUdpRecvRequest.Create();
    lvRequest.Listener := Self;
    FRecvRequestList.Add(lvRequest);

    FReleaseEvent.ResetEvent();
    InterlockedIncrement(FRequestingCounter);
    if (not lvRequest.PostRequest(pvBlockSize)) then
    begin
      DecRequestingCounter();
    end;
  end;
end;

procedure TDiocpUdpListener.Start;
begin
  FEnable := true;
  FRawSocket.CreateUdpOverlappedSocket();
  FRawSocket.Bind(FHost, FPort);
end;

procedure TDiocpUdpListener.Stop;
begin
  FEnable := false;
  FRawSocket.Close;
  ClearRecvRequest();
end;

function TDiocpUdpRecvRequest.GetRecvBufferLen: Integer;
begin
  Result := FBytesTransferred;
end;

function TDiocpUdpRecvRequest.GetRecvBuffer: PAnsiChar;
begin
  Result := FWSARecvBuffer.buf;
end;

function TDiocpUdpRecvRequest.GetRemoteAddr: String;
begin
  Result := inet_ntoa(FWSARecvFrom.sin_addr);
end;

function TDiocpUdpRecvRequest.GetRemotePort: Integer;
begin
  Result := ntohs(FWSARecvFrom.sin_port);
end;

function TDiocpUdpRecvRequest.PostRequest(pvBlockSize:Integer): Boolean;
begin
  if FInnerBuffer.len <> pvBlockSize then
  begin
    if FInnerBuffer.len > 0 then FreeMem(FInnerBuffer.buf);
    FInnerBuffer.len := pvBlockSize;
    GetMem(FInnerBuffer.buf, pvBlockSize);
  end;
  Result := PostRequest(FInnerBuffer.buf, pvBlockSize);
end;

procedure TDiocpUdpRecvRequest.HandleResponse;
begin
  inherited;
  FRequestState := rsResponding;
  FListener.DoRecv(Self);
end;

function TDiocpUdpRecvRequest.PostRequest(pvBuffer: PAnsiChar; pvBufferLen:
    Cardinal): Boolean;
var
  lvRet, lvDNACounter:Integer;
  lpNumberOfBytesRecvd: Cardinal;
begin
  Result := False;
  lpNumberOfBytesRecvd := 0;
  FWSARecvdFlag := 0;
  FWSAFromLen := SizeOf(FWSARecvFrom);

  FWSARecvBuffer.buf := pvBuffer;
  FWSARecvBuffer.len := pvBufferLen;
  FRequestState := rsPosting;
  lvRet := diocp.winapi.winsock2.WSARecvFrom(FListener.FRawSocket.SocketHandle,
     @FWSARecvBuffer,
     1,
     @lpNumberOfBytesRecvd,
     FWSARecvdFlag,
     @FWSARecvFrom,
     @FWSAFromLen,
     LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
     nil
     );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
      FRequestState := rsNone;
      {$IFDEF WRITE_LOG}
      FOwner.logMessage(strRecvPostError, [FListener.FRawSocket.SocketHandle, lvRet]);
      {$ENDIF}
    end else
    begin

    end;
  end else
  begin
    Result := True;
  end;                        
end;

end.
