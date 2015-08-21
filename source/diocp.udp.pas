(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   
 *)

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

unit diocp.udp;

interface

uses
  diocp.core.engine, Classes, SysUtils, diocp.core.rawWinSocket,
  diocp.winapi.winsock2, SyncObjs, Windows, utils.locker, utils.queues,
  utils.hashs;

type
  /// <summary>
  /// 数据释放方式
  /// </summary>
  TDataReleaseType = (
    dtNone {不自动释放},
    dtFreeMem {调用FreeMem释放内存},
    dtDispose {调用Dispose释放数据，适用于New分配的数据}
    );
    
  TDiocpUdpListener = class;
  TDiocpUdp = class;

  TRequestState = (rsNone, rsPosting, rsResponding);

  TDiocpUdpSendRequest = class(TIocpRequest)
  private
    FSocket:   TSocket;

    // 回收对象
    FUdpOwner: TDiocpUdp;
    FInnerBuffer: diocp.winapi.winsock2.TWsaBuf;
    FWSASendFlag: Cardinal;
    FWSAToAddr: diocp.winapi.winsock2.TSockAddrIn;
    FWSAToAddrLen: Integer;

    FSendBufferReleaseType: TDataReleaseType;

    procedure CheckClearSendBuffer;
  protected
    FRequestState: TRequestState;
    procedure HandleResponse; override;
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = True);
        overload;
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvBufReleaseType:
        TDataReleaseType); overload;
  public
    destructor Destroy; override;
    /// <summary>
    ///
    /// </summary>
    function PostRequest: Boolean; overload;
    /// <summary>
    ///   请求状态
    /// </summary>
    property RequestState: TRequestState read FRequestState;
  end;

  TDiocpUdpSession = class(TObject)
  private
    FData: Pointer;
    FLocker: TIocpLocker;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Data: Pointer read FData write FData;
    property Locker: TIocpLocker read FLocker;
  end;

  TDiocpUdpSessionClass =  class of TDiocpUdpSession;

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

    procedure SendResponse(pvBuffer:PAnsiChar; pvBufferLen:Cardinal; pvCopyBuf:
        Boolean = True);

    function GetSession: TDiocpUdpSession;

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
    FSessions: TDHashTableSafe;
    FSessionClass:TDiocpUdpSessionClass;
    FDefaultListener: TDiocpUdpListener;
    FIocpEngine: TIocpEngine;
    FOnRecv: TDiocpUdpRecvRequestEvent;

    FSendRequestPool: TSafeQueue;

    /// <summary>
    ///   触发接收事件
    /// </summary>
    procedure DoRecv(pvRequest:TDiocpUdpRecvRequest);
    function GetSession(pvSessionID:String): TDiocpUdpSession;

    function GetSendRequest():TDiocpUdpSendRequest;
    procedure ReleaseSendRequest(pvRequest:TDiocpUdpSendRequest);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Stop();
    procedure Start();

    procedure WSASendTo(const ToAddr: TSockAddrIn; buf: Pointer; len: Cardinal; CopyBuf: Boolean = true); overload;

    procedure WSASendTo(pvRemoteIP:String; pvRemotePort:Integer; buf: Pointer; len:
        Cardinal; CopyBuf: Boolean = true); overload;

    procedure RegisterSessionClass(pvClass:TDiocpUdpSessionClass);

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
  FSessions := TDHashTableSafe.Create;
  FSessionClass := TDiocpUdpSession;

  FSendRequestPool := TSafeQueue.Create;
end;

destructor TDiocpUdp.Destroy;
begin
  Stop();
  FDefaultListener.Free;
  FreeAndNil(FIocpEngine);
  FSessions.FreeAllDataAsObject;
  FSessions.Free;

  FSendRequestPool.FreeDataObject;
  FSendRequestPool.Free;
  inherited Destroy;
end;

procedure TDiocpUdp.DoRecv(pvRequest:TDiocpUdpRecvRequest);
begin
  if Assigned(FOnRecv) then
  begin
    FOnRecv(pvRequest);
  end;
end;

function TDiocpUdp.GetSendRequest: TDiocpUdpSendRequest;
begin
  Result := TDiocpUdpSendRequest(FSendRequestPool.DeQueue);
  if Result = nil then
  begin
    Result := TDiocpUdpSendRequest.Create();
  end;
end;

function TDiocpUdp.GetSession(pvSessionID:String): TDiocpUdpSession;
begin
  FSessions.Lock;
  try
    Result :=TDiocpUdpSession(FSessions.ValueMap[pvSessionID]);
    if Result = nil then
    begin
      Result := FSessionClass.Create;
      FSessions.ValueMap[pvSessionID] := Result;
    end;
  finally
    FSessions.unLock;
  end;

end;

procedure TDiocpUdp.RegisterSessionClass(pvClass:TDiocpUdpSessionClass);
begin
  FSessionClass := pvClass;
end;

procedure TDiocpUdp.ReleaseSendRequest(pvRequest:TDiocpUdpSendRequest);
begin
  FSendRequestPool.EnQueue(pvRequest);
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

procedure TDiocpUdp.WSASendTo(const ToAddr: TSockAddrIn; buf: Pointer;
  len: Cardinal; CopyBuf: Boolean);
var
  lvRequest:TDiocpUdpSendRequest;
begin
  lvRequest := GetSendRequest();
  lvRequest.FSocket := FDefaultListener.FRawSocket.SocketHandle;
  lvRequest.FUdpOwner := Self;
  lvRequest.FWSAToAddr := ToAddr;
  lvRequest.SetBuffer(buf, len, CopyBuf);
  lvRequest.PostRequest();
end;

procedure TDiocpUdp.WSASendTo(pvRemoteIP:String; pvRemotePort:Integer; buf:
    Pointer; len: Cardinal; CopyBuf: Boolean = true);
var
  sockaddr: TSockAddrIn;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  sockaddr.sin_family := AF_INET;
  sockaddr.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(pvRemoteIP)));
  sockaddr.sin_port := htons(pvRemotePort);
  WSASendTo(sockaddr, buf, len, CopyBuf);
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
    FRecvRequestList.Clear;
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
  if pvRequest.RecvBufferLen > 0 then FOwner.DoRecv(pvRequest);
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
  if not FRawSocket.Bind(FHost, FPort) then
  begin
    RaiseLastOSError;
  end;
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

function TDiocpUdpRecvRequest.GetSession: TDiocpUdpSession;
var
  lvSessionID:String;
begin
  lvSessionID := RemoteAddr + '_' + IntToStr(RemotePort);
  Result := FListener.Owner.GetSession(lvSessionID);
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

procedure TDiocpUdpRecvRequest.SendResponse(pvBuffer:PAnsiChar;
    pvBufferLen:Cardinal; pvCopyBuf: Boolean = True);
begin
  FListener.Owner.WSASendTo(FWSARecvFrom, pvBuffer, pvBufferLen, pvCopyBuf);
end;

constructor TDiocpUdpSession.Create;
begin
  inherited Create;
  FLocker := TIocpLocker.Create();
end;

destructor TDiocpUdpSession.Destroy;
begin
  FreeAndNil(FLocker);
  inherited Destroy;
end;

destructor TDiocpUdpSendRequest.Destroy;
begin
  CheckClearSendBuffer;
  inherited Destroy;
end;

{ TDiocpUdpSendRequest }

procedure TDiocpUdpSendRequest.CheckClearSendBuffer;
begin
  if FInnerBuffer.len > 0 then begin
    case FSendBufferReleaseType of
      dtDispose: Dispose(FInnerBuffer.buf);
      dtFreeMem: FreeMem(FInnerBuffer.buf);
    end;
  end;
  FSendBufferReleaseType := dtNone;
  FInnerBuffer.len := 0;
end;

procedure TDiocpUdpSendRequest.HandleResponse;
begin
  inherited;
  FRequestState := rsResponding;

  FUdpOwner.ReleaseSendRequest(Self);
end;

function TDiocpUdpSendRequest.PostRequest: Boolean;
var
  lvRet, lvDNACounter:Integer;
  lpNumberOfBytesRecvd: Cardinal;
begin
  Result := False;
  lpNumberOfBytesRecvd := 0;
  FWSASendFlag := 0;
  FWSAToAddrLen := SizeOf(FWSAToAddr);

  FRequestState := rsPosting;
  lvRet := diocp.winapi.winsock2.WSASendTo(FSocket,
     @FInnerBuffer,
     1,
     @lpNumberOfBytesRecvd,
     FWSASendFlag,
     TSockAddr(FWSAToAddr),
     FWSAToAddrLen,
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
      FOwner.logMessage(strRecvPostError, [FSocket, lvRet]);
      {$ENDIF}
    end else
    begin

    end;
  end else
  begin
    Result := True;
  end;
end;

procedure TDiocpUdpSendRequest.SetBuffer(buf: Pointer; len: Cardinal;
    pvCopyBuf: Boolean);
var
  lvBuf: PAnsiChar;
begin
  if pvCopyBuf then begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    SetBuffer(lvBuf, len, dtFreeMem);
  end else
    SetBuffer(buf, len, dtNone);
end;

procedure TDiocpUdpSendRequest.SetBuffer(buf: Pointer; len: Cardinal;
    pvBufReleaseType: TDataReleaseType);
begin
  CheckClearSendBuffer;
  FInnerBuffer.buf := buf;
  FInnerBuffer.len := len;
  FSendBufferReleaseType := pvBufReleaseType;
end;

end.
