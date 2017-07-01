unit uRemoteServerDIOCPImpl;

interface

{$DEFINE USE_INDY}

uses
  uIRemoteServer,
  {$IFDEF USE_INDY}
  IdTCPClient,
  uDTcpClientCoderImpl,
  {$ELSE}
  diocp_tcp_blockClient,
  uDTcpClientCoderImpl,
  {$ENDIF}

  uStreamCoderSocket,
  SimpleMsgPack,
  Classes,
  SysUtils,
  utils_zipTools,
  uICoderSocket;

type
  TRemoteServerDIOCPImpl = class(TInterfacedObject, IRemoteServer)
  private
    {$IFDEF USE_INDY}
    FTcpClient: TIdTCPClient;
    {$ELSE}
    FTcpClient: TDiocpBlockTcpClient;
    {$ENDIF}
    FCoderSocket: ICoderSocket;
    FMsgPack:TSimpleMsgPack;
    FSendStream:TMemoryStream;
    FRecvStream:TMemoryStream;
  protected
    /// <summary>
    ///   执行远程动作
    /// </summary>
    function Execute(pvCmdIndex: Integer; var vData: OleVariant): Boolean; stdcall;
  public
    constructor Create;
    procedure setHost(pvHost: string);
    procedure setPort(pvPort:Integer);
    procedure open;
    destructor Destroy; override;
  end;

implementation

constructor TRemoteServerDIOCPImpl.Create;
begin
  inherited Create;
  {$IFDEF USE_INDY}
  FTcpClient := TIdTCPClient.Create(nil);
  FCoderSocket := TIdTCPClientCoderImpl.Create(FTcpClient);
  {$ELSE}
  FTcpClient := TDiocpBlockTcpClient.Create(NIL);
  FCoderSocket := TDTcpClientCoderImpl.Create(FTcpClient);
  {$ENDIF}
  
  FMsgPack := TSimpleMsgPack.Create;
  FRecvStream := TMemoryStream.Create;
  FSendStream := TMemoryStream.Create;
end;

destructor TRemoteServerDIOCPImpl.Destroy;
begin
  FCoderSocket := nil;
  FTcpClient.Disconnect;
  FTcpClient.Free;
  FMsgPack.Free;
  FRecvStream.Free;
  FSendStream.Free;
  inherited Destroy;
end;

{ TRemoteServerDIOCPImpl }

function TRemoteServerDIOCPImpl.Execute(pvCmdIndex: Integer; var vData:
    OleVariant): Boolean;
begin
  if not FTcpClient.Active then FTcpClient.Connect;
  FSendStream.Clear;
  FRecvStream.Clear;
  FMsgPack.Clear;
  FMsgPack.ForcePathObject('cmd.index').AsInteger := pvCmdIndex;
  FMsgPack.ForcePathObject('cmd.data').AsVariant := vData;
  FMsgPack.EncodeToStream(FSendStream);

  TZipTools.ZipStream(FSendStream, FSendStream);


  TStreamCoderSocket.SendObject(FCoderSocket, FSendStream);

  TStreamCoderSocket.RecvObject(FCoderSocket, FRecvStream);

  TZipTools.UnZipStream(FRecvStream, FRecvStream);
  FRecvStream.Position := 0;
  
  FMsgPack.DecodeFromStream(FRecvStream);

  Result := FMsgPack.ForcePathObject('__result.result').AsBoolean;

  if not Result then
    if FMsgPack.ForcePathObject('__result.msg').AsString <> '' then
    begin
      raise Exception.Create(FMsgPack.ForcePathObject('__result.msg').AsString);
    end;

  vData := FMsgPack.ForcePathObject('__result.data').AsVariant;
end;

procedure TRemoteServerDIOCPImpl.open;
begin
  FTcpClient.Disconnect;
  FTcpClient.Connect;
end;

procedure TRemoteServerDIOCPImpl.setHost(pvHost: string);
begin
  FTcpClient.Host := pvHost;
end;

procedure TRemoteServerDIOCPImpl.setPort(pvPort: Integer);
begin
  FTcpClient.Port := pvPort;  
end;

end.
