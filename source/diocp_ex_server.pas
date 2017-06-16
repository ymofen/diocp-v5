(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft

 *   1. 扩展服务器TDiocpExTcpServer, 可以定义开始标志和结束标志(也可以只设定结束标志)，然后自动进行解包触发OnContextDataAction事件。
 *   2. 字符串服务器TDiocpStringTcpServer, 可以设定开始字符串和结束字符串(也可以只设定结束字符串)，然后自动进行解包触发OnContextStringAction事件。
 *      2015-07-15 09:00:09
 *
 *   3. 修复ex.server编码问题，发送大数据时，无法解码的bug
 *      2015-08-17 14:25:56

*)

unit diocp_ex_server;

interface

{$DEFINE StartEndCut}

uses
  diocp_tcp_server, utils_rawPackage, utils_safeLogger, SysUtils, Classes,
  utils_strings, diocp_res;

type
  TDiocpExContext = class;
  TDiocpStringContext = class;
  
  TContextDataActionEvent = procedure(pvContext:TDiocpExContext; pvData: Pointer;
      pvDataLen: Integer) of object;

  TContextStringActionEvent = procedure(pvContext:TDiocpStringContext;
      pvDataString:String) of object;


  TDiocpExContext = class(TIocpClientContext)
  private
    FCacheBuffer: TRawPackage;
    FRecvData: array of Byte;
  protected
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
    procedure OnDataAction(pvData: Pointer; pvDataLen: Integer);
    procedure DoCleanUp;override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    ///   自动添加前后标志
    /// </summary>
    procedure WriteData(pvData: Pointer; pvDataLen: Integer);
  end;



  TDiocpExTcpServer = class(TDiocpTcpServer)
  private
    FStartData: array [0..254] of Byte;
    FStartDataLen:Byte;

    FEndData:array [0..254] of Byte;
    FEndDataLen: Byte;

    /// 设置最大的数据包长度
    FMaxDataLen:Integer;
    FOnContextDataAction: TContextDataActionEvent;
  protected
    procedure DoDataAction(pvContext: TDiocpExContext; pvData: Pointer; pvDataLen:
        Integer);virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetStart(pvData:Pointer; pvDataLen:Byte);
    procedure SetEnd(pvData:Pointer; pvDataLen:Byte);

    /// <summary>
    ///  设置最大的数据包长度
    ///  不能设置小于0的数字
    ///   10M (1024 * 1024 * 10)
    /// </summary>
    procedure SetMaxDataLen(pvDataLen:Integer);

    property OnContextDataAction: TContextDataActionEvent read FOnContextDataAction write FOnContextDataAction; 
  end;


  TDiocpStringContext = class(TDiocpExContext)
  public
    procedure WriteAnsiString(pvData:AnsiString);
  end;

  TDiocpStringTcpServer = class(TDiocpExTcpServer)
  private
    FOnContextStringAction: TContextStringActionEvent;
  protected
    procedure DoDataAction(pvContext: TDiocpExContext; pvData: Pointer; pvDataLen:
        Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetPackEndStr(pvEndStr:AnsiString);
    procedure SetPackStartStr(pvStartStr:AnsiString);
    property OnContextStringAction: TContextStringActionEvent read FOnContextStringAction write FOnContextStringAction;
  end;

implementation


constructor TDiocpExContext.Create;
begin
  inherited Create;
end;

destructor TDiocpExContext.Destroy;
begin
  inherited Destroy;
end;



procedure TDiocpExContext.DoCleanUp;
begin
  inherited DoCleanUp;
  FCacheBuffer.FEndBytesLength := 0;
  FCacheBuffer.FStartBytesLength := 0;
end;

procedure TDiocpExContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD);
var
  j, i, x, r:Integer;
  str:AnsiString;
  pstr, pbuf, prsearch:PAnsiChar;

  lvStartData:Pointer;
  lvStartDataLen:Byte;
  
  lvEndData:Pointer;
  lvEndDataLen:Byte;

  lvOwner:TDiocpExTcpServer;

  lvPtr:PByte;

begin
  lvOwner := TDiocpExTcpServer(Owner);

  lvStartDataLen := lvOwner.FStartDataLen;
  lvEndDataLen := lvOwner.FEndDataLen;

  if (FCacheBuffer.FEndBytesLength = 0) and (FCacheBuffer.FStartBytesLength = 0)  then
  begin
    SetPackageMaxLength(@FCacheBuffer, lvOwner.FMaxDataLen);
    lvStartData := @lvOwner.FStartData[0];
    lvEndData := @lvOwner.FEndData[0];

    if lvStartDataLen > 0 then
    begin
      SetLength(FCacheBuffer.FStartBytes, lvStartDataLen);
      Move(lvStartData^, FCacheBuffer.FStartBytes[0], lvStartDataLen);
      FCacheBuffer.FStartBytesLength := lvStartDataLen;
    end;
    if lvEndDataLen > 0 then
    begin
      SetLength(FCacheBuffer.FEndBytes, lvEndDataLen);
      Move(lvEndData^, FCacheBuffer.FEndBytes[0], lvEndDataLen);
      FCacheBuffer.FEndBytesLength := lvEndDataLen;
    end;
    ResetPacakge(@FCacheBuffer);
  end;

  lvPtr := PByte(buf);
  for i := 0 to len -1 do
  begin
    r := InputBuffer(@FCacheBuffer, lvPtr^);
    inc(lvPtr);
    if r = 1 then
    begin
{$IFDEF StartEndCut}
      // 去掉头尾
      OnDataAction(@FCacheBuffer.FRawBytes[lvStartDataLen], FCacheBuffer.FRawLength - lvStartDataLen - lvEndDataLen);
{$ELSE}
      OnDataAction(@FCacheBuffer.FRawBytes[0], FCacheBuffer.FRawLength );
{$ENDIF}
      ResetPacakge(@FCacheBuffer);
    end;

  end;




//  pbuf := PAnsiChar(buf);
//  r := len;
//
//
//
//  // 已经有数据了
//  if (FCacheBuffer.validCount > 0) then
//  begin
//    // 不够数据
//    if FCacheBuffer.validCount < lvEndDataLen then Exit;
//    
//    // 查找结束字符串   
//    prsearch := SearchPointer(pbuf, len, 0, lvEndData, lvEndDataLen);
//    if prsearch = nil then
//    begin  // 没有结束标志
//      FCacheBuffer.AddBuffer(buf, len);
//      Exit;
//    end else
//    begin   // 有结束标志了，拼包
//      j := prsearch-pbuf;
//      i := self.FCacheBuffer.validCount;
//      if i > 0 then
//      begin
//        SetLength(FRecvData, i + j);
//        pstr := PAnsiChar(@FRecvData[0]);
//        FCacheBuffer.readBuffer(pstr, i);
//        pstr := pstr + i;
//        Move(pbuf^, pstr^, j);
//        Inc(pbuf, j);
//        Dec(r, j);
//
//        FCacheBuffer.clearBuffer();
//        OnDataAction(@FRecvData[0], i + j);
//      end;
//    end;
//  end;  
//  
//  while r > 0 do
//  begin
//    if lvStartDataLen > 0 then
//    begin
//      prsearch := SearchPointer(pbuf, r, 0, lvStartData, lvStartDataLen);
//      if prsearch = nil then
//      begin  // 没有开始标志buf无效
//        Break;
//      end else
//      begin
//        j := prsearch - pbuf;
//        // 丢弃到开始标志之前的数据
//        Inc(pbuf, j + lvStartDataLen);   // 跳过开始标志
//        Dec(r, j + lvStartDataLen);
//      end;
//    end;
//
//    prsearch := SearchPointer(pbuf, r, 0, lvEndData, lvEndDataLen);//(pbuf, r, 0);
//    if prsearch <> nil then
//    begin
//      j := prsearch - pbuf;
//      if j = 0 then
//      begin  // 只有一个结束标志
//
//      end else
//      begin
//        SetLength(FRecvData, j);
//        pstr := PAnsiChar(@FRecvData[0]);
//        Move(pbuf^, pstr^, j);
//        Inc(pbuf, j);
//        Dec(r, j);
//        OnDataAction(pstr, j);
//      end;
//      Inc(pbuf, lvEndDataLen);   // 跳过结束标志
//      Dec(r, lvEndDataLen); 
//    end else
//    begin     // 剩余数据处理
//      if r > 0 then FCacheBuffer.AddBuffer(pbuf, r);
//      if FCacheBuffer.validCount > lvOwner.FMaxDataLen then
//      begin                      // 超过最大数据包大小
//        FCacheBuffer.clearBuffer();
//      end;
//
//      Break;
//    end;
//  end;
end;

procedure TDiocpExContext.OnDataAction(pvData: Pointer; pvDataLen: Integer);
var
  lvOwner:TDiocpExTcpServer;
begin
  try
    lvOwner := TDiocpExTcpServer(Owner);
    lvOwner.DoDataAction(self, pvData, pvDataLen);
  except
    on E:Exception do
    begin
      if Owner <> nil then
      begin
        Owner.LogMessage(strOnResponseException, [SocketHandle, 'OnDataAction', e.Message], '异常', lgvError);

        DoOwnerClientContext(-1);
      end else
      begin
        __svrLogger.logMessage(strOnResponseException, [SocketHandle, 'OnDataAction', e.Message], '异常', lgvError);
      end;
    end;
  end;
end;

procedure TDiocpExContext.WriteData(pvData: Pointer; pvDataLen: Integer);
var
  j, i, x, r:Integer;
  str:AnsiString;
  pstr, pbuf, prsearch:PAnsiChar;

  lvStartData:Pointer;
  lvStartDataLen:Byte;
  
  lvEndData:Pointer;
  lvEndDataLen:Byte;

  lvOwner:TDiocpExTcpServer;

  lvSendBuffer:array of byte;  
begin
  lvOwner := TDiocpExTcpServer(Owner);
{$IFDEF StartEndCut}
      // 去掉头尾
  lvStartData := @lvOwner.FStartData[0];
  lvStartDataLen := lvOwner.FStartDataLen;
  lvEndData := @lvOwner.FEndData[0];
  lvEndDataLen := lvOwner.FEndDataLen;
  j := lvStartDataLen + pvDataLen + lvEndDataLen;
{$ELSE}
  lvStartDataLen:=0;
  lvEndDataLen:=0;
  j := pvDataLen;
{$ENDIF}



  SetLength(lvSendBuffer, j);
  if lvStartDataLen > 0 then
  begin
    Move(lvStartData^, lvSendBuffer[0], lvStartDataLen);
  end;

  Move(pvData^, lvSendBuffer[lvStartDataLen], pvDataLen);

  if lvEndDataLen > 0 then
  begin
    Move(lvEndData^, lvSendBuffer[lvStartDataLen + pvDatalen], lvEndDataLen);
  end;

  PostWSASendRequest(@lvSendBuffer[0], j);
end;

{ TDiocpExTcpServer }

constructor TDiocpExTcpServer.Create(AOwner: TComponent);
begin
  inherited;
  RegisterContextClass(TDiocpExContext);
  FMaxDataLen := 1024 * 1024 * 10;  // 10M
end;

procedure TDiocpExTcpServer.DoDataAction(pvContext: TDiocpExContext; pvData:
    Pointer; pvDataLen: Integer);
begin
  if Assigned(FOnContextDataAction) then
  begin
    FOnContextDataAction(pvContext, pvData, pvDataLen);
  end;  
end;

procedure TDiocpExTcpServer.SetEnd(pvData:Pointer; pvDataLen:Byte);
begin
  Move(pvData^, FEndData[0], pvDataLen);
  FEndDataLen := pvDataLen;
end;

procedure TDiocpExTcpServer.SetMaxDataLen(pvDataLen:Integer);
begin
  FMaxDataLen := pvDataLen;
  Assert(FMaxDataLen > 0);
end;

procedure TDiocpExTcpServer.SetStart(pvData:Pointer; pvDataLen:Byte);
begin
  Move(pvData^, FStartData[0], pvDataLen);
  FStartDataLen := pvDataLen;
end;

constructor TDiocpStringTcpServer.Create(AOwner: TComponent);
begin
  inherited;
  RegisterContextClass(TDiocpStringContext);
end;

procedure TDiocpStringTcpServer.DoDataAction(pvContext: TDiocpExContext; pvData:
    Pointer; pvDataLen: Integer);
var
  lvStr:String;
begin
  inherited;
  lvStr := ByteBufferToString(PByte(pvData), pvDataLen);
  if Assigned(FOnContextStringAction) then
  begin
    FOnContextStringAction(TDiocpStringContext(pvContext), lvStr);
  end;    
end;

procedure TDiocpStringTcpServer.SetPackEndStr(pvEndStr:AnsiString);
begin
  SetEnd(PAnsiChar(pvEndStr), Length(pvEndStr));
end;

procedure TDiocpStringTcpServer.SetPackStartStr(pvStartStr:AnsiString);
begin
  SetStart(PAnsiChar(pvStartStr), Length(pvStartStr));
end;

procedure TDiocpStringContext.WriteAnsiString(pvData:AnsiString);
begin
  WriteData(PAnsiChar(pvData), Length(pvData));
end;

end.
