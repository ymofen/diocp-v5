unit utils_redis;

interface

uses
  utils_DValue, SysUtils, diocp_core_rawWinSocket, diocp_winapi_winsock2, Classes, utils_rawPackage,
  utils_async, utils_strings, SysConst;

const
  MAX_LEN = 10240;

type
  TRedisCommand = class(TObject)
  private
    FRawPackage: TRawPackage;
    FCommand: String;
    FData: TDValue;

    FDecodeFlag: Integer;
    FDecodeArgs: Integer;
    FDecodeCurrentIndex: Integer;
    FDecodeCurrentParamLength: Integer;
    FDecodeLength: Integer;

    FLastCmdString: String;
    FResponseMessage: String;


    // 用单行回复，回复的第一个字节将是“+”
    // 错误消息，回复的第一个字节将是“-”
    // 整型数字，回复的第一个字节将是“:”
    // 批量回复，回复的第一个字节将是“$”
    // 多个批量回复，回复的第一个字节将是“*”
    FDecodeTypeChr: Char;
    FDecodePackTypeChr: Char;
    FLastResponseIntValue: Integer;

    /// <summary>
    /// * 多行
    /// </summary>
    /// <returns> 0, 需要更多的数据, 1: 完整, -1：错误</returns>
    function InputBufferForMultiResponse(pvBuf: Byte): Integer;

    function InputBufferForTypeChar(pvBuf: Byte): Char;

    function InputBufferForDefaultStr(pvBuf: Byte): Integer;

    procedure ConfigStartAndEndBytes(pvStart, pvEnd: string);
  public
    function InputBufferForDecode(pvBuf: Byte): Integer;

    constructor Create;
    destructor Destroy; override;
    property Command: String read FCommand write FCommand;

    property Data: TDValue read FData;
    property ResponseMessage: String read FResponseMessage;

    property LastResponseIntValue: Integer read FLastResponseIntValue write
        FLastResponseIntValue;

    procedure Clear;

    procedure MakeSubscribe(pvSubscribeArgs: array of string);

    procedure MakePublish(pvChannel:String; pvMessage:String);

    procedure MakeHSET(pvKey, pvField, pvValue:String);

    procedure MakeSET(pvKey, pvValue: String);


    function ExtractSubscribeMessage: String;
  end;

  TRedisClient = class(TObject)
  private
    FActive: Boolean;
    FRawSocket: TRawSocket;
    FStream: TMemoryStream;
    FHost: String;
    FPort: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Close;
    function SendCMD(pvRedisCMD: TRedisCommand): Integer;
    function RecvCMD(pvRedisCMD: TRedisCommand): Integer;

    function ReceiveLength: Integer;

    procedure ExecuteCMD(pvRedisCMD:TRedisCommand);

    procedure CheckRecvResult(pvSocketResult: Integer);
    property Active: Boolean read FActive;
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;

  end;

function utils_redis_tester: string;

implementation

resourcestring
  STRING_E_RECV_ZERO = '服务端主动断开关闭';
  STRING_E_TIMEOUT   = '服务端响应超时';

{$IFDEF POSIX}

{$ELSE}
// <2007版本的Windows平台使用
//   SOSError = 'System Error.  Code: %d.'+sLineBreak+'%s';
procedure RaiseLastOSErrorException(LastError: Integer);
var       // 高版本的 SOSError带3个参数
  Error: EOSError;
begin
  if LastError <> 0 then
    Error := EOSError.CreateResFmt(@SOSError, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := EOSError.CreateRes(@SUnkOSError);
  Error.ErrorCode := LastError;
  raise Error;
end;
{$ENDIF}


function utils_redis_tester: string;
var
  lvRedisCmd: TRedisCommand;
  lvRedisClient: TRedisClient;
begin
  lvRedisClient := TRedisClient.Create;
  lvRedisCmd := TRedisCommand.Create;
  lvRedisClient.Host := '127.0.0.1';
  lvRedisClient.Port := 6379;
  lvRedisClient.Connect;

  lvRedisCmd.MakeSubscribe(['tester', 'news']);

  lvRedisClient.SendCMD(lvRedisCmd);

  lvRedisCmd.Clear;
  if lvRedisClient.RecvCMD(lvRedisCmd) = -1 then
  begin
    RaiseLastOSError;
  end;

  lvRedisCmd.Clear;
  if lvRedisClient.RecvCMD(lvRedisCmd) = -1 then
  begin
    RaiseLastOSError;
  end;

  Result := lvRedisCmd.ResponseMessage;

end;

constructor TRedisCommand.Create;
begin
  inherited Create;
  FData := TDValue.Create();
  SetPackageMaxLength(@FRawPackage, MAX_LEN);
end;

destructor TRedisCommand.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TRedisCommand.MakeSubscribe(pvSubscribeArgs: array of string);
var
  i:Integer;
begin
  Clear;
  Command := 'subscribe';
  Data.Clear;

  for i := Low(pvSubscribeArgs) to High(pvSubscribeArgs) do
  begin
    Data.Add.AsString := pvSubscribeArgs[i];

  end;
  //Data.Add.AsString := pvSubscribeArgs;
end;

procedure TRedisCommand.Clear;
begin
  FData.Clear;
  ResetPacakge(@FRawPackage);
  FDecodePackTypeChr := Char(0);
  FDecodeTypeChr := Char(0);
  FDecodeCurrentIndex := 0;
  FDecodeCurrentParamLength := 0;
end;

procedure TRedisCommand.ConfigStartAndEndBytes(pvStart, pvEnd: string);
var
  lvBytes: TBytes;
  r: Integer;
begin
  SetLength(lvBytes, 128);
  r := StringToBytes(pvStart, lvBytes);
  SetPackageStartBytes(@FRawPackage, lvBytes, 0, r);

  r := StringToBytes(pvEnd, lvBytes);
  SetPackageEndBytes(@FRawPackage, lvBytes, 0, r);
end;

function TRedisCommand.ExtractSubscribeMessage: String;
begin
  Result := '';
  if LowerCase(FCommand) = 'message' then
  begin
    if FData.Count > 1 then     
      Result := FData[1].AsString;
  end;
end;

function TRedisCommand.InputBufferForDecode(pvBuf: Byte): Integer;
var
  lvBytes: TBytes;
  l: Integer;
  lvTempStr:String;
begin
  if FDecodePackTypeChr = Char(0) then
  begin
    // 用单行回复，回复的第一个字节将是“+”
    // 错误消息，回复的第一个字节将是“-”
    // 整型数字，回复的第一个字节将是“:”
    // 批量回复，回复的第一个字节将是“$”
    // 多个批量回复，回复的第一个字节将是“*”
    FDecodePackTypeChr := InputBufferForTypeChar(pvBuf);
  end;

  if FDecodePackTypeChr = '*' then
  begin
    l := InputBufferForMultiResponse(pvBuf);
    if l = -1 then
    begin
      ResetPacakge(@FRawPackage);
      FDecodeFlag := 0;
      Result := -1;
      Exit;
    end;
    Result := l;
    Exit;
  end
  else if FDecodePackTypeChr in ['+', '-', ':'] then
  begin // + 单行回复
    if FDecodeTypeChr = Char(0) then
    begin
      FDecodeTypeChr := FDecodePackTypeChr;
      ResetPacakge(@FRawPackage);
      ConfigStartAndEndBytes(FDecodeTypeChr, sLineBreak);
    end;
    // type chr也输入到rawPackage中
    l := InputBuffer(@FRawPackage, pvBuf);
    if l = 1 then
    begin
      if FDecodeTypeChr = ':' then
      begin
        lvTempStr := BytesToString(FRawPackage.FRawBytes, 1);
        FLastResponseIntValue := StrToInt(lvTempStr);
      end;
      FResponseMessage := Utf8BytesToString(FRawPackage.FRawBytes, 1);
      Result := 1;
      ResetPacakge(@FRawPackage);
      Exit;
    end;
  end;

  Result := 0;
end;

function TRedisCommand.InputBufferForMultiResponse(pvBuf: Byte): Integer;
var
  r: Integer;
  s: String;
  function InnerCheckResponse(pvByteIndex:Integer):Integer;
  begin
    if FDecodeCurrentIndex = 0 then
    begin
      FLastCmdString := Trim(BytesToString(FRawPackage.FRawBytes, pvByteIndex));
      FCommand := FLastCmdString;
    end
    else
    begin
      FLastCmdString := Trim(Utf8BytesToString(FRawPackage.FRawBytes, pvByteIndex));
      FData.Add.AsString := FLastCmdString;
    end;

    Inc(FDecodeCurrentIndex);
    if FDecodeCurrentIndex = FDecodeArgs then
    begin
      Result := 1;
    end else
    begin
      Result := 0;
    end;
  end;

begin
  if (FDecodeCurrentParamLength = 0) and (FDecodeTypeChr = Char(0)) then
  begin
    FDecodeTypeChr := Char(pvBuf);
    if FDecodeTypeChr = '*' then
    begin
      ResetPacakge(@FRawPackage);
      ConfigStartAndEndBytes('*', sLineBreak);
    end
    else if FDecodeTypeChr = '$' then
    begin
      ResetPacakge(@FRawPackage);
      ConfigStartAndEndBytes('$', sLineBreak);
    end
    else if FDecodeTypeChr = ':' then
    begin
      ResetPacakge(@FRawPackage);
      ConfigStartAndEndBytes(':', sLineBreak);
    end
    else if FDecodeTypeChr = '+' then
    begin
      ResetPacakge(@FRawPackage);
      ConfigStartAndEndBytes('+', sLineBreak);
    end
    else if FDecodeTypeChr = '-' then
    begin
      ResetPacakge(@FRawPackage);
      ConfigStartAndEndBytes('-', sLineBreak);
    end
    else
    begin
      Result := -1;
      Exit;
    end;
  end;

  if FDecodeTypeChr = '*' then
  begin
    Result := InputBufferForDefaultStr(pvBuf);
    if Result = 1 then
    begin
      FDecodeArgs := StrToInt(FLastCmdString);
      FDecodeCurrentIndex := 0;
      ResetPacakge(@FRawPackage); //
      FDecodeTypeChr := Char(0);
      Result := 0;
    end;
  end
  else if FDecodeTypeChr = '$' then
  begin
    Result := InputBufferForDefaultStr(pvBuf);
    if Result = 1 then
    begin
      //
      FDecodeCurrentParamLength := StrToInt(FLastCmdString) + 2;
      FDecodeTypeChr := Char(0);
      ResetPacakge(@FRawPackage); //
      Result := 0;
    end;
  end
  else if FDecodeCurrentParamLength > 0 then
  begin
    if FRawPackage.FRawLength <= FDecodeCurrentParamLength then
    begin
      FRawPackage.FRawBytes[FRawPackage.FRawLength] := pvBuf;
      Inc(FRawPackage.FRawLength);
    end;
    if FRawPackage.FRawLength = FDecodeCurrentParamLength then
    begin
      if InnerCheckResponse(0) = 1 then
      begin
        FDecodeCurrentParamLength := 0;
        ResetPacakge(@FRawPackage); // 准备接收参数
        Result := 1;
        Exit;
      end;
      FDecodeCurrentParamLength := 0;
      ResetPacakge(@FRawPackage); // 准备接收参数
    end;

    Result := 0;
  end
  else if FDecodeTypeChr = ':' then
  begin
    Result := InputBufferForDefaultStr(pvBuf);
    if Result = 1 then
    begin
      Result := InnerCheckResponse(1);
      FLastResponseIntValue := StrToInt(FLastCmdString);
      if Result = 1 then
      begin
        ResetPacakge(@FRawPackage); //
        FDecodeTypeChr := Char(0);
        Result := 1;
        Exit;
      end;
      Result := 0;
    end;
  end;
end;

function TRedisCommand.InputBufferForDefaultStr(pvBuf: Byte): Integer;
begin
  Result := InputBuffer(@FRawPackage, pvBuf);
  if Result = 1 then
  begin
    FLastCmdString := Trim(BytesToString(FRawPackage.FRawBytes, 1));
  end;
end;

function TRedisCommand.InputBufferForTypeChar(pvBuf: Byte): Char;
begin
  Result := Char(pvBuf);
  if not(Result in ['*', '$', '+', '-', ':']) then
  begin
    Result := Char(0);
  end;
end;

procedure TRedisCommand.MakeHSET(pvKey, pvField, pvValue:String);
begin
  Clear;
  Command := 'HSET';

  Data.Clear;
  Data.Add.AsString := pvKey;
  Data.Add.AsString := pvField;
  Data.Add.AsString := pvValue;

end;

procedure TRedisCommand.MakePublish(pvChannel:String; pvMessage:String);
begin
//  Clear;
//  Command := 'subscribe';
//  Data.Clear;
//
//  for i := Low(pvSubscribeArgs) to High(pvSubscribeArgs) do
//  begin
//    Data.Add.AsString := pvSubscribeArgs[i];
//  end;
end;

procedure TRedisCommand.MakeSET(pvKey, pvValue: String);
begin
  Clear;
  Command := 'SET';

  Data.Clear;
  Data.Add.AsString := pvKey;
  Data.Add.AsString := pvValue;

end;

constructor TRedisClient.Create;
begin
  inherited Create;
  FRawSocket := TRawSocket.Create();
  FStream := TMemoryStream.Create;
end;

destructor TRedisClient.Destroy;
begin
  FreeAndNil(FRawSocket);
  FStream.Free;
  inherited Destroy;
end;

procedure TRedisClient.CheckRecvResult(pvSocketResult: Integer);
var
  lvErrorCode:Integer;
begin
  if pvSocketResult = -2 then
  begin
    self.Close;
    raise Exception.Create(STRING_E_TIMEOUT);
  end;
  {$IFDEF POSIX}
  if (pvSocketResult = -1) or (pvSocketResult = 0) then
  begin
     try
       RaiseLastOSError;
     except
       FRawSocket.Close;
       raise;
     end;
   end;
  {$ELSE}
  if (pvSocketResult = SOCKET_ERROR) then
  begin
    lvErrorCode := GetLastError;
    FRawSocket.Close;     // 出现异常后断开连接

    {$if CompilerVersion < 23}
    RaiseLastOSErrorException(lvErrorCode);
    {$ELSE}
    RaiseLastOSError(lvErrorCode);
    {$ifend}
  end;
  {$ENDIF}
end;

procedure TRedisClient.Connect;
begin
  FRawSocket.CreateTcpSocket;
  if not FRawSocket.Connect(FHost, FPort) then
  begin
    RaiseLastOSError;
  end;
  FActive := True;
end;

procedure TRedisClient.Close;
begin
  FRawSocket.Close;
  FActive := False;
end;

procedure TRedisClient.ExecuteCMD(pvRedisCMD:TRedisCommand);
var
  r:Integer;
begin
  SendCMD(pvRedisCMD);
  pvRedisCMD.Clear;
  r := RecvCMD(pvRedisCMD);
  CheckRecvResult(r);
end;

function TRedisClient.ReceiveLength: Integer;
begin
  Result := FRawSocket.ReceiveLength;
end;

function TRedisClient.RecvCMD(pvRedisCMD: TRedisCommand): Integer;
var
  lvData: Byte;
  lvRecvBuff: AnsiString;
begin
  while True do
  begin
    SetLength(lvRecvBuff, 1024);
    // l := FRawSocket.RecvBuf(PAnsiChar(lvRecvBuff)^, 1024);
    Result := FRawSocket.RecvBuf(lvData, 1);
    if Result <= 0 then
    begin
      Break;
    end
    else
    begin
      Result := pvRedisCMD.InputBufferForDecode(lvData);
      if Result = 1 then
      begin
        Break;
      end;
    end;
  end;

end;

function TRedisClient.SendCMD(pvRedisCMD: TRedisCommand): Integer;
var
  lvData, lvArgCmd: String;
  lvEndBytes: array [0 .. 1] of Byte;
  lvBytes, lvArgBytes: TBytes;
  l, l2, i: Integer;
  lvItem: TDValue;
begin
  lvEndBytes[0] := 13;
  lvEndBytes[1] := 10;
  FStream.Clear;
  SetLength(lvBytes, 1024);
  SetLength(lvArgBytes, 1024);;

  // arg num
  lvData := Format('*%d' + sLineBreak, [pvRedisCMD.Data.Count + 1]);
  l := StringToUtf8Bytes(lvData, lvBytes);
  FStream.Write(lvBytes[0], l);

  // cmd
  lvData := pvRedisCMD.Command;
  l := StringToUtf8Bytes(lvData, lvBytes);
  lvArgCmd := Format('$%d%s', [l, sLineBreak]);
  l2 := StringToUtf8Bytes(lvArgCmd, lvArgBytes);
  // len
  FStream.Write(lvArgBytes[0], l2);
  // cmd
  FStream.Write(lvBytes[0], l);
  FStream.Write(lvEndBytes[0], 2);

  for i := 0 to pvRedisCMD.Data.Count - 1 do
  begin
    lvItem := pvRedisCMD.Data[i];
    // arg[i];
    lvData := lvItem.AsString;
    l := StringToUtf8Bytes(lvData, lvBytes);

    lvArgCmd := Format('$%d%s', [l, sLineBreak]);
    l2 := StringToUtf8Bytes(lvArgCmd, lvArgBytes);
    // len
    FStream.Write(lvArgBytes[0], l2);
    // cmd
    FStream.Write(lvBytes[0], l);
    FStream.Write(lvEndBytes[0], 2);
  end;

  Result := FRawSocket.SendBuf(FStream.Memory^, FStream.Size);
end;

end.
