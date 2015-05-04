unit DiocpFileOperator;

interface

uses
  SysUtils, SimpleMsgPack, Classes, Math,
  uStreamCoderSocket, uICoderSocket;


type
  TDiocpFileOperator = class(TObject)
  private
    FFileSize:Int64;
    FFileCheckSum:Cardinal;

    FCoderSocket: ICoderSocket;

    FCMDStream: TMemoryStream;
    FCMDObj:TSimpleMsgPack;

    function CheckSumAFile(pvFile:string): Cardinal;
    procedure pressINfo(pvSendObject: TSimpleMsgPack; pvRFile, pvType: String);
    procedure CheckReady;
  public
    constructor Create(ACoderSocket: ICoderSocket);
    destructor Destroy; override;
    procedure close;

    procedure SetCoderSocket(const pvSocket:ICoderSocket);

    procedure readFileINfo(pvRFile, pvType: string; pvChecksum: Boolean = true);

    function ReadFileSize(pvRFile, pvType: string):Int64;

    /// <summary>
    ///   上传一个文件
    /// </summary>
    /// <returns>
    ///  返回文件大小
    /// </returns>
    /// <param name="pvRFile"> (String) </param>
    /// <param name="pvLocalFile"> (string) </param>
    /// <param name="pvType"> (string) </param>
    function UploadFile(pvRFile:String; pvLocalFile:string; pvType:string): Int64;

    /// <summary>
    ///   下载一个文件
    /// </summary>
    /// <param name="pvRFile"> (String) </param>
    /// <param name="pvLocalFile"> (string) </param>
    /// <param name="pvType"> (string) </param>
    procedure DownFile(pvRFile:String; pvLocalFile:string; pvType:string);

    /// <summary>
    /// 复制一个文件
    /// </summary>
    /// <param name="pvRFile"> (String) </param>
    /// <param name="pvRDestFile"> (String) </param>
    /// <param name="pvType"> (String) </param>
    procedure CopyAFile(pvRFile, pvRDestFile, pvType: String);

    /// <summary>
    ///   删除一个文件
    /// </summary>
    /// <param name="pvRFile"> (String) </param>
    /// <param name="pvType"> (string) </param>
    procedure DeleteFile(pvRFile:String; pvType: string);
    class function verifyData(const buf; len:Cardinal): Cardinal;
    class function verifyStream(pvStream:TStream; len:Cardinal): Cardinal;

    property FileCheckSum: Cardinal read FFileCheckSum;

    property FileSize: Int64 read FFileSize;



    
  end;

implementation


const
  SEC_SIZE = 1024 * 1024;

procedure TDiocpFileOperator.close;
begin
  FCoderSocket.CloseSocket;
end;

procedure TDiocpFileOperator.CopyAFile(pvRFile, pvRDestFile, pvType: String);
var
  lvFileStream:TFileStream;
  lvRecvObj, lvSendObj:TSimpleMsgPack;
  i, l, lvSize:Integer;
begin
  lvSendObj := TSimpleMsgPack.Create;
  lvRecvObj := TSimpleMsgPack.Create;
  try
    lvSendObj.Clear();
    lvSendObj.S['cmd.namespace'] := 'fileaccess';
    lvSendObj.I['cmd.index'] := 5;   //copy文件
    lvSendObj.S['fileName'] := pvRFile;
    lvSendObj.S['newFile'] := pvRDestFile;

    lvSendObj.S['catalog'] := pvType;


    TStreamCoderSocket.SendObject(FCoderSocket, lvSendObj);
    TStreamCoderSocket.RecvObject(FCoderSocket, lvRecvObj);
    if not lvRecvObj.B['__result.result'] then
    begin
      raise Exception.Create(lvRecvObj.S['__result.msg']);
    end;
  finally
    lvSendObj.Free;
    lvRecvObj.Free;
  end;    
end;

constructor TDiocpFileOperator.Create(ACoderSocket: ICoderSocket);
begin
  inherited Create;
  FCMDStream := TMemoryStream.Create;
  FCMDObj := TSimpleMsgPack.Create;
  FCoderSocket := ACoderSocket;
end;

procedure TDiocpFileOperator.DeleteFile(pvRFile:String; pvType: string);
var
  i, l, lvSize:Integer;
begin
  CheckReady;
  FCMDObj.Clear();
  FCMDObj.S['cmd.namespace'] := 'fileaccess';
  FCMDObj.I['cmd.index'] := 4;   //删除文件
  FCMDObj.S['fileName'] := pvRFile;
  FCMDObj.S['catalog'] := pvType;
  FCMDStream.Clear;
  FCMDObj.EncodeToStream(FCMDStream);

  TStreamCoderSocket.SendObject(FCoderSocket, FCMDStream);

  FCMDStream.Clear;
  TStreamCoderSocket.RecvObject(FCoderSocket, FCMDStream);
  FCMDStream.Position := 0;
  FCMDObj.DecodeFromStream(FCMDStream);

  if not FCMDObj.B['__result.result'] then
  begin
    raise Exception.Create(FCMDObj.S['__result.msg']);
  end;

end;

destructor TDiocpFileOperator.Destroy;
begin
  FCMDStream.Free;
  FCMDObj.Free;
  FCoderSocket := nil;
  inherited Destroy;
end;

procedure TDiocpFileOperator.DownFile(pvRFile:String; pvLocalFile:string;
    pvType:string);
var
  lvRFileSize:Integer;
var
  lvFileStream:TFileStream;
  i, l, lvSize:Integer;
  lvFileName:String;
  lvCrc, lvChecksum, lvLocalCheckSum:Cardinal;
  lvBytes:TBytes;
begin
  CheckReady;
  readFileINfo(pvRFile, pvType);

  lvRFileSize := FFileSize;

  if lvRFileSize = 0 then
  begin
    raise Exception.CreateFmt('远程文件[%s]不存在!', [pvRFile]);
  end;

  lvLocalCheckSum := CheckSumAFile(pvLocalFile);
  if lvCheckSum = lvLocalCheckSum then
  begin
    Exit;
  end;
        
  //将文件分段下载<每段固定大小>
  //循环发送
  //  {
  //     fileName:'xxxx',  //客户端请求文件
  //     start:0,          //客户端请求开始位置

  //     filesize:11111,   //文件总大小
  //     crc:xxxx,         //服务端返回
  //     blockSize:4096   //服务端返回
  //  }


  lvFileName := pvLocalFile;
  SysUtils.DeleteFile(lvFileName);

  lvFileStream := TFileStream.Create(lvFileName, fmCreate or fmShareDenyWrite);
  try
    while true do
    begin
        
      FCMDObj.Clear();
      pressINfo(FCMDObj, pvRFile, pvType);


      FCMDObj.I['cmd.index'] := 1;
      FCMDObj.I['start'] := lvFileStream.Position;

      FCMDStream.Clear;
      FCMDObj.EncodeToStream(FCMDStream);

      TStreamCoderSocket.SendObject(FCoderSocket, FCMDStream);

      FCMDStream.Clear;
      TStreamCoderSocket.RecvObject(FCoderSocket, FCMDStream);
      FCMDStream.Position := 0;
      FCMDObj.DecodeFromStream(FCMDStream);

      if not FCMDObj.B['__result.result'] then
      begin
        raise Exception.Create(FCMDObj.S['__result.msg']);
      end;

//      lvCrc := TCRCTools.crc32Stream(lvRecvObj.Stream);
//      if lvCrc <> lvRecvObj.I['crc'] then
//      begin
//        raise Exception.Create('crc校验失败!');
//      end;
      lvBytes := FCMDObj.ForcePathObject('data').AsBytes;
      lvFileStream.Write(lvBytes[0], Length(lvBytes));

        
      //文件下载完成
      if lvFileStream.Size = FCMDObj.I['fileSize'] then
      begin
        Break;
      end;
    end;
  finally
    lvFileStream.Free;
  end; 
end;

procedure TDiocpFileOperator.CheckReady;
begin
  if FCoderSocket = nil then
    raise Exception.Create('缺少CoderSocket操作接口!');
end;

function TDiocpFileOperator.CheckSumAFile(pvFile:string): Cardinal;
var
  lvFileStream:TFileStream;
  lvCrc:Cardinal;
begin
  result := 0;
  if FileExists(pvFile) then
  begin
    lvFileStream := TFileStream.Create(pvFile, fmOpenRead);
    try
      result := verifyStream(lvFileStream, 0);
    finally
      lvFileStream.Free;
    end;
  end;  
end;

procedure TDiocpFileOperator.pressINfo(pvSendObject: TSimpleMsgPack; pvRFile,
    pvType: String);
begin
  pvSendObject.S['cmd.namespace'] := 'fileaccess';
  pvSendObject.S['fileName'] := pvRFile;
  pvSendObject.S['catalog'] := pvType;
end;

procedure TDiocpFileOperator.readFileINfo(pvRFile, pvType: string; pvChecksum:
    Boolean = true);
var
  lvFileStream:TFileStream;
  i, l, lvSize:Integer;
begin
  CheckReady;
  FCMDObj.Clear();
  FCMDObj.S['cmd.namespace'] := 'fileaccess';
  FCMDObj.I['cmd.index'] := 3;   //文件信息
  FCMDObj.B['cmd.checksum'] := pvChecksum;   //获取checksum值
  FCMDObj.S['fileName'] := pvRFile;
  FCMDObj.S['catalog'] := pvType;

  FCMDStream.Clear;
  FCMDObj.EncodeToStream(FCMDStream);
  TStreamCoderSocket.SendObject(FCoderSocket, FCMDStream);

  FCMDStream.Clear;
  TStreamCoderSocket.RecvObject(FCoderSocket, FCMDStream);
  FCMDStream.Position := 0;
  FCMDObj.DecodeFromStream(FCMDStream);
  
  if not FCMDObj.B['__result.result'] then
  begin
    raise Exception.Create(FCMDObj.S['__result.msg']);
  end;
  FFileSize := FCMDObj.I['info.size'];
  FFileCheckSum := FCMDObj.I['info.checksum'];

end;

function TDiocpFileOperator.ReadFileSize(pvRFile, pvType: string): Int64;
begin
  readFileINfo(pvRFile, pvType, False);
  Result := FFileSize;
end;

procedure TDiocpFileOperator.SetCoderSocket(const pvSocket: ICoderSocket);
begin
  FCoderSocket := pvSocket;
end;

{ TDiocpFileOperator }


function TDiocpFileOperator.UploadFile(pvRFile:String; pvLocalFile:string;
    pvType:string): Int64;
var
  lvFileStream:TFileStream;

  lvPosition, i, l, lvSize:Int64;
  lvCheckSum, lvLocalCheckSum:Cardinal;
begin
  CheckReady;
  //将文件分段传递<每段固定大小> 4K
  //循环发送
  //  {
  //     fileName:'xxxx',
  //     crc:xxxx,
  //     start:0,   //开始位置
  //     eof:true,  //最后一个
  //  }

  
  //lvLocalCheckSum := CheckSumAFile(pvLocalFile);

  lvFileStream := TFileStream.Create(pvLocalFile, fmOpenRead);
  try
    //readFileINfo(pvRFile, pvType);

//    lvCheckSum := FFileCheckSum;
//
//
//    if lvCheckSum = lvLocalCheckSum then
//    begin
////      if FProgConsole <> nil then
////      begin
////        FProgConsole.SetHintText('秒传文件...');
////        FProgConsole.SetPosition(lvFileStream.Size);
////        Sleep(1000);
////      end;
//      Exit;
//    end;

    while true do
    begin
//      if FProgConsole <> nil then
//      begin
//        if FProgConsole.IsBreaked then Break;
//      end;
//
      FCMDObj.Clear();
      if pvRFile = '' then
      begin
        pressINfo(FCMDObj, ExtractFileName(pvLocalFile), pvType);
      end else
      begin
        pressINfo(FCMDObj, pvRFile, pvType);
      end;
      FCMDObj.S['cmd.namespace'] := 'fileaccess';
      FCMDObj.I['cmd.index'] := 2;   //上传文件

      lvPosition:=lvFileStream.Position;
      FCMDObj.I['start'] := lvPosition;
//      if lvFileStream.Position = 102400 then
//      begin
//        FCMDObj.I['start'] := lvFileStream.Position;
//      end;
//      if lvFileStream.Position = 0 then
//      begin
//        FCMDObj.I['start'] := 0;
//      end;

//     FCMDObj.S['startStr'] := IntToStr(lvFileStream.Position);
      
      lvSize := Min(SEC_SIZE, lvFileStream.Size-lvFileStream.Position);
      if lvSize = 0 then
      begin
        Break;
      end else
      begin
        FCMDObj.ForcePathObject('data').LoadBinaryFromStream(lvFileStream, lvSize);

        FCMDObj.I['size'] := lvSize;
        if (lvFileStream.Position = lvFileStream.Size) then
        begin
          FCMDObj.B['eof'] := true;
        end;

        FCMDStream.Clear;
        FCMDObj.EncodeToStream(FCMDStream);


        TStreamCoderSocket.SendObject(FCoderSocket, FCMDStream);

        FCMDStream.Clear;
        TStreamCoderSocket.RecvObject(FCoderSocket, FCMDStream);
        FCMDStream.Position := 0;
        FCMDObj.DecodeFromStream(FCMDStream);

        if not FCMDObj.B['__result.result'] then
        begin
          raise Exception.Create(FCMDObj.S['__result.msg']);
        end;

  //      if FProgConsole <> nil then
  //      begin
  //        FProgConsole.SetPosition(lvFileStream.Position);
  //      end;

        if (lvFileStream.Position = lvFileStream.Size) then
        begin
          Break;
        end;
      end;
    end;
    Result := lvFileStream.Size;
  finally
    lvFileStream.Free;
  end;
end;

class function TDiocpFileOperator.verifyData(const buf; len: Cardinal): Cardinal;
var
  i:Cardinal;
  p:PByte;
begin
  i := 0;
  Result := 0;
  p := PByte(@buf);
  while i < len do
  begin
    Result := Result + p^;
    Inc(p);
    Inc(i);
  end;
end;

class function TDiocpFileOperator.verifyStream(pvStream:TStream; len:Cardinal):
    Cardinal;
var
  l, j:Cardinal;
  lvBytes:TBytes;
begin
  SetLength(lvBytes, 1024);

  if len = 0 then
  begin
    j := pvStream.Size - pvStream.Position;
  end else
  begin
    j := len;
  end;

  Result := 0;

  while j > 0 do
  begin
    if j <1024 then l := j else l := 1024;
    
    pvStream.ReadBuffer(lvBytes[0], l);

    Result := Result + verifyData(lvBytes[0], l);
    Dec(j, l);
  end;
end;

end.
