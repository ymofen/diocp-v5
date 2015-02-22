unit uMsgPackObject;

interface

uses
  SysUtils, QMsgPack, uBufferRW, uZipTools;

type
  TMsgPackObject = class(TObject)
  private
    FZiped:Byte;
    FNameSpaceID:Integer;
    FMsgPack: TQMsgPack;
    FHeadBytes:TBytes;
    FBufferBytes:TBytes;
  public
    constructor create;
    destructor destroy; override;
    procedure setHeadBytes(pvHead:TBytes);
    procedure setBufferBytes(pvBuffer:TBytes);

    ///解码到msgPack
    procedure Decode;

    property MsgPack: TQMsgPack read FMsgPack;    
  end;

implementation

constructor TMsgPackObject.Create;
begin
  inherited Create;
  FMsgPack := TqmsgPack.Create;
end;

procedure TMsgPackObject.Decode;
var
  lvBufferReader:IBufferReader;
begin
  FZiped:= 0;
  if Length(FHeadBytes) > 0 then
  begin
    lvBufferReader := TBufferReader.create(@FHeadBytes[0], length(FHeadBytes));
    lvBufferReader.read(FZiped, SizeOf(FZiped));
    lvBufferReader.read(FNameSpaceID, SizeOf(FNameSpaceID));
    lvBufferReader := nil;
  end;

  if (FMsgPack = nil) then
  begin
    FMsgPack := TQMsgPack.Create;
  end;
  
  //压缩了
  if FZiped = 1 then
  begin
    FBufferBytes := TBytes(TZipTools.unCompressBuf(FBufferBytes[0], Length(FBufferBytes)));
    FMsgPack.Parse(FBufferBytes);
  end else
  begin
    //直接解析
    FMsgPack.Parse(FBufferBytes);
  end; 
end;

destructor TMsgPackObject.Destroy;
begin
  if FMsgPack <> nil then
  begin
    FMsgPack.Free;
  end;
  setLength(FBufferBytes, 0);
  setLength(FHeadBytes, 0);
  inherited Destroy;
end;

procedure TMsgPackObject.setBufferBytes(pvBuffer: TBytes);
begin
  FBufferBytes := pvBuffer;
end;

procedure TMsgPackObject.setHeadBytes(pvHead:TBytes);
begin
  FHeadBytes := pvHead;  
end;

end.
