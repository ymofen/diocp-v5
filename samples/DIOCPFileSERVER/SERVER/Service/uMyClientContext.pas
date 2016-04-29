unit uMyClientContext;

interface

uses
  diocp_coder_tcpServer, Classes,  SysUtils, uZipTools, SimpleMsgPack;

type
  TMyClientContext = class(TIOCPCoderClientContext)
  protected

    procedure OnDisconnected; override;

    procedure OnConnected;override;

  protected

    /// <summary>
    ///   on received a object
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure DoContextAction(const pvDataObject:TObject); override;
  public

    
  end;

implementation

uses
  uFileOperaHandler;

{ TMyClientContext }

procedure TMyClientContext.DoContextAction(const pvDataObject:TObject);
var
  lvMsgPack:TSimpleMsgPack;
  lvStream :TStream;
begin
  lvStream := TStream(pvDataObject);
  lvMsgPack := TSimpleMsgPack.Create;
  try
    try
      lvStream.Position := 0;
      // unpack
      lvMsgPack.DecodeFromStream(lvStream);

      TFileOperaHandler.Execute(lvMsgPack);

      lvMsgPack.ForcePathObject('__result.result').AsBoolean := true;
    except
      on E:Exception do
      begin
        lvMsgPack.Clear;
        lvMsgPack.ForcePathObject('__result.result').AsBoolean := false;
        lvMsgPack.ForcePathObject('__result.msg').AsString := e.Message;
      end;
    end;

    lvStream.Size := 0;
    lvMsgPack.EncodeToStream(lvStream);
    lvStream.Position := 0;
    // send to client
    self.writeObject(lvStream);

  finally
    lvMsgPack.Free;
  end;
end;

procedure TMyClientContext.OnConnected;
begin
  inherited;
end;

procedure TMyClientContext.OnDisconnected;
begin
  inherited;
end;

end.
