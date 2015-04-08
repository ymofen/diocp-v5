unit uMyClientContext;

interface

uses
  diocp.coder.tcpServer, SysUtils, Classes, Windows, Math, SimpleMsgPack;


type
  TMyClientContext = class(TIOCPCoderClientContext)
  private
  protected
    procedure OnDisconnected; override;

    procedure OnConnected; override;
  public
    /// <summary>
    ///   数据处理
    /// </summary>
    /// <param name="pvObject"> (TObject) </param>
    procedure DoContextAction(const pvObject: TObject); override;
  end;

implementation

uses
  CHATHandler, utils.safeLogger;

procedure TMyClientContext.DoContextAction(const pvObject: TObject);
var
  lvCMDObj:TSimpleMsgPack;
begin
  lvCMDObj := TSimpleMsgPack.Create;
  try
    try
      TMemoryStream(pvObject).Position := 0;
      lvCMDObj.DecodeFromStream(TMemoryStream(pvObject));

      
      CHATExecute(lvCMDObj, Self);    

      if lvCMDObj.O['cmdIndex'] <> nil then
      begin
        if lvCMDObj.O['result.code'] = nil then
        begin
          lvCMDObj.I['result.code'] := 0;
        end;
      end;
    except
      on E:Exception do
      begin
        lvCMDObj.ForcePathObject('result.code').AsInteger := -1;
        lvCMDObj.ForcePathObject('result.msg').AsString := e.Message;
        sfLogger.logMessage('处理逻辑出现异常:'+ e.Message);
        {$IFDEF CONSOLE}
        writeln('处理逻辑出现异常:'+ e.Message);
        {$ENDIF}
      end;
    end;

    TMemoryStream(pvObject).Clear; 
    lvCMDObj.EncodeToStream(TMemoryStream(pvObject));
    TMemoryStream(pvObject).Position := 0;
    WriteObject(pvObject);
  finally
    lvCMDObj.Free;
  end;
end;

procedure TMyClientContext.OnConnected;
begin

end;

procedure TMyClientContext.OnDisconnected;
begin
end;

end.
