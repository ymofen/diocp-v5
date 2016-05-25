unit JSonStream;


interface

uses
  superobject, Classes, SysUtils, Windows;

type
  TJsonStream = class(TObject)
  private
    FJson: ISuperObject;

    FStream: TStream;

    FInnerStream: TMemoryStream;

    function GetStream: TStream;

    procedure SetJson(const Value: ISuperObject);

    procedure SetStream(const Value: TStream);
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Clear(pvClearJSon: Boolean = true);

    function setJSonString(pvString:String): Boolean;

    function getJSonString: String;

    procedure setResultMsg(msg :string);

    procedure setResult(result:Boolean);

    function getResult: Boolean;

    function getResultStreamType: Integer;

    function getResultMsg: string;

    function getErrCode: Integer;

    property Json: ISuperObject read FJson write SetJson;

    property Stream: TStream read GetStream write SetStream;



  end;

implementation


constructor TJsonStream.Create;
begin
  inherited Create;
  FJson := SO();
  FInnerStream := TMemoryStream.Create();
  Stream := FInnerStream;
end;

destructor TJsonStream.Destroy;
begin
  FJson := nil;
  FreeAndNil(FInnerStream);
  inherited Destroy;
end;

procedure TJsonStream.Clear(pvClearJSon: Boolean = true);
begin
  if pvClearJSon then FJson.Clear();
  if FStream <> nil then
  begin
    FStream.Size := 0;
    FStream.Position := 0;
  end;
end;

function TJsonStream.getErrCode: Integer;
begin
  Result := FJson.I['__result.errCode'];
end;

function TJsonStream.getJSonString: String;
begin
  Result := FJson.AsJSon(False, False);
end;

function TJsonStream.getResult: Boolean;
begin
  if (FJson <> nil) and (FJson.O['__result.result'] <> nil) then
  begin
    Result := FJson.B['__result.result'];
  end else
  begin
    Result := True;
  end;
end;

function TJsonStream.getResultMsg: string;
begin
  if FJson <> nil then
  begin
    Result := FJson.S['__result.msg'];
  end else
  begin
    Result := '';
  end;
end;

function TJsonStream.getResultStreamType: Integer;
begin
  if (FJson <> nil) and (FJson.O['__result.streamType'] <> nil) then
  begin
    Result := FJson.I['__result.streamType'];
  end else
  begin
    Result := 0;
  end;
end;

function TJsonStream.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TJsonStream.SetJson(const Value: ISuperObject);
begin
  FJson := Value;
end;

function TJsonStream.setJSonString(pvString:String): Boolean;
begin
  FJson := SO(pvString);
  if (FJson <> nil) and (FJson.IsType(stObject)) then
  begin
    Result := true;
  end else
  begin
    FJson := SO();
  end;            
end;

procedure TJsonStream.setResult(result:Boolean);
begin
  if FJson = nil then FJson := SO();

  FJson.B['__result.result'] := result;
end;

procedure TJsonStream.setResultMsg(msg: string);
begin
  if FJson = nil then FJson := SO();
  
  FJson.S['__result.msg'] := msg;
end;

procedure TJsonStream.SetStream(const Value: TStream);
begin
  FStream := Value;
end;

end.
