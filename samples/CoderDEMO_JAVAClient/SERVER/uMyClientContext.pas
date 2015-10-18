unit uMyClientContext;

interface

uses
  diocp.coder.tcpServer, SysUtils, Classes, Windows, Math, superobject,
  diocp_ex_strObjectCoder;


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

procedure TMyClientContext.DoContextAction(const pvObject: TObject);
var
  lvJSON:ISuperObject;
begin
  lvJSON := SO(TStringObject(pvObject).DataString);
  if lvJSON <> nil then
  begin
    lvJSON.S['agent'] := 'DIOCP服务器进行了处理';

    // 设置返回的数据为JSON 字符串
    TStringObject(pvObject).DataString := lvJSON.AsJSon(True, False);
  end;

  // 会写对象
  writeObject(pvObject);
end;

procedure TMyClientContext.OnConnected;
begin

end;

procedure TMyClientContext.OnDisconnected;
begin
end;

end.
