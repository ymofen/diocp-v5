unit frm_Login;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmLogin = class(TForm)
    edtUserID: TEdit;
    edtPaw: TEdit;
    btnOk: TButton;
    btnClose: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
    procedure OnContextActionEx(AObject: TObject);
  public
    { Public declarations }
  end;

var
  frmLogin: TfrmLogin;

implementation

uses frm_Client, ClientIocpOper, SimpleMsgPack;

{$R *.dfm}

procedure TfrmLogin.btnCloseClick(Sender: TObject);
begin
  Self.ModalResult := mrClose;
end;

procedure TfrmLogin.btnOkClick(Sender: TObject);
begin
  DiocpContext.Host := '192.168.1.10';
  DiocpContext.Port := 60544;
  // DiocpContext现在还没有连接，所以不用LockContext
  DiocpContext.OnContextAction := OnContextActionEx;
  CMD_Login(edtUserID.Text, edtPaw.Text);  // 登录请求
end;

procedure TfrmLogin.OnContextActionEx(AObject: TObject);
var
  vStream: TMemoryStream;
  vMsgPack: TSimpleMsgPack;
begin
  vStream := TMemoryStream(AObject);
  vMsgPack:= TSimpleMsgPack.Create;
  try
    vMsgPack.DecodeFromStream(vStream);  // 解密消息
    if vMsgPack.ForcePathObject('result.code').AsInteger <> -1 then  // 请求在服务端执行成功
    begin
      if vMsgPack.ForcePathObject('requestID').AsString = 'login' then  // 是登录请求返回的信息
      begin
        if vMsgPack.ForcePathObject('result.code').AsInteger = 0 then  // 登录成功
        begin
          DiocpContext.LockContext('登录完成', Self);
          try
            CurUserID := edtUserID.Text;
            DiocpContext.OnContextAction := nil;  // 交出事件，准备给客户窗体使用
          finally
            DiocpContext.UnLockContext('登录完成', Self);
          end;
          Self.ModalResult := mrOk;
        end;
      end
    end;
  finally
    vMsgPack.Free;
  end;
end;

end.
