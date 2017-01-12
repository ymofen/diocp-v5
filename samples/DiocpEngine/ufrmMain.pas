unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, diocp_core_engine, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    tmrIInfo: TTimer;
    Label1: TLabel;
    btnExitAWorker: TButton;
    btnAddAWorker: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btn1: TButton;
    edtNum: TEdit;
    procedure btn1Click(Sender: TObject);
    procedure btnAddAWorkerClick(Sender: TObject);
    procedure btnExitAWorkerClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tmrIInfoTimer(Sender: TObject);
  private
    { Private declarations }
    FEngine: TIocpEngine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEngine := TIocpEngine.Create();
  FEngine.Start;
end;

destructor TForm1.Destroy;
begin
  FreeAndNil(FEngine);
  inherited Destroy;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  FEngine.WorkerCount := StrToInt(edtNum.Text);
end;

procedure TForm1.btnAddAWorkerClick(Sender: TObject);
begin
  FEngine.CreateWorker(True);
end;

procedure TForm1.btnExitAWorkerClick(Sender: TObject);
begin
  FEngine.PostAExitRequest;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  if not FEngine.Active then  FEngine.Start;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  FEngine.SafeStop();
end;

procedure TForm1.tmrIInfoTimer(Sender: TObject);
begin
  Label1.Caption := Format('%d/%d/%d', [FEngine.ActiveWorkerCount, FEngine.WorkerCount, FEngine.MaxWorkerCount]);
end;

end.
