unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, utils_byteTools, qstring;

type
  TForm1 = class(TForm)
    mmoOutputs: TMemo;
    btnNumToBytes: TButton;
    edtNum: TEdit;
    mmoBuffer: TMemo;
    btnGetBitUInt64: TButton;
    procedure btnGetBitUInt64Click(Sender: TObject);
    procedure btnNumToBytesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnGetBitUInt64Click(Sender: TObject);
var
  lvBytes, lvBytes2:TBytes;
  lvNum:UInt64;
begin
  lvBytes := TByteTools.HexStrToBytes(mmoBuffer.Lines.Text);

  mmoOutputs.Lines.Add(TByteTools.varToByteString(lvBytes[0], 6));
//  ExchangeByteOrder(PQCharA(@lvBytes[0]), 6);
//  mmoOutputs.Lines.Add(TByteTools.varToHexString(lvBytes[0], 6));
  TByteTools.SwapBuff(@lvBytes[0], 1, 5);
  mmoOutputs.Lines.Add(TByteTools.varToByteString(lvBytes[0], 6));

//
//  lvNum := TByteTools.GetBitUInt64(@lvBytes[0], 1, 5);
//  mmoOutputs.Lines.Add(IntToStr(lvNum));
//  mmoOutputs.Lines.Add(TByteTools.varToHexString(lvNum, 8));
end;

procedure TForm1.btnNumToBytesClick(Sender: TObject);
var
  num:UInt64;
begin
  num := StrToInt64(edtNum.Text);
  mmoOutputs.Lines.Add(TByteTools.varToHexString(num, 8));
end;

end.
