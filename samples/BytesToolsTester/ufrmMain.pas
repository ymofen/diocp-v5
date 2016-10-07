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
    btnByteAsBinary: TButton;
    procedure btnByteAsBinaryClick(Sender: TObject);
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

procedure TForm1.btnByteAsBinaryClick(Sender: TObject);
var
  v, v1, v2:Byte;

  w:Word;
begin
  v := $84;
  mmoOutputs.Lines.Add('0x84:' + TByteTools.varToBinaryString(v, 1));

  w := $ABCD;
  mmoOutputs.Lines.Add(Format('%2x: %d, swap16:%2x, binary:%s', [w, w,
    TByteTools.swap16(w), TByteTools.varToBinaryString(w, 2, ' ')]));

  v := $81;
  v1 := v;
  TByteTools.SetLow4Bit(v1, $FE);

  v2 := v;
  TByteTools.SetHigh4Bit(v2, $FE);
  mmoOutputs.Lines.Add(Format('%d: %2x, binary:%s, off low4bit:%2x, on high4bit:%2x',
    [v, v, TByteTools.varToBinaryString(v, 1, ' '),
    v1, v2]));

  
end;

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
