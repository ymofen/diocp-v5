unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, diocp_ex_http_common;

type
  TForm1 = class(TForm)
    pgcMain: TPageControl;
    tsUtils_Strings: TTabSheet;
    btnGetStrValueOfName: TButton;
    mmoLog: TMemo;
    btnSB: TButton;
    btnCompare: TButton;
    tsHttpResponseHeader: TTabSheet;
    mmoHeader: TMemo;
    btnDecodeHeader: TButton;
    procedure btnCompareClick(Sender: TObject);
    procedure btnDecodeHeaderClick(Sender: TObject);
    procedure btnGetStrValueOfNameClick(Sender: TObject);
    procedure btnSBClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  utils_strings;

{$R *.dfm}

procedure TForm1.btnCompareClick(Sender: TObject);
var
  s1, s2:DStringW;
  p1, p2:PDCharW;
  r, l:Integer;
begin
  s1 := 'HTTP/1.1 503';
  s2 := 'HTTP/1.1';
  p1 := PDCharW(s1);
  p2 := PDCharW(s2);
  l := Length(s2) * 2;
  r := CompareWStrIgnoreCase(p1, p2, 0);
  mmoLog.Lines.Add(Format('compare(%s, %s, %d) = %d', [s1, s2,l, r]));
end;

procedure TForm1.btnDecodeHeaderClick(Sender: TObject);
var
  lvResp:THttpRespHeader;
  r:Integer;
begin
  lvResp := THttpRespHeader.Create();
  try
    r := lvResp.ParseHeader(mmoHeader.Lines.Text);
    mmoLog.Lines.Add(Format('r:%d, code :%d, code str:%s', [r, lvResp.HttpCode, lvResp.RespCodeStr]));
  finally
    lvResp.Free;
  end;

end;

procedure TForm1.btnGetStrValueOfNameClick(Sender: TObject);
var
  s, s1:String;
begin
  mmoLog.Clear;
  s := 'content-type:application/json; charset=utf-8;';
  s1 := GetStrValueOfName(s, 'charset', ['='], [';']); 
  mmoLog.Lines.Add('''' + s+ ''':'#9 + '''' + s1 + '''');

  s := 'content-type:application/json; charset=utf-8';
  s1 := GetStrValueOfName(s, 'charset', ['='], [';']); 
  mmoLog.Lines.Add('''' + s+ ''':'#9 + '''' + s1 + '''');

  s := 'content-type:application/json; charsets=utf-8; charset=GBK3212';
  s1 := GetStrValueOfName(s, 'charset', ['='], [';']);
  mmoLog.Lines.Add('''' + s+ ''':'#9 + '''' + s1 + '''');

  
  s := 'content-type:application/json; charsets=utf-8; charset= GBK3212 ';
  s1 := GetStrValueOfName(s, 'charset', ['=', ' '], [';']);
  mmoLog.Lines.Add('''' + s+ ''':'#9 + '''' + s1 + '''');

  s := 'content-type:application/json; charset =  utf-8';
  s1 := GetStrValueOfName(s, 'charset', ['=', ' '], [';', ' ']);
  mmoLog.Lines.Add('''' + s+ ''':'#9 + '''' + s1 + '''');

  s := '';
  s1 := GetStrValueOfName(s, 'charset', ['=', ' '], [';', ' ']);
  mmoLog.Lines.Add('''' + s+ ''':'#9 + '''' + s1 + '''');
end;

procedure TForm1.btnSBClick(Sender: TObject);
var
  lvSB1, lvSB2:TDStringWBuilder;
begin
  lvSB1 := TDStringWBuilder.Create;
  lvSB2 := TDStringWBuilder.Create;
  lvSB1.Append('HELLO000000');
  lvSB2.Append('YMF');
  lvSB1.DecChar(4).Append(lvSB2);
  mmoLog.Lines.Add(lvSB1.ToString);

end;

end.
