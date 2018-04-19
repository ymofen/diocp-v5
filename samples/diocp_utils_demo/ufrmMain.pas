unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    pgcMain: TPageControl;
    tsUtils_Strings: TTabSheet;
    btnGetStrValueOfName: TButton;
    mmoLog: TMemo;
    btnSB: TButton;
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
