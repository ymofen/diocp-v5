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
    procedure btnGetStrValueOfNameClick(Sender: TObject);
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

end.
