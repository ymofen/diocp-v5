unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, utils_hashs;

type
  TfrmMain = class(TForm)
    btnTester: TButton;
    mmoLog: TMemo;
    procedure btnTesterClick(Sender: TObject);
  private
    { Private declarations }
    FHashTable: TDHashTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TfrmMain.Destroy;
begin

  inherited Destroy;
end;

procedure TfrmMain.btnTesterClick(Sender: TObject);
var
  i, r, iObj: Integer;
  lvSucc:Boolean;
  lvList, lvList2:TList;
  lvHashTable:TDHashTable;
begin
  lvHashTable := TDHashTable.Create(10949);
  lvList := TList.Create;
  lvList2 := TList.Create;
  try
    lvHashTable.Clear;
    for i := 0 to 65535 - 1 do
    begin
      Randomize;
      iObj := Random(65535);
      if lvList.IndexOf(TObject(iObj)) = -1 then
      begin
        lvHashTable.Add(iObj, TObject(iObj));
        lvList.Add(TObject(iObj));
      end;
    end;

    for i := 0 to 65535 - 1 do
    begin
      Randomize;
      iObj := Random(65535);
      if lvList.IndexOf(TObject(iObj)) = -1 then
      begin
        lvHashTable.Add(iObj, TObject(iObj));
        lvList.Add(TObject(iObj));
      end;

      lvSucc := lvHashTable.DeleteFirst(iObj);
      if not lvSucc then
      begin
//        lvHashTable.Add(iObj, TObject(iObj));
//        lvSucc := lvHashTable.DeleteFirst(iObj);
//        if not lvSucc then
//        begin
//          mmoLog.Lines.Add(IntToStr(iObj));
//        end;
        Assert(lvSucc);
      end;
      lvList.Remove(TObject(iObj));
    end;

    Randomize;
    iObj := Random(65535);
    if lvList.IndexOf(TObject(iObj)) = -1 then
    begin
      lvHashTable.Add(iObj, TObject(iObj));
      lvList.Add(TObject(iObj));
    end;

//    for i := 0 to 1000 - 1 do
//    begin
//      lvSucc := lvHashTable.DeleteFirst(iObj);
//      Assert(lvSucc);
//      lvHashTable.Add(iObj, TObject(iObj));
//    end;


    lvList2.Clear;
    lvHashTable.GetDatas(lvList2);
    mmoLog.Lines.Add(Format('hash %d, list:%d',
      [lvHashTable.Count, lvList2.Count]));

    for i := 0 to lvList.Count - 1 do
    begin
      r := lvList2.IndexOf(lvList[i]);
      if r =-1 then
      begin
        mmoLog.Lines.Add(Format('err:%d', [IntPtr(lvList[i])]));
      end;


    end;


  finally
    lvHashTable.Free;
    lvList.Free;
    lvList2.Free;
  end;

  

  


  
end;

end.
