unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, utils_hashs, utils_strings;

type
  TfrmMain = class(TForm)
    btnTester: TButton;
    mmoLog: TMemo;
    btnTester2: TButton;
    dlgOpen: TOpenDialog;
    edtFindAddr: TEdit;
    edtKey: TEdit;
    procedure btnTester2Click(Sender: TObject);
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

procedure TfrmMain.btnTester2Click(Sender: TObject);
var
  lvStrs:TStrings;
  s, s1, s2, s3:String;
  lvPtr:PChar;
  i, c, c1, c2, c3, c4: Integer;

var
  r, iObj: Integer;
  lvSucc:Boolean;
  lvList, lvList2:TList;
  lvHashTable:TDHashTable;
begin
  if not dlgOpen.Execute then exit;
  lvHashTable := TDHashTable.Create(10949);
  lvStrs := TStringList.Create;
  lvList:= TList.Create;
  try
    lvStrs.LoadFromFile(dlgOpen.FileName);
    for i := 0 to lvStrs.Count - 1 do
    begin
      s := lvStrs[i];
      lvPtr := PChar(s);
      SkipUntil(lvPtr, [']']);
      SkipUntil(lvPtr, [':']);
      SkipChars(lvPtr, [':']);

      // flag, 1,2,3,4
      s1 := LeftUntil(lvPtr, [',']);

      if s1 = '1' then
      begin
        // count
        SkipChars(lvPtr, [':', ',']);
        s1 := LeftUntil(lvPtr, [',']);
        c1 := StrToInt(s1);
      end else if s1 = '3' then
      begin
        // count
        SkipChars(lvPtr, [':', ',']);
        s1 := LeftUntil(lvPtr, [',']);
        c2 := StrToInt(s1);
      end else  if s1 = '2' then
      begin  // add end
        // count
        SkipChars(lvPtr, [':', ',']);
        s1 := LeftUntil(lvPtr, [',']);
        c := StrToInt(s1);

        // key
        SkipChars(lvPtr, [':', ',']);
        s2 := LeftUntil(lvPtr, [',']);

        // obj
        SkipChars(lvPtr, [':', ',']);
        s3 := LeftUntil(lvPtr, [',']);
        if s3 = '' then s3 := lvPtr;
        s3 := Trim(s3);


        if s3 = edtFindAddr.Text then
        begin
          s3 := edtFindAddr.Text;
          c3 := i;
          if i = 8413 then
          begin
            c3 := 8413;
          end;
        end;
        

        lvHashTable.Add(StrToInt(s2), TObject(StrToInt(s3)));

        Assert((c = lvHashTable.Count) and (c = c1 + 1));
      end else if s1='4' then
      begin      // remove
        // count
        SkipChars(lvPtr, [':', ',']);
        s1 := LeftUntil(lvPtr, [',']);
        c := StrToInt(s1);

        // key
        SkipChars(lvPtr, [':', ',']);
        s2 := LeftUntil(lvPtr, [',']);

        // obj
        SkipChars(lvPtr, [':', ',']);
        s3 := LeftUntil(lvPtr, [',']);
        if s3 = '' then s3 := lvPtr;
        s3 := Trim(s3);

        if s3 = edtFindAddr.Text then
        begin
          s3 := edtFindAddr.Text;
          c4 := i;
        end;

        lvHashTable.DeleteFirst(StrToInt(s2));

        Assert((c = lvHashTable.Count) and (c = c2 - 1));
      end;
    end;

    mmoLog.Lines.Add(Format('c3:%d, c4:%d', [c3,c4]));

    c := StrToInt(edtKey.Text);

    if lvHashTable.FindFirstData(c) <> nil then
    begin
      mmoLog.Lines.Add('found');
    end else
    begin
      mmoLog.Lines.Add('not found!');
    end;

    lvHashTable.GetDatas(lvList);

    c := StrToInt(edtFindAddr.Text);
    if lvList.IndexOf(TObject(c)) = -1 then
    begin
      mmoLog.Lines.Add('list not found');
    end else
    begin
      mmoLog.Lines.Add('list found!');
    end;


    



  finally
    lvStrs.Free;
    lvHashTable.Free;
    lvList.Free;
  end;

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
