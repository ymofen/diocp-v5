unit ufrmMain;

interface

{$IF CompilerVersion>25}  // XE4(VER250)
  {$DEFINE HAVE_GENERICS}
{$IFEND}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, utils_DValue, utils_strings, ComCtrls,
  utils_dvalue_multiparts, utils_dvalue_msgpack, utils_base64, utils_dvalue_dataset,
  DB, DBClient, ComObj, Grids, DBGrids, utils_byteTools, Math;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    mmoData: TMemo;
    btnParseJSON: TButton;
    btnEncodeJSON: TButton;
    btnClear: TButton;
    btnObjectTester: TButton;
    pgcMain: TPageControl;
    tsJSON: TTabSheet;
    tsMultiParts: TTabSheet;
    btnSave: TButton;
    btnParse: TButton;
    tsMsgPack: TTabSheet;
    btnMsgPackTester: TButton;
    btnParseAFile: TButton;
    dlgOpenFile: TOpenDialog;
    tsDValue: TTabSheet;
    btnDValue: TButton;
    btnBase64: TButton;
    Button1: TButton;
    btnDValueCloneFrom: TButton;
    btnDValueSetLength: TButton;
    btnInputJSONBuffer: TButton;
    btnRemovePath: TButton;
    mmoLog: TMemo;
    btnSetJSON: TButton;
    tsDataSet: TTabSheet;
    btnConvertToDValue: TButton;
    cdsDemo: TClientDataSet;
    btnInitCDSDemo: TButton;
    dbgrdDemo: TDBGrid;
    dsMain: TDataSource;
    mmoJSONData: TMemo;
    btnDValueToDataSet: TButton;
    btnEmptyDemo: TButton;
    btnClearTimeOut: TButton;
    tsLoadFile: TTabSheet;
    btnLoadTextFrom: TButton;
    btnAdd1000: TButton;
    btnSort: TButton;
    btnParseDValue: TButton;
    btnSortDValue: TButton;
    btnDelete: TButton;
    btnParseFile: TButton;
    procedure btnAdd1000Click(Sender: TObject);
    procedure btnBase64Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnClearTimeOutClick(Sender: TObject);
    procedure btnConvertToDValueClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDValueClick(Sender: TObject);
    procedure btnDValueCloneFromClick(Sender: TObject);
    procedure btnDValueSetLengthClick(Sender: TObject);
    procedure btnDValueToDataSetClick(Sender: TObject);
    procedure btnEmptyDemoClick(Sender: TObject);
    procedure btnEncodeJSONClick(Sender: TObject);
    procedure btnInitCDSDemoClick(Sender: TObject);
    procedure btnInputJSONBufferClick(Sender: TObject);
    procedure btnLoadTextFromClick(Sender: TObject);
    procedure btnMsgPackTesterClick(Sender: TObject);
    procedure btnObjectTesterClick(Sender: TObject);
    procedure btnParseAFileClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure btnParseDValueClick(Sender: TObject);
    procedure btnParseFileClick(Sender: TObject);
    procedure btnParseJSONClick(Sender: TObject);
    procedure btnRemovePathClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSetJSONClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure btnSortDValueClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FDValueObj:TDValue;
    FLogObj: TDValue;
    FDValue: TDValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

function CreateField(const pvDataSet: TDataSet; const pvFieldName: string;
    const pvFieldType: TFieldType; const pvSize: Integer = 0; const
    pvDisplayWidth: Integer = 0; const pvDisplayLabel: String = STRING_EMPTY):
    TField;

var
  Form1: TForm1;
  __sn:Integer;

implementation

uses
  utils_DValue_JSON, utils_textfile;

{$R *.dfm}

function CreateField(const pvDataSet: TDataSet; const pvFieldName: string;
    const pvFieldType: TFieldType; const pvSize: Integer = 0; const
    pvDisplayWidth: Integer = 0; const pvDisplayLabel: String = STRING_EMPTY):
    TField;
var
  lvOwner:TComponent;
begin
  lvOwner := pvDataSet;
  case pvFieldType of
    ftString:
      begin
        Result := TStringField.Create(lvOwner);
        Result.Size := pvSize;
        if Result.Size = 0 then
        begin
          Result.Size := 80;
        end;
      end;
    ftWideString:
      begin
        Result := TWideStringField.Create(lvOwner);
        Result.Size := pvSize;
        if Result.Size = 0 then
        begin
          Result.Size := 80;
        end;
      end;
    ftBCD:
      begin
        Result := TBCDField.Create(lvOwner);
        TBCDField(Result).Precision := 18;
        TBCDField(Result).Size := 4;
      end;
    ftGuid:
      begin
        Result := TGuidField.Create(lvOwner);
      end;
    ftBlob:
      begin
        Result := TBlobField.Create(lvOwner);
      end;
    ftMemo:
      begin
        Result := TMemoField.Create(lvOwner);
      end;
    ftBoolean:
      begin
        Result := TBooleanField.Create(lvOwner);
      end;
    ftDateTime:
      begin
        Result := TDateTimeField.Create(lvOwner);
      end;
    ftSmallint:
      begin
        Result := TSmallintField.Create(lvOwner);
      end;
    ftInteger:
      begin
        Result := TIntegerField.Create(lvOwner);
      end;
    ftWord:
      begin
        Result := TWordField.Create(lvOwner);
      end;
  end;
  if Result = nil then raise Exception.CreateFmt('�ֶ�(%s)����ʧ��', [pvFieldName]);

  Result.FieldName := pvFieldName;
  Result.DataSet := pvDataSet;
  Result.FieldKind := fkData;
  if Length(pvDisplayLabel) > 0 then
  begin
    Result.DisplayLabel := pvDisplayLabel;
  end;

  if pvDisplayWidth > 0 then
  begin
    Result.DisplayWidth := pvDisplayWidth;
  end;
//  if stringCanName(pvFieldName) then
//  begin
//    Result.Name := pvDataSet.Name + pvFieldName;
//  end;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogObj := TDValue.Create();
  FDValue := TDValue.Create();
end;

destructor TForm1.Destroy;
begin
  FreeAndNil(FDValue);
  FreeAndNil(FLogObj);
  inherited Destroy;
end;

procedure TForm1.btnAdd1000Click(Sender: TObject);
var
  lvItem:TDValue;
  i: Integer;
  s:String;
begin
  // �鿴�ڴ����

  for i := 0 to 10000 - 1 do
  begin
    lvItem:= TDValue.Create();
    try
      lvItem.ForceByName('sn').AsInteger := i;
      lvItem.ForceByName('content').AsString := '��־����';
      lvItem.ForceByName('time').AsDateTime := Now();
      s := JSONEncode(lvItem);
      lvItem.Clear;
      JSONParser(s, lvItem);
      if FLogObj.Count > 100 then
      begin
        FLogObj.Delete(0);
      end;
      FLogObj.AddArrayChild(lvItem.Clone());
    finally
      lvItem.Free;
    end;
  end;
end;

procedure TForm1.btnBase64Click(Sender: TObject);
var
  lvDValue:TDValue;
begin
  lvDValue:= TDValue.Create();
  lvDValue.Base64LoadFromFile(ParamStr(0));
  lvDValue.Base64SaveToFile('base64OK.exe');
  lvDValue.Free;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  mmoData.Clear;
end;

procedure TForm1.btnClearTimeOutClick(Sender: TObject);
var
  sn:Integer;
begin
  sn := InterlockedIncrement(__sn);
  if FDValueObj = nil then
  begin
    FDValueObj := TDValue.Create();

    mmoLog.Clear;
  end;
  FDValueObj.ForceByPath(Format('a.%d.value', [sn])).AsString := NowString;
  FDValueObj.ForceByPath(Format('a.%d.tick', [sn])).AsInteger := GetTickCount;
  FDValueObj.ForceByPath('a').ClearLastModify(10000);

  //
  mmoLog.Lines.Add(JSONEncode(FDValueObj, True, False));
end;

procedure TForm1.btnConvertToDValueClick(Sender: TObject);
var
  lvDValue, lvList:TDValue;
begin
  if not cdsDemo.Active then raise Exception.Create('DEMO���ݼ���δ��ʼ��');
  
  lvDValue := TDValue.Create();
  lvList := lvDValue.ForceByName('list').AsArray();  // ����Ҫ�����ͷ�
  ConvertDataSetToDValue(self.cdsDemo, lvList);
  mmoJSONData.Clear;
  mmoJSONData.Lines.Add(JSONEncode(lvDValue));
  lvDValue.Free;
end;

procedure TForm1.btnDValueClick(Sender: TObject);
var
  lvDValue, lvDValue2:TDValue;

begin
  lvDValue := TDValue.Create();
  lvDValue.AddArrayChild.AsString := 'abc';
  ShowMessage(JSONEncode(lvDValue));
  lvDValue.Delete(0);
  ShowMessage(JSONEncode(lvDValue));
  lvDValue.AddArrayChild.AsString := 'efg';

  // ���봦��
  lvDValue2 := lvDValue.UnAttach(0);
  ShowMessage(JSONEncode(lvDValue2));

  DisposeDValueObject(lvDValue);
  DisposeDValueObject(lvDValue2);


end;

procedure TForm1.btnDValueCloneFromClick(Sender: TObject);
var
  lvDValue, lvDValue2:TDValue;
begin
  lvDValue := TDValue.Create();
  lvDValue2 := TDValue.Create();
  lvDValue.ForceByName('a').AsInteger := 1;
  lvDValue2.CloneFrom(lvDValue);
  ShowMessage(Format('%d -> %d', [lvDValue.ForceByName('a').AsInteger, lvDValue2.ForceByName('a').AsInteger]));
  lvDValue2.ForceByName('a').AsInteger := 3;
  ShowMessage(Format('%d -> %d', [lvDValue.ForceByName('a').AsInteger, lvDValue2.ForceByName('a').AsInteger]));
  lvDValue.Free;
  lvDValue2.Free;
end;

procedure TForm1.btnDValueSetLengthClick(Sender: TObject);
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create();
  lvDValue.Value.SetArraySize(1);
  lvDValue.Value.Items[0].AsString := 'abcd';
  lvDValue.Value.SetArraySize(2);
  ShowMessage(lvDValue.Value.Items[0].AsString);
  lvDValue.Free;
end;

procedure TForm1.btnDValueToDataSetClick(Sender: TObject);
var
  lvDValue:TDValue;
begin
  if not cdsDemo.Active then raise Exception.Create('DEMO���ݼ���δ��ʼ��');
  if cdsDemo.RecordCount > 0 then raise Exception.Create('�������DEMO���ݼ�(Empty)');
  
  
  lvDValue := TDValue.Create();
  JSONParser(mmoJSONData.Lines.Text, lvDValue);
  AppendFromDValueList(cdsDemo, lvDValue);
  lvDValue.Free;

end;

procedure TForm1.btnEmptyDemoClick(Sender: TObject);
begin
  cdsDemo.EmptyDataSet;
end;

procedure TForm1.btnEncodeJSONClick(Sender: TObject);
var
  lvDValue, lvItem:TDValue;
  lvValue:Integer;
  lvSB:TDStringBuilder;
  s:String;
  lvBytes:TBytes;
begin
  lvDValue := TDValue.Create();
  lvDValue.ForceByPath('p2.obj').BindObject(Self, faNone);
  lvDValue.ForceByPath('p2.n').AsInteger := 3;
//  lvDValue.ForceByName('name').AsString := '����abc';
//  lvDValue.ForceByName('__msgid').AsInteger := 1001;
//  lvDValue.ForceByPath('p1.name').AsString := 'D10.�����';
//  lvDValue.ForceByPath('p2.p2_1.name').AsString := 'D10.�����';
//  lvDValue.ForceByPath('p2.num').AsInteger := 1;
//
//
//  lvItem := lvDValue.ForceByName('array').AddArrayChild;
//  lvItem.ForceByName('key1').AsString := '����Ԫ��1';
//  lvDValue.ForceByName('array').AddArrayChild.AsString := '����Ԫ��2';

  s :=JSONEncode(lvDValue, true, False, [vdtObject]);
  if trim(mmoData.Lines.Text) = '' then
  begin
    mmoData.Lines.Add(s);
  end;

  lvDValue.Free;

  lvBytes := StringToBytes(Trim(s));
  mmoData.Lines.Add(TByteTools.varToHexString(lvBytes[0], Length(lvBytes)));

  ShowMessage(s);
end;

procedure TForm1.btnInitCDSDemoClick(Sender: TObject);
var
  i: Integer;
begin
  // ��ʼ�����ݼ�
  cdsDemo.Close;
  cdsDemo.FieldDefs.Clear;
  cdsDemo.Fields.Clear;
  CreateField(cdsDemo, 'fid', ftGuid).DisplayWidth := 10;
  CreateField(cdsDemo, 'fcode', ftString, 50).DisplayWidth := 20;
  CreateField(cdsDemo, 'fname', ftString, 50).DisplayWidth := 20;
  cdsDemo.CreateDataSet;


  // ��ʾ��¼
  for i := 0 to 100 - 1 do
  begin
    cdsDemo.Append;
    cdsDemo.FieldByName('fid').AsString := CreateClassID;
    cdsDemo.FieldByName('fcode').AsString := Format('%0.3d', [i + 1]);
    cdsDemo.FieldByName('fname').AsString := 'dvalue';
    cdsDemo.Post;
  end;
end;

procedure TForm1.btnInputJSONBufferClick(Sender: TObject);
var
  lvJSONBuffer:TJsonBuffer;
  lvCache:TMemoryStream;
  lvStr:AnsiString;
  lvBuf:PByte;
  l:Integer;
  i, r: Integer;
begin
  lvStr := mmoData.Lines.Text;
  lvCache := TMemoryStream.Create;
  try
    lvJSONBuffer.FBuffer := lvCache;
    ResetJsonBuffer(@lvJSONBuffer);
    lvBuf := PByte(lvStr);
    l := Length(lvStr);
    for i := 0 to l - 1 do
    begin
      r := InputJsonBuffer(@lvJSONBuffer, lvBuf^);
      Inc(lvBuf);
      if r = 1 then
      begin
        ShowMessage(ByteBufferToString(lvCache.Memory, lvCache.Size));
        ResetJsonBuffer(@lvJSONBuffer);
        lvCache.Clear;
      end;

    end;

  finally
    lvCache.Free;
  end;

  ;
end;

procedure TForm1.btnLoadTextFromClick(Sender: TObject);
var
  s, lvFileName:String;
begin
//  lvFileName := ExtractFilePath(ParamStr(0)) + 'text.txt';
//  mmoLog.Clear;
//  s := LoadTextFromFile(lvFileName);
//  mmoLog.Lines.Add(s);
//
//  s := LoadTextA(lvFileName);
//
//  mmoLog.Lines.Add('=================');
//  mmoLog.Lines.Add(s);

end;

procedure TForm1.btnMsgPackTesterClick(Sender: TObject);
var
  lvFileStream:TFileStream;
  lvDValue:TDValue;
  lvFileName:String;
begin
  lvFileName := ExtractFilePath(ParamStr(0)) + 'dvalue_msgpack.dat';
  DeleteFile(lvFileName);

  lvFileStream := TFileStream.Create(lvFileName, fmCreate);
  try
    lvDValue := TDValue.Create();
    lvDValue.ForceByPath('hello.��ע').AsString:= 'HELLO�й�' + sLineBreak + 'World ���';
    lvDValue.ForceByName('fileID').AsString:= ExtractFileName(ParamStr(0));
    lvDValue.ForceByName('data').AsStream.LoadFromFile(ParamStr(0));
    MsgPackEncode(lvDValue, lvFileStream);
    lvDValue.Free;
  finally
    lvFileStream.Free;
  end;

  lvDValue := TDValue.Create();
  MsgPackParseFromFile(lvFileName, lvDValue);
  ShowMessage(lvDValue.ForceByPath('hello.��ע').AsString);
  lvDValue.ForceByName('data').AsStream.SaveToFile('dvalue_parse.dat');
  lvDValue.Free;

end;

procedure TForm1.btnObjectTesterClick(Sender: TObject);
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create();

  // ����Ϊ����Ϊ3
  lvDValue.Value.SetArraySize(3);

  lvDValue.Value.Items[0].BindObject(TButton.Create(nil), faFree);
  lvDValue.Value.Items[1].BindObject(TButton.Create(nil), faFree);
  lvDValue.Value.Items[2].BindObject(TButton.Create(nil), faFree);

  // ����Ϊ����Ϊ2(���ͷ�һ��)
  lvDValue.Value.SetArraySize(2);
  
  lvDValue.Free;

end;

procedure TForm1.btnParseAFileClick(Sender: TObject);
var
  lvDVAlue:TDValue;
  lvTickCount:Cardinal;
begin
  if not dlgOpenFile.Execute then Exit;
  lvDVAlue := TDValue.Create();
  lvTickCount := GetTickCount;
  MultiPartsParseFromFile(lvDVAlue, dlgOpenFile.FileName);
  Self.Caption := Format('MultiPartsParseFromFile, time:%d ns', [GetTickCount - lvTickCount]);

  if lvDVAlue.Count > 0 then
  begin
    ShowMessage(
     Format('%s:%s', [lvDVAlue.Items[0].ForceByName('name').AsString,
            ExtractValueAsUtf8String(lvDVAlue, lvDVAlue.Items[0].ForceByName('name').AsString, '')]));
  end;

  ShowMessage(JSONEncode(lvDVAlue, false, True));
  lvDVAlue.Free;


end;

procedure TForm1.btnParseClick(Sender: TObject);
var
  lvDVAlue:TDValue;
  lvTickCount:Cardinal;
begin
  lvDVAlue := TDValue.Create();
  lvTickCount := GetTickCount;
  MultiPartsParseFromFile(lvDVAlue, 'dvalue_multparts.dat');
  Self.Caption := Format('MultiPartsParseFromFile, time:%d ns', [GetTickCount - lvTickCount]);
  SavePartValueToFile(lvDVAlue, 'filedata', 'abc.dat');
  ShowMessage(ExtractValueAsUtf8String(lvDVAlue, 'fileID', ''));
  lvDVAlue.Free;
end;

procedure TForm1.btnParseJSONClick(Sender: TObject);
var
  lvDValue, lvDValue2, lvAccountGroup:TDValue;
begin
  lvDValue := TDValue.Create();
  try
    JSONParser(mmoData.Lines.Text, lvDValue);
    ShowMessage(JSONEncode(lvDValue));

    //lvDValue2 := lvDValue.FindByName('maps').Items[0];


    //ShowMessage(lvDValue2.ForceByPath('name').AsString);
  finally
    lvDValue.Free;
  end;

end;

procedure TForm1.btnRemovePathClick(Sender: TObject);
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create;
  lvDValue.ForceByPath('a.b.c').AsString := '1234';
  lvDValue.ForceByPath('a.b.e').AsInteger := 567;
  mmoLog.Lines.Add(JSONEncode(lvDValue, false, True));
  lvDValue.RemoveByPath('a.b.c');
  mmoLog.Lines.Add(JSONEncode(lvDValue, false, True));
  lvDValue.RemoveByPath('a.b');
  mmoLog.Lines.Add(JSONEncode(lvDValue, false, True));
  lvDValue.Free;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  lvFileStream:TFileStream;

  lvDValue:TDValue;

  lvBuilder:TDBufferBuilder;
begin

  lvBuilder := TDBufferBuilder.Create;
  lvDValue := TDValue.Create();
  lvDValue.ForceByName('��ע').AsString:= 'HELLO�й�' + sLineBreak + 'World ���';
  lvDValue.ForceByName('fileID').AsString:= ExtractFileName(ParamStr(0));
  AddFieldValue(lvDValue, 'e����e', '�ܶ��ַ�abc');
  AddFilePart(lvDValue, 'data', ParamStr(0));

  MultiPartsEncode(lvDValue, lvBuilder, '');
  lvDValue.Free;

  lvBuilder.SaveToFile(ExtractFilePath(ParamStr(0)) + 'dvalue_multparts.dat');
  lvBuilder.Free;


end;

procedure TForm1.btnSetJSONClick(Sender: TObject);
var
  lvDValue:TDValue;
begin
  lvDValue := TDValue.Create;
  lvDValue.ForceByPath('a.b.c').AsString := '1234';
  JSONParser('{"value":123}', lvDValue.ForceByPath('a.b.e'));
  mmoLog.Lines.Add(JSONEncode(lvDValue, false, false));

  JSONParser('{"value":456}', lvDValue.ForceByPath('a.b.e'));
  mmoLog.Lines.Add(JSONEncode(lvDValue, false, false));
  JSONParser('{"key":001}', lvDValue.ForceByPath('a.b.e'));
  mmoLog.Lines.Add(JSONEncode(lvDValue, false, false));
  lvDValue.Free;

end;


function MySort(Item1, Item2: Pointer): Integer;
var
  lvItm1, lvItm2:TDValue;
begin
  lvItm1 := Item1;
  lvItm2 := Item2;
  Result := CompareValue(lvItm1.GetValueByName('id', 0), lvItm2.GetValueByName('id', 0));
end;
  
procedure TForm1.btnParseDValueClick(Sender: TObject);
begin
  FDValue.Clear;
  JSONParser(mmoData.Lines.Text, FDValue);
end;

procedure TForm1.btnSortClick(Sender: TObject);
var
  lvDValue:TDValue;

begin
  lvDValue := TDValue.Create;
  JSONParser('{id:2, val:2}', lvDValue.ForceByName('id_2'));
  JSONParser('{id:1, val:1}', lvDValue.ForceByName('id_1'));
  mmoLog.Lines.Add(JSONEncode(lvDValue, false, false));

{$IFDEF HAVE_GENERICS}
{$ELSE}
  lvDValue.Sort(MySort);
{$ENDIF}
  mmoLog.Lines.Add('after sort');
  mmoLog.Lines.Add(JSONEncode(lvDValue, false, false));
  lvDValue.Free;

end;

function UnloadPosiCompareFunc(Item1, Item2: Pointer): Integer;
var
  lvItm1, lvItm2:TDValue;
begin
  lvItm1 := Item1;
  lvItm2 := Item2;
  Result := CompareValue(lvItm1.GetValueByName('icount', 0), lvItm2.GetValueByName('icount', 0));
  if Result = 0 then
  begin
    Result := CompareValue(lvItm1.ForceByName('lasttick').AsDateTime, lvItm2.ForceByName('lasttick').AsDateTime);
  end;
  //Result := -Result;
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
begin
  while FDValue.Count > 5 do
  begin
    FDValue.Delete(0);
  end;
  mmoLog.Lines.Add('after delete');
  mmoLog.Lines.Add(JSONEncode(FDValue, false, True));
end;

procedure TForm1.btnParseFileClick(Sender: TObject);
var
  s:String;
var
  lvDValue, lvDValue2, lvAccountGroup:TDValue;
begin
  if not dlgOpenFile.Execute then exit;
  s := LoadTextFromFile(dlgOpenFile.FileName);

  lvDValue := TDValue.Create();
  try
    JSONParser(s, lvDValue);
    ShowMessage(Format('�����ɹ�, �ڵ���:%d', [lvDValue.Count]));
  finally
    lvDValue.Free;
  end;


end;

procedure TForm1.btnSortDValueClick(Sender: TObject);
begin
{$IFDEF HAVE_GENERICS}
{$ELSE}
  FDValue.Sort(UnloadPosiCompareFunc);
{$ENDIF}

  mmoLog.Lines.Add('after sort');
  mmoLog.Lines.Add(JSONEncode(FDValue, false, True));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  lvStream, lvStream2:TMemoryStream;
  lvStr1, lvStr2:string;
  l:Integer;

begin
  lvStream := TMemoryStream.Create();
  lvStream2 := TMemoryStream.Create();
  lvStream.LoadFromFile(ParamStr(0));
  lvStr1 := Base64Encode(lvStream);
  lvStr2 := Base64Encode(PByte(lvStream.Memory), lvStream.Size);
  if lvStr1 <> lvStr2 then
  begin
    ShowMessage('no');
  end;

  lvStream2.SetSize(Length(lvStr2));
  l:=Base64Decode(PByte(lvStr2), Length(lvStr2), PByte(lvStream2.Memory));
  lvStream2.SetSize(l);
  lvStream2.SaveToFile('base64_2.dat');

  lvStream2.Clear;
  Base64Decode(lvStr1, lvStream2);

  lvStream2.SaveToFile('base64.dat');


  lvStream.Free;
  lvStream2.Free;
  


end;

end.
