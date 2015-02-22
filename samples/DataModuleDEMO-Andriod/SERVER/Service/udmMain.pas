(*
  每个客户端一个实例, 在线程中运行
  
*)
unit udmMain;


interface

uses
  SysUtils, Classes, DB, ADODB, Provider, IniFiles, DBClient;

type
  TdmMain = class(TDataModule)
    conMain: TADOConnection;
    dspMain: TDataSetProvider;
    qryMain: TADOQuery;
    cdsMain: TClientDataSet;
    procedure DataModuleCreate(Sender: TObject);
  public
    /// <summary>
    /// 客户端调用
    //     执行入口函数
    //  对vData所做的修改将会返回到客户端
    /// </summary>
    function Execute(pvCmdIndex: Integer; var vData: OleVariant; var vMsg: string):
        Boolean;
  end;

var
  dmMain: TdmMain;

implementation

{$R *.dfm}

procedure TdmMain.DataModuleCreate(Sender: TObject);
var
  lvINI:TIniFile;
  lvStr:String;
  lvFile:String;
begin
  qryMain.DisableControls;
  lvFile := ChangeFileExt(ParamStr(0), '.db.ini');
  lvINI := TIniFile.Create(lvFile);
  try
    lvStr := lvINI.ReadString('main', 'connectionString', '');
    if lvStr <> '' then
    begin
      conMain.ConnectionString := lvStr;
    end else
    begin
      lvINI.WriteString('main', 'connectionString', conMain.ConnectionString);
    end;                                       
  finally
    lvINI.Free;
  end;
end;

function TdmMain.Execute(pvCmdIndex: Integer; var vData: OleVariant; var vMsg:
    string): Boolean;
begin
  case pvCmdIndex of
    0:
      begin
        // 返回服务端时间给客户端
        vData := Now();
        Result := true;
      end;
    1:  // 查询数据
      begin
        // vData 认为是传入的SQL语句
        //   执行后, vData为查询的数据，可以用于对ClientData.Data的赋值

        qryMain.Close;
        qryMain.SQL.Clear;
        qryMain.SQL.Add(vData);
        qryMain.Open;

        dspMain.DataSet := qryMain;
        vData := dspMain.Data;

        /// 测试CDS解析数据是是否正常XE6自带的CDS解析时会出现异常
//        try
//          cdsMain.Data := vData;
//        except
//          on E:Exception do
//          begin
//            raise Exception.Create('服务端尝试赋值给cdsMain.Data时出现了异常:' + e.Message);
//          end;
//        end;

        qryMain.Close;
        Result := true;
      end;
    2:
      begin
        // vData 为执行的语句
        conMain.BeginTrans;
        try
          qryMain.Close;
          qryMain.SQL.Clear;
          qryMain.SQL.Add(vData);
          qryMain.ExecSQL;
          conMain.CommitTrans;

          VarClear(vData);
          
          Result := true;


        except
          conMain.RollbackTrans;
          raise;
        end;
      end;
  end;
end;

end.
