unit utils_URL;

interface

uses
  utils_strings;


type
  TURL = class(TObject)
  private
    FRawHostStr:String;

    FHost: string;
    FUser: String;
    FPassword: String;
    FParamStr: String;
    FPort: string;
    FProtocol: string;
    FURI: String;
    /// <summary>
    ///   分析路径部分
    ///   127.0.0.1:9983
    ///   user:password@127.0.0.1:9983/diocp/a.html
    /// </summary>
    procedure InnerParseUrlPath(const pvPath:String);
  public
    /// <summary>
    ///  解析URL
    ///  http://user:password@127.0.0.1:9983/diocp/a.html?qry=abcd&dd=xl
    /// </summary>
    procedure SetURL(pvURL:String);

    /// <summary>
    ///   协议, http, https, ftp, ws
    /// </summary>
    property Protocol: string read FProtocol write FProtocol;

    /// <summary>
    ///   主机地址
    /// </summary>
    property Host: string read FHost write FHost;

    /// <summary>
    ///   端口
    /// </summary>
    property Port: string read FPort write FPort;



    /// <summary>
    ///   参数
    /// </summary>
    property ParamStr: String read FParamStr write FParamStr;
    property RawHostStr: String read FRawHostStr;

    /// <summary>
    ///   URI部分
    /// </summary>
    property URI: String read FURI;
  end;



implementation

{ TURL }

procedure TURL.InnerParseUrlPath(const pvPath: String);
var
  lvP, lvTempP:PChar;
  lvTempStr:String;
begin
  if length(pvPath) = 0 then Exit;

  lvP := PChar(pvPath);
  /// user:password@
  lvTempStr := LeftUntil(lvP, ['@']);

  if lvTempStr <> '' then
  begin  // 存在用户名和密码
    lvTempP := PChar(lvTempStr);

    FUser := LeftUntil(lvTempP, [':']);
    if FUser <> '' then
    begin
      SkipChars(lvTempP, [':']);
      FPassword := lvTempP;
    end else
    begin
      // 无密码
      FUser := lvTempStr;
    end;
    SkipChars(lvP, ['@']);
  end;
  
  /// 127.0.0.1:9983/
  lvTempStr := LeftUntil(lvP, ['/']);
  if lvTempStr = '' then  // 没有URI
  begin
    lvTempStr := lvP;
    lvP := nil;
  end;
  FRawHostStr := lvTempStr;
  lvTempP := PChar(lvTempStr);
  FHost := LeftUntil(lvTempP, [':']);
  if FHost <> '' then
  begin
    SkipChars(lvTempP, [':']);
    FPort := lvTempP;
  end else
  begin  // 没有指定Port
    FHost := lvTempStr;
    FPort := '80';
  end;
  
  if lvP = nil then FURI := '/' else FURI := lvP;          
end;

procedure TURL.SetURL(pvURL: String);
var
  lvPSave, lvPUrl:PChar;
  lvTempStr, lvPath:String;
begin
  FProtocol := '';
  FHost := '';
  FRawHostStr := '';
  FPort := '';
  FPassword := '';
  FUser := '';
  FURI := '';

  lvPUrl := PChar(pvURL);

  if (lvPUrl = nil) or (lvPUrl^ = #0) then Exit;

  // http, ftp... or none
  FProtocol := LeftUntilStr(lvPUrl, '://');
  if FProtocol <> '' then lvPUrl := lvPUrl + 3; // 跳过 ://

  lvPSave := lvPUrl;  // 保存位置

  ///  路径和参数
  ///  www.diocp.org/image/xxx.asp
  lvTempStr := LeftUntil(lvPUrl, ['?']);

  // 如果没有参数
  if lvTempStr = '' then
  begin
    /// 路径和书签
    lvTempStr := LeftUntil(lvPUrl, ['#']);
    if lvTempStr <> '' then
    begin
      InnerParseUrlPath(lvTempStr);

      // lvPUrl = 剩下标签部分
    end else
    begin  // 没有书签，没有参赛,全部是路径
      lvTempStr := lvPUrl;
      InnerParseUrlPath(lvTempStr);
    end;
  end else
  begin  // 有参数
    InnerParseUrlPath(lvTempStr);

    SkipChars(lvPUrl, ['?']);
    // lvPUrl = 剩下参数和书签部分
    FParamStr := lvPUrl;
  end;




  





  


end;

end.
