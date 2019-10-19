unit utils_diocp_http_tools;

interface

uses
  diocp_ex_httpClient, utils_dvalue;

function GetHttpResponseAsJSON(pvHttpClt:TDiocpHttpClient): TDValue;

procedure PostHttpWithJSON(pvHttpClt:TDiocpHttpClient; pvUrl:String; pvReq,
    pvResp:TDValue); overload;

function PostHttpWithJSON(pvHttpClt:TDiocpHttpClient; pvUrl:String;
    pvReq:TDValue): string; overload;

implementation

uses
  utils_DValue_JSON;

function GetHttpResponseAsJSON(pvHttpClt:TDiocpHttpClient): TDValue;
var
  lvVal:TDValue;
  lvStr:String;
begin
  lvVal := TDValue.Create();
  Result := lvVal;
  lvStr := pvHttpClt.GetResponseBodyAsString;
  JSONParser(lvStr, Result);
end;

procedure PostHttpWithJSON(pvHttpClt:TDiocpHttpClient; pvUrl:String; pvReq,
    pvResp:TDValue);
var
  lvStr:String;
begin
  lvStr := JSONEncode(pvReq, False, False);
  pvHttpClt.RequestContentType := 'applicaiton/json';
  pvHttpClt.SetRequestBodyAsString(lvStr, True);
  pvHttpClt.Post(pvUrl);
  lvStr := pvHttpClt.GetResponseBodyAsString;
  JSONParser(lvStr, pvResp);
end;

function PostHttpWithJSON(pvHttpClt:TDiocpHttpClient; pvUrl:String;
    pvReq:TDValue): string; overload;
var
  lvStr:String;
begin
  lvStr := JSONEncode(pvReq, False, False);
  pvHttpClt.RequestContentType := 'applicaiton/json';
  pvHttpClt.SetRequestBodyAsString(lvStr, True);
  pvHttpClt.Post(pvUrl);
  lvStr := pvHttpClt.GetResponseBodyAsString;
  Result := lvStr;
end;

end.
