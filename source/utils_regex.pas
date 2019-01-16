unit utils_regex;

interface

uses
  PerlRegEx, SysUtils, utils_strings;

type
  TRegExHelper = class(TObject)
  private
    FRegEx: TPerlRegEx;
    FRegExPattern:String;
    FDataString:string;
    FMatchedString:string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetString(const pvRegExPattern, pvDataString:string);
    function ExecMatch: Boolean;

    function GetMatchedText: String;

  end;

function GetArea(const S, pvLeft, pvRight: string; const pvIndex: Integer = 1):
    string;
function IsMatch(const pvRegExPattern, pvDataString:string): Boolean;


implementation

function GetArea(const S, pvLeft, pvRight: string; const pvIndex: Integer = 1):
    string;
var
  lvCurr: Integer;
  FRegEx: TPerlRegEx;
begin
  Result := ''; 
  FRegEx := TPerlRegEx.Create;
  try
    //(?<=\[#SECTION)([\w\W]*?)(?=\[#\/SECTION#\])
    FRegEx.Subject := S;
    FRegEx.RegEx := '(?<=' + FRegEx.EscapeRegExChars(pvLeft) + ')'
      + '([\w\W]*?)' + '(?=' + FRegEx.EscapeRegExChars(pvRight) + ')';

    //FRegEx.RegEx := '(?<=\[#SECTION)([\w\W]*?)(?=\[#/SECTION#\])';

    lvCurr := 0;
    while FRegEx.MatchAgain do
    begin
      Inc(lvCurr);
      if lvCurr = pvIndex then
      begin
        Result := FRegEx.MatchedText;
      end;
    end;
  finally
    FRegEx.Free;
  end;
end;

function IsMatch(const pvRegExPattern, pvDataString:string): Boolean;
var
  lvRegEx:TRegExHelper;
begin
  lvRegEx := TRegExHelper.Create;
  try
    lvRegEx.SetString(pvRegExPattern, pvDataString);
    Result := lvRegEx.ExecMatch;
  finally
    lvRegEx.Free;
  end;
end;

constructor TRegExHelper.Create;
begin
  inherited Create;
  FRegEx := TPerlRegEx.Create();
end;

destructor TRegExHelper.Destroy;
begin
  FreeAndNil(FRegEx);
  inherited Destroy;
end;

function TRegExHelper.ExecMatch: Boolean;
begin
  FMatchedString := STRING_EMPTY;
  Result := FRegEx.MatchAgain;
end;

function TRegExHelper.GetMatchedText: String;
begin
  if Length(FMatchedString) = 0 then
  begin
    FMatchedString := FRegEx.MatchedText;
  end;
  Result := FMatchedString;
end;

procedure TRegExHelper.SetString(const pvRegExPattern, pvDataString: string);
begin
  FRegExPattern := pvRegExPattern;
  FDataString := pvDataString;
  FRegEx.RegEx := pvRegExPattern;
  FRegEx.Subject := FDataString;
end;

end.
