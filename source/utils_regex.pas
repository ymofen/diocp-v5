unit utils_regex;

interface

uses
  SysUtils, utils_strings
{$IF (RTLVersion>=26)}
  , System.RegularExpressions
{$ELSE}
  , PerlRegEx
{$ENDIF}
  ;


type

  TRegExHelper = class(TObject)
  private
    {$IF (RTLVersion>=26)}
    FRegEx : TRegEx;
    FMatch:TMatch;
    FMatchFlag:Integer;
    {$ELSE}
    FRegEx: TPerlRegEx;
    {$ENDIF}

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
function EscapeRegExPattern(const str :string): string;


implementation

function GetArea(const S, pvLeft, pvRight: string; const pvIndex: Integer = 1):
    string;
var
  lvCurr: Integer;
  {$IF (RTLVersion>=26)}
  FRegEx : TRegEx;
  lvPattern:string;
  lvMatch:TMatch;
  {$ELSE}
  FRegEx: TPerlRegEx;
  {$ENDIF}

begin
  Result := '';

  {$IF (RTLVersion>=26)}
  lvPattern := '(?<=' + TRegEx.Escape(pvLeft) + ')'
      + '([\w\W]*?)' + '(?=' + FRegEx.Escape(pvRight) + ')';
  lvCurr := 0;
  for lvMatch in TRegEx.Matches(s, lvPattern) do
  begin
    Inc(lvCurr);
    if lvCurr = pvIndex then
    begin
      Result := lvMatch.Value;
    end;
  end;
  {$ELSE}
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
  {$ENDIF}
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

function EscapeRegExPattern(const str :string): string;
begin
  {$IF (RTLVersion>=26)}
  Result := TRegEx.Escape(str);
  {$ELSE}
  Result := TPerlRegEx.EscapeRegExPattern(str);
  {$ENDIF}
end;

constructor TRegExHelper.Create;
begin
  inherited Create;
  {$IF (RTLVersion>=26)}
  {$ELSE}
  FRegEx := TPerlRegEx.Create();
  {$ENDIF}
end;

destructor TRegExHelper.Destroy;
begin
  {$IF (RTLVersion>=26)}
  {$ELSE}
  FreeAndNil(FRegEx);
  {$ENDIF}
  inherited Destroy;
end;

function TRegExHelper.ExecMatch: Boolean;
begin
  FMatchedString := STRING_EMPTY;
  {$IF (RTLVersion>=26)}
  if FMatchFlag = 1 then       // Æ¥Åä¹ý
  begin
    FMatch := FMatch.NextMatch;
  end else
  begin
    FMatch := FRegEx.Match(FDataString, FRegExPattern);
    FMatchFlag := 1;
  end;
  Result := FMatch.Success;
  {$ELSE}
  Result := FRegEx.MatchAgain;
  {$ENDIF}
end;

function TRegExHelper.GetMatchedText: String;
begin
  {$IF (RTLVersion>=26)}
  if Length(FMatchedString) = 0 then
  begin
    if FMatchFlag = 1 then
    begin
      FMatchedString := FMatch.Value;
    end;
  end;
  {$ELSE}
  if Length(FMatchedString) = 0 then
  begin
    FMatchedString := FRegEx.MatchedText;
  end;
  {$ENDIF}
  Result := FMatchedString;
end;

procedure TRegExHelper.SetString(const pvRegExPattern, pvDataString: string);
begin
  FRegExPattern := pvRegExPattern;
  FDataString := pvDataString;
  {$IF (RTLVersion>=26)}
  FMatchFlag := 0;
  {$ELSE}
  FRegEx.RegEx := pvRegExPattern;
  FRegEx.Subject := FDataString;
  {$ENDIF}
end;

end.
