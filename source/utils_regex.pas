unit utils_regex;

interface

uses
  PerlRegEx;

function GetArea(const S, pvLeft, pvRight: string; const pvIndex: Integer = 1):
    string;

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

end.
