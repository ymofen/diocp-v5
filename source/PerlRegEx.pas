{**************************************************************************************************}
{                                                                                                  }
{ Perl Regular Expressions VCL component                                                           }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is PerlRegEx.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jan Goyvaerts.                                     }
{ Portions created by Jan Goyvaerts are Copyright (C) 1999, 2005, 2008, 2010  Jan Goyvaerts.       }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Design & implementation, by Jan Goyvaerts, 1999, 2005, 2008, 2010                                }
{                                                                                                  }
{ TPerlRegEx is available at http://www.regular-expressions.info/delphi.html                       }
{                                                                                                  }
{**************************************************************************************************}

unit PerlRegEx;

interface

uses
  Windows, Messages, SysUtils, Classes,
  pcre;
{$HPPEMIT '#pragma comment(lib,"msvcrt.lib")'}
type
  TPerlRegExOptions = set of (
    preCaseLess,       // /i -> Case insensitive
    preMultiLine,      // /m -> ^ and $ also match before/after a newline, not just at the beginning and the end of the string
    preSingleLine,     // /s -> Dot matches any character, including \n (newline). Otherwise, it matches anything except \n
    preExtended,       // /x -> Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out
    preAnchored,       // /A -> Successful match can only occur at the start of the subject or right after the previous match
    preUnGreedy,       // Repeat operators (+, *, ?) are not greedy by default (i.e. they try to match the minimum number of characters instead of the maximum)
    preNoAutoCapture   // (group) is a non-capturing group; only named groups capture
  );

type
  TPerlRegExState = set of (
    preNotBOL,         // Not Beginning Of Line: ^ does not match at the start of Subject
    preNotEOL,         // Not End Of Line: $ does not match at the end of Subject
    preNotEmpty        // Empty matches not allowed
  );

const
  // Maximum number of subexpressions (backreferences)
  // Subexpressions are created by placing round brackets in the regex, and are referenced by \1, \2, ...
  // In Perl, they are available as $1, $2, ... after the regex matched; with TPerlRegEx, use the Subexpressions property
  // You can also insert \1, \2, ... in the replacement string; \0 is the complete matched expression
  MAX_SUBEXPRESSIONS = 99;

{$IFDEF UNICODE}
// All implicit string casts have been verified to be correct
{$WARN IMPLICIT_STRING_CAST OFF}
// Use UTF-8 in Delphi 2009 and later, so Unicode strings are handled correctly.
// PCRE does not support UTF-16
type
  PCREString = UTF8String;
{$ELSE UNICODE}
// Use AnsiString in Delphi 2007 and earlier
type
  PCREString = AnsiString;
{$ENDIF UNICODE}

type
  TPerlRegExReplaceEvent = procedure(Sender: TObject; var ReplaceWith: PCREString) of object;

type
  TPerlRegEx = class
  private    // *** Property storage, getters and setters
    FCompiled, FStudied: Boolean;
    FOptions: TPerlRegExOptions;
    FState: TPerlRegExState;
    FRegEx, FReplacement, FSubject: PCREString;
    FStart, FStop: Integer;
    FOnMatch: TNotifyEvent;
    FOnReplace: TPerlRegExReplaceEvent;
    function GetMatchedText: PCREString;
    function GetMatchedLength: Integer;
    function GetMatchedOffset: Integer;
    procedure SetOptions(Value: TPerlRegExOptions);
    procedure SetRegEx(const Value: PCREString);
    function GetGroupCount: Integer;
    function GetGroups(Index: Integer): PCREString;
    function GetGroupLengths(Index: Integer): Integer;
    function GetGroupOffsets(Index: Integer): Integer;
    procedure SetSubject(const Value: PCREString);
    procedure SetStart(const Value: Integer);
    procedure SetStop(const Value: Integer);
    function GetFoundMatch: Boolean;
  private    // *** Variables used by PCRE
    Offsets: array[0..(MAX_SUBEXPRESSIONS+1)*3] of Integer;
    OffsetCount: Integer;
    pcreOptions: Integer;
    pattern, hints, chartable: Pointer;
    FSubjectPChar: PAnsiChar;
    FHasStoredGroups: Boolean;
    FStoredGroups: array of PCREString;
    function GetSubjectLeft: PCREString;
    function GetSubjectRight: PCREString;
  protected
    procedure CleanUp;
        // Dispose off whatever we created, so we can start over. Called automatically when needed, so it is not made public
    procedure ClearStoredGroups;
  public
    constructor Create;
        // Come to life
    destructor Destroy; override;
        // Clean up after ourselves
    class function EscapeRegExChars(const S: string): string;
        // Escapes regex characters in S so that the regex engine can be used to match S as plain text
    procedure Compile;
        // Compile the regex. Called automatically by Match
    procedure Study;
        // Study the regex. Studying takes time, but will make the execution of the regex a lot faster.
        // Call study if you will be using the same regex many times
    function Match: Boolean;
        // Attempt to match the regex, starting the attempt from the beginning of Subject
    function MatchAgain: Boolean;
        // Attempt to match the regex to the remainder of Subject after the previous match (as indicated by Start)
    function Replace: PCREString;
        // Replace matched expression in Subject with ComputeReplacement.  Returns the actual replacement text from ComputeReplacement
    function ReplaceAll: Boolean;
        // Repeat MatchAgain and Replace until you drop.  Returns True if anything was replaced at all.
    function ComputeReplacement: PCREString;
        // Returns Replacement with backreferences filled in
    procedure StoreGroups;
        // Stores duplicates of Groups[] so they and ComputeReplacement will still return the proper strings
        // even if FSubject is changed or cleared
    function NamedGroup(const Name: PCREString): Integer;
        // Returns the index of the named group Name
    procedure Split(Strings: TStrings; Limit: Integer);
        // Split Subject along regex matches.  Capturing groups are ignored.
    procedure SplitCapture(Strings: TStrings; Limit: Integer); overload;
    procedure SplitCapture(Strings: TStrings; Limit: Integer; Offset: Integer); overload;
        // Split Subject along regex matches.  Capturing groups are added to Strings as well.
    property Compiled: Boolean read FCompiled;
        // True if the RegEx has already been compiled.
    property FoundMatch: Boolean read GetFoundMatch;
        // Returns True when Matched* and Group* indicate a match
    property Studied: Boolean read FStudied;
        // True if the RegEx has already been studied
    property MatchedText: PCREString read GetMatchedText;
        // The matched text
    property MatchedLength: Integer read GetMatchedLength;
        // Length of the matched text
    property MatchedOffset: Integer read GetMatchedOffset;
        // Character offset in the Subject string at which MatchedText starts
    property Start: Integer read FStart write SetStart;
        // Starting position in Subject from which MatchAgain begins
    property Stop: Integer read FStop write SetStop;
        // Last character in Subject that Match and MatchAgain search through
    property State: TPerlRegExState read FState write FState;
        // State of Subject
    property GroupCount: Integer read GetGroupCount;
        // Number of matched capturing groups
    property Groups[Index: Integer]: PCREString read GetGroups;
        // Text matched by capturing groups
    property GroupLengths[Index: Integer]: Integer read GetGroupLengths;
        // Lengths of the text matched by capturing groups
    property GroupOffsets[Index: Integer]: Integer read GetGroupOffsets;
        // Character offsets in Subject at which the capturing group matches start
    property Subject: PCREString read FSubject write SetSubject;
        // The string on which Match() will try to match RegEx
    property SubjectLeft: PCREString read GetSubjectLeft;
        // Part of the subject to the left of the match
    property SubjectRight: PCREString read GetSubjectRight;
        // Part of the subject to the right of the match
  public
    property Options: TPerlRegExOptions read FOptions write SetOptions;
        // Options
    property RegEx: PCREString read FRegEx write SetRegEx;
        // The regular expression to be matched
    property Replacement: PCREString read FReplacement write FReplacement;
        // Text to replace matched expression with. \number and $number backreferences will be substituted with Groups
        // TPerlRegEx supports the "JGsoft" replacement text flavor as explained at http://www.regular-expressions.info/refreplace.html
    property OnMatch: TNotifyEvent read FOnMatch write FOnMatch;
        // Triggered by Match and MatchAgain after a successful match
    property OnReplace: TPerlRegExReplaceEvent read FOnReplace write FOnReplace;
        // Triggered by Replace and ReplaceAll just before the replacement is done, allowing you to determine the new PCREString
  end;

{
  You can add TPerlRegEx instances to a TPerlRegExList to match them all together on the same subject,
  as if they were one regex regex1|regex2|regex3|...
  TPerlRegExList does not own the TPerlRegEx components, just like a TList
  If a TPerlRegEx has been added to a TPerlRegExList, it should not be used in any other situation
  until it is removed from the list
}

type
  TPerlRegExList = class
  private
    FList: TList;
    FSubject: PCREString;
    FMatchedRegEx: TPerlRegEx;
    FStart, FStop: Integer;
    function GetRegEx(Index: Integer): TPerlRegEx;
    procedure SetRegEx(Index: Integer; Value: TPerlRegEx);
    procedure SetSubject(const Value: PCREString);
    procedure SetStart(const Value: Integer);
    procedure SetStop(const Value: Integer);
    function GetCount: Integer;
  protected
    procedure UpdateRegEx(ARegEx: TPerlRegEx);
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(ARegEx: TPerlRegEx): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function IndexOf(ARegEx: TPerlRegEx): Integer;
    procedure Insert(Index: Integer; ARegEx: TPerlRegEx);
  public
    function Match: Boolean;
    function MatchAgain: Boolean;
    property RegEx[Index: Integer]: TPerlRegEx read GetRegEx write SetRegEx;
    property Count: Integer read GetCount;
    property Subject: PCREString read FSubject write SetSubject;
    property Start: Integer read FStart write SetStart;
    property Stop: Integer read FStop write SetStop;
    property MatchedRegEx: TPerlRegEx read FMatchedRegEx;
  end;

implementation


         { ********* Unit support routines ********* }

function FirstCap(const S: string): string;
begin
  if S = '' then Result := ''
  else begin
    Result := AnsiLowerCase(S);
  {$IFDEF UNICODE}
    CharUpperBuffW(@Result[1], 1);
  {$ELSE}
    CharUpperBuffA(@Result[1], 1);
  {$ENDIF}
  end
end;

function InitialCaps(const S: string): string;
var
  I: Integer;
  Up: Boolean;
begin
  Result := AnsiLowerCase(S);
  Up := True;
{$IFDEF UNICODE}
  for I := 1 to Length(Result) do begin
    case Result[I] of
      #0..'&', '(', '*', '+', ',', '-', '.', '?', '<', '[', '{', #$00B7:
        Up := True
      else
        if Up and (Result[I] <> '''') then begin
          CharUpperBuffW(@Result[I], 1);
          Up := False
        end
    end;
  end;
{$ELSE UNICODE}
  if SysLocale.FarEast then begin
    I := 1;
    while I <= Length(Result) do begin
      if Result[I] in LeadBytes then begin
        Inc(I, 2)
      end
      else begin
        if Result[I] in [#0..'&', '('..'.', '?', '<', '[', '{'] then Up := True
        else if Up and (Result[I] <> '''') then begin
          CharUpperBuffA(@Result[I], 1);
          Result[I] := UpperCase(Result[I])[1];
          Up := False
        end;
        Inc(I)
      end
    end
  end
  else
    for I := 1 to Length(Result) do begin
      if Result[I] in [#0..'&', '('..'.', '?', '<', '[', '{', #$B7] then Up := True
      else if Up and (Result[I] <> '''') then begin
        CharUpperBuffA(@Result[I], 1);
        Result[I] := AnsiUpperCase(Result[I])[1];
        Up := False
      end
    end;
{$ENDIF UNICODE}
end;


         { ********* TPerlRegEx component ********* }

procedure TPerlRegEx.CleanUp;
begin
  FCompiled := False; FStudied := False;
  pcre_dispose(pattern, hints, nil);
  pattern := nil;
  hints := nil;
  ClearStoredGroups;
  OffsetCount := 0;
end;

procedure TPerlRegEx.ClearStoredGroups;
begin
  FHasStoredGroups := False;
  FStoredGroups := nil;
end;

procedure TPerlRegEx.Compile;
var
  Error: PAnsiChar;
  ErrorOffset: Integer;
begin
  if FRegEx = '' then
    raise Exception.Create('TPerlRegEx.Compile() - Please specify a regular expression in RegEx first');
  CleanUp;
  Pattern := pcre_compile(PAnsiChar(FRegEx), pcreOptions, @Error, @ErrorOffset, chartable);
  if Pattern = nil then
    raise Exception.Create(Format('TPerlRegEx.Compile() - Error in regex at offset %d: %s', [ErrorOffset, AnsiString(Error)]));
  FCompiled := True
end;

(* Backreference overview:

Assume there are 13 backreferences:

Text        TPerlRegex    .NET      Java       ECMAScript
$17         $1 + "7"      "$17"     $1 + "7"   $1 + "7"
$017        $1 + "7"      "$017"    $1 + "7"   $1 + "7"
$12         $12           $12       $12        $12
$012        $1 + "2"      $12       $12        $1 + "2"
${1}2       $1 + "2"      $1 + "2"  error      "${1}2"
$$          "$"           "$"       error      "$"
\$          "$"           "\$"      "$"        "\$"
*)

function TPerlRegEx.ComputeReplacement: PCREString;
var
  Mode: AnsiChar;
  S: PCREString;
  I, J, N: Integer;

  procedure ReplaceBackreference(Number: Integer);
  var
    Backreference: PCREString;
  begin
    Delete(S, I, J-I);
    if Number <= GroupCount then begin
      Backreference := Groups[Number];
      if Backreference <> '' then begin
        // Ignore warnings; converting to UTF-8 does not cause data loss
        case Mode of
          'L', 'l': Backreference := AnsiLowerCase(Backreference);
          'U', 'u': Backreference := AnsiUpperCase(Backreference);
          'F', 'f': Backreference := FirstCap(Backreference);
          'I', 'i': Backreference := InitialCaps(Backreference);
        end;
        if S <> '' then begin
          Insert(Backreference, S, I);
          I := I + Length(Backreference);
        end
        else begin
          S := Backreference;
          I := MaxInt;
        end
      end;
    end
  end;

  procedure ProcessBackreference(NumberOnly, Dollar: Boolean);
  var
    Number, Number2: Integer;
    Group: PCREString;
  begin
    Number := -1;
    if (J <= Length(S)) and (S[J] in ['0'..'9']) then begin
      // Get the number of the backreference
      Number := Ord(S[J]) - Ord('0');
      Inc(J);
      if (J <= Length(S)) and (S[J] in ['0'..'9']) then begin
        // Expand it to two digits only if that would lead to a valid backreference
        Number2 := Number*10 + Ord(S[J]) - Ord('0');
        if Number2 <= GroupCount then begin
          Number := Number2;
          Inc(J)
        end;
      end;
    end
    else if not NumberOnly then begin
      if Dollar and (J < Length(S)) and (S[J] = '{') then begin
        // Number or name in curly braces
        Inc(J);
        case S[J] of
          '0'..'9': begin
            Number := Ord(S[J]) - Ord('0');
            Inc(J);
            while (J <= Length(S)) and (S[J] in ['0'..'9']) do begin
              Number := Number*10 + Ord(S[J]) - Ord('0');
              Inc(J)
            end;
          end;
          'A'..'Z', 'a'..'z', '_': begin
            Inc(J);
            while (J <= Length(S)) and (S[J] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do Inc(J);
            if (J <= Length(S)) and (S[J] = '}') then begin
              Group := Copy(S, I+2, J-I-2);
              Number := NamedGroup(Group);
            end
          end;
        end;
        if (J > Length(S)) or (S[J] <> '}') then Number := -1
          else Inc(J)
      end
      else if Dollar and (S[J] = '_') then begin
        // $_ (whole subject)
        Delete(S, I, J+1-I);
        Insert(Subject, S, I);
        I := I + Length(Subject);
        Exit;
      end
      else case S[J] of
        '&': begin
          // \& or $& (whole regex match)
          Number := 0;
          Inc(J);
        end;
        '+': begin
          // \+ or $+ (highest-numbered participating group)
          Number := GroupCount;
          Inc(J);
        end;
        '`': begin
          // \` or $` (backtick; subject to the left of the match)
          Delete(S, I, J+1-I);
          Insert(SubjectLeft, S, I);
          I := I + Offsets[0] - 1;
          Exit;
        end;
        '''': begin
          // \' or $' (straight quote; subject to the right of the match)
          Delete(S, I, J+1-I);
          Insert(SubjectRight, S, I);
          I := I + Length(Subject) - Offsets[1];
          Exit;
        end
      end;
    end;
    if Number >= 0 then ReplaceBackreference(Number)
      else Inc(I)
  end;

begin
  S := FReplacement;
  I := 1;
  while I < Length(S) do begin
    case S[I] of
      '\': begin
        J := I + 1;
        Assert(J <= Length(S), 'CHECK: We let I stop one character before the end, so J cannot point beyond the end of the PCREString here');
        case S[J] of
          '$', '\': begin
            Delete(S, I, 1);
            Inc(I);
          end;
          'g': begin
            if (J < Length(S)-1) and (S[J+1] = '<') and (S[J+2] in ['A'..'Z', 'a'..'z', '_']) then begin
              // Python-style named group reference \g<name>
              J := J+3;
              while (J <= Length(S)) and (S[J] in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) do Inc(J);
              if (J <= Length(S)) and (S[J] = '>') then begin
                N := NamedGroup(Copy(S, I+3, J-I-3));
                Inc(J);
                Mode := #0;
                if N > 0 then ReplaceBackreference(N)
                  else Delete(S, I, J-I)
              end
              else I := J
            end
            else I := I+2;
          end;
          'l', 'L', 'u', 'U', 'f', 'F', 'i', 'I': begin
            Mode := S[J];
            Inc(J);
            ProcessBackreference(True, False);
          end;
          else begin
            Mode := #0;
            ProcessBackreference(False, False);
          end;
        end;
      end;
      '$': begin
        J := I + 1;
        Assert(J <= Length(S), 'CHECK: We let I stop one character before the end, so J cannot point beyond the end of the PCREString here');
        if S[J] = '$' then begin
          Delete(S, J, 1);
          Inc(I);
        end
        else begin
          Mode := #0;
          ProcessBackreference(False, True);
        end
      end;
      else Inc(I)
    end
  end;
  Result := S
end;

constructor TPerlRegEx.Create;
begin
  inherited Create;
  FState := [preNotEmpty];
  chartable := pcre_maketables;
{$IFDEF UNICODE}
  pcreOptions := PCRE_UTF8 or PCRE_NEWLINE_ANY;
{$ELSE}
  pcreOptions := PCRE_NEWLINE_ANY;
{$ENDIF}
end;

destructor TPerlRegEx.Destroy;
begin
  pcre_dispose(pattern, hints, chartable);
  inherited Destroy;
end;

class function TPerlRegEx.EscapeRegExChars(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  I := Length(Result);
  while I > 0 do begin
    case Result[I] of
      '.', '[', ']', '(', ')', '?', '*', '+', '{', '}', '^', '$', '|', '\':
        Insert('\', Result, I);
      #0: begin
        Result[I] := '0';
        Insert('\', Result, I);
      end;
    end;
    Dec(I);
  end;
end;

function TPerlRegEx.GetFoundMatch: Boolean;
begin
  Result := OffsetCount > 0;
end;

function TPerlRegEx.GetMatchedText: PCREString;
begin
  Assert(FoundMatch, 'REQUIRE: There must be a successful match first');
  Result := GetGroups(0);
end;

function TPerlRegEx.GetMatchedLength: Integer;
begin
  Assert(FoundMatch, 'REQUIRE: There must be a successful match first');
  Result := GetGroupLengths(0)
end;

function TPerlRegEx.GetMatchedOffset: Integer;
begin
  Assert(FoundMatch, 'REQUIRE: There must be a successful match first');
  Result := GetGroupOffsets(0)
end;

function TPerlRegEx.GetGroupCount: Integer;
begin
  Assert(FoundMatch, 'REQUIRE: There must be a successful match first');
  Result := OffsetCount-1
end;

function TPerlRegEx.GetGroupLengths(Index: Integer): Integer;
begin
  Assert(FoundMatch, 'REQUIRE: There must be a successful match first');
  Assert((Index >= 0) and (Index <= GroupCount), 'REQUIRE: Index <= GroupCount');
  Result := Offsets[Index*2+1]-Offsets[Index*2]
end;

function TPerlRegEx.GetGroupOffsets(Index: Integer): Integer;
begin
  Assert(FoundMatch, 'REQUIRE: There must be a successful match first');
  Assert((Index >= 0) and (Index <= GroupCount), 'REQUIRE: Index <= GroupCount');
  Result := Offsets[Index*2]
end;

function TPerlRegEx.GetGroups(Index: Integer): PCREString;
begin
  Assert(FoundMatch, 'REQUIRE: There must be a successful match first');
  if Index > GroupCount then Result := ''
    else if FHasStoredGroups then Result := FStoredGroups[Index]
    else Result := Copy(FSubject, Offsets[Index*2], Offsets[Index*2+1]-Offsets[Index*2]);
end;

function TPerlRegEx.GetSubjectLeft: PCREString;
begin
  Result := Copy(Subject, 1, Offsets[0]-1);
end;

function TPerlRegEx.GetSubjectRight: PCREString;
begin
  Result := Copy(Subject, Offsets[1], MaxInt);
end;

function TPerlRegEx.Match: Boolean;
var
  I, Opts: Integer;
begin
  ClearStoredGroups;
  if not Compiled then Compile;
  if preNotBOL in State then Opts := PCRE_NOTBOL else Opts := 0;
  if preNotEOL in State then Opts := Opts or PCRE_NOTEOL;
  if preNotEmpty in State then Opts := Opts or PCRE_NOTEMPTY;
  OffsetCount := pcre_exec(Pattern, Hints, FSubjectPChar, FStop, 0, Opts, @Offsets[0], High(Offsets));
  Result := OffsetCount > 0;
  // Convert offsets into PCREString indices
  if Result then begin
    for I := 0 to OffsetCount*2-1 do
      Inc(Offsets[I]);
    FStart := Offsets[1];
    if Offsets[0] = Offsets[1] then Inc(FStart); // Make sure we don't get stuck at the same position
    if Assigned(OnMatch) then OnMatch(Self)
  end;
end;

function TPerlRegEx.MatchAgain: Boolean;
var
  I, Opts: Integer;
begin
  ClearStoredGroups;
  if not Compiled then Compile;
  if preNotBOL in State then Opts := PCRE_NOTBOL else Opts := 0;
  if preNotEOL in State then Opts := Opts or PCRE_NOTEOL;
  if preNotEmpty in State then Opts := Opts or PCRE_NOTEMPTY;
  if FStart-1 > FStop then OffsetCount := -1
    else OffsetCount := pcre_exec(Pattern, Hints, FSubjectPChar, FStop, FStart-1, Opts, @Offsets[0], High(Offsets));
  Result := OffsetCount > 0;
  // Convert offsets into PCREString indices
  if Result then begin
    for I := 0 to OffsetCount*2-1 do
      Inc(Offsets[I]);
    FStart := Offsets[1];
    if Offsets[0] = Offsets[1] then Inc(FStart); // Make sure we don't get stuck at the same position
    if Assigned(OnMatch) then OnMatch(Self)
  end;
end;

function TPerlRegEx.NamedGroup(const Name: PCREString): Integer;
begin
  Result := pcre_get_stringnumber(Pattern, PAnsiChar(Name));
end;

function TPerlRegEx.Replace: PCREString;
begin
  Assert(FoundMatch, 'REQUIRE: There must be a successful match first');
  // Substitute backreferences
  Result := ComputeReplacement;
  // Allow for just-in-time substitution determination
  if Assigned(OnReplace) then OnReplace(Self, Result);
  // Perform substitution
  Delete(FSubject, MatchedOffset, MatchedLength);
  if Result <> '' then Insert(Result, FSubject, MatchedOffset);
  FSubjectPChar := PAnsiChar(FSubject);
  // Position to continue search
  FStart := FStart - MatchedLength + Length(Result);
  FStop := FStop - MatchedLength + Length(Result);
  // Replacement no longer matches regex, we assume
  ClearStoredGroups;
  OffsetCount := 0;
end;

function TPerlRegEx.ReplaceAll: Boolean;
begin
  if Match then begin
    Result := True;
    repeat
      Replace
    until not MatchAgain;
  end
  else Result := False;
end;

procedure TPerlRegEx.SetOptions(Value: TPerlRegExOptions);
begin
  if (FOptions <> Value) then begin
    FOptions := Value;
  {$IFDEF UNICODE}
    pcreOptions := PCRE_UTF8 or PCRE_NEWLINE_ANY;
  {$ELSE}
    pcreOptions := PCRE_NEWLINE_ANY;
  {$ENDIF}
    if (preCaseLess in Value) then pcreOptions := pcreOptions or PCRE_CASELESS;
    if (preMultiLine in Value) then pcreOptions := pcreOptions or PCRE_MULTILINE;
    if (preSingleLine in Value) then pcreOptions := pcreOptions or PCRE_DOTALL;
    if (preExtended in Value) then pcreOptions := pcreOptions or PCRE_EXTENDED;
    if (preAnchored in Value) then pcreOptions := pcreOptions or PCRE_ANCHORED;
    if (preUnGreedy in Value) then pcreOptions := pcreOptions or PCRE_UNGREEDY;
    if (preNoAutoCapture in Value) then pcreOptions := pcreOptions or PCRE_NO_AUTO_CAPTURE;
    CleanUp
  end
end;

procedure TPerlRegEx.SetRegEx(const Value: PCREString);
begin
  if FRegEx <> Value then begin
    FRegEx := Value;
    CleanUp
  end
end;

procedure TPerlRegEx.SetStart(const Value: Integer);
begin
  if Value < 1 then FStart := 1
  else FStart := Value;
  // If FStart > Length(Subject), MatchAgain() will simply return False
end;

procedure TPerlRegEx.SetStop(const Value: Integer);
begin
  if Value > Length(Subject) then FStop := Length(Subject)
    else FStop := Value;
end;

procedure TPerlRegEx.SetSubject(const Value: PCREString);
begin
  FSubject := Value;
  FSubjectPChar := PAnsiChar(Value);
  FStart := 1;
  FStop := Length(Subject);
  if not FHasStoredGroups then OffsetCount := 0;
end;

procedure TPerlRegEx.Split(Strings: TStrings; Limit: Integer);
var
  Offset, Count: Integer;
begin
  Assert(Strings <> nil, 'REQUIRE: Strings');
  if (Limit = 1) or not Match then Strings.Add(Subject)
  else begin
    Offset := 1;
    Count := 1;
    repeat
      Strings.Add(Copy(Subject, Offset, MatchedOffset - Offset));
      Inc(Count);
      Offset := MatchedOffset + MatchedLength;
    until ((Limit > 1) and (Count >= Limit)) or not MatchAgain;
    Strings.Add(Copy(Subject, Offset, MaxInt));
  end
end;

procedure TPerlRegEx.SplitCapture(Strings: TStrings; Limit, Offset: Integer);
var
  Count: Integer;
  bUseOffset : boolean;
  iOffset : integer;
begin
  Assert(Strings <> nil, 'REQUIRE: Strings');
  if (Limit = 1) or not Match then Strings.Add(Subject)
  else
  begin
    bUseOffset := Offset <> 1;
    if Offset <> 1 then
      Dec(Limit);
    iOffset := 1;
    Count := 1;
    repeat
      if bUseOffset then
      begin
        if MatchedOffset >= Offset then
        begin
          bUseOffset := False;
          Strings.Add(Copy(Subject, 1, MatchedOffset -1));
          if Self.GroupCount > 0 then
            Strings.Add(Self.Groups[Self.GroupCount]);
        end;
      end
      else
      begin
        Strings.Add(Copy(Subject, iOffset, MatchedOffset - iOffset));
        Inc(Count);
        if Self.GroupCount > 0 then
          Strings.Add(Self.Groups[Self.GroupCount]);
      end;
      iOffset := MatchedOffset + MatchedLength;
    until ((Limit > 1) and (Count >= Limit)) or not MatchAgain;
    Strings.Add(Copy(Subject, iOffset, MaxInt));
  end
end;

procedure TPerlRegEx.SplitCapture(Strings: TStrings; Limit: Integer);
begin
  SplitCapture(Strings,Limit,1);
end;

procedure TPerlRegEx.StoreGroups;
var
  I: Integer;
begin
  if OffsetCount > 0 then begin
    ClearStoredGroups;
    SetLength(FStoredGroups, GroupCount+1);
    for I := GroupCount downto 0 do
      FStoredGroups[I] := Groups[I];
    FHasStoredGroups := True;
  end
end;

procedure TPerlRegEx.Study;
var
  Error: PAnsiChar;
begin
  if not FCompiled then Compile;
  Hints := pcre_study(Pattern, 0, @Error);
  if Error <> nil then
    raise Exception.Create('TPerlRegEx.Study() - Error studying the regex: ' + AnsiString(Error));
  FStudied := True
end;

{ TPerlRegExList }

function TPerlRegExList.Add(ARegEx: TPerlRegEx): Integer;
begin
  Result := FList.Add(ARegEx);
  UpdateRegEx(ARegEx);
end;

procedure TPerlRegExList.Clear;
begin
  FList.Clear;
end;

constructor TPerlRegExList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure TPerlRegExList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TPerlRegExList.Destroy;
begin
  FList.Free;
  inherited
end;

function TPerlRegExList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TPerlRegExList.GetRegEx(Index: Integer): TPerlRegEx;
begin
  Result := TPerlRegEx(Pointer(FList[Index]));
end;

function TPerlRegExList.IndexOf(ARegEx: TPerlRegEx): Integer;
begin
  Result := FList.IndexOf(ARegEx);
end;

procedure TPerlRegExList.Insert(Index: Integer; ARegEx: TPerlRegEx);
begin
  FList.Insert(Index, ARegEx);
  UpdateRegEx(ARegEx);
end;

function TPerlRegExList.Match: Boolean;
begin
  SetStart(1);
  FMatchedRegEx := nil;
  Result := MatchAgain;
end;

function TPerlRegExList.MatchAgain: Boolean;
var
  I, MatchStart, MatchPos: Integer;
  ARegEx: TPerlRegEx;
begin
  if FMatchedRegEx <> nil then
    MatchStart := FMatchedRegEx.MatchedOffset + FMatchedRegEx.MatchedLength
  else
    MatchStart := FStart;
  FMatchedRegEx := nil;
  MatchPos := MaxInt;
  for I := 0 to Count-1 do begin
    ARegEx := RegEx[I];
    if (not ARegEx.FoundMatch) or (ARegEx.MatchedOffset < MatchStart) then begin
      ARegEx.Start := MatchStart;
      ARegEx.MatchAgain;
    end;
    if ARegEx.FoundMatch and (ARegEx.MatchedOffset < MatchPos) then begin
      MatchPos := ARegEx.MatchedOffset;
      FMatchedRegEx := ARegEx;
    end;
    if MatchPos = MatchStart then Break;
  end;
  Result := MatchPos < MaxInt;
end;

procedure TPerlRegExList.SetRegEx(Index: Integer; Value: TPerlRegEx);
begin
  FList[Index] := Value;
  UpdateRegEx(Value);
end;

procedure TPerlRegExList.SetStart(const Value: Integer);
var
  I: Integer;
begin
  if FStart <> Value then begin
    FStart := Value;
    for I := Count-1 downto 0 do
      RegEx[I].Start := Value;
    FMatchedRegEx := nil;
  end;
end;

procedure TPerlRegExList.SetStop(const Value: Integer);
var
  I: Integer;
begin
  if FStop <> Value then begin
    FStop := Value;
    for I := Count-1 downto 0 do
      RegEx[I].Stop := Value;
    FMatchedRegEx := nil;
  end;
end;

procedure TPerlRegExList.SetSubject(const Value: PCREString);
var
  I: Integer;
begin
  if FSubject <> Value then begin
    FSubject := Value;
    for I := Count-1 downto 0 do
      RegEx[I].Subject := Value;
    FMatchedRegEx := nil;
  end;
end;

procedure TPerlRegExList.UpdateRegEx(ARegEx: TPerlRegEx);
begin
  ARegEx.Subject := FSubject;
  ARegEx.Start := FStart;
end;

end.
