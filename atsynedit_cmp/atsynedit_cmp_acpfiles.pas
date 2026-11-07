{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_AcpFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Graphics,
  ATSynEdit;

procedure DoEditorCompletionAcp(AEdit: TATSynEdit;
  const AFilenameAcp: string; ACaseSens: boolean);


implementation

uses
  ATStringProc,
  ATSynEdit_Carets,
  ATSynEdit_Cmp_Form;

procedure SReplaceAllPercentChars(var S: string);
const
  cHexDigits: set of byte = [Ord('0')..Ord('9'), Ord('A')..Ord('F')];
  cEncodableChars: set of byte = [$20..$2F, Ord('|')];
var
  i, j, N, Code: integer;

  //decodes 2 uppercase hex digits at APos, true if decoded char is encodable
  function TryDecodeHexPair(const AStr: string; APos: integer; out ACode: integer): boolean;
  var
    N1, N2: integer;
  begin
    Result:= false;
    N1:= Ord(AStr[APos]);
    N2:= Ord(AStr[APos+1]);
    if (N1 in cHexDigits) and (N2 in cHexDigits) then
    begin
      if N1 <= Ord('9') then
        Dec(N1, Ord('0'))
      else
        Dec(N1, Ord('A') - 10);
      if N2 <= Ord('9') then
        Dec(N2, Ord('0'))
      else
        Dec(N2, Ord('A') - 10);
      ACode:= N1 * 16 + N2;
      Result:= ACode in cEncodableChars;
    end;
  end;

begin
  N:= Length(S);
  if N < 3 then Exit;

  //find 1st encodable '%XX', to skip entire job for common case
  i:= 1;
  while i <= N - 2 do
  begin
    if (S[i] = '%') and TryDecodeHexPair(S, i + 1, Code) then
      Break;
    Inc(i);
  end;
  if i > N - 2 then Exit;

  //replace all encodable pairs, compacting string in place:
  //result is always shorter, so write pos never overtakes read pos
  j:= i;
  while i <= N - 2 do
  begin
    if (S[i] = '%') and TryDecodeHexPair(S, i + 1, Code) then
    begin
      S[j]:= Chr(Code);
      Inc(j);
      Inc(i, 3);
      Continue;
    end;
    S[j]:= S[i];
    Inc(i);
    Inc(j);
  end;
  while i <= N do
  begin
    S[j]:= S[i];
    Inc(i);
    Inc(j);
  end;
  SetLength(S, j - 1);
end;


type
  { TAcp }

  TAcp = class
  private
    ListAcpType: TStringlist;
    ListAcpText: TStringlist;
    ListAcpDesc: TStringlist;
    FLexerWordChars: string;
    procedure DoLoadAcpFile(const fn: string; IsPascal: boolean);
    procedure DoOnGetCompleteProp(Sender: TObject;
      AContent: TStringList;
      out ACharsLeft, ACharsRight: integer);
    function GetActualNonWordChars: UnicodeString;
  public
    Ed: TATSynEdit;
    CaseSens: boolean;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

var
  Acp: TAcp = nil;

//parse control string from .acp file (starts with #)
procedure SParseString_AcpControlLine(const s: string;
  var WordChars: string;
  var IsBracketSep: boolean);
var
  n: Integer;
begin
  if SBeginsWith(s, '#chars') then
  begin
    WordChars:= '';
    IsBracketSep:= true;
    n:= Pos(' ', s);
    if n>0 then
    begin
      WordChars:= Copy(s, n+1, MaxInt);
      IsBracketSep:= Pos('(', WordChars)=0;
    end;
  end;
end;


//parse string from .acp file
procedure SParseString_AcpStd(
  const S: string;
  IsBracketSep: boolean;
  out SType, SId, SPar, SHint: string);
const
  cMaxHintLen = 300;
var
  a, b, c: Integer;
begin
  SType:= '';
  SId:= '';
  SPar:= '';
  SHint:= '';
  if Trim(s)='' then Exit;

  a:= PosEx(' ', s, 1);
  b:= PosEx(' ', s, a+1);
  if b=0 then
    b:= Length(s)+1;

  if IsBracketSep then
  begin
    c:= PosEx('(', s, a+1);
    if (c<b) and (c<>0) then
      b:= c;
  end;

  c:= PosEx('|', s, b);
  if c=0 then
    c:= MaxInt div 2;

  SType:= Copy(s, 1, a-1);
  SId:= Copy(s, a+1, b-a-1);
  SPar:= Copy(s, b, c-b);
  SHint:= Copy(s, c+1, cMaxHintLen);

  SReplaceAllPercentChars(SId);
  SReplaceAllPercentChars(SPar);

  SPar:= StringReplace(SPar, ';', ',', [rfReplaceAll]); //Pascal lexer has ";" param separator
  SPar:= StringReplace(SPar, '[,', ',[', [rfReplaceAll]); //for optional params
end;


procedure TAcp.DoLoadAcpFile(const fn: string; IsPascal: boolean);
var
  List: TStringList;
  s, SType, SText, SPar, SHint: string;
  IsBracketSep: boolean;
  i: Integer;
begin
  ListAcpType.Clear;
  ListAcpText.Clear;
  ListAcpDesc.Clear;

  FLexerWordChars:= '';
  IsBracketSep:= true;

  List:= TStringList.Create;
  try
    List.LoadFromFile(fn);
    for i:= 0 to List.Count-1 do
    begin
      s:= List[i];
      if s='' then
        Continue;

      if s[1]='#' then
      begin
        SParseString_AcpControlLine(s, FLexerWordChars, IsBracketSep);
        Continue;
      end;

      SParseString_AcpStd(s, IsBracketSep, SType, SText, SPar, SHint);
      if SText<>'' then
      begin
        if IsPascal then
        begin
          SDeleteFrom(SText, ':');
          if Pos('):', SPar)>0 then
          begin
            SDeleteFrom(SPar, '):');
            SPar:= SPar+')';
          end;
        end;

        ListAcpType.Add(SType);
        ListAcpText.Add(SText);
        ListAcpDesc.Add(SPar+CompletionOps.HintSep+SHint);
      end;
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure SDeleteOneChar(var S: UnicodeString; ch: WideChar);
var
  N: integer;
begin
  N:= Pos(ch, S);
  if N>0 then
    Delete(S, N, 1);
end;

function TAcp.GetActualNonWordChars: UnicodeString;
var
  i: integer;
begin
  Result:= Ed.OptNonWordChars;
  for i:= 1 to Length(FLexerWordChars) do
    SDeleteOneChar(Result, FLexerWordChars[i]);
end;

procedure TAcp.DoOnGetCompleteProp(Sender: TObject;
  AContent: TStringList; out ACharsLeft, ACharsRight: integer);
var
  Caret: TATCaretItem;
  s_word_w: atString;
  s_type, s_text, s_desc, s_word: string;
  n: integer;
  ok: boolean;
begin
  AContent.Clear;
  ACharsLeft:= 0;
  ACharsRight:= 0;

  Caret:= Ed.Carets[0];
  EditorGetCurrentWord(Ed,
    Caret.PosX, Caret.PosY,
    GetActualNonWordChars,
    s_word_w,
    ACharsLeft,
    ACharsRight);
  s_word:= Utf8Encode(s_word_w);

  for n:= 0 to ListAcpText.Count-1 do
  begin
    s_type:= '';
    s_text:= '';
    s_desc:= '';

    if n<ListAcpType.Count then
      s_type:= ListAcpType[n];
    if n<ListAcpText.Count then
      s_text:= ListAcpText[n];
    if n<ListAcpDesc.Count then
      s_desc:= ListAcpDesc[n];

    if s_word<>'' then
    begin
      if CaseSens then
        ok:= SBeginsWith(s_text, s_word)
      else
        ok:= SBeginsWith(UpperCase(s_text), UpperCase(s_word));
      if not ok then Continue;
    end;

    AContent.Add(s_type+'|'+s_text+'|'+s_desc);
  end;
end;

constructor TAcp.Create;
begin
  inherited;
  ListAcpType:= TStringlist.create;
  ListAcpText:= TStringlist.create;
  ListAcpDesc:= TStringlist.create;
end;

destructor TAcp.Destroy;
begin
  FreeAndNil(ListAcpType);
  FreeAndNil(ListAcpText);
  FreeAndNil(ListAcpDesc);
  inherited;
end;

procedure DoEditorCompletionAcp(AEdit: TATSynEdit;
  const AFilenameAcp: string; ACaseSens: boolean);
var
  bPascal: boolean;
begin
  if not FileExists(AFilenameAcp) then exit;
  bPascal:= Pos('Pascal', ExtractFileName(AFilenameAcp))>0;
  Acp.DoLoadAcpFile(AFilenameAcp, bPascal);
  Acp.Ed:= AEdit;
  Acp.CaseSens:= ACaseSens;
  EditorShowCompletionListbox(AEdit, @Acp.DoOnGetCompleteProp);
end;

initialization
  Acp:= TAcp.Create;

finalization
  FreeAndNil(Acp);

end.

