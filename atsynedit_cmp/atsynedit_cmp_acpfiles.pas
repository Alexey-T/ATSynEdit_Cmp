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
var
  i: Integer;
begin
  for i:= $20 to $2F do
    SReplaceAll(S, '%'+IntToHex(i, 2), Chr(i));

  i:= $7C;
  SReplaceAll(S, '%'+IntToHex(i, 2), Chr(i));
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

  SReplaceAll(SPar, ';', ','); //Pascal lexer has ";" param separator
  SReplaceAll(SPar, '[,', ',['); //for optional params
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
    s_type:= ListAcpType[n];
    s_text:= ListAcpText[n];
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

