{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_CSS;

{$mode objfpc}{$H+}

interface

uses
  ATSynEdit,
  ATSynEdit_Cmp_CSS_Provider;

procedure DoEditorCompletionCss(AEdit: TATSynEdit);

type
  TATCompletionOptionsCss = record
    Provider: TATCssProvider;
    FilenameCssList: string; //from CudaText: data/autocompletespec/css_list.ini
    FilenameCssColors: string; //from CudaText: data/autocompletespec/css_colors.ini
    FilenameCssSelectors: string; //from CudaText: data/autocompletespec/css_sel.ini
    PrefixProp: string;
    PrefixVar: string;
    PrefixAtRule: string;
    PrefixPseudo: string;
    PrefixDir: string;
    PrefixFile: string;
    LinesToLookup: integer;
    NonWordChars: UnicodeString;
    AppendSemicolon: boolean;
    FileMaskURL: string;
  end;

var
  CompletionOpsCss: TATCompletionOptionsCss;

implementation

uses
  SysUtils, Classes, Graphics,
  StrUtils, Math,
  ATSynEdit_Carets,
  ATSynEdit_RegExpr,
  ATSynEdit_Cmp_Filenames,
  ATSynEdit_Cmp_Form;

type
  TQuoteKind = (qkNone, qkSingle, qkDouble);

type
  { TAcp }

  TAcp = class
  private
    ListSel: TStringList; //CSS at-rules (@) and pseudo elements (:)
    procedure DoOnGetCompleteProp(Sender: TObject; AContent: TStringList;
      out ACharsLeft, ACharsRight: integer);
    procedure DoOnChoose(Sender: TObject; const ASnippetId: string; ASnippetIndex: integer);
    procedure FindAllCustomProps(L: TStringList; AMaxLine: integer);
  public
    Ed: TATSynEdit;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

var
  Acp: TAcp = nil;

type
  TCompletionCssContext = (
    CtxNone,
    CtxPropertyName,
    CtxPropertyValue,
    CtxSelectors,
    CtxUrl,
    CtxVar
    );

function SFindRegex(const SText, SRegex: UnicodeString; NGroup: integer): string;
var
  R: TRegExpr;
begin
  Result:= '';
  R:= TRegExpr.Create;
  try
    R.ModifierS:= false;
    R.ModifierM:= true;
    R.ModifierI:= true;

    R.Expression:= SRegex;
    R.InputString:= SText;

    if R.ExecPos(1) then
      Result:= Copy(SText, R.MatchPos[NGroup], R.MatchLen[NGroup]);
  finally
    R.Free;
  end;
end;

function EditorGetCaretInCurlyBrackets(Ed: TATSynEdit; APosX, APosY: integer): boolean;
var
  S: UnicodeString;
  X, Y: integer;
begin
  Result:= false;
  for Y:= APosY downto Max(0, APosY-CompletionOpsCss.LinesToLookup) do
  begin
    S:= Ed.Strings.Lines[Y];
    if Y=APosY then
      Delete(S, APosX+1, MaxInt);
    for X:= Length(S) downto 1 do
    begin
      if S[X]='{' then
        exit(true);
      if S[X]='}' then
        exit(false);
    end;
  end;
end;

function GetQuoteKind(const S: string): TQuoteKind;
begin
  case S of
    '''':
      Result:= qkSingle;
    '"':
      Result:= qkDouble;
    else
      Result:= qkNone;
  end;
end;

procedure EditorGetCssContext(Ed: TATSynEdit; APosX, APosY: integer;
  out AContext: TCompletionCssContext; out ATag: string; out AQuoteKind: TQuoteKind);
const
  //char class for all chars in css values
  cRegexChars = '[''"\w\s\.,:/~&%@!=\#\$\^\-\+\(\)\?]';
  //regex to catch css property name, before css attribs and before ":", at line end
  cRegexProp = '([\w\-]+):\s*' + cRegexChars + '*$';
  cRegexAtRule = '(@[a-z\-]*)$';
  cRegexSelectors = '\w+(:+[a-z\-]*)$';
  cRegexGroup = 1; //group 1 in (..)

  //group 1: quote char, group 2: URL text
  cRegexUrl =      'url\(\s*([''"]?)(' + '[\w\.,/~@!=\-\(\)\[\]]*' + ')$';
  cRegexUrlEmpty = 'url\(\s*(''|"|)$';

  cRegexVar = 'var\(\s*([\w\-]*)$';
var
  S, S2: UnicodeString;
  SQuote: string;
  NPos: integer;
begin
  AContext:= CtxNone;
  ATag:= '';
  AQuoteKind:= qkDouble;

  S:= Ed.Strings.LineSub(APosY, 1, APosX);

  //detect 'var()' context
  NPos:= RPos(' var(', S);
  if NPos=0 then
    NPos:= RPos(':var(', S);
  if NPos>0 then
  begin
    S2:= Copy(S, NPos+1, MaxInt);
    if SFindRegex(S2, cRegexVar, 0)<>'' then
    begin
      ATag:= SFindRegex(S2, cRegexVar, 1);
      AContext:= CtxVar;
      exit;
    end;
  end;

  //detect 'url()' context
  NPos:= RPos(' url(', S);
  if NPos=0 then
    NPos:= RPos(':url(', S);
  if NPos>0 then
  begin
    S2:= Copy(S, NPos+1, MaxInt);

    //empty URL before caret?
    if SFindRegex(S2, cRegexUrlEmpty, 0)<>'' then
    begin
      AContext:= CtxUrl;
      SQuote:= SFindRegex(S2, cRegexUrlEmpty, 1);
      AQuoteKind:= GetQuoteKind(SQuote);
      exit;
    end;

    //not empty URL?
    ATag:= SFindRegex(S2, cRegexUrl, 2);
    if ATag<>'' then
    begin
      AContext:= CtxUrl;
      SQuote:= SFindRegex(S2, cRegexUrl, 1);
      AQuoteKind:= GetQuoteKind(SQuote);
      exit;
    end;
  end;

  ATag:= SFindRegex(S, cRegexAtRule, cRegexGroup);
  if ATag<>'' then
  begin
    AContext:= CtxSelectors;
    exit;
  end;

  if EditorGetCaretInCurlyBrackets(Ed, APosX, APosY) then
  begin
    AContext:= CtxPropertyName;
    if S='' then
      exit;
    ATag:= SFindRegex(S, cRegexProp, cRegexGroup);
    if ATag<>'' then
      AContext:= CtxPropertyValue;
  end
  else
  begin
    ATag:= SFindRegex(S, cRegexSelectors, cRegexGroup);
    if ATag<>'' then
      AContext:= CtxSelectors;
  end;
end;

function IsQuoteRight(QuoteKind: TQuoteKind; ch: WideChar): boolean; inline;
begin
  case ch of
    '"':
      begin
        if QuoteKind=qkDouble then
          Result:= true;
      end;
    '''':
      begin
        if QuoteKind=qkSingle then
          Result:= true;
      end;
    ')':
      begin
        if QuoteKind=qkNone then
          Result:= true;
      end;
    else
      Result:= false;
  end;
end;

function IsQuoteLeftOrSlash(QuoteKind: TQuoteKind; ch: WideChar): boolean; inline;
begin
  case ch of
    '"':
      begin
        if QuoteKind=qkDouble then
          Result:= true;
      end;
    '''':
      begin
        if QuoteKind=qkSingle then
          Result:= true;
      end;
    '(':
      begin
        if QuoteKind=qkNone then
          Result:= true;
      end;
    '/':
      Result:= true;
    else
      Result:= false;
  end;
end;

procedure EditorGetDistanceToQuotes(Ed: TATSynEdit;
  AQuoteKind: TQuoteKind;
  out ALeft, ARight: integer);
var
  Caret: TATCaretItem;
  S: UnicodeString;
  Len, X, i: integer;
begin
  ALeft:= 0;
  ARight:= 0;

  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
  S:= Ed.Strings.Lines[Caret.PosY];
  Len:= Length(S);
  X:= Caret.PosX+1;

  i:= X;
  while (i<=Len) and not IsQuoteRight(AQuoteKind, S[i]) do Inc(i);
  ARight:= i-X;

  i:= X;
  while (i>1) and (i<=Len) and not IsQuoteLeftOrSlash(AQuoteKind, S[i-1]) do Dec(i);
  ALeft:= Max(0, X-i);
end;

{ TAcp }

procedure TAcp.DoOnGetCompleteProp(Sender: TObject;
  AContent: TStringList; out ACharsLeft, ACharsRight: integer);
  //
  procedure GetFileNames(AResult: TStringList; AQuoteKind: TQuoteKind; const AText, AFileMask: string);
  begin
    EditorGetDistanceToQuotes(Ed, AQuoteKind, ACharsLeft, ACharsRight);
    CalculateCompletionFilenames(AResult,
      ExtractFileDir(Ed.FileName),
      AText,
      AFileMask,
      CompletionOpsCss.PrefixDir,
      CompletionOpsCss.PrefixFile,
      true, //AddSlash
      false
      );
  end;
  //
var
  Caret: TATCaretItem;
  L: TStringList;
  s_word, s_NonWordChars: UnicodeString;
  s_tag, s_item, s_val, s_valsuffix: string;
  context: TCompletionCssContext;
  quote: TQuoteKind;
  ok: boolean;
begin
  AContent.Clear;
  ACharsLeft:= 0;
  ACharsRight:= 0;
  Caret:= Ed.Carets[0];

  EditorGetCssContext(Ed, Caret.PosX, Caret.PosY, context, s_tag, quote);

  s_NonWordChars:= CompletionOpsCss.NonWordChars;
  if context=CtxVar then
    s_NonWordChars+= '()';

  EditorGetCurrentWord(Ed,
    Caret.PosX, Caret.PosY,
    s_NonWordChars,
    s_word,
    ACharsLeft,
    ACharsRight);

  case context of
    CtxPropertyValue:
      begin
        L:= TStringList.Create;
        try
          CompletionOpsCss.Provider.GetValues(s_tag, L);
          for s_item in L do
          begin
            s_val:= s_item;

            //filter values by cur word (not case sens)
            if s_word<>'' then
            begin
              ok:= StartsText(s_word, s_val);
              if not ok then Continue;
            end;

            //handle values like 'rgb()', 'val()'
            if EndsStr('()', s_val) then
            begin
              SetLength(s_val, Length(s_val)-2);
              s_valsuffix:= '|()';
            end
            else
              s_valsuffix:= ''; //CompletionOps.SuffixSep+' ';

            AContent.Add(CompletionOpsCss.PrefixProp+' "'+s_tag+'"|'+s_val+s_valsuffix);
          end;
        finally
          FreeAndNil(L);
        end;
      end;

    CtxPropertyName:
      begin
        //if caret is inside property
        //  back|ground: left;
        //then we must replace "background: ", ie replace extra 2 chars
        s_item:= Ed.Strings.LineSub(Caret.PosY, Caret.PosX+ACharsRight+1, 2);
        if s_item=': ' then
          Inc(ACharsRight, 2);

        L:= TStringList.Create;
        try
          CompletionOpsCss.Provider.GetProps(L);
          for s_item in L do
          begin
            //filter by cur word (not case sens)
            if s_word<>'' then
            begin
              ok:= StartsText(s_word, s_item);
              if not ok then Continue;
            end;

            AContent.Add(CompletionOpsCss.PrefixProp+'|'+s_item+#1': ');
          end;
        finally
          FreeAndNil(L);
        end;
      end;

    CtxSelectors:
      begin
        ACharsLeft:= Length(s_tag);

        if s_tag[1]='@' then
          s_val:= CompletionOpsCss.PrefixAtRule
        else
        if s_tag[1]=':' then
          s_val:= CompletionOpsCss.PrefixPseudo
        else
          exit;

        for s_item in ListSel do
        begin
          if (s_tag='') or StartsText(s_tag, s_item) then
            AContent.Add(s_val+'|'+s_item);
        end;
      end;

    CtxUrl:
      begin
        GetFileNames(AContent, quote, s_tag, CompletionOpsCss.FileMaskURL);
      end;

    CtxVar:
      begin
        case Length(s_tag) of
          1:
            begin
              if not StartsStr('-', s_tag) then exit;
            end;
          2..MaxInt:
            begin
              if not StartsStr('--', s_tag) then exit;
            end;
        end;

        L:= TStringList.Create;
        try
          FindAllCustomProps(L, Caret.PosY);
          for s_item in L do
          begin
            if s_tag<>'' then
            begin
              ok:= StartsText(s_tag, s_item);
              if not ok then Continue;
            end;
            AContent.Add(CompletionOpsCss.PrefixVar+'|'+s_item);
          end;
        finally
          FreeAndNil(L);
        end;
      end;
  end;
end;

procedure TAcp.DoOnChoose(Sender: TObject; const ASnippetId: string;
  ASnippetIndex: integer);
var
  Caret: TATCaretItem;
  S: UnicodeString;
begin
  if CompletionOpsCss.AppendSemicolon then
  begin
    Caret:= Ed.Carets[0];
    if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
    S:= Ed.Strings.Lines[Caret.PosY];
    if S='' then exit;
    if S[Length(S)]<>';' then
    begin
      S+= ';';
      Ed.Strings.Lines[Caret.PosY]:= S;
      Ed.DoEventChange(Caret.PosY);
      Ed.Update;
    end;
  end;
end;

constructor TAcp.Create;
begin
  inherited;
  ListSel:= TStringlist.create;
end;

destructor TAcp.Destroy;
begin
  FreeAndNil(ListSel);
  inherited;
end;

procedure TAcp.FindAllCustomProps(L: TStringList; AMaxLine: integer);
  //
  function _IsCharSpace(ch: WideString): boolean;
  begin
    case ch of
      ' ', #9:
        Result:= true;
      else
        Result:= false;
    end;
  end;
  //
  function _IsCharId(ch: WideChar): boolean;
  begin
    case ch of
      'a'..'z',
      'A'..'Z',
      '0'..'9',
      '_', '-':
        Result:= true;
      else
        Result:= false;
    end;
  end;

var
  S, SId: UnicodeString;
  NStart, NEnd, iLine: integer;
begin
  L.Clear;
  L.Sorted:= true;
  L.CaseSensitive:= true;

  //TODO: consider CSS comments
  for iLine:= 0 to Min(Ed.Strings.Count-1, AMaxLine) do
  begin
    S:= Ed.Strings.Lines[iLine];
    NStart:= Pos('--', S);
    if NStart=0 then Continue;
    if (NStart>1) and not _IsCharSpace(S[NStart-1]) then Continue;
    NEnd:= NStart+2;
    if NEnd>Length(S) then Continue;
    while (NEnd<=Length(S)) and _IsCharId(S[NEnd]) do
      Inc(NEnd);
    if S[NEnd]<>':' then Continue;
    SId:= Copy(S, NStart, NEnd-NStart);
    L.Add(SId);
  end;
end;

//-----------------------------
procedure DoEditorCompletionCss(AEdit: TATSynEdit);
begin
  Acp.Ed:= AEdit;

  if CompletionOpsCss.Provider=nil then
  begin
    CompletionOpsCss.Provider:= TATCssBasicProvider.Create(
      CompletionOpsCss.FilenameCssList,
      CompletionOpsCss.FilenameCssColors);
  end;

  //optional list, load only once
  if Acp.ListSel.Count=0 then
  begin
    if FileExists(CompletionOpsCss.FilenameCssSelectors) then
      Acp.ListSel.LoadFromFile(CompletionOpsCss.FilenameCssSelectors);
  end;

  EditorShowCompletionListbox(AEdit,
    @Acp.DoOnGetCompleteProp,
    nil,
    @Acp.DoOnChoose);
end;

initialization

  Acp:= TAcp.Create;

  with CompletionOpsCss do
  begin
    Provider:= nil;
    FilenameCssList:= '';
    FilenameCssColors:= '';
    FilenameCssSelectors:= '';
    PrefixProp:= 'css';
    PrefixVar:= 'var';
    PrefixAtRule:= 'at-rule';
    PrefixPseudo:= 'pseudo';
    PrefixDir:= 'folder';
    PrefixFile:= 'file';
    LinesToLookup:= 50;
    NonWordChars:= '#@.{};''"<>';
      //don't include ':' because we need to complete :pseudo and ::pseudo
      //don't include '!' because we need to complete !important thing
    AppendSemicolon:= true;
    FileMaskURL:= '*.png;*.gif;*.jpg;*.jpeg;*.ico;*.cur;*.svg;*.woff;*.css';
  end;

finalization

  with CompletionOpsCss do
    if Assigned(Provider) then
      FreeAndNil(Provider);
  FreeAndNil(Acp);

end.

