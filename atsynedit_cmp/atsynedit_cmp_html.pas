{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_HTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit;

procedure DoEditorCompletionHtml(Ed: TATSynEdit);

type
  TATCompletionOptionsHtml = record
    FilenameHtmlList: string; //from CudaText: data/autocompletespec/html_list.ini
    FileMaskPictures: string;
    FileMaskHREF: string;
    PrefixTag: string;
    PrefixAttrib: string;
    PrefixValue: string;
    MaxLinesPerTag: integer;
    NonWordChars: UnicodeString;
  end;

var
  CompletionOpsHtml: TATCompletionOptionsHtml;

implementation

uses
  StrUtils,
  ATStringProc,
  ATStringProc_Separator,
  ATSynEdit_Carets,
  ATSynEdit_RegExpr,
  ATSynEdit_Cmp_Form,
  ATSynEdit_Cmp_CSS,
  ATSynEdit_Cmp_Filenames,
  Dialogs,
  Math;

type
  TCompletionHtmlContext = (
    ctxNone,
    ctxTags,
    ctxAttrs,
    ctxValues,
    ctxValuesQuoted,
    ctxValueHref,
    ctxValueImageSrc
    );

function _IsFilenameOk(const fn: string): boolean;
begin
  Result:= false;
  if SBeginsWith(fn, 'http:') then exit;
  if SBeginsWith(fn, 'https:') then exit;
  if Pos('://', fn)>0 then exit;
  Result:= true;
end;


function IsTagNeedsClosingTag(const S: string): boolean;
begin
  case S of
    'area',
    'base',
    'basefont',
    'br',
    'embed',
    'frame',
    'hr',
    'img',
    'input',
    'keygen',
    'link',
    'meta',
    'param',
    'source',
    'track':
      Result:= false;
    else
      Result:= true;
  end;
end;

type
  { TAcp }

  TAcp = class
  private
    List: TStringlist;
    procedure DoOnGetCompleteProp(Sender: TObject; out AText: string;
      out ACharsLeft, ACharsRight: integer);
  public
    Ed: TATSynEdit;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

var
  Acp: TAcp = nil;

function SFindRegex(const SText, SRegex: string; NGroup: integer): string;
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

function _StringEndsWithUnclosedQuote(const S: string; out AValueStr: string): boolean;
var
  i: integer;
begin
  Result:= false;
  AValueStr:= '';
  for i:= Length(S) downto 1 do
    case S[i] of
       '=':
         exit;
       '"', '''':
         begin
           AValueStr:= Copy(S, i+1, MaxInt);
           exit(true);
         end;
    end;
end;

function EditorGetHtmlContext(Ed: TATSynedit;
  APosX, APosY: integer;
  out ATagName, AAttrName, AValueStr: string;
  out ATagClosing: boolean;
  out ACharAfter: char): TCompletionHtmlContext;
const
  //regex to catch tag name at line start
  cRegexTagPart = '^\w+\b';
  cRegexTagOnly = '^\w*$';
  cRegexTagClose = '^/\w*$';
  //character class for all chars inside quotes
  cRegexChars = '[\s\w,\.:;\-\+\*\?=\(\)\[\]\{\}/\\\|~`\^\$&%\#@!\n]';
  //regex to catch attrib name, followed by "=" and not-closed quote, only at line end
  //this regex has $ at end so it's found just before the caret
  cRegexAttr = '\b([\w\-]+)\s*\=\s*([''"]' + cRegexChars + '*)?$';
  //regex group
  cGroupTagPart = 0;
  cGroupTagOnly = 0;
  cGroupTagClose = 0;
  cGroupAttr = 1;
var
  S: atString;
  NPrev, N: integer;
  ch: WideChar;
begin
  ATagName:= '';
  AAttrName:= '';
  AValueStr:= '';
  ATagClosing:= false;
  ACharAfter:= ' ';
  Result:= ctxNone;

  //get str before caret
  S:= Ed.Strings.LineSub(APosY, 1, APosX);

  //detect char after caret and next wordchars
  N:= APosX;
  repeat
    ch:= Ed.Strings.LineCharAt(APosY, N);
    if not IsCharWordA(ch) then
    begin
      ACharAfter:= ch;
      Break;
    end;
    Inc(N);
  until false;

  //add few previous lines to support multiline tags
  if APosY>0 then
  begin
    NPrev:= Max(0, APosY-CompletionOpsHtml.MaxLinesPerTag);
    for N:= APosY-1 downto NPrev do
      S:= Ed.Strings.Lines[N]+' '+S;
  end;

  //cut string before last "<" or ">" char
  N:= Length(S);
  while (N>0) and (S[N]<>'<') and (S[N]<>'>') do Dec(N);
  if N=0 then Exit;
  Delete(S, 1, N);

  ATagName:= LowerCase(SFindRegex(S, cRegexTagClose, cGroupTagClose));
  if ATagName<>'' then
  begin
    ATagClosing:= true;
    exit(ctxTags);
  end;

  ATagName:= LowerCase(SFindRegex(S, cRegexTagOnly, cGroupTagOnly));
  if ATagName<>'' then
    exit(ctxTags);

  ATagName:= LowerCase(SFindRegex(S, cRegexTagPart, cGroupTagPart));
  if ATagName<>'' then
  begin
    AAttrName:= LowerCase(SFindRegex(S, cRegexAttr, cGroupAttr));
    if AAttrName<>'' then
    begin
      if _StringEndsWithUnclosedQuote(S, AValueStr) then
      begin
        Result:= ctxValuesQuoted;
        if (ATagName='a') and (AAttrName='href') then
          Result:= ctxValueHref
        else
        if (ATagName='img') and (AAttrName='src') then
          Result:= ctxValueImageSrc;
      end
      else
        Result:= ctxValues;
    end
    else
      Result:= ctxAttrs;
  end
  else
    Result:= ctxTags;
end;

procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
var
  Caret: TATCaretItem;
  Context: TCompletionHtmlContext;
  Sep, Sep2: TATStringSeparator;
  s_word: atString;
  s_tag, s_attr, s_item, s_subitem, s_value,
  s_tag_bracket, s_tag_close: string;
  s_quote, s_space, s_equalchar: string;
  ok, bClosing: boolean;
  NextChar: char;
  i: integer;
begin
  AText:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;

  Caret:= Ed.Carets[0];
  Context:= EditorGetHtmlContext(Ed,
    Caret.PosX,
    Caret.PosY,
    s_tag,
    s_attr,
    s_value,
    bClosing,
    NextChar);

  EditorGetCurrentWord(Ed,
    Caret.PosX,
    Caret.PosY,
    CompletionOpsHtml.NonWordChars,
    s_word,
    ACharsLeft,
    ACharsRight);

  case Context of
    ctxTags:
      begin
        for i:= 0 to List.Count-1 do
        begin
          s_item:= List.Names[i];

          //special handle of some tags: a, img, link...
          if s_item='a' then s_item:= 'a'#1' href="'#1'"></a>' else
          if s_item='img' then s_item:= 'img'#1' src="'#1'">' else
          if s_item='link' then s_item:= 'link'#1' rel="stylesheet" type="text/css" href="'#1'">' else
          //usual handle of all tags
          begin
            s_tag_bracket:= '';
            s_tag_close:= '';
            if NextChar<>'>' then
            begin
              s_tag_bracket:= '>';
              if not bClosing and IsTagNeedsClosingTag(s_item) then
                s_tag_close:= #1'</'+s_item+'>';
            end;
            s_item:= s_item+#1+s_tag_bracket+s_tag_close;
          end;

          //filter items
          if s_word<>'' then
          begin
            ok:= SBeginsWith(UpperCase(s_item), UpperCase(s_word));
            if not ok then Continue;
          end;
          AText:= AText+CompletionOpsHtml.PrefixTag+'|'+s_item+#13;
        end;
      end;

    ctxAttrs:
      begin
        s_item:= List.Values[s_tag];
        if s_item='' then exit;

        if NextChar='=' then
          s_equalchar:= ''
        else
          s_equalchar:= '=';

        Sep.Init(s_item, '|');
        repeat
          if not Sep.GetItemStr(s_subitem) then Break;
          s_subitem:= SGetItem(s_subitem, '<');
          if s_subitem='' then Break;

          //keep only items which begin with s_word
          if s_word<>'' then
          begin
            ok:= SBeginsWith(UpperCase(s_subitem), UpperCase(s_word));
            if not ok then Continue;
          end;
          AText:= AText+s_tag+' '+CompletionOpsHtml.PrefixAttrib+'|'+s_subitem+#1+s_equalchar+#13;
        until false;
      end;

    ctxValues,
    ctxValuesQuoted:
      begin
        if Context=ctxValuesQuoted then
        begin
          s_quote:= '';
          s_space:= '';
        end
        else
        begin
          s_quote:= '"';
          s_space:= ' ';
        end;
        s_item:= List.Values[s_tag];
        if s_item='' then exit;
        Sep.Init(s_item, '|');
        repeat
          if not Sep.GetItemStr(s_subitem) then Break;
          if SGetItem(s_subitem, '<')<>s_attr then Continue;
          Sep2.Init(s_subitem, '?');
          repeat
            if not Sep2.GetItemStr(s_value) then Break;
            AText:= AText+s_attr+' '+CompletionOpsHtml.PrefixValue+'|'+s_quote+s_value+s_quote+#1+s_space+#13;
          until false;
        until false;
      end;

    ctxValueHref:
      begin
        if not _IsFilenameOk(s_value) then exit;
        AText:= CalculateCompletionFilenames(ExtractFileDir(Ed.FileName), s_value, CompletionOpsHtml.FileMaskHREF);
      end;

    ctxValueImageSrc:
      begin
        if not _IsFilenameOk(s_value) then exit;
        AText:= CalculateCompletionFilenames(ExtractFileDir(Ed.FileName), s_value, CompletionOpsHtml.FileMaskPictures);
      end;
  end;
end;

constructor TAcp.Create;
begin
  inherited;
  List:= TStringlist.create;
end;

destructor TAcp.Destroy;
begin
  FreeAndNil(List);
  inherited;
end;

procedure DoEditorCompletionHtml(Ed: TATSynEdit);
var
  Caret: TATCaretItem;
  Context: TCompletionHtmlContext;
  S_Tag, S_Attr, S_Value: string;
  S: atString;
  bClosing, bAddBracket: boolean;
  NextChar: char;
  ch: WideChar;
  i: integer;
begin
  Acp.Ed:= Ed;

  //load file only once
  if Acp.List.Count=0 then
  begin
    if not FileExists(CompletionOpsHtml.FilenameHtmlList) then exit;
    Acp.List.LoadFromFile(CompletionOpsHtml.FilenameHtmlList);
  end;

  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
  S:= Ed.Strings.Lines[Caret.PosY];

  //we are inside style="..." ? call CSS completions.
  Context:= EditorGetHtmlContext(Ed,
    Caret.PosX,
    Caret.PosY,
    S_Tag,
    S_Attr,
    S_Value,
    bClosing,
    NextChar);
  if (Context in [ctxValues, ctxValuesQuoted]) and (S_Attr='style') then
  begin
    DoEditorCompletionCss(Ed);
    exit;
  end;

  //bAddBracket: insert missing '<' if completion was called without it
  bAddBracket:= false;
  if Caret.PosX>Length(S) then exit;
  if Caret.PosX=0 then
    bAddBracket:= true
  else
  begin
    //check that before caret it's not bad position:
    //- someword after line start
    //- someword after ">"
    i:= Caret.PosX;
    if (i>0) and (i<=Length(S)) and IsCharWordA(S[i]) then
    begin
      while (i>0) and IsCharWordA(S[i]) do Dec(i);
      if i=0 then exit;
      if S[i]='>' then exit;
    end;

    //check nearest non-space char lefter than caret
    i:= Caret.PosX;
    while (i>0) and (S[i]=' ') do Dec(i);
    if i>0 then
    begin
      ch:= S[i];
      if (Pos(ch, '<="''/:.-,')=0) and not IsCharWord(ch, cDefaultNonWordChars) then
        bAddBracket:= true;
    end
    else
      bAddBracket:= true;
  end;

  if bAddBracket then
  begin
    Insert('<', S, Caret.PosX+1);
    Ed.Strings.Lines[Caret.PosY]:= S;
    Caret.PosX:= Caret.PosX+1;
    Ed.Update(true);
    Ed.DoEventChange;
  end;

  DoEditorCompletionListbox(Ed, @Acp.DoOnGetCompleteProp,
    nil, '', 0,
    true //allow carets in HTML like Sublime does
    );
end;

initialization
  Acp:= TAcp.Create;

  with CompletionOpsHtml do
  begin
    FilenameHtmlList:= '';
    FileMaskPictures:= '*.png;*.gif;*.jpg;*.jpeg;*.ico';
    FileMaskHREF:= '*.htm;*.html;*.php*';
    PrefixTag:= 'tag';
    PrefixAttrib:= 'attrib';
    PrefixValue:= 'value';
    MaxLinesPerTag:= 40;
    NonWordChars:= '+*=/\()[]{}<>"''.,:;~?!@#$%^&|`…'; // '-' is word char
  end;

finalization
  FreeAndNil(Acp);

end.

