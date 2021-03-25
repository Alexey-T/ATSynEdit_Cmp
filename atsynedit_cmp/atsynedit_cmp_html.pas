{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_HTML;

{$mode objfpc}{$H+}

interface

uses
  ATSynEdit,
  ATSynEdit_Cmp_HTML_Provider;

procedure DoEditorCompletionHtml(Ed: TATSynEdit);

type
  TATCompletionOptionsHtml = record
    Provider: TATHtmlProvider;
    FilenameHtmlList: string; //from CudaText: data/autocompletespec/html_list.ini
    FilenameHtmlEvents: string; //from CudaText: data/autocompletespec/html_events.ini
    FilenameHtmlEntities: string; //from CudaText: data/autocompletespec/html_entities.ini
    FileMaskHREF: string;
    FileMaskLinkHREF: string;
    FileMaskPictures: string;
    FileMaskScript: string;
    FileMaskFrame: string;
    FileMaskAudio: string;
    FileMaskVideo: string;
    FileMaskSomeSrc: string;
    PrefixTag: string;
    PrefixAttrib: string;
    PrefixValue: string;
    PrefixDir: string;
    PrefixFile: string;
    PrefixEntity: string;
    MaxLinesPerTag: integer;
    NonWordChars: UnicodeString;
  end;

var
  CompletionOpsHtml: TATCompletionOptionsHtml;

implementation

uses
  SysUtils, Classes, Graphics, StrUtils,
  ATStringProc,
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
    ctxEntity,
    ctxValueHref,
    ctxValueLinkHref,
    ctxValueFrameSrc,
    ctxValueScriptSrc,
    ctxValueImageSrc,
    ctxValueAudioSrc,
    ctxValueVideoSrc,
    ctxValueSourceSrc
    );

function IsQuote(ch: WideChar): boolean; inline;
begin
  case ch of
    '"', '''':
      Result:= true;
    else
      Result:= false;
  end;
end;

function IsQuoteOrSlash(ch: WideChar): boolean; inline;
begin
  case ch of
    '"', '''', '/', '\':
      Result:= true;
    else
      Result:= false;
  end;
end;

procedure EditorGetDistanceToQuotes(Ed: TATSynEdit; out ALeft, ARight: integer; out AAddSlash: boolean);
var
  Caret: TATCaretItem;
  S: UnicodeString;
  Len, X, i: integer;
begin
  ALeft:= 0;
  ARight:= 0;
  AAddSlash:= true;

  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
  S:= Ed.Strings.Lines[Caret.PosY];
  Len:= Length(S);
  X:= Caret.PosX+1;

  i:= X;
  while (i<=Len) and not IsQuote(S[i]) do Inc(i);
  ARight:= i-X;

  i:= X;
  while (i>1) and (i<=Len) and not IsQuoteOrSlash(S[i-1]) do Dec(i);
  ALeft:= Max(0, X-i);
end;


function IsTagNeedsClosingTag(const S: string): boolean;
begin
  case S of
    'area',
    'base',
    'br',
    'col',
    'command', //obsolete
    'embed',
    'frame', //not in html5
    'hr',
    'img',
    'input',
    'keygen', //obsolete
    'link',
    'menuitem', //obsolete
    'meta',
    'param',
    'source',
    'track',
    'wbr':
      Result:= false;
    else
      Result:= true;
  end;
end;

type
  { TAcp }

  TAcp = class
  private
    procedure DoOnGetCompleteProp(Sender: TObject; out AText: string;
      out ACharsLeft, ACharsRight: integer);
  public
    Ed: TATSynEdit;
    ListEntities: TStringList;
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
  S: UnicodeString;
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

  //detect HTML entity like &name;
  if (APosX>0) and (APosX<=Ed.Strings.LinesLen[APosY]) then
  begin
    N:= Length(S);
    while (N>0) and IsCharWordInIdentifier(S[N]) do
      Dec(N);
    if (N>0) and (S[N]='&') then
      exit(ctxEntity);
  end;

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
        if ((ATagName='frame') or (ATagName='iframe')) and (AAttrName='src') then
          Result:= ctxValueFrameSrc
        else
        if (ATagName='link') and (AAttrName='href') then
          Result:= ctxValueLinkHref
        else
        if (ATagName='script') and (AAttrName='src') then
          Result:= ctxValueScriptSrc
        else
        if ((ATagName='img') or (ATagName='input')) and (AAttrName='src') then
          Result:= ctxValueImageSrc
        else
        if (ATagName='audio') and (AAttrName='src') then
          Result:= ctxValueAudioSrc
        else
        if (ATagName='video') and (AAttrName='src') then
          Result:= ctxValueVideoSrc
        else
        if (ATagName='source') and (AAttrName='src') then
          Result:= ctxValueSourceSrc;
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

{ TAcp }

procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
  //
  function GetFileNames(const AText, AFileMask: string): string;
  var
    bAddSlash: boolean;
  begin
    EditorGetDistanceToQuotes(Ed, ACharsLeft, ACharsRight, bAddSlash);
    Result:= CalculateCompletionFilenames(
      ExtractFileDir(Ed.FileName),
      AText,
      AFileMask,
      CompletionOpsHtml.PrefixDir,
      CompletionOpsHtml.PrefixFile,
      bAddSlash,
      false
      );
  end;
  //
var
  Caret: TATCaretItem;
  Context: TCompletionHtmlContext;
  s_word: atString;
  s_tag, s_attr, s_item, s_value,
  s_tag_bracket, s_tag_close: string;
  s_quote, s_space, s_equalchar: string;
  ok, bClosing: boolean;
  NextChar: char;
  L: TStringList;
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
        L:= TStringList.Create;
        try
          CompletionOpsHtml.Provider.GetTags(L);

          for i:= 0 to L.Count-1 do
          begin
            s_item:= L[i];

            //special handle of some tags: a, img, link...
            if s_item='a' then
              s_item:= 'a'#1' href="'#1'"></a>'
            else
            if s_item='img' then
              s_item:= 'img'#1' src="'#1'">'
            else
            if s_item='link' then
              s_item:= 'link'#1' rel="stylesheet" type="text/css" href="'#1'">'
            else
            begin
              //usual handle of all tags
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
              ok:= StartsText(s_word, s_item);
              if not ok then Continue;
            end;
            AText+= CompletionOpsHtml.PrefixTag+'|'+s_item+#10;
          end;
        finally
          FreeAndNil(L);
        end;
      end;

    ctxAttrs:
      begin
        if NextChar='=' then
          s_equalchar:= ''
        else
          s_equalchar:= '=';

        L:= TStringList.Create;
        try
          CompletionOpsHtml.Provider.GetTagProps(s_tag, L);

          //keep only items which begin with s_word
          for s_item in L do
          begin
            if s_word<>'' then
            begin
              ok:= StartsText(s_word, s_item);
              if not ok then Continue;
            end;
            AText+= s_tag+' '+CompletionOpsHtml.PrefixAttrib+'|'+s_item+#1+s_equalchar+#10;
          end;
        finally
          FreeAndNil(L);
        end;
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

        L:= TStringList.Create;
        try
          CompletionOpsHtml.Provider.GetTagPropValues(s_tag, s_attr, L);
          for s_value in L do
            AText+= s_attr+' '+CompletionOpsHtml.PrefixValue+'|'+s_quote+s_value+s_quote+#1+s_space+#10;
        finally
          FreeAndNil(L);
        end;
      end;

    ctxEntity:
      begin
        if not Assigned(ListEntities) then
        begin
          ListEntities:= TStringList.Create;
          if FileExists(CompletionOpsHtml.FilenameHtmlEntities) then
            ListEntities.LoadFromFile(CompletionOpsHtml.FilenameHtmlEntities);
        end;
        for s_value in ListEntities do
          if StartsText(s_word, s_value) then
            AText+= CompletionOpsHtml.PrefixEntity+'|'+s_value+';'+#10;
      end;

    ctxValueHref:
      begin
        AText:= GetFileNames(s_value, CompletionOpsHtml.FileMaskHREF);
      end;

    ctxValueLinkHref:
      begin
        AText:= GetFileNames(s_value, CompletionOpsHtml.FileMaskLinkHREF);
      end;

    ctxValueScriptSrc:
      begin
        AText:= GetFileNames(s_value, CompletionOpsHtml.FileMaskScript);
      end;

    ctxValueFrameSrc:
      begin
        AText:= GetFileNames(s_value, CompletionOpsHtml.FileMaskFrame);
      end;

    ctxValueImageSrc:
      begin
        AText:= GetFileNames(s_value, CompletionOpsHtml.FileMaskPictures);
      end;

    ctxValueAudioSrc:
      begin
        AText:= GetFileNames(s_value, CompletionOpsHtml.FileMaskAudio);
      end;

    ctxValueVideoSrc:
      begin
        AText:= GetFileNames(s_value, CompletionOpsHtml.FileMaskVideo);
      end;

    ctxValueSourceSrc:
      begin
        AText:= GetFileNames(s_value, CompletionOpsHtml.FileMaskSomeSrc);
      end;
  end;
end;

constructor TAcp.Create;
begin
  inherited;
end;

destructor TAcp.Destroy;
begin
  FreeAndNil(ListEntities);
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

  if CompletionOpsHtml.Provider=nil then
    CompletionOpsHtml.Provider:= TATHtmlBasicProvider.Create(
      CompletionOpsHtml.FilenameHtmlList,
      CompletionOpsHtml.FilenameHtmlEvents);

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
  if Context<>ctxEntity then
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
    Provider:= nil;
    FilenameHtmlList:= '';
    FilenameHtmlEvents:= '';
    FilenameHtmlEntities:= '';
    FileMaskHREF:= AllFilesMask;
    FileMaskLinkHREF:= '*.css';
    FileMaskPictures:= '*.png;*.gif;*.jpg;*.jpeg;*.ico';
    FileMaskScript:= '*.js';
    FileMaskFrame:= '*.htm;*.html;*.php*;*.asp;*.aspx';
    FileMaskAudio:= '*.mp3;*.ogg;*.wav';
    FileMaskVideo:= '*.mp4;*.ogg;*.webm';
    FileMaskSomeSrc:= FileMaskAudio+';'+FileMaskVideo;
    PrefixTag:= 'tag';
    PrefixAttrib:= 'attrib';
    PrefixValue:= 'value';
    PrefixDir:= 'folder';
    PrefixFile:= 'file';
    PrefixEntity:= 'entity';
    MaxLinesPerTag:= 40;
    NonWordChars:= '+*=/\()[]{}<>"''.,:;~?!@#$%^&|`…'; // '-' is word char
  end;

finalization

  with CompletionOpsHtml do
    if Assigned(Provider) then
      FreeAndNil(Provider);

  FreeAndNil(Acp);

end.

