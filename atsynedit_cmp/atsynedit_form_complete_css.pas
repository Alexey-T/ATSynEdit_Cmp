{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Form_Complete_CSS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit,
  ATSynEdit_Carets,
  ATSynEdit_RegExpr;

//it needs file css_list.ini from SynWrite distro
procedure DoEditorCompletionCss(AEdit: TATSynEdit;
  const AFilenameCssList, AFilenameCssSelectors: string);

var
  cCssPrefixProps: string = 'prop';
  cCssPrefixAtRule: string = 'at-rule';
  cCssPrefixPseudo: string = 'pseudo';
  cCssLinesToLookUp: integer = 30; //how many lines to see up, to find nearest {} bracket

implementation

uses
  Math,
  Dialogs,
  ATStringProc,
  ATStringProc_Separator,
  ATSynEdit_form_complete;

const
  cNonWordChars = '#!@.{};'; //don't include ':'

type
  { TAcp }

  TAcp = class
  private
    List: TStringlist; //CSS props and values of props
    ListSel: TStringList; //CSS at-rules (@) and pseudo elements (:)
    procedure DoOnGetCompleteProp(Sender: TObject; out AText: string;
      out ACharsLeft, ACharsRight: integer);
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
    CtxSelectors
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
  for Y:= APosY downto Max(0, APosY-cCssLinesToLookUp) do
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

procedure EditorGetCssContext(Ed: TATSynEdit; APosX, APosY: integer;
  out AContext: TCompletionCssContext; out ATag: string);
const
  //char class for all chars in css values
  cRegexChars = '[''"\w\s\.,:/~&%@!=\#\$\^\-\+\(\)\?]';
  //regex to catch css property name, before css attribs and before ":", at line end
  cRegexProp = '([\w\-]+):\s*' + cRegexChars + '*$';
  cRegexAtRule = '(@[a-z\-]*)$';
  cRegexSelectors = '\w+(:+[a-z\-]*)$';
  cRegexGroup = 1; //group 1 in (..)
var
  S: UnicodeString;
begin
  AContext:= CtxNone;
  ATag:= '';

  S:= Ed.Strings.LineSub(APosY, 1, APosX);

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


procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
var
  Caret: TATCaretItem;
  s_word: atString;
  s_tag, s_item, s_val: string;
  context: TCompletionCssContext;
  Sep: TATStringSeparator;
  n: integer;
  ok: boolean;
begin
  AText:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;
  Caret:= Ed.Carets[0];

  EditorGetCssContext(Ed, Caret.PosX, Caret.PosY, context, s_tag);

  case context of
    CtxPropertyValue:
      begin
        s_item:= List.Values[s_tag];
        if s_item='' then exit;

        EditorGetCurrentWord(Ed,
          Caret.PosX, Caret.PosY,
          cNonWordChars,
          s_word,
          ACharsLeft,
          ACharsRight);

        Sep.Init(s_item);
        repeat
          if not Sep.GetItemStr(s_val) then Break;

          //filter values by cur word (not case sens)
          if s_word<>'' then
          begin
            ok:= SBeginsWith(UpperCase(s_val), UpperCase(s_word));
            if not ok then Continue;
          end;

          AText:= AText+cCssPrefixProps+' "'+s_tag+'"|'+s_val+#1' '#13;
        until false;
      end;

    CtxPropertyName:
      begin
        EditorGetCurrentWord(Ed,
          Caret.PosX, Caret.PosY,
          cNonWordChars,
          s_word,
          ACharsLeft,
          ACharsRight);

        //if caret is inside word
        //  back|ground: left;
        //then we must replace "background" with ": "
        s_item:= Ed.Strings.LineSub(Caret.PosY, Caret.PosX+ACharsRight+1, 2);
        if s_item=': ' then
          Inc(ACharsRight, 2);

        for n:= 0 to List.Count-1 do
        begin
          s_item:= List.Names[n];

          //filter by cur word (not case sens)
          if s_word<>'' then
          begin
            ok:= SBeginsWith(UpperCase(s_item), UpperCase(s_word));
            if not ok then Continue;
          end;

          AText:= AText+cCssPrefixProps+'|'+s_item+#1': '#13;
        end;
      end;

    CtxSelectors:
      begin
        ACharsLeft:= Length(s_tag);

        if s_tag[1]='@' then
          s_val:= cCssPrefixAtRule
        else
        if s_tag[1]=':' then
          s_val:= cCssPrefixPseudo
        else
          exit;

        for s_item in ListSel do
        begin
          if (s_tag='') or SBeginsWith(s_item, s_tag) then
            AText+= s_val+'|'+s_item+#13;
        end;
      end;
  end;
end;

constructor TAcp.Create;
begin
  inherited;
  List:= TStringlist.create;
  ListSel:= TStringlist.create;
end;

destructor TAcp.Destroy;
begin
  FreeAndNil(ListSel);
  FreeAndNil(List);
  inherited;
end;

procedure DoEditorCompletionCss(AEdit: TATSynEdit; const AFilenameCssList,
  AFilenameCssSelectors: string);
begin
  Acp.Ed:= AEdit;

  //load file only once
  if Acp.List.Count=0 then
  begin
    if not FileExists(AFilenameCssList) then exit;
    Acp.List.LoadFromFile(AFilenameCssList);
  end;

  //optional list, load only once
  if Acp.ListSel.Count=0 then
  begin
    if FileExists(AFilenameCssSelectors) then
      Acp.ListSel.LoadFromFile(AFilenameCssSelectors);
  end;

  DoEditorCompletionListbox(AEdit, @Acp.DoOnGetCompleteProp);
end;

initialization
  Acp:= TAcp.Create;

finalization
  FreeAndNil(Acp);

end.

