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
  ATSynEdit_RegExpr,
  Dialogs;

//it needs file css_list.ini from SynWrite distro
procedure DoEditorCompletionCss(AEdit: TATSynEdit;
  const AFilenameCssList, AFilenameCssSelectors: string);


implementation

uses
  ATStringProc,
  ATStringProc_Separator,
  ATSynEdit_form_complete;

type
  { TAcp }

  TAcp = class
  private
    List: TStringlist;
    ListAt: TStringList;
    ListColon: TStringList;
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

function EditorGetCssTag(Ed: TATSynEdit; APosX, APosY: integer): string;
const
  //char class for all chars in css values
  cRegexChars = '[''"\w\s\.,:/~&%@!=\#\$\^\-\+\(\)\?]';
  //regex to catch css property name, before css attribs and before ":", at line end
  cRegexProp = '([\w\-]+):\s*' + cRegexChars + '*$';
  cRegexGroup = 1; //group 1 in (..)
var
  S: atString;
begin
  Result:= '';
  S:= Ed.Strings.LineSub(APosY, 1, APosX);
  if S<>'' then
    Result:= SFindRegex(S, cRegexProp, cRegexGroup);
end;


procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
const
  cNonWordChars = '#!@.'; //don't include ':'
var
  Caret: TATCaretItem;
  s_word: atString;
  s_tag, s_item, s_val: string;
  Sep: TATStringSeparator;
  n: integer;
  ok: boolean;
begin
  AText:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;
  Caret:= Ed.Carets[0];

  s_tag:= EditorGetCssTag(Ed, Caret.PosX, Caret.PosY);
  if s_tag<>'' then
  //show list of values for s_tag
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

      AText:= AText+'css '+s_tag+'|'+s_val+#1' '#13;
    until false;
  end
  else
  //show list of all tags
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

      AText:= AText+'css'+'|'+s_item+#1': '#13;
    end;
  end;
end;

constructor TAcp.Create;
begin
  inherited;
  List:= TStringlist.create;
  ListAt:= TStringlist.create;
  ListColon:= TStringlist.create;
end;

destructor TAcp.Destroy;
begin
  FreeAndNil(ListColon);
  FreeAndNil(ListAt);
  FreeAndNil(List);
  inherited;
end;

procedure DoEditorCompletionCss(AEdit: TATSynEdit; const AFilenameCssList,
  AFilenameCssSelectors: string);
var
  ListTemp: TStringList;
  S: string;
begin
  Acp.Ed:= AEdit;

  //load file only once
  if Acp.List.Count=0 then
  begin
    if not FileExists(AFilenameCssList) then exit;
    Acp.List.LoadFromFile(AFilenameCssList);
  end;

  //optional lists, load only once
  if Acp.ListAt.Count=0 then
  begin
    if FileExists(AFilenameCssSelectors) then
    try
      ListTemp:= TStringList.Create;
      ListTemp.LoadFromFile(AFilenameCssSelectors);

      for S in ListTemp do
      begin
        if S='' then Continue;
        if S[1]='@' then
          Acp.ListAt.Add(Copy(S, 2, MaxInt))
        else
        if S[1]=':' then
          Acp.ListColon.Add(Copy(S, 2, MaxInt));
      end;
    finally
      FreeAndNil(ListTemp);
    end;
  end;

  DoEditorCompletionListbox(AEdit, @Acp.DoOnGetCompleteProp);
end;

initialization
  Acp:= TAcp.Create;

finalization
  FreeAndNil(Acp);

end.

