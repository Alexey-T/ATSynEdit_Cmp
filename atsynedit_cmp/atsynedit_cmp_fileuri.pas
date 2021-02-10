{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_FileURI;

{$mode objfpc}{$H+}

interface

uses
  ATSynEdit;

procedure DoEditorCompletionFileURI(Ed: TATSynEdit);
function DoEditorCompletionFileURIContext(Ed: TATSynEdit;
  out AFileName: UnicodeString;
  out ACharsLeft, ACharsRight: integer): boolean;

implementation

uses
  SysUtils, Classes, Graphics,
  Dialogs,
  ATSynEdit_Carets,
  ATSynEdit_Cmp_Form,
  ATSynEdit_Cmp_Filenames;
  
type
  { TAcp }

  TAcp = class
  private
    procedure DoOnGetCompleteProp(Sender: TObject; out AText: string;
      out ACharsLeft, ACharsRight: integer);
  public
    Ed: TATSynEdit;
  end;

var
  Acp: TAcp = nil;

function IsCharFromFilename(ch: WideChar): boolean;
begin
  case ch of
    'a'..'z',
    'A'..'Z',
    '0'..'9',
    '_',
    '/', '\',
    '-', '+',
    '(', ')', '[', ']',
    ':', '%', '.', ',', '=', '&':
      Result:= true;
    else
      Result:= false;
  end;
end;

const
  cFilePrefix = 'file:///';

function IsPrefixEndsAt(const S: UnicodeString; APos: integer): boolean;
begin
  Result:= false;
  if APos<Length(cFilePrefix) then exit;
  if APos>Length(S) then exit;
  if (S[APos]='/') and
    (S[APos-1]='/') and
    (S[APos-2]='/') and
    (S[APos-3]=':') and
    (S[APos-4]='e') and
    (S[APos-5]='l') and
    (S[APos-6]='i') and
    (S[APos-7]='f') then
    Result:= true;
end;

function DoEditorCompletionFileURIContext(Ed: TATSynEdit;
  out AFileName: UnicodeString;
  out ACharsLeft, ACharsRight: integer): boolean;
var
  Caret: TATCaretItem;
  S: UnicodeString;
  ch: WideChar;
  i: integer;
begin
  Result:= false;
  AFileName:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;

  if Ed.Carets.Count<>1 then exit;
  if Ed.Carets.IsSelection then exit;
  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
  S:= Ed.Strings.Lines[Caret.PosY];
  if Caret.PosX>Length(S) then exit;
  if Caret.PosX<=Length(cFilePrefix) then exit;

  for i:= Caret.PosX downto Length(cFilePrefix) do
  begin
    ch:= S[i];
    if not IsCharFromFilename(ch) then
      exit;
    if IsPrefixEndsAt(S, i) then
    begin
      Result:= true;
      AFileName:= Copy(S, i+1, Caret.PosX-i);
      ACharsLeft:= Length(AFileName);
      exit
    end;
  end;
end;

procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
  //
  function GetFileNames(const AFileName: string): string;
  var
    bAddSlash: boolean;
  begin
    bAddSlash:= true;
    Result:= CalculateCompletionFilenames(
      ExtractFileDir(AFileName),
      ExtractFileName(AFileName),
      AllFilesMask,
      'folder',
      'file',
      bAddSlash
      );
  end;
  //
var
  SFileName: UnicodeString;
begin
  if not DoEditorCompletionFileURIContext(Ed, SFileName, ACharsLeft, ACharsRight) then
  begin
    AText:= '';
    ACharsLeft:= 0;
    ACharsRight:= 0;
    exit;
  end;
  AText:= GetFileNames(SFileName);
end;

procedure DoEditorCompletionFileURI(Ed: TATSynEdit);
var
  SFilename: UnicodeString;
  NCharsLeft, NCharsRight: integer;
begin
  Acp.Ed:= Ed;

  if not DoEditorCompletionFileURIContext(Ed, SFilename, NCharsLeft, NCharsRight) then exit;

  DoEditorCompletionListbox(Ed, @Acp.DoOnGetCompleteProp,
    nil, '', 0,
    false
    );
end;

initialization
  Acp:= TAcp.Create;

finalization
  FreeAndNil(Acp);

end.

