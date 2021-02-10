{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_FileURI;

{$mode objfpc}{$H+}

interface

uses
  ATSynEdit;

function DoEditorCompletionFileURI(Ed: TATSynEdit): boolean;

type
  TATCompletionOptionsFile = record
    PrefixDir: string;
    PrefixFile: string;
    FilenameChars: set of char;
  end;

var
  CompletionOpsFile: TATCompletionOptionsFile;

implementation

uses
  SysUtils, Classes, Graphics,
  ATSynEdit_Carets,
  ATSynEdit_Cmp_Form,
  ATSynEdit_Cmp_Filenames;

const
  cFilePrefix = 'file:///';

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
  if Ord(ch)>255 then exit(false);
  Result:= char(Ord(ch)) in CompletionOpsFile.FilenameChars;
end;

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

function GetContext(Ed: TATSynEdit;
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

  for i:= Caret.PosX to Length(S)-1 do
    if IsCharFromFilename(S[i+1]) then
      Inc(ACharsRight)
    else
      Break;

  for i:= Caret.PosX downto Length(cFilePrefix) do
  begin
    ch:= S[i];
    if not IsCharFromFilename(ch) then
      exit;
    if IsPrefixEndsAt(S, i) then
    begin
      Result:= true;
      AFileName:= Copy(S, i+1, Caret.PosX-i);
      ACharsLeft:= Length(ExtractFileName(AFileName));
      exit
    end;
  end;
end;

procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
var
  SFileName: UnicodeString;
begin
  if not GetContext(Ed, SFileName, ACharsLeft, ACharsRight) then
  begin
    AText:= '';
    ACharsLeft:= 0;
    ACharsRight:= 0;
    exit;
  end;

  AText:= CalculateCompletionFilenames(
    ExtractFileDir(SFileName),
    ExtractFileName(SFileName),
    AllFilesMask,
    CompletionOpsFile.PrefixDir,
    CompletionOpsFile.PrefixFile,
    true //bAddSlash
    );
end;

function DoEditorCompletionFileURI(Ed: TATSynEdit): boolean;
var
  SFilename: UnicodeString;
  NCharsLeft, NCharsRight: integer;
begin
  Result:= GetContext(Ed, SFilename, NCharsLeft, NCharsRight);
  if not Result then exit;

  if not Assigned(Acp) then
    Acp:= TAcp.Create;
  Acp.Ed:= Ed;

  DoEditorCompletionListbox(Ed, @Acp.DoOnGetCompleteProp,
    nil, '', 0,
    false
    );
end;


initialization

  with CompletionOpsFile do
  begin
    PrefixDir:= 'folder';
    PrefixFile:= 'file';
    FilenameChars:= [
      'a'..'z',
      'A'..'Z',
      '0'..'9',
      '_',
      '/', '\',
      '-', '+',
      '(', ')', '[', ']',
      ':', '%', '.', ',', '=', '&'];
  end;

finalization
  if Assigned(Acp) then
    FreeAndNil(Acp);

end.

