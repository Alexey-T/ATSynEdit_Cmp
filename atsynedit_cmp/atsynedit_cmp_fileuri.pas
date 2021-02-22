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
  end;

var
  CompletionOpsFile: TATCompletionOptionsFile;

implementation

uses
  SysUtils, Classes, Graphics,
  StrUtils,
  URIParser,
  ATSynEdit_Carets,
  ATSynEdit_Cmp_Form,
  ATSynEdit_Cmp_Filenames;

const
  cFilePrefix = 'file://';

//copy from https://github.com/graemeg/freepascal/blob/master/packages/fcl-base/src/uriparser.pp
const
  GenDelims = [':', '/', '?', '#', '[', ']', '@'];
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/'];

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
  if ch='%' then exit(true);
  Result:= char(Ord(ch)) in ValidPathChars;
end;

function GetContext(Ed: TATSynEdit;
  out AFileName: string;
  out ACharsLeft, ACharsRight: integer): boolean;
var
  Caret: TATCaretItem;
  S: UnicodeString;
  NPos, i: integer;
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

  NPos:= RPosEx(cFilePrefix, S, Caret.PosX);
  if NPos=0 then exit;

  if not URIToFilename(
    Copy(S, NPos, Caret.PosX-NPos+1),
    AFileName) then exit;

  if Pos('localhost', AFileName)=1 then
    Delete(AFileName, 1, Length('localhost'));

  ACharsLeft:= Length(ExtractFileName(AFileName));
  Result:= true;
end;

procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
var
  SFileName: string;
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
    true, //bAddSlash
    true
    );
end;

function DoEditorCompletionFileURI(Ed: TATSynEdit): boolean;
var
  SFilename: string;
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
  end;

finalization
  if Assigned(Acp) then
    FreeAndNil(Acp);

end.

