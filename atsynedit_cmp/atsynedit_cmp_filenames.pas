unit ATSynEdit_Cmp_Filenames;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

procedure EditorGetCompletionFilenames(const ACurDir, AText: string; AList: TStringList; const AFileMask: string);

type
  TATCompletionOptionsFilenames = record
    PrefixDir: string;
    PrefixFile: string;
  end;

var
  CompletionOpsFilenames: TATCompletionOptionsFilenames;

implementation

uses
  ATStringProc,
  Dialogs,
  FileUtil;

procedure EditorGetCompletionFilenames(const ACurDir, AText: string;
  AList: TStringList; const AFileMask: string);
var
  L: TStringList;
  SDir, SName, S, S2: string;
begin
  AList.Clear;
  if ACurDir='' then exit;

  SDir:= ACurDir+'/'+ExtractFileDir(AText);
  SName:= ExtractFileName(AText);

  L:= TStringList.Create;
  try
    L.Clear;
    FindAllDirectories(L, SDir, false{SubDirs});
    L.Sort;

    for S in L do
    begin
      S2:= ExtractFileName(S);
      if SBeginsWith(S2, '.') then
        Continue;
      if (SName='') or SBeginsWith(S2, SName) then
        AList.Add(CompletionOpsFilenames.PrefixDir+'|'+S2+'/');
    end;

    L.Clear;
    FindAllFiles(L, SDir, AFileMask, false{SubDirs});
    L.Sort;

    for S in L do
    begin
      S2:= ExtractFileName(S);
      if SBeginsWith(S2, '.') then
        Continue;
      if (SName='') or SBeginsWith(S2, SName) then
        AList.Add(CompletionOpsFilenames.PrefixFile+'|'+S2);
    end;
  finally
    FreeAndNil(L);
  end;
end;

initialization

  CompletionOpsFilenames.PrefixDir:= 'dir';
  CompletionOpsFilenames.PrefixFile:= 'file';

end.
