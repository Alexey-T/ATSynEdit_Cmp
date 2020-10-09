unit ATSynEdit_Cmp_Filenames;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

function CalculateCompletionFilenames(const ACurDir, AText, AFileMask, APrefixDir, APrefixFile: string): string;

implementation

uses
  ATStringProc,
  Dialogs,
  FileUtil;

function _IsValueFilename(const S: string): boolean;
begin
  Result:= false;
  if SBeginsWith(S, 'http:') then exit;
  if SBeginsWith(S, 'https:') then exit;
  if Pos('://', S)>0 then exit;
  Result:= true;
end;

function CalculateCompletionFilenames(const ACurDir, AText, AFileMask,
  APrefixDir, APrefixFile: string): string;
var
  L: TStringList;
  SDir, SName, S, S2: string;
begin
  Result:= '';
  if not _IsValueFilename(AText) then exit;
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
        Result+= APrefixDir+'|'+S2+'/'#13;
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
        Result+= APrefixFile+'|'+S2+#13;
    end;
  finally
    FreeAndNil(L);
  end;
end;

end.
