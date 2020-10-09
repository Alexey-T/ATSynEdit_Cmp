{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_Filenames;

{$mode objfpc}{$H+}

interface

function CalculateCompletionFilenames(const ACurDir, AText, AFileMask,
  APrefixDir, APrefixFile: string; AddDirSlash: boolean): string;

implementation

uses
  SysUtils, Classes,
  ATStringProc,
  FileUtil;

function IsValueFilename(const S: string): boolean;
begin
  Result:= false;
  if Pos('://', S)>0 then exit;
  Result:= true;
end;

(*
function IsHiddenFilename(const fn: string): boolean; inline;
begin
  {$ifdef windows}
  Result:= (FileGetAttr(fn) and (faHidden or faSysFile))<>0;
  {$else}
  Result:= SBeginsWith(ExtractFileName(fn), '.');
  {$endif}
end;
*)

function CalculateCompletionFilenames(const ACurDir, AText, AFileMask,
  APrefixDir, APrefixFile: string; AddDirSlash: boolean): string;
var
  FinderDirs: TFileSearcher;
  FinderFiles: TFileSearcher;
  L: TStringList;
  SDirName, SFileName, SItem, SItemShort: string;
begin
  Result:= '';
  if (AText<>'') and not IsValueFilename(AText) then exit;
  if ACurDir='' then exit;

  SDirName:= ACurDir+'/'+ExtractFileDir(AText);
  if not DirectoryExists(SDirName) then exit;
  SFileName:= ExtractFileName(AText);

  L:= TStringList.Create;
  FinderDirs:= TListDirectoriesSearcher.Create(L);
  FinderFiles:= TListFileSearcher.Create(L);

  try
    L.Clear;
    FinderDirs.FileAttribute:= faAnyFile and not (faHidden or faSysFile);
    FinderDirs.Search(SDirName, AllFilesMask, false{SubDirs});
    L.Sort;

    for SItem in L do
    begin
      SItemShort:= ExtractFileName(SItem);
      if (SFileName='') or SBeginsWith(SItemShort, SFileName) then
      begin
        Result+= APrefixDir+'|'+SItemShort;
        if AddDirSlash then
          Result+= '/';
        Result+= #10;
      end;
    end;

    L.Clear;
    FinderFiles.FileAttribute:= faAnyFile and not (faHidden or faSysFile);
    FinderFiles.Search(SDirName, AFileMask, false{SubDirs});
    L.Sort;

    for SItem in L do
    begin
      SItemShort:= ExtractFileName(SItem);
      if (SFileName='') or SBeginsWith(SItemShort, SFileName) then
        Result+= APrefixFile+'|'+SItemShort+#10;
    end;
  finally
    FreeAndNil(L);
    FreeAndNil(FinderDirs);
    FreeAndNil(FinderFiles);
  end;
end;

end.
