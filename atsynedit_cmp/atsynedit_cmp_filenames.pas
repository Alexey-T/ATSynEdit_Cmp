unit ATSynEdit_Cmp_Filenames;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function CalculateCompletionFilenames(const ACurDir, AText, AFileMask,
  APrefixDir, APrefixFile: string; AddDirSlash: boolean): string;

implementation

uses
  Classes,
  ATStringProc,
  FileUtil;

function IsValueFilename(const S: string): boolean;
begin
  Result:= false;
  if Pos('://', S)>0 then exit;
  Result:= true;
end;

function IsHiddenFilename(const fn: string): boolean; inline;
begin
  {$ifdef windows}
  Result:= (FileGetAttr(fn) and (faHidden or faSysFile))<>0;
  {$else}
  Result:= SBeginsWith(ExtractFileName(fn), '.');
  {$endif}
end;

function CalculateCompletionFilenames(const ACurDir, AText, AFileMask,
  APrefixDir, APrefixFile: string; AddDirSlash: boolean): string;
var
  L: TStringList;
  SDirName, SFileName, SItem, SItemShort: string;
begin
  Result:= '';
  if not IsValueFilename(AText) then exit;
  if ACurDir='' then exit;

  SDirName:= ACurDir+'/'+ExtractFileDir(AText);
  SFileName:= ExtractFileName(AText);

  L:= TStringList.Create;
  try
    L.Clear;
    FindAllDirectories(L, SDirName, false{SubDirs});
    L.Sort;

    for SItem in L do
    begin
      if IsHiddenFilename(SItem) then Continue;
      SItemShort:= ExtractFileName(SItem);
      if (SFileName='') or SBeginsWith(SItemShort, SFileName) then
      begin
        Result+= APrefixDir+'|'+SItemShort;
        if AddDirSlash then
          Result+= '/';
        Result+= #13;
      end;
    end;

    L.Clear;
    FindAllFiles(L, SDirName, AFileMask, false{SubDirs});
    L.Sort;

    for SItem in L do
    begin
      if IsHiddenFilename(SItem) then Continue;
      SItemShort:= ExtractFileName(SItem);
      if (SFileName='') or SBeginsWith(SItemShort, SFileName) then
        Result+= APrefixFile+'|'+SItemShort+#13;
    end;
  finally
    FreeAndNil(L);
  end;
end;

end.
