{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_Filenames;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  FileUtil,
  Classes,
  URIParser,
  ATStringProc;

procedure CalculateCompletionFilenames(AResult: TStringList;
  const ACurDir, AText, AFileMask,
  APrefixDir, APrefixFile: string; AddDirSlash, AURIEncode: boolean);

implementation

const
  //GenDelims = [':', '/', '?', '#', '[', ']', '@'];
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/'];

//copy from FPC's URIParser unit
function Escape(const s: String; const Allowed: TSysCharSet): String;
var
  i, L: Integer;
  P: PChar;
begin
  L := Length(s);
  for i := 1 to Length(s) do
    if not (s[i] in Allowed) then Inc(L,2);
  if L = Length(s) then
  begin
    Result := s;
    Exit;
  end;

  SetLength(Result, L);
  P := @Result[1];
  for i := 1 to Length(s) do
  begin
    if not (s[i] in Allowed) then
    begin
      P^ := '%'; Inc(P);
      StrFmt(P, '%.2x', [ord(s[i])]); Inc(P);
    end
    else
      P^ := s[i];
    Inc(P);
  end;
end;

function EscapeWebFilename(const s: String): String;
begin
  Result:= Escape(s, ValidPathChars);
end;

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

procedure CalculateCompletionFilenames(AResult: TStringList;
  const ACurDir, AText, AFileMask,
  APrefixDir, APrefixFile: string; AddDirSlash, AURIEncode: boolean);
  //
  function MaybeEscape(const S: string): string;
  begin
    if AURIEncode then
      Result:= Escape(S, ValidPathChars)
    else
      Result:= S;
  end;
  //
var
  FinderDirs: TFileSearcher;
  FinderFiles: TFileSearcher;
  L: TStringList;
  SDirName, SFileName, SItem, SItemShort: string;
  SRes: string;
begin
  AResult.Clear;
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
    FinderDirs.FileAttribute:= faAnyFile and not (faHidden{%H-} or faSysFile{%H-});
    FinderDirs.Search(SDirName, AllFilesMask, false{SubDirs});
    L.Sort;

    for SItem in L do
    begin
      SItemShort:= ExtractFileName(SItem);
      if (SFileName='') or SBeginsWith(SItemShort, SFileName) then
      begin
        SRes:= APrefixDir+'|'+MaybeEscape(SItemShort);
        if AddDirSlash then
          SRes+= '/';
        AResult.Add(SRes);
      end;
    end;

    L.Clear;
    FinderFiles.FileAttribute:= faAnyFile and not (faHidden{%H-} or faSysFile{%H-});
    FinderFiles.Search(SDirName, AFileMask, false{SubDirs});
    L.Sort;

    for SItem in L do
    begin
      SItemShort:= MaybeEscape(ExtractFileName(SItem));
      if (SFileName='') or SBeginsWith(SItemShort, SFileName) then
        AResult.Add(APrefixFile+'|'+SItemShort);
    end;
  finally
    FreeAndNil(L);
    FreeAndNil(FinderDirs);
    FreeAndNil(FinderFiles);
  end;
end;

end.
