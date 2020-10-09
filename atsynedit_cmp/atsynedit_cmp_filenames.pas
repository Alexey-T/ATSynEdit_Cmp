unit ATSynEdit_Cmp_Filenames;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

procedure EditorGetCompletionFilenames(const ACurDir, AText: string; L: TStringList);


implementation

procedure EditorGetCompletionFilenames(const ACurDir, AText: string; L: TStringList);
begin
  L.Clear;

  {
  L.Add('??1');
  L.Add('??2');
  L.Add('??3');
  }

end;


end.
