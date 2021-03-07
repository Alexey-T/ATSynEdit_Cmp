{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_CSS_Provider;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TATCssProvider = class abstract
  public
    procedure GetProps(L: TStringList); virtual; abstract;
    procedure GetValues(const AProp: string; L: TStringList); virtual; abstract;
  end;

type
  { TATCssBasicProvider }

  TATCssBasicProvider = class(TATCssProvider)
  private
    ListProps: TStringList;
    ListColors: TStringList;
  public
    constructor Create(const AFilenameProps, AFilenameColors: string);
    destructor Destroy; override;
    procedure GetProps(L: TStringList); override;
    procedure GetValues(const AProp: string; L: TStringList); override;
  end;

implementation

uses
  ATStringProc,
  ATStringProc_Separator;

{ TATCssBasicProvider }

constructor TATCssBasicProvider.Create(const AFilenameProps, AFilenameColors: string);
var
  i: integer;
begin
  ListProps:= TStringList.Create;
  ListColors:= TStringList.Create;

  if FileExists(AFilenameProps) then
    ListProps.LoadFromFile(AFilenameProps);
  if FileExists(AFilenameColors) then
    ListColors.LoadFromFile(AFilenameColors);

  for i:= ListProps.Count-1 downto 0 do
    if ListProps[i]='' then
      ListProps.Delete(i);

  for i:= ListColors.Count-1 downto 0 do
    if ListColors[i]='' then
      ListColors.Delete(i);
end;

destructor TATCssBasicProvider.Destroy;
begin
  FreeAndNil(ListColors);
  FreeAndNil(ListProps);
  inherited Destroy;
end;

procedure TATCssBasicProvider.GetProps(L: TStringList);
var
  S, SKey, SVal: string;
begin
  L.Clear;
  for S in ListProps do
  begin
    SSplitByChar(S, '=', SKey, SVal);
    L.Add(SKey);
  end;
end;

procedure TATCssBasicProvider.GetValues(const AProp: string; L: TStringList);
var
  S, SKey, SVal, SItem: string;
  Sep: TATStringSeparator;
  N: integer;
begin
  L.Clear;

  //add values specific to AProp
  for S in ListProps do
  begin
    SSplitByChar(S, '=', SKey, SVal);
    if SameText(AProp, SKey) then
    begin
      Sep.Init(SVal, ',');
      while Sep.GetItemStr(SItem) do
        L.Add(SItem);
      Break;
    end;
  end;

  //auto add 'color' values
  N:= L.IndexOf('$c');
  if N>=0 then
  begin
    L.Delete(N);
    L.AddStrings(ListColors);
  end
  else
  if SEndsWith(AProp, '-background') or SEndsWith(AProp, '-color') then
    L.AddStrings(ListColors);

  //add common values
  L.Add('inherit');
  L.Add('initial');
  L.Add('unset');
  L.Add('var()');
end;

end.

