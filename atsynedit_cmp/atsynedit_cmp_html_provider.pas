{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_HTML_Provider;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TATHtmlProvider = class abstract
  public
    procedure GetTags(L: TStringList); virtual; abstract;
    procedure GetTagProps(const ATag: string; L: TStringList); virtual; abstract;
    procedure GetTagPropValues(const ATag, AProp: string; L: TStringList); virtual; abstract;
  end;

  TATHtmlBasicProvider = class(TATHtmlProvider)
  private
    ListAll: TStringList;
    ListGlobals: TStringList;
    ListMediaTypes: TStringList;
  public
    constructor Create(const AFilenameList, AFilenameGlobals, AFilenameMediaTypes: string);
    destructor Destroy; override;
    procedure GetTags(L: TStringList); override;
    procedure GetTagProps(const ATag: string; L: TStringList); override;
    procedure GetTagPropValues(const ATag, AProp: string; L: TStringList); override;
  end;


implementation

uses
  ATStringProc,
  ATStringProc_Separator;

function IsTagWithMimeType(const ATag: string): boolean;
begin
  case ATag of
    'a',
    'embed',
    'link',
    'object',
    'script',
    'source',
    'style':
      Result:= true;
    else
      Result:= false;
  end;
end;

procedure StripEmptyFromList(L: TStringList);
var
  i: integer;
begin
  for i:= L.Count-1 downto 0 do
    if L[i]='' then
      L.Delete(i);
end;

{ TATHtmlBasicProvider }

constructor TATHtmlBasicProvider.Create(const AFilenameList, AFilenameGlobals, AFilenameMediaTypes: string);
begin
  ListAll:= TStringList.Create;
  ListGlobals:= TStringList.Create;
  ListMediaTypes:= TStringList.Create;

  if FileExists(AFilenameList) then
  begin
    ListAll.LoadFromFile(AFilenameList);
    StripEmptyFromList(ListAll);
  end;

  if FileExists(AFilenameGlobals) then
  begin
    ListGlobals.LoadFromFile(AFilenameGlobals);
    StripEmptyFromList(ListGlobals);
  end;

  if FileExists(AFilenameMediaTypes) then
  begin
    ListMediaTypes.LoadFromFile(AFilenameMediaTypes);
    StripEmptyFromList(ListMediaTypes);
  end;
end;

destructor TATHtmlBasicProvider.Destroy;
begin
  FreeAndNil(ListMediaTypes);
  FreeAndNil(ListGlobals);
  FreeAndNil(ListAll);
  inherited Destroy;
end;

procedure TATHtmlBasicProvider.GetTags(L: TStringList);
var
  S, SKey, SVal: string;
begin
  L.Clear;
  L.Sorted:= true;

  for S in ListAll do
  begin
    SSplitByChar(S, '=', SKey, SVal);
    L.Add(SKey);
  end;
end;

procedure TATHtmlBasicProvider.GetTagProps(const ATag: string; L: TStringList);
var
  S, SKey, SVal, SItem: string;
  Sep: TATStringSeparator;
begin
  L.Clear;
  L.Sorted:= true;

  for S in ListGlobals do
  begin
    SSplitByChar(S, '<', SKey, SVal);
    L.Add(SKey);
  end;

  for S in ListAll do
  begin
    SSplitByChar(S, '=', SKey, SVal);
    if SameText(SKey, ATag) then
    begin
      Sep.Init(SVal, '|');
      while Sep.GetItemStr(SItem) do
      begin
        SItem:= SGetItem(SItem, '<');
        L.Add(SItem);
      end;
      Break;
    end;
  end;
end;

procedure TATHtmlBasicProvider.GetTagPropValues(const ATag, AProp: string; L: TStringList);
  //
  function AddFromData(const AData: string): boolean;
  var
    SKey, SVal, SItem: string;
    Sep: TATStringSeparator;
  begin
    Result:= false;
    SSplitByChar(AData, '<', SKey, SVal);
    if SameText(AProp, SKey) then
    begin
      Sep.Init(SVal, '?');
      while Sep.GetItemStr(SItem) do
        L.Add(SItem);
      exit(true);
    end;
  end;
  //
var
  SRoot, SRootKey, SRootVal, SItem: string;
  Sep: TATStringSeparator;
begin
  L.Clear;
  L.Sorted:= true;

  if (AProp='type') and IsTagWithMimeType(ATag) then
  begin
    L.AddStrings(ListMediaTypes);
    exit;
  end;

  for SItem in ListGlobals do
    if AddFromData(SItem) then
      exit;

  for SRoot in ListAll do
  begin
    SSplitByChar(SRoot, '=', SRootKey, SRootVal);
    if SameText(SRootKey, ATag) then
    begin
      Sep.Init(SRootVal, '|');
      while Sep.GetItemStr(SItem) do
        if AddFromData(SItem) then Break;
      Break;
    end;
  end;
end;

end.

