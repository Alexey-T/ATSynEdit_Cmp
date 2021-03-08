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
    ListEvents: TStringList;
  public
    constructor Create(const AFilenameList, AFilenameEvents: string);
    destructor Destroy; override;
    procedure GetTags(L: TStringList); override;
    procedure GetTagProps(const ATag: string; L: TStringList); override;
    procedure GetTagPropValues(const ATag, AProp: string; L: TStringList); override;
  end;


implementation

uses
  ATStringProc,
  ATStringProc_Separator;

{ TATHtmlBasicProvider }

constructor TATHtmlBasicProvider.Create(const AFilenameList, AFilenameEvents: string);
var
  i: integer;
begin
  ListAll:= TStringList.Create;
  ListEvents:= TStringList.Create;

  if FileExists(AFilenameList) then
    ListAll.LoadFromFile(AFilenameList);
  if FileExists(AFilenameEvents) then
    ListEvents.LoadFromFile(AFilenameEvents);

  for i:= ListAll.Count-1 downto 0 do
    if ListAll[i]='' then
      ListAll.Delete(i);

  for i:= ListEvents.Count-1 downto 0 do
    if ListEvents[i]='' then
      ListEvents.Delete(i);
end;

destructor TATHtmlBasicProvider.Destroy;
begin
  FreeAndNil(ListEvents);
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

  L.Add('class');
  L.Add('id');

  for S in ListAll do
  begin
    SSplitByChar(S, '=', SKey, SVal);
    if SameText(SKey, ATag) then
    begin
      Sep.Init(SVal, '|');
      while Sep.GetItemStr(SItem) do
      begin
        SItem:= SGetItem(SItem, '<');
        if SItem='$e' then
          L.AddStrings(ListEvents)
        else
          L.Add(SItem);
      end;
      Break;
    end;
  end;
end;

procedure TATHtmlBasicProvider.GetTagPropValues(const ATag, AProp: string; L: TStringList);
var
  SRoot, SRootKey, SRootVal, SItem, SItem2, SItemBegin, SItemEnd: string;
  Sep, Sep2: TATStringSeparator;
begin
  L.Clear;
  L.Sorted:= true;

  if SameText(AProp, 'dir') then
  begin
    L.Add('ltr');
    L.Add('rtl');
    L.Add('auto');
    exit;
  end;

  for SRoot in ListAll do
  begin
    SSplitByChar(SRoot, '=', SRootKey, SRootVal);
    if SameText(SRootKey, ATag) then
    begin
      Sep.Init(SRootVal, '|');
      while Sep.GetItemStr(SItem) do
      begin
        SSplitByChar(SItem, '<', SItemBegin, SItemEnd);
        if SameText(SItemBegin, AProp) then
        begin
          Sep2.Init(SItemEnd, '?');
          while Sep2.GetItemStr(SItem2) do
            L.Add(SItem2);
          Break;
        end;
      end;
      Break;
    end;
  end;
end;

end.

