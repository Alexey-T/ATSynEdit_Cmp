{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_RenderHTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

procedure CanvasTextOutHTML(C: TCanvas; X, Y: integer; const Text: string);
function CanvasTextWidthHTML(C: TCanvas; const Text: string): integer;

implementation

const
  CapacityDelta = 40;

type
  TCharAtr = record
    AtrChar: Widechar;
    AtrBold,
    AtrItalic,
    AtrUnder,
    AtrStrike: boolean;
  end;

  TCharAtrArray = array of TCharAtr;

function AtrToFontStyles(const Atr: TCharAtr): TFontStyles;
begin
  Result:= [];
  if Atr.AtrBold then Include(Result, fsBold);
  if Atr.AtrItalic then Include(Result, fsItalic);
  if Atr.AtrUnder then Include(Result, fsUnderline);
  if Atr.AtrStrike then Include(Result, fsStrikeOut);
end;

function AtrSameStyles(const A1, A2: TCharAtr): boolean;
begin
  Result:=
    (A1.AtrBold=A2.AtrBold) and
    (A1.AtrItalic=A2.AtrItalic) and
    (A1.AtrUnder=A2.AtrUnder) and
    (A1.AtrStrike=A2.AtrStrike);
end;

procedure CalcAtrArray(const Text: string; out Atr: TCharAtrArray; out AtrLen: integer);
var
  SWide, STag: UnicodeString;
  NLen: integer;
  bBold, bItalic, bUnder, bStrike, bTagClosing: boolean;
  i, j: integer;
begin
  AtrLen:= 0;
  SetLength(Atr, 0);
  if Text='' then exit;

  SWide:= UTF8Decode(Text);
  SetLength(Atr, CapacityDelta);
  bBold:= false;
  bItalic:= false;
  bUnder:= false;
  bStrike:= false;
  bTagClosing:= false;
  NLen:= Length(SWide);

  i:= 0;
  repeat
    Inc(i);
    if i>NLen then Break;

    //tag is found
    if SWide[i]='<' then
    begin
      j:= i+1;
      while (j<=NLen) and (SWide[j]<>'>') do Inc(j);
      if j>NLen then Break;
      bTagClosing:= SWide[i+1]='/';
      if bTagClosing then
      begin
        STag:= Copy(SWide, i+2, j-i-2);
      end
      else
      begin
        STag:= Copy(SWide, i+1, j-i-1);
      end;

      case STag of
        'b':
          begin
            bBold:= not bTagClosing;
          end;
        'i':
          begin
            bItalic:= not bTagClosing;
          end;
        'u':
          begin
            bUnder:= not bTagClosing;
          end;
        's':
          begin
            bStrike:= not bTagClosing;
          end;
      end;
      i:= j;
    end
    else
    begin
      //text out of tags
      if AtrLen>=Length(Atr) then
        SetLength(Atr, Length(Atr)+CapacityDelta);
      Inc(AtrLen);
      with Atr[AtrLen-1] do
      begin
        AtrChar:= SWide[i];
        AtrBold:= bBold;
        AtrItalic:= bItalic;
        AtrUnder:= bUnder;
        AtrStrike:= bStrike;
      end;
    end;
  until false;
end;

procedure CanvasTextOutHTML(C: TCanvas; X, Y: integer; const Text: string);
var
  AtrLen: integer;
  Atr: TCharAtrArray;
  SFragment: UnicodeString;
  SFragmentA: string;
  ch: Widechar;
  i: integer;
begin
  CalcAtrArray(Text, Atr, AtrLen);
  if AtrLen=0 then exit;
  SFragment:= '';
  C.Brush.Style:= bsClear; //fix overlapping of fragments

  for i:= 0 to AtrLen-1 do
  begin
    ch:= Atr[i].AtrChar;
    if (i=0) or AtrSameStyles(Atr[i], Atr[i-1]) then
      SFragment+= ch
    else
    begin
      C.Font.Style:= AtrToFontStyles(Atr[i-1]);
      SFragmentA:= UTF8Encode(SFragment);
      C.TextOut(X, Y, SFragmentA);
      Inc(X, C.TextWidth(SFragmentA));
      SFragment:= ch;
    end;
  end;

  if SFragment<>'' then
  begin
    C.Font.Style:= AtrToFontStyles(Atr[AtrLen-1]);
    SFragmentA:= UTF8Encode(SFragment);
    C.TextOut(X, Y, SFragmentA);
  end;

  C.Font.Style:= [];
end;

function CanvasTextWidthHTML(C: TCanvas; const Text: string): integer;
var
  AtrLen: integer;
  Atr: TCharAtrArray;
  SFragment: UnicodeString;
  SFragmentA: string;
  ch: Widechar;
  i: integer;
begin
  Result:= 0;

  CalcAtrArray(Text, Atr, AtrLen);
  if AtrLen=0 then exit;
  SFragment:= '';

  for i:= 0 to AtrLen-1 do
  begin
    ch:= Atr[i].AtrChar;
    if (i=0) or AtrSameStyles(Atr[i], Atr[i-1]) then
      SFragment+= ch
    else
    begin
      C.Font.Style:= AtrToFontStyles(Atr[i-1]);
      SFragmentA:= UTF8Encode(SFragment);
      Inc(Result, C.TextWidth(SFragmentA));
      SFragment:= ch;
    end;
  end;

  if SFragment<>'' then
  begin
    C.Font.Style:= AtrToFontStyles(Atr[AtrLen-1]);
    SFragmentA:= UTF8Encode(SFragment);
    Inc(Result, C.TextWidth(SFragmentA));
  end;

  C.Font.Style:= [];
end;

end.

