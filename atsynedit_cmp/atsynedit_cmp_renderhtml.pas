{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Cmp_RenderHTML;

{$mode objfpc}{$H+}

interface

uses
  Graphics;

procedure CanvasTextOutHTML(C: TCanvas; X, Y: integer; const Text: string);
function CanvasTextWidthHTML(C: TCanvas; const Text: string): integer;

implementation

uses
  SysUtils, StrUtils;

//realloc dyn-array by Delta elements at once
const
  CapacityDelta = 40;

type
  TCharAtr = record
    AtrChar: Widechar;
    AtrBold,
    AtrItalic,
    AtrUnder,
    AtrStrike: boolean;
    AtrColor: TColor;
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

function AtrToFontColor(const Atr: TCharAtr; DefColor: TColor): TColor;
begin
  Result:= Atr.AtrColor;
  if Result=clNone then
    Result:= DefColor;
end;

function AtrSameStyles(const A1, A2: TCharAtr): boolean;
begin
  Result:=
    (A1.AtrBold=A2.AtrBold) and
    (A1.AtrItalic=A2.AtrItalic) and
    (A1.AtrUnder=A2.AtrUnder) and
    (A1.AtrStrike=A2.AtrStrike) and
    (A1.AtrColor=A2.AtrColor);
end;

function HexCodeToInt(N: integer): integer;
begin
  case N of
    ord('0')..ord('9'):
      Result:= N-Ord('0');
    ord('a')..ord('f'):
      Result:= N-Ord('a')+10;
    ord('A')..ord('F'):
      Result:= N-Ord('A')+10;
    else
      Result:= 0;
  end;
end;

function _IsHexDigitCode(N: integer): boolean;
begin
  case N of
    Ord('0')..Ord('9'),
    Ord('a')..Ord('f'),
    Ord('A')..Ord('F'):
      Result:= true;
    else
      Result:= false;
  end;
end;

function HtmlTokenToColor(const S: UnicodeString): TColor;
var
  NLen, i: integer;
  N1, N2, N3: integer;
begin
  Result:= clNone;
  NLen:= Length(S);

  for i:= 1 to NLen do
    if not _IsHexDigitCode(Ord(S[i])) then exit;

  case NLen of
    6, 8:
      begin
        N1:= HexCodeToInt(ord(S[1]))*16 + HexCodeToInt(ord(S[2]));
        N2:= HexCodeToInt(ord(S[3]))*16 + HexCodeToInt(ord(S[4]));
        N3:= HexCodeToInt(ord(S[5]))*16 + HexCodeToInt(ord(S[6]));
        Result:= RGBToColor(N1, N2, N3);
      end;
    3, 4:
      begin
        N1:= HexCodeToInt(ord(S[1]))*17;
        N2:= HexCodeToInt(ord(S[2]))*17;
        N3:= HexCodeToInt(ord(S[3]))*17;
        Result:= RGBToColor(N1, N2, N3);
      end;
  end;
end;

function _Unicode_StartsStr(const Sub, S: UnicodeString): boolean;
begin
  Result:= (Length(S)>=Length(Sub)) and (Copy(S, 1, Length(Sub))=Sub);
end;

function _Unicode_EndsStr(const Sub, S: UnicodeString): boolean;
begin
  Result:= (Length(S)>=Length(Sub)) and (Copy(S, Length(S)-Length(Sub)+1, Length(Sub))=Sub);
end;

procedure CalcAtrArray(const Text: string; out Atr: TCharAtrArray; out AtrLen: integer);
var
  SWide, STag: UnicodeString;
  NLen: integer;
  bBold, bItalic, bUnder, bStrike, bTagClosing, bTagKnown: boolean;
  NColor, NColorPrev: TColor;
  i, j, NCharPos: integer;
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
  NColor:= clNone;
  NColorPrev:= clNone;
  NLen:= Length(SWide);

  i:= 0;
  repeat
    Inc(i);
    if i>NLen then Break;

    //tag is found
    if SWide[i]='<' then
    begin
      bTagKnown:= false;
      bTagClosing:= false;
      STag:= '';
      j:= i+1;
      while (j<=NLen) and (SWide[j]<>'>') do Inc(j);
      if j<=NLen then
      begin
        bTagClosing:= SWide[i+1]='/';
        if bTagClosing then
          STag:= Copy(SWide, i+2, j-i-2)
        else
          STag:= Copy(SWide, i+1, j-i-1);
      end;

      if not bTagClosing and _Unicode_StartsStr('font color="#', STag) and _Unicode_EndsStr('"', STag) then
      begin
        bTagKnown:= true;
        NCharPos:= Pos('#', STag);
        STag:= Copy(STag, NCharPos+1, Length(STag)-NCharPos-1);
        NColorPrev:= NColor;
        NColor:= HtmlTokenToColor(STag);
      end
      else
      if bTagClosing and (STag='font') then
      begin
        bTagKnown:= true;
        NColor:= NColorPrev;
      end
      else
      case STag of
        'b':
          begin
            bTagKnown:= true;
            bBold:= not bTagClosing;
          end;
        'i':
          begin
            bTagKnown:= true;
            bItalic:= not bTagClosing;
          end;
        'u':
          begin
            bTagKnown:= true;
            bUnder:= not bTagClosing;
          end;
        's':
          begin
            bTagKnown:= true;
            bStrike:= not bTagClosing;
          end;
      end;

      if bTagKnown then
      begin
        i:= j;
        Continue;
      end;
    end;

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
      AtrColor:= NColor;
    end;
  until false;
end;

procedure CanvasTextOutHTML(C: TCanvas; X, Y: integer; const Text: string);
var
  AtrLen: integer;
  Atr: TCharAtrArray;
  SFragment: UnicodeString;
  SFragmentA: string;
  NDefColor: TColor;
  ch: Widechar;
  i: integer;
begin
  CalcAtrArray(Text, Atr, AtrLen);
  if AtrLen=0 then exit;
  SFragment:= '';
  C.Brush.Style:= bsClear; //fix overlapping of fragments
  NDefColor:= C.Font.Color;

  for i:= 0 to AtrLen-1 do
  begin
    ch:= Atr[i].AtrChar;
    if (i=0) or AtrSameStyles(Atr[i], Atr[i-1]) then
      SFragment+= ch
    else
    begin
      C.Font.Style:= AtrToFontStyles(Atr[i-1]);
      C.Font.Color:= AtrToFontColor(Atr[i-1], NDefColor);
      SFragmentA:= UTF8Encode(SFragment);
      C.TextOut(X, Y, SFragmentA);
      Inc(X, C.TextWidth(SFragmentA));
      SFragment:= ch;
    end;
  end;

  if SFragment<>'' then
  begin
    C.Font.Style:= AtrToFontStyles(Atr[AtrLen-1]);
    C.Font.Color:= AtrToFontColor(Atr[AtrLen-1], NDefColor);
    SFragmentA:= UTF8Encode(SFragment);
    C.TextOut(X, Y, SFragmentA);
  end;

  C.Font.Style:= [];
  C.Font.Color:= NDefColor;
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

