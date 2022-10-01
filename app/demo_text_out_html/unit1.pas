unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    procedure Edit1Change(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
  TCanvasCharAttr = record
    AtrChar: Widechar;
    AtrBold,
    AtrItalic,
    AtrUnder: boolean;
  end;

procedure CanvasTextOutWithHTML(C: TCanvas; X, Y: integer; const Text: string);
  //
  function AtrToFontStyles(var Atr: TCanvasCharAttr): TFontStyles;
  begin
    Result:= [];
    if Atr.AtrBold then Include(Result, fsBold);
    if Atr.AtrItalic then Include(Result, fsItalic);
    if Atr.AtrUnder then Include(Result, fsUnderline);
  end;
  //
  function AtrSameStyles(var A1, A2: TCanvasCharAttr): boolean;
  begin
    Result:=
      (A1.AtrBold=A2.AtrBold) and
      (A1.AtrItalic=A2.AtrItalic) and
      (A1.AtrUnder=A2.AtrUnder);
  end;
  //
const
  CapacityDelta = 40;
var
  Atr: array of TCanvasCharAttr;
  AtrLen: integer;
  SWide, STag, SFragment: UnicodeString;
  ch: Widechar;
  NLen: integer;
  bBold, bItalic, bUnder, bTagClosing: boolean;
  PosX: integer;
  i, j: integer;
begin
  if Text='' then exit;
  SWide:= UTF8Decode(Text);
  AtrLen:= 0;
  SetLength(Atr, CapacityDelta);
  bBold:= false;
  bItalic:= false;
  bUnder:= false;
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
      end;
    end;
  until false;

  if AtrLen=0 then exit;
  PosX:= X;
  SFragment:= '';

  for i:= 0 to AtrLen-1 do
  begin
    ch:= Atr[i].AtrChar;
    if (i=0) or AtrSameStyles(Atr[i], Atr[i-1]) then
      SFragment+= ch
    else
    begin
      C.Font.Style:= AtrToFontStyles(Atr[i-1]);
      C.TextOut(PosX, Y, SFragment);
      Inc(PosX, C.TextWidth(SFragment));
      SFragment:= ch;
    end;
  end;

  C.Font.Style:= AtrToFontStyles(Atr[AtrLen-1]);
  C.TextOut(PosX, Y, SFragment);
end;

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Font.Name:= 'default';
  Canvas.Font.Size:= 12;
  CanvasTextOutWithHTML(Canvas, 20, 80, Edit1.Text);
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Invalidate;
end;

end.

