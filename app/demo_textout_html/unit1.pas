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

uses
  ATSynEdit_Cmp_RenderHTML;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Font.Name:= 'default';
  Canvas.Font.Size:= 12;
  Canvas.Brush.Style:= bsClear;
  CanvasTextOutWithHTML(Canvas, 20, 80, Edit1.Text);
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Invalidate;
end;

end.

