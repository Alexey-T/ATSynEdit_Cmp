unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ATSynEdit,
  ATSynEdit_Carets,
  ATSynEdit_Commands,
  ATSynEdit_Cmp_Form;

type

  { TForm1 }

  TForm1 = class(TForm)
    Ed: TATSynEdit;
    Panel1: TPanel;
    procedure EdCommand(Sender: TObject; ACommand: integer;
      AInvoke: TATCommandInvoke; const AText: string;
      var AHandled: boolean);
    procedure EdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure DoCompletionProp(Sender: TObject;
      AContent: TStringList; out ACharsLeft, ACharsRight: integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  cmd_AutoComplete = 3000;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  Ed.Strings.LineAdd('Some text here, press Ctrl+Space');
  Ed.DoCaretSingle(14, 0);
  Ed.Update(true);
end;

procedure TForm1.DoCompletionProp(Sender: TObject; AContent: TStringList; out
  ACharsLeft, ACharsRight: integer);
const
  cNonWordChars = '-+*=/\()[]{}<>"''.,:;~?!@#$%^&|`…';
var
  Caret: TATCaretItem;
  SWord: UnicodeString;
begin
  if Ed.Carets.Count<>1 then
  begin
    ShowMessage('Completion cannot handle multi-carets');
    exit;
  end;
  Caret:= Ed.Carets[0];
  EditorGetCurrentWord(Ed,
    Caret.PosX, Caret.PosY,
    cNonWordChars,
    SWord,
    ACharsLeft,
    ACharsRight);

  AContent.Clear;
  AContent.Add('func|SomeFunc|(param1, param2)'#9'Function description');
  AContent.Add('var|SomeId1|'#9'Description one');
  AContent.Add('var|AnotherId2|'#9'Description two');
end;

procedure TForm1.EdCommand(Sender: TObject; ACommand: integer;
  AInvoke: TATCommandInvoke; const AText: string; var AHandled: boolean);
begin
  if ACommand=cmd_AutoComplete then
  begin
    AHandled:= true;
    EditorShowCompletionListbox(Ed, @DoCompletionProp);
  end;
end;

procedure TForm1.EdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=Ord(' ')) and (Shift=[ssCtrl]) then
    Ed.DoCommand(cmd_AutoComplete, TATCommandInvoke.AppInternal);
end;

end.

