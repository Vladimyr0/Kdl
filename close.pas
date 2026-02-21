unit close;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TCloseForm }

  TCloseForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click (Sender: TObject);
    procedure FormClose (Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CloseForm: TCloseForm;

implementation

uses kdar;

{$R *.lfm}

{ TCloseForm }

procedure TCloseForm.Button1Click (Sender: TObject);
begin
  Tag := 0;
  if OnTop then begin
     MainForm.FormStyle := fsSystemStayOnTop;
     CloseForm.FormStyle := fsStayOnTop;
  end;
  CloseForm.Close;
end;

procedure TCloseForm.FormClose (Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  if Tag = 1 then MainForm.Close else begin
    MainForm.AlphaBlendValue := 255;
    MainForm.AlphaBlend := false;
  end;
end;

procedure TCloseForm.FormKeyDown (Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Button1.CLick;
end;

end.

