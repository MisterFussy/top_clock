{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_instructions;

{$mode ObjFPC}{$H+}

interface

uses
  unit_options,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type

  { TFormInstructions }

  TFormInstructions = class(TForm)
    GroupBoxTimerMode: TGroupBox;
    GroupBoxGeneral: TGroupBox;
    LabelActionsTimerMode: TLabel;
    LabelActionTitle: TLabel;
    LabelActions: TLabel;
    LabelActionTitleTimerMode: TLabel;
    LabelInstructionsMode: TLabel;
    LabelInstructionTitle: TLabel;
    LabelInstructions: TLabel;
    LabelInstructionTitleTimerMode: TLabel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
  private

  public

  end;

var
  FormInstructions: TFormInstructions;

implementation

{$R *.lfm}

{ TFormInstructions }


procedure TFormInstructions.ButtonCloseClick(Sender: TObject);
begin
  inherited;
  Close();
end;


procedure TFormInstructions.FormCreate(Sender: TObject);
begin
  Caption                             := 'Instructions';
  BorderStyle                         := bsDialog;
  FormStyle                           := fsSystemStayOnTop; // stay on top

  LabelActionTitle.Alignment          := taRightJustify;
  LabelActionTitle.AutoSize           := False;
  LabelActions.Alignment              := taRightJustify;
  LabelActions.AutoSize               := False;

  LabelActionTitleTimerMode.Alignment := taRightJustify;
  LabelActionTitleTimerMode.AutoSize  := False;
  LabelActionsTimerMode.Alignment     := taRightJustify;
  LabelActionsTimerMode.AutoSize      := False;

  (* I don't like this here, but this is a way to put the real value
     of HideSeconds in the instruction. Note, for the GUI part I use XXX,
     so the width of the TLabel it comes in is wide enough.
  *)
  LabelActions.Caption := Format(
    'move clock%sfade in/out%spopup menu%shide for %.2d sec%sturn off clock',
    [LineEnding, LineEnding, LineEnding, AppOptions.HideSeconds, LineEnding]
  );
end;


procedure TFormInstructions.FormDblClick(Sender: TObject);
begin
  Close;
end;


end.

