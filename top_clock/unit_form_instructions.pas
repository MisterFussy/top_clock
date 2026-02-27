unit unit_form_instructions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type

  { TFormInstructions }

  TFormInstructions = class(TForm)
    ButtonClose: TButton;
    LabelActionTitle: TLabel;
    LabelActions: TLabel;
    LabelInstructionTitle: TLabel;
    LabelInstructions: TLabel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  inherited;
  TForm(Sender).Caption := 'Instructions';
end;

end.

