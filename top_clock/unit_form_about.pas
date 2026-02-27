{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_about;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonClose: TButton;
    LabelAbout: TLabel;
    LabelLink: TLabel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelLinkClick(Sender: TObject);
    procedure LabelLinkMouseEnter(Sender: TObject);
    procedure LabelLinkMouseLeave(Sender: TObject);
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  inherited;

  TForm(Sender).Caption := 'About';

  LabelLink.Caption    := 'https://github.com/MisterFussy/top_clock';
  LabelLink.Cursor     := crHandPoint;
  LabelLink.Font.Color := clBlue;
  LabelLink.Font.Style := [fsUnderline];
end;


procedure TFormAbout.ButtonCloseClick(Sender: TObject);
begin
  inherited;
  Close();
end;


procedure TFormAbout.LabelLinkClick(Sender: TObject);
begin
 OpenURL(TLabel(Sender).Caption);
end;

procedure TFormAbout.LabelLinkMouseEnter(Sender: TObject);
begin
 TLabel(Sender).Font.Color := clRed;
end;

procedure TFormAbout.LabelLinkMouseLeave(Sender: TObject);
begin
 TLabel(Sender).Font.Color := clBlue;
end;

end.

