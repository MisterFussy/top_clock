{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_timerexpired;

{$mode ObjFPC}{$H+}

interface

uses
  Windows,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormTimerExpired }

  TFormTimerExpired = class(TForm)
    LabelTimerExpired: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormTimerExpired: TFormTimerExpired;

implementation

{$R *.lfm}

{ TFormTimerExpired }

procedure TFormTimerExpired.FormCreate(Sender: TObject);
begin
  Caption     := 'Timer Expired';
  BorderStyle := bsDialog;
  FormStyle   := fsSystemStayOnTop; // stay on top
end;


procedure TFormTimerExpired.FormDblClick(Sender: TObject);
begin
  Close;
end;


procedure TFormTimerExpired.FormShow(Sender: TObject);
begin
  MessageBeep(MB_ICONINFORMATION); // info
end;

end.

