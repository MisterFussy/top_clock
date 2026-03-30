{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_options;

{$mode ObjFPC}{$H+}

interface

uses
  unit_options, Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Spin, EditBtn, ComCtrls, Types;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    ButtonLoadDefaults: TButton;
    ButtonDisplayColor: TButton;
    ButtonTextColor: TButton;
    CheckBoxShowSeconds: TCheckBox;
    ColorDialog1: TColorDialog;
    EditSS: TEdit;
    EditMM: TEdit;
    EditHH: TEdit;
    GroupBoxHideTime: TGroupBox;
    GroupBoxControl: TGroupBox;
    GroupBoxAppearance: TGroupBox;
    RadioGroupRunMode: TRadioGroup;
    RadioGroupTimeFormat: TRadioGroup;
    UpDownHH: TUpDown;
    UpDownMM: TUpDown;
    UpDownSS: TUpDown;
    procedure ButtonDisplayColorClick(Sender: TObject);
    procedure ButtonLoadDefaultsClick(Sender: TObject);
    procedure ButtonTextColorClick(Sender: TObject);
    procedure CheckBoxShowSecondsClick(Sender: TObject);
    procedure EditHHEditingDone(Sender: TObject);
    procedure EditHHMMSSChange(Sender: TObject);
    procedure EditMMEditingDone(Sender: TObject);
    procedure EditSSEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupRunModeClick(Sender: TObject);
    procedure RadioGroupTimeFormatClick(Sender: TObject);
    procedure UpDownHHClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownMMClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownSSClick(Sender: TObject; Button: TUDBtnType);
  private

  public

  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.lfm}

{ TFormOptions }


procedure TFormOptions.FormShow(Sender: TObject);
var
  H : integer;
  M : integer;
  S : integer;
begin
  if RadioGroupTimeFormat.Items.Count > 0 then
    RadioGroupTimeFormat.ItemIndex := Ord(AppOptions.TimeFormat);
  if RadioGroupRunMode.Items.Count > 0 then
    RadioGroupRunMode.ItemIndex    := Ord(AppOptions.RunMode);
  CheckBoxShowSeconds.Checked      := AppOptions.ShowSeconds;
  H                                := AppOptions.HideSeconds div 3600;
  M                                := (AppOptions.HideSeconds mod 3600) div 60;
  S                                := (AppOptions.HideSeconds mod 3600) mod 60;
  EditHH.Text                      := Format('%.2d', [H]); // Format as 2 digits
  EditMM.Text                      := Format('%.2d', [M]); // Format as 2 digits
  EditSS.Text                      := Format('%.2d', [S]); // Format as 2 digits
end;


procedure TFormOptions.CheckBoxShowSecondsClick(Sender: TObject);
begin
  AppOptions.ShowSeconds := CheckBoxShowSeconds.Checked;
end;


procedure TFormOptions.EditHHEditingDone(Sender: TObject);
var
  H : Integer;
begin
  H := StrToIntDef(EditHH.Text, 0);
  if H > 24 then H := 24;  // clamp to valid range
  if H < 0  then H := 0;
  EditHH.Text := Format('%.2d', [H]);
end;


procedure TFormOptions.EditHHMMSSChange(Sender: TObject);
begin
  AppOptions.HideSeconds :=
    StrToIntDef(editHH.Text, 0) * 3600 +
    StrToIntDef(editMM.Text, 0) *   60 +
    StrToIntDef(editSS.Text, DefaultAppOptions.HideSeconds);
end;


procedure TFormOptions.EditMMEditingDone(Sender: TObject);
var
  M : Integer;
begin
  M := StrToIntDef(EditMM.Text, 0);
  if M > 59 then M := 59;  // clamp to valid range
  if M < 0  then M := 0;
  EditMM.Text := Format('%.2d', [M]);
end;


procedure TFormOptions.EditSSEditingDone(Sender: TObject);
var
  S : Integer;
begin
  S := StrToIntDef(EditSS.Text, AppOptions.HideSeconds);
  if S > 59 then S := 59;  // clamp to valid range
  if S < 0  then S := 0;
  EditSS.Text := Format('%.2d', [S]); // Format as 2 digits
end;


procedure TFormOptions.ButtonDisplayColorClick(Sender: TObject);
begin
  ColorDialog1.Color := AppOptions.DisplayColor;
  if ColorDialog1.Execute then
    AppOptions.DisplayColor := ColorDialog1.Color;
end;


procedure TFormOptions.ButtonLoadDefaultsClick(Sender: TObject);
var
  H : integer;
  M : integer;
  S : integer;
begin
  AppOptions                     := DefaultAppOptions;          // load defaults
  RadioGroupTimeFormat.ItemIndex := Ord(AppOptions.TimeFormat); // transfer to controls
  CheckBoxShowSeconds.Checked    := AppOptions.ShowSeconds;
  RadioGroupRunMode.ItemIndex    := Ord(AppOptions.RunMode);
  H                              := AppOptions.HideSeconds div 3600;
  M                              := (AppOptions.HideSeconds mod 3600) div 60;
  S                              := (AppOptions.HideSeconds mod 3600) mod 60;
  EditHH.Text                    := Format('%.2d', [H]); // Format as 2 digits
  EditMM.Text                    := Format('%.2d', [M]); // Format as 2 digits
  EditSS.Text                    := Format('%.2d', [S]); // Format as 2 digits
end;


procedure TFormOptions.ButtonTextColorClick(Sender: TObject);
begin
  ColorDialog1.Color     := AppOptions.DisplayColor;
  if ColorDialog1.Execute then
    AppOptions.TextColor := ColorDialog1.Color;
end;


procedure TFormOptions.FormCreate(Sender: TObject);
begin
  Caption                     := 'Options';
  CheckBoxShowSeconds.Checked := AppOptions.ShowSeconds;

  BorderStyle                 := bsDialog;
  FormStyle                   := fsSystemStayOnTop; // stay on top
end;


procedure TFormOptions.FormDblClick(Sender: TObject);
begin
  Close;
end;


procedure TFormOptions.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    begin
      ReleaseCapture;
      SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
    end;
end;


procedure TFormOptions.RadioGroupRunModeClick(Sender: TObject);
begin
  AppOptions.RunMode             := TRunMode(RadioGroupRunMode.ItemIndex);
  if RadioGroupRunMode.ItemIndex = 0 then
    RadioGroupTimeFormat.Enabled := True
  else
    RadioGroupTimeFormat.Enabled := False; // disable Time Format buttons if not in clock mode
end;


procedure TFormOptions.RadioGroupTimeFormatClick(Sender: TObject);
begin
  if RadioGroupTimeFormat.ItemIndex <> -1 then
    AppOptions.TimeFormat := TTimeFormat(RadioGroupTimeFormat.ItemIndex);

  AppOptions.ShowSeconds  := CheckBoxShowSeconds.Checked;
end;

procedure TFormOptions.UpDownHHClick(Sender: TObject; Button: TUDBtnType);
var
  H : integer;
begin
  H := StrToIntDef(EditHH.Text, 0);
  case Button of
    btNext : Inc(H); // up arrow
    btPrev : Dec(H); // down arror
  end;
  if H > 24 then     // rollover/under
    H := 0
  else if H < 0 then
    H := 24;
  EditHH.Text := Format('%.2d', [H]); // Format as 2 digits
end;

procedure TFormOptions.UpDownMMClick(Sender: TObject; Button: TUDBtnType);
var
  M : Integer;
begin
  M := StrToIntDef(EditMM.Text, 0);
  case Button of
    btNext : Inc(M); // up arrow
    btPrev : Dec(M); // down arror
  end;
  if M > 59 then     // rollover/under
    M := 0
  else if M < 0 then
    M := 59;
  EditMM.Text := Format('%.2d', [M]); // Format as 2 digits
end;

procedure TFormOptions.UpDownSSClick(Sender: TObject; Button: TUDBtnType);
var
  S : Integer;
begin
  S := StrToIntDef(EditSS.Text, AppOptions.HideSeconds);
  case Button of
    btNext : Inc(S); // up arrow
    btPrev : Dec(S); // down arror
  end;
  if S > 59 then     // rollover/under
    S := 0
  else if S < 0 then
    S := 59;
  EditSS.Text := Format('%.2d', [S]); // Format as 2 digits
end;


end.

