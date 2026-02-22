{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_options;

{$mode ObjFPC}{$H+}

interface

uses
  unit_options, Windows,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    ButtonLoadDefaults: TButton;
    ButtonDisplayColor: TButton;
    ButtonTextColor: TButton;
    CheckBoxShowSeconds: TCheckBox;
    ColorDialog1: TColorDialog;
    EditTimerSeconds: TEdit;
    RadioGroupRunMode: TRadioGroup;
    RadioGroupTimeFormat: TRadioGroup;
    procedure ButtonDisplayColorClick(Sender: TObject);
    procedure ButtonLoadDefaultsClick(Sender: TObject);
    procedure ButtonTextColorClick(Sender: TObject);
    procedure CheckBoxShowSecondsClick(Sender: TObject);
    procedure EditTimerSecondsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupRunModeClick(Sender: TObject);
    procedure RadioGroupTimeFormatClick(Sender: TObject);
  private

  public

  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.lfm}

{ TFormOptions }


procedure TFormOptions.FormShow(Sender: TObject);
begin
  if RadioGroupTimeFormat.Items.Count > 0 then
    RadioGroupTimeFormat.ItemIndex := Ord(AppOptions.TimeFormat);
  if RadioGroupRunMode.Items.Count > 0 then
    RadioGroupRunMode.ItemIndex    := Ord(AppOptions.RunMode);
  CheckBoxShowSeconds.Checked      := AppOptions.ShowSeconds;
  EditTimerSeconds.Text            := IntToStr(AppOptions.TimerSeconds);
end;


procedure TFormOptions.CheckBoxShowSecondsClick(Sender: TObject);
begin
  AppOptions.ShowSeconds := CheckBoxShowSeconds.Checked;
end;


procedure TFormOptions.ButtonDisplayColorClick(Sender: TObject);
begin
  ColorDialog1.Color := AppOptions.DisplayColor;
  if ColorDialog1.Execute then
    AppOptions.DisplayColor := ColorDialog1.Color;
end;

procedure TFormOptions.ButtonLoadDefaultsClick(Sender: TObject);
begin
  AppOptions                     := DefaultAppOptions;          // load defaults

  RadioGroupTimeFormat.ItemIndex := Ord(AppOptions.TimeFormat); // transfer to controls
  CheckBoxShowSeconds.Checked    := AppOptions.ShowSeconds;
  RadioGroupRunMode.ItemIndex    := Ord(AppOptions.RunMode);
  end;

procedure TFormOptions.ButtonTextColorClick(Sender: TObject);
begin
  ColorDialog1.Color := AppOptions.DisplayColor;
  if ColorDialog1.Execute then
    AppOptions.TextColor := ColorDialog1.Color;
end;

procedure TFormOptions.EditTimerSecondsClick(Sender: TObject);
begin
  AppOptions.TimerSeconds := StrToIntDef(EditTimerSeconds.Text,60);
end;

procedure TFormOptions.FormCreate(Sender: TObject);
var
  i  : integer;
begin
  inherited;
  // only enable first option (clock mode)
  // other options are experimental
  for i := 0 to RadioGroupRunMode.ControlCount - 1 do
    if RadioGroupRunMode.Controls[i] is TRadioButton then
    begin
      TRadioButton(RadioGroupRunMode.Controls[i]).Enabled := (i = 0);
    end;
end;

procedure TFormOptions.RadioGroupRunModeClick(Sender: TObject);
begin
  AppOptions.RunMode := TRunMode(RadioGroupRunMode.ItemIndex);
end;

procedure TFormOptions.RadioGroupTimeFormatClick(Sender: TObject);
begin
  inherited;
  if RadioGroupTimeFormat.ItemIndex <> -1 then
    AppOptions.TimeFormat := TTimeFormat(RadioGroupTimeFormat.ItemIndex);
  AppOptions.ShowSeconds := CheckBoxShowSeconds.Checked;
end;

end.

