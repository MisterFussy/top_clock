{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_main;

{$mode objfpc}{$H+}

interface

uses
  unit_options, unit_form_placer, unit_form_options,
  unit_form_instructions, unit_form_about,
  StdCtrls, ExtCtrls, LCLIntf, LCLType, Windows,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    MenuItemOptions: TMenuItem;
    MenuItemFader: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemInstruction: TMenuItem;
    MenuItemAbout: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    TimerHide: TTimer;
    TimerFader: TTimer;
    TimerSecond: TTimer;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemOptionsClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemInstructionClick(Sender: TObject);
    procedure TimerFaderTimer(Sender: TObject);
    procedure TimerHideTimer(Sender: TObject);
    procedure TimerSecondTimer(Sender: TObject);
  private
    OptionsForm: TFormOptions;
    function FormatTimeString: string;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  public
    FadeCountDown : boolean;
  end;

var
  FormMain: TFormMain;

var
  StopwatchStart : TDateTime;
  TimerRemaining : Integer;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormChangeBounds(Sender: TObject);
var
  Region : HRGN;
  Radius : integer;
begin
  inherited;
  // round corners of form
  // see TFormMain.WMNCHitTest() for use of same equation
  Radius := Round((Width + Height) / 8);
  Region := CreateRoundRectRgn(0, 0, Width, Height, Radius, Radius);
  SetWindowRgn(Handle, Region, True);
end;


procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  SaveOptions(Self);
end;


procedure TFormMain.FormCreate(Sender: TObject);
begin
  AppOptions            := DefaultAppOptions; // load defaults for all
  LoadOptions(Self);                          // .ini file over-rides

  StopwatchStart        := Now;
  TimerRemaining        := AppOptions.TimerSeconds;

  TimerSecond.Interval  := 1000; // 1 second
  TimerSecond.Enabled   := True;

  TimerHide.Interval    := 2000; // 2 seconds, hide window time
  TimerHide.Enabled     := False;

  BorderIcons           := [];     // disable all borders
  BorderStyle           := bsNone; // disable caption bar and borders
  FormStyle             := fsSystemStayOnTop;
  Color                 := AppOptions.DisplayColor;

  // make form partially transparent
  AlphaBlend            := True;
  AlphaBlendValue       := 130;

  // limit the minimum size
  Constraints.MinWidth  := 60;
  Constraints.MinHeight := 20;

end;


procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Mouse wheel -> adjust AlphaBlendValue
  if WheelDelta > 0 then
    AlphaBlendValue := Min(255, AlphaBlendValue + 10)
  else
    AlphaBlendValue := Max(20, AlphaBlendValue - 10);

  Handled := True;
end;


// Using a TLabel is too difficult to size & center properly, hence
// the form is painted below - time added with Canvas.TextOut()
procedure TFormMain.FormPaint(Sender: TObject);
var
  ScreenText   : string;
  TargetWidth  ,
  TargetHeight : Integer;
  FontSize     : Integer;
  Margin       : Integer;
begin
  Color              := AppOptions.DisplayColor;
  ScreenText         := FormatTimeString; // current time formatted
  Margin             := 1;
  TargetWidth        := ClientWidth  - Margin;
  TargetHeight       := ClientHeight - Margin;
  FontSize           := 8;                 // starting font size
  Canvas.Font.Color  := AppOptions.TextColor;

  // increase font size until text is almost as wide as the form
  Canvas.Font.Size   := FontSize;
  while (Canvas.TextWidth(ScreenText) < TargetWidth) and
    (Canvas.TextHeight(ScreenText) < TargetHeight) do
  begin
    Inc(FontSize);
    Canvas.Font.Size := FontSize;
  end;

  // if we overshot, step back one
  if Canvas.TextWidth(ScreenText) > TargetWidth then
  begin
    Dec(FontSize);
    Canvas.Font.Size := FontSize;
  end;

  // draw centered vertically and horizontally
  Canvas.TextOut(
    (ClientWidth  - Canvas.TextWidth (ScreenText)) div 2,
    (ClientHeight - Canvas.TextHeight(ScreenText)) div 2,
    ScreenText
  );
end;


procedure TFormMain.FormDblClick(Sender: TObject);
begin
  Close;
end;


procedure TFormMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    begin
      if ssShift in Shift then
        begin
          // hide form, start timer
          Visible           := False;
          TimerHide.Enabled := True;
        end
      else
        begin
          ReleaseCapture;
          SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
        end;
    end;
end;


procedure TFormMain.MenuItemOptionsClick(Sender: TObject);
begin
  if not Assigned(FormOptions) then
    FormOptions := TFormOptions.Create(Self);

  PlaceFormRelative(FormMain, FormOptions, psBelowThenAboveThenCenter, 8);
  FormOptions.Show;
end;


procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  if not Assigned(FormAbout) then
    FormAbout := TFormAbout.Create(Self);

  PlaceFormRelative(FormMain, FormAbout, psBelowThenAboveThenCenter, 8);
  FormAbout.Show;
end;


procedure TFormMain.MenuItemCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TFormMain.MenuItemInstructionClick(Sender: TObject);
begin
//  MessageDlg('Instructions:' + LineEnding + LineEnding +
//             'Left Mouse Down Drag    '#9' - move clock' + LineEnding +
//             'Left Mouse Double Click '#9' - turn off clock' + LineEnding +
//             'Mouse Wheel Up/Down     '#9' - fade in/out' + LineEnding +
//             'Right Mouse Click       '#9' - popup menu' + LineEnding +
//             'Shift+Left Mouse Click  '#9' - hide for 2 sec' + LineEnding +
//             LineEnding,
//             mtInformation, [mbOK], 0);

  if not Assigned(FormInstructions) then
    FormInstructions := TFormInstructions.Create(Self);

  PlaceFormRelative(FormMain, FormInstructions, psBelowThenAboveThenCenter, 8);
  FormInstructions.Show;


end;

procedure TFormMain.TimerFaderTimer(Sender: TObject);
const ALPHA_DELTA : integer = 0; // set to 10 and see the app fade in and out
begin
  if FadeCountDown then
    AlphaBlendValue := AlphaBlendValue - ALPHA_DELTA
  else
    AlphaBlendValue := AlphaBlendValue + ALPHA_DELTA;

  if AlphaBlendValue < 0 + ALPHA_DELTA then
    begin
      AlphaBlendValue := ALPHA_DELTA + 1;
      FadeCountDown   := False;
    end;

  if AlphaBlendValue > 255 - ALPHA_DELTA then
    begin
      AlphaBlendValue := 255 - ALPHA_DELTA - 1;
      FadeCountDown   := True;
    end;

  Invalidate;
end;


procedure TFormMain.TimerHideTimer(Sender: TObject);
begin
  // Timer expired → show form again
  Self.Show;
  Self.BringToFront;

  TimerHide.Enabled := False;
end;

procedure TFormMain.TimerSecondTimer(Sender: TObject);
begin
  Invalidate; // calls FormPaint() every second
end;


procedure TFormMain.WMNCHitTest(var Msg: TWMNCHitTest);
const
  GripSize = 8;     // pixels near edge that trigger resize
var
  X, Y   : Integer; // mouse position relative to edges
  Radius : integer; // radius of corner, adjusted for GripSize
begin
  inherited;
  X := Msg.XPos - Left;
  Y := Msg.YPos - Top;
  // see TFormMain.FormChangeBounds() for use of same equation
  Radius := Round(((Width + Height) / 8) / (GripSize / 2));
  // corners
  if      (X <         (GripSize + Radius)) and (Y <          (GripSize + Radius)) then Msg.Result := HTTOPLEFT
  else if (X > Width - (GripSize + Radius)) and (Y <          (GripSize + Radius)) then Msg.Result := HTTOPRIGHT
  else if (X <         (GripSize + Radius)) and (Y > Height - (GripSize + Radius)) then Msg.Result := HTBOTTOMLEFT
  else if (X > Width - (GripSize + Radius)) and (Y > Height - (GripSize + Radius)) then Msg.Result := HTBOTTOMRIGHT
  // edges
  else if (X <          GripSize) then Msg.Result := HTLEFT
  else if (X > Width  - GripSize) then Msg.Result := HTRIGHT
  else if (Y <          GripSize) then Msg.Result := HTTOP
  else if (Y > Height - GripSize) then Msg.Result := HTBOTTOM;
end;

function TFormMain.FormatTimeString: string;
var
  date_time : TDateTime;
begin
  case AppOptions.RunMode of

    rmClock:
      date_time := Now;

    rmStopwatch:
      date_time := Now - StopwatchStart;

    rmTimer:
      begin
        Dec(TimerRemaining);
        if TimerRemaining < 0 then TimerRemaining := 0;
        date_time := EncodeTime(0,0,TimerRemaining,0);
      end;
  end;

  case AppOptions.TimeFormat of
    tf12Hour:
      if AppOptions.ShowSeconds then
        Result := FormatDateTime('hh:mm:ss am/pm', date_time)
      else
        Result := FormatDateTime('hh:mm am/pm', date_time);

    tf24Hour:
      if AppOptions.ShowSeconds then
        Result := FormatDateTime('hh:mm:ss', date_time)
      else
        Result := FormatDateTime('hh:mm', date_time);

  end;

end;

end.

