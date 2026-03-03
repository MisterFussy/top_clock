{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_main;

{$mode objfpc}{$H+}

interface

uses
  unit_options, unit_form_placer, unit_form_options, unit_form_instructions,
  unit_form_about, StdCtrls, ExtCtrls, LCLIntf, LCLType, Windows, Classes,
  SysUtils, Forms, Controls, Graphics, Dialogs, Menus, EditBtn, ButtonPanel,
  Buttons, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    ImagePause: TImage;
    ImagePlay: TImage;
    ImageStop: TImage;
    ImageReset: TImage;
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
    procedure FormShow(Sender: TObject);
    procedure ImagePauseClick(Sender: TObject);
    procedure ImagePlayClick(Sender: TObject);
    procedure ImageResetClick(Sender: TObject);
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
  StopwatchStart : TDateTime;       // time stopwatch starts
  StopwatchPause : TDateTime;       // time startwatch paused
  StopWatchState : TStopWatchState; // state of st
  TimerRemaining : Integer;         // seconds left of timer

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
  // see TFormMain.WMNCHitTest() for use of same equation for Radius
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

  StopwatchStart        := now;
  StopWatchState        := swIdle;

  TimerRemaining        := AppOptions.TimerSeconds;

  TimerSecond.Interval  := 1000;              // 1 second clock interval
  TimerSecond.Enabled   := True;

  TimerHide.Interval    := 2000;              // 2 seconds, hide window time
  TimerHide.Enabled     := False;

  BorderIcons           := [];                // disable all borders
  BorderStyle           := bsNone;            // disable caption bar and borders
  FormStyle             := fsSystemStayOnTop;
  Color                 := AppOptions.DisplayColor;

  // Form position and size set in TFormMain.FormShow()

  AlphaBlend            := True;              // make form partially transparent
  AlphaBlendValue       := 130;

  Constraints.MinWidth  := 60;                // limit the minimum size
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
// the form is painted below - time is added with Canvas.TextOut() in the
// Target area:
//
// RunMode = rmClock                    / RunMode = rmStopwatch or rmTimer
//                                      /
//         ClientWidth                  /           ClientWidth
//         __________________________   /           ___________________________
// Client |                          |  /   Client |                           |
// Height |          TargetWidth     |  /   Height |           TargetWidth     |
//        |          ___________     |  /          |           ___________     |
//        |  Target |           |    |  /          |   Target |           |    |
//        |  Height | Target    |    |  /          |   Height | Target    |    |
//        |         | Area      |    |  /          |          | Area      |    |
//        |         |           |    |  /          |          |           |    |
//        |         |           |    |  /          |          |___________|    |
//        |         |           |    |  /          |  Control |           |    |
//        |         |___________|    |  /          |  Height  |           |    |
//        |                          |  /          |          |           |    |
//        |                          |  /          |          |___________|    |
//        |__________________________|  /          |___________________________|
//

procedure TFormMain.FormPaint(Sender: TObject);
const
  SCALE_FACTOR   : double = 0.6;
var
  ScreenText     : string;
  TargetWidth    ,
  TargetHeight   : Integer;
  ImageWidth     ,
  ImageHeight    : Integer;
  FontSize       : Integer;
  Margin         : Integer;
  ImageTop       : Integer;
begin
  inherited;
  Color              := AppOptions.DisplayColor;
  ScreenText         := FormatTimeString; // current time formatted
  Margin             := 1;
  case AppOptions.RunMode of
    rmClock :
      begin
        TargetWidth  := ClientWidth  - Margin;
        TargetHeight := ClientHeight - Margin;
      end;
    rmStopwatch, rmTimer :
      begin
        TargetWidth    :=       ClientWidth                  - Margin;
        TargetHeight   := Round(ClientHeight * SCALE_FACTOR) - Margin;
      end;
  end;
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

  // draw centered vertically and horizontally in target area
  case AppOptions.RunMode of
    rmClock :
      Canvas.TextOut(
        (ClientWidth  - Canvas.TextWidth (ScreenText)) div 2,
        (ClientHeight - Canvas.TextHeight(ScreenText)) div 2,
        ScreenText
      );
    rmStopwatch, rmTimer :
      Canvas.TextOut(
        (ClientWidth                        - Canvas.TextWidth (ScreenText)) div 2,
        (Round(ClientHeight * SCALE_FACTOR) - Canvas.TextHeight(ScreenText)) div 2,
        ScreenText
      );
  end;

  // Start painting the controls

  ImageHeight       := ClientHeight - TargetHeight; // images fills up bottum of Client
  ImageWidth        := ImageHeight;                 // keep image square
  ImageTop          := TargetHeight - Margin * 2;   // place controls under the Target area

  ImageReset.Height := ImageHeight;
  ImagePause.Height := ImageHeight;
  ImagePlay.Height  := ImageHeight;
  ImageStop.Height  := ImageHeight;
  ImageReset.Width  := ImageWidth;
  ImagePause.Width  := ImageWidth;
  ImagePlay.Width   := ImageWidth;
  ImageStop.Width   := ImageWidth;

  ImageReset.Top    := ImageTop;
  ImagePause.Top    := ImageTop;
  ImagePlay.Top     := ImageTop;
  ImageStop.Top     := ImageTop;

  case AppOptions.RunMode of
    rmClock     :
      begin
        ImagePause.Visible := False;
        ImagePlay.Visible  := False;
        ImageStop.Visible  := False;
        ImageReset.Visible := False;
      end;
    rmStopwatch :
      begin
        case StopWatchState of
          swIdle   :
            begin
              ImagePause.Visible := False;
              ImagePlay.Visible  := True;
              ImageStop.Visible  := False;
              ImageReset.Visible := True;
              ImageReset.Left    := Round(Width / 2) - ImageWidth * 1;
              ImagePlay.Left     := Round(Width / 2);
            end;
          swTiming :
            begin
              ImagePause.Visible := True;
              ImagePlay.Visible  := False;
              ImageStop.Visible  := False;
              ImageReset.Visible := True;
              ImageReset.Left    := Round(Width / 2) - ImageWidth * 1;
              ImagePause.Left    := Round(Width / 2);
            end;
          swPaused :
            begin
              ImagePause.Visible := False;
              ImagePlay.Visible  := True;
              ImageStop.Visible  := False;
              ImageReset.Visible := True;
              ImageReset.Left    := Round(Width / 2) - ImageWidth * 1;
              ImagePlay.Left     := Round(Width / 2);
            end;
          end;
      end;
    rmTimer     :
      begin
        ImagePause.Visible := False;
        ImagePlay.Visible  := True;
        ImageStop.Visible  := False;
        ImageReset.Visible := False;
      end;
  end; // case AppOptions.RunMode
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


procedure TFormMain.FormShow(Sender: TObject);
begin
  // Set Form position and size here, because postioning and DPI scaling
  // may may occur
  Left   := AppOptions.Left;
  Top    := AppOptions.Top;
  Height := AppOptions.Height;
  Width  := AppOptions.Width;
end;


procedure TFormMain.ImagePauseClick(Sender: TObject);
begin
  StopwatchPause         := now - StopwatchStart;
  StopWatchState         := swPaused;
end;


procedure TFormMain.ImagePlayClick(Sender: TObject);
begin
  if StopWatchState <> swPaused then
    StopwatchStart       := now;
  StopWatchState         := swTiming;
end;


procedure TFormMain.ImageResetClick(Sender: TObject);
begin
  StopwatchStart := now;
  StopwatchPause := 0;
  if StopwatchState = swPaused then
    StopwatchState := swIdle;
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
      date_time := now;

    rmStopwatch:
      case StopWatchState of
        swIdle   : date_time := 0;
        swTiming : date_time := now - StopwatchStart;
        swPaused : date_time := StopwatchPause;
      end;

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

  case AppOptions.RunMode of
    rmClock:
      null;
    rmStopwatch:
      Result := FormatDateTime('hh:mm:ss.zzz', date_time);
    rmTimer:
      null;
  end;

end;

end.
