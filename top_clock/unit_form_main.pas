{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_main;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls, ExtCtrls, LCLIntf, LCLType, Windows,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus;

type

  { TFormMain }

  TFormMain = class(TForm)
    MenuItemClose: TMenuItem;
    MenuItemInstruction: TMenuItem;
    MenuItemAbout: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    TimerFader: TTimer;
    TimerSecond: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemInstructionClick(Sender: TObject);
    procedure TimerFaderTimer(Sender: TObject);
    procedure TimerSecondTimer(Sender: TObject);
  private

  public
    FadeCountDown : Boolean;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

const DisplayText = '12:34';

procedure TFormMain.FormCreate(Sender: TObject);
var
  Region : HRGN;
begin
  TimerSecond.Interval := 1000; // 1 second
  TimerSecond.Enabled  := True;

  BorderIcons          := [];     // disable all borders
  BorderStyle          := bsNone; // disable caption bar and borders
  FormStyle            := fsSystemStayOnTop;
  Color                := clBlack;

  // round corners of form
  Region               := CreateRoundRectRgn(0, 0, Width, Height, 20, 20);
  SetWindowRgn(Handle, Region, True);

  // make form partially transparent
  AlphaBlend           := True;
  AlphaBlendValue      := 150;

end;

// Using a TLabel is too difficult to size & center properly, hence
// the form is painte below - time added with Canvas.TextOut()
procedure TFormMain.FormPaint(Sender: TObject);
var
  ScreenText : string;
  TargetWidth: Integer;
  FontSize   : Integer;
begin
  ScreenText         := TimeToStr(Time);  // current time is displayed
  TargetWidth        := ClientWidth - 10; // small margin
  FontSize           := 12;               // starting font size

  Canvas.Font.Color  := clWhite;

  // Increase font size until text is almost as wide as the form
  Canvas.Font.Size   := FontSize;
  while (Canvas.TextWidth(ScreenText) < TargetWidth) do
  begin
    Inc(FontSize);
    Canvas.Font.Size := FontSize;
  end;

  // If we overshot, step back one
  if Canvas.TextWidth(ScreenText) > TargetWidth then
  begin
    Dec(FontSize);
    Canvas.Font.Size := FontSize;
  end;

  // Draw centered vertically and horizontally
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
  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
  end
  else
  begin
    PopupMenu1.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;


procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('Brought to you by:' + LineEnding +
             'Mister Fussy' +
              LineEnding,
              mtInformation, [mbOK], 0);
end;

procedure TFormMain.MenuItemCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MenuItemInstructionClick(Sender: TObject);
begin
  MessageDlg('Instructions:' + LineEnding +
             ' - Left Mouse Down Drag    - moves clock' + LineEnding +
             ' - Left Mouse Double Click - turns off clock' + LineEnding +
             ' - Right Mouse Click       - popup menu' + LineEnding +
             LineEnding,
             mtInformation, [mbOK], 0);
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

procedure TFormMain.TimerSecondTimer(Sender: TObject);
begin
  Invalidate;
end;

end.

