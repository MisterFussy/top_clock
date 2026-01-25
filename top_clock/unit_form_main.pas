{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_form_main;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls, ExtCtrls, LCLIntf, LCLType, Windows,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TFormMain }

  TFormMain = class(TForm)
    LabelTime: TLabel;
    TimerSecond: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure LabelTimeDblClick(Sender: TObject);
    procedure TimerSecondTimer(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  R : HRGN;
begin
  TimerSecond.Interval  := 1000; // 1 second
  TimerSecond.Enabled   := True;

  LabelTime.Caption     := TimeToStr(Time);
  LabelTime.OnMouseDown := @FormMouseDown;
  LabelTime.Align       := alClient;

  BorderIcons           := [];     // disable all borders
  BorderStyle           := bsNone; // disable caption bar and borders
  FormStyle             := fsSystemStayOnTop;

  // round corners of form
  R                 := CreateRoundRectRgn(0, 0, Width, Height, 20, 20);
  SetWindowRgn(Handle, R, True);
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
  MessageDlg('About this clock',
             'Instructions:' + LineEnding +
             ' - Left Mouse Down Drag    - moves clock' + LineEnding +
             ' - Left Mouse Double Click - turns off clock' + LineEnding +
             ' - Right Mouse Click       - this message dialog' + LineEnding +
             LineEnding +
             'Brought to you by:' + LineEnding +
             'Mister Fussy',
             mtInformation, [mbOK], 0);
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
//  LabelTime.Font.Size := Round(Width / 12);
  LabelTime.Font.Size := Round(Height / 4);
end;

procedure TFormMain.LabelTimeDblClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.TimerSecondTimer(Sender: TObject);
begin
  LabelTime.Caption := TimeToStr(Time);
end;

end.

