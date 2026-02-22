{ Copyright (c) 2026 Mister Fussy
  MIT License: Permission granted to use, copy, modify,
  and distribute this software. See LICENSE for details.
  Provided "AS IS" without warranty. }

unit unit_options;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics, IniFiles, Forms,
  Classes, SysUtils;

type
  TTimeFormat = (tf12Hour, tf24Hour);
  TRunMode    = (rmClock, rmTimer, rmStopwatch);

  TAppOptions = record
    TimeFormat   : TTimeFormat;
    DisplayColor : TColor;
    TextColor    : TColor;
    RunMode      : TRunMode;
    ShowSeconds  : Boolean;
    TimerSeconds : Integer; // countdown timer
    Left         : Integer;
    Top          : Integer;
    Width        : Integer;
    Height       : Integer;
  end;

const
  DefaultAppOptions: TAppOptions = (
    TimeFormat   : tf12Hour;
    DisplayColor : clBlack;
    TextColor    : clWhite;
    RunMode      : rmClock;
    ShowSeconds  : True;
    TimerSeconds : 60;
    Left         : 0;
    Top          : 0;
    Width        : 134;
    Height       : 44
  );

var
  AppOptions: TAppOptions;

procedure LoadOptions(Form: TForm);
procedure SaveOptions(Form: TForm);

implementation
const
  INI_FILENAME = 'top_clock.ini';

procedure LoadOptions(Form: TForm);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(INI_FILENAME);
  try
    AppOptions.TimeFormat   := TTimeFormat(ini.ReadInteger('Options','TimeFormat',Ord(DefaultAppOptions.TimeFormat)));
    AppOptions.DisplayColor := TColor(ini.ReadInteger('Options','DisplayColor',DefaultAppOptions.DisplayColor));
    AppOptions.TextColor    := TColor(ini.ReadInteger('Options','TextColor',DefaultAppOptions.TextColor));
    AppOptions.RunMode      := TRunMode(ini.ReadInteger('Options','RunMode',Ord(DefaultAppOptions.RunMode)));
    AppOptions.ShowSeconds  := ini.ReadBool('Options','ShowSeconds',DefaultAppOptions.ShowSeconds);
    AppOptions.TimerSeconds := ini.ReadInteger('Options','TimerSeconds',DefaultAppOptions.TimerSeconds);

(* TODO: Figure out why Width and Height keep shrinking in size

    Form.Left   := ini.ReadInteger('Window','Left',DefaultAppOptions.Left);
    Form.Top    := ini.ReadInteger('Window','Top',DefaultAppOptions.Top);
    Form.Width  := ini.ReadInteger('Window','Width',DefaultAppOptions.Width);
    Form.Height := ini.ReadInteger('Window','Height',DefaultAppOptions.Height);
*)
  finally
    ini.Free;
  end;
end;

procedure SaveOptions(Form: TForm);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(INI_FILENAME);
  try
    ini.WriteInteger('Options','TimeFormat',Ord(AppOptions.TimeFormat));
    ini.WriteInteger('Options','DisplayColor',AppOptions.DisplayColor);
    ini.WriteInteger('Options','TextColor',AppOptions.TextColor);
    ini.WriteInteger('Options','RunMode',Ord(AppOptions.RunMode));
    ini.WriteBool   ('Options','ShowSeconds',AppOptions.ShowSeconds);
    ini.WriteInteger('Options','TimerSeconds',AppOptions.TimerSeconds);

    ini.WriteInteger('Window','Left',Form.Left);
    ini.WriteInteger('Window','Top',Form.Top);
    ini.WriteInteger('Window','Width',Form.Width);
    ini.WriteInteger('Window','Height',Form.Height);
  finally
    ini.Free;
  end;
end;

end.

