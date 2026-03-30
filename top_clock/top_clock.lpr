program top_clock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit_form_main, unit_options, unit_form_options, unit_form_about,
  unit_form_placer, unit_form_instructions, unit_form_timerexpired;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormInstructions, FormInstructions);
  Application.CreateForm(TFormTimerExpired, FormTimerExpired);
  Application.Run;
end.

