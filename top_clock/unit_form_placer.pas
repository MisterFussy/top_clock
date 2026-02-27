(* Copyright (c) 2026 Mister Fussy
   MIT License: Permission granted to use, copy, modify,
   and distribute this software. See LICENSE for details.
   Provided "AS IS" without warranty.
*)

(* *********************************************************************

TITLE     : Form placement helpers

PURPOSE   : Provide procedures for positioning a child form relative to a
            parent form.

FEATURES  :
  • Multi-monitor aware (uses the monitor containing the parent form)
  • Taskbar safe (uses WorkAreaRect)
  • One-shot placement (no automatic following)
  • Placement Strategies
    • Below only
    • Above only
    • Center on screen
    • Below → Above → Center (fallback chain)
    • Above → Below → Center (fallback chain)

USAGE     :
  Call PlaceFormRelative(...) whenever:
  • Showing a non-modal form
  • Parent form moves or resizes (optional)
  • Deterministic placement is desired
  • Example:
      PlaceFormRelative(
        MainForm,
        OptionsForm,
        psBelowThenAboveThenCenter,
        8
      );

********************************************************************* *)

unit unit_form_placer;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Controls, Types;
//  Classes, SysUtils;

type
(*
  TPlacementStrategy - Determines placement attempt order
    psBelowOnly  - Place below parent (even if partially off-screen)
    psAboveOnly  - Place above parent
    psCenterOnly - Place in center of the monitor
    psBelowThenAboveThenCenter - Place in order below
    - Try below parent
    - If it does not fit, try above
    - If it still does not fit, center on screen
    psAboveThenBelowThenCenter
    - Try above parent
    - If it does not fit, try below
    - If it still does not fit, center on screen
*)
  TPlacementStrategy = (
    psBelowOnly               ,
    psAboveOnly               ,
    psCenterOnly              ,
    psBelowThenAboveThenCenter,
    psAboveThenBelowThenCenter
  );

(* *********************************************************************

TITLE     : PlaceFormRelative

PURPOSE   :
  Position a Child form relative to a Parent form using
  a placement strategy.

PARAMETERS:
  ParentForm : the form positioning the child
  ChildForm  : the child form to be positioned
  Strategy   : placement strategy (see TPlacementStrategy)
  GapPixels  : vertical spacing between parent and child

FEATURES  :
  • Determines which monitor the parent form is on
  • Uses the usable work area of that monitor
  • Attempts placement based on the chosen strategy
  • Ensures the form is fully visible when possible
  • Falls back to centering if necessary

NON-FEATURES (WHAT THIS FUNCTION DOES NOT DO):
  • Does NOT show the form
  • Does NOT dock or attach the form
  • Does NOT track movement
  • Does NOT modify event handlers

USAGE     :
  PlaceFormRelative(MainForm, ToolForm, psBelowThenAboveThenCenter, 6);
  ToolForm.Show;

********************************************************************* *)
procedure PlaceFormRelative(
  ParentForm : TCustomForm;
  ChildForm  : TCustomForm;
  Strategy   : TPlacementStrategy;
  GapPixels  : Integer
);

implementation

(* *********************************************************************
TITLE     : GetParentWorkArea
PURPOSE   : Returns the usable screen area (excluding taskbars, docks,
            etc.) for the monitor containing the ParentForm
USAGE     : Used by:
            • PlaceFormRelative
********************************************************************* *)
function GetParentWorkArea(AForm: TCustomForm): TRect;
var
  M : TMonitor;
begin
  M        := Screen.MonitorFromWindow(AForm.Handle);
  if Assigned(M) then
    Result := M.WorkareaRect
  else
    Result := Screen.WorkAreaRect; // safe fallback
end;


(* *********************************************************************
TITLE     : FitsInWorkArea
PURPOSE   : Checks if a rectangle is fully contained within a work area
USAGE     : Used by:
            • Placement attempt logic
********************************************************************* *)
function FitsInWorkArea(const R, Work: TRect): Boolean;
begin
  Result :=
    (R.Left   >= Work.Left)  and
    (R.Top    >= Work.Top)   and
    (R.Right  <= Work.Right) and
    (R.Bottom <= Work.Bottom);
end;


(* *********************************************************************
TITLE     : CenterRectInWorkArea
PURPOSE   : Centers a rectangle within a given work area
USAGE     : Used by:
            • Final fallback when other placements do not fit
********************************************************************* *)
function CenterRectInWorkArea(Width, Height: Integer; Work: TRect): TRect;
begin
  Result.Left   := Work.Left + (Work.Width  - Width)  div 2;
  Result.Top    := Work.Top  + (Work.Height - Height) div 2;
  Result.Right  := Result.Left + Width;
  Result.Bottom := Result.Top  + Height;
end;

(* *********************************************************************
TITLE     : PlaceFormRelative
********************************************************************* *)
procedure PlaceFormRelative(
  ParentForm : TCustomForm;
  ChildForm  : TCustomForm;
  Strategy   : TPlacementStrategy;
  GapPixels  : Integer
);
var
  WorkRect : TRect;
  TryRect  : TRect;

  procedure TryBelow;
  begin
    TryRect.Left   := ParentForm.Left;
    TryRect.Top    := ParentForm.Top + ParentForm.Height + GapPixels;
    TryRect.Right  := TryRect.Left + ChildForm.Width;
    TryRect.Bottom := TryRect.Top  + ChildForm.Height;
  end;

  procedure TryAbove;
  begin
    TryRect.Left   := ParentForm.Left;
    TryRect.Top    := ParentForm.Top - ChildForm.Height - GapPixels;
    TryRect.Right  := TryRect.Left + ChildForm.Width;
    TryRect.Bottom := TryRect.Top  + ChildForm.Height;
  end;

  procedure ApplyRect(const R: TRect);
  begin
    ChildForm.SetBounds(R.Left, R.Top, ChildForm.Width, ChildForm.Height);
  end;

begin
  WorkRect := GetParentWorkArea(ParentForm);
  case Strategy of
    psBelowOnly:
      begin
        TryBelow;
        ApplyRect(TryRect);
        Exit;
      end;
    psAboveOnly:
      begin
        TryAbove;
        ApplyRect(TryRect);
        Exit;
      end;
    psCenterOnly:
      begin
        ApplyRect(CenterRectInWorkArea(ChildForm.Width, ChildForm.Height, WorkRect));
        Exit;
      end;
    psBelowThenAboveThenCenter:
      begin
        TryBelow;
        if FitsInWorkArea(TryRect, WorkRect) then
        begin
          ApplyRect(TryRect);
          Exit;
        end;
        TryAbove;
        if FitsInWorkArea(TryRect, WorkRect) then
        begin
          ApplyRect(TryRect);
          Exit;
        end;
        ApplyRect(CenterRectInWorkArea(ChildForm.Width, ChildForm.Height, WorkRect));
        Exit;
      end;
    psAboveThenBelowThenCenter:
      begin
        TryAbove;
        if FitsInWorkArea(TryRect, WorkRect) then
        begin
          ApplyRect(TryRect);
          Exit;
        end;
        TryBelow;
        if FitsInWorkArea(TryRect, WorkRect) then
        begin
          ApplyRect(TryRect);
          Exit;
        end;
        ApplyRect(CenterRectInWorkArea(ChildForm.Width, ChildForm.Height, WorkRect));
        Exit;
      end;
  end;
end;

end.


