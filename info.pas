unit info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Math, BGRABitmap, EasyLazFreeType, BGRABitmapTypes, BGRATextFX, LCLType;

type

  { TInForm }

  TInForm = class(TForm)
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    GrowUp: boolean;
  end;

var
  InForm: TInForm;
  maxx, maxy: integer;

implementation

uses kdar;

var
  InFormLeft, InFormTop: integer;
  dragging1, sizing1: boolean;

{$R *.lfm}

{ TInForm }

procedure TInForm.FormDeactivate(Sender: TObject);
begin
  if (InForm.Height > SvitokMinHeight) or not popping then begin
     Close;
     if popping and not (rolling or folding or scrolling or ((CalSpeed.x <> 0)
     or (CalSpeed.y <> 0) or AttractionX or AttractionY)) then
        MainForm.Timer_10Hz.Enabled := false;
     popping := false;
     if textbmp <> nil then textbmp.Free;
     if textbmpjammed <> nil then textbmpjammed.Free;
     if textbmpheader <> nil then textbmpheader.Free;
     if textbmpfooter <> nil then textbmpfooter.Free;
     textbmp := nil;
     textbmpjammed := nil;
     textbmpheader := nil;
     textbmpfooter := nil;
  end;
end;

procedure TInForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  PopTick := 0;
  If Key = vk_Escape then begin
     Close;
     if popping and not (rolling or folding or scrolling or ((CalSpeed.x <> 0)
        or (CalSpeed.y <> 0) or AttractionX or AttractionY)) then
        MainForm.Timer_10Hz.Enabled := false;
     popping := false;
     if textbmp <> nil then textbmp.Free;
     if textbmpjammed <> nil then textbmpjammed.Free;
     if textbmpheader <> nil then textbmpheader.Free;
     if textbmpfooter <> nil then textbmpfooter.Free;
     textbmp := nil;
     textbmpjammed := nil;
     textbmpheader := nil;
     textbmpfooter := nil;
  end else if (Key = vk_Right) and (SwitchKind in Switchable) then begin // чпок вправо
     InfAttrX := true;
     InfSpeed.x := InfSpeed.x - 1;
     FlipToX := - 1;
     MainForm.Timer_10Hz.Enabled := true;
  end else if (Key = vk_Left) and (SwitchKind in Switchable) then begin // чпок влево
     InfAttrX := true;
     InfSpeed.x := InfSpeed.x + 1;
     FlipToX := 1;
     MainForm.Timer_10Hz.Enabled := true;
  end else if Key = vk_Up then begin                            // шаг вверх
     InfSpeed.y := 0;
     InfCalPos.y := InfCalPos.y - Round (StringStep * Scale);
     if InfCalPos.y < 0 then InfCalPos.y := 0;
     Invalidate;
  end else if Key = vk_Down then begin                          // шаг вниз
     InfSpeed.y := 0;
     InfCalPos.y := InfCalPos.y + Round (StringStep * Scale);
     if InfCalPos.y > maxy then InfCalPos.y := maxy;
     Invalidate;
  end else if (Key = vk_Prior) and (InfCalPos.y > 0) then begin // пинок вверх
     MaxRollTime.y := Round (Sqrt (InForm.Height - ssvitoks[0].Height - ssvitoks[2].Height));
     if MaxRollTime.y < 5 then MaxRollTime.y := 5;
     InfSpeed.y := - MaxRollTime.y - 1;
     ScrollTime := 0;
     InfAttrY := false;
     MainForm.Timer_10Hz.Enabled := true;
  end else if (Key = vk_Next) and (InfCalPos.y < maxy) then begin // пинок вниз
     MaxRollTime.y := Round (Sqrt (InForm.Height - ssvitoks[0].Height - ssvitoks[2].Height));
     if MaxRollTime.y < 5 then MaxRollTime.y := 5;
     InfSpeed.y := MaxRollTime.y + 1;
     ScrollTime := 0;
     InfAttrY := false;
     MainForm.Timer_10Hz.Enabled := true;
  end;
end;

procedure TInForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  if Button = mbLeft then begin
     if (Y < TitleHeight * Scale) and not scrolling then begin
        InFormLeft := X;
        InFormTop := Y;
        dragging1 := true;
        sizing1 := false;
     end else if (Y > (InForm.Height - TitleHeight * Scale)) and not scrolling then begin
        InFormTop := Y - InForm.Height;
        sizing1 := true;
        dragging1 := false;
     end else if not (sizing1 or dragging1) then begin
        scrolling := true;
        InfBegPos.x := X;
        InfBegPos.y := Y;
        InfJogPos.x := 0;
        InfJogPos.y := 0;
        InfOriPos.x := X;
        InfOriPos.y := Y;
        InfCalPos.x := InfCalPos.x + InfCurPos.x;
        InfCalPos.y := InfCalPos.y + InfCurPos.y;
        InfCurPos.x := 0;
        InfCurPos.y := 0;
        InfPrePos.x := 0;
        InfPrePos.y := 0;
        InfSpeed.x := 0;
        InfSpeed.y := 0;
        InfAttrX := false;
        InfAttrY := false;
        InfRollTime := 0;
        ScrollTime := 0;
        for i := Low (ideltas) to High (ideltas) do ideltas[i] := Point (0, 0);
        MainForm.Timer_10Hz.Enabled := true;
     end;
  end;
  PopTick := 0;
end;

procedure TInForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  h: integer;
begin
  if Y < TitleHeight * Scale then Cursor := crDrag else
     if Y > (InForm.Height - TitleHeight * Scale) then Cursor := crSizeNS else
        Cursor := crDefault;
  if dragging1 then begin
     InForm.Left := InForm.Left + X - InFormLeft;
     InForm.Top := InForm.Top + Y - InFormTop;
  end else if sizing1 then begin
     h := Y - InFormTop;
     if h > SvitokMaxHeight then h := SvitokMaxHeight;
     if h < SvitokMinHeight then h := SvitokMinHeight;
     if textbmp = nil then maxy := 0 else
        maxy := textbmp.Height - Height + ssvitoks[0].Height + ssvitoks[2].Height;
     if maxy < 0 then maxy := 0;
     MainForm.ShapeSvitok (h);
  end else if scrolling then begin
     if (SwitchKind in Switchable) then begin
        InfCurPos.x := InfBegPos.x - X;
        if Abs (InfCalPos.x + InfCurPos.x) > maxx then
           InfCurPos.x := maxx * Sign (InfCurPos.x) - InfCalPos.x;
        InfJogPos.x := InfOriPos.x - X;
     end;
     InfCurPos.y := InfBegPos.y - Y;
       if InfCalPos.y + InfCurPos.y < 0 then InfCurPos.y := - InfCalPos.y
          else if InfCalPos.y + InfCurPos.y > maxy then
             InfCurPos.y := maxy - InfCalPos.y;
     InfJogPos.y := InfOriPos.y - Y;
     Invalidate;
  end else begin
     if arrbmp_tout < 1 then Invalidate;
     if (Y > ssvitoks[0].Height) and (Y < (Height - ssvitoks[2].Height)) then
        arrbmp_tout := Arrows_visible;
  end;
  PopTick := 0;
end;

procedure TInForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  if scrolling then begin
     if (SwitchKind in Switchable) then begin
        InfJogPos.x := InfOriPos.x - X;
        InfCalPos.x := InfCalPos.x + InfBegPos.x - X;
        InfCurPos.x := 0;
        InfBegPos.x := 0;
        if Abs (InfCalPos.x) > maxx then InfCalPos.x := maxx * Sign (InfCalPos.x);
        InfSpeed.x := 0;
     end;
     InfJogPos.y := InfOriPos.y - Y;
     InfCalPos.y := InfCalPos.y + InfBegPos.y - Y;
     InfCurPos.y := 0;
     InfBegPos.y := 0;
     if InfCalPos.y < 0 then InfCalPos.y := 0
        else if InfCalPos.y > maxy then InfCalPos.y := maxy;
     InfSpeed.y := 0;
     for i := Low (ideltas) to High (ideltas) do begin
       InfSpeed.x := InfSpeed.x + ideltas[i].x;
       InfSpeed.y := InfSpeed.y + ideltas[i].y;
     end;
     InfSpeed.x := InfSpeed.x div (High (ideltas) - Low (ideltas) + 1);
     InfSpeed.y := InfSpeed.y div (High (ideltas) - Low (ideltas) + 1);
     MaxRollTime.x := Round (StdRollTime * Abs (InfSpeed.x) / DesiredSpeed.x);
     MaxRollTime.y := Round (StdRollTime * Abs (InfSpeed.y) / DesiredSpeed.y);
     if MaxRollTime.x < 5 then MaxRollTime.x := 5;
     if MaxRollTime.y < 5 then MaxRollTime.y := 5;
     ScrollTime := 0;
  end;
  scrolling := false;
  dragging1 := false;
  sizing1 := false;
  PopTick := 0;
end;

procedure TInForm.FormPaint(Sender: TObject);
var
  i, j, dx, dy, h1, h2, thp: integer;
  bp: TBGRABitmap;
begin
  i := 0;
  dy := 0;
  repeat
    ssvitoks[i].Draw (Canvas, 0, dy, true);
    dy := dy + ssvitoks[i].Height;
    if (i = 0) or (dy >= (InForm.Height - ssvitoks[2].Height)) then i := i + 1;
    if dy > (InForm.Height - ssvitoks[2].Height) then
       dy := InForm.Height - ssvitoks[2].Height;
  until i > 2;
  if f1 <> nil then with f1 do begin    // set font properties
     SizeInPixels := Round (18 * Scale);
     Style := [];
     WidthFactor := 0.9;
  end;
  h1 := Round ((SvitokTextDiff div 2 - 8) * Scale);
  if textbmpheader <> nil then begin
     j := (InForm.Width - Round (f1.TextWidth (STitle))) div 2
         - InfCalPos.x - InfCurPos.x - Round (8 * Scale);
     h2 := Max (textbmpheader.Width - Max (textbmpheader.Width + j - InForm.Width + h1, 0), 0);
     if j < h1 then begin
        dx := h1 - j;
        j := h1;
     end else dx := 0;
     if dx > textbmpheader.Width then dx := textbmpheader.Width;
     if h2 < dx then h2 := dx;
     if (textbmpheader = nil) or (h2 <= dx) or (textbmpheader.Height <= 0) then bp := nil else
        bp := textbmpheader.GetPart (Rect (dx, 0, h2, textbmpheader.Height)) as TBGRABitmap;
     if bp <> nil then begin
        bp.Draw (Canvas, j, Round (14 * Scale), false);
        bp.Free;
     end;
  end;
  if textbmpfooter <> nil then begin
     j := (InForm.Width - Round (f1.TextWidth (Footer))) div 2
         - InfCalPos.x - InfCurPos.x + Round (8 * Scale);
     h2 := Max (textbmpfooter.Width - Max (textbmpfooter.Width + j - InForm.Width + h1, 0), 0);
     if j < h1 then begin
        dx := h1 - j;
        j := h1;
     end else dx := 0;
     if dx > textbmpfooter.Width then dx := textbmpfooter.Width;
     if h2 < dx then h2 := dx;
     if (textbmpfooter = nil) or (h2 <= dx) or (textbmpfooter.Height <= 0) then bp := nil else
        bp := textbmpfooter.GetPart (Rect (dx, 0, h2, textbmpfooter.Height)) as TBGRABitmap;
     if bp <> nil then begin
        bp.Draw (Canvas, j, InForm.Height - Round (32 * Scale), false);
        bp.Free;
     end;
  end;
  i := Round (StringStep * StartString * Scale) + InfCalPos.y + InfCurPos.y;
  j := Round (SvitokTextDiff * Scale) div 2 - InfCalPos.x - InfCurPos.x;
  if j < 0 then dx := -j else dx := 0;
  thp := Round ((TitleHeight + 8) * Scale);
  if (textbmp <> nil) and (i < textbmp.Height) then begin
     dy := i + InForm.Height - ssvitoks[0].Height - ssvitoks[2].Height;
     if dy > textbmp.Height then dy := textbmp.Height;
     if textbmpjammed = nil then h1 := 0
        else h1 := Round (textbmpjammed.Height * dy / textbmp.Height);
     dy := dy + Round (14 * Scale);
     if dy > textbmp.Height then dy := textbmp.Height;
     h2 := dy - textbmpjammed.Height + h1;
     if (textbmp = nil) or (textbmp.Width <= dx) or (h2 <= i) then bp := nil else
        bp := textbmp.GetPart (Rect (dx, i, textbmp.Width, h2)) as TBGRABitmap; // основной текст
     if bp <> nil then begin
        bp.Draw (Canvas, j + 2 + dx, thp, false);
        bp.Free;
     end;
     if (textbmp <> nil) and (textbmpjammed <> nil) and (dy < textbmp.Height) then begin
        if (textbmpjammed = nil) or (textbmpjammed.Width <= dx) or
              (textbmpjammed.Height <= h1) then bp := nil else
           bp := textbmpjammed.GetPart (Rect (dx, h1, textbmpjammed.Width,
                 textbmpjammed.Height)) as TBGRABitmap;  // "кучка мусора" внизу
        if bp <> nil then begin
           h1 := Round ((TitleHeight + 2) * Scale);
           bp.Draw (Canvas, j + dx, InForm.Height - bp.Height - h1, false);
           bp.Draw (Canvas, j + dx, InForm.Height - bp.Height - h1, false);
           bp.Free;
        end;
     end;
  end;
  if (textbmp <> nil) and (textbmpjammed <> nil) and (i > 0) then begin
     h2 := Round (i * textbmpjammed.Height / textbmp.Height);
     if (textbmpjammed = nil) or (textbmpjammed.Width <= dx) or (h2 <= 0) then
        bp := nil else
     bp := textbmpjammed.GetPart (Rect (dx, 0, textbmpjammed.Width, h2)) as TBGRABitmap;
     if bp <> nil then begin                          // "кучка мусора" вверху
        bp.Draw (Canvas, j + dx, thp, false);
        bp.Draw (Canvas, j + dx, thp, false);
        bp.Free;
     end;
  end;
  if (arrbmp_tout > 0) and not scrolling then begin    // рисуем стрелочки
     if (InfCalPos.y + InfCurPos.y > 0) and (sarrbmp[0] <> nil) then
        sarrbmp[0].Draw (Canvas, (ClientWidth - sarrbmp[0].Width) div 2, thp, false);
     if (InfCalPos.y + InfCurPos.y < maxy) and (sarrbmp[1] <> nil) then
        sarrbmp[1].Draw (Canvas, (ClientWidth - sarrbmp[1].Width) div 2,
                         ClientHeight - thp - sarrbmp[1].Height, false);
     if (SwitchKind in Switchable) and (sarrbmp[2] <> nil)
           and (sarrbmp[3] <> nil) then begin
        sarrbmp[2].Draw (Canvas, Round (SvitokTextDiff * Scale) div 2,
                        (ClientHeight - sarrbmp[2].Height) div 2, false);
        sarrbmp[3].Draw (Canvas,
           ClientWidth - Round ((SvitokTextDiff div 2 + 12) * Scale),
          (ClientHeight - sarrbmp[3].Height) div 2, false);
     end;
  end;
end;

procedure TInForm.FormShow(Sender: TObject);
begin
  InfJogPos.x := 0;
  InfJogPos.y := 0;
  InfCurPos.x := 0;
  InfCurPos.y := 0;
  InfCalPos.x := 0;
  InfCalPos.y := 0;
  InfPrePos.x := 0;
  InfPrePos.y := 0;
  InfSpeed.x := 0;
  InfSpeed.y := 0;
  InfAttrX := false;
  InfAttrY := false;
  InfRollTime := 0;
  ScrollTime := 0;
  maxx := InForm.Width - Round (SvitokTextDiff * Scale);
  if textbmp = nil then maxy := 0 else
     maxy := textbmp.Height - Height + ssvitoks[0].Height + ssvitoks[2].Height;
  if maxy < 0 then maxy := 0;
end;

end.

