{
RR Color Swapper -- A graphical utility to swap texture colors for Total War: Rome Remastered.

Copyright (C) 2024-2025 Vartan Haghverdi

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
}

unit RRCS.Swapping;

{$mode ObjFPC}{$H+}{$J-}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, RRCS.Labeling;

{ Return the positive difference of the hue values of two pixels. }
function HueDelta(P1, P2: THSLAPixel): integer;

{ Return the sum of the hue values of two pixels, wrapping back around
if the sum surpasses the highest possible hue value. }
function HueAdd(P1, P2: THSLAPixel): integer;

{ Swap all pixels in an image (ImgOut) containing a hue value no more than a given
percentage (Threshold) different than a given hue value (HueIn) with
a different hue value (HueOut). Only do this for pixels that lie within a given
region (RegionLabel), as indicated by a label matrix (ImgLabels).

NOTE: This procedure is destructive as it performs the swap in-place and modifies
the passed image (Img). }
{
procedure HueSwapInRegion(var Img: TBGRABitmap; ImgLabels: TIntMatrix;
  RegionLabel: integer; HueIn, HueOut: THSLAPixel; Threshold: double = 0.05);
}
procedure HueSwapInRegion(const ImgIn: TBGRABitmap; var ImgOut: TBGRABitmap;
  ImgLabels: TIntMatrix; RegionLabel: integer; HueIn, HueOut: THSLAPixel;
  Threshold: double = 0.05);

{ Return a copy of an image (Img) in which all pixels containing a hue value no
more than a given percentage (Threshold) different than a given hue value
(HueIn) have been swapped with a different hue value (HueOut). Only do this for
pixels that lie within a given region (RegionLabel), as indicated by a label
matrix (ImgLabels). }
function HueSwapInRegionS(const Img: TBGRABitmap; ImgLabels: TIntMatrix;
  RegionLabel: integer; HueIn, HueOut: THSLAPixel;
  Threshold: double = 0.05): TBGRABitmap;

implementation

function HueDelta(P1, P2: THSLAPixel): integer;
begin
  Result := Abs(integer(P1.hue) - integer(P2.hue));
end;

function HueAdd(P1, P2: THSLAPixel): integer;
begin
  // High() here returns 2^16 - 1 hence the need for + 1.
  Result := (P1.hue + P2.hue) mod (High(THSLAPixel.hue) + 1);
end;

procedure HueSwapInRegion(const ImgIn: TBGRABitmap; var ImgOut: TBGRABitmap;
  ImgLabels: TIntMatrix; RegionLabel: integer; HueIn, HueOut: THSLAPixel;
  Threshold: double);
var
  X, Y: integer;
  P1, P2: THSLAPixel;
begin
  if (not Assigned(ImgIn)) or (not Assigned(ImgOut)) then
    Exit;

  for Y := 0 to Pred(ImgIn.Height) do
    for X := 0 to Pred(ImgIn.Width) do
    begin
      if RegionLabel <> ImgLabels[X][Y] then
        Continue;
      P1 := ImgIn.ScanLine[Y][X];

      // High() here returns 2^16 - 1 hence the need for + 1.
      if HueDelta(HueIn, P1) / double(High(THSLAPixel.hue) + 1) < Threshold then
      begin
        P2 := P1;
        P2.hue := (integer(P2.hue) - integer(HueIn.hue)) mod High(THSLAPixel.hue);
        P1.hue := HueAdd(HueOut, P2);
        ImgOut.ScanLine[Y][X] := P1;
      end;
    end;
end;

function HueSwapInRegionS(const Img: TBGRABitmap; ImgLabels: TIntMatrix;
  RegionLabel: integer; HueIn, HueOut: THSLAPixel; Threshold: double): TBGRABitmap;
begin
  Exit;
  //Result := TBGRABitmap.Create(Img);
  //HueSwapInRegion(Result, ImgLabels, RegionLabel, HueIn, HueOut, Threshold);
end;

end.
