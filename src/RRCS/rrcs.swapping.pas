{
RR Color Swapper -- A graphical utility to swap texture colors Total War: Rome Remastered

Copyright (C) 2024 Vartan Haghverdi

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

function HueDelta(P1, P2: THSLAPixel): integer;
function HueAdd(P1, P2: THSLAPixel): integer;
procedure HueSwapInRegion(Img: TBGRABitmap; ImgLabels: TIntMatrix;
  RegionLabel: integer; HueIn, HueOut: THSLAPixel; Threshold: double = 0.05);
function HueSwapInRegionS(const Img: TBGRABitmap; ImgLabels: TIntMatrix;
  RegionLabel: integer; HueIn, HueOut: THSLAPixel;
  Threshold: double = 0.05): TBGRABitmap;

implementation

function HueDelta(P1, P2: THSLAPixel): integer;
begin
  if P1.hue > P2.hue then
    Result := P1.hue - P2.hue
  else
    Result := P2.hue - P1.hue;
end;

function HueAdd(P1, P2: THSLAPixel): integer;
begin
  Result := (P1.hue + P2.hue) mod High(THSLAPixel.hue);
end;

procedure HueSwapInRegion(Img: TBGRABitmap; ImgLabels: TIntMatrix;
  RegionLabel: integer; HueIn, HueOut: THSLAPixel; Threshold: double);
var
  X, Y: integer;
  P1, P2: THSLAPixel;
begin
  for Y := 0 to Pred(Img.Height) do
    for X := 0 to Pred(Img.Width) do
    begin
      if RegionLabel <> ImgLabels[X][Y] then
        Continue;
      P1 := Img.ScanAt(X, Y);
      if HueDelta(HueIn, P1) / double(High(THSLAPixel.hue)) < Threshold then
      begin
        P2 := P1;
        P2.hue := (integer(P2.hue) - integer(HueIn.hue)) mod High(THSLAPixel.hue);
        P1.hue := HueAdd(HueOut, P2);
        Img.ScanLine[Y][X] := P1;
      end;
    end;
end;

function HueSwapInRegionS(const Img: TBGRABitmap; ImgLabels: TIntMatrix;
  RegionLabel: integer; HueIn, HueOut: THSLAPixel; Threshold: double): TBGRABitmap;
var
  X, Y: integer;
  P1, P2: THSLAPixel;
begin
  Result := TBGRABitmap.Create(Img);
  for Y := 0 to Pred(Img.Height) do
    for X := 0 to Pred(Img.Width) do
    begin
      if RegionLabel <> ImgLabels[X][Y] then
        Continue;
      P1 := Img.ScanAt(X, Y);
      if HueDelta(HueIn, P1) / double(High(THSLAPixel.hue)) < Threshold then
      begin
        P2 := P1;
        P2.hue := (P2.hue - HueIn.hue) mod High(THSLAPixel.hue);
        P1.hue := HueAdd(HueOut, P2);
        Result.ScanLine[Y][X] := P1;
      end;
    end;
end;

end.
