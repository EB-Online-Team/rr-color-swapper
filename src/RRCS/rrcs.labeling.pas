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

unit RRCS.Labeling;

{$mode ObjFPC}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Generics.Collections, Imaging, BGRABitmap, BGRABitmapTypes;

type
  TIntMatrix = array of array of integer;
  TVec2 = array of integer; { Pixel coordinate in (X,Y) order. }
  TVec2Queue = specialize TQueue<TVec2>;

function LabelImage(Image: TBGRABitmap): TIntMatrix;

implementation

function IsPixelTransparent(Image: TBGRABitmap; X, Y: integer): boolean;
begin
  Result := Image.ScanAt(X, Y).alpha = 0;
end;

procedure InspectPixel(Image: TBGRABitmap; X, Y: integer; RegionLabel: integer;
  LabelArray: TIntMatrix; PixelQueue: TVec2Queue);
var
  Pixel: TVec2;
begin
  Pixel := [X, Y];
  if (not IsPixelTransparent(Image, X, Y)) and (LabelArray[X][Y] = 0) then
  begin
    LabelArray[X][Y] := RegionLabel;
    PixelQueue.Enqueue(Pixel);
  end;
end;

procedure InspectNeighboringPixels(Image: TBGRABitmap; X, Y: integer;
  RegionLabel: integer; LabelArray: TIntMatrix; PixelQueue: TVec2Queue);
begin
  if Y > 0 then
  begin
    // N
    InspectPixel(Image, X, Y - 1, RegionLabel, LabelArray, PixelQueue);
    // NW
    if X > 0 then
      InspectPixel(Image, X - 1, Y - 1, RegionLabel, LabelArray, PixelQueue);
    // NE
    if X < Pred(Image.Width) then
      InspectPixel(Image, X + 1, Y - 1, RegionLabel, LabelArray, PixelQueue);
  end;
  if Y < Pred(Image.Height) then
  begin
    // S
    InspectPixel(Image, X, Y + 1, RegionLabel, LabelArray, PixelQueue);
    // SW
    if X > 0 then
      InspectPixel(Image, X - 1, Y + 1, RegionLabel, LabelArray, PixelQueue);
    // SE
    if X < Pred(Image.Width) then
      InspectPixel(Image, X + 1, Y + 1, RegionLabel, LabelArray, PixelQueue);
  end;
  // W
  if X > 0 then
    InspectPixel(Image, X - 1, Y, RegionLabel, LabelArray, PixelQueue);
  // E
  if X < Pred(Image.Width) then
    InspectPixel(Image, X + 1, Y, RegionLabel, LabelArray, PixelQueue);
end;

function LabelImage(Image: TBGRABitmap): TIntMatrix;
var
  X, Y, RegionLabel: integer;
  LabelArray: TIntMatrix;
  PixelQueue: TVec2Queue;
  Pixel: TVec2;
begin
  LabelArray := Default(TIntMatrix);
  SetLength(LabelArray, Image.Width, Image.Height);
  PixelQueue := TVec2Queue.Create;
  RegionLabel := 1;
  for X := 0 to Pred(Image.Width) do
    for Y := 0 to Pred(Image.Height) do
    begin
      if (not IsPixelTransparent(Image, X, Y)) and (LabelArray[X][Y] = 0) then
      begin
        LabelArray[X][Y] := RegionLabel;
        PixelQueue.Enqueue([X, Y]);
        repeat
          Pixel := PixelQueue.Dequeue;
          InspectNeighboringPixels(Image, Pixel[0], Pixel[1], RegionLabel,
            LabelArray, PixelQueue);
        until PixelQueue.Count = 0;
        Inc(RegionLabel);
      end;
    end;
  FreeAndNil(PixelQueue);
  Exit(LabelArray);
end;

end.
