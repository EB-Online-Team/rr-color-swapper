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

unit Forms.Main;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ActnList, StdActns, Imaging, ImagingTypes, ImagingClasses,
  ImagingComponents, RRCS.Labeling;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    alMain:      TActionList;
    actExit:     TFileExit;
    actOpen:     TFileOpen;
    mnuMainHelpAbout: TMenuItem;
    mnuMainFile: TMenuItem;
    mnuMainFileOpen: TMenuItem;
    mnuMainFileSave: TMenuItem;
    mnuMainFileSaveAs: TMenuItem;
    mnuMainHelp: TMenuItem;
    mnuMainFileExit: TMenuItem;
    mnuMain:     TMainMenu;
    imgMain:     TImage;
    pnlMain:     TPanel;
    procedure actOpenAccept(Sender: TObject);
    procedure imgMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure mnuMainHelpAboutClick(Sender: TObject);
  private
    Img: TSingleImage;
    ImgLabels: TIntMatrix;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.mnuMainHelpAboutClick(Sender: TObject);
const
  AboutMessage: string = 'RR Color Swapper' + LineEnding +
    'Copyright (C) 2024 Vartan Haghverdi';
begin
  MessageDlg('About', AboutMessage, mtInformation, [mbOK], 0);
end;

procedure TfrmMain.actOpenAccept(Sender: TObject);
var
  ImgBmp: TImagingBitmap;
begin
  FreeAndNil(Img);
  Img := TSingleImage.CreateFromFile((Sender as TFileOpen).Dialog.FileName);
  Img.Format := ifA8R8G8B8;
  ImgBmp := TImagingBitmap.Create;
  ImgBmp.Assign(Img);
  imgMain.Picture.Graphic := ImgBmp;
  ImgLabels := LabelImage(Img);
  FreeAndNil(ImgBmp);
end;

procedure TfrmMain.imgMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  RegionLabel: integer;
  PixelColor: TColor32Rec;
begin
  if Button = mbLeft then
  begin
    RegionLabel := ImgLabels[X][Y];
    for X := 0 to Pred(Img.Width) do
      for Y := 0 to Pred(Img.Height) do
        if ImgLabels[X][Y] <> RegionLabel then
        begin
          PixelColor := GetPixel32(Img.ImageDataPointer^, X, Y);
          PixelColor.A := 0;
          SetPixel32(Img.ImageDataPointer^, X, Y, PixelColor);
        end;
{
          imgMain.Canvas.Pixels[X, Y] :=
            ($22 << 24) or ((imgMain.Canvas.Pixels[X, Y] << 8) >> 8);
}
    imgMain.Refresh;
  end;
end;

end.
