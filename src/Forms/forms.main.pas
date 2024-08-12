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
  ActnList, StdActns, StdCtrls, Imaging, ImagingClasses, ImagingComponents,
  BGRABitmap, BGRABitmapTypes, HColorPicker, RRCS.Labeling, RRCS.Swapping;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    alMain: TActionList;
    actExit: TFileExit;
    actOpen: TFileOpen;
    HColorPicker1: THColorPicker;
    HColorPicker2: THColorPicker;
    mnuMainHelpAbout: TMenuItem;
    mnuMainFile: TMenuItem;
    mnuMainFileOpen: TMenuItem;
    mnuMainFileSave: TMenuItem;
    mnuMainFileSaveAs: TMenuItem;
    mnuMainHelp: TMenuItem;
    mnuMainFileExit: TMenuItem;
    mnuMain: TMainMenu;
    imgMain: TImage;
    Panel1: TPanel;
    pnlMain: TPanel;
    procedure actOpenAccept(Sender: TObject);
    procedure HColorPicker2Change(Sender: TObject);
    procedure imgMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure mnuMainHelpAboutClick(Sender: TObject);
  private
    Img: TSingleImage;
    ImgBitmap, ImgBitmap2: TBGRABitmap;
    ImgLabels: TIntMatrix;
    SelectedRegion: integer;
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
begin
  FreeAndNil(Img);
  FreeAndNil(ImgBitmap);
  Img := TSingleImage.CreateFromFile((Sender as TFileOpen).Dialog.FileName);
  if Img.SaveToFile('tmp.tga') then
  begin
    ImgBitmap := TBGRABitmap.Create('tmp.tga');
    imgMain.Picture.Assign(ImgBitmap);
    ImgLabels := LabelImage(ImgBitmap);
  end
  else
    ShowMessage('Error while opening image.');
end;

procedure TfrmMain.HColorPicker2Change(Sender: TObject);
begin
  FreeAndNil(ImgBitmap2);
  ImgBitmap2 := HueSwapInRegionS(ImgBitmap, ImgLabels, SelectedRegion,
    HColorPicker1.SelectedColor, HColorPicker2.SelectedColor);
  imgMain.Picture.Assign(ImgBitmap2);
end;

procedure TfrmMain.imgMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    if not Assigned(ImgBitmap) then
      Exit;
    if (X >= ImgBitmap.Width) or (Y >= ImgBitmap.Height) then
      Exit;
    SelectedRegion := ImgLabels[X][Y];
    if SelectedRegion = 0 then
      Exit;
    FreeAndNil(ImgBitmap);
    ImgBitmap := TBGRABitmap.Create('tmp.tga');
    imgMain.Picture.Assign(ImgBitmap);
    for X := 0 to Pred(Img.Width) do
      for Y := 0 to Pred(Img.Height) do
        if ImgLabels[X][Y] <> SelectedRegion then
          ImgBitmap.ScanLine[Y][X].alpha := 64;
    imgMain.Picture.Assign(ImgBitmap);
  end;
end;

end.
