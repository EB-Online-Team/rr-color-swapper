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

unit Forms.Main;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ActnList, StdActns, StdCtrls, Buttons, ComCtrls, LazFileUtils, Imaging,
  ImagingClasses, ImagingComponents, BGRABitmap, BGRABitmapTypes,
  mbColorPreview, mbDeskPickerButton, HColorPicker, RRCS.Labeling,
  RRCS.Swapping;

type
  TfrmMain = class(TForm)
    alMain: TActionList;
    actExit: TFileExit;
    actOpen: TFileOpen;
    btnReset: TButton;
    hcpSwappedColorPicker: THColorPicker;
    imgOriginal: TImage;
    imgSwapped: TImage;
    Label1: TLabel;
    lblOriginal: TLabel;
    lblSwapped: TLabel;
    clrOriginalPrimary: TmbColorPreview;
    clrSwappedPrimary: TmbColorPreview;
    btnOriginalColorPicker: TmbDeskPickerButton;
    mnuMain: TMainMenu;
    mnuMainFile: TMenuItem;
    mnuMainFileExit: TMenuItem;
    mnuMainFileOpen: TMenuItem;
    mnuMainFileSave: TMenuItem;
    mnuMainFileSaveAs: TMenuItem;
    mnuMainHelp: TMenuItem;
    mnuMainHelpAbout: TMenuItem;
    pnlOriginalConfig: TPanel;
    pnlSwappedConfig: TPanel;
    pnlOriginal: TPanel;
    pnlSwapped: TPanel;
    splMain: TSplitter;
    sbMain: TStatusBar;
    TrackBar1: TTrackBar;
    procedure actOpenAccept(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure hcpSwappedColorPickerChange(Sender: TObject);
    procedure imgOriginalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure btnOriginalColorPickerSelColorChange(Sender: TObject);
    procedure mnuMainHelpAboutClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    OriginalPath, SwappedPath, SwappedDir: string;
    SIOriginal, SISwapped: TSingleImage;
    BMPOriginal, BMPSwapped: TBGRABitmap;
    ImgLabels: TIntMatrix;
    SelectedRegion: integer;
    Threshold: double;
  public
  end;

var
  frmMain: TfrmMain;

const
  APP_TITLE = 'RR Color Swapper';
  APP_VERSION = '0.1.0';
  APP_AUTHOR = 'Vartan Haghverdi';
  APP_COPYRIGHT = 'Copyright (C) 2024-2025 ' + APP_AUTHOR;
  TMP_ORIGINAL_PATH = 'tmp.tga';
  TMP_SWAPPED_PATH = 'tmp_swapped.tga';

implementation

{$R *.lfm}

procedure TfrmMain.mnuMainHelpAboutClick(Sender: TObject);
const
  AboutMessage: string = APP_TITLE + ' v' + APP_VERSION + LineEnding + APP_COPYRIGHT;
begin
  MessageDlg('About', AboutMessage, mtInformation, [mbOK], 0);
end;

procedure TfrmMain.TrackBar1Change(Sender: TObject);
begin
  Threshold := TrackBar1.Position / 100.0;
  Label1.Caption := 'Threshold: ' + IntToStr(TrackBar1.Position) + '%';
end;

procedure TfrmMain.actOpenAccept(Sender: TObject);
begin
  // free prior images (if any)
  FreeAndNil(SIOriginal);
  FreeAndNil(SISwapped);
  FreeAndNil(BMPOriginal);
  FreeAndNil(BMPSwapped);
  imgOriginal.Picture.Clear;
  imgSwapped.Picture.Clear;

  // Save original filename and assemble output filename.
  OriginalPath := (Sender as TFileOpen).Dialog.FileName;
  SwappedDir := ConcatPaths([ExtractFileDir(OriginalPath), 'swapped']);
  if not DirectoryExists(SwappedDir) then
    CreateDir(SwappedDir);
  SwappedPath := ConcatPaths([SwappedDir, ExtractFileName(OriginalPath)]);

  // load two copies of the image (one original, one to hold swapped colors)
  SIOriginal := TSingleImage.CreateFromFile((Sender as TFileOpen).Dialog.FileName);
  SISwapped := TSingleImage.CreateFromImage(SIOriginal);

  // use a temporary TGA to load the image onto the form and extract regions
  if SIOriginal.SaveToFile(TMP_ORIGINAL_PATH) then
  begin
    BMPOriginal := TBGRABitmap.Create(TMP_ORIGINAL_PATH);
    BMPSwapped := TBGRABitmap.Create(BMPOriginal);
    imgOriginal.Picture.Assign(BMPOriginal);
    imgSwapped.Picture.Assign(BMPSwapped);
    ImgLabels := LabelImage(BMPOriginal);
  end
  else
    ShowMessage('Error while opening image.');
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  // exit if no image has been loaded
  if not Assigned(SISwapped) then Exit;

  FreeAndNil(SISwapped);
  FreeAndNil(BMPSwapped);
  SISwapped := TSingleImage.CreateFromImage(SIOriginal);
  BMPOriginal := TBGRABitmap.Create(TMP_ORIGINAL_PATH);
  BMPSwapped := TBGRABitmap.Create(BMPOriginal);
  imgOriginal.Picture.Assign(BMPOriginal);
  imgSwapped.Picture.Assign(BMPSwapped);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Threshold := 0.03;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Delete temporary TGA file that was used during runtime as an image buffer.
  DeleteFile(TMP_ORIGINAL_PATH);
  DeleteFile(TMP_SWAPPED_PATH);
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  // free prior images (if any)
  FreeAndNil(SIOriginal);
  FreeAndNil(SISwapped);
  FreeAndNil(BMPOriginal);
  FreeAndNil(BMPSwapped);

  // Save original filename and assemble output filename.
  OriginalPath := FileNames[0];
  SwappedDir := ConcatPaths([ExtractFileDir(OriginalPath), 'swapped']);
  if not DirectoryExists(SwappedDir) then
    CreateDir(SwappedDir);
  SwappedPath := ConcatPaths([SwappedDir, ExtractFileName(OriginalPath)]);

  // load two copies of the image (one original, one to hold swapped colors)
  SIOriginal := TSingleImage.CreateFromFile(OriginalPath);
  SISwapped := TSingleImage.CreateFromImage(SIOriginal);

  // use a temporary TGA to load the image onto the form and extract regions
  if SIOriginal.SaveToFile(TMP_ORIGINAL_PATH) then
  begin
    BMPOriginal := TBGRABitmap.Create(TMP_ORIGINAL_PATH);
    BMPSwapped := TBGRABitmap.Create(BMPOriginal);
    imgOriginal.Picture.Assign(BMPOriginal);
    imgSwapped.Picture.Assign(BMPSwapped);
    ImgLabels := LabelImage(BMPOriginal);
  end
  else
    ShowMessage('Error while opening image.');
end;

procedure TfrmMain.hcpSwappedColorPickerChange(Sender: TObject);
begin
  // exit if no image has been loaded
  if not Assigned(SISwapped) then Exit;

  // update color preview to show the new swap color
  clrSwappedPrimary.Color := hcpSwappedColorPicker.SelectedColor;

  // swap colors
  HueSwapInRegion(BMPOriginal, BMPSwapped, ImgLabels, SelectedRegion,
    clrOriginalPrimary.Color, clrSwappedPrimary.Color, Threshold);

  // display updated image
  imgSwapped.Picture.Assign(BMPSwapped);

  // save swapped output
  BMPSwapped.SaveToFile(TMP_SWAPPED_PATH);
  SISwapped.LoadFromFile(TMP_SWAPPED_PATH);
  SISwapped.SaveToFile(SwappedPath);
end;

procedure TfrmMain.imgOriginalMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // only accept left clicks
  if Button <> mbLeft then
    Exit;

  // verify that there is an image
  if not Assigned(BMPOriginal) then
    Exit;

  // verify that the cursor is on the image
  X := X * imgOriginal.Picture.Width div imgOriginal.Width;
  Y := Y * imgOriginal.Picture.Height div imgOriginal.Height;
  if (X >= BMPOriginal.Width) or (Y >= BMPOriginal.Height) then
    Exit;

  // verify the user didn't click on the transparent background
  SelectedRegion := ImgLabels[X][Y];
  if SelectedRegion = 0 then
  begin
    FreeAndNil(BMPOriginal);
    BMPOriginal := TBGRABitmap.Create(TMP_ORIGINAL_PATH);
    imgOriginal.Picture.Assign(BMPOriginal);
    Exit;
  end;

  // set pixels outside the selected region to 50% opacity
  FreeAndNil(BMPOriginal);
  BMPOriginal := TBGRABitmap.Create(TMP_ORIGINAL_PATH);
  for X := 0 to Pred(BMPOriginal.Width) do
    for Y := 0 to Pred(BMPOriginal.Height) do
      if ImgLabels[X][Y] <> SelectedRegion then
        BMPOriginal.ScanLine[Y][X].alpha := 64;

  imgOriginal.Picture.Assign(BMPOriginal);
end;

procedure TfrmMain.btnOriginalColorPickerSelColorChange(Sender: TObject);
begin
  clrOriginalPrimary.Color := btnOriginalColorPicker.SelectedColor;
end;

end.
