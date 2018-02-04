
{------------------------------------------------------------------------------

  Miguel A. Risco Castillo TestRotImage v1.3
  http://ue.accesus.com/uecontrols

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

------------------------------------------------------------------------------}

unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRABitmap, BGRABitmapTypes,
  ExtCtrls, ComCtrls, ExtDlgs, uERotImage;

type

  { TTestRotImage }

  TTestRotImage = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    edDebug: TCheckBox;
    ColorButton1: TColorButton;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    StaticText1: TStaticText;
    TrackBar1: TTrackBar;
    uERotImage1: TuERotImage;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure edDebugChange(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure uERotImage1BeforeRotation(Sender: TObject);
    procedure uERotImage1Rotation(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  TestRotImageF: TTestRotImage;

implementation

{$R *.lfm}

{ TTestRotImage }

procedure TTestRotImage.SpinEdit1Change(Sender: TObject);
begin
  uERotImage1.Angle:=TrackBar1.Position;
  StaticText1.Caption:=inttostr(TrackBar1.Position);
end;

procedure TTestRotImage.uERotImage1BeforeRotation(Sender: TObject);
var x,y: integer;
    p: PBGRAPixel;
begin
  if Checkbox2.Checked then for y := 0 to ueRotImage1.Bitmap.Height-1 do
  begin
    p := ueRotImage1.Bitmap.Scanline[y];
    for x := 0 to ueRotImage1.Bitmap.Width-1 do
    begin
      p^.red := x*256 div ueRotImage1.Bitmap.Width;
      p^.green := y*256 div ueRotImage1.Bitmap.Height;
      p^.blue := 256-(x*256 div ueRotImage1.Bitmap.Width);
      p^.alpha := 255;
      inc(p);
    end;
  end;
end;

procedure TTestRotImage.uERotImage1Rotation(Sender: TObject);
var layer: TBGRABitmap;
begin
  if Checkbox3.Checked then
  begin
    layer := TBGRABitmap.Create(ueRotImage1.Bitmap.Width,ueRotImage1.Bitmap.Height);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       BGRA(240,240,240),BGRA(0,0,0),
                       gtRadial, PointF(layer.Width*2/5,layer.Height*2/5), PointF(layer.Width*1/2,layer.Height*1/2),
                       dmSet);
    ueRotImage1.Bitmap.BlendImage(0,0,layer,boColorDodge);
    layer.Free;
  end;
end;


procedure TTestRotImage.CheckBox1Change(Sender: TObject);
begin
  uERotImage1.Transparent:= Checkbox1.Checked;
end;

procedure TTestRotImage.CheckBox2Change(Sender: TObject);
begin
  uERotImage1.ReDraw;
end;

procedure TTestRotImage.CheckBox4Change(Sender: TObject);
begin
  uERotImage1.Center:=Checkbox4.Checked;
end;

procedure TTestRotImage.CheckBox5Change(Sender: TObject);
begin
  uERotImage1.UniqueSize:=Checkbox5.Checked;
end;

procedure TTestRotImage.ColorButton1ColorChanged(Sender: TObject);
begin
  uERotImage1.Color:=ColorButton1.ButtonColor;
end;

procedure TTestRotImage.edDebugChange(Sender: TObject);
begin
  uERotImage1.Debug:=edDebug.Checked;
end;

procedure TTestRotImage.Image1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then uERotImage1.LoadFromFile(OpenPictureDialog1.FileName);
end;

end.



