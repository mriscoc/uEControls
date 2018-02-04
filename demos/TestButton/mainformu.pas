unit mainformu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Buttons, uebutton, uETileImage, uERotImage, ueBase,
  BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    CB_Skin: TCheckBox;
    StaticText1: TStaticText;
    uEButton1: TuEButton;
    uEButton2: TuEButton;
    uERotImage1: TuERotImage;
    uETileImage1: TuETileImage;
    procedure CB_SkinChange(Sender: TObject);
    procedure uERotImage1BeforeRotation(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CB_SkinChange(Sender: TObject);
begin
  if CB_Skin.Checked then
    uEButton2.LoadImageFromBGRA(uERotImage1.Bitmap)
  else
    uEButton2.Image.Clear;
end;

procedure TForm1.uERotImage1BeforeRotation(Sender: TObject);
var
  i,h,y:integer;
  r:integer;
  c0,c1,c2,c3:TBGRAPixel;
begin
  With uERotImage1 do
  begin
    r:=Width;
    h:=Height div 4;
    Bitmap.SetSize(r,h*4);
    for i:=0 to 3 do
    begin
      y:=h*i;
      case i of
        0:begin
            c0:=BGRAPixelTransparent;
            c1:=ColorToBGRA(clSilver,50);
            c2:=BGRAPixelTransparent;
            c3:=BGRAPixelTransparent;
          end;
        1:begin
            c0:=ColorToBGRA(clMaroon);     // Background
            c1:=ColorToBGRA(clSilver);     // MainFrame
            c2:=ColorToBGRA(clRed);        // InnerFrame
            c3:=ColorToBGRA(clYellow);     // BotomGradient c3 to c0
          end;
        2:begin
            c0:=ColorToBGRA(clNavy);        // Background
            c1:=ColorToBGRA(clSkyBlue);     // MainFrame
            c2:=ColorToBGRA(clTeal);        // InnerFrame
            c3:=CSSCyan;                    // BotomGradient c3 to c0
          end;
        3:begin
          c0:=ColorToBGRA(clGray);          // Background
          c1:=ColorToBGRA(clMedGray);       // MainFrame
          c2:=BGRAWhite;                    // InnerFrame / TopGradient c2 to 25% c3
          c3:=ColorToBGRA(clSilver);        // BotomGradient c3 to c0
          end;
      end;
      Bitmap.FillRect(2,y+2,r-2,y+h-2,c0,dmSet); //Background
      Bitmap.GradientFill(2,y+2,r-2,y+h-2,c3,c0,gtRadial,PointF(r div 2,y+h),PointF(r,y+h),dmset); //BotomGradient
      Bitmap.GradientFill(1,y+2,r-1,y+(h div 2),c3,MergeBGRA(c3,1,BGRAPixelTransparent,4),gtLinear,PointF(0,y),PointF(0,y+(h div 2)),dmDrawWithTransparency);  //TopGradient
      Bitmap.RoundRect(1,y+1,r-1,y+h-1,5,5,c2);  //Innerframe
      Bitmap.RoundRect(0,y,r,y+h,8,8,c1);        //Mainframe
    end;
  end;
end;

end.

