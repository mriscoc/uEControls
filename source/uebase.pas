{------------------------------------------------------------------------------
  uEBase v1.2.1  2018-08-30
  Author: Miguel A. Risco-Castillo
  http://ue.accesus.com/uecontrols

  Base unit for uE Controls

  THE COPYRIGHT NOTICES IN THE SOURCE CODE MAY NOT BE REMOVED OR MODIFIED.
  IF YOU MODIFY AND/OR DISTRIBUTE THE CODE TO ANY THIRD PARTY THEN YOU MUST NOT
  VEIL THE ORIGINAL AUTHOR. IT MUST ALWAYS BE CLEARLY IDENTIFIABLE.

  The contents of this file are subject in priority to the License in this header,
  in the license.txt file and the Mozilla Public License Version 1.1 (MPL);
  you may not use this file except in compliance with these licenses. You may obtain
  a copy of the MPL License at http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the Licenses is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the Licenses for
  the specific language governing rights and limitations under the Licenses.
  
  Release Notes
  
  v1.2.1 2018-08-30
  Update cAbout
  
  v1.2.0 2018-01-18  
------------------------------------------------------------------------------}

unit uEBase;
{$mode objfpc}{$H+}

interface

uses
  Dialogs,
  Classes, SysUtils, Controls, LCLProc, LCLType, Graphics, BGRABitmap, BGRABitmapTypes;

const
  cAbout='uEControls v6.2 (c) Miguel A. Risco-Castillo'+LineEnding+'http://ue.accesus.com/uecontrols';

type

  { TuEBaseControl }

  TuEBaseControl = class(TGraphicControl)
  private
    FAbout:string;
    FBGRAColor: TBGRAPixel;
    FDebug:boolean;
    FUpdateCount: Integer;
    FTransparent: Boolean;
    FBitmap: TBGRABitmap;
    function GetAbout: string;
  protected
    procedure DoOnResize; override;
    function DestRect: TRect; virtual;
    procedure Paint; override;
    procedure DrawControl; virtual;
    procedure RenderControl; virtual;
    procedure SetDebug(AValue: boolean);
    procedure SetTransparent(const AValue: Boolean);
    procedure SetColor(AValue: TColor); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetBGRAColor(AValue: TBGRAPixel);
    procedure SetBitmap(AValue: TBGRABitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure UpdateControl; virtual;
    procedure ReDraw; virtual;
    function IsUpdating: Boolean;
    property BGRAColor:TBGRAPixel read FBGRAColor write SetBGRAColor;
  published
    property Bitmap:TBGRABitmap read FBitmap write SetBitmap;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
//  This property allow to use rulers for properly alignment of images
    property Debug:boolean read FDebug write SetDebug;
//  uEControls v6.0 (c) Miguel A. Risco-Castillo http://ue.accesus.com/uecontrols'
    property About:string read FAbout; // This property must not be removed to follow the licence statements
  end;

{support}

procedure AssignFontToBGRA(Source: TFont; Dest: TBGRABitmap);
procedure AssignBGRAtoImage(Source:TBGRABitmap; Dest:TBitmap);
function Darken(Color:TColor; Percent:Byte):TBGRAPixel;

implementation

{support}
procedure AssignFontToBGRA(Source: TFont; Dest: TBGRABitmap);
begin
  Dest.FontAntialias := True;

  Dest.FontName := Source.Name;
  Dest.FontStyle := Source.Style;
  Dest.FontOrientation := Source.Orientation;

  case Source.Quality of
    fqNonAntialiased: Dest.FontQuality := fqSystem;
    fqAntialiased: Dest.FontQuality := fqFineAntialiasing;
    fqProof: Dest.FontQuality := fqFineClearTypeRGB;
    fqDefault, fqDraft, fqCleartype, fqCleartypeNatural: Dest.FontQuality :=
        fqSystemClearType;
  end;

  Dest.FontHeight := -Source.Height;
end;

procedure AssignBGRAtoImage(Source: TBGRABitmap; Dest: TBitmap);
var TempBitmap:TBitmap;
begin
  try
    TempBitmap := TBitmap.Create;
    With TempBitmap
    do begin
      PixelFormat:=pf32bit;      //Enable transparent bitmap
      SetSize(Source.Width,Source.Height);
      Canvas.Pixels[0,0]:=clBlack;
    end;
//    Source.SetPixel(0,0,BGRAPixelTransparent);
    Source.Draw(TempBitmap.Canvas,0,0,true); //Replace with Source
    Dest.Assign(TempBitmap);
  finally
    if assigned(TempBitmap) then FreeThenNil(TempBitmap);
  end;
end;

function Darken(Color:TColor; Percent:Byte):TBGRAPixel;
begin
  Result:=ColorToBGRA(ColorToRGB(Color));
  With Result do
  begin
    red:=red-muldiv(red,Percent,100);  //Percent% closer to black
    green:=green-muldiv(green,Percent,100);
    blue:=blue-muldiv(blue,Percent,100);
  end;
end;

{ TuEBaseControl }

constructor TuEBaseControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDebug:=false;
  FAbout:=cAbout;
  FTransparent:= True;
  FBitmap:=TBGRABitmap.Create;
end;

destructor TuEBaseControl.Destroy;
begin
  if Assigned(FBitmap) then FreeAndNil(FBitMap);
  inherited Destroy;
end;

function TuEBaseControl.GetAbout: string;
begin
  Result:=About;
end;

procedure TuEBaseControl.SetBitmap(AValue: TBGRABitmap);
begin
  if FBitmap=AValue then Exit;
  FBitmap:=AValue;
  Invalidate;
end;

procedure TuEBaseControl.SetDebug(AValue: boolean);
begin
  if FDebug=AValue then Exit;
  FDebug:=AValue;
  RenderControl;
  invalidate;
end;

procedure TuEBaseControl.SetTransparent(const AValue: Boolean);
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  RenderControl;
  invalidate;
end;

procedure TuEBaseControl.SetColor(AValue: TColor);
begin
  if inherited Color = AValue then exit;
  inherited SetColor(AValue);
  FBGRAColor := ColortoBGRA(ColorToRGB(AValue));
  RenderControl;
end;

procedure TuEBaseControl.SetAutoSize(Value: Boolean);
begin
  If AutoSize=Value then exit;
  inherited SetAutoSize(Value);
  RenderControl;
end;

procedure TuEBaseControl.SetBGRAColor(AValue: TBGRAPixel);
begin
  if FBGRAColor=AValue then Exit;
  inherited SetColor(BGRAtoColor(AValue));
  FBGRAColor:=AValue;
  RenderControl;
end;

procedure TuEBaseControl.DoOnResize;
begin
  inherited DoOnResize;
  RenderControl;
end;

function TuEBaseControl.DestRect: TRect;
begin
  Result:=Rect(0,0,ClientWidth-1,ClientHeight-1);
end;

procedure TuEBaseControl.Paint;
begin
  if (csCreating in FControlState) or IsUpdating then Exit;
  DrawControl;
  inherited Paint;
end;

procedure TuEBaseControl.DrawControl;
var
  w,h,xc,yc:integer;
  procedure DrawFrame;
  begin
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(w, 0);
      LineTo(w, h);
      LineTo(0, h);
      LineTo(0, 0);
    end;
  end;
  procedure DrawRuler;
  begin
    with Canvas do
    begin
      Pen.Color := clRed;
      Pen.Style := psDot;
      MoveTo(0, 0);
      LineTo(w, 0);
      LineTo(w, h);
      LineTo(0, h);
      LineTo(0, 0);
      MoveTo(xc,0);
      LineTo(xc,h);
      MoveTo(0,yc);
      LineTo(w,yc);
    end;
  end;
begin
  w:=Self.Width-1;
  h:=Self.Height-1;
  xc:=w div 2;
  yc:=h div 2;
  if not FTransparent then
  begin
    Canvas.Brush.Color:=Color;
    Canvas.FillRect(0,0,w,h);
  end;
  if assigned(FBitmap) then FBitmap.Draw(Canvas,DestRect,false);
  if csDesigning in ComponentState then DrawFrame;
  if FDebug then DrawRuler;
end;

procedure TuEBaseControl.RenderControl;
begin
  Invalidate;
end;

procedure TuEBaseControl.BeginUpdate;
begin
  FUpdateCount += 1;
end;

procedure TuEBaseControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    FUpdateCount -= 1;
    if FUpdateCount=0 then
      UpdateControl;
  end;
end;

procedure TuEBaseControl.UpdateControl;
begin
  Invalidate;
end;

function TuEBaseControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount>0;
end;

procedure TuEBaseControl.ReDraw;
begin
  RenderControl;
end;

end.

