{------------------------------------------------------------------------------
  TuERotImage v1.5  2019-05-03
  Author: Miguel A. Risco-Castillo
  http://ue.accesus.com/uecontrols

  using some ideas from:
  TRotateImage v1.54 by Kambiz R. Khojasteh

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

  v1.5 03/05/2019
  Remove deprecated property: TPicture
  Change default transparency mode

  v1.4 15/04/2019
  Add default nil value to temporary image in loadfromfile

  v1.3
  Fix offset (changes in BGRABitmap PutImageAngle)

  v1.2
  Fix loadfromfile
  Remove Picture Property
------------------------------------------------------------------------------}

unit uERotImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Types, BGRABitmap, BGRABitmapTypes, uEBase, Math;

type

  { TCustomuERotImage }

  TCustomuERotImage = class(TuEBaseControl)
  private
    FImage: TBitmap;
    FOffsetX: integer;
    FOffsetY: integer;
    FOnImageChanged: TNotifyEvent;
    FStretch: Boolean;
    FCenter: Boolean;
    FProportional: Boolean;
    FAngle: Extended;
    FUniqueSize: Boolean;
    FMaxSize: Integer;
    FOnRotation: TNotifyEvent;
    FOnBeforeRotation: TNotifyEvent;
    procedure BitMapSetSize;
    function GetCanvas: TCanvas;
    procedure SetCenter(const AValue: Boolean);
    procedure SetImage(const AValue: TBitmap);
    procedure SetOffsetX(AValue: integer);
    procedure SetOffsetY(AValue: integer);
    procedure SetStretch(const AValue: Boolean);
    procedure SetProportional(const AValue: Boolean);
    procedure SetAngle(const Value: Extended);
    procedure SetUniqueSize(Value: Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure ImageChanged(Sender : TObject); virtual;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect: TRect; override;
    procedure RenderControl; override;
    procedure DoRotation; virtual;
    procedure DoBeforeRotation; virtual;
    property Canvas: TCanvas read GetCanvas;
    property MaxSize: Integer read FMaxSize;
    property Angle: Extended read FAngle write SetAngle;
    property OffsetX:integer read FOffsetX write SetOffsetX default 0;
    property OffsetY:integer read FOffsetY write SetOffsetY default 0;
    property BorderSpacing;
    property Center: Boolean read FCenter write SetCenter default False;
    property Image: TBitmap read FImage write SetImage;
    property Proportional: Boolean read FProportional write setProportional default False;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property UniqueSize: Boolean read FUniqueSize write SetUniqueSize default False;
    property OnImageChanged: TNotifyEvent read FOnImageChanged write FOnImageChanged;
    property OnRotation: TNotifyEvent read FOnRotation write FOnRotation;
    property OnBeforeRotation: TNotifyEvent read FOnBeforeRotation write FOnBeforeRotation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFromFile(f:string):boolean; virtual;
    procedure ForceRotate(AValue: Extended);
  end;


  { TuECustomRotImage }

  TuERotImage = class(TCustomuERotImage)
  published
    property Debug;
    property Align;
    property Anchors;
    property Angle;
    property OffsetX;
    property OffsetY;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property MaxSize;
    property ParentColor;
    property ParentShowHint;
    property Image;
    property Bitmap;
    property PopupMenu;
    property Proportional;
    property ShowHint;
    property Stretch;
    property Transparent;
    property UniqueSize;
    property Visible;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnImageChanged;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnRotation;
    property OnBeforeRotation;
    property OnStartDock;
    property OnStartDrag;
  public
  end;


implementation

uses LCLProc;

const DefaultSize=90;

constructor TCustomuERotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FAngle := 0;
  FCenter := False;
  FProportional := False;
  FStretch := False;
  FUniqueSize := False;
  FImage := TBitmap.Create;
  FImage.TransparentMode:= tmFixed;
  //FImage.Canvas.Pixels[0,0]:=clblack;
  //FImage.TransparentColor:=clFuchsia;
  FImage.OnChange := @ImageChanged;
  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomuERotImage.Destroy;
begin
  FImage.OnChange := nil;
  FreeThenNil(FImage);
  inherited Destroy;
end;

function TCustomuERotImage.LoadFromFile(f: string): boolean;
var
  tbmp:TBGRABitmap=nil;
begin
  result:=false;
  try
    tbmp:=TBGRABitmap.Create(f);
    AssignBGRAtoImage(tbmp,FImage);
    result:=true;
  finally
    if assigned(tbmp) then FreeAndNil(tbmp);
  end;
end;

procedure TCustomuERotImage.RenderControl;
var
  w,h:integer;
  xc,yc,ox,oy:real;
  tbmp:TBGRABitmap;
begin
  if (csLoading in ComponentState) or (csCreating in FControlState) or IsUpdating then Exit;
  If not assigned(Bitmap) or not assigned(FImage) then exit;
  BitMapSetSize;
  Bitmap.Fill(BGRAPixelTransparent);
  DoBeforeRotation;
  //
  tbmp:=TBGRABitmap.Create;
  if not FImage.Empty then tbmp.Assign(FImage)
  else begin
    tbmp.Assign(Bitmap);
    Bitmap.Fill(BGRAPixelTransparent);
  end;
  w:=tbmp.width;
  h:=tbmp.height;
  xc:=w div 2;
  yc:=h div 2;
  ox:=(Bitmap.Width div 2)-xc;
  oy:=(Bitmap.Height div 2)-yc;
  if (w mod 2)=0 then xc:=xc-0.5;
  if (h mod 2)=0 then yc:=yc-0.5;
  Bitmap.PutImageAngle(ox,oy,tbmp,FAngle,xc,yc,255,true,true);
  if Debug then
  begin
    Bitmap.Rectangle(0,0,Bitmap.Width,Bitmap.Height,ColorToBGRA(clBlue),dmSet);
  end;
  if assigned (tbmp) then tbmp.free;
  //
  DoRotation;
  inherited RenderControl;
end;

procedure TCustomuERotImage.BitMapSetSize;
var
  iw,ih:integer;
  w,h:integer;
  rad,s,c:Extended;
begin
  if FImage.Empty then
  begin
    FMaxSize:=Round(Sqrt(Sqr(ClientWidth) + Sqr(ClientHeight)))+1;
    Bitmap.SetSize(FMaxSize,FMaxSize);
    exit;
  end else
  begin
    FMaxSize := Round(Sqrt(Sqr(FImage.Width) + Sqr(FImage.Height)));
    if UniqueSize then Bitmap.SetSize(FMaxSize,FMaxSize)
    else begin
      iw:=FImage.width;
      ih:=FImage.height;
      rad:=FAngle*PI/180;
      s:=abs(sin(rad));
      c:=abs(cos(rad));
      w:= round(2*(iw*c/2 + ih*s/2));
      h:= round(2*(iw*s/2 + ih*c/2));
      Bitmap.SetSize(w,h);
    end;
  end;
  if AutoSize
  then begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
end;

function TCustomuERotImage.GetCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

procedure TCustomuERotImage.SetCenter(const AValue: Boolean);
begin
  if FCenter = AValue then exit;
  FCenter := AValue;
  RenderControl;
end;

procedure TCustomuERotImage.SetImage(const AValue: TBitmap);
begin
  if (FImage <> nil) and (FImage=AValue) then exit;
  FImage.Assign(AValue);
end;

procedure TCustomuERotImage.SetOffsetX(AValue: integer);
begin
  if FOffsetX=AValue then Exit;
  FOffsetX:=AValue;
  RenderControl;
end;

procedure TCustomuERotImage.SetOffsetY(AValue: integer);
begin
  if FOffsetY=AValue then Exit;
  FOffsetY:=AValue;
  RenderControl;
end;

procedure TCustomuERotImage.SetStretch(const AValue: Boolean);
begin
  if FStretch = AValue then exit;
  FStretch := AValue;
  invalidate;
end;

procedure TCustomuERotImage.SetProportional(const AValue: Boolean);
begin
  if FProportional = AValue then exit;
  FProportional := AValue;
  invalidate;
end;

procedure TCustomuERotImage.ForceRotate(AValue: Extended);
begin
  FAngle:=AValue;
  RenderControl;
end;

procedure TCustomuERotImage.SetAngle(const Value: Extended);
begin
  if Value <> FAngle then
  begin
    FAngle := Value;
    RenderControl;
  end;
end;

procedure TCustomuERotImage.SetUniqueSize(Value: Boolean);
begin
  if Value <> FUniqueSize then
  begin
    FUniqueSize := Value;
    RenderControl;
  end;
end;

procedure TCustomuERotImage.ImageChanged(Sender: TObject);
begin
  FImage.OnChange := nil;
  RenderControl;
  if Assigned(OnImageChanged) then OnImageChanged(Self);
  FImage.OnChange := @ImageChanged;
end;

procedure TCustomuERotImage.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := Bitmap.Width;
  PreferredHeight := Bitmap.Height;
end;

class function TCustomuERotImage.GetControlClassDefaultSize: TSize;
begin
  Result.CX := DefaultSize;
  Result.CY := DefaultSize;
end;

function TCustomuERotImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (MaxSize <> 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      if UniqueSize then
        NewWidth := MaxSize
      else
        NewWidth := Bitmap.Width;
    if Align in [alNone, alTop, alBottom] then
      if UniqueSize then
        NewHeight := MaxSize
      else
        NewHeight := Bitmap.Height;
  end;
end;

procedure TCustomuERotImage.DoRotation;
begin
  if Assigned(FOnRotation) then FOnRotation(Self);
end;

procedure TCustomuERotImage.DoBeforeRotation;
begin
  if Assigned(FOnBeforeRotation) then FOnBeforeRotation(Self);
end;

class procedure TCustomuERotImage.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

function TCustomuERotImage.DestRect: TRect;
var
  BitmapW,BitmapH: Integer;
  x,y,w,h:integer;
  s:real;
//Bitmap es la imagen rotada
//FImage es la imagen original
begin
  if not Assigned(Bitmap) then exit;
  BitmapW := Bitmap.Width;
  BitmapH := Bitmap.Height;

  if FStretch then
  begin
    if FProportional then
    begin
      s:=min(ClientWidth/BitmapW,ClientHeight/BitmapH);
      w:=Round(BitmapW*s);
      h:=Round(BitmapH*s);
    end else
    begin
      w:=ClientWidth;
      h:=ClientHeight;
    end;
    if FCenter then
    begin
      x:=ClientWidth div 2 - w div 2;
      y:=ClientHeight div 2 - h div 2;
    end else
    begin
      x:=0;
      y:=0;
    end;
  end else
  begin
    w:=BitmapW;
    h:=BitmapH;
    if FCenter then
    begin
      x:=ClientWidth div 2 - w div 2-1;
      y:=ClientHeight div 2 - h div 2-1;
    end else
    begin
      x:=FOffsetX-w div 2-1;
      y:=FOffsetY-h div 2-1;
    end;
  end;
  Result:=Rect(x,y,x+w,y+h);
end;

end.

