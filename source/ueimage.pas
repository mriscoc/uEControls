{------------------------------------------------------------------------------
  uEImage v1.0  03/05/2019
  Author: Miguel A. Risco-Castillo
  http://ue.accesus.com/uecontrols

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
------------------------------------------------------------------------------}

unit uEImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Types, BGRABitmap, BGRABitmapTypes, uEBase, LCLType, LCLIntf, math;

type

  { TCustomuEImage }

  TCustomuEImage = class(TuEBaseControl)
  private
    FCenter: Boolean;
    FImage: TBitmap;
    FOnImageChanged: TNotifyEvent;
    FProportional: Boolean;
    FStretch: Boolean;
    function GetCanvas: TCanvas;
    procedure SetCenter(AValue: Boolean);
    procedure SetImage(const AValue: TBitmap);
    procedure SetProportional(AValue: Boolean);
    procedure SetStretch(AValue: Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure ImageChanged(Sender : TObject); virtual;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure DrawControl; override;
    property Canvas: TCanvas read GetCanvas;
    //property BorderSpacing;
    property Image: TBitmap read FImage write SetImage;
    property OnImageChanged: TNotifyEvent read FOnImageChanged write FOnImageChanged;
    property Center: Boolean read FCenter write SetCenter;
    property Proportional: Boolean read FProportional write SetProportional;
    property Stretch: Boolean read FStretch write SetStretch;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFromFile(f:string):boolean; virtual;
    function DestRect: TRect; override;
  end;


  { TuEImage }

  TuEImage = class(TCustomuEImage)
  published
    property Debug;
    property Align;
    property Anchors;
    property AutoSize;
    property Bitmap;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property Image;
    property PopupMenu;
    property ShowHint;
    property Transparent;
    property Visible;
    property Center;
    property Proportional;
    property Stretch;
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
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses LCLProc;

const DefaultSize=90;

constructor TCustomuEImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FImage:= TBitmap.Create;
  FImage.Clear;
  FImage.TransparentMode:= tmFixed;
  FImage.OnChange := @ImageChanged;
  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomuEImage.Destroy;
begin
  FImage.OnChange := nil;
  FreeThenNil(FImage);
  inherited Destroy;
end;

function TCustomuEImage.LoadFromFile(f: string): boolean;
begin
  result:=false;
  try
    Bitmap.LoadFromFile(f);
  except
    exit;
  end;
  AssignBGRAtoImage(Bitmap,FImage);
  result:=true;
end;

function TCustomuEImage.DestRect: TRect;
var
  BitmapW,BitmapH: Integer;
  x,y,w,h:integer;
  s:real;
//Bitmap es la imagen final
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
  end else
  begin
    w:=BitmapW;
    h:=BitmapH;
  end;
  if FCenter then
  begin
    x:=(ClientWidth  - w) div 2;
    y:=(ClientHeight - h) div 2;
  end else
  begin
    x:=0;
    y:=0;
  end;
  Result:=Rect(x,y,x+w,y+h);
end;

procedure TCustomuEImage.DrawControl;
var
  R:TRect;
begin
  Bitmap.SetSize(0,0); // Evita que uEBase redibuje Bitmap
  If  assigned(FImage) and (FImage.Width>0) and (FImage.Height > 0) then
  begin
    Bitmap.assign(FImage);
    R:=DestRect;
    Bitmap:=Bitmap.Resample(R.Width,R.Height) as TBGRABitmap;
  end;
  inherited;
end;

function TCustomuEImage.GetCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

procedure TCustomuEImage.SetCenter(AValue: Boolean);
begin
  if FCenter=AValue then Exit;
  FCenter:=AValue;
  invalidate;
end;

procedure TCustomuEImage.SetImage(const AValue: TBitmap);
begin
  if (FImage <> nil) and (FImage=AValue) then exit;
  FImage.Assign(AValue);
end;

procedure TCustomuEImage.SetProportional(AValue: Boolean);
begin
  if FProportional=AValue then Exit;
  FProportional:=AValue;
  invalidate;
end;

procedure TCustomuEImage.SetStretch(AValue: Boolean);
begin
  if FStretch=AValue then Exit;
  FStretch:=AValue;
  invalidate;
end;

procedure TCustomuEImage.ImageChanged(Sender: TObject);
begin
  if Assigned(OnImageChanged) then OnImageChanged(Self);
  invalidate;
end;

procedure TCustomuEImage.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := FImage.Width;
  PreferredHeight := FImage.Height;
end;

class function TCustomuEImage.GetControlClassDefaultSize: TSize;
begin
  Result.CX := DefaultSize;
  Result.CY := DefaultSize;
end;

class procedure TCustomuEImage.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

end.

