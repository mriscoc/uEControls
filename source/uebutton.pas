{------------------------------------------------------------------------------
  uEButton v1.5 2018-11-04
  Author: Miguel A. Risco-Castillo

  Forked from BCImageButton of BGRAControls, Author: Lainz.

  What is new?:
  - support for load images from object inspector
  - can draw default images by it self
  - support for Glyphs
  - support for button layouts
  - shadow for text (use clNone for disable shadow)
  - redraw when properties (caption, font, etc.) are changed

  v1.5 2018-11-04
  - Improved Animation
  - Convert glyph and caption to grayscale when the control is disabled.
  - Verification of fileexists before to attempt to LoadImageFromFile and
    LoadGlyphFromFile.
  
  v1.4 2018-02-04
  - support for load skin and glyph image from bgrabitmap

  v1.3 2018-02-03
  - support for new uebase version
    
  v1.2 2016-01-24
  - support for load multiple file formats to image and glyph
  - Defaultimage must be explicitly called

  v1.1 2015-05-23
  - remove ´&´from caption

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

unit uEButton;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Controls, Graphics, LResources, LMessages, ExtCtrls,
  Types,
  uEBase,
  BGRABitmap, BGRABitmapTypes, BGRASliceScaling;

type
  TuEButtonLayout =  (blGlyphLeft,blGlyphRight,blGlyphTop,blGlyphBottom); 

type
  TuEGraphicButtonState = (gbsNormal, gbsHover, gbsActive, gbsDisabled);

  TOnRenderControl = procedure(Sender: TObject; Bitmap: TBGRABitmap;
    State: TuEGraphicButtonState) of object;

type

  { TuEGraphicButton }

  TuEGraphicButton = class(TuEBaseControl)
  protected
    FState: TuEGraphicButtonState;
    FModalResult: TModalResult;
  protected
    procedure DoClick; virtual;
    procedure DoMouseDown; virtual;
    procedure DoMouseUp; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
  end;

  { TuESliceScalingOptions }

  TuECustomSliceScalingOptions = class(TPersistent)
  protected
    FOwner: TControl;
    FBitmap: TBGRABitmap;
    FAutoDetectRepeat, FRepeatTop, FRepeatLeft, FRepeatMiddleHorizontal,
    FRepeatMiddleVertical, FRepeatRight, FRepeatBottom: boolean;
    FMarginTop, FMarginRight, FMarginBottom, FMarginLeft, FNumberOfItems: integer;
    FDirection: TSliceScalingDirection;
    FDrawMode: TDrawMode;
    FResampleMode: TResampleMode;
    FResampleFilter: TResampleFilter;
  private
    procedure SetFBitmap(AValue: TBGRABitmap);
    procedure SetFMarginBottom(AValue: integer);
    procedure SetFMarginLeft(AValue: integer);
    procedure SetFMarginRight(AValue: integer);
    procedure SetFMarginTop(AValue: integer);
    procedure SetFAutoDetectRepeat(AValue: boolean);
    procedure SetFDirection(AValue: TSliceScalingDirection);
    procedure SetFDrawMode(AValue: TDrawMode);
    procedure SetFNumberOfItems(AValue: integer);
    procedure SetFRepeatBottom(AValue: boolean);
    procedure SetFRepeatLeft(AValue: boolean);
    procedure SetFRepeatMiddleHorizontal(AValue: boolean);
    procedure SetFRepeatMiddleVertical(AValue: boolean);
    procedure SetFRepeatRight(AValue: boolean);
    procedure SetFRepeatTop(AValue: boolean);
    procedure SetFResampleFilter(AValue: TResampleFilter);
    procedure SetFResampleMode(AValue: TResampleMode);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  published
    property Bitmap: TBGRABitmap read FBitmap write SetFBitmap;
    property AutoDetectRepeat: boolean read FAutoDetectRepeat
      write SetFAutoDetectRepeat default False;
    property RepeatTop: boolean read FRepeatTop write SetFRepeatTop default False;
    property RepeatLeft: boolean read FRepeatLeft write SetFRepeatLeft default False;
    property RepeatMiddleHorizontal: boolean
      read FRepeatMiddleHorizontal write SetFRepeatMiddleHorizontal default False;
    property RepeatMiddleVertical: boolean read FRepeatMiddleVertical
      write SetFRepeatMiddleVertical default False;
    property RepeatRight: boolean read FRepeatRight write SetFRepeatRight default False;
    property RepeatBottom: boolean
      read FRepeatBottom write SetFRepeatBottom default False;
    property MarginTop: integer read FMarginTop write SetFMarginTop default 0;
    property MarginRight: integer read FMarginRight write SetFMarginRight default 0;
    property MarginBottom: integer read FMarginBottom write SetFMarginBottom default 0;
    property MarginLeft: integer read FMarginLeft write SetFMarginLeft default 0;
    property NumberOfItems: integer
      read FNumberOfItems write SetFNumberOfItems default 1;
    property Direction: TSliceScalingDirection read FDirection write SetFDirection;
    property DrawMode: TDrawMode read FDrawMode write SetFDrawMode default
      dmDrawWithTransparency;
    property ResampleMode: TResampleMode read FResampleMode
      write SetFResampleMode default rmFineResample;
    property ResampleFilter: TResampleFilter read FResampleFilter
      write SetFResampleFilter default rfBestQuality;
  end;

  { TuEButtonSliceScalingOptions }

  TuEButtonSliceScalingOptions = class(TuECustomSliceScalingOptions)
  private
    procedure SetFCenter(AValue: boolean);
    procedure SetFProportional(AValue: boolean);
    procedure SetFStretch(AValue: boolean);
  protected
    FCenter, FStretch, FProportional: boolean;
  published
    property NumberOfItems: integer read FNumberOfItems default 4;
    property Center: boolean read FCenter write SetFCenter default True;
    property Stretch: boolean read FStretch write SetFStretch default True;
    property Proportional: boolean
      read FProportional write SetFProportional default False;
  public
    constructor Create(AOwner: TControl);
    procedure Assign(Source: TPersistent); override;
  end;

  { TFading }

  TFadingMode = (fmSuspended, fmFadeIn, fmFadeOut, fmFadeInCycle, fmFadeOutCycle, fmFadeInOut, fmFadeOutIn);

  TFading = record
  private
    FAlpha: byte;
    FMode: TFadingMode;
    FAlphaStep: byte;
    FDuration: integer;
    FPrevDate: TDateTime;
    FElapsedMsAccumulator: integer;
  public
    procedure SetFAlpha(AValue: byte);
    procedure SetFMode(AValue: TFadingMode);
    procedure SetFAlphaStep(AValue: byte);
    procedure SetFDuration(AValue: integer);
  public
    function Execute(AStepCount: integer= 1): byte; // execute and return new alpha
    function Reset: byte;   // reset and return new alpha
    procedure PutImage(ADestination: TBGRACustomBitmap; AX,AY: integer; ASource: TBGRACustomBitmap);
    procedure FillRect(ADestination: TBGRACustomBitmap; ARect: TRect; AColor: TBGRAPixel);
  public
    property Alpha: byte read FAlpha write SetFAlpha;
    property Mode: TFadingMode read FMode write SetFMode;
    property Step: byte read FAlphaStep write SetFAlphaStep;
    property Duration: integer read FDuration write SetFDuration;
  end;


  { TuECustomImageButton }

  TuECustomImageButton = class(TuEGraphicButton)
  private
    { Private declarations }
    FBitmapOptions: TuEButtonSliceScalingOptions;
    FBGRAMultiSliceScaling: TBGRAMultiSliceScaling;
    FBGRANormal, FBGRAHover, FBGRAActive, FBGRADisabled: TBGRABitmap;
    FDestRect: TRect;
    FGlyph: TBitmap;
    FImage: TBitmap;
    FLayout: TuEButtonLayout;
    FOnGlyphChanged: TNotifyEvent;
    FOnImageChanged: TNotifyEvent;
    FShowAccelChar: boolean;
    FSpacing: integer;
    FTextShadowColor: Tcolor;
    FTimer: TTimer;
    FFade: TFading;
    FAnimation: boolean;
    FBitmapFile: string;
    FTextVisible: boolean;
    procedure SetFAnimation(AValue: boolean);
    procedure SetFBitmapFile(AValue: string);
    procedure SetFBitmapOptions(AValue: TuEButtonSliceScalingOptions);
    procedure Fade({%H-}Sender: TObject);
    procedure SetFTextVisible(AValue: boolean);
    procedure SetGlyph(AValue: TBitmap);
    procedure SetImage(AValue: TBitmap);
    procedure SetLayout(AValue: TuEButtonLayout);
    procedure SetShowAccelChar(AValue: boolean);
    procedure SetSpacing(AValue: integer);
    procedure SetTextShadowColor(AValue: Tcolor);
    procedure CMTextChanged(var {%H-}Message: TLMessage); message CM_TEXTCHANGED;
  protected
    { Protected declarations }
    procedure DrawControl; override;
    procedure RenderControl; override;
    procedure ImageChanged(Sender : TObject); virtual;
    procedure FontChanged(Sender: TObject); override;
    procedure GlyphChanged(Sender : TObject); virtual;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED; virtual;
    procedure DrawDefaultImage;
    procedure DoMouseDown; override;
    procedure DoMouseUp; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  public
    { Public declarations }
    property Image: TBitmap read FImage write SetImage;
    property OnImageChanged: TNotifyEvent read FOnImageChanged write FOnImageChanged;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property OnGlyphChanged: TNotifyEvent read FOnGlyphChanged write FOnGlyphChanged;
    property BitmapOptions: TuEButtonSliceScalingOptions
      read FBitmapOptions write SetFBitmapOptions;
    property Animation: boolean read FAnimation write SetFAnimation default True;
    property BitmapFile: string read FBitmapFile write SetFBitmapFile;
    property TextVisible: boolean read FTextVisible write SetFTextVisible default True;
    property TextShadowColor:Tcolor read FTextShadowColor write SetTextShadowColor;
    property Spacing:integer read FSpacing write SetSpacing;
    property Layout:TuEButtonLayout read FLayout write SetLayout;
    property ShowAccelChar:boolean read FShowAccelChar write SetShowAccelChar default False;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Draw and use Default Image }
    procedure DefaultImage;
    { It loads the 'BitmapFile' }
    procedure LoadFromBitmapResource(Resource: string; ResourceType: PChar); overload;
    procedure LoadFromBitmapResource(Resource: string); overload;
    procedure LoadFromBitmapFile;
    procedure Assign(Source: TPersistent); override;
    procedure LoadGlyphFromFile(f: string);
    procedure LoadImageFromFile(f: string);
    procedure LoadGlyphFromBGRA(bm: TBGRABitmap);
    procedure LoadImageFromBGRA(bm: TBGRABitmap);
    { Streaming }
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);virtual;
    procedure AssignFromFile(AFileName: string);
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
  end;

  TuEButton = class(TuECustomImageButton)
  published
    property Debug;
    property Image;
    property Glyph;
    property Layout;
    property TextShadowColor;
    property Spacing;
    property Action;
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    property BidiMode;
    property BitmapFile;
    property BitmapOptions;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModalResult;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TextVisible;
    property ShowAccelChar;
    //property Toggle;
    property Visible;
    { The About property must not be removed to follow the licence statements }
    property About;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnGlyphChanged;
    property OnImageChanged;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;

{ support functions }

  function CalculateAspectRatioH(W1, H1, W2: integer): integer; //result H2
  function CalculateAspectRatioW(W1, H1, H2: integer): integer; //result W2
  function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
    Stretch, Proportional, Center: boolean): TRect;

implementation

function CalculateAspectRatioH(W1, H1, W2: integer): integer;
begin
  Result := Round(H1 / W1 * W2);
end;

function CalculateAspectRatioW(W1, H1, H2: integer): integer;
begin
  Result := Round(W1 / H1 * H2);
end;

function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
var
  w: integer;
  h: integer;
begin
  // Stretch or Proportional when Image (Width or Height) is bigger than Destination
  if Stretch or (Proportional and ((ImageW > DestW) or (ImageH > DestH))) then
  begin
    // Proportional when Image (Width or Height) is bigger than 0
    if Proportional and (ImageW > 0) and (ImageH > 0) then
    begin
      w := DestW;
      h := CalculateAspectRatioH(ImageW, ImageH, DestW);
      if h > DestH then
      begin
        h := DestH;
        w := CalculateAspectRatioW(ImageW, ImageH, DestH);
      end;
      ImageW := w;
      ImageH := h;
    end
    // Stretch not Proportional or when Image (Width or Height) is 0
    else
    begin
      ImageW := DestW;
      ImageH := DestH;
    end;
  end;

  Result := Rect(0, 0, ImageW, ImageH);

  // Center: Destination (Width or Height) - Image divided by 2
  if Center then
  begin
    Result.Left := Round((DestW - ImageW) div 2);
    Result.Top := Round((DestH - ImageH) div 2);
  end;
end;


{ TuEButtonSliceScalingOptions }

procedure TuEButtonSliceScalingOptions.SetFCenter(AValue: boolean);
begin
  if FCenter = AValue then
    Exit;
  FCenter := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuEButtonSliceScalingOptions.SetFProportional(AValue: boolean);
begin
  if FProportional = AValue then
    Exit;
  FProportional := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuEButtonSliceScalingOptions.SetFStretch(AValue: boolean);
begin
  if FStretch = AValue then
    Exit;
  FStretch := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TuEButtonSliceScalingOptions.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
  FNumberOfItems := 4;
  FCenter := True;
  FProportional := False;
  FStretch := True;
end;

procedure TuEButtonSliceScalingOptions.Assign(Source: TPersistent);
begin
  if Source is TuEButtonSliceScalingOptions then
  begin
    FAutoDetectRepeat := TuEButtonSliceScalingOptions(Source).AutoDetectRepeat;
    FCenter := TuEButtonSliceScalingOptions(Source).Center;
    FRepeatTop := TuEButtonSliceScalingOptions(Source).RepeatTop;
    FRepeatLeft := TuEButtonSliceScalingOptions(Source).RepeatLeft;
    FRepeatMiddleHorizontal :=
      TuEButtonSliceScalingOptions(Source).RepeatMiddleHorizontal;
    FRepeatMiddleVertical := TuEButtonSliceScalingOptions(
      Source).RepeatMiddleVertical;
    FRepeatRight := TuEButtonSliceScalingOptions(Source).RepeatRight;
    FRepeatBottom := TuEButtonSliceScalingOptions(Source).RepeatBottom;
    FMarginTop := TuEButtonSliceScalingOptions(Source).MarginTop;
    FMarginRight := TuEButtonSliceScalingOptions(Source).MarginRight;
    FMarginBottom := TuEButtonSliceScalingOptions(Source).MarginBottom;
    FMarginLeft := TuEButtonSliceScalingOptions(Source).MarginLeft;
    FDirection := TuEButtonSliceScalingOptions(Source).Direction;
    FDrawMode := TuEButtonSliceScalingOptions(Source).DrawMode;
    FResampleMode := TuEButtonSliceScalingOptions(Source).ResampleMode;
    FResampleFilter := TuEButtonSliceScalingOptions(Source).ResampleFilter;
    FStretch := TuEButtonSliceScalingOptions(Source).Stretch;
    FProportional := TuEButtonSliceScalingOptions(Source).Proportional;
  end
  else
    inherited Assign(Source);
end;

{ TuECustomSliceScalingOptions }

procedure TuECustomSliceScalingOptions.SetFBitmap(AValue: TBGRABitmap);
begin
  if FBitmap = AValue then
    Exit;
  FBitmap := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFMarginBottom(AValue: integer);
begin
  if FMarginBottom = AValue then
    Exit;
  FMarginBottom := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFMarginLeft(AValue: integer);
begin
  if FMarginLeft = AValue then
    Exit;
  FMarginLeft := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFMarginRight(AValue: integer);
begin
  if FMarginRight = AValue then
    Exit;
  FMarginRight := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFMarginTop(AValue: integer);
begin
  if FMarginTop = AValue then
    Exit;
  FMarginTop := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFAutoDetectRepeat(AValue: boolean);
begin
  if FAutoDetectRepeat = AValue then
    Exit;
  FAutoDetectRepeat := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFDirection(AValue: TSliceScalingDirection);
begin
  if FDirection = AValue then
    Exit;
  FDirection := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFDrawMode(AValue: TDrawMode);
begin
  if FDrawMode = AValue then
    Exit;
  FDrawMode := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFNumberOfItems(AValue: integer);
begin
  if FNumberOfItems = AValue then
    Exit;
  FNumberOfItems := AValue;
end;

procedure TuECustomSliceScalingOptions.SetFRepeatBottom(AValue: boolean);
begin
  if FRepeatBottom = AValue then
    Exit;
  FRepeatBottom := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFRepeatLeft(AValue: boolean);
begin
  if FRepeatLeft = AValue then
    Exit;
  FRepeatLeft := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFRepeatMiddleHorizontal(AValue: boolean);
begin
  if FRepeatMiddleHorizontal = AValue then
    Exit;
  FRepeatMiddleHorizontal := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFRepeatMiddleVertical(AValue: boolean);
begin
  if FRepeatMiddleVertical = AValue then
    Exit;
  FRepeatMiddleVertical := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFRepeatRight(AValue: boolean);
begin
  if FRepeatRight = AValue then
    Exit;
  FRepeatRight := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFRepeatTop(AValue: boolean);
begin
  if FRepeatTop = AValue then
    Exit;
  FRepeatTop := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFResampleFilter(AValue: TResampleFilter);
begin
  if FResampleFilter = AValue then
    Exit;
  FResampleFilter := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TuECustomSliceScalingOptions.SetFResampleMode(AValue: TResampleMode);
begin
  if FResampleMode = AValue then
    Exit;
  FResampleMode := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TuECustomSliceScalingOptions.Create(AOwner: TControl);
begin
  FOwner := AOwner;
  FBitmap := nil;
  FAutoDetectRepeat := False;
  FRepeatTop := False;
  FRepeatLeft := False;
  FRepeatMiddleHorizontal := False;
  FRepeatMiddleVertical := False;
  FRepeatRight := False;
  FRepeatBottom := False;
  FMarginTop := 2;
  FMarginRight := 2;
  FMarginBottom := 2;
  FMarginLeft := 2;
  FNumberOfItems := 1;
  FDirection := sdVertical;
  FDrawMode := dmDrawWithTransparency;
  FResampleMode := rmFineResample;
  FResampleFilter := rfBestQuality;
  inherited Create;
end;

destructor TuECustomSliceScalingOptions.Destroy;
begin
  if FBitmap <> nil then
    FreeAndNil(FBitmap);
  inherited Destroy;
end;

{ TuEGraphicButton }

procedure TuEGraphicButton.DoClick;
var
  Form: TCustomForm;
begin
  if ModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;
  end;
end;

procedure TuEGraphicButton.DoMouseDown;
var
  NewState: TuEGraphicButtonState;
begin
  NewState := gbsActive;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TuEGraphicButton.DoMouseUp;
var
  NewState: TuEGraphicButtonState;
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);

  if (p.x >= 0) and (p.x <= Width) and (p.y >= 0) and (p.y <= Height) then
    NewState := gbsHover
  else
    NewState := gbsNormal;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TuEGraphicButton.DoMouseEnter;
var
  NewState: TuEGraphicButtonState;
begin
  if Enabled then
    NewState := gbsHover
  else
  begin
    FState := gbsNormal;
    NewState := FState;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TuEGraphicButton.DoMouseLeave;
var
  NewState: TuEGraphicButtonState;
begin
  if Enabled then
    NewState := gbsNormal
  else
  begin
    FState := gbsNormal;
    NewState := FState;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TuEGraphicButton.Click;
begin
  DoClick;
  inherited Click;
end;

procedure TuEGraphicButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    DoMouseDown;
end;

procedure TuEGraphicButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp;
end;

procedure TuEGraphicButton.MouseEnter;
begin
  inherited MouseEnter;
  DoMouseEnter;
end;

procedure TuEGraphicButton.MouseLeave;
begin
  inherited MouseLeave;
  DoMouseLeave;
end;

{ TFading }

procedure TFading.SetFAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TFading.SetFMode(AValue: TFadingMode);
begin
  if FMode = AValue then
    Exit;
  FMode := AValue;
  FPrevDate:= 0;
end;

procedure TFading.SetFAlphaStep(AValue: byte);
begin
  if FAlphaStep = AValue then
    Exit
  else
    FAlphaStep := AValue;
end;

procedure TFading.SetFDuration(AValue: integer);
begin
  FDuration:= AValue;
end;

function TFading.Execute(AStepCount: integer= 1): byte;
var curDate: TDateTime;
  alphaStep: byte;
  timeGrain: integer;
begin
  if FAlphaStep <= 0 then
    alphaStep := 1
  else
    alphaStep := FAlphaStep;

  if FDuration > 0 then
  begin
    curDate := Now;
    if FPrevDate = 0 then
    begin
      FPrevDate := curDate;
      FElapsedMsAccumulator := 0;
      result := FAlpha;
      exit;
    end;

    inc(FElapsedMsAccumulator, round((curDate-FPrevDate)*(24*60*60*1000)) );
    timeGrain := round(FDuration*alphaStep/255);
    if timeGrain <= 0 then timeGrain := 1;
    AStepCount := FElapsedMsAccumulator div timeGrain;
    FElapsedMsAccumulator:= FElapsedMsAccumulator mod timeGrain;
    FPrevDate := curDate;
  end;

  if AStepCount < 0 then AStepCount := 0
  else if AStepCount > 255 then AStepCount := 255;

  case FMode of
    fmFadeIn, fmFadeInOut, fmFadeInCycle:
    begin
      if (FAlpha = 255) and (FMode = fmFadeInCycle) then
        FAlpha := 0
      else
      if FAlpha + alphaStep*AStepCount >= 255 then
      begin
        FAlpha := 255;
        if FMode = fmFadeInOut then
          FMode := fmFadeOutIn
        else if FMode <> fmFadeInCycle then
          FMode := fmSuspended;
      end
      else
        FAlpha += alphaStep*AStepCount;
    end;
    fmFadeOut,fmFadeOutIn, fmFadeOutCycle:
    begin
      if (FAlpha = 0) and (FMode = fmFadeOutCycle) then
        FAlpha := 255
      else
      if FAlpha - alphaStep*AStepCount <= 0 then
      begin
        FAlpha := 0;
        if FMode = fmFadeOutIn then
          FMode := fmFadeInOut
        else if FMode <> fmFadeOutCycle then
          FMode := fmSuspended;
      end
      else
        FAlpha -= alphaStep*AStepCount;
    end;
  end;

  Result := FAlpha;
end;

function TFading.Reset: byte;
begin
  case FMode of
    fmFadeIn, fmFadeInOut:
    begin
      FAlpha := 0;
    end;
    fmFadeOut,fmFadeOutIn:
    begin
      FAlpha := 255;
    end;
  end;
  Result := FAlpha;
  FPrevDate := 0;
end;

procedure TFading.PutImage(ADestination: TBGRACustomBitmap; AX, AY: integer;
  ASource: TBGRACustomBitmap);
begin
  ADestination.PutImage(AX,AY,ASource,dmDrawWithTransparency,Alpha);
end;

procedure TFading.FillRect(ADestination: TBGRACustomBitmap; ARect: TRect;
  AColor: TBGRAPixel);
begin
  ADestination.FillRect(ARect, BGRA(AColor.red,AColor.green,AColor.blue,AColor.alpha*Alpha div 255),dmDrawWithTransparency);
end;

{ TuECustomImageButton }

procedure TuECustomImageButton.Fade(Sender: TObject);
begin
  if FFade.Mode <> fmSuspended then
  begin
    Invalidate;
  end;
end;

procedure TuECustomImageButton.SetFTextVisible(AValue: boolean);
begin
  if FTextVisible = AValue then
    Exit;
  FTextVisible := AValue;
  RenderControl;
end;

procedure TuECustomImageButton.SetGlyph(AValue: TBitmap);
begin
  if FGlyph=AValue then Exit;
  FGlyph:=AValue;
end;

procedure TuECustomImageButton.SetImage(AValue: TBitmap);
begin
  if FImage=AValue then Exit;
  FImage:=AValue;
end;

procedure TuECustomImageButton.SetLayout(AValue: TuEButtonLayout);
begin
  if FLayout=AValue then Exit;
  FLayout:=AValue;
  RenderControl;
end;

procedure TuECustomImageButton.SetShowAccelChar(AValue: boolean);
begin
  if FShowAccelChar=AValue then Exit;
  FShowAccelChar:=AValue;
  RenderControl;
end;

procedure TuECustomImageButton.SetSpacing(AValue: integer);
begin
  if FSpacing=AValue then Exit;
  FSpacing:=AValue;
  RenderControl;
end;

procedure TuECustomImageButton.SetTextShadowColor(AValue: Tcolor);
begin
  if FTextShadowColor=AValue then Exit;
  FTextShadowColor:=AValue;
  RenderControl;
end;

procedure TuECustomImageButton.CMTextChanged(var Message: TLMessage);
begin
 inherited;
 RenderControl;
end;

procedure TuECustomImageButton.SetFBitmapOptions(AValue:
  TuEButtonSliceScalingOptions);
begin
  if FBitmapOptions = AValue then
    Exit;
  FBitmapOptions := AValue;
end;

procedure TuECustomImageButton.SetFAnimation(AValue: boolean);
begin
  if FAnimation = AValue then
    Exit;
  FAnimation := AValue;
end;

procedure TuECustomImageButton.SetFBitmapFile(AValue: string);
begin
  if FBitmapFile = AValue then
    Exit;
  FBitmapFile := AValue;
end;

procedure TuECustomImageButton.DrawControl;
var
  temp: TBGRABitmap;
begin
  if (Color <> clDefault) and (Color <> clNone) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(0, 0, Width, Height);
  end;

  if Enabled then
  begin
    case FState of
      gbsNormal, gbsHover: FBGRANormal.Draw(Canvas, FDestRect.Left,
          FDestRect.Top, False);
      gbsActive: FBGRAActive.Draw(Canvas, FDestRect.Left, FDestRect.Top, False);
    end;

    temp := TBGRABitmap.Create(Width, Height);
    FFade.Execute;
    FFade.PutImage(temp, 0, 0, FBGRAHover);
    temp.Draw(Canvas, FDestRect.Left, FDestRect.Top, False);
    temp.Free;
  end
  else
    FBGRADisabled.Draw(Canvas, FDestRect.Left, FDestRect.Top, False);

  {$IFDEF DEBUG}
  FDrawCount += 1;
  {$ENDIF}

  {$IFDEF DEBUG}
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(0, 0, GetDebugText);
  {$ENDIF}
end;

procedure TuECustomImageButton.RenderControl;

  procedure DrawGlyphnCaption(ABitmap: TBGRABitmap; Greyed:boolean);
  var
    gx,gy,tx,ty:integer;
    ts:TSize;
    cptn:string;
    AChar,AWidth:integer;
    FCol,SCol:TBGRAPixel;
  begin
    cptn:=caption;
    AChar:=pos('&',cptn);
    if FShowAccelChar and (AChar<>0) then delete(cptn,AChar,1);
    Bitmap.Assign(FGlyph);
    AssignFontToBGRA(Font, ABitmap);
    if TextVisible then ts:=ABitmap.TextSize(cptn) else
    begin
      ts.cx:=0;
      ts.cy:=0;
    end;
    If not Greyed then begin
      FCol:=ColorToBGRA(ColorToRGB(Font.Color));
      SCol:=ColorToBGRA(ColorToRGB(TextShadowColor));
    end else
    Begin
      FCol:=ColorToBGRA(ColorToRGB(Font.Color)).ToGrayscale();
      SCol:=ColorToBGRA(ColorToRGB(TextShadowColor)).ToGrayscale();
    end;
    case FLayout of
      blGlyphTop:begin
        gx:=(Width-Bitmap.Width) div 2;
        gy:=(Height-(Bitmap.Height+ts.cy+FSpacing)) div 2;
        tx:=(Width-ts.cx) div 2;
        ty:=(Height+FGlyph.height+FSpacing-ts.cy) div 2;
      end;
      blGlyphBottom:begin
        gx:=(Width-Bitmap.Width) div 2;
        gy:=(Height-(Bitmap.Height-ts.cy-FSpacing)) div 2;
        tx:=(Width-ts.cx) div 2;
        ty:=(Height-FGlyph.height-FSpacing-ts.cy) div 2;
      end;
      blGlyphLeft:begin
        gx:=(Width-(Bitmap.Width+ts.cx+FSpacing)) div 2;
        gy:=(Height-Bitmap.Height) div 2;
        tx:=(Width+FGlyph.Width+FSpacing-ts.cx) div 2;
        ty:=(Height-ts.cy) div 2;
      end;
      blGlyphRight:begin
        gx:=(Width-(Bitmap.Width-ts.cx-FSpacing)) div 2;
        gy:=(Height-Bitmap.Height) div 2;
        tx:=(Width-FGlyph.Width-FSpacing-ts.cx) div 2;
        ty:=(Height-ts.cy) div 2;
      end;
    end;

    if Greyed then SetBitmap(TBGRABitmap(Bitmap.FilterGrayscale));
    //if Greyed then Bitmap.Assign(Bitmap.FilterGrayscale);
    ABitmap.PutImage(gx,gy,Bitmap,dmDrawWithTransparency);
    if TextVisible then
    begin
      if TextShadowColor<>clNone then ABitmap.TextOut(tx+1,ty+1,cptn,SCol);
      ABitmap.TextOut(tx,ty,cptn,FCol);
      if FShowAccelChar and (AChar<>0) and (AChar<>length(caption))then
      begin
        AWidth:=ABitmap.TextSize(Copy(cptn,AChar,1)).cx;
        AChar:=ABitmap.TextSize(LeftStr(cptn,AChar)).cx;
        if TextShadowColor<>clNone then ABitmap.DrawHorizLine(tx+AChar-AWidth+1,ty+ts.cy-1,tx+AChar,SCol);
        ABitmap.DrawHorizLine(tx+AChar-AWidth,ty+ts.cy-2,tx+AChar-1,FCol);
      end;
    end;
  end;

var
  i: integer;
begin
  { Free cache bitmaps }
  if FBGRANormal <> nil then
    FreeAndNil(FBGRANormal);
  if FBGRAHover <> nil then
    FreeAndNil(FBGRAHover);
  if FBGRAActive <> nil then
    FreeAndNil(FBGRAActive);
  if FBGRADisabled <> nil then
    FreeAndNil(FBGRADisabled);

  { Create cache bitmaps }
  FBGRANormal := TBGRABitmap.Create(Width, Height);
  FBGRAHover := TBGRABitmap.Create(Width, Height);
  FBGRAActive := TBGRABitmap.Create(Width, Height);
  FBGRADisabled := TBGRABitmap.Create(Width, Height);

  { Free FBGRAMultiSliceScaling }
  if FBGRAMultiSliceScaling <> nil then
    FreeAndNil(FBGRAMultiSliceScaling);

  if (FBitmapOptions.Bitmap <> nil) then
  begin
    { Create FBGRAMultiSliceScaling }
    FBGRAMultiSliceScaling := TBGRAMultiSliceScaling.Create(FBitmapOptions.Bitmap,
      FBitmapOptions.MarginTop, FBitmapOptions.MarginRight,
      FBitmapOptions.MarginBottom, FBitmapOptions.MarginLeft,
      FBitmapOptions.NumberOfItems, FBitmapOptions.Direction);

    { Set FBGRAMultiSliceScaling properties }
    for i := 0 to High(FBGRAMultiSliceScaling.SliceScalingArray) do
    begin
      FBGRAMultiSliceScaling.SliceScalingArray[i].ResampleFilter :=
        FBitmapOptions.ResampleFilter;
      FBGRAMultiSliceScaling.SliceScalingArray[i].ResampleMode :=
        FBitmapOptions.ResampleMode;
      FBGRAMultiSliceScaling.SliceScalingArray[i].DrawMode := FBitmapOptions.DrawMode;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpTop] :=
        FBitmapOptions.RepeatTop;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpBottom] :=
        FBitmapOptions.RepeatBottom;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpLeft] :=
        FBitmapOptions.RepeatLeft;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpRight] :=
        FBitmapOptions.RepeatRight;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpMiddleHorizontal] :=
        FBitmapOptions.RepeatMiddleHorizontal;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpMiddleVertical] :=
        FBitmapOptions.RepeatMiddleVertical;
      if FBitmapOptions.AutoDetectRepeat then
        FBGRAMultiSliceScaling.SliceScalingArray[i].AutodetectRepeat;
    end;

    { Calculate FDestRect }
    FDestRect := CalculateDestRect(
      FBGRAMultiSliceScaling.SliceScalingArray[0].BitmapWidth,
      FBGRAMultiSliceScaling.SliceScalingArray[0].BitmapHeight, Width,
      Height, FBitmapOptions.Stretch, FBitmapOptions.Proportional,
      FBitmapOptions.Center);

    { Draw in cache bitmaps }
    FBGRAMultiSliceScaling.Draw(0, FBGRANormal, 0, 0, FDestRect.Right,
      FDestRect.Bottom, Debug);
    FBGRAMultiSliceScaling.Draw(1, FBGRAHover, 0, 0, FDestRect.Right,
      FDestRect.Bottom, Debug);
    FBGRAMultiSliceScaling.Draw(2, FBGRAActive, 0, 0, FDestRect.Right,
      FDestRect.Bottom, Debug);
    FBGRAMultiSliceScaling.Draw(3, FBGRADisabled, 0, 0, FDestRect.Right,
      FDestRect.Bottom, Debug);
  end
  else
  begin
    { Calculate FDestRect }
    FDestRect := Rect(0, 0, Width, Height);

    { Draw default style in cache bitmaps }
    FBGRANormal.Rectangle(0, 0, Width, Height, BGRABlack, BGRA(240, 240, 240, 64),
      dmSet);
    FBGRAHover.Rectangle(0, 0, Width, Height, BGRA(128,192,255), BGRA(255, 255, 255, 64),
      dmSet);
    FBGRAActive.Rectangle(0, 0, Width, Height, BGRAWhite, BGRA(192, 220, 255, 128),
      dmSet);
    FBGRADisabled.Rectangle(0, 0, Width, Height, BGRA(192,192,192), BGRA(192, 192, 192, 64),
      dmSet);
  end;

  DrawGlyphnCaption(FBGRANormal,false);
  DrawGlyphnCaption(FBGRAHover,false);
  DrawGlyphnCaption(FBGRAActive,false);
  DrawGlyphnCaption(FBGRADisabled,true);

  {$IFDEF DEBUG}
  FRenderCount += 1;
  {$ENDIF}
  inherited RenderControl;
end;

procedure TuECustomImageButton.ImageChanged(Sender: TObject);
begin
  if not Assigned(FImage) then exit;
  BeginUpdate;
  if (FImage.Width>0) then
    if BitmapOptions.Bitmap <> nil then
      BitmapOptions.Bitmap.assign(FImage)
    else
      BitmapOptions.Bitmap := TBGRABitmap.Create(FImage)
  else if BitmapOptions.Bitmap <> nil then
  begin
    BitmapOptions.Bitmap.Free;
    BitmapOptions.Bitmap:=nil;
  end;
  EndUpdate;
  RenderControl;
  if Assigned(OnImageChanged) then OnImageChanged(Self);
end;

procedure TuECustomImageButton.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  RenderControl;
end;

procedure TuECustomImageButton.GlyphChanged(Sender: TObject);
begin
  RenderControl;
  if Assigned(OnGlyphChanged) then OnGlyphChanged(Self);
end;

procedure TuECustomImageButton.CMChanged(var Message: TLMessage);
begin
  if csReadingState in ControlState then
    Exit;
  RenderControl;
end;

procedure TuECustomImageButton.DefaultImage;
begin
  //------ Load default image ---------
  DrawDefaultImage;
  with BitmapOptions do
  begin
    FNumberOfItems:=4;
    FDirection:=sdVertical;
    FMarginBottom:=2;
    FMarginLeft:=2;
    FMarginRight:=2;
    FMarginTop:=2;
    FStretch:=true;
  end;
end;

procedure TuECustomImageButton.DrawDefaultImage;
var
  i,r,h,y:integer;
  c0,c1,c2,c3:TBGRAPixel;
begin
  if (Width=0) or (Height=0) then exit;
  r:=50;
  h:=Height;
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

  AssignBGRAtoImage(Bitmap,Image);

end;

{$IFDEF DEBUG}
function TuECustomImageButton.GetDebugText: string;
begin
  Result := 'Render: ' + IntToStr(FRenderCount) + ' Draw: ' + IntToStr(FDrawCount);
end;

{$ENDIF}

procedure TuECustomImageButton.DoMouseDown;
begin
  FFade.Mode := fmFadeOut;
  if FAnimation then
    FFade.Step := 60
  else
    FFade.Step := 255;

  inherited DoMouseDown;
end;

procedure TuECustomImageButton.DoMouseUp;
var
  Ctrl : TControl;
begin
  FFade.Mode := fmFadeIn;
  if FAnimation then
    FFade.Step := 20
  else
    FFade.Step := 255;

  Ctrl := Application.GetControlAtMouse;
  if Ctrl = Self then
    DoMouseEnter
  else
    DoMouseLeave;

  inherited DoMouseUp;
end;

procedure TuECustomImageButton.DoMouseEnter;
begin
  FFade.Mode := fmFadeIn;
  if FAnimation then
    FFade.Step := 15
  else
    FFade.Step := 255;

  inherited DoMouseEnter;
end;

procedure TuECustomImageButton.DoMouseLeave;
begin
  FFade.Mode := fmFadeOut;
  if FAnimation then
    FFade.Step := 8
  else
    FFade.Step := 255;

  inherited DoMouseLeave;
end;

constructor TuECustomImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DEBUG}
  FDrawCount := 0;
  FRenderCount := 0;
  {$ENDIF}
  DisableAutoSizing;
  FImage := TBitmap.Create;
  FImage.Clear;
  FImage.OnChange := @ImageChanged;
  FGlyph := TBitmap.Create;
  FGlyph.Clear;
  FGlyph.OnChange := @GlyphChanged;
  FTextShadowColor:= clNone;
  FSpacing:=0;
  Font.OnChange:=@FontChanged;
  Include(FControlState, csCreating);
  BeginUpdate;
  try
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
    ControlStyle := ControlStyle + [csAcceptsControls];
    FBitmapOptions := TuEButtonSliceScalingOptions.Create(Self);

    FFade.Step := 15;
    FFade.Mode := fmFadeOut;
    FTimer := TTimer.Create(Self);
    FTimer.Interval := 15;
    FTimer.OnTimer := @Fade;
    FAnimation := True;

    FTextVisible := True;
    FShowAccelChar := False;
  finally
    Exclude(FControlState, csCreating);
    EnableAutoSizing;
    EndUpdate;
  end;
end;

destructor TuECustomImageButton.Destroy;
begin
  if assigned(FBGRAMultiSliceScaling) then FreeAndNil(FBGRAMultiSliceScaling);
  if assigned(FBGRANormal) then FreeAndNil(FBGRANormal);
  if assigned(FBGRAHover) then FreeAndNil(FBGRAHover);
  if assigned(FBGRAActive) then FreeAndNil(FBGRAActive);
  if assigned(FBGRADisabled) then FreeAndNil(FBGRADisabled);
  if assigned(FBitmapOptions) then FreeAndNil(FBitmapOptions);
  FImage.OnChange := nil;
  FreeAndNil(FImage);
  FGlyph.OnChange := nil;
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TuECustomImageButton.LoadFromBitmapResource(Resource: string;
  ResourceType: PChar);
var
  res: TResourceStream;
begin
  res := TResourceStream.Create(HInstance, Resource, ResourceType);

  if BitmapOptions.Bitmap <> nil then
    BitmapOptions.Bitmap.Free;

  BitmapOptions.Bitmap := TBGRABitmap.Create(res);
  res.Free;
end;

procedure TuECustomImageButton.LoadFromBitmapResource(Resource: string);
begin
  LoadFromBitmapResource(Resource, RT_RCDATA);
end;

procedure TuECustomImageButton.LoadFromBitmapFile;
begin
  if BitmapFile <> '' then
    if BitmapOptions.Bitmap <> nil then
      BitmapOptions.Bitmap.LoadFromFile(BitmapFile)
    else
      BitmapOptions.Bitmap := TBGRABitmap.Create(BitmapFile);
end;

procedure TuECustomImageButton.Assign(Source: TPersistent);
begin
  if Source is TuECustomImageButton then
  begin
    FBitmapOptions.Assign(TuECustomImageButton(Source).BitmapOptions);
    FAnimation := TuECustomImageButton(Source).Animation;
    FBitmapFile := TuECustomImageButton(Source).BitmapFile;
    FTextVisible := TuECustomImageButton(Source).TextVisible;

    if TuECustomImageButton(Source).BitmapOptions.Bitmap <> nil then
    begin
      if FBitmapOptions.Bitmap <> nil then
        FBitmapOptions.Bitmap.Free;

      FBitmapOptions.Bitmap :=
        TBGRABitmap.Create(TuECustomImageButton(Source).BitmapOptions.Bitmap.Bitmap);
    end
    else
      LoadFromBitmapFile;

    RenderControl;
  end
  else
    inherited Assign(Source);
end;

procedure TuECustomImageButton.LoadGlyphFromFile(f:string);
var p:TPicture;
begin
  if not FileExists(f) then exit;
  try
    p:=TPicture.Create;
    p.LoadFromFile(f);
    Glyph.Assign(p.Bitmap);
  finally
    p.Free;
  end;
end;

procedure TuECustomImageButton.LoadImageFromFile(f: string);
begin
  if not FileExists(f) then exit;
  BitmapFile:=f;
  LoadFromBitmapFile;
end;

procedure TuECustomImageButton.LoadGlyphFromBGRA(bm: TBGRABitmap);
begin
  AssignBGRAtoImage(bm,FGlyph);
end;

procedure TuECustomImageButton.LoadImageFromBGRA(bm: TBGRABitmap);
begin
  AssignBGRAtoImage(bm,FImage);
end;

procedure TuECustomImageButton.SaveToFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    WriteComponentAsTextToStream(AStream, Self);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure TuECustomImageButton.LoadFromFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(Self), @OnFindClass);
  finally
    AStream.Free;
  end;
end;

procedure TuECustomImageButton.AssignFromFile(AFileName: string);
var
  AStream: TMemoryStream;
  AButton: TuEButton;
begin
  AButton := TuEButton.Create(nil);
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(AButton), @OnFindClass);
    Assign(AButton);
  finally
    AStream.Free;
    AButton.Free;
  end;
end;

procedure TuECustomImageButton.OnFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TuEButton') = 0 then
    ComponentClass := TuEButton;
end;

class function TuECustomImageButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 30;
end;

end.
