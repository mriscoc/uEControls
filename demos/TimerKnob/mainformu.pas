unit MainFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRABitmapTypes,
  ExtCtrls, StdCtrls, ExtDlgs, uEKnob, ueled, uERotImage;

type

  { TMainForm }

  TMainForm = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    uEKnob1: TuEKnob;
    uELED1: TuELED;
    uELED2: TuELED;
    uERotImage1: TuERotImage;
    procedure Timer1Timer(Sender: TObject);
    procedure uEKnob1Click(Sender: TObject);
  private
    procedure end_of_timer;
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if uEKnob1.Position>0 then
  begin
    uEKnob1.Position:=uEKnob1.Position-0.1;
    uERotImage1.Angle:=(uEKnob1.Position*360+180);
    Label1.Caption:=format('%2.1f',[uEKnob1.Position]);
  end else end_of_timer;
end;

procedure TMainForm.uEKnob1Click(Sender: TObject);
begin
  uELED1.Active:=false;
  uELED2.Active:=true;
end;

procedure TMainForm.end_of_timer;
begin
  uELED1.Active:=true;
  uELED2.Active:=false;
end;

end.

