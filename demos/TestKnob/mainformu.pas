unit MainFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRABitmapTypes,
  ComCtrls, Spin, ExtCtrls, uEKnob, ueled;

type

  { TMainForm }

  TMainForm = class(TForm)
    ed_Val: TFloatSpinEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    uEKnob1: TuEKnob;
    uEKnob2: TuEKnob;
    uEKnob3: TuEKnob;
    uEKnob4: TuEKnob;
    uEKnob5: TuEKnob;
    uEKnob6: TuEKnob;
    uEKnob7: TuEKnob;
    uELED1: TuELED;
    procedure Ed_ValChange(Sender: TObject);
    procedure uEKnob1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.uEKnob1Change(Sender: TObject);
var i:integer;
begin
  Ed_Val.Value:=uEKnob1.Position;
  ueLED1.active:=uEKnob1.Position>0;
  i:=round(uEKnob1.Position);
  case i of
    00..50 : uELED1.Color:=clLime;
    51..80 : uELED1.Color:=clYellow;
    81..100: uELED1.Color:=clRed;
  end;
end;

procedure TMainForm.Ed_ValChange(Sender: TObject);
begin
  uEKnob1.Position:=ed_Val.value;
end;

end.

