unit MainFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs,
  ExtCtrls, ueled;

type

  { TMainForm }

  TMainForm = class(TForm)
    Timer1: TTimer;
    uELED1: TuELED;
    uELED10: TuELED;
    uELED11: TuELED;
    uELED2: TuELED;
    uELED3: TuELED;
    uELED4: TuELED;
    uELED5: TuELED;
    uELED6: TuELED;
    uELED7: TuELED;
    uELED8: TuELED;
    uELED9: TuELED;
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;
  count:integer=0;

implementation

{$R *.lfm}

{ TMainForm }

{ TMainForm }

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  uELED1.Active:=false;
  uELED2.Active:=false;
  uELED3.Active:=false;
  case count of
    0..4  : uELED1.Active:=true;
    5..6  : uELED2.Active:=true;
    7..10 : uELED3.Active:=true;
  end;
  inc(count);
  if count>10 then count:=0;
  uELED4.Active:=not uELED4.Active;
  uELED5.Active:=not uELED4.Active;
end;

end.

