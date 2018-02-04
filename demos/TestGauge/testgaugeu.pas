unit testgaugeU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  uEGauge, uEKnob;

type

  { TForm1 }

  TForm1 = class(TForm)
    TrackBar1: TTrackBar;
    uEGauge1: TuEGauge;
    procedure TrackBar1Change(Sender: TObject);
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

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  uEGauge1.Position:=TrackBar1.Position;
end;

end.

