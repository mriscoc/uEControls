unit mainformu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uEImage;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    uEImage1: TuEImage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  uEImage1.LoadFromFile('image.png');
  Image1.Picture.LoadFromFile('image.png');
end;

initialization
  {$I mainformu.lrs}

end.

