unit MainFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uESelector;

type

  { TMainForm }

  TMainForm = class(TForm)
    Label1: TLabel;
    uESelector1: TuESelector;
    procedure FormCreate(Sender: TObject);
    procedure uESelector1Change(Sender: TObject);
    procedure LabelsUpdate;
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

procedure TMainForm.uESelector1Change(Sender: TObject);
begin
  LabelsUpdate;
end;

procedure TMainForm.LabelsUpdate;
begin
  With uESelector1 do Label1.Caption:='Index: '+inttostr(Index)+#$0d+#$0a+
                                       'Item:  '+Items.Names[Index]+#$0d+#$0a+
                                       'Value: '+Items.ValueFromIndex[Index];
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  uESelector1.OffsetAngle:=-900;
  LabelsUpdate;
end;

end.

