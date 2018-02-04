program TestRotImage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, uecontrols;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestRotImage, TestRotImageF);
  Application.Run;
end.

