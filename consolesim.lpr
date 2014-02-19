program consolesim;

{$mode objfpc}{$H+}

uses
  //{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  //{$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, consolesimMain , sysutils, spacehackcontrols
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  writeln('');
  writeln('About to init');
  Application.Initialize;
  writeln('Main program: init done');
  Application.CreateForm(TfrmMain, frmMain);
  writeln('Main program: main form created');
  Application.Run;
end.

