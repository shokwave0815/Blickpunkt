program Blickpunkt;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
   {$IFDEF UseCThreads} cthreads, {$ENDIF}
  {$ENDIF}
  Interfaces,
  Forms,
  main,
  FileHelper,
  ListHelper,
  configuration;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
