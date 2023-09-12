program SampleNamesGenerator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, GenerateSampleName, DetailForm, InsertStatementForm,
savedsettings, dataexport
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TformMain, formMain);
  Application.CreateForm(TNameDetailForm, NameDetailForm);
  Application.CreateForm(TInsertStatementForm, FormInsertStatement);
  Application.Run;
end.

