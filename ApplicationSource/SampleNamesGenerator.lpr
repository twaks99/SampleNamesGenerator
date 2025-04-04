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
  Forms, dataModule, main, GenerateSampleName, DetailForm, InsertStatementForm,
savedsettings, dataexport, MultipleCitiesSelect
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TdataModuleMain, dataModuleMain);
  Application.CreateForm(TformMain, formMain);
  Application.CreateForm(TNameDetailForm, NameDetailForm);
  Application.CreateForm(TInsertStatementForm, FormInsertStatement);
  Application.CreateForm(TMultipleCitiesSelect, MultipleCitiesSelect);
  Application.Run;
end.

