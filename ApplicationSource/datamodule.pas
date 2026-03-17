unit dataModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, CityRecords;

type

  { TdataModuleMain }

  TdataModuleMain = class(TDataModule)
    connectionMain: TSQLite3Connection;
    queryStates: TSQLQuery;
    queryCities: TSQLQuery;
    transactionMain: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private

  public
    CityGroupsList: TCityGroupsList;
    function GetCitiesListByGroupName(grpName : String) : TCityRecordsList;
    procedure UpdateCitiesListByGroupName(grpName : String; ctList : TCityRecordsList);
  end;

var
  dataModuleMain: TdataModuleMain;

implementation

{$R *.lfm}

{ TdataModuleMain }

procedure TdataModuleMain.DataModuleCreate(Sender: TObject);
begin
  CityGroupsList := TCityGroupsList.Create;
  connectionMain.DatabaseName := 'SampleNames.db';
  connectionMain.Open;
  queryStates.Open;
end;

function TdataModuleMain.GetCitiesListByGroupName(grpName : String) : TCityRecordsList;
var
  i : Integer;
  ctList : TCityRecordsList;
begin
  ctList := TCityRecordsList.Create;
  for i := 0 to CityGroupsList.Count - 1 do begin
    if (CityGroupsList[i].GroupName.ToUpper = grpName.ToUpper) then begin
      ctList := CityGroupsList[i].CitiesList;
      break;
    end;
  end;
  Result := ctList;
end;

procedure TdataModuleMain.UpdateCitiesListByGroupName(grpName : String; ctList : TCityRecordsList);
var
  i : Integer;
begin
  for i := 0 to CityGroupsList.Count - 1 do begin
    if (CityGroupsList[i].GroupName.ToUpper = grpName.ToUpper) then begin
      CityGroupsList[i].CitiesList := ctList;
      break;
    end;
  end;
end;

end.

