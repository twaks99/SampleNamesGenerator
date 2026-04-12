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
  public
    CityGroupsList: TCityGroupsList;
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


end.

