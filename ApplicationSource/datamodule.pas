unit dataModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB;

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

  end;

var
  dataModuleMain: TdataModuleMain;

implementation

{$R *.lfm}

{ TdataModuleMain }

procedure TdataModuleMain.DataModuleCreate(Sender: TObject);
begin
  connectionMain.DatabaseName := 'SampleNames.db';
  connectionMain.Open;
  queryStates.Open;
end;

end.

