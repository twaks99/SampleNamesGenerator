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
  private

  public

  end;

var
  dataModuleMain: TdataModuleMain;

implementation

{$R *.lfm}

end.

