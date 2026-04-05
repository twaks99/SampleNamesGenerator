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
    function GetListOfCountries(ctList : TCityRecordsList) : TStringList;
    function GetListOfStates(ctList : TCityRecordsList; cntryName : String) : TStringList;
    function GetListOfCities(ctList : TCityRecordsList; cntryName, stName : String) : TStringList;
  public
    CityGroupsList: TCityGroupsList;
    function GetCitiesListByGroupName(grpName : String) : TCityRecordsList;
    function OrderCitiesListByState(ctList : TCityRecordsList) : TCityRecordsList;
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

function TdataModuleMain.OrderCitiesListByState(ctList : TCityRecordsList) : TCityRecordsList;
var
  OrderedRecords : TCityRecordsList;
  countries, states, cities : TStringList;
  i, j, k : Integer;
  cityRecord : TCityRecord;
begin
  OrderedRecords := TCityRecordsList.Create;

  countries := GetListOfCountries(ctList);
  for i := 0 to countries.Count - 1 do begin
    //Get List of states and sort.
    states := GetListOfStates(ctList, countries[i]);
    states.Sort;
    //For each state, find list of cities.
    for j := 0 to states.Count - 1 do begin
      cities := GetListOfCities(ctList, countries[i], states[j]);
      cities.Sort;
      //For each city, add a record to the city/state list
      for k := 0 to cities.Count - 1 do begin
        cityRecord := TCityRecord.Create(cities[k], states[j], countries[i]);
        OrderedRecords.Add(cityRecord);
      end;
    end;
  end;
  Result := OrderedRecords;
end;

function TdataModuleMain.GetListOfCountries(ctList : TCityRecordsList) : TStringList;
var
  countryList : TStringList;
  i : Integer;
begin
  countryList := TStringList.Create;
  for i := 0 to ctList.Count - 1 do begin
    if (countryList.IndexOf(ctList[i].CountryCode) < 0) then begin
      countryList.Add(ctList[i].CountryCode);
    end;
  end;
  Result := countryList;
end;

function TdataModuleMain.GetListOfStates(ctList : TCityRecordsList; cntryName : String) : TStringList;
var
  stateList : TStringList;
  i : Integer;
begin
  stateList := TStringList.Create;
  for i := 0 to ctList.Count - 1 do begin
    if (ctList[i].CountryCode = cntryName) then begin
      if (stateList.IndexOf(ctList[i].StateName) < 0) then begin
        stateList.Add(ctList[i].StateName);
      end;
    end;
  end;
  Result := stateList;
end;

function TdataModuleMain.GetListOfCities(ctList : TCityRecordsList; cntryName, stName : String) : TStringList;
var
  cityList : TStringList;
  i : Integer;
begin
  cityList := TStringList.Create;
  for i := 0 to ctList.Count - 1 do begin
    if ((ctList[i].CountryCode = cntryName) And (ctList[i].StateName = stName)) then begin
      if (cityList.IndexOf(ctList[i].CityName) < 0) then begin
        cityList.Add(ctList[i].CityName);
      end;
    end;
  end;
  Result := cityList;
end;

end.

