unit CityRecords;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TCityRecord = Class(TObject)
    constructor Create;
    constructor Create(cityName, stateName, countryCode : String);
    constructor Create(recordLine: String);
  public
    CityName: String;
    StateName: String;
    CountryCode: String;
    function ToString : String; override;
  end;

  TCityRecordsList = specialize TObjectList<TCityRecord>;

  TCityListGroup = Class(TObject)
    constructor Create(grpName : String);
  public
    GroupName : String;
    CitiesList : TCityRecordsList;
    function GetCitiesListByGroupName(grpName : String) : TCityRecordsList;
    function OrderCitiesListByState(ctList : TCityRecordsList) : TCityRecordsList;
    procedure UpdateCitiesListByGroupName(grpName : String; ctList : TCityRecordsList);
  private
    function GetListOfCountries(ctList : TCityRecordsList) : TStringList;
    function GetListOfStates(ctList : TCityRecordsList; cntryName : String) : TStringList;
    function GetListOfCities(ctList : TCityRecordsList; cntryName, stName : String) : TStringList;
  end;

  TCityGroupsList = specialize TObjectList<TCityListGroup>;

implementation

constructor TCityRecord.Create;
begin
  CityName := '';
  StateName := '';
  CountryCode := 'US';
end;

constructor TCityRecord.Create(cityName, stateName, countryCode : String);
begin
  self.CityName := cityName;
  self.StateName := stateName;
  if (countryCode.Length <> 2) then begin
    self.CountryCode := 'US';
  end
  else begin
    self.CountryCode := countryCode;
  end;
end;

constructor TCityRecord.Create(recordLine: String);
var
  recordParts: TStringArray;
begin
  recordParts := recordLine.Split(';');
  self.CityName := recordParts[0].Trim;
  self.StateName := recordParts[1].Trim;
  self.CountryCode := recordParts[2].Trim;
end;

function TCityRecord.ToString : String;
begin
  Result := self.CityName + '; ' + self.StateName + '; ' + self.countryCode;
end;

constructor TCityListGroup.Create(grpName : String);
begin
  self.GroupName:= grpName;
  self.CitiesList := TCityRecordsList.Create;
end;

function TCityListGroup.GetCitiesListByGroupName(grpName : String) : TCityRecordsList;
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

procedure TCityListGroup.UpdateCitiesListByGroupName(grpName : String; ctList : TCityRecordsList);
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

function TCityListGroup.OrderCitiesListByState(ctList : TCityRecordsList) : TCityRecordsList;
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

function TCityListGroup.GetListOfCountries(ctList : TCityRecordsList) : TStringList;
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

function TCityListGroup.GetListOfStates(ctList : TCityRecordsList; cntryName : String) : TStringList;
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

function TCityListGroup.GetListOfCities(ctList : TCityRecordsList; cntryName, stName : String) : TStringList;
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

