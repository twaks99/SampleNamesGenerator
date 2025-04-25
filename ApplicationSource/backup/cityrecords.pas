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
  recordParts := recordLine.Split('; ');
  self.CityName := recordParts[0].Trim;
  self.StateName := recordParts[1].Trim;
  self.CountryCode := recordParts[2].Trim;
end;

function TCityRecord.ToString : String;
begin
  Result := self.CityName + '; ' + self.StateName + '; ' + self.countryCode;
end;

end.

