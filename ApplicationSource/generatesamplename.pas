unit GenerateSampleName;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Generics.Collections, SQLite3Conn, Math, fgl,
  dataModule;

const
  CommonLetters: array[0..19] of Char = ('A','B','C','D','E','F','G','H',
    'I','J','K','L','M','N','O','P','R','S','T','W');
type
  TNameMap = specialize TFPGMap<String, String>;

  TSampleName = Class(TObject)
    constructor Create(id: Integer; fName, lName: String; mi, gndr : Char;
      email, address, city, state, zip, phone: String; bDate: TDateTime);
    constructor Create;
  public
    ID : Integer;
    FirstName : String;
    LastName : String;
    MI : Char;
    Gender: Char;
    Email: String;
    Address: String;
    City: String;
    StateCode: String;
    ZipCode: String;
    PhoneNumber: String;
    BirthDate: TDateTime;
    Function BirthDateString : String;
    Function GenerateDictionary : TNameMap;
  end;

  TSampleNamesList = specialize TObjectList<TSampleName>;

  TZipCode = Class(TObject)
    constructor Create(zip, city, state : String);
  public
    ZipCode: String;
    City: String;
    StateCode: String;
  end;

  TZipCodesList = specialize TObjectList<TZipCode>;

  TSampleNamesGenerator = Class(TObject)
    constructor Create();
  public
    CountryCode: String;
    StateCode: String;
    City: String;
    NumRowsGenerate: Integer;
    function GenerateSampleSet(country, state, cityName: String; numRows, mdist, fdist: Integer;
        rdist, nearbyCities: Boolean): TSampleNamesList;
  private
    //DBConnection : TSQLite3Connection;
    DataRetrieveQuery : TSQLQuery;
    FemaleFirstNames: TStringList;
    MaleFirstNames: TStringList;
    LastNames: TStringList;
    StreetNames: TStringList;
    AreaCodes: TStringList;
    ZipCodes: TZipCodesList;
    EmailDomains: TStringList;
    StateLoaded: String;
    CurrentRecordId: Integer;
    MaleCount: Integer;
    FemaleCount: Integer;
    RandomGenderDist: Boolean;
    function GenerateRandomFixedDigitNumber(numDigits: Integer): Int64;
    function CreateSampleName(gender : char) : TSampleName;
    procedure CalculateGenderDistribution(malePct, femalePct, numRows : Integer);
    procedure RetrieveZipCodeListIncludingNearbyCities;
    procedure RetrieveZipCodeListSelectedCity;
    procedure LoadLists;
    procedure GetAreaCodesForState(state: String);
    procedure RunQuery(Sql: TStringList);
  end;

implementation

constructor TSampleName.Create(id: Integer; fName, lName: String; mi, gndr : Char;
    email, address, city, state, zip, phone: String; bDate: TDateTime);
begin
  self.ID:= id;
  self.FirstName:= fName;
  self.LastName:= lName;
  self.MI:= mi;
  self.Gender:= gndr;
  self.Email:= email;
  self.Address:= address;
  self.City:= city;
  self.StateCode:= state;
  self.ZipCode:= zip;
  self.PhoneNumber:= phone;
  self.BirthDate:= bDate;
end;

constructor TSampleName.Create;
begin
  self.ID:= 0;
  self.FirstName:= String.Empty;
  self.LastName:= String.Empty;
  self.MI:= ' ';
  self.Gender:= ' ';
  self.Email:= String.Empty;
  self.Address:= String.Empty;
  self.City:= String.Empty;
  self.StateCode:= String.Empty;
  self.ZipCode:= String.Empty;
  self.PhoneNumber:= String.Empty;
  self.BirthDate:= Now;
end;

function TSampleName.BirthDateString : String;
begin
  Result := FormatDateTime('YYYY-MM-DD', BirthDate);
end;

function TSampleName.GenerateDictionary : TNameMap;
var
  nameDictionary : TNameMap;
begin
  nameDictionary := TNameMap.Create;
  nameDictionary.Add('ID', self.ID.ToString);
  nameDictionary.Add('FirstName', self.FirstName);
  nameDictionary.Add('LastName', self.LastName);
  nameDictionary.Add('MI', String.Empty + self.MI);
  nameDictionary.Add('Gender', String.Empty + self.Gender);
  nameDictionary.Add('Email', self.Email);
  nameDictionary.Add('Address', self.Address);
  nameDictionary.Add('City', self.City);
  nameDictionary.Add('StateCode', self.StateCode);
  nameDictionary.Add('ZipCode', self.ZipCode);
  nameDictionary.Add('PhoneNumber', self.PhoneNumber);
  nameDictionary.Add('BirthDate', self.BirthDateString);
  Result := nameDictionary;
end;


constructor TZipCode.Create(zip, city, state : String);
begin
  self.ZipCode:= zip;
  Self.City:= city;
  self.StateCode:= state;
end;

constructor TSampleNamesGenerator.Create();
begin
  DataRetrieveQuery := TSQLQuery.Create(Nil);
  DataRetrieveQuery.DataBase := dataModule.dataModuleMain.connectionMain;
  DataRetrieveQuery.Transaction := dataModule.dataModuleMain.transactionMain;
  FemaleFirstNames := TStringList.Create;
  MaleFirstNames := TStringList.Create;
  LastNames := TStringList.Create;
  StreetNames := TStringList.Create;
  AreaCodes := TStringList.Create;
  ZipCodes := TZipCodesList.Create;
  EmailDomains := TStringList.Create;
  StateLoaded := String.Empty;
  CurrentRecordId := 0;
  MaleCount := 0;
  FemaleCount := 0;
  RandomGenderDist := true;
  LoadLists;
end;

function TSampleNamesGenerator.GenerateSampleSet(country, state, cityName: String;
      numRows, mdist, fdist: Integer; rdist, nearbyCities: Boolean) : TSampleNamesList;
var
  i, genNumber, maleCntr, femaleCntr : Integer;
  gndr : Char;
begin
  CountryCode := country;
  StateCode:= state;
  City:= cityName;
  NumRowsGenerate:= numRows;
  CurrentRecordId := 0;
  RandomGenderDist := rdist;
  Result := TSampleNamesList.Create;
  if (nearbyCities) then begin
    RetrieveZipCodeListIncludingNearbyCities;
  end
  else begin
    RetrieveZipCodeListSelectedCity;
  end;
  CalculateGenderDistribution(mdist, fdist, numRows);
  maleCntr := 0;
  femaleCntr := 0;
  //Generate list of sample names
  for i := 1 to numRows do begin
    //Get Gender.
    if (RandomGenderDist) then begin
      genNumber:= Random(2);
      if (genNumber = 0) then
        gndr := 'M'
      else
        gndr := 'F';
    end
    else begin
      if (maleCntr < MaleCount) then begin
        gndr := 'M';
        Inc(maleCntr);
      end
      else begin
        gndr := 'F';
        Inc(femaleCntr);
      end;
    end;
    Result.Add(CreateSampleName(gndr));
  end;
end;

procedure TSampleNamesGenerator.CalculateGenderDistribution(malePct, femalePct, numRows : Integer);
var
  diff : Integer;
begin
  MaleCount := Trunc(numRows.ToDouble * (malePct.ToDouble / 100.0));
  FemaleCount := Trunc(numRows.ToDouble * (femalePct.ToDouble / 100.0));
  diff := numRows - (MaleCount + FemaleCount);
  if (diff > 0) then begin
    //Find the smallest number and add the difference to it.
    if (MaleCount < FemaleCount) then begin
      MaleCount := MaleCount + diff;
    end
    else begin
        FemaleCount := FemaleCount + diff;
    end;
  end;
end;

function TSampleNamesGenerator.CreateSampleName(gender : char) : TSampleName;
var
  SampleName: TSampleName;
  ZipCode: TZipCode;
  Year, genNumber: Integer;
  SampleDate: TDateTime;
begin
  SampleName := TSampleName.Create;
  inc(CurrentRecordId);

  //Get ID
  SampleName.ID:= CurrentRecordId;
  SampleName.Gender:= gender;
  //First Name
  if (gender = 'M') then begin
    genNumber := Random(MaleFirstNames.Count);
    SampleName.FirstName:= MaleFirstNames[genNumber];
  end
  else begin
    genNumber := Random(FemaleFirstNames.Count);
    SampleName.FirstName:= FemaleFirstNames[genNumber];
  end;
  //Last Name
  genNumber:= Random(LastNames.Count);
  SampleName.LastName:= LastNames[genNumber];
  //Middle Initial
  genNumber := Random(20);
  SampleName.MI:= CommonLetters[genNumber];
  //Email
  genNumber:= Random(EmailDomains.Count);
  SampleName.Email := LowerCase(SampleName.FirstName[1]) + LowerCase(SampleName.LastName)
    + IntToStr(GenerateRandomFixedDigitNumber(2)) + '@' + EmailDomains[genNumber];
  //Address
  genNumber:= Random(StreetNames.Count);
  SampleName.Address:= IntToStr(GenerateRandomFixedDigitNumber(4)) + ' ' + StreetNames[genNumber];
  //City, State, Zip
  genNumber:= Random(ZipCodes.Count);
  ZipCode := ZipCodes[genNumber];
  SampleName.City:= ZipCode.City;
  SampleName.StateCode:= ZipCode.StateCode;
  SampleName.ZipCode:= ZipCode.ZipCode;
  //Area Code & telephone Number
  GetAreaCodesForState(ZipCode.StateCode);
  genNumber:= Random(AreaCodes.Count);
  SampleName.PhoneNumber:= '(' + AreaCodes[genNumber] + ') ' + IntToStr(GenerateRandomFixedDigitNumber(3))
    + '-' + IntToStr(GenerateRandomFixedDigitNumber(4));
  //Birth Date
  Year:= CurrentYear - (20 + Random(76));
  SampleDate:= EncodeDate(Year, 1, 1);
  SampleName.BirthDate:= SampleDate + Random(360);
  Result := SampleName;
end;

procedure TSampleNamesGenerator.RetrieveZipCodeListIncludingNearbyCities;
var
  MinLatt, MaxLatt, MinLong, MaxLong: Float;
  PtExpand: Float;
  SqlList: TStringList;
begin
  //Get max lattitude & longitude for city
  PtExpand := 0.15;
  SqlList := TStringList.Create;
  if (CountryCode = 'US') then begin
    SqlList.Add('SELECT MIN(ABS(LATTITUDE)) AS Min_Lattitude, MAX(ABS(LATTITUDE)) AS Max_Lattitude, ');
    SqlList.Add('MIN(ABS(LONGITUDE)) AS Min_Longitude, MAX(ABS(LONGITUDE)) AS Max_Longitude ');
    SqlList.Add('FROM ZipCodes WHERE STATE = ''' + StateCode + ''' AND CITY = ''' + City + '''');
  end
  else begin
    SqlList.Add('SELECT MIN(ABS(LATTITUDE)) AS Min_Lattitude, MAX(ABS(LATTITUDE)) AS Max_Lattitude, ');
    SqlList.Add('MIN(ABS(LONGITUDE)) AS Min_Longitude, MAX(ABS(LONGITUDE)) AS Max_Longitude ');
    SqlList.Add('FROM CanadaZipCodes WHERE PROVINCE = ''' + StateCode + ''' AND CITY = ''' + City + '''');
  end;
  RunQuery(SqlList);
  while (not DataRetrieveQuery.EOF) do begin
    MinLatt := DataRetrieveQuery.FieldByName('Min_Lattitude').AsFloat;
    MaxLatt := DataRetrieveQuery.FieldByName('Max_Lattitude').AsFloat;
    MinLong := DataRetrieveQuery.FieldByName('Min_Longitude').AsFloat;
    MaxLong := DataRetrieveQuery.FieldByName('Max_Longitude').AsFloat;
    DataRetrieveQuery.Next;
  end;
  DataRetrieveQuery.Close;

  //Now get list of nearby zip codes.
  SqlList.Clear;
  if (CountryCode = 'US') then begin
    SqlList.Add('SELECT city, state, zip_code ');
    SqlList.Add('FROM zipcodes ');
    SqlList.Add('WHERE ABS(lattitude) BETWEEN ' + FloatToStr(MinLatt - PtExpand) + ' AND ' + FloatToStr(MaxLatt + PtExpand) + ' ');
    SqlList.Add('  AND ABS(longitude) BETWEEN ' + FloatToStr(MinLong - PtExpand) + ' AND ' + FloatToStr(MaxLong + PtExpand) + ' ');
    SqlList.Add('ORDER BY state, city');
  end
  else begin
    SqlList.Add('SELECT CITY, PROVINCE AS state, POSTAL_CODE AS zip_code ');
    SqlList.Add('FROM CanadaZipCodes ');
    SqlList.Add('WHERE ABS(lattitude) BETWEEN ' + FloatToStr(MinLatt - PtExpand) + ' AND ' + FloatToStr(MaxLatt + PtExpand) + ' ');
    SqlList.Add('  AND ABS(longitude) BETWEEN ' + FloatToStr(MinLong - PtExpand) + ' AND ' + FloatToStr(MaxLong + PtExpand) + ' ');
    SqlList.Add('ORDER BY state, city');
  end;
  RunQuery(SqlList);
  ZipCodes.Clear;
  while (not DataRetrieveQuery.EOF) do begin
    ZipCodes.Add(TZipCode.Create(
      DataRetrieveQuery.FieldByName('zip_code').AsString,
      DataRetrieveQuery.FieldByName('city').AsString,
      DataRetrieveQuery.FieldByName('state').AsString));
    DataRetrieveQuery.Next;
  end;
  DataRetrieveQuery.Close;
end;

procedure TSampleNamesGenerator.RetrieveZipCodeListSelectedCity;
var
  MinLatt, MaxLatt, MinLong, MaxLong: Float;
  PtExpand: Float;
  SqlList: TStringList;
begin
  SqlList := TStringList.Create;
  if (CountryCode = 'US') then begin
    SqlList.Add('SELECT city, state, zip_code ');
    SqlList.Add('FROM zipcodes ');
    SqlList.Add('WHERE state = ''' + StateCode + ''' ');
    SqlList.Add('  AND city = ''' + City + ''' ');
  end
  else begin
    SqlList.Add('SELECT CITY, PROVINCE AS state, POSTAL_CODE AS zip_code ');
    SqlList.Add('FROM CanadaZipCodes ');
    SqlList.Add('WHERE PROVINCE = ''' + StateCode + ''' ');
    SqlList.Add('  AND city = ''' + City + ''' ');
  end;
  RunQuery(SqlList);
  ZipCodes.Clear;
  while (not DataRetrieveQuery.EOF) do begin
    ZipCodes.Add(TZipCode.Create(
      DataRetrieveQuery.FieldByName('zip_code').AsString,
      DataRetrieveQuery.FieldByName('city').AsString,
      DataRetrieveQuery.FieldByName('state').AsString));
    DataRetrieveQuery.Next;
  end;
  DataRetrieveQuery.Close;
end;

procedure TSampleNamesGenerator.GetAreaCodesForState(state: String);
var
  SqlList: TStringList;
begin
  if (not StateLoaded.Equals(state)) then begin
    SqlList := TStringList.Create;
    SqlList.Add('SELECT AREA_CODE FROM AreaCodes ');
    SqlList.Add('WHERE STATE_CODE = ''' + state + ''' AND COUNTRY_CODE = ''' + CountryCode + ''' ');
    RunQuery(SqlList);
    AreaCodes.Clear;
    while (not DataRetrieveQuery.EOF) do begin
      AreaCodes.Add(DataRetrieveQuery.FieldByName('AREA_CODE').AsString);
      DataRetrieveQuery.Next;
    end;
    DataRetrieveQuery.Close;
   StateLoaded := state;
  end;
end;

procedure TSampleNamesGenerator.LoadLists;
var
  SqlList: TStringList;
begin
  SqlList := TStringList.Create;
  //female first names
  SqlList.Add('SELECT first_name FROM FemaleFirstNames');
  RunQuery(SqlList);
  FemaleFirstNames.Clear;
  while (not DataRetrieveQuery.EOF) do begin
    FemaleFirstNames.Add(DataRetrieveQuery.FieldByName('first_name').AsString);
    DataRetrieveQuery.Next;
  end;
  //male first names
  SqlList.Clear;
  SqlList.Add('SELECT first_name FROM MaleFirstNames');
  RunQuery(SqlList);
  MaleFirstNames.Clear;
  while (not DataRetrieveQuery.EOF) do begin
    MaleFirstNames.Add(DataRetrieveQuery.FieldByName('first_name').AsString);
    DataRetrieveQuery.Next;
  end;
  //Last Names
  SqlList.Clear;
  SqlList.Add('SELECT LAST_NAME FROM LastNames');
  RunQuery(SqlList);
  LastNames.Clear;
  while (not DataRetrieveQuery.EOF) do begin
    LastNames.Add(DataRetrieveQuery.FieldByName('LAST_NAME').AsString);
    DataRetrieveQuery.Next;
  end;
  //Street Names
  SqlList.Clear;
  SqlList.Add('SELECT STREET_NAME || '' '' || STREET_TYPE AS StreetName FROM StreetNames');
  RunQuery(SqlList);
  StreetNames.Clear;
  while (not DataRetrieveQuery.EOF) do begin
    StreetNames.Add(DataRetrieveQuery.FieldByName('StreetName').AsString);
    DataRetrieveQuery.Next;
  end;
  //Email Domains
  SqlList.Clear;
  SqlList.Add('SELECT Email_Domain FROM EmailDomains');
  RunQuery(SqlList);
  EmailDomains.Clear;
  while (not DataRetrieveQuery.EOF) do begin
    EmailDomains.Add(DataRetrieveQuery.FieldByName('Email_Domain').AsString);
    DataRetrieveQuery.Next;
  end;
  DataRetrieveQuery.Close;
end;

function TSampleNamesGenerator.GenerateRandomFixedDigitNumber(numDigits: Integer): Int64;
var
  baseNum, randSeed: Int64;
begin
  if (numDigits = 1) then begin
    Result := Random(10);
  end
  else begin
    baseNum:= 10 ** (numDigits - 1);
    randSeed := (10 ** (numDigits)) - baseNum;
    Result := baseNum + Random(randSeed);
  end;
end;

procedure TSampleNamesGenerator.RunQuery(Sql: TStringList);
var
  i: Integer;
begin
  DataRetrieveQuery.Close;
  DataRetrieveQuery.SQL.Clear;
  for i := 0 to Sql.Count - 1 do begin
    DataRetrieveQuery.SQL.Add(Sql[i]);
  end;
  DataRetrieveQuery.Open;
  DataRetrieveQuery.First;
end;

end.

