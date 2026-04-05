unit MultipleCitiesSelect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, dataModule, CityRecords;

type

  { TCitiesSelectForm }

  TCitiesSelectForm = class(TForm)
    btnSortList: TBitBtn;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    btnRemove: TBitBtn;
    btnAddCity: TBitBtn;
    comboCountry: TComboBox;
    comboState: TComboBox;
    comboCity: TComboBox;
    comboGroup: TComboBox;
    lblCountry: TLabel;
    lblCitiesList: TLabel;
    lblState: TLabel;
    lblCity: TLabel;
    lblGroup: TLabel;
    listCities: TListBox;
    procedure btnAddCityClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSortListClick(Sender: TObject);
    procedure comboCountrySelect(Sender: TObject);
    procedure comboGroupSelect(Sender: TObject);
    procedure comboStateSelect(Sender: TObject);
		procedure FormCreate(Sender: TObject);
    procedure listCitiesSelectionChange(Sender: TObject; User: boolean);
  private
    procedure PopulateCityGroupsCombo;
    procedure PopulateStatesCombo;
    procedure PopulateCitiesCombo;
    procedure PopulateStateListBox;
    procedure SaveCityList;
    function GetStateCodeFromCombo : String;
    function GetStateNameFromCode(state_code : String) : String;
    function IsCityRecordDuplicate(cityRecord : TCityRecord) : Boolean;
  public
    CountryCode: String;
    Cities: TCityRecordsList;
    GroupSelected : String;
    procedure InitForm;
  end;

var
  CitiesSelectForm: TCitiesSelectForm;

implementation

{$R *.lfm}

procedure TCitiesSelectForm.comboStateSelect(Sender: TObject);
begin
  comboCity.Caption := '';
  PopulateCitiesCombo;
end;

procedure TCitiesSelectForm.FormCreate(Sender: TObject);
begin
  InitForm;
end;

procedure TCitiesSelectForm.listCitiesSelectionChange(Sender: TObject;
  User: boolean);
var
  selCityRecord : TCityRecord;
  i : Integer;
  selectedCountry, state_name : String;
begin
  selCityRecord := Cities[listCities.ItemIndex];
  if selCityRecord.CountryCode = 'US' then
    selectedCountry := 'United States'
  else
    selectedCountry := 'Canada';
  //select country
  CountryCode := selCityRecord.CountryCode;
  for i := 0 to comboCountry.Items.Count - 1 do begin
    if (comboCountry.Items[i] = selectedCountry) then begin
      comboCountry.ItemIndex := i;
      PopulateStatesCombo;
      break;
    end;
  end;
  //select state
  state_name := GetStateNameFromCode(selCityRecord.StateName);
  for i := 0 to comboState.Items.Count - 1 do begin
    if (comboState.Items[i] = state_name) then begin
      comboState.ItemIndex:= i;
      PopulateCitiesCombo;
      break;
    end;
  end;
  //select city
  for i := 0 to comboCity.Items.Count - 1 do begin
    if (comboCity.Items[i] = selCityRecord.CityName) then begin
      comboCity.ItemIndex := i;
      break;
    end;
  end;
end;

procedure TCitiesSelectForm.btnRemoveClick(Sender: TObject);
var
  selectedIdx: Integer;
begin
  if (listCities.ItemIndex >= 0) then begin
    selectedIdx := listCities.ItemIndex;
    listCities.Items.Delete(selectedIdx);
    Cities.Delete(selectedIdx);
  end;
end;

procedure TCitiesSelectForm.btnCancelClick(Sender: TObject);
begin
  self.Hide;
end;

procedure TCitiesSelectForm.btnAddCityClick(Sender: TObject);
var
  cityName, stateName : String;
  selectedCity : TCityRecord;
begin
  cityName := comboCity.Items[comboCity.ItemIndex];
  stateName := comboState.Items[comboState.ItemIndex];
  selectedCity := TCityRecord.Create(cityName, GetStateCodeFromCombo, CountryCode);
  if (not IsCityRecordDuplicate(selectedCity)) then begin
    Cities.Add(selectedCity);
    listCities.Items.Add(selectedCity.ToString);
  end;
end;

procedure TCitiesSelectForm.btnSaveClick(Sender: TObject);
begin
  SaveCityList;
  self.Hide;
end;

procedure TCitiesSelectForm.SaveCityList;
var
  grpName : String;
begin
  grpName := comboGroup.Items[comboGroup.ItemIndex];
  dataModuleMain.UpdateCitiesListByGroupName(grpName, Cities);
end;

procedure TCitiesSelectForm.btnSortListClick(Sender: TObject);
var
  OrderedCitiesList : TCityRecordsList;
begin
  OrderedCitiesList := dataModuleMain.OrderCitiesListByState(Cities);
  Cities := OrderedCitiesList;
  PopulateStateListBox;
  SaveCityList;
end;

procedure TCitiesSelectForm.comboCountrySelect(Sender: TObject);
var
  countrySelected: String;
begin
  countrySelected := comboCountry.Items[comboCountry.ItemIndex];
  if (countrySelected = 'United States') then
    CountryCode:= 'US'
  else
    CountryCode := 'CA';
  PopulateStatesCombo;
end;

procedure TCitiesSelectForm.comboGroupSelect(Sender: TObject);
var
  i : Integer;
  grpName : String;
begin
  grpName:= comboGroup.Items[comboGroup.ItemIndex];
  GroupSelected := grpName;
  if (grpName <> '') then begin
    Cities := dataModuleMain.GetCitiesListByGroupName(grpName);
    PopulateStateListBox;
  end;
end;

procedure TCitiesSelectForm.InitForm;
begin
  CountryCode := 'US';
  Cities := TCityRecordsList.Create;
  PopulateCityGroupsCombo;
  PopulateStatesCombo;
  PopulateStateListBox;
end;

procedure TCitiesSelectForm.PopulateCityGroupsCombo;
var
  i : Integer;
begin
  comboGroup.Items.Clear;
  for i := 0 to dataModuleMain.CityGroupsList.Count - 1 do begin
    comboGroup.Items.Add(dataModuleMain.CityGroupsList[i].GroupName);
  end;
end;

procedure TCitiesSelectForm.PopulateStatesCombo;
begin
  comboState.Items.Clear;
  dataModule.dataModuleMain.queryStates.Close;
  dataModule.dataModuleMain.queryStates.Params[0].Value := CountryCode;
  dataModule.dataModuleMain.queryStates.Open;
  dataModule.dataModuleMain.QueryStates.First;
  while (not dataModule.dataModuleMain.QueryStates.EOF) do begin
    comboState.Items.Add(dataModule.dataModuleMain.QueryStates.FieldByName('state_name').AsString);
    dataModule.dataModuleMain.QueryStates.Next;
  end;
end;

procedure TCitiesSelectForm.PopulateStateListBox;
var
  i : Integer;
begin
  listCities.Items.Clear;
  for i := 0 to Cities.Count - 1 do begin
    listCities.Items.Add(Cities[i].ToString);
  end;
end;

procedure TCitiesSelectForm.PopulateCitiesCombo;
var
  state_id: String;
  sql: TStringList;
begin
  state_id := GetStateCodeFromCombo;

  if (not String.IsNullOrEmpty(state_id)) then begin
    if (dataModule.dataModuleMain.queryCities.Active) then begin
      dataModule.dataModuleMain.queryCities.Close;
    end;
    sql := TStringList.Create;
    if (countryCode = 'US') then begin
      sql.Add('SELECT DISTINCT city FROM zipcodes ');
      sql.Add('WHERE state = :state ');
      sql.Add('ORDER BY city');
    end
    else begin
      sql.Add('SELECT DISTINCT CITY FROM CanadaZipCodes ');
      sql.Add('WHERE PROVINCE = :state ');
      sql.Add('ORDER BY CITY');
    end;
    dataModule.dataModuleMain.queryCities.SQL := sql;
    dataModule.dataModuleMain.queryCities.Params[0].Value := state_id;
    dataModule.dataModuleMain.queryCities.Open;
    dataModule.dataModuleMain.queryCities.First;
    comboCity.Items.Clear;
    while (not dataModule.dataModuleMain.queryCities.EOF) do begin
      comboCity.Items.Add(dataModule.dataModuleMain.queryCities.FieldByName('city').AsString);
      dataModule.dataModuleMain.queryCities.Next;
    end;
  end;
end;

// gets the state code from the state name selected in the state combo box by looping 
//through the states query and comparing the state name of each record with the state 
// name selected in the combo box
function TCitiesSelectForm.GetStateCodeFromCombo : String;
var
  state_name, state_code: String;
begin
  state_code := String.Empty;
  state_name := String.Empty;
  if (comboState.ItemIndex >= 0) then begin
    state_name := comboState.Items[comboState.ItemIndex];
    dataModule.dataModuleMain.queryStates.First;
    while (not dataModule.dataModuleMain.queryStates.EOF) do begin
      if (dataModule.dataModuleMain.queryStates.FieldByName('state_name').AsString = state_name) then begin
        state_code := dataModule.dataModuleMain.queryStates.FieldByName('state_code').AsString;
        Break;
      end;
      dataModule.dataModuleMain.queryStates.Next;
    end;
  end;
  Result := state_code;
end;

// gets the state name from the state code by looping through the states query and 
// comparing the state code of each record with the state code passed as a parameter
// returns the state name if a match is found, an empty string otherwise
// this function is used to get the state name to display in the state combo box 
// when a city record is selected from the list of cities.
function TCitiesSelectForm.GetStateNameFromCode(state_code : String) : String;
var
  state_name : String;
begin
  state_name := String.Empty;
  dataModule.dataModuleMain.queryStates.First;
  while (not dataModule.dataModuleMain.queryStates.EOF) do begin
    if (dataModule.dataModuleMain.queryStates.FieldByName('state_code').AsString = state_code) then begin
      state_name := dataModule.dataModuleMain.queryStates.FieldByName('state_name').AsString;
      break;
		end;
    dataModule.dataModuleMain.queryStates.Next;
	end;
  Result := state_name;
end;

// checks if the city record already exists in the list of cities to prevent duplicates
// returns true if the city record already exists in the list, false otherwise
// the check is based on the country code, state name and city name of the city record
// this function is called when the user tries to add a new city record to the list of 
// cities.
function TCitiesSelectForm.IsCityRecordDuplicate(cityRecord : TCityRecord) : Boolean;
var
  i : Integer;
  InList : Boolean;
begin
  InList := False;
  for i := 0 to Cities.Count - 1 do begin
    if ((Cities[i].CountryCode = cityRecord.CountryCode) and
        (Cities[i].StateName = cityRecord.StateName) and
        (Cities[i].CityName = cityRecord.CityName)) then begin
      InList := True;
      break;
		end;
	end;
  Result := InList;
end;

end.

