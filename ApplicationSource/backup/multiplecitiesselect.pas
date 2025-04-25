unit MultipleCitiesSelect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, dataModule, CityRecords;

type

  { TCitiesSelectForm }

  TCitiesSelectForm = class(TForm)
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    btnRemove: TBitBtn;
    btnAddCity: TBitBtn;
    comboState: TComboBox;
    comboCity: TComboBox;
    lblCitiesList: TLabel;
    lblState: TLabel;
    lblCity: TLabel;
    listCities: TListBox;
    procedure btnAddCityClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure comboStateSelect(Sender: TObject);
  private
    procedure PopulateStatesCombo;
    procedure PopulateCitiesCombo;
    procedure PopulateStateListBox;
    function GetStateCodeFromCombo : String;
  public
    CountryCode: String;
    CitiesList: TCityRecordsList;
    procedure InitForm(cntryCode: String; ctList: TCityRecordsList);
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

procedure TCitiesSelectForm.btnRemoveClick(Sender: TObject);
var
  selectedIdx: Integer;
begin
  if (listCities.ItemIndex >= 0) then begin
    selectedIdx := listCities.ItemIndex;
    listCities.Items.Delete(selectedIdx);
    CitiesList.Delete(selectedIdx);
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
  CitiesList.Add(selectedCity);
  listCities.Items.Add(selectedCity.ToString);
end;

procedure TCitiesSelectForm.btnSaveClick(Sender: TObject);
begin
  dataModule.dataModuleMain.MasterCitiesList := CitiesList;
  self.Hide;
end;

procedure TCitiesSelectForm.InitForm(cntryCode: String; ctList: TCityRecordsList);
begin
  self.CountryCode := cntryCode;
  self.CitiesList := ctList;
  PopulateStatesCombo;
  PopulateStateListBox;
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
  for i := 0 to CitiesList.Count - 1 do begin
    listCities.Items.Add(CitiesList[i].ToString);
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

end.

