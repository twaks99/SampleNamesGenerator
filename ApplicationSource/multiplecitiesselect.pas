unit MultipleCitiesSelect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, dataModule;

type

  { TMultipleCitiesSelect }

  TMultipleCitiesSelect = class(TForm)
    btnAddCity: TBitBtn;
    comboState: TComboBox;
    comboCity: TComboBox;
    lblCitiesList: TLabel;
    lblState: TLabel;
    lblCity: TLabel;
    listCities: TListBox;
    procedure comboStateSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure PopulateStatesCombo;
    procedure PopulateCitiesCombo;
    function GetStateCodeFromCombo : String;
  public
    CountryCode: String;
  end;

var
  MultipleCitiesSelect: TMultipleCitiesSelect;

implementation

{$R *.lfm}

procedure TMultipleCitiesSelect.comboStateSelect(Sender: TObject);
begin
  PopulateCitiesCombo;
end;

procedure TMultipleCitiesSelect.FormCreate(Sender: TObject);
begin

end;

procedure TMultipleCitiesSelect.PopulateStatesCombo;
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

procedure TMultipleCitiesSelect.PopulateCitiesCombo;
var
  state_id: String;
  sql: TStringList;
begin
  state_id := GetStateCodeFromCombo();

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
    comboCities.Items.Clear;
    while (not dataModule.dataModuleMain.queryCities.EOF) do begin
      comboCities.Items.Add(dataModule.dataModuleMain.queryCities.FieldByName('city').AsString);
      dataModule.dataModuleMain.queryCities.Next;
    end;
  end;
end;

function TMultipleCitiesSelect.GetStateCodeFromCombo : String;
var
  state_name, state_code: String;
begin
  state_code := String.Empty;
  state_name := String.Empty;
  if (comboState.ItemIndex >= 0) then begin
    state_name := comboStates.Items[comboState.ItemIndex];
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

