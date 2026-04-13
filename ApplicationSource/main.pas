unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, Grids, dataModule, GenerateSampleName, fpSpreadsheet, fpsTypes,
  Clipbrd, ExtCtrls, Buttons, Menus, Spin, fpsallformats, DetailForm,
  InsertStatementForm, savedsettings, dataexport, MultipleCitiesSelect, CityRecords;

type

  { TformMain }

  TformMain = class(TForm)
    btnMultipleCities: TBitBtn;
    btnGenerate: TBitBtn;
    btnClose: TBitBtn;
    btnClipboard: TBitBtn;
    btnExport: TBitBtn;
    chkMultipleCities: TCheckBox;
    chkIncludeNearby: TCheckBox;
    chkRandomGDist: TCheckBox;
    comboCountry: TComboBox;
    comboStates: TComboBox;
    comboCities: TComboBox;
    dialogExport: TSaveDialog;
    grpRowCount: TGroupBox;
    grpGenderDisttribution: TGroupBox;
    lblCountry: TLabel;
    lblMaleDist: TLabel;
    lblFemaleDist: TLabel;
    lblNumRows: TLabel;
    MainMenu1: TMainMenu;
    menugroupFile: TMenuItem;
    menuitemGenInsertStatements: TMenuItem;
    menuitemPopClipRow: TMenuItem;
    menuitemClipRow: TMenuItem;
    menuItemShowDetails: TMenuItem;
    menuitemExport: TMenuItem;
    menuitemClipboard: TMenuItem;
    menuitemClose: TMenuItem;
    menugroupAction: TMenuItem;
    menuitemGenerateNames: TMenuItem;
    popupNamesGrid: TPopupMenu;
    radiogroupDelimeter: TRadioGroup;
    spinMaleDist: TSpinEdit;
    spinFemaleDist: TSpinEdit;
    lblState: TLabel;
    lblCities: TLabel;
    lblSampleNames: TLabel;
    gridResults: TStringGrid;
    txtNumRows: TEdit;
    procedure btnClipboardClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnMultipleCitiesClick(Sender: TObject);
    procedure chkRandomGDistChange(Sender: TObject);
    procedure comboCountrySelect(Sender: TObject);
    procedure comboStatesSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure gridResultsDblClick(Sender: TObject);
    procedure menuitemClipRowClick(Sender: TObject);
    procedure menuitemGenInsertStatementsClick(Sender: TObject);
    procedure menuItemShowDetailsClick(Sender: TObject);
    procedure spinFemaleDistChange(Sender: TObject);
    procedure spinMaleDistChange(Sender: TObject);
  private
    SamplesGenerator: TSampleNamesGenerator;
    SampleNamesList: TSampleNamesList;
    settings: TSavedSettings;
    CountryCode: String;
    //CitiesList: TCityRecordsList;
    //MultipleCities: Boolean;
    function GetStateCodeFromCombo : String;
    procedure PopulateStatesCombo;
    procedure PopulateSampleNamesGrid;
    procedure SetSavedStateName;
    procedure PopulateCitiesCombo;
    procedure SelectSavedCountry;
  public

  end;

var
  formMain: TformMain;

implementation

{$R *.lfm}

{ TformMain }

procedure TformMain.FormCreate(Sender: TObject);
begin
  CountryCode := 'US';
  Settings := dataModuleMain.SavedSettings;
  SamplesGenerator := TSampleNamesGenerator.Create();
  SelectSavedCountry;
  PopulateStatesCombo;
end;

procedure TformMain.SelectSavedCountry;
begin
  if (not String.IsNullOrEmpty(Settings.CountryName)) then begin
    CountryCode := Settings.CountryName;
    if (Settings.CountryName = 'US') then
      comboCountry.ItemIndex := 0
    else
      comboCountry.ItemIndex := 1;
    comboCountrySelect(self);
  end;
end;

procedure TformMain.gridResultsDblClick(Sender: TObject);
var
	selectedIdx: Integer;
  sampleName: TSampleName;
begin
	selectedIdx:= gridResults.Selection.Top - 1;
  if (selectedIdx > 0) then begin
		sampleName := SampleNamesList[selectedIdx];
  	NameDetailForm.SetNameRecord(sampleName);
	  NameDetailForm.ShowModal;
  end;
end;

procedure TformMain.menuitemClipRowClick(Sender: TObject);
var
  delim: Char;
  clipTxt, crlf : String;
  colnum, selectedIdx : Integer;
begin
	if (radiogroupDelimeter.Items[radiogroupDelimeter.ItemIndex].Equals('Tab')) then
  	delim := Chr(9)
  else
  	delim := ',';
  crlf := Chr(13) + Chr(10);
  clipTxt := String.Empty;
  selectedIdx:= gridResults.Selection.Top;
  //copy column header names.
  for colnum := 0 to gridResults.ColCount - 1 do begin
    if (colnum > 0) then begin
      clipTxt := clipTxt + delim;
    end;
    clipTxt:= clipTxt + gridResults.Columns[colnum].Title.Caption;
  end;
  clipTxt:= clipTxt + crlf;

  for colnum := 0 to gridResults.ColCount - 1 do begin
    if (colnum > 0) then begin
      clipTxt:= clipTxt + delim;
    end;
    clipTxt := clipTxt + gridResults.Cells[colnum, selectedIdx];
  end;
  clipTxt := clipTxt + crlf;

  Clipboard.AsText := clipTxt;

end;

procedure TformMain.menuitemGenInsertStatementsClick(Sender: TObject);
begin
  if (SampleNamesList = nil) then begin
    ShowMessage('There is no list available.');
  end
  else begin
    FormInsertStatement.SetNamesList(SampleNamesList, Settings);
    FormInsertStatement.ShowModal;
  end;
end;

procedure TformMain.menuItemShowDetailsClick(Sender: TObject);
var
	selectedIdx: Integer;
  sampleName: TSampleName;
begin
	selectedIdx:= gridResults.Selection.Top - 1;
	sampleName := SampleNamesList[selectedIdx];
  NameDetailForm.SetNameRecord(sampleName);
  NameDetailForm.ShowModal;
end;

procedure TformMain.spinFemaleDistChange(Sender: TObject);
begin
  spinMaleDist.Value := 100 - spinFemaleDist.Value;
end;

procedure TformMain.spinMaleDistChange(Sender: TObject);
begin
  spinFemaleDist.Value := 100 - spinMaleDist.Value;
end;

procedure TformMain.comboStatesSelect(Sender: TObject);
begin
  comboCities.Caption := '';
  PopulateCitiesCombo;
end;

procedure TformMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Settings.SaveSettingsToFile;
end;

procedure TformMain.PopulateCitiesCombo;
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
    if (CountryCode = 'US') then begin
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

procedure TformMain.btnGenerateClick(Sender: TObject);
var
  state_code, city_name, row_count: String;
  num_rows, maleDist, femaleDist: Integer;
  randomGenderDist, includeNearbyCities, multCities : Boolean;
begin
  row_count:= Trim(txtNumRows.Text);
  num_rows := StrToInt(txtNumRows.Text);
  maleDist := spinMaleDist.Value;
  femaleDist := spinFemaleDist.Value;
  randomGenderDist := chkRandomGDist.Checked;
  includeNearbyCities := chkIncludeNearby.Checked;
  multCities := chkMultipleCities.Checked;
  if (multCities) then begin
    if (CitiesSelectForm.GroupSelected = String.Empty) then begin
      ShowMessage('Please select a city group.');
    end
    else begin
      SampleNamesList := SamplesGenerator.GenerateSampleSet(CountryCode, '', '', num_rows,
        maleDist, femaleDist, randomGenderDist, includeNearbyCities, multCities, CitiesSelectForm.GroupSelected);
      if (SamplesGenerator.Successful) then begin
        PopulateSampleNamesGrid;
      end
      else begin
        ShowMessage(SamplesGenerator.ErrorMsg);
      end;
    end;
  end
  else if ((comboStates.ItemIndex >= 0) and (comboCities.ItemIndex >= 0) and (not (row_count.Equals(String.Empty)))) then begin
    state_code := GetStateCodeFromCombo();
    city_name := comboCities.Items[comboCities.ItemIndex];

    SampleNamesList := SamplesGenerator.GenerateSampleSet(CountryCode, state_code, city_name, num_rows,
      maleDist, femaleDist, randomGenderDist, includeNearbyCities, multCities, CitiesSelectForm.GroupSelected);
    PopulateSampleNamesGrid;
    Settings.CountryName:= CountryCode;
    Settings.StateName:= GetStateCodeFromCombo;
    Settings.CityName := city_name;
    Settings.NumRows := num_rows;
    Settings.MalePercentage := maleDist;
    Settings.FemalePercentage := femaleDist;
    Settings.RandomGenderDistribution := randomGenderDist;
    Settings.IncludeNearbyCities := chkIncludeNearby.Checked;
    Settings.MultipleCities := chkMultipleCities.Checked;
  end
  else begin
    MessageDlg('Message', 'State, City, and Number of rows are required fields.', TMsgDlgType.mtInformation, [mbOK], '');
  end;
end;

procedure TformMain.btnMultipleCitiesClick(Sender: TObject);
begin
  //CitiesSelectForm.InitForm;
  CitiesSelectForm.ShowModal;
end;

procedure TformMain.chkRandomGDistChange(Sender: TObject);
begin
  if (chkRandomGDist.Checked) then begin
    spinMaleDist.Enabled := false;
    spinFemaleDist.Enabled := false;
  end
  else begin
    spinMaleDist.Enabled := true;
    spinFemaleDist.Enabled := true;
  end;
end;

procedure TformMain.comboCountrySelect(Sender: TObject);
var
  countrySelected: String;
begin
  countrySelected := comboCountry.Items[comboCountry.ItemIndex];
  if (countrySelected = 'United States') then
    CountryCode:= 'US'
  else
    CountryCode := 'CA';
  dataModuleMain.queryStates.Params[0].Value := CountryCode;
  dataModuleMain.queryStates.Close;
  dataModuleMain.queryStates.Open;
  PopulateStatesCombo;
end;

procedure TformMain.btnClipboardClick(Sender: TObject);
var
  delim: Char;
  clipTxt, crlf : String;
  rownum, colnum : Integer;
begin
	if (radiogroupDelimeter.Items[radiogroupDelimeter.ItemIndex].Equals('Tab')) then
  	delim := Chr(9)
  else
  	delim := ',';
  crlf := Chr(13) + Chr(10);
  clipTxt := String.Empty;
  //copy column header names.
  for colnum := 0 to gridResults.ColCount - 1 do begin
    if (colnum > 0) then begin
      clipTxt := clipTxt + delim;
    end;
    clipTxt:= clipTxt + gridResults.Columns[colnum].Title.Caption;
  end;
  clipTxt:= clipTxt + crlf;

  for rownum := 1 to gridResults.RowCount - 1  do begin
    for colnum := 0 to gridResults.ColCount - 1 do begin
      if (colnum > 0) then begin
        clipTxt:= clipTxt + delim;
      end;
      clipTxt := clipTxt + gridResults.Cells[colnum, rownum];
    end;
    clipTxt := clipTxt + crlf;
  end;
  Clipboard.AsText := clipTxt;

end;

procedure TformMain.btnCloseClick(Sender: TObject);
begin
  self.Close;
end;

procedure TformMain.btnExportClick(Sender: TObject);
var
  dataExporter: TDataExport;
  exportFileName: String;
begin
	if dialogExport.Execute then begin
    //MessageDlg('Message', 'Filename: ' + savedlgExport.FileName, TMsgDlgType.mtInformation, [mbOK], '');
    if (dialogExport.FileName <> String.Empty) then begin
      exportFileName := dialogExport.FileName;
      dataExporter := TDataExport.Create;
      dataExporter.ExportFile(SampleNamesList, exportFileName);
      if (not dataExporter.ExportSuccessful) then begin
        MessageDlg('Message', dataExporter.StatusMsg, TMsgDlgType.mtInformation, [mbOK], '');
      end;
    end
    else begin
      MessageDlg('Message', 'A filename must be specified.', TMsgDlgType.mtInformation, [mbOK], '');
		end;
  end;

end;

procedure TformMain.PopulateStatesCombo;
begin
  comboStates.Items.Clear;
  dataModuleMain.QueryStates.First;
  while (not dataModuleMain.QueryStates.EOF) do begin
    comboStates.Items.Add(dataModuleMain.QueryStates.FieldByName('state_name').AsString);
    dataModuleMain.QueryStates.Next;
  end;
  if (Settings.StateName <> String.Empty) then
    SetSavedStateName;
end;

procedure TformMain.SetSavedStateName;
var
  savedStateCode, savedCityName, currentStateCode : String;
  idx : Integer;
begin
  savedStateCode:= Settings.StateName;
  savedCityName:= Settings.CityName;
  idx := 0;
  dataModuleMain.queryStates.First;
  while (not dataModuleMain.queryStates.EOF) do begin
    currentStateCode := dataModuleMain.queryStates.FieldByName('state_code').AsString;
    if (dataModuleMain.queryStates.FieldByName('state_code').AsString = savedStateCode) then begin
      comboStates.ItemIndex:= idx;
      Break;
    end;
    Inc(idx);
    dataModuleMain.queryStates.Next;
  end;
  //Populate combo box for cities.
  PopulateCitiesCombo;
  //Set city to saved value.
  if (dataModule.dataModuleMain.queryCities.Active) then begin
    dataModule.dataModuleMain.queryCities.First;
    idx := 0;
    while (not dataModule.dataModuleMain.queryCities.EOF) do begin
      if (dataModule.dataModuleMain.queryCities.FieldByName('city').AsString = savedCityName) then begin
        comboCities.ItemIndex:= idx;
        break;
      end;
      Inc(idx);
      dataModule.dataModuleMain.queryCities.Next;
    end;
  end;
  //Set value for number of rows
  txtNumRows.Text := IntToStr(Settings.NumRows);
  //Set Gender distribution parameters.
  chkRandomGDist.Checked := Settings.RandomGenderDistribution;
  chkIncludeNearby.Checked := Settings.IncludeNearbyCities;
  spinMaleDist.Value := Settings.MalePercentage;
  spinFemaleDist.Value := Settings.FemalePercentage;
  chkMultipleCities.Checked := Settings.MultipleCities;
end;

function TformMain.GetStateCodeFromCombo : String;
var
  state_name, state_code: String;
begin
  state_code := String.Empty;
  state_name := String.Empty;
  if (comboStates.ItemIndex >= 0) then begin
    state_name := comboStates.Items[comboStates.ItemIndex];
    dataModuleMain.queryStates.First;
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

procedure TformMain.PopulateSampleNamesGrid;
var
  row_num, cnt: integer;
begin
  row_num := 1;
  cnt := 0;
  gridResults.Clear;
  gridResults.RowCount := SampleNamesList.Count + 1;
  while (cnt < SampleNamesList.Count) do begin
		gridResults.Cells[0, row_num] := IntToStr(SampleNamesList[cnt].ID);
    gridResults.Cells[1, row_num] := SampleNamesList[cnt].FirstName;
		gridResults.Cells[2, row_num] := SampleNamesList[cnt].LastName;
		gridResults.Cells[3, row_num] := SampleNamesList[cnt].MI;
    gridResults.Cells[4, row_num] := SampleNamesList[cnt].Gender;
    gridResults.Cells[5, row_num] := SampleNamesList[cnt].Email;
    gridResults.Cells[6, row_num] := SampleNamesList[cnt].Address;
    gridResults.Cells[7, row_num] := SampleNamesList[cnt].City;
    gridResults.Cells[8, row_num] := SampleNamesList[cnt].StateCode;
    gridResults.Cells[9, row_num] := SampleNamesList[cnt].ZipCode;
    gridResults.Cells[10, row_num] := SampleNamesList[cnt].PhoneNumber;
    gridResults.Cells[11, row_num] := FormatDateTime('YYYY-MM-DD', SampleNamesList[cnt].BirthDate);

    row_num := row_num + 1;
    cnt := cnt + 1;
  end;
end;

end.

