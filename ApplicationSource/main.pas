unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, Grids, GenerateSampleName, fpSpreadsheet, fpsTypes,
  Clipbrd, ExtCtrls, Buttons, Menus, Spin, fpsallformats, DetailForm,
  InsertStatementForm, savedsettings, dataexport;

type

  { TformMain }

  TformMain = class(TForm)
    btnGenerate: TBitBtn;
    btnClose: TBitBtn;
    btnClipboard: TBitBtn;
    btnExport: TBitBtn;
    chkIncludeNearby: TCheckBox;
    chkRandomGDist: TCheckBox;
    comboCountry: TComboBox;
    comboStates: TComboBox;
    comboCities: TComboBox;
    connectionMain: TSQLite3Connection;
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
    queryStates: TSQLQuery;
    queryCities: TSQLQuery;
    gridResults: TStringGrid;
    transactionMain: TSQLTransaction;
    txtNumRows: TEdit;
    procedure btnClipboardClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
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
    SavedSettings: TSavedSettings;
    countryCode: String;
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
  countryCode := 'US';
  connectionMain.DatabaseName := 'SampleNames.db';
  connectionMain.Open;
  queryStates.Open;
  SavedSettings := TSavedSettings.Create;
  SamplesGenerator := TSampleNamesGenerator.Create(connectionMain);
  SelectSavedCountry;
  PopulateStatesCombo;
end;

procedure TformMain.SelectSavedCountry;
begin
  if (not String.IsNullOrEmpty(SavedSettings.CountryName)) then begin
    countryCode := SavedSettings.CountryName;
    if (SavedSettings.CountryName = 'US') then
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
    FormInsertStatement.SetNamesList(SampleNamesList, SavedSettings);
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
  PopulateCitiesCombo;
end;

procedure TformMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SavedSettings.SaveSettingsToFile;
end;

procedure TformMain.PopulateCitiesCombo;
var
  state_id: String;
  sql: TStringList;
begin
  state_id := GetStateCodeFromCombo();

  if (not String.IsNullOrEmpty(state_id)) then begin
    if (queryCities.Active) then begin
      queryCities.Close;
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
    queryCities.SQL := sql;
    queryCities.Params[0].Value := state_id;
    queryCities.Open;
    queryCities.First;
    comboCities.Items.Clear;
    while (not queryCities.EOF) do begin
      comboCities.Items.Add(queryCities.FieldByName('city').AsString);
      queryCities.Next;
    end;
  end;
end;

procedure TformMain.btnGenerateClick(Sender: TObject);
var
  state_code, city_name, row_count: String;
  num_rows, maleDist, femaleDist: Integer;
  randomGenderDist, includeNearbyCities : Boolean;
begin
  row_count:= Trim(txtNumRows.Text);
	if ((comboStates.ItemIndex >= 0) and (comboCities.ItemIndex >= 0) and (not (row_count.Equals(String.Empty)))) then begin
  	state_code := GetStateCodeFromCombo();
	  city_name := comboCities.Items[comboCities.ItemIndex];
  	num_rows := StrToInt(txtNumRows.Text);
    maleDist := spinMaleDist.Value;
    femaleDist := spinFemaleDist.Value;
    randomGenderDist := chkRandomGDist.Checked;
    includeNearbyCities := chkIncludeNearby.Checked;
	  SampleNamesList := SamplesGenerator.GenerateSampleSet(countryCode, state_code, city_name, num_rows,
      maleDist, femaleDist, randomGenderDist, includeNearbyCities);
		PopulateSampleNamesGrid;
    SavedSettings.CountryName:= countryCode;
    SavedSettings.StateName:= GetStateCodeFromCombo;
    SavedSettings.CityName := city_name;
    SavedSettings.NumRows := num_rows;
    SavedSettings.MalePercentage := maleDist;
    SavedSettings.FemalePercentage := femaleDist;
    SavedSettings.RandomGenderDistribution := randomGenderDist;
    SavedSettings.IncludeNearbyCities := chkIncludeNearby.Checked;
  end
	else begin
		MessageDlg('Message', 'State, City, and Number of rows are required fields.', TMsgDlgType.mtInformation, [mbOK], '');
  end;
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
    countryCode:= 'US'
  else
    countryCode := 'CA';
  queryStates.Params[0].Value := countryCode;
  queryStates.Close;
  queryStates.Open;
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
  QueryStates.First;
  while (not QueryStates.EOF) do begin
    comboStates.Items.Add(QueryStates.FieldByName('state_name').AsString);
    QueryStates.Next;
  end;
  if (SavedSettings.StateName <> String.Empty) then
	  SetSavedStateName;
end;

procedure TformMain.SetSavedStateName;
var
  savedStateCode, savedCityName, currentStateCode : String;
  idx : Integer;
begin
  savedStateCode:= SavedSettings.StateName;
  savedCityName:= SavedSettings.CityName;
  idx := 0;
  queryStates.First;
  while (not queryStates.EOF) do begin
    currentStateCode := queryStates.FieldByName('state_code').AsString;
    if (queryStates.FieldByName('state_code').AsString = savedStateCode) then begin
      comboStates.ItemIndex:= idx;
      Break;
    end;
    Inc(idx);
    queryStates.Next;
  end;
  //Populate combo box for cities.
  PopulateCitiesCombo;
  //Set city to saved value.
  if (queryCities.Active) then begin
    queryCities.First;
    idx := 0;
    while (not queryCities.EOF) do begin
      if (queryCities.FieldByName('city').AsString = savedCityName) then begin
        comboCities.ItemIndex:= idx;
        break;
      end;
      Inc(idx);
      queryCities.Next;
    end;
  end;
  //Set value for number of rows
  txtNumRows.Text := IntToStr(SavedSettings.NumRows);
  //Set Gender distribution parameters.
  chkRandomGDist.Checked := SavedSettings.RandomGenderDistribution;
  chkIncludeNearby.Checked := SavedSettings.IncludeNearbyCities;
  spinMaleDist.Value := SavedSettings.MalePercentage;
  spinFemaleDist.Value := SavedSettings.FemalePercentage;
end;

function TformMain.GetStateCodeFromCombo : String;
var
  state_name, state_code: String;
begin
  state_code := String.Empty;
  state_name := String.Empty;
  if (comboStates.ItemIndex >= 0) then begin
    state_name := comboStates.Items[comboStates.ItemIndex];
    queryStates.First;
    while (not queryStates.EOF) do begin
      if (queryStates.FieldByName('state_name').AsString = state_name) then begin
        state_code := queryStates.FieldByName('state_code').AsString;
        Break;
      end;
      queryStates.Next;
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

