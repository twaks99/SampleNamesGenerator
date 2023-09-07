unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, Grids, GenerateSampleName, fpSpreadsheet, fpsTypes,
  Clipbrd, ExtCtrls, Buttons, Menus, Spin, fpsallformats, DetailForm,
  InsertStatementForm, savedsettings;

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
    procedure ExportToCSVFile(fileName: String);
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
  ExportWb: TsWorkbook;
  ExportWs: TsWorksheet;
  ExportFileName, ExportExt: String;
  colnum, rownum: Integer;
begin
	if dialogExport.Execute then begin
    //MessageDlg('Message', 'Filename: ' + savedlgExport.FileName, TMsgDlgType.mtInformation, [mbOK], '');
    if (dialogExport.FileName <> String.Empty) then begin
      ExportFileName := dialogExport.FileName;
      ExportExt:= ExtractFileExt(ExportFileName);
      if (ExportExt = '.csv') then begin
        ExportToCSVFile(ExportFileName);
      end
      else begin
        ExportWb := TsWorkbook.Create;
        ExportWs := ExportWb.AddWorksheet('Sheet 1');
        for colnum := 0 to gridResults.ColCount - 1 do begin
          ExportWs.WriteText(0, colnum, gridResults.Columns[colnum].Title.Caption);
          //HeaderCell := ExportWs.GetCell(0, colnum);
          ExportWs.WriteFontStyle(0, colnum, [fssBold]);
  	    end;

        //Set column widths.
        ExportWs.WriteColWidth(0, 0.4, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(1, 0.95, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(2, 1.0, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(3, 0.4, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(4, 0.6, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(5, 2.2, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(6, 2.1, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(7, 1.6, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(8, 0.55, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(9, 0.55, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(10, 1.05, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);
        ExportWs.WriteColWidth(11, 0.9, TsSizeUnits.suInches, TsColWidthtype.cwtCustom);

        rownum := 1;
  	    while (rownum <= SampleNamesList.Count) do begin
          ExportWs.WriteNumber(rownum, 0, SampleNamesList[rownum - 1].ID);
			    ExportWs.WriteText(rownum, 1, SampleNamesList[rownum - 1].FirstName);
          ExportWs.WriteText(rownum, 2, SampleNamesList[rownum - 1].LastName);
          ExportWs.WriteText(rownum, 3, SampleNamesList[rownum - 1].MI);
          ExportWs.WriteText(rownum, 4, SampleNamesList[rownum - 1].Gender);
          ExportWs.WriteText(rownum, 5, SampleNamesList[rownum - 1].Email);
          ExportWs.WriteText(rownum, 6, SampleNamesList[rownum - 1].Address);
          ExportWs.WriteText(rownum, 7, SampleNamesList[rownum - 1].City);
          ExportWs.WriteText(rownum, 8, SampleNamesList[rownum - 1].StateCode);
          ExportWs.WriteText(rownum, 9, SampleNamesList[rownum - 1].ZipCode);
          ExportWs.WriteText(rownum, 10, SampleNamesList[rownum - 1].PhoneNumber);
          ExportWs.WriteDateTime(rownum, 11, SampleNamesList[rownum - 1].BirthDate);

          ExportWs.WriteNumberFormat(rownum, 11, TsNumberFormat.nfShortDate, 'yyyy-mm-dd');
			    rownum:= rownum + 1;
        end;
        //Save to file
    	  if (ExportExt = '.xlsx') then begin
	    	  ExportWb.WriteToFile(ExportFileName, fpsTypes.sfOOXML, true);
	      end
  	    else begin  //Otherwise, write to open document spreadsheet format
    	    ExportWb.WriteToFile(ExportFileName, fpsTypes.sfOpenDocument, true);
	      end;
      end;
    end
    else begin
      MessageDlg('Message', 'A filename must be specified.', TMsgDlgType.mtInformation, [mbOK], '');
		end;
  end;

end;

procedure TformMain.ExportToCSVFile(fileName: String);
var
  colNum, rowNum: Integer;
  fileContents: TStringList;
  lineText: String;
begin
  try
    fileContents:= TStringList.Create;
    lineText:= String.Empty;
    for colNum := 0 to gridResults.Columns.Count - 1 do begin
      if (colNum > 0) then begin
        lineText:= lineText + ',';
      end;
      lineText:= lineText + '"' + gridResults.Columns[colNum].Title.Caption + '"';
    end;
    fileContents.Add(lineText);

    for rowNum:= 0 to SampleNamesList.Count - 1 do begin
      lineText:= String.Empty;
      lineText:= lineText + '"' + IntToStr(SampleNamesList[rowNum].ID);
      lineText:= lineText + '"' + SampleNamesList[rowNum].FirstName + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].LastName + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].MI + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].Gender + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].Email + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].Address + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].City + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].StateCode + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].ZipCode + '",';
      lineText:= lineText + '"' + SampleNamesList[rownum].PhoneNumber + '",';
      lineText:= lineText + '"' + FormatDateTime('YYYY-MM-DD', SampleNamesList[rownum].BirthDate) + '"';
      fileContents.Add(lineText);
    end;
    fileContents.SaveToFile(fileName);
  except
    on e: EInOutError do begin
      MessageDlg('Message', e.Message, TMsgDlgType.mtInformation, [mbOK], '');
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

