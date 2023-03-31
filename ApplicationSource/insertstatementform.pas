unit InsertStatementForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  Buttons, fgl, Generics.Collections, GenerateSampleName, Clipbrd, savedsettings;

const
  SourceFields: array[0..11] of String = ('ID','FirstName','LastName','MI',
    'Gender','Email','Address','City','StateCode','ZipCode',
    'PhoneNumber','BirthDate');
  NumFields: Integer = 12;

type

  TColumnMap = class(TObject)
    constructor Create(sourceColumn, DBColumn: String);
    public
      SourceColumnName: String;
      DBColumnName: String;
  end;

  TColumnMapList = specialize TObjectList<TColumnMap>;

  { TInsertStatementForm }

  TInsertStatementForm = class(TForm)
    btnCopyClipboard: TBitBtn;
    btnGenerate: TBitBtn;
    btnClose: TBitBtn;
    editTableName: TEdit;
    lblResults: TLabel;
    lblTableName: TLabel;
    lblGridColumns: TLabel;
    gridColumns: TStringGrid;
    memoSelectedNames: TMemo;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClipboardClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetNamesList(listNames: TSampleNamesList; settings: TSavedSettings);
    procedure PopulateColumnsGrid;
    procedure UpdateSavedSettings;
  private
    namesList: TSampleNamesList;
    columnMapList: TColumnMapList;
    savedSettings: TSavedSettings;
  public

  end;

var
  FormInsertStatement: TInsertStatementForm;

implementation

{$R *.lfm}

constructor TColumnMap.Create(sourceColumn, DBColumn: String);
begin
  self.SourceColumnName:= sourceColumn;
  self.DBColumnName:= DBColumn;
end;

{ TInsertStatementForm }

procedure TInsertStatementForm.FormCreate(Sender: TObject);
begin
  columnMapList := TColumnMapList.Create;
end;

procedure TInsertStatementForm.SetNamesList(listNames: TSampleNamesList; settings: TSavedSettings);
begin
  namesList:= listNames;
  savedSettings:= settings;
end;

procedure TInsertStatementForm.PopulateColumnsGrid;
var
  row_num, cnt, i: integer;
  useSavedSettings: Boolean;
  thisSavedMapping: TSavedColumnMapping;
begin
  row_num := 1;
  cnt := 0;
  gridColumns.Clear;
  gridColumns.RowCount:= NumFields + 1;
  useSavedSettings := (savedSettings.SavedColumnMappings.Count > 0);
  //Populate the table name from the saved settings.
  if (useSavedSettings) then
    editTableName.Text := savedSettings.DBTableName;
  //Populate the column mappings. Use the column names from the saved settings if defined.
  while cnt < NumFields do begin
    gridColumns.Cells[0, row_num] := SourceFields[cnt];
    if (useSavedSettings) then begin
      for i := 0 to savedSettings.SavedColumnMappings.Count -1 do begin
        thisSavedMapping := savedSettings.SavedColumnMappings[i];
        if (thisSavedMapping.SourceColumnName = SourceFields[cnt]) then begin
          gridColumns.Cells[1, row_num] := thisSavedMapping.DBColumnName;
          if (thisSavedMapping.UseMapping) then
            gridColumns.Cells[2, row_num] := '1'
          else
            gridColumns.Cells[2, row_num] := '0';
          Break;
        end;
      end;
    end
    else begin
      gridColumns.Cells[1, row_num] := SourceFields[cnt];
      gridColumns.Cells[2, row_num] := '1';
    end;
    Inc(row_num);
    Inc(cnt);
  end;
end;

procedure TInsertStatementForm.btnCloseClick(Sender: TObject);
begin
  self.Hide;
end;

procedure TInsertStatementForm.btnCopyClipboardClick(Sender: TObject);
var
  clipText, crlf : String;
  i : Integer;
begin
  clipText := String.Empty;
  crlf := Chr(13) + Chr(10);
  for i := 0 to memoSelectedNames.Lines.Count - 1 do begin
    clipText := clipText + memoSelectedNames.Lines[i] + crlf;
  end;
  Clipboard.AsText := clipText;
end;

procedure TInsertStatementForm.btnGenerateClick(Sender: TObject);
var
  i, j : Integer;
  tableName, insertClause, valuesClause, sourceColName: String;
  nameRecord : TNameMap;
begin
  tableName := editTableName.Text;
  if (tableName.Trim = String.Empty) then begin
    ShowMessage('A table name must be specified.');
  end
  else begin
    memoSelectedNames.Lines.Clear;
    for i := 1 to gridColumns.RowCount - 1 do begin
      if (gridColumns.Cells[2, i] = '1') then begin
        //memoSelectedNames.Lines.Add(gridColumns.Cells[1, i]);
        columnMapList.Add(TColumnMap.Create(gridColumns.Cells[0, i], gridColumns.Cells[1, i]));
      end;
    end;

    //Construct the header clause
    insertClause:= 'INSERT INTO ' + tableName + '(';
    for i := 0 to columnMapList.Count - 1 do begin
      if (i > 0) then begin
        insertClause:= insertClause + ', ';
      end;
      insertClause:= insertClause + columnMapList[i].DBColumnName;
    end;
    insertClause := insertClause + ') VALUES ';
    memoSelectedNames.Lines.Add(insertClause);

    //Construct the insert statements for each row.
    for i := 0 to namesList.Count - 1 do begin
      valuesClause:= '(';
      for j := 0 to columnMapList.Count - 1 do begin
        if (j > 0) then begin
          valuesClause := valuesClause + ', ';
        end;
        nameRecord := namesList[i].GenerateDictionary;
        sourceColName:= columnMapList[j].SourceColumnName;
        valuesClause := valuesClause + '''' + nameRecord[sourceColName] + '''';
      end;
      valuesClause := valuesClause + ')';
      if (i = namesList.Count - 1) then begin
        valuesClause := valuesClause + ';';
      end
      else begin
        valuesClause := valuesClause + ',';
      end;
      memoSelectedNames.Lines.Add(valuesClause);
      nameRecord.Free;
    end;
    UpdateSavedSettings;
  end;
end;

procedure TInsertStatementForm.UpdateSavedSettings;
var
  i : Integer;
  sourceColumn, dbColumn : String;
  useThisMap : Boolean;
begin
  savedSettings.DBTableName:= editTableName.Text;
  savedSettings.SavedColumnMappings.Clear;
  for i := 1 to gridColumns.RowCount - 1 do begin
    sourceColumn := gridColumns.Cells[0, i];
    dbColumn := gridColumns.Cells[1, i];
    useThisMap := (gridColumns.Cells[2, i] = '1');
    savedSettings.SavedColumnMappings.Add(TSavedColumnMapping.Create(sourceColumn, dbColumn, useThisMap));
  end;
end;

procedure TInsertStatementForm.FormActivate(Sender: TObject);
begin
  PopulateColumnsGrid;
end;




end.

