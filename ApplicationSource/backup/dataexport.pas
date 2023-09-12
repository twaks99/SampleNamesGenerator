unit dataexport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, fpSpreadsheet, fpsTypes, fpsallformats,
  fpjson, jsonparser, GenerateSampleName;

  type
    TDataExport = Class(TObject)
      constructor Create;
    public
      ExportSuccessful : Boolean;
      StatusMsg : String;
      procedure ExportFile(namesList: TSampleNamesList; fileName : String);
    private
      SampleNames : TSampleNamesList;
      TitleList : TStringList;
      ExportFileName : String;
      ExportFormat : String;
      procedure DetermineFileType;
      procedure ExportToCSV;
      procedure ExportToSpreadsheet;
      procedure ExportToXML;
      procedure ExportToJSON;
      function CreateXMLValueNode(doc: TXMLDocument; nodeName, nodeValue : String) : TDOMNode;
      function CharToString(charValue: char) : String;
    end;

implementation

constructor TDataExport.Create;
begin
  ExportSuccessful := false;
  StatusMsg:= String.Empty;
  ExportFileName := String.Empty;
  ExportFormat:= 'spreadsheet';
  //Create the list of titles
  TitleList := TStringList.Create;
  TitleList.Add('Id');
  TitleList.Add('First Name');
  TitleList.Add('Last Name');
  TitleList.Add('MI');
  TitleList.Add('Gender');
  TitleList.Add('Email');
  TitleList.Add('Address');
  TitleList.Add('City');
  TitleList.Add('State');
  TitleList.Add('Zip');
  TitleList.Add('Phone');
  TitleList.Add('Birth Date');
end;

procedure TDataExport.ExportFile(namesList: TSampleNamesList; fileName: String);
begin
  SampleNames := namesList;
  ExportFileName := fileName;
  DetermineFileType;
  case ExportFormat of
    'excel' : ExportToSpreadsheet;
    'ods' : ExportToSpreadsheet;
    'csv' : ExportToCSV;
    'xml' : ExportToXML;
    'json' : ExportToJSON;
  end;
end;

procedure TDataExport.DetermineFileType;
var
  fileExt : String;
begin
  fileExt := ExtractFileExt(ExportFileName);
  case fileExt of
    '.xlsx' : ExportFormat := 'excel';
    '.ods' : ExportFormat := 'ods';
    '.csv' : ExportFormat := 'csv';
    '.xml' : ExportFormat := 'xml';
    '.json' : ExportFormat := 'json';
  end;
end;

procedure TDataExport.ExportToSpreadsheet;
var
  ExportWb: TsWorkbook;
  ExportWs: TsWorksheet;
  colnum, rownum: Integer;
begin
  try
    ExportWb := TsWorkbook.Create;
    ExportWs := ExportWb.AddWorksheet('Sheet 1');
    for colnum := 0 to TitleList.Count - 1 do begin
      ExportWs.WriteText(0, colnum, TitleList[colNum]);
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
    while (rownum <= SampleNames.Count) do begin
      ExportWs.WriteNumber(rownum, 0, SampleNames[rownum - 1].ID);
		  ExportWs.WriteText(rownum, 1, SampleNames[rownum - 1].FirstName);
      ExportWs.WriteText(rownum, 2, SampleNames[rownum - 1].LastName);
      ExportWs.WriteText(rownum, 3, SampleNames[rownum - 1].MI);
      ExportWs.WriteText(rownum, 4, SampleNames[rownum - 1].Gender);
      ExportWs.WriteText(rownum, 5, SampleNames[rownum - 1].Email);
      ExportWs.WriteText(rownum, 6, SampleNames[rownum - 1].Address);
      ExportWs.WriteText(rownum, 7, SampleNames[rownum - 1].City);
      ExportWs.WriteText(rownum, 8, SampleNames[rownum - 1].StateCode);
      ExportWs.WriteText(rownum, 9, SampleNames[rownum - 1].ZipCode);
      ExportWs.WriteText(rownum, 10, SampleNames[rownum - 1].PhoneNumber);
      ExportWs.WriteDateTime(rownum, 11, SampleNames[rownum - 1].BirthDate);

      ExportWs.WriteNumberFormat(rownum, 11, TsNumberFormat.nfShortDate, 'yyyy-mm-dd');
		  rownum:= rownum + 1;
    end;
    //Save to file
    if (ExportFormat = 'excel') then begin
	    ExportWb.WriteToFile(ExportFileName, fpsTypes.sfOOXML, true);
	  end
    else begin  //Otherwise, write to open document spreadsheet format
      ExportWb.WriteToFile(ExportFileName, fpsTypes.sfOpenDocument, true);
	  end;
    ExportSuccessful := true;;
  except
    ExportSuccessful := false;
    StatusMsg := 'Export to spreadsheet failed.';
  end;
end;

procedure TDataExport.ExportToCSV;
var
  colNum, rowNum: Integer;
  fileContents: TStringList;
  lineText: String;
begin
  try
    fileContents:= TStringList.Create;
    lineText:= String.Empty;
    for colNum := 0 to TitleList.Count - 1 do begin
      if (colNum > 0) then begin
        lineText:= lineText + ',';
      end;
      lineText:= lineText + '"' + TitleList[colNum] + '"';
    end;
    fileContents.Add(lineText);

    for rowNum:= 0 to SampleNames.Count - 1 do begin
      lineText:= String.Empty;
      lineText:= lineText + '"' + IntToStr(SampleNames[rowNum].ID);
      lineText:= lineText + '"' + SampleNames[rowNum].FirstName + '",';
      lineText:= lineText + '"' + SampleNames[rownum].LastName + '",';
      lineText:= lineText + '"' + SampleNames[rownum].MI + '",';
      lineText:= lineText + '"' + SampleNames[rownum].Gender + '",';
      lineText:= lineText + '"' + SampleNames[rownum].Email + '",';
      lineText:= lineText + '"' + SampleNames[rownum].Address + '",';
      lineText:= lineText + '"' + SampleNames[rownum].City + '",';
      lineText:= lineText + '"' + SampleNames[rownum].StateCode + '",';
      lineText:= lineText + '"' + SampleNames[rownum].ZipCode + '",';
      lineText:= lineText + '"' + SampleNames[rownum].PhoneNumber + '",';
      lineText:= lineText + '"' + FormatDateTime('YYYY-MM-DD', SampleNames[rownum].BirthDate) + '"';
      fileContents.Add(lineText);
    end;
    fileContents.SaveToFile(ExportFileName);
    ExportSuccessful := true;
  except
    on e: EInOutError do begin
      StatusMsg := e.Message;
      ExportSuccessful := false;
    end;
    on e: Exception do begin
      StatusMsg := e.Message;
      ExportSuccessful := false;
    end;
  end;

end;

procedure TDataExport.ExportToXML;
var
  doc : TXMLDocument;
  rootNode, elementNode : TDOMNode;
  rowNum : Integer;
  nameRecord : TSampleName;
begin
  try
    doc := TXMLDocument.Create;
    rootNode := doc.CreateElement('NamesList');
    doc.AppendChild(rootNode);
    rootNode := doc.DocumentElement;

    for rowNum := 0 to SampleNames.Count - 1 do begin
      nameRecord := SampleNames[rowNum];
      elementNode := doc.CreateElement('NameRecord');
      elementNode.AppendChild(CreateXMLValueNode(doc, 'ID', nameRecord.ID.ToString));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'FirstName', nameRecord.FirstName));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'LastName', nameRecord.LastName));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'MI', CharToString(nameRecord.MI)));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'Gender', CharToString(nameRecord.Gender)));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'Email', nameRecord.Email));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'Address', nameRecord.Address));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'City', nameRecord.City));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'State', nameRecord.StateCode));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'ZipCode', nameRecord.ZipCode));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'Phone', nameRecord.PhoneNumber));
      elementNode.AppendChild(CreateXMLValueNode(doc, 'BirthDate', FormatDateTime('YYYY-MM-DD', nameRecord.BirthDate)));
      rootNode.AppendChild(elementNode);
      //elementNode.Free;
    end;
    WriteXMLFile(doc, ExportFileName);
    ExportSuccessful := true;
  except
    on E: Exception do begin
      ExportSuccessful := false;
      StatusMsg:= 'Export to XML failed';
    end;
  end;
end;

procedure TDataExport.ExportToJSON;
var
  jsonStream: TFPJSStream;
  jData: TJSONData;
  namesList: TJSONArray;
  elementNode: TJSONObject;
  nameRecord: TSampleName;
  rowNum: Integer;
begin
  try
    jsonStream := TFPJSStream.Create;
    namesList := TJSONArray.Create;
    for rowNum := 0 to SampleNames.Count - 1 do begin
      nameRecord := SampleNames[rowNum];
      elementNode := TJSONObject.Create;
      elementNode.Add('ID', nameRecord.ID);
      elementNode.Add('FirstName', nameRecord.FirstName);
      elementNode.Add('LastName', nameRecord.LastName);
      elementNode.Add('MI', CharToString(nameRecord.MI));
      elementNode.Add('Gender', CharToString(nameRecord.Gender));
      elementNode.Add('Email', nameRecord.Email);
      elementNode.Add('Address', nameRecord.Address);
      elementNode.Add('City', nameRecord.City);
      elementNode.Add('State', nameRecord.StateCode);
      elementNode.Add('ZipCode', nameRecord.ZipCode);
      elementNode.Add('Phone', nameRecord.PhoneNumber);
      elementNode.Add('BirthDate', FormatDateTime('YYYY-MM-DD', nameRecord.BirthDate));
      namesList.Add(elementNode);
      //elementNode.Free;
    end;
    jData := namesList;
    jData.DumpJSON(jsonStream);
    jsonStream.SaveToFile(ExportFileName);
    ExportSuccessful := true;
  except
    on E: Exception do begin
      ExportSuccessful := false;
      StatusMsg := 'Export to JSON failed.';
    end;
  end;
end;

function TDataExport.CreateXMLValueNode(doc: TXMLDocument; nodeName, nodeValue : String) : TDOMNode;
var
  parentNode, txtValueNode : TDOMNode;
begin
  parentNode := doc.CreateElement(nodeName);
  txtValueNode := doc.CreateTextNode(nodeValue);
  parentNode.AppendChild(txtValueNode);
  Result := parentNode;
end;

function TDataExport.CharToString(charValue: char) : String;
begin
  Result := '';
  Result := Result + charValue;
end;

end.

