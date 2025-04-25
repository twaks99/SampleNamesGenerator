unit savedsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, Generics.Collections, dataModule, CityRecords;

const
	SettingsFileName : String = 'savedsettings.xml';

type
  TSavedColumnMapping = Class(TObject)
    constructor Create(sourceCol, DBCol : String; useMap : Boolean);
    public
      SourceColumnName : String;
      DBColumnName : String;
      UseMapping : Boolean;
  end;

  TSavedColumnMappingList = specialize TObjectList<TSavedColumnMapping>;

  TSavedSettings = Class(TObject)
    constructor Create;
    public
      CountryName : String;
      StateName : String;
      CityName : String;
      NumRows : Integer;
      RandomGenderDistribution : Boolean;
      IncludeNearbyCities : Boolean;
      MalePercentage : Integer;
      FemalePercentage : Integer;
      DBTableName : String;
      MultipleCities : Boolean;
			SavedColumnMappings : TSavedColumnMappingList;
      procedure SaveSettingsToFile;
    private
      procedure ReadSettingsFromFile;
      function CreateXMLValueNode(doc: TXMLDocument; nodeName, nodeValue : String) : TDOMNode;
  end;

implementation

constructor TSavedColumnMapping.Create(sourceCol, DBCol : String; useMap : Boolean);
begin
  SourceColumnName := sourceCol;
  DBColumnName := DBCol;
  UseMapping := useMap;
end;

constructor TSavedSettings.Create;
begin
  CountryName := String.Empty;
  StateName := String.Empty;
  CityName := String.Empty;
  NumRows := 0;
  RandomGenderDistribution := true;
  IncludeNearbyCities := false;
  MultipleCities := false;
  MalePercentage := 0;
  FemalePercentage := 0;
  SavedColumnMappings := TSavedColumnMappingList.Create;
	ReadSettingsFromFile;
end;

procedure TSavedSettings.ReadSettingsFromFile;
var
	doc : TXMLDocument;
  fldListNode, fldItemNode : TDOMNode;
  sourceColumn, dbColumn, useMapStr, nodeValueStr : String;
  cName, stateCode, countryCode : String;
  useThisMap : Boolean;
  cityRecord : TCityRecord;
begin
  if (FileExists(SettingsFileName)) then begin
	  ReadXMLFile(doc, SettingsFileName);
    try
      if (doc.DocumentElement.FindNode('CountryName') <> nil) then
        CountryName := doc.DocumentElement.FindNode('CountryName').FirstChild.NodeValue
      else
        CountryName := String.Empty;
	    StateName := doc.DocumentElement.FindNode('StateName').FirstChild.NodeValue;
      CityName := doc.DocumentElement.FindNode('CityName').FirstChild.NodeValue;
      NumRows := StrToInt(doc.DocumentElement.FindNode('NumRecords').FirstChild.NodeValue);
      DBTableName := doc.DocumentElement.FindNode('DBTableName').FirstChild.NodeValue;
      //Random Gender Distribution
      if (doc.DocumentElement.FindNode('RandomGenderDistribution') <> nil) then begin
        nodeValueStr := doc.DocumentElement.FindNode('RandomGenderDistribution').FirstChild.NodeValue;
        RandomGenderDistribution := (nodeValueStr = 'true');
      end;
      if (doc.DocumentElement.FindNode('MalePercentage') <> nil) then
        MalePercentage := StrToInt(doc.DocumentElement.FindNode('MalePercentage').FirstChild.NodeValue);
      if (doc.DocumentElement.FindNode('FemalePercentage') <> nil) then
        FemalePercentage := StrToInt(doc.DocumentElement.FindNode('FemalePercentage').FirstChild.NodeValue);
      //Include Nearby Cities
      if (doc.DocumentElement.FindNode('IncludeNearbyCities') <> nil) then begin
        nodeValueStr := doc.DocumentElement.FindNode('IncludeNearbyCities').FirstChild.NodeValue;
        IncludeNearbyCities := (nodeValueStr = 'true');
      end;
      //Multiple Cities
      if (doc.DocumentElement.FindNode('MultipleCities') <> nil) then begin
        nodeValueStr := doc.DocumentElement.FindNode('MultipleCities').FirstChild.NodeValue;
        MultipleCities := (nodeValueStr = 'true')
      end
      else begin
        MultipleCities := false;
      end;
      //List of Cities
      if (doc.DocumentElement.FindNode('CitiesList') <> nil) then begin
        dataModule.dataModuleMain.MasterCitiesList.Clear;
        fldListNode := doc.DocumentElement.FindNode('CitiesList');
        fldItemNode := fldListNode.FirstChild;
        while Assigned(fldItemNode) do begin
          cName := fldItemNode.FindNode('CityName').FirstChild.NodeValue;
          stateCode := fldItemNode.FindNode('StateCode').FirstChild.NodeValue;
          countryCode := fldItemNode.FindNode('CountryCode').FirstChild.NodeValue;
          cityRecord := TCityRecord.Create(cName, stateCode, countryCode);
          dataModule.dataModuleMain.MasterCitiesList.Add(cityRecord);
          fldItemNode := fldItemNode.NextSibling;
        end;
      end;
      //Get Fields List.
      fldListNode := doc.DocumentElement.FindNode('FieldsList');
      fldItemNode := fldListNode.FirstChild;
      while Assigned(fldItemNode) do begin
		    sourceColumn := fldItemNode.FindNode('SourceColumn').FirstChild.NodeValue;
        dbColumn := fldItemNode.FindNode('DBColumn').FirstChild.NodeValue;
		    useMapStr := fldItemNode.FindNode('UseMapping').FirstChild.NodeValue;
        if (useMapStr = 'true') then
    	    useThisMap := true
        else
          useThisMap := false;
        SavedColumnMappings.Add(TSavedColumnMapping.Create(sourceColumn, dbColumn, useThisMap));
        fldItemNode := fldItemNode.NextSibling;
      end;
    finally
      doc.Free;
    end;
  end
  else begin
	  StateName := String.Empty;
    CityName := String.Empty;
    NumRows := 0;
    RandomGenderDistribution := true;
    MalePercentage := 0;
    FemalePercentage := 0;
  end;
end;

procedure TSavedSettings.SaveSettingsToFile;
var
	doc : TXMLDocument;
  rootNode, parentNode, txtValueNode, fldListNode, fldItemNode, itemNode : TDOMNode;
  i : Integer;
  useColStr : String;
  cityRecord : TCityRecord;
begin
  try
    doc := TXMLDocument.Create;
    rootNode := doc.CreateElement('SavedSettings');
    doc.AppendChild(rootNode);
    rootNode := doc.DocumentElement;
    //Country Node
    rootNode.appendChild(CreateXMLValueNode(doc, 'CountryName', CountryName));
    //State Node
    rootNode.AppendChild(CreateXMLValueNode(doc, 'StateName', StateName));
    //City Node
    rootNode.AppendChild(CreateXMLValueNode(doc, 'CityName', CityName));
    //Number of Records
    rootNode.AppendChild(CreateXMLValueNode(doc, 'NumRecords', IntToStr(NumRows)));
    //table name
    rootNode.AppendChild(CreateXMLValueNode(doc, 'DBTableName', DBTableName));
    //Random Gender Distribution
    if (RandomGenderDistribution) then
      useColStr := 'true'
    else
      useColStr := 'false';
    rootNode.AppendChild(CreateXMLValueNode(doc, 'RandomGenderDistribution', useColStr));
    //Male distribution percentage
    rootNode.AppendChild(CreateXMLValueNode(doc, 'MalePercentage', IntToStr(MalePercentage)));
    //Female distribution percentage
    rootNode.AppendChild(CreateXMLValueNode(doc, 'FemalePercentage', IntToStr(FemalePercentage)));
    //Include Nearby Cities in Search
    if (IncludeNearbyCities) then
      useColStr:= 'true'
    else
      useColStr := 'false';
    rootNode.AppendChild(CreateXMLValueNode(doc, 'IncludeNearbyCities', useColStr));
    //Multiple Cities
    if (MultipleCities) then
      useColStr := 'true'
    else
      useColStr := 'false';
    rootNode.AppendChild(CreateXMLValueNode(doc, 'MultipleCities', useColStr));
    //List of Cities
    if (dataModule.dataModuleMain.MasterCitiesList.Count > 0) then begin
      fldListNode := doc.CreateElement('CitiesList');
      rootNode.AppendChild(fldListNode);
      for i := 0 to dataModule.dataModuleMain.MasterCitiesList.Count - 1 do begin
        cityRecord:= dataModule.dataModuleMain.MasterCitiesList[i];
        fldItemNode := doc.CreateElement('CityRecord');
        fldItemNode.AppendChild(CreateXMLValueNode(doc, 'CityName', cityRecord.CityName));
        fldItemNode.AppendChild(CreateXMLValueNode(doc, 'StateCode', cityRecord.StateName));
        fldItemNode.AppendChild(CreateXMLValueNode(doc, 'CountryCode', cityRecord.CountryCode));
        fldListNode.AppendChild(fldItemNode);
      end;
    end;

    //column mapping list
    fldListNode := doc.CreateElement('FieldsList');
    rootNode.AppendChild(fldListNode);
    //save entry for each column mapping object.
    for i := 0 to self.SavedColumnMappings.Count - 1 do begin
      fldItemNode := doc.CreateElement('Field');
      fldItemNode.AppendChild(CreateXMLValueNode(doc, 'SourceColumn', SavedColumnMappings[i].SourceColumnName));
      fldItemNode.AppendChild(CreateXMLValueNode(doc, 'DBColumn', SavedColumnMappings[i].DBColumnName));

      if (SavedColumnMappings[i].UseMapping) then
        useColStr := 'true'
      else
        useColStr := 'false';
      fldItemNode.AppendChild(CreateXMLValueNode(doc, 'UseMapping', useColStr));
      fldListNode.AppendChild(fldItemNode);
    end;
    WriteXMLFile(doc, SettingsFileName);
  finally
    doc.Free;
  end;
end;

function TSavedSettings.CreateXMLValueNode(doc: TXMLDocument; nodeName, nodeValue : String) : TDOMNode;
var
  parentNode, txtValueNode : TDOMNode;
begin
  parentNode := doc.CreateElement(nodeName);
  txtValueNode := doc.CreateTextNode(nodeValue);
  parentNode.AppendChild(txtValueNode);
  Result := parentNode;
end;

end.

