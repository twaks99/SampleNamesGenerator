unit savedsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, Generics.Collections;

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
      StateName : String;
      CityName : String;
      NumRows : Integer;
      RandomGenderDistribution : Boolean;
      MalePercentage : Integer;
      FemalePercentage : Integer;
      DBTableName : String;
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
  StateName := String.Empty;
  CityName := String.Empty;
  NumRows := 0;
  RandomGenderDistribution := true;
  MalePercentage := 0;
  FemalePercentage := 0;
  SavedColumnMappings := TSavedColumnMappingList.Create;
	ReadSettingsFromFile;
end;

procedure TSavedSettings.ReadSettingsFromFile;
var
	doc : TXMLDocument;
  fldListNode, fldItemNode : TDOMNode;
  sourceColumn, dbColumn, useMapStr, randomDistStr : String;
  useThisMap : Boolean;
begin
  if (FileExists(SettingsFileName)) then begin
	  ReadXMLFile(doc, SettingsFileName);
	  StateName := doc.DocumentElement.FindNode('StateName').FirstChild.NodeValue;
    CityName := doc.DocumentElement.FindNode('CityName').FirstChild.NodeValue;
    NumRows := StrToInt(doc.DocumentElement.FindNode('NumRecords').FirstChild.NodeValue);
    DBTableName := doc.DocumentElement.FindNode('DBTableName').FirstChild.NodeValue;
    randomDistStr := doc.DocumentElement.FindNode('RandomGenderDistribution').FirstChild.NodeValue;
    if (randomDistStr = 'true') then
      RandomGenderDistribution := true
    else
      RandomGenderDistribution := false;
    MalePercentage := StrToInt(doc.DocumentElement.FindNode('MalePercentage').FirstChild.NodeValue);
    FemalePercentage := StrToInt(doc.DocumentElement.FindNode('FemalePercentage').FirstChild.NodeValue);
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
    doc.Free;
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
begin
  try
    doc := TXMLDocument.Create;
    rootNode := doc.CreateElement('SavedSettings');
    doc.AppendChild(rootNode);
    rootNode := doc.DocumentElement;
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

