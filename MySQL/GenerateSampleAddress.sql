CREATE DEFINER=`root`@`localhost` PROCEDURE `GenerateSampleAddress`(
	_StateCode CHAR(2),
    _CityName VARCHAR(100),
    _Gender CHAR(1)
)
BEGIN

DECLARE _AddressNumber VARCHAR(5);
DECLARE _StreetName VARCHAR(130);
DECLARE _ZipCode CHAR(5);
DECLARE _MaxZipID INT;
DECLARE _MaxAreaCodeID INT;
DECLARE _AreaCode CHAR(3);
DECLARE _PhoneNumber VARCHAR(15);
DECLARE _FirstName VARCHAR(50);
DECLARE _LastName VARCHAR(50);
DECLARE _MI CHAR(1);
DECLARE _EMail VARCHAR(100);

#Generate a sample name
CALL GenerateSampleName(_Gender, _FirstName, _LastName, _MI, _EMail);

#Filter out zip code records for the specified state.
DROP TABLE IF EXISTS ZipsInState;
CREATE TEMPORARY TABLE ZipsInState(
	ID int not null auto_increment primary key,
    City varchar(100) not null,
    Zip_Code char(5) not null,
    State char(2) not null);
    
INSERT INTO ZipsInState(City, Zip_Code, State)
SELECT City, Zip_Code, State
FROM zipcodes
WHERE state = _StateCode AND city = _CityName;

SELECT MAX(ID) INTO _MaxZipID FROM ZipsInState;

#Filter out area code records for the specified state.
DROP TABLE IF EXISTS AreaCodesInState;
CREATE TEMPORARY TABLE AreaCodesInState(
	ID int not null auto_increment primary key,
    Area_Code char(3) not null,
    State_Code char(2) not null);
    
INSERT INTO AreaCodesInState(Area_Code, State_Code)
SELECT Area_Code, State_Code
FROM areacodes 
WHERE state_code = _StateCode;
    
SELECT MAX(ID) INTO _MaxAreaCodeID FROM AreaCodesInState;

#Generate Address Number
SET _AddressNumber = ROUND((RAND() * 9598), 0);

#Generate a street name
SELECT CONCAT(mn.Street_Name, ' ', mn.Street_Type) INTO _StreetName
FROM streetnames AS mn
	INNER JOIN (
		SELECT ROUND((RAND() * (SELECT max_id FROM maxtableid WHERE table_name = 'StreetNames')), 0) AS RandID
	) AS rid ON mn.ID = rid.RandID;

#Select a zip code and extract all information with it.
SELECT City, Zip_Code, State  INTO _CityName, _ZipCode, _StateCode
FROM ZipsInState AS mn
	INNER JOIN (
		SELECT ROUND((RAND() * _MaxZipID), 0) AS RandID
	) AS rid ON mn.ID = rid.RandID;
    
#Select an area code for the state.
SELECT Area_Code  INTO _AreaCode
FROM AreaCodesInState AS mn
	INNER JOIN (
		SELECT ROUND((RAND() * _MaxAreaCodeID), 0) AS RandID
	) AS rid ON mn.ID = rid.RandID;

SET _PhoneNumber = CONCAT('(', _AreaCode, ') ', GenerateRandomNumber(3), '-', GenerateRandomNumber(4));

SELECT
	_FirstName AS FirstName, 
    _LastName AS LastName, 
    _MI AS MI, 
    _EMail AS Email,
	CONCAT(_AddressNumber, ' ', _StreetName) AS Address,
    _CityName AS City,
    _StateCode AS State,
    _ZipCode AS Zip,
    _PhoneNumber AS Phone;

END