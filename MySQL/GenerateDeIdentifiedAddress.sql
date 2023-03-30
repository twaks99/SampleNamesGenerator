CREATE DEFINER=`root`@`localhost` PROCEDURE `GenerateDeIdentifiedAddress`(
	_Gender CHAR(1),
    _ZipCode CHAR(5),
    _BirthYear INT
)
BEGIN

DECLARE _AddressNumber VARCHAR(5);
DECLARE _StreetName VARCHAR(130);
DECLARE _CityName VARCHAR(100);
DECLARE _CountyName VARCHAR(100);
DECLARE _PhoneNumber VARCHAR(15);
DECLARE _FirstName VARCHAR(50);
DECLARE _LastName VARCHAR(50);
DECLARE _MI CHAR(1);
DECLARE _EMail VARCHAR(100);
DECLARE _StreetID int;
DECLARE _ZipID int;
DECLARE _AreaCodeID int;
DECLARE _BirthDate date;
DECLARE _MaxAreaCodeID INT;
DECLARE _StateCode CHAR(2);
DECLARE _AreaCode CHAR(3);

#Filter out area code records for the specified state.
DROP TABLE IF EXISTS AreaCodesInState;
CREATE TEMPORARY TABLE AreaCodesInState(
	ID int not null auto_increment primary key,
    Area_Code char(3) not null,
    State_Code char(2) not null);
    
INSERT INTO AreaCodesInState(Area_Code, State_Code)
SELECT AREA_CODE, STATE_CODE FROM areacodes 
WHERE state_code = (SELECT state FROM zipcodes WHERE zip_code = _ZipCode);
    
SELECT MAX(ID) INTO _MaxAreaCodeID FROM AreaCodesInState;

#Generate a sample name
CALL GenerateSampleName(_Gender, _FirstName, _LastName, _MI, _EMail);

#Generate Address Number
SET _AddressNumber = ROUND((RAND() * 9598), 0);
    
#Generate a street name
SET _StreetID = ROUND((RAND() * (SELECT max_id FROM maxtableid WHERE table_name = 'StreetNames')), 0);
SELECT CONCAT(mn.Street_Name, ' ', mn.Street_Type) INTO _StreetName
FROM streetnames AS mn
WHERE ID = _StreetID;

#Select a zip code and extract all information with it.
SELECT City, Zip_Code, State  INTO _CityName, _ZipCode, _StateCode
FROM zipcodes AS mn
WHERE zip_code = _ZipCode;
    
#Select an area code for the state.
SET _AreaCodeID = 0;
WHILE (_AreaCodeID = 0) DO
	SET _AreaCodeID = ROUND((RAND() * _MaxAreaCodeID), 0);
END WHILE;
SELECT Area_Code  INTO _AreaCode
FROM AreaCodesInState AS mn
WHERE ID = _AreaCodeID;

SET _PhoneNumber = CONCAT('(', _AreaCode, ') ', GenerateRandomNumber(3), '-', GenerateRandomNumber(4));

SET _BirthDate = GenerateSampleDate(_BirthYear, NULL, NULL);

SELECT
	_FirstName AS FirstName, 
	_LastName AS LastName, 
	_MI AS MI, 
	_Gender AS Gender,
	_EMail AS Email,
	CONCAT(_AddressNumber, ' ', _StreetName) AS Address,
	_CityName AS City,
	_StateCode AS State,
	_ZipCode AS Zip,
	_PhoneNumber AS Phone,
	_BirthDate AS BirthDate;

END