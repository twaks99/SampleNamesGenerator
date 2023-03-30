CREATE DEFINER=`root`@`localhost` PROCEDURE `GenerateSampleAddressList`(
	_StateCode CHAR(2),
    _City VARCHAR(100),
    _NumRows INT
)
BEGIN

DECLARE _AddressNumber VARCHAR(5);
DECLARE _StreetName VARCHAR(130);
DECLARE _CityName VARCHAR(100);
DECLARE _ZipCode CHAR(5);
DECLARE _CountyName VARCHAR(100);
DECLARE _MaxZipID INT;
DECLARE _MaxAreaCodeID INT;
DECLARE _AreaCode CHAR(3);
DECLARE _PhoneNumber VARCHAR(15);
DECLARE _FirstName VARCHAR(50);
DECLARE _LastName VARCHAR(50);
DECLARE _MI CHAR(1);
DECLARE _EMail VARCHAR(100);
DECLARE _CityLattitude float;
DECLARE _CityLongitude float;
DECLARE _Diff float;
DECLARE _Counter int;
DECLARE _StreetID int;
DECLARE _ZipID int;
DECLARE _AreaCodeID int;
DECLARE _Gender char(1);
DECLARE _BirthDate date;
    
SET _Diff = 0.2;
SET _Counter = 0;

DROP TABLE IF EXISTS NamesList;
CREATE TEMPORARY TABLE NamesList (
	ID int not null auto_increment primary key,
    FirstName varchar(50) not null,
    LastName varchar(50) not null,
    MI char(1) null,
    Gender char(1) not null,
    EMail varchar(100) null,
    Address varchar(150) not null,
    City varchar(100) not null,
    StateCode char(2) not null,
    ZipCode char(5) null,
    PhoneNumber varchar(15) null,
    BirthDate date null);

#Filter out zip code records for the specified state.
DROP TABLE IF EXISTS NearbyZips;
CREATE TEMPORARY TABLE NearbyZips(
	ID int not null auto_increment primary key,
    City varchar(100) not null,
    Zip_Code char(5) not null,
    State char(2) not null);

IF (_City IS NULL) THEN
	INSERT INTO NearbyZips(City, Zip_Code, State)
	SELECT City, Zip_Code, State
	FROM zipcodes
	WHERE state = _StateCode;
ELSE
	SELECT MAX(lattitude), MAX(longitude) INTO _CityLattitude, _CityLongitude
    FROM zipcodes
    WHERE city = _City AND state = _StateCode;
    
    INSERT INTO NearbyZips(City, Zip_Code, State)
    SELECT city, zip_code, state
    FROM zipcodes
    WHERE (lattitude > (_CityLattitude - _Diff) AND lattitude < (_CityLattitude + _Diff))
		AND (longitude < (_CityLongitude + _Diff) and longitude > (_CityLongitude - _Diff))
	ORDER BY state, city;
END IF;

SELECT MAX(ID) INTO _MaxZipID FROM NearbyZips;

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

WHILE (_Counter < _NumRows) DO
	#Generate a sample name
	SET _Gender = CASE WHEN round((rand() * 1),0) = 0 THEN 'M' ELSE 'F' END; 
	CALL GenerateSampleName(_Gender, _FirstName, _LastName, _MI, _EMail);

	#Generate Address Number
	SET _AddressNumber = ROUND((RAND() * 9598), 0);
    
	#Generate a street name
    SET _StreetID = ROUND((RAND() * (SELECT max_id FROM maxtableid WHERE table_name = 'StreetNames')), 0);
	SELECT CONCAT(mn.Street_Name, ' ', mn.Street_Type) INTO _StreetName
	FROM streetnames AS mn
	WHERE ID = _StreetID;

	#Select a zip code and extract all information with it.
    SET _ZipID = COALESCE(ROUND((RAND() * _MaxZipID), 0), 1);
	SELECT City, Zip_Code, State  INTO _CityName, _ZipCode, _StateCode
	FROM NearbyZips AS mn
	WHERE ID = _ZipID;
    
	#Select an area code for the state.
    SET _AreaCodeID = 0;
    WHILE (_AreaCodeID = 0) DO
		SET _AreaCodeID = ROUND((RAND() * _MaxAreaCodeID), 0);
	END WHILE;
	SELECT Area_Code  INTO _AreaCode
	FROM AreaCodesInState AS mn
	WHERE ID = _AreaCodeID;

	SET _PhoneNumber = CONCAT('(', _AreaCode, ') ', GenerateRandomNumber(3), '-', GenerateRandomNumber(4));

	SET _BirthDate = GenerateSampleDate((YEAR(CURDATE()) - (20 + ROUND((RAND() * 50)))), NULL, NULL);


	INSERT INTO NamesList(FirstName, LastName, MI, Gender, EMail, Address, City, StateCode, 
		ZipCode, PhoneNumber, BirthDate)
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
        
	SET _Counter = _Counter + 1;
END WHILE;

SELECT * FROM NamesList;

END