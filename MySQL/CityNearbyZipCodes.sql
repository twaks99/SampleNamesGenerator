CREATE DEFINER=`root`@`localhost` PROCEDURE `CityNearbyZipCodes`(
	_CityName varchar(100),
    _StateCode char(2))
BEGIN
	DECLARE _CityLattitude float;
    DECLARE _CityLongitude float;
    DECLARE _Diff float;
    
    SET _Diff = 0.2;
    
    SELECT MAX(lattitude), MAX(longitude) INTO _CityLattitude, _CityLongitude
    FROM zipcodes
    WHERE city = _CityName AND state = _StateCode;
    
    SELECT id, zip_code, city, state, lattitude, longitude
    FROM zipcodes
    WHERE (lattitude > (_CityLattitude - _Diff) AND lattitude < (_CityLattitude + _Diff))
		AND (longitude < (_CityLongitude + _Diff) and longitude > (_CityLongitude - _Diff))
	ORDER BY state, city;
    
END