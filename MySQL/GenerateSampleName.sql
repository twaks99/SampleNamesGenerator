CREATE DEFINER=`root`@`localhost` PROCEDURE `GenerateSampleName`(
	_Gender char(1),
    OUT _GenFirstName varchar(50),
    OUT _GenLastName varchar(50),
    OUT _GenMI char(1),
    OUT _GenEmail varchar(100)
)
BEGIN
	DECLARE _LastNameID int;
  DECLARE _FirstNameID int;
  DECLARE _EmailID int;
  DECLARE _MiID int;
    
    SET _FirstNameID = 0;
	IF (_Gender = 'M') THEN
		WHILE (_FirstNameID = 0) DO
			SET _FirstNameID = ROUND((RAND() * (SELECT max_id FROM maxtableid WHERE table_name = 'MaleFirstNames')), 0);
		END WHILE;
        SELECT mn.first_name INTO _GenFirstName
        FROM malefirstnames AS mn
		WHERE ID = _FirstNameID;
    END IF;
    IF (_Gender = 'F') THEN
		WHILE (_FirstNameID = 0) DO
			SET _FirstNameID = ROUND((RAND() * (SELECT max_id FROM maxtableid WHERE table_name = 'FemaleFirstNames')), 0);
		END WHILE;
		SELECT mn.first_name INTO _GenFirstName
        FROM femalefirstnames AS mn
		WHERE ID = _FirstNameID;
    END IF;
    
    SET _LastNameID = 0;
    WHILE (_LastNameID = 0) DO
		SET _LastNameID = ROUND((RAND() * (SELECT max_id FROM maxtableid WHERE table_name = 'LastNames')), 0);
    END WHILE;
    SELECT mn.last_name INTO _GenLastName
	FROM lastnames AS mn
	WHERE ID = _LastNameID;
	
    SET _MiID = 0;
    WHILE (_MiID = 0) DO
		SET _MiID = ROUND((RAND() * (SELECT max_id FROM maxtableid WHERE table_name = 'CommonLetters')), 0);
	END WHILE;
	SELECT mn.Letter INTO _GenMI
	FROM commonletters AS mn
	WHERE ID = _MiID;
	
    SET _EmailID = 0;
    WHILE (_EmailID = 0) DO
		SET _EmailID = ROUND((RAND() * (SELECT max_id FROM maxtableid WHERE table_name = 'EmailDomains')), 0);
	END WHILE;
	SELECT CONCAT(LOWER(LEFT(_GenFirstName, 1)), LOWER( _GenLastName), CAST(ROUND(RAND() * 9999) AS CHAR), '@', mn.Email_Domain) 
    INTO _GenEmail
	FROM emaildomains AS mn
	WHERE ID = _EmailID;
        
END