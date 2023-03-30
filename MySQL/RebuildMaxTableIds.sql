CREATE DEFINER=`root`@`localhost` PROCEDURE `RebuildMaxTableIds`()
BEGIN

DELETE FROM maxtableid;

INSERT INTO maxtableid(table_name, max_id) SELECT 'AreaCodes', MAX(id) FROM areacodes;
INSERT INTO maxtableid(table_name, max_id) SELECT 'CommonLetters', MAX(id) FROM commonletters;
INSERT INTO maxtableid(table_name, max_id) SELECT 'EmailDomains', MAX(id) FROM emaildomains;
INSERT INTO maxtableid(table_name, max_id) SELECT 'FemaleFirstNames', MAX(id) FROM femalefirstnames;
INSERT INTO maxtableid(table_name, max_id) SELECT 'MaleFirstNames', MAX(id) FROM malefirstnames;
INSERT INTO maxtableid(table_name, max_id) SELECT 'LastNames', MAX(id) FROM lastnames;
INSERT INTO maxtableid(table_name, max_id) SELECT 'States', MAX(id) FROM states;
INSERT INTO maxtableid(table_name, max_id) SELECT 'StreetNames', MAX(id) FROM streetnames;
INSERT INTO maxtableid(table_name, max_id) SELECT 'ZipCodes', MAX(id) FROM zipcodes;


END