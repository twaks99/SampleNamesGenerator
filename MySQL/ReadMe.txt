This directory provides the data in MySQL for the generation of sample names. The SQL files are the code for each of the 
stored procedures that are used in this database. The primary entry point is the procedure GenerateSampleAddressList that
takes the following 3 parameters: 
	
	State Code (US states only)
	City Name (This must be within the specified state)
	Number of records to generate

A sample SQL statement to call this stored procedure to generate 50 sample names for Baltimore, MD is shown below.

	call GenerateSampleAddressList('MD', 'Baltimore', 50)