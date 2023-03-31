# MySQL Database for Sample Names
This directory provides the data in MySQL for the generation of sample names. The SQL files included in this directory contains the code for each of the 
stored procedures that are used in this database. 

There are two primary procedures that can be used to generate sample names right from this database using the included stored procedures.
1. The first one called *GenerateSampleAddress* generates a single row with a name and address for a specified state with the following parameters:  , city, and gender ("M" or "F"). Below is and example to generate a single row for a female in Baltimore, MD.
    - **StateCode:** Two-letter code for the state (US states only).
    - **CityName:** The name of the city within the specified state.
    - **Gender:** The desired gender of the fictional person ("M" or "F").

    A sample SQL statement to call this stored procedure for a female in Baltimore, MD is shown below.

    `call GenerateSampleAddress('MD', 'Baltimore', 'F')`

2. The second procedure is *GenerateSampleAddressList* that generates a list of sample names with addresses that requires the following parameters:
    - **StateCode:** Two-letter code for the state (US states only).
    - **City:** The name of the city within the specified state.
    - **NumRows:** Number of records to generate. 

    A sample SQL statement to call this stored procedure to generate 50 sample names for Baltimore, MD is shown below.

    `call GenerateSampleAddressList('MD', 'Baltimore', 50)`