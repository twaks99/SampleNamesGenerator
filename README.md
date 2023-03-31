# Sample Names Generator
This is a project of mine to easily generate sample names for the purpose of populating test data or for de-identifying personal information. The database contains a stores of common male and female names (for the purpose of this application, there are only two genders), last names, US zip codes and area codes, common street names, and email domains. These can be used to build sample names with addresses, phone numbers, email addresses, and birth dates.

There are three parts to this repository.
1. **MySQL Database:** This folder holds the full database backup as well as all of the stored procedures to be used in a MySQL database. This database can be used a stand-alone tool for generating the sample data using the stored procedures.
2. **SQLite Database:** This folder holds a self-contained file of all of the data needed to generate the sample names. This holds all of the same data as the MySQL database but in SQLite format. This engine does not support stored procedures, so it is up to the application developer to implement the logic for the random generation of names and addresses.
3. **Application Source:** This folder contains the source code for a desktop application to generate random names and addresses from the SQLite database. This application has been written in Lazarus/FreePascal. This has been tested on Windows and Linux.
