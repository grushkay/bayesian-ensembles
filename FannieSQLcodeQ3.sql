-- ---------------------------------------
-- Initial steps and performance settings.
-- ---------------------------------------
-- Step 1. Timeouts

-- Change your time out settings.
-- From the Workbench menu on a Windows machine:
-- 	 Edit > Preferences > SQL Editor > MySQL Session >
--   DBMS connection keep-alive interval (in seconds) > 6000
--   DBMS connection read time out (in seconds) > 6000
-- On a Mac, these items are under Workbench's Preferences.

-- Step 2. Storage Engine

-- Speed up the loading and joining of tables.
-- Change your storage engine from InnoDB to MyISAM.
-- Restart your computer, right click on the MySQL Workbench icon > Run as Administrator.
-- From the Workbench menu on a Windows machine,
--   Server > Options File >
--     default-storage-engine > MyISAM
--     default_tmp_storage_engine > MyISAM
-- Note: The default data storage engine in MySQL is InnoDB, which is robust for transactional systems, 
-- such as databases for e-commerce sites. For reporting databases, MyISAM performs much faster, 
-- since it gives up transactional integrity that we do not need here.
-- Run the following two lines to also set the innodb engine.

set global innodb_buffer_pool_size=268435456;
SELECT @@innodb_buffer_pool_size;

-- Step 3. Writing out csvs

-- Run the safe updates line below, which allows you to write out a csv file. 
-- Then close the file and close down MySQL. You need to reconnect for this setting to take effect.
SET SQL_SAFE_UPDATES = 0;
-- The only directory that you have privilege to write to is 
-- C:/ProgramData/MySQL/MySQL Server 5.7/Uploads/ for Windows and /private/tmp/ for Mac. 
-- To check that this place is the privileged place, run the following line: 
SHOW VARIABLES LIKE "secure_file_priv";
-- Look into this folder to find any csv files you write out from MySQL.


-- ---------------------------
-- Create the fannie database. 
-- ---------------------------

-- Do this only once.
CREATE DATABASE fannie_db;
-- See what database you have on your server.
SHOW DATABASES;
-- Once it's there, use the fannie database.
USE fannie_db;


-- -------------------------------
-- Create a table for acquistions.
-- -------------------------------

-- The column names come from the file https://loanperformancedata.fanniemae.com/lppub-docs/lppub_file_layout.pdf. 
-- We use underscores for spaces in names.
-- NOTE: In NUMERIC(14,10), 14 is the total maximum number of digits, with up to 10 to the right of the decimal point. 

-- Drop the table if you want to redefine  it.
DROP TABLE IF EXISTS acquisitions;

CREATE TABLE acquisitions
(
LOAN_IDENTIFIER VARCHAR(20),
CHANNEL VARCHAR(1),
SELLER_NAME VARCHAR(200),
ORIGINAL_INTEREST_RATE NUMERIC(14,10),
ORIGINAL_UNPAID_PRINCIPAL_BALANCE NUMERIC(11,2),
ORIGINAL_LOAN_TERM NUMERIC(3,0),
ORIGINATION_DATE VARCHAR(7),
FIRST_PAYMENT_DATE VARCHAR(7),
ORIGINAL_LOAN_TO_VALUE NUMERIC(14,10),
ORIGINAL_COMBINED_LOAN_TO_VALUE NUMERIC(14,10),
NUMBER_OF_BORROWERS NUMERIC(3,0),
DEBT_TO_INCOME_RATIO NUMERIC(14,10),
BORROWER_CREDIT_SCORE NUMERIC(3,0),
FIRST_TIME_HOME_BUYER_INDICATOR VARCHAR(1),
LOAN_PURPOSE VARCHAR(1),
PROPERTY_TYPE VARCHAR(2),
NUMBER_OF_UNITS VARCHAR(10),
OCCUPANCY_STATUS VARCHAR(1),
PROPERTY_STATE VARCHAR(20),
ZIP_3_DIGIT VARCHAR(10),
MORTGAGE_INSURANCE_PERCENTAGE NUMERIC(14,10),
PRODUCT_TYPE VARCHAR(20),
CO_BORROWER_CREDIT_SCORE NUMERIC(3,0),
MORTGAGE_INSURANCE_TYPE NUMERIC(1,0) 
);

-- Load the data from the text file Acquisition_2008Q2.txt.
-- NOTE: make sure you use forward slashes in the file path!
LOAD DATA LOCAL INFILE 'C:/Users/grushkay/Dropbox/Pooling Probabilities for Classification/Empirical Analysis/Files for submission/Fannie/Acquisition_2007Q3.txt' 
INTO TABLE acquisitions
COLUMNS TERMINATED BY '|' 
LINES TERMINATED BY '\n'
(LOAN_IDENTIFIER, 
CHANNEL, 
SELLER_NAME, 
@vORIGINAL_INTEREST_RATE, 
@vORIGINAL_UNPAID_PRINCIPAL_BALANCE, 
@vORIGINAL_LOAN_TERM, 
ORIGINATION_DATE, 
FIRST_PAYMENT_DATE, 
@vORIGINAL_LOAN_TO_VALUE, 
@vORIGINAL_COMBINED_LOAN_TO_VALUE, 
@vNUMBER_OF_BORROWERS, 
@vDEBT_TO_INCOME_RATIO,
@vBORROWER_CREDIT_SCORE, 
FIRST_TIME_HOME_BUYER_INDICATOR, 
LOAN_PURPOSE, 
PROPERTY_TYPE, 
NUMBER_OF_UNITS, 
OCCUPANCY_STATUS,
PROPERTY_STATE, 
ZIP_3_DIGIT, 
@vMORTGAGE_INSURANCE_PERCENTAGE, 
PRODUCT_TYPE, 
@vCO_BORROWER_CREDIT_SCORE,
@vMORTGAGE_INSURANCE_TYPE
)
SET
ORIGINAL_INTEREST_RATE = nullif(@vORIGINAL_INTEREST_RATE,''),
ORIGINAL_UNPAID_PRINCIPAL_BALANCE = nullif(@vORIGINAL_UNPAID_PRINCIPAL_BALANCE,''),
ORIGINAL_LOAN_TERM = nullif(@vORIGINAL_LOAN_TERM,''),
ORIGINAL_LOAN_TO_VALUE = nullif(@vORIGINAL_LOAN_TO_VALUE,''),
ORIGINAL_COMBINED_LOAN_TO_VALUE = nullif(@vORIGINAL_COMBINED_LOAN_TO_VALUE,''),
NUMBER_OF_BORROWERS = nullif(@vNUMBER_OF_BORROWERS,''),
DEBT_TO_INCOME_RATIO = nullif(@vDEBT_TO_INCOME_RATIO,''),
BORROWER_CREDIT_SCORE = nullif(@vBORROWER_CREDIT_SCORE,''),
MORTGAGE_INSURANCE_PERCENTAGE = nullif(@vMORTGAGE_INSURANCE_PERCENTAGE,''),
CO_BORROWER_CREDIT_SCORE = nullif(@vCO_BORROWER_CREDIT_SCORE,''),
MORTGAGE_INSURANCE_TYPE = nullif(@vMORTGAGE_INSURANCE_TYPE,'');

-- Ignore any warnings you get.
-- NOTE: the @v and SET lines above are there to replace the empty cells with NULL.

-- Create LOAN IDENTIFIER as a unique index here. By not defining it when you create the table, you save time in loading the data.
-- In general, having an index helps speed up your joins.  Making it unique is for data integrity so that you will be prevented from 
-- inadvertantly loading duplicate data.
CREATE UNIQUE INDEX uidx_acquisitions ON acquisitions(LOAN_IDENTIFIER);

-- Count the number of rows.
SELECT COUNT(*) FROM acquisitions;

-- Count the number of columns.
SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
	WHERE TABLE_SCHEMA = 'fannie_db'
	AND TABLE_NAME = 'acquisitions';

-- Look at first 10 rows.
SELECT * FROM acquisitions LIMIT 100;


-- -------------------------------
-- Create a table for performance.
-- -------------------------------

DROP TABLE IF EXISTS performance;

CREATE TABLE performance
(LOAN_IDENTIFIER VARCHAR(20),
MONTHLY_REPORTING_PERIOD VARCHAR(10), 
SERVICER_NAME VARCHAR(80), 
CURRENT_INTEREST_RATE NUMERIC(14,10),
CURRENT_ACTUAL_UNPAID_PRINCIPAL_BALANCE NUMERIC(11,2),
LOAN_AGE NUMERIC(10,0),
REMAINING_MONTHS_TO_LEGAL_MATURITY NUMERIC(3,0),
ADJUSTED_REMAINING_MONTHS_TO_MATURITY NUMERIC(3,0),
MATURITY_DATE VARCHAR(7),
METROPOLITAN_STATISTICAL_AREA VARCHAR(5),
CURRENT_LOAN_DELINQUENCY_STATUS VARCHAR(5),
MODIFICATION_FLAG VARCHAR(1),
ZERO_BALANCE_CODE VARCHAR(2),
ZERO_BALANCE_EFFECTIVE_DATE VARCHAR(7),
LAST_PAID_INSTALLMENT_DATE VARCHAR(10),
FORECLOSURE_DATE VARCHAR(10),
DISPOSITION_DATE VARCHAR(10),
FORECLOSURE_COSTS NUMERIC(27,12),
PROPERTY_PRESERVATION_AND_REPAIR_COSTS NUMERIC(27,12),
ASSET_REVOVERY_COSTS NUMERIC(27,12),
MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS NUMERIC(27,12),
ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY NUMERIC(27,12),
NET_SALES_PROCEEDS NUMERIC(27,12),
CREDIT_ENHANCEMENT_PROCEEDS NUMERIC(27,12),
REPURCHASE_MAKE_WHOLE_PROCEEDS NUMERIC(27,12),
OTHER_FORECLOSURE_PROCEEDS NUMERIC(27,12),
NON_INTEREST_BEARING_UPB NUMERIC(11,2),
PRINCIPAL_FORGIVENESS_UPB NUMERIC(11,2),
REPURCHASE_MAKE_WHOLE_PROCEEDS_FLAG VARCHAR(1)
);

-- Load the data from the text file Performance_2008Q2.txt.
LOAD DATA LOCAL INFILE 'C:/Users/grushkay/Dropbox/Pooling Probabilities for Classification/Empirical Analysis/Files for submission/Fannie/Performance_2007Q3.txt' 
INTO TABLE performance
COLUMNS TERMINATED BY '|' 
LINES TERMINATED BY '\n'
(LOAN_IDENTIFIER,
MONTHLY_REPORTING_PERIOD, 
SERVICER_NAME, 
@vCURRENT_INTEREST_RATE,
@vCURRENT_ACTUAL_UNPAID_PRINCIPAL_BALANCE,
@vLOAN_AGE,
@vREMAINING_MONTHS_TO_LEGAL_MATURITY,
@vADJUSTED_REMAINING_MONTHS_TO_MATURITY,
MATURITY_DATE,
METROPOLITAN_STATISTICAL_AREA,
CURRENT_LOAN_DELINQUENCY_STATUS,
MODIFICATION_FLAG,
ZERO_BALANCE_CODE,
ZERO_BALANCE_EFFECTIVE_DATE,
LAST_PAID_INSTALLMENT_DATE,
FORECLOSURE_DATE,
DISPOSITION_DATE,
@vFORECLOSURE_COSTS,
@vPROPERTY_PRESERVATION_AND_REPAIR_COSTS,
@vASSET_REVOVERY_COSTS,
@vMISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS,
@vASSOCIATED_TAXES_FOR_HOLDING_PROPERTY,
@vNET_SALES_PROCEEDS,
@vCREDIT_ENHANCEMENT_PROCEEDS,
@vREPURCHASE_MAKE_WHOLE_PROCEEDS,
@vOTHER_FORECLOSURE_PROCEEDS,
@vNON_INTEREST_BEARING_UPB,
@vPRINCIPAL_FORGIVENESS_UPB,
@vREPURCHASE_MAKE_WHOLE_PROCEEDS_FLAG
)
SET
CURRENT_INTEREST_RATE = nullif(@vCURRENT_INTEREST_RATE,''),
CURRENT_ACTUAL_UNPAID_PRINCIPAL_BALANCE = nullif(@vCURRENT_ACTUAL_UNPAID_PRINCIPAL_BALANCE,''),
LOAN_AGE = nullif(@vLOAN_AGE,''),
REMAINING_MONTHS_TO_LEGAL_MATURITY = nullif(@vREMAINING_MONTHS_TO_LEGAL_MATURITY,''),
ADJUSTED_REMAINING_MONTHS_TO_MATURITY = nullif(@vADJUSTED_REMAINING_MONTHS_TO_MATURITY,''),
FORECLOSURE_COSTS = nullif(@vFORECLOSURE_COSTS,''),
PROPERTY_PRESERVATION_AND_REPAIR_COSTS = nullif(@vPROPERTY_PRESERVATION_AND_REPAIR_COSTS,''),
ASSET_REVOVERY_COSTS = nullif(@vASSET_REVOVERY_COSTS,''),
MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS = nullif(@vMISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS,''),
ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY = nullif(@vASSOCIATED_TAXES_FOR_HOLDING_PROPERTY,''),
NET_SALES_PROCEEDS = nullif(@vNET_SALES_PROCEEDS,''),
CREDIT_ENHANCEMENT_PROCEEDS = nullif(@vCREDIT_ENHANCEMENT_PROCEEDS,''),
REPURCHASE_MAKE_WHOLE_PROCEEDS = nullif(@vREPURCHASE_MAKE_WHOLE_PROCEEDS,''),
OTHER_FORECLOSURE_PROCEEDS = nullif(@vOTHER_FORECLOSURE_PROCEEDS,''),
NON_INTEREST_BEARING_UPB = nullif(@vNON_INTEREST_BEARING_UPB,''),
PRINCIPAL_FORGIVENESS_UPB = nullif(@vPRINCIPAL_FORGIVENESS_UPB,''),
REPURCHASE_MAKE_WHOLE_PROCEEDS_FLAG = nullif(@vREPURCHASE_MAKE_WHOLE_PROCEEDS_FLAG,'');

-- Create a unique index on the combination of LOAN IDENTIFIER and LOAN_AGE. This creation will take several minutes.
CREATE UNIQUE INDEX uidx_performance ON performance(LOAN_IDENTIFIER, LOAN_AGE);

SELECT COUNT(*) FROM performance;
SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
	WHERE TABLE_SCHEMA = 'fannie_db'
	AND TABLE_NAME = 'performance';
SELECT * FROM performance LIMIT 1000;


-- ----------------------------
-- Create a table for loan age.
-- ----------------------------

DROP TABLE IF EXISTS loan_age;

-- Create a new table of LOAN IDENTIFIER and the loan age in the last row for each borrower.
-- The GROUP BY line will return one row for each LOAN_IDENTIFIER.

CREATE TABLE loan_age
(
LOAN_IDENTIFIER VARCHAR(20),
LAST_LOAN_AGE NUMERIC(10,0)
);

INSERT INTO loan_age
SELECT 
LOAN_IDENTIFIER,
MAX(LOAN_AGE) AS LAST_LOAN_AGE
FROM 
performance
GROUP BY
LOAN_IDENTIFIER;

CREATE UNIQUE INDEX uidx_loan_age ON loan_age(LOAN_IDENTIFIER, LAST_LOAN_AGE);

SELECT COUNT(*) FROM loan_age;
SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
	WHERE TABLE_SCHEMA = 'fannie_db'
	AND TABLE_NAME = 'loan_age';
SELECT * FROM loan_age LIMIT 10;


-- ---------------------------------------
-- Create a table for performance summary.
-- ---------------------------------------

DROP TABLE IF EXISTS performance_summary;

-- TASK 1: 
-- Join loan_age and performance into a new table called performance_summary. Also, create LOAN IDENTIFIER as its index.
CREATE TABLE performance_summary
(
LOAN_IDENTIFIER VARCHAR(20),
LAST_LOAN_AGE NUMERIC(10,0),
METROPOLITAN_STATISTICAL_AREA VARCHAR(5),
ZERO_BALANCE_CODE VARCHAR(2)
);

INSERT INTO performance_summary
SELECT 
loan_age.LOAN_IDENTIFIER,
loan_age.LAST_LOAN_AGE,
performance.METROPOLITAN_STATISTICAL_AREA,
performance.ZERO_BALANCE_CODE
FROM 
loan_age INNER JOIN 
performance ON loan_age.LOAN_IDENTIFIER = performance.LOAN_IDENTIFIER
	AND loan_age.LAST_LOAN_AGE = performance.LOAN_AGE;

CREATE UNIQUE INDEX uidx_performance_summary ON performance_summary(LOAN_IDENTIFIER);

SELECT COUNT(*) FROM performance_summary;
SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
	WHERE TABLE_SCHEMA = 'fannie_db'
	AND TABLE_NAME = 'performance_summary';
SELECT * FROM performance_summary LIMIT 10;


-- ---------------------------------------------------------
-- Create a table for acquisitions with default information.
-- ---------------------------------------------------------

DROP TABLE IF EXISTS acquisitions_with_default_info;

CREATE TABLE acquisitions_with_default_info
(
LOAN_IDENTIFIER VARCHAR(20),
CHANNEL VARCHAR(1),
SELLER_NAME VARCHAR(80),
ORIGINAL_INTEREST_RATE NUMERIC(14,10),
ORIGINAL_UNPAID_PRINCIPAL_BALANCE NUMERIC(11,2),
ORIGINAL_LOAN_TERM NUMERIC(3,0),
ORIGINATION_DATE VARCHAR(7),
FIRST_PAYMENT_DATE VARCHAR(7),
ORIGINAL_LOAN_TO_VALUE NUMERIC(14,10),
ORIGINAL_COMBINED_LOAN_TO_VALUE NUMERIC(14,10),
NUMBER_OF_BORROWERS NUMERIC(3,0),
DEBT_TO_INCOME_RATIO NUMERIC(14,10),
BORROWER_CREDIT_SCORE NUMERIC(3,0),
FIRST_TIME_HOME_BUYER_INDICATOR VARCHAR(1),
LOAN_PURPOSE VARCHAR(1),
PROPERTY_TYPE VARCHAR(2),
NUMBER_OF_UNITS VARCHAR(10),
OCCUPANCY_STATUS VARCHAR(1),
PROPERTY_STATE VARCHAR(20),
ZIP_3_DIGIT VARCHAR(10),
MORTGAGE_INSURANCE_PERCENTAGE NUMERIC(14,10),
PRODUCT_TYPE VARCHAR(20),
CO_BORROWER_CREDIT_SCORE NUMERIC(3,0),
MORTGAGE_INSURANCE_TYPE NUMERIC(1,0),
METROPOLITAN_STATISTICAL_AREA VARCHAR(5),
ZERO_BALANCE_CODE VARCHAR(2),
LAST_LOAN_AGE NUMERIC(10,0)
);

-- TASK 2: 
-- Join performance summary and acquisitions into the new table called acquisitions_with_default_info.
INSERT INTO acquisitions_with_default_info
SELECT
acquisitions.LOAN_IDENTIFIER,
acquisitions.CHANNEL, 
acquisitions.SELLER_NAME, 
acquisitions.ORIGINAL_INTEREST_RATE,
acquisitions.ORIGINAL_UNPAID_PRINCIPAL_BALANCE,
acquisitions.ORIGINAL_LOAN_TERM,
acquisitions.ORIGINATION_DATE,
acquisitions.FIRST_PAYMENT_DATE,
acquisitions.ORIGINAL_LOAN_TO_VALUE,
acquisitions.ORIGINAL_COMBINED_LOAN_TO_VALUE,
acquisitions.NUMBER_OF_BORROWERS,
acquisitions.DEBT_TO_INCOME_RATIO,
acquisitions.BORROWER_CREDIT_SCORE,
acquisitions.FIRST_TIME_HOME_BUYER_INDICATOR,
acquisitions.LOAN_PURPOSE,
acquisitions.PROPERTY_TYPE,
acquisitions.NUMBER_OF_UNITS,
acquisitions.OCCUPANCY_STATUS,
acquisitions.PROPERTY_STATE,
acquisitions.ZIP_3_DIGIT,
acquisitions.MORTGAGE_INSURANCE_PERCENTAGE,
acquisitions.PRODUCT_TYPE,
acquisitions.CO_BORROWER_CREDIT_SCORE,
acquisitions.MORTGAGE_INSURANCE_TYPE,
performance_summary.METROPOLITAN_STATISTICAL_AREA,
performance_summary.ZERO_BALANCE_CODE,
performance_summary.LAST_LOAN_AGE
FROM 
performance_summary INNER JOIN
acquisitions ON performance_summary.LOAN_IDENTIFIER = acquisitions.LOAN_IDENTIFIER;

CREATE UNIQUE INDEX uidx_acquisitions_with_default_info ON acquisitions_with_default_info(LOAN_IDENTIFIER);

SELECT COUNT(*) FROM acquisitions_with_default_info;
SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS
	WHERE TABLE_SCHEMA = 'fannie_db'
	AND TABLE_NAME = 'acquisitions_with_default_info';
SELECT * FROM acquisitions_with_default_info LIMIT 10;

-- ----------------------------------------------------------------------
-- Write the acquisitions_with_default_info table to a csv file.
-- ----------------------------------------------------------------------

SELECT 'LOAN_IDENTIFIER',
'CHANNEL', 
'SELLER_NAME', 
'ORIGINAL_INTEREST_RATE',
'ORIGINAL_UNPAID_PRINCIPAL_BALANCE',
'ORIGINAL_LOAN_TERM',
'ORIGINATION_DATE',
'FIRST_PAYMENT_DATE',
'ORIGINAL_LOAN_TO_VALUE',
'ORIGINAL_COMBINED_LOAN_TO_VALUE',
'NUMBER_OF_BORROWERS',
'DEBT_TO_INCOME_RATIO',
'BORROWER_CREDIT_SCORE',
'FIRST_TIME_HOME_BUYER_INDICATOR',
'LOAN_PURPOSE',
'PROPERTY_TYPE',
'NUMBER_OF_UNITS',
'OCCUPANCY_STATUS',
'PROPERTY_STATE',
'ZIP_3_DIGIT',
'MORTGAGE_INSURANCE_PERCENTAGE',
'PRODUCT_TYPE',
'CO_BORROWER_CREDIT_SCORE',
'MORTGAGE_INSURANCE_TYPE',
'METROPOLITAN_STATISTICAL_AREA',
'ZERO_BALANCE_CODE',
'LAST_LOAN_AGE'
FROM DUAL
UNION
SELECT * FROM acquisitions_with_default_info
INTO OUTFILE 'C:/ProgramData/MySQL/MySQL Server 5.7/Uploads/acquisitions_with_default_infoQ3.csv'
COLUMNS TERMINATED BY '|' OPTIONALLY ENCLOSED BY '' ESCAPED BY ''
LINES TERMINATED BY '\n';

-- Note: The file above cannot be overwritten from SQL
-- and needs to be deleted manually before the OUTFILE statement can be rerun.