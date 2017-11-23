# Data

NYC DOB full cleaned data with zip: https://storage.googleapis.com/dob-data/NYC/DOB_Permit_cleaned.csv

NYC Crime processed data non-violent with zip: https://storage.googleapis.com/crime-data/nyc/nyc_crime_nonviolent_with_zip.csv

NYC Crime processed data violent with zip: https://storage.googleapis.com/crime-data/nyc/nyc_crime_violent_with_zip.csv


BigQuery for processing raw NYC Crime data
```sql
SELECT
  CMPLNT_NUM,
  CMPLNT_FR_DT,
  CMPLNT_TO_DT,
  RPT_DT,
  KY_CD,
  OFNS_DESC,
  CRM_ATPT_CPTD_CD,
  BORO_NM,
  CAST(ROUND(LATITUDE,4) AS STRING) AS LAT_TRIM,
  CAST(ROUND(LONGITUDE,4) AS STRING) AS LON_TRIM,
  LATITUDE,
  LONGITUDE
FROM
  `capstone-181304.nyc_crime.nyc_crime`
WHERE
  KY_CD IN (
  SELECT
    KY_CD
  FROM
    `capstone-181304.nyc_crime.crime_category_violent`)
  AND LATITUDE IS NOT NULL
  AND LONGITUDE IS NOT NULL
```

Violent Crimes definition
```
MURDER & NON-NEGL. MANSLAUGHTER	 
HOMICIDE-NEGLIGENT-VEHICLE	 
HOMICIDE-NEGLIGENT,UNCLASSIFIE	 
RAPE	 
ROBBERY	 
FELONY ASSAULT	 
BURGLARY	 
GRAND LARCENY	 
GRAND LARCENY OF MOTOR VEHICLE	 
ARSON	 
DANGEROUS DRUGS	 
DANGEROUS WEAPONS	 
KIDNAPPING & RELATED OFFENSES	 
KIDNAPPING AND RELATED OFFENSES	 
KIDNAPPING	 
NYS LAWS-UNCLASSIFIED FELONY	 
DANGEROUS DRUGS	 
DANGEROUS WEAPONS	 
ASSAULT 3 & RELATED OFFENSES
```
