  library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(data.table)
library(odbc)
library(RMariaDB)
library(ggridges)
library(forcats)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "marc.escobosa",
  password = "***********",
  host = "*************",
  dbname = "mimiciiiv14",
  port = 3306
)


sql <- "SELECT di.SUBJECT_ID, p.DOD IS NOT NULL AS DECEASED
FROM DIAGNOSES_ICD di
JOIN PATIENTS p  ON p.SUBJECT_ID = di.SUBJECT_ID 
WHERE di.ICD9_CODE NOT REGEXP '^1[4-9][0-9]|^2[0-3][0-9]' 
"

sql2 <- "SELECT  p.SUBJECT_ID,
di.SEQ_NUM,
di.HADM_ID,
did.LONG_TITLE,
di.ICD9_CODE,
p.EXPIRE_FLAG, a.ADMISSION_TYPE, a.ADMISSION_LOCATION, a.DISCHARGE_LOCATION, a.INSURANCE, a.RELIGION, a.MARITAL_STATUS, a.ETHNICITY, a.DIAGNOSIS, a.HOSPITAL_EXPIRE_FLAG,
a.HAS_CHARTEVENTS_DATA, 
p.GENDER,
ROUND(DATEDIFF(a.ADMITTIME, p.DOB)/365.242) AS AGE,
DATEDIFF(a.DISCHTIME, a.ADMITTIME) AS LOS,
a.ETHNICITY, p.DOB,
p.DOD IS NOT NULL AS DECEASED
FROM DIAGNOSES_ICD di
INNER JOIN D_ICD_DIAGNOSES did ON di.ICD9_CODE = did.ICD9_CODE 
INNER JOIN PATIENTS p ON p.SUBJECT_ID=di.SUBJECT_ID
INNER JOIN ADMISSIONS a ON a.HADM_ID=di.HADM_ID
WHERE di.ICD9_CODE  REGEXP  '^1[4-9][0-9]|^2[0-3][0-9]'"

sql3 <- "SELECT did.ICD9_CODE, di.HADM_ID, did.LONG_TITLE , COUNT(DISTINCT SUBJECT_ID ) AS N,(COUNT(DISTINCT SUBJECT_ID) / (SELECT COUNT(DISTINCT(SUBJECT_ID))  
FROM DIAGNOSES_ICD di
WHERE di.ICD9_CODE REGEXP '^1[4-9][0-9]|^2[0-3][0-9]') *100) AS Prevalence 
FROM DIAGNOSES_ICD di
INNER JOIN D_ICD_DIAGNOSES did ON did.ICD9_CODE = di.ICD9_CODE 
WHERE di.HADM_ID IN (SELECT di.HADM_ID 
FROM DIAGNOSES_ICD di 
WHERE di.ICD9_CODE NOT REGEXP '^1[4-9][0-9]|^2[0-3][0-9]') 
GROUP BY did.ICD9_CODE 
ORDER BY Prevalence DESC"

tbl <- dbGetQuery(con, sql)
tbl2 <- dbGetQuery(con, sql2)
tbl3 <- dbGetQuery(con, sql3)
write.table(tbl,file = "table_all_subjects.txt")
write.table(tbl2, file = "tbl2.txt")
write.table(tbl3,file = "tblComor.txt")


