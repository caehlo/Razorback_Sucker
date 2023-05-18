# load packages
library(dplyr)
library(RMariaDB)
library(ggplot2)
library(ggridges)
library(readxl)
library(ggtext)
library(ggrepel)
library(lubridate)
library(dbplyr)

# make MySQL connection
setwd("C:/Users/chase/OneDrive/Documents/R/PIT tag database")
MySQLSettings <- "brk828_nativef1_sensing.cnf"
con <- dbConnect(RMariaDB::MariaDB(),default.file=MySQLSettings, group="settings")
dbListTables(con)

# Download source material
scan_db <- tbl(con, "scan_data")
effort_db <- tbl(con, "scan_effort")
location_db <- tbl(con, "location")
release_db <- tbl(con, "WebTable")
dbListFields(con, 'WebTable')

# Connecting tables to get scans from Topock Marsh with release history
ScanningTable <- release_db %>%
  select(LID, DISP_DATE, PIT = TAG_NUMBER, STATUS, SPECIES_ID, TL) %>% 
  filter(STATUS == 'Release', SPECIES_ID == 'XYTE') %>%
  inner_join(location_db %>% select(LID = LOCATION_ID, MSCP_REACH, ZONE) %>%
               filter(MSCP_REACH %in% c(2,3)), by = "LID") %>%
  left_join(scan_db %>% select(PIT, date), by = 'PIT') %>%
  collect()
ScanningTable <- ScanningTable %>% mutate(Release_Date = mdy(DISP_DATE)) %>% select(-DISP_DATE)

#set maximum date of 2 years prior to stocking and minimum date as fish that have been stocked within the last 10 years
MaximumDate <- Sys.Date() - years(2)
MinimumDate <- Sys.Date() - years(10)
XYTE <- ScanningTable %>% filter(Release_Date >= MinimumDate & Release_Date <= MaximumDate) %>% 
  group_by(PIT, TL, Release_Date, MSCP_REACH) %>% summarize(ZONE = max(ZONE), Scan_Date = max(date)) %>% 
  mutate(DAL = difftime(Scan_Date, Release_Date)) %>% mutate(DAL = as.numeric(DAL))

