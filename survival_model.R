# load packages
library(dplyr)
library(RMariaDB)
library(ggplot2)
library(readxl)
library(lubridate)
library(dbplyr)
library(binom)
library(readr)

# make MySQL connection
setwd("C:/Users/chase/OneDrive/Documents/Razorback_Sucker")
MySQLSettings <- "C:/Users/chase/OneDrive/Documents/R/PIT tag database/brk828_nativef1_sensing.cnf"
con <- dbConnect(RMariaDB::MariaDB(),default.file=MySQLSettings, group="settings")
dbListTables(con)

# Download source material
scan_db <- tbl(con, "scan_data")
effort_db <- tbl(con, "scan_effort")
location_db <- tbl(con, "location")
release_db <- tbl(con, "WebTable")
dbListFields(con, 'WebTable')

#Connecting tables and data wrangling to get stocked fish and scan history from Razorback in Reaches 2 and 3
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

#Create new table to add 0's and 1's and season based on if the fish was scanned at least 1 year post stocking
XYTE <- ScanningTable %>% filter(Release_Date >= MinimumDate & Release_Date <= MaximumDate) %>% 
  group_by(PIT, TL, Release_Date, MSCP_REACH) %>% summarize(ZONE = max(ZONE), Scan_Date = max(date)) %>%
  mutate(Survivor =
           case_when(is.na(Scan_Date) ~ 0,
                     Scan_Date >= Release_Date + 365 ~ 1,
                     Scan_Date < Release_Date + 365 ~ 0)) %>%
  mutate(ReleaseSeason = 
           case_when(format(Release_Date, format = "%m") %in% c("9", "10", "11") ~ "pre-spawn",
                     format(Release_Date, format = "%m") %in% c("12", "01", "02", "03") ~ "spawn",
                     format(Release_Date, format = "%m") %in% c("04", "05") ~ "post-spawn")) %>% 
  select(-Scan_Date, -Release_Date) %>% na.omit() %>% mutate(MSCP_REACH = as.factor(MSCP_REACH))

#glm model
model <- glm(Survivor ~ TL * MSCP_REACH * ReleaseSeason,
             family = binomial(link = 'logit'), data = XYTE)

summary(model)


#import test stocking data to apply model to
testdata <- read_csv("~/Razorback_Sucker/testdata.csv", 
                     col_types = cols(MSCP_REACH = col_factor(levels = c("2", 
                                                                         "3"))))
#apply model to test stocking data
pred <- predict(model, newdata = testdata, type = "response", se.fit = TRUE)
lowerCI <- pred$fit - (1.96 * pred$se.fit)
upperCI <- pred$fit + (1.96 * pred$se.fit)
testdata <- cbind(testdata, pred, lowerCI, upperCI)
Estimate <- testdata %>% summarize(prob = sum(fit), LCI = sum(lowerCI), UCI = sum(upperCI))

#Total length and probability
TL_regr <- ggplot(testdata, aes(x = TL, y = fit)) + 
  geom_line(linewidth = 1, color = 'steelblue2') + 
  geom_ribbon(aes(x = TL, ymin = lowerCI, ymax = upperCI), alpha = 0.3, fill = 'steelblue2')
ggsave(plot = TL_regr, file = 'TL_regr.png')

#frequency of probabilities
Prob_dist <- ggplot(testdata, aes(x = fit)) + 
  geom_histogram(position="identity", alpha = 0.75, fill = 'steelblue2')
ggsave(plot = Prob_dist, file = 'Prob_dist.png')


#Playing around with monte carlo simulations
num_simulations <- 1000
mc_results <- replicate(num_simulations, rbinom(n = nrow(testdata), size = 1, prob = testdata$fit))
#calculating the mean of the results
proportion_survived <- data.frame(rowMeans(mc_results))

#getting confidence intervals from the monte carlo
confidence_level <- 0.95
conf_intervals <- t(apply(mc_results, 1, function(x) {
  prop.test(sum(x), length(x), conf.level = confidence_level)$conf.int
}))

#cbinding Monte Carlo results to the test data sample and changing column names
MC_final <- cbind(testdata$TL, proportion_survived, conf_intervals)
MC_final <- MC_results %>% rename('prob' = 'rowMeans.mc_results.', 'LCI' = '1', 'UCI' = '2', 'TL' = 'testdata$TL')
MC_estimate <- MC_results %>% summarize(prob = sum(prob), LCI = sum(LCI), UCI = sum(UCI))

#save objects to workflow
saveRDS(XYTE, file = "XYTE_model_data.rds")
saveRDS(model, file = "XYTE_model.rds")
saveRDS(testdata, file = 'test_stocking_data.rds')


