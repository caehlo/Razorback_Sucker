# load packages
library(dplyr)
library(RMariaDB)
library(ggplot2)
library(readxl)
library(lubridate)
library(dbplyr)
library(binom)
library(readr)
library(ggpubr)
library(formattable)
model <- readRDS('C:/Users/chase/OneDrive/Documents/Razorback_Sucker/XYTE_model.rds')
# make MySQL connection
setwd("C:/Users/chase/OneDrive/Documents/Razorback_Sucker")
MySQLSettings <- "C:/Users/chase/OneDrive/Documents/R/PIT tag database/brk828_nativef1_sensing.cnf"
con <- dbConnect(RMariaDB::MariaDB(),default.file=MySQLSettings, group="settings")
dbListTables(con)
releases <- dbReadTable(con, 'WebTable')
location <- dbReadTable(con, 'location')

#create actual stocking data table
actuals <- releases %>% filter(STATUS == 'Release', SPECIES_ID == 'XYTE', 
                               between(COLLECTION_DATE, as.Date('2021-12-31'), 
                                       as.Date('2023-01-01')), TL >= 300) %>%
  select(COLLECTION_DATE, LID, TL) %>% 
  inner_join(location %>% select(LID = LOCATION_ID, MSCP_REACH) %>%
                                                    filter(MSCP_REACH %in% c(2,3,4)), by = "LID") %>% 
  mutate(ReleaseSeason = case_when(format(COLLECTION_DATE, format = "%m") %in% c("9", "10", "11") ~ "pre-spawn",
                                   format(COLLECTION_DATE, format = "%m") %in% c("12", "01", "02", "03") ~ "spawn",
                                   format(COLLECTION_DATE, format = "%m") %in% c("04", "05") ~ "post-spawn"))
actuals$MSCP_REACH <- as.factor(actuals$MSCP_REACH)
ggplot(actuals, aes(x = TL)) + geom_histogram() + facet_grid(MSCP_REACH ~ .)

#create a proposed stocking data table
# Calculate the quantiles to know TL cutoffs for an equal 3 part distribution
quantiles <- quantile(actuals$TL, probs = c(1/3, 2/3))

# Extract the cutoff values for each section
cutoff1 <- quantiles[1]
cutoff2 <- quantiles[2]

# Print the cutoff values
cat("Cutoff 1:", cutoff1, "\n")
cat("Cutoff 2:", cutoff2, "\n")

#set MSCP_REACH based on the TL cutoff values
proposed <- actuals %>% 
  select(-MSCP_REACH, -ReleaseSeason) %>% 
  mutate(MSCP_REACH = case_when(TL <= cutoff1 ~ 3,
                                TL >= cutoff2 ~ 2,
                                TL > cutoff1 ~ 4,)) %>%
  mutate(ReleaseSeason = 'spawn')
proposed$MSCP_REACH <- as.factor(proposed$MSCP_REACH)
ggplot(proposed, aes(x = TL)) + geom_histogram() + facet_grid(MSCP_REACH~.)

#apply model to actuals
pred <- predict(model, newdata = actuals, type = "response", se.fit = TRUE)
lowerCI <- pred$fit - (1.96 * pred$se.fit)
upperCI <- pred$fit + (1.96 * pred$se.fit)
actuals <- cbind(actuals, pred, lowerCI, upperCI)

pred2 <- predict(model, newdata = proposed, type = "response", se.fit = TRUE)
lowerCI2 <- pred$fit - (1.96 * pred$se.fit)
upperCI2 <- pred$fit + (1.96 * pred$se.fit)
proposed <- cbind(proposed, pred2, lowerCI2, upperCI2)

#clean up columns and combine actual and proposed
actuals <- actuals %>% mutate(method = 'actuals')
proposed <- proposed %>% rename('lowerCI' = 'lowerCI2', 'upperCI' = 'upperCI2') %>% mutate(method = 'proposed')
data <- rbind(actuals, proposed)

ggplot(data, aes(x = fit, fill = method)) + geom_histogram(position="identity", alpha = 0.75) + facet_grid(.~MSCP_REACH)

#Playing around with monte carlo simulations
num_simulations <- 1000
mc_results <- replicate(num_simulations, rbinom(n = nrow(data), size = 1, prob = data$fit))
#calculating the mean of the results
proportion_survived <- data.frame(rowMeans(mc_results))

#getting confidence intervals from the monte carlo
confidence_level <- 0.95
conf_intervals <- t(apply(mc_results, 1, function(x) {
  prop.test(sum(x), length(x), conf.level = confidence_level)$conf.int
}))

#cbinding Monte Carlo results to the test data sample and changing column names
MC_final <- cbind(data$TL, data$MSCP_REACH, data$method, proportion_survived, conf_intervals)
MC_final <- MC_final %>% rename('prob' = 'rowMeans.mc_results.', 'LCI' = '1', 'UCI' = '2', 
                                'TL' = 'data$TL', 'MSCP_REACH' = 'data$MSCP_REACH', 'Method' = 'data$method')
MC_estimate <- MC_final %>% group_by(Reach = MSCP_REACH, Method) %>% 
  summarize(Stocked = n(), Mean_TL = mean(TL), Est_Surv = sum(prob), LCI = sum(LCI), UCI = sum(UCI)) %>%
  mutate(Perc_Surv = Est_Surv/Stocked)
formattable(MC_estimate)

MC_estimate %>% group_by(Method) %>% summarize(sum(Est_Surv))


