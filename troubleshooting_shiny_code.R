# Read the first CSV file
data1 <- read.csv("test_2.csv")

# Read the second CSV file
data2 <- read.csv("test_3.csv")

# Read the third CSV file
data3 <- read.csv("test_4.csv")

# Combine the three datasets into a single dataframe
combined_data <- rbind(data1, data2, data3)

locations <- readRDS('locations.rds')

saveRDS(locations, 'locations.rds')

merged_data <- left_join(combined_data, locations, by = "LOCATION", copy = TRUE)
merged_data$RELEASE_DATE <- as.Date(merged_data$RELEASE_DATE)
# Perform necessary mutations and column selection
merged_data <- merged_data %>%
  mutate(ReleaseSeason = case_when(
    format(RELEASE_DATE, format = "%m") %in% c("9", "10", "11") ~ "pre-spawn",
    format(RELEASE_DATE, format = "%m") %in% c("12", "01", "02", "03") ~ "spawn",
    format(RELEASE_DATE, format = "%m") %in% c("04", "05") ~ "post-spawn"
  )) %>% select(SPECIES_ID, RELEASE_DATE, LOCATION, TL, MSCP_REACH, ReleaseSeason) %>% na.omit()
merged_data$MSCP_REACH <- as.factor(merged_data$MSCP_REACH)
model <- readRDS("XYTE_model.rds")
predictions <- predict(model, newdata = merged_data, type = "response", se.fit = TRUE)
merged_data$fit <- predictions$fit
merged_data$se.fit <- predictions$se.fit
merged_data$lowerCI <- merged_data$fit - (1.96 * merged_data$se.fit)
merged_data$upperCI <- merged_data$fit + (1.96 * merged_data$se.fit)

merged_data

#Playing around with monte carlo simulations
num_simulations <- 1000
mc_results <- replicate(num_simulations, rbinom(n = nrow(merged_data), size = 1, prob = merged_data$fit))
#calculating the mean of the results
proportion_survived <- data.frame(rowMeans(mc_results))
colnames(proportion_survived) <- "prob"
confidence_level <- 0.95
conf_intervals <- t(apply(mc_results, 1, function(x) {
  prop.test(sum(x), length(x), conf.level = confidence_level)$conf.int
}))
MC_final <- cbind(merged_data$TL, merged_data$MSCP_REACH, proportion_survived, conf_intervals)
colnames(MC_final) <- c('TL', 'Reach', 'prob', 'LCI', 'UCI')
MC_estimate <- MC_final %>% group_by(Reach) %>% 
  summarize(Stocked = n(), Est_Surv = sum(prob), LCI = sum(LCI), UCI = sum(UCI)) %>%
  mutate(Perc_Surv = Est_Surv/Stocked)
MC_estimate
datatable(MC_estimate, options = list(pageLength = 10, scrollX = TRUE))
view(table)
