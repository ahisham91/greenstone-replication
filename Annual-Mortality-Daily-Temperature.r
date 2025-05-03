#Aggregate Response function between Annual Mortality Rate and Average Daily Temperatures
#This code replicates the results in the paper "The Effect of Temperature on Mortality in the United States: New Evidence from the National Health Interview Survey" by Deschenes and Greenstone (2007)
#The data is from the National Health Interview Survey (NHIS) and the temperature data is from the National Oceanic and Atmospheric Administration (NOAA)
getwd()
rm(list=ls())
library(lfe)
data <- read.csv("icp_indiv_2_dg2011_rep_nomiss.csv")
dg2011 <- felm(cruderate ~ tday_lt10 + tday_10_20 + tday_20_30 + tday_30_40 + tday_40_50 + tday_50_60 + tday_70_80 + tday_80_90 + tday_gt90 +
                 prec_10_15 + prec_15_20 + prec_20_25 + prec_25_30 + prec_30_35 + prec_35_40 + prec_40_45 + prec_45_50 + prec_50_55 + prec_55_60 + prec_gt60 |
                 countycode + ssyy | 0 | countycode, data = data, weights = data$population)
summary(dg2011)


temperature_bins <- c("tday_lt10", "tday_10_20", "tday_20_30", "tday_30_40", "tday_40_50", "tday_50_60", "tday_70_80", "tday_80_90", "tday_gt90")
estimates <- c(3.7287, 2.6741, 3.5762, 1.8128, 1.4522, 0.2559, 0.8356, 1.3653, 5.3453)
std_errors <- c(1.3838, 1.1061, 0.8509, 0.6685, 0.5699, 0.3271, 0.6805, 0.8860, 1.3827)

results_df <- data.frame(temperature_bin = temperature_bins, estimate = estimates, std_error = std_errors)

# Normalize the estimates so that the 50º–60º F category is set to 0
results_df$normalized_estimate <- ifelse(results_df$temperature_bin == "tday_50_60", 0, results_df$estimate)

# Calculate the error bars (+/- 2 standard errors)
results_df$upper_error <- results_df$normalized_estimate + 2 * results_df$std_error
results_df$lower_error <- results_df$normalized_estimate - 2 * results_df$std_error

# Create the plot
library(ggplot2)

ggplot(results_df, aes(x = temperature_bin, y = normalized_estimate)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower_error, ymax = upper_error), width = 0.2) +
  geom_text(aes(label = round(estimate, 2)), vjust = -2) +
  labs(title = "Aggregate Response Function between Annual Mortality Rate and Average Daily Temperatures",
       x = "Temperature Bin",
       y = "Age-Adjusted Mortality Rate Change (per 100,000, relative to 50º–60º F)",
       caption = "Error bars represent ±2 standard errors.") +
  theme_minimal()