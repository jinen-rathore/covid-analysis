library(dplyr)
library(tidyr)
library(data.table)
library(forecast)
library(zoo)

countryCodes = list("AUS", "CUB", "IND", "FRA", "ZAF", "USA")

for(countryCode in countryCodes){
  subDir <- paste("D:/VIT/SEM5/CSE3505 FDA/jComponent/Airma/imputedPlots/", countryCode)
  path <- paste("D:/VIT/SEM5/CSE3505 FDA/jComponent/countries/", countryCode, ".csv", sep = "")
  
  df = read.csv(path,stringsAsFactors = T)
  
  subdf <- df %>% select(total_cases, new_cases, total_deaths, new_deaths, new_vaccinations, total_vaccinations)
  
  subdf[1,c(3,4,5,6)] <- 0
  
  imputed_df <- na.locf(subdf)
  
  days <- c(1:length(df$date))
  
  
  #using Arima model on total cases:
  
  fit_total_cases <- auto.arima(imputed_df$total_cases)
  print("----------SUMMARY TOTAL CASES----------")
  print(summary(fit_total_cases))
  
  forecasted_total_cases <- forecast(fit_total_cases, 100)
  
  png(file = paste(subDir, "_Total_Cases.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_total_cases, main = paste("Total Cases in",countryCode, sep = " ") ,xlab = "Days", ylab = "Total Cases")
  dev.off()
  
  #using Arima model on new cases:
  
  fit_new_cases <- auto.arima(imputed_df$new_cases)
  print("----------SUMMARY NEW CASES----------")
  print(summary(fit_new_cases))
  
  forecasted_new_cases <- forecast(fit_new_cases, 100)
  
  png(file = paste(subDir, "_New_Cases.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_new_cases, main = paste("New Cases in",countryCode, sep = " ") ,xlab = "Days", ylab = "New Cases")
  dev.off()
  
  #using Arima model on total deaths:
  
  fit_total_deaths <- auto.arima(imputed_df$total_deaths)
  print("----------SUMMARY TOTAL DEATHS----------")
  print(summary(fit_total_deaths))
  
  forecasted_total_deaths <- forecast(fit_total_deaths, 100)
  
  png(file = paste(subDir, "_Total_Deaths.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_total_deaths, main = paste("Total Deaths in",countryCode, sep = " ") ,xlab = "Days", ylab = "Total Deaths")
  dev.off()
  
  #using Arima model on new deaths:
  
  fit_new_deaths <- auto.arima(imputed_df$new_deaths)
  print("----------SUMMARY NEW DEATHS----------")
  print(summary(fit_new_deaths))
  
  forecasted_new_deaths <- forecast(fit_new_deaths, 100)
  
  png(file = paste(subDir, "_New_Deaths.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_new_deaths, main = paste("New Deaths in",countryCode, sep = " ") ,xlab = "Days", ylab = "New Deaths")
  dev.off()
  
  #using Arima model on total vaccinations:
  
  fit_total_vaccinations <- auto.arima(imputed_df$total_vaccinations)
  print("----------SUMMARY TOTAL VACCINATIONS----------")
  print(summary(fit_total_vaccinations))
  
  forecasted_total_vaccinations <- forecast(fit_total_vaccinations, 100)
  
  png(file = paste(subDir, "_Total_Vaccinations.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_total_vaccinations, main = paste("Total Vaccinations in",countryCode, sep = " ") ,xlab = "Days", ylab = "Total Vaccinations")
  dev.off()
  
  #using Arima model on new vaccinations:
  
  fit_new_vaccinations <- auto.arima(imputed_df$new_vaccinations)
  print("----------SUMMARY NEW VACCINATIONS----------")
  print(summary(fit_new_vaccinations))
  
  forecasted_new_vaccinations <- forecast(fit_new_vaccinations, 100)
  
  png(file = paste(subDir, "_New_Vaccinations.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_new_vaccinations, main = paste("New Vaccinations in",countryCode, sep = " ") ,xlab = "Days", ylab = "New Vaccinations")
  dev.off()
}
