library(dplyr)
library(tidyr)
library(data.table)
library(forecast)

countryCodes = list("AUS", "CUB", "IND", "FRA", "ZAF", "USA")

for(countryCode in countryCodes){
  subDir <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code - github/FDA_J_COMP/Airma/plots/", countryCode)
  path <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code - github/FDA_J_COMP/countries/", countryCode, ".csv", sep = "")
  
  df = read.csv(path,stringsAsFactors = T)
  
  total.cases <- df$total_cases
  total.cases <- total.cases[complete.cases(total.cases)]
  
  new.cases <- df$new_cases
  new.cases <- new.cases[complete.cases(new.cases)]
  
  total.deaths <- df$total_deaths
  total.deaths <- total.deaths[complete.cases(total.deaths)]
  
  new.deaths <- df$new_deaths
  new.deaths <- new.deaths[complete.cases(new.deaths)]
  
  total.vaccinations <- df$total_vaccinations
  total.vaccinations <- total.vaccinations[complete.cases(total.vaccinations)]
  
  new.vaccinations <- df$new_vaccinations
  new.vaccinations <- new.vaccinations[complete.cases(new.vaccinations)]
  
  days <- c(1:length(df$date))
  
  
  #using Arima model on total cases:
  
  fit_total_cases <- auto.arima(total.cases)
  print(summary(fit_total_cases))
  
  forecasted_total_cases <- forecast(fit_total_cases, 100)
  
  png(file = paste(subDir, "_Total_Cases.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_total_cases, main = paste("Total Cases in",countryCode, sep = " ") ,xlab = "Days", ylab = "Total Cases")
  dev.off()
  
  #using Arima model on new cases:
  
  fit_new_cases <- auto.arima(new.cases)
  print(summary(fit_new_cases))
  
  forecasted_new_cases <- forecast(fit_new_cases, 100)
  
  png(file = paste(subDir, "_New_Cases.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_new_cases, main = paste("New Cases in",countryCode, sep = " ") ,xlab = "Days", ylab = "New Cases")
  dev.off()
  
  #using Arima model on total deaths:
  
  fit_total_deaths <- auto.arima(total.deaths)
  print(summary(fit_total_deaths))
  
  forecasted_total_deaths <- forecast(fit_total_deaths, 100)
  
  png(file = paste(subDir, "_Total_Deaths.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_total_deaths, main = paste("Total Deaths in",countryCode, sep = " ") ,xlab = "Days", ylab = "Total Deaths")
  dev.off()
  
  #using Arima model on new deaths:
  
  fit_new_deaths <- auto.arima(new.deaths)
  print(summary(fit_new_deaths))
  
  forecasted_new_deaths <- forecast(fit_new_deaths, 100)
  
  png(file = paste(subDir, "_New_Deaths.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_new_deaths, main = paste("New Deaths in",countryCode, sep = " ") ,xlab = "Days", ylab = "New Deaths")
  dev.off()
  
  #using Arima model on total vaccinations:
  
  fit_total_vaccinations <- auto.arima(total.vaccinations)
  print(summary(fit_total_vaccinations))
  
  forecasted_total_vaccinations <- forecast(fit_total_vaccinations, 100)
  
  png(file = paste(subDir, "_Total_Vaccinations.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_total_vaccinations, main = paste("Total Vaccinations in",countryCode, sep = " ") ,xlab = "Days", ylab = "Total Vaccinations")
  dev.off()
  
  #using Arima model on new vaccinations:
  
  fit_new_vaccinations <- auto.arima(new.vaccinations)
  print(summary(fit_new_vaccinations))
  
  forecasted_new_vaccinations <- forecast(fit_new_vaccinations, 100)
  
  png(file = paste(subDir, "_New_Vaccinations.png", sep = ""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(forecasted_new_vaccinations, main = paste("New Vaccinations in",countryCode, sep = " ") ,xlab = "Days", ylab = "New Vaccinations")
  dev.off()
}
