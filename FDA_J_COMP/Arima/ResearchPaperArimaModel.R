library(tseries)
library(forecast)
library(hydroGOF)
library(zoo)
library(dplyr)

countryCodes = list("IND","USA")

for(countryCode in countryCodes){
  
  path <- paste("D:/VIT/SEM5/CSE3505 FDA/jComponent/countries/", countryCode ,".csv", sep = "")
  print("------------------------------------------------------")
  print(countryCode)
  print("------------------------------------------------------")
  df = read.csv(path,stringsAsFactors = T)
  
  subdf <- df %>% select(date, new_cases, total_deaths, total_vaccinations)
  # imputing the dataset using last value carried forward method
  subdf[1,c(3,4)] <- 0
  imputed_df <- na.locf(subdf)
  
  imputed_df$date = as.Date(imputed_df$date, format = c("%d/%m/%y"))
  
  ##########################################################################################################
  # new cases 
  new.cases.ts <- ts(imputed_df$new_cases, start = as.Date(head(df$date,1),format = c("%d/%m/%y")), 
                     end = as.Date(tail(df$date,1),format = c("%d/%m/%y")))
  
  print(adf.test(new.cases.ts))
  
  model.new.cases <- auto.arima(new.cases.ts)
  checkresiduals(model.new.cases)
  print("----------SUMMARY NEW CASES----------")
  print(summary(model.new.cases))
  print("----------ACCURACY NEW CASES----------")
  print(accuracy(model.new.cases))
  forecasted_new_cases <- forecast(model.new.cases, 100)
  autoplot(forecasted_new_cases)
  
  ##########################################################################################################
  #total deaths
  
  total.deaths.ts <- ts(imputed_df$total_deaths, start = as.Date(head(df$date,1),format = c("%d/%m/%y")), 
                     end = as.Date(tail(df$date,1),format = c("%d/%m/%y")))
  
  print(adf.test(total.deaths.ts))
  
  model.total.deaths <- auto.arima(total.deaths.ts)
  checkresiduals(model.total.deaths)
  print("----------SUMMARY TOTAL DEATHS----------")
  print(summary(model.total.deaths))
  print("----------ACCURACY TOTAL DEATHS----------")
  print(accuracy(model.total.deaths))
  
  forecasted_total_deaths <- forecast(model.total.deaths, 100)
  autoplot(forecasted_total_deaths)
  ##########################################################################################################
  
  #total vaccinations
  
  total.vaccinations.ts <- ts(imputed_df$total_vaccinations, start = as.Date(head(df$date,1),format = c("%d/%m/%y")), 
                        end = as.Date(tail(df$date,1),format = c("%d/%m/%y")))
  
  print(adf.test(total.vaccinations.ts))
  
  model.total.vaccinations <- auto.arima(total.vaccinations.ts)
  checkresiduals(model.total.vaccinations)
  print("----------SUMMARY TOTAL VACCINATIONS----------")
  print(summary(model.total.vaccinations))
  print("----------ACCURACY TOTAL VACCINATIONS----------")
  print(accuracy(model.total.vaccinations))
  
  forecasted_total_vaccinations <- forecast(model.total.vaccinations, 100)
  autoplot(forecasted_total_vaccinations)
  ##########################################################################################################
}
