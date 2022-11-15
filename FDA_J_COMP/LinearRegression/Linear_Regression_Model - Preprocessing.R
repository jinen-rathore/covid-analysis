countryCodes = list("AUS", "CUB", "IND", "FRA", "ZAF", "USA")

for(countryCode in countryCodes) {
  subDirectory <- paste("./PreProcessedPlots/",countryCode)
  path <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code/FDA_J_COMP/countries/", countryCode, ".csv", sep ="")
  countryData <- read.csv(path)
  
  # imputing NA values using kNN approach
  #install.packages("VIM")
  # library('VIM')
  # countryData <- kNN(countryData, variable = c("total_vaccinations", "total_cases", "total_deaths", "total_tests", "stringency_index", "new_cases"), k = 2)
  
  #for (i in 1:25) {
  #  if (is.na(subset_orig$total_vaccinations[i])) subset_orig$total_vaccinations[i] = 0
  #}
  #for (i in 25:(length(subset_orig$total_vaccinations) - 25)) {
  #  if (is.na(subset_orig$total_vaccinations[i])) {
  #    val = (mean(na.omit(subset_orig$total_vaccinations[(i-25):(i-1)])) + mean(na.omit(subset_orig$total_vaccinations[(i+1):(i+25)]))) / 2
  #    subset_orig$total_vaccinations[i] = val
  #  }
  #}
  
  
  #countryData <- data.frame(countryData)
  
  summary(countryData)
  head(countryData)
  
  days <- c(1:length(countryData$date))
  
  countryVaccinations <- countryData$total_vaccinations
  
  for (i in 1:25) {
    if (is.na(countryVaccinations[i])) countryVaccinations[i] = 0
  }
  for (i in 25:(length(countryVaccinations) - 25)) {
    if (is.na(countryVaccinations[i])) {
      val = (mean(na.omit(countryVaccinations[(i-25):(i-1)])) + mean(na.omit(countryVaccinations[(i+1):(i+25)]))) / 2
      countryVaccinations[i] = val
    }
  }
  
  countryDeaths <- countryData$total_deaths
  
  for (i in 1:25) {
    if (is.na(countryDeaths[i])) countryDeaths[i] = 0
  }
  for (i in 25:(length(countryDeaths) - 25)) {
    if (is.na(countryDeaths[i])) {
      val = (mean(na.omit(countryDeaths[(i-25):(i-1)])) + mean(na.omit(countryDeaths[(i+1):(i+25)]))) / 2
      countryDeaths[i] = val
    }
  }

  countryTests <- countryData$total_tests
  
  
  for (i in 1:25) {
    if (is.na(countryTests[i])) countryTests[i] = 0
  }
  for (i in 25:(length(countryTests) - 25)) {
    if (is.na(countryTests[i])) {
      val = (mean(na.omit(countryTests[(i-25):(i-1)])) + mean(na.omit(countryTests[(i+1):(i+25)]))) / 2
      countryTests[i] = val
    }
  }

  countryCases <- countryData$total_cases
  
  for (i in 1:25) {
    if (is.na(countryCases[i])) countryCases[i] = 0
  }
  for (i in 25:(length(countryCases) - 25)) {
    if (is.na(countryCases[i])) {
      val = (mean(na.omit(countryCases[(i-25):(i-1)])) + mean(na.omit(countryCases[(i+1):(i+25)]))) / 2
      countryCases[i] = val
    }
  }
  
  countryStringencyIndex <- countryData$stringency_index
  
  for (i in 1:25) {
    if (is.na(countryStringencyIndex[i])) countryStringencyIndex[i] = 0
  }
  for (i in 25:(length(countryStringencyIndex) - 25)) {
    if (is.na(countryStringencyIndex[i])) {
      val = (mean(na.omit(countryStringencyIndex[(i-25):(i-1)])) + mean(na.omit(countryStringencyIndex[(i+1):(i+25)]))) / 2
      countryStringencyIndex[i] = val
    }
  }
  
  countryNewCases <- countryData$new_cases
  
  for (i in 1:25) {
    if (is.na(countryNewCases[i])) countryNewCases[i] = 0
  }
  for (i in 25:(length(countryNewCases) - 25)) {
    if (is.na(countryNewCases[i])) {
      val = (mean(na.omit(countryNewCases[(i-25):(i-1)])) + mean(na.omit(countryNewCases[(i+1):(i+25)]))) / 2
      countryNewCases[i] = val
    }
  }
  
  # Generating Linear Regression Plots
  
  # countryData[is.na(countryData)] = 0 #to make the NA values 0
  
  # Deaths v/s Total Cases
  
  mod <- lm(countryDeaths ~ countryCases)
  
  cat("Summary of Linear Regression Model for the case - Deaths v/s Total Cases")
  summary(mod)
  
  matrix_coef <- summary(mod)$coefficients  # Extract coefficients in matrix
  matrix_coef 
  
  my_estimates <- matrix_coef[ , 1]                     # Matrix manipulation to extract estimates
  my_estimates
  
  png(file = paste(subDirectory,"_Deaths_Cases_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryDeaths, type="l", lwd=3.0, main=paste("Deaths v/s Cases in", countryCode, "after Preprocessing", sep=" "), xlab = "Total Cases", ylab = "Total Deaths")
  
  abline(mod, col=2, lwd = 3)
  dev.off()
  
  # Vaccinations v/s Deaths
  
  mod1 <- lm(countryVaccinations ~ countryDeaths)
  
  #alternative approach to imputing the missing data using the linear regression model
  
  # tmpDF <- data.frame(countryVaccinations, countryDeaths)
  # 
  # tmpDF <- predict(mod1, tmpDF)
  # 
  # tmpDF <- as.data.frame(tmpDF)
  # 
  # for(index in 1:length(countryVaccinations)) {
  #   if(is.na(countryVaccinations[index])) {
  #     countryVaccinations[index] = tmpDF[index, 1]
  #   }
  # }
  # 
  # mod1 <- lm(countryVaccinations ~ countryDeaths)
  
  cat("Summary of Linear Regression Model for the case - Vaccinations v/s Total Deaths")
  summary(mod1)
  
  matrix_coef <- summary(mod1)$coefficients  # Extract coefficients in matrix
  matrix_coef 
  
  my_estimates <- matrix_coef[ , 1]                     # Matrix manipulation to extract estimates
  my_estimates
  
  png(file = paste(subDirectory,"_Vaccinations_Deaths_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryDeaths, countryVaccinations, type="l", lwd=3.0, main=paste("Vaccinations v/s Deaths in", countryCode, "after Preprocessing", sep=" "), xlab = "Total Deaths", ylab = "Total Vaccinations")
  
  abline(mod1, col=2, lwd = 3)
  dev.off()
  
  # Tests v/s Cases
  
  mod2 <- lm(countryTests ~ countryCases)
  
  cat("Summary of Linear Regression Model for the case - Tests v/s Cases")
  summary(mod2)
  
  matrix_coef <- summary(mod2)$coefficients  # Extract coefficients in matrix
  matrix_coef 
  
  my_estimates <- matrix_coef[ , 1]                     # Matrix manipulation to extract estimates
  my_estimates
  
  png(file = paste(subDirectory,"_Tests_Cases_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryTests, type="l", lwd=3.0, main=paste("Tests v/s Cases in", countryCode, "after Preprocessing", sep=" "), xlab = "Total Cases", ylab = "Total Tests")
  
  abline(mod2, col=2, lwd = 3)
  dev.off()
  
  # Deaths v/s Timeline
  
  mod3 <- lm(countryDeaths ~ days)
  
  cat("Summary of Linear Regression Model for the case - Deaths v/s Time")
  summary(mod3)
  
  matrix_coef <- summary(mod3)$coefficients  # Extract coefficients in matrix
  matrix_coef 
  
  my_estimates <- matrix_coef[ , 1]                     # Matrix manipulation to extract estimates
  my_estimates
  
  png(file = paste(subDirectory,"_Deaths_Days_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(days, countryDeaths, type="l", lwd=3.0, main=paste("Deaths v/s Days in", countryCode, "after Preprocessing", sep=" "), xlab = "Days", ylab = "Total Deaths")
  
  abline(mod3, col=2, lwd = 3)
  dev.off()
  
  # Stringency Index v/s Cases 
  
  # countryData$total_cases[is.na(countryData$total_cases)] = 0
  
  mod4 <- lm(countryStringencyIndex ~ countryCases)
  
  cat("Summary of Linear Regression Model for the case - Stringency Index v/s Cases")
  summary(mod4)
  
  matrix_coef <- summary(mod4)$coefficients  # Extract coefficients in matrix
  matrix_coef 
  
  my_estimates <- matrix_coef[ , 1]                     # Matrix manipulation to extract estimates
  my_estimates

  png(file = paste(subDirectory,"_StringencyIndex_Cases_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryStringencyIndex, type="l", lwd=3.0, main=paste("Stringency Index v/s Cases in", countryCode, "after Preprocessing", sep=" "), xlab = "Total Cases", ylab = "Stringency Index")
  
  abline(mod4, col=2, lwd = 3)
  dev.off()
}