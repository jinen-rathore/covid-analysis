countryCodes = list("AUS", "CUB", "IND", "FRA", "ZAF", "USA")

for(countryCode in countryCodes) {
  subDirectory <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code/FDA_J_COMP/LinearRegression/PreProcessedPlots/",countryCode)
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
  
  
  countryVaccinations <- countryData$total_vaccinations
  countryCases <- countryData$total_cases
  countryDeaths <- countryData$total_deaths
  countryTests <- countryData$total_tests
  countryStringencyIndex <- countryData$stringency_index
  countryNewCases <- countryData$new_cases
  days <- c(1:length(countryData$date))
  
  # Generating Linear Regression Plots
  
  # countryData[is.na(countryData)] = 0 #to make the NA values 0
  
  # Deaths v/s Total Cases
  
  mod <- lm(countryDeaths ~ countryCases)
  
  tmpDF <- data.frame(countryDeaths, countryCases)
  
  tmpDF <- predict(mod, tmpDF)
  
  tmpDF <- as.data.frame(tmpDF)
  
  for(index in 1:length(countryDeaths)) {
    if(is.na(countryDeaths[index])) {
      countryDeaths[index] = tmpDF[index, 1]
    }
  }
  
  # countryDeaths <- tmpDF[,1]
  
  mod <- lm(countryDeaths ~ countryCases)
  
  cat("Summary of Linear Regression Model for the case - Deaths v/s Total Cases")
  summary(mod)
  png(file = paste(subDirectory,"_Deaths_Cases_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryDeaths, main=paste("Deaths v/s Cases in", countryCode, "after Preprocessing", sep=" "), xlab = "Total Cases", ylab = "Total Deaths")
  
  abline(mod, col=2, lwd = 3)
  dev.off()
  
  # Vaccinations v/s Deaths
  
  mod1 <- lm(countryVaccinations ~ countryDeaths)
  
  tmpDF <- data.frame(countryVaccinations, countryDeaths)
  
  tmpDF <- predict(mod1, tmpDF)
  
  tmpDF <- as.data.frame(tmpDF)
  
  for(index in 1:length(countryVaccinations)) {
    if(is.na(countryVaccinations[index])) {
      countryVaccinations[index] = tmpDF[index, 1]
    }
  }
  
  mod1 <- lm(countryVaccinations ~ countryDeaths)
  
  cat("Summary of Linear Regression Model for the case - Vaccinations v/s Total Deaths")
  summary(mod1)
  
  png(file = paste(subDirectory,"_Vaccinations_Deaths_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryDeaths, countryVaccinations, main=paste("Vaccinations v/s Deaths in", countryCode, "after Preprocessing", sep=" "), xlab = "Total Deaths", ylab = "Total Vaccinations")
  
  abline(mod1, col=2, lwd = 3)
  dev.off()
  
  # Tests v/s Cases
  
  mod2 <- lm(countryTests ~ countryCases)
  
  tmpDF <- data.frame(countryTests, countryCases)
  
  tmpDF <- predict(mod2, tmpDF)
  
  tmpDF <- as.data.frame(tmpDF)
  
  for(index in 1:length(countryTests)) {
    if(is.na(countryTests[index])) {
      countryTests[index] = tmpDF[index, 1]
    }
  }
  
  mod2 <- lm(countryTests ~ countryCases)
  
  cat("Summary of Linear Regression Model for the case - Tests v/s Cases")
  summary(mod2)
  
  png(file = paste(subDirectory,"_Tests_Cases_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryTests, main=paste("Tests v/s Cases in", countryCode, "after Preprocessing", sep=" "), xlab = "Total Cases", ylab = "Total Tests")
  
  abline(mod2, col=2, lwd = 3)
  dev.off()
  
  # Deaths v/s Timeline
  
  mod3 <- lm(countryDeaths ~ days)
  
  tmpDF <- data.frame(countryDeaths, days)
  
  tmpDF <- predict(mod3, tmpDF)
  
  tmpDF <- as.data.frame(tmpDF)
  
  for(index in 1:length(countryDeaths)) {
    if(is.na(countryDeaths[index])) {
      countryDeaths[index] = tmpDF[index, 1]
    }
  }
  
  mod3 <- lm(countryDeaths ~ days)
  
  cat("Summary of Linear Regression Model for the case - Deaths v/s Time")
  summary(mod3)
  
  png(file = paste(subDirectory,"_Deaths_Days_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(days, countryDeaths, main=paste("Deaths v/s Days in", countryCode, "after Preprocessing", sep=" "), xlab = "Days", ylab = "Total Deaths")
  
  abline(mod3, col=2, lwd = 3)
  dev.off()
  
  # Stringency Index v/s Cases 
  
  # countryData$total_cases[is.na(countryData$total_cases)] = 0
  
  mod4 <- lm(countryStringencyIndex ~ countryCases)
  
  tmpDF <- data.frame(countryStringencyIndex, countryCases)
  
  tmpDF <- predict(mod4, tmpDF)
  
  tmpDF <- as.data.frame(tmpDF)
  
  for(index in 1:length(countryStringencyIndex)) {
    if(is.na(countryStringencyIndex[index])) {
      countryStringencyIndex[index] = tmpDF[index, 1]
    }
  }
  
  mod4 <- lm(countryStringencyIndex ~ countryCases)
  
  cat("Summary of Linear Regression Model for the case - Stringency Index v/s Cases")
  summary(mod4)

  png(file = paste(subDirectory,"_StringencyIndex_Cases_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryStringencyIndex, type="l", lwd="2", main=paste("Stringency Index v/s Cases in", countryCode, "after Preprocessing", sep=" "), xlab = "Total Cases", ylab = "Stringency Index")
  
  abline(mod4, col=2, lwd = 3)
  dev.off()
}