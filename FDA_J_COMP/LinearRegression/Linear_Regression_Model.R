countryCodes = list("IND", "CUB", "IDN", "IRQ", "POL", "UKR")

for(countryCode in countryCodes) {
  subDirectory <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code/FDA_J_COMP/LinearRegression/",countryCode)
  path <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code/FDA_J_COMP/countries/", countryCode, ".csv", sep ="")
  countryData <- read.csv(path)
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
  
  summary(mod)
  png(file = paste(subDirectory,"_Deaths_Cases.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryDeaths, main=paste("Deaths v/s Cases in", countryCode, sep=" "), xlab = "Total Cases", ylab = "Total Deaths")
  
  abline(mod, col=2, lwd = 3)
  dev.off()
  
  # Vaccinations v/s Deaths
  
  mod1 <- lm(countryVaccinations ~ countryDeaths)
  
  summary(mod1)
  
  png(file = paste(subDirectory,"_Vaccinations_Deaths.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryDeaths, countryVaccinations, main=paste("Vaccinations v/s Deaths in", countryCode, sep=" "), xlab = "Total Deaths", ylab = "Total Vaccinations")
  
  abline(mod1, col=2, lwd = 3)
  dev.off()
  
  # Tests v/s Cases
  
  mod2 <- lm(countryTests ~ countryCases)
  
  summary(mod2)
  
  png(file = paste(subDirectory,"_Tests_Cases.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryTests, main=paste("Tests v/s Cases in", countryCode, sep=" "), xlab = "Total Cases", ylab = "Total Tests")
  
  abline(mod2, col=2, lwd = 3)
  dev.off()
  
  # Deaths v/s Timeline
  
  mod3 <- lm(countryDeaths ~ days)
  
  summary(mod3)
  
  png(file = paste(subDirectory,"_Deaths_Days.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(days, countryDeaths, main=paste("Deaths v/s Days in", countryCode, sep=" "), xlab = "Days", ylab = "Total Deaths")
  
  abline(mod3, col=2, lwd = 3)
  dev.off()
  
  # Stringency Index v/s Cases 
  
  countryData$total_cases[is.na(countryData$total_cases)] = 0
  
  mod4 <- lm(countryStringencyIndex ~ countryCases)
  
  summary(mod4)

  png(file = paste(subDirectory,"_StringencyIndex_Cases.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
  plot(countryCases, countryStringencyIndex, type="l", lwd="2", main=paste("Stringency Index v/s Cases in", countryCode, sep=" "), xlab = "Total Cases", ylab = "Stringency Index")
  
  abline(mod4, col=2, lwd = 3)
  dev.off()
}