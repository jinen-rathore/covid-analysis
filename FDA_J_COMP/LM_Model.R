countryData <- read.csv("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code/FDA_J_COMP/countries/IND.csv")
countryVaccinations <- countryData$total_vaccinations
countryCases <- countryData$total_cases
countryDeaths <- countryData$total_deaths
countryTests <- countryData$total_tests
countryStringencyIndex <- countryData$stringency_index
countryNewCases <- countryData$new_cases
days <- c(1:length(countryData$date))

# countryData[is.na(countryData)] = 0 #to make the NA values 0

# Deaths v/s Cases

mod <- lm(countryDeaths ~ countryCases)

summary(mod)

plot(countryCases, countryDeaths, main="Deaths v/s Cases", xlab = "Total Deaths", ylab = "Total Cases")

abline(mod, col=2, lwd = 3)

# Vaccinations v/s Deaths

mod1 <- lm(countryVaccinations ~ countryDeaths)

summary(mod1)

plot(countryDeaths, countryVaccinations, main="Vaccinations v/s Deaths", xlab = "Total Deaths", ylab = "Total Vaccinations")

abline(mod1, col=2, lwd = 3)

# Tests v/s Cases

mod2 <- lm(countryTests ~ countryCases)

summary(mod2)

plot(countryCases, countryTests, main="Tests v/s Cases", xlab = "Total Cases", ylab = "Total Tests")

abline(mod2, col=2, lwd = 3)

# Deaths v/s  Timeline?

mod3 <- lm(countryDeaths ~ days)

summary(mod3)

plot(days, countryDeaths, main="Deaths v/s Days", xlab = "Days", ylab = "Total Deaths")

abline(mod3, col=2, lwd = 3)

# Stringency Index v/s Cases 

countryData$total_cases[is.na(countryData$total_cases)] = 0

mod4 <- lm(countryStringencyIndex ~ countryCases)

summary(mod4)

plot(countryCases, countryStringencyIndex, type="l", lwd="2", main="Stringency Index v/s Cases", xlab = "Total Cases", ylab = "Stringency Index")

abline(mod4, col=2, lwd = 3)