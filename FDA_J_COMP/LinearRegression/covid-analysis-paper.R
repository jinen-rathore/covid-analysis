countryCodes = list("IND", "USA")

for(countryCode in countryCodes) {
    subDirectory <- paste("./PaperPlots/", countryCode)
    path <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code/FDA_J_COMP/countries/", countryCode, ".csv", sep ="")
    countryData <- read.csv(path)

    # New Cases v/s Days

    mod1 <- lm(countryNewCases ~ days)

    # error analysis
    print(paste("Error Analysis for newCases vs Days - ", countryCode))
    library(zoo)
    library(hydroGOF)

    predict(mod1)
    countryNewCases

    gof(predict(mod1), countryNewCases)

    png(file = paste(subDirectory,"_NewCases_Days.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(days, countryNewCases, type="l", lwd=3.0, main=paste("New Cases v/s Days in", countryCode, sep=" "), xlab = "Days", ylab = "New Cases")

    abline(mod1, col=2, lwd = 3)
    dev.off()


    # Deaths v/s Timeline

    mod2 <- lm(countryDeaths ~ days)

    # error analysis
    print("Error Analysis for total deaths vs Days - ", countryCode)
    library(zoo)
    library(hydroGOF)

    predict(mod2)

    gof(predict(mod2), countryDeaths)



    # cat("Summary of Linear Regression Model for the case - Deaths v/s Time")
    # summary(mod2)
    # 
    # matrix_coef <- summary(mod2)$coefficients  # Extract coefficients in matrix
    # matrix_coef 
    # 
    # my_estimates <- matrix_coef[ , 1]                     # Matrix manipulation to extract estimates
    # my_estimates

    png(file = paste(subDirectory,"_Deaths_Days_PP.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(days, countryDeaths, type="l", lwd=3.0, main=paste("Deaths v/s Days in", countryCode, sep=" "), xlab = "Days", ylab = "Total Deaths")

    abline(mod2, col=2, lwd = 3)
    dev.off()


    # Total Vacc v/s Total Cases

    mod3 <- lm(countryVaccinations ~ countryCases)

    # error analysis
    print("Error Analysis for totVax vs total cases - ", countryCode)
    library(zoo)
    library(hydroGOF)

    length(predict(mod3))

    gof(predict(mod3), countryVaccinations[300:922])

    png(file = paste(subDirectory,"_TotalVax_TotalCases.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(countryCases, countryVaccinations, type="l", lwd=3.0, main=paste("Total Vaccinations v/s Total Cases in", countryCode, sep=" "), xlab = "Total Cases", ylab = "Total Vaccinations")

    abline(mod3, col=2, lwd = 3)
    dev.off()
}