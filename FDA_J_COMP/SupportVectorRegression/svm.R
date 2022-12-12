# Rajat Mishra, 20BCE1251

library(hydroGOF)
library(e1071)

countries = list("AUS", "CUB", "IND", "FRA", "ZAF", "USA")
csv_dir = "D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code/FDA_J_COMP/countries/"
plt_dir = "./SupportVectorRegression/plots_post-processing/"

for (ct in countries) {
    print(ct)
    
    data = read.csv(paste(csv_dir, ct, ".csv", sep = ""))
    subset = subset(data, select = c(total_cases, new_cases, total_deaths, new_deaths, total_tests, positive_rate, total_vaccinations, new_vaccinations, stringency_index))
    
    for (r in 1:nrow(subset)) {
        for (c in 1:ncol(subset)) {
            if (is.na(subset[r, c]))
                subset[r, c] = 0
        }
    }
    
    total_cases = subset$total_cases
    new_cases = subset$new_cases
    total_deaths = subset$total_deaths
    new_deaths = subset$new_deaths
    total_tests = subset$total_tests
    positive_rate = subset$positive_rate
    total_vaccinations = subset$total_vaccinations
    new_vaccinations = subset$new_vaccinations
    stringency_index = subset$stringency_index
    days = c(1:nrow(subset))
    
    # Total Deaths vs. Days
    X = days
    Y = total_deaths
    data1 = data.frame(X, Y)
    
    model = lm(data1$Y ~ data1$X, data1)
    pred <- predict(model, data1)
    RMSE = rmse(pred, data1$Y)
    
    png(file = paste(plt_dir, ct, "_TotDeaths_Days.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data1, main = paste(ct, "Total Deaths v. Days", sep = ": "), type="l", lwd = 3, xlab = "Days", ylab = "Total Deaths")
    modelsvm = svm(data1$Y ~ data1$X, data1)
    predYsvm = predict(modelsvm, data1)
    
    points(data1$X, predYsvm, col = "red", pch = 3)
    
    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data1$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("Total Deaths v. Days:", txt))
    ############################################################################
    
    # total Cases vs. total vaccinations
    X = total_vaccinations
    Y = total_cases
    data2 = data.frame(X, Y)
    
    model = lm(data2$Y ~ data2$X, data2)
    pred <- predict(model, data2)
    RMSE = rmse(pred, data2$Y)
    
    png(file = paste(plt_dir, ct, "_Cases_Vaccinations.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data2, main = paste(ct, "total Cases v. total Vaccinations", sep = ": "), type="l", lwd = 3, xlab = "Total Vaccinations", ylab = "Total Cases")
    modelsvm = svm(data2$Y ~ data2$X, data2)
    predYsvm = predict(modelsvm, data2)
    
    points(data2$X, predYsvm, col = "red", pch = 3)

    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data2$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("Total Vaxx v. Cases:", txt))
    ############################################################################
    
    # New Cases vs. Days
    X = days
    Y = new_cases
    data3 = data.frame(X, Y)
    
    model = lm(data3$Y ~ data3$X, data3)
    pred <- predict(model, data3)
    RMSE = rmse(pred, data3$Y)
    
    png(file = paste(plt_dir, ct, "_NewCases_Days.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data3, main = paste(ct, "New Cases v. Days", sep = ": "), type="l", lwd = 3, xlab = "Days", ylab = "New Cases")
    modelsvm = svm(data3$Y ~ data3$X, data3)
    predYsvm = predict(modelsvm, data3)
    
    points(data3$X, predYsvm, col = "red", pch = 3)

    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data3$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("New Cases vs Days:", txt))
    ############################################################################
    
    # New Cases vs. Days
    normalize <- function(x) {
        return (((x - min(x)) / (max(x) - min(x))) * 100)
    }
    new_cases_norm <- normalize(new_cases)

    X = days
    Y = stringency_index
    data4 = data.frame(X, Y)
    
    model = lm(data4$Y ~ data4$X, data4)
    pred <- predict(model, data4)
    RMSE = rmse(pred, data4$Y)
    
    png(file = paste(plt_dir, ct, "_StringencyIdx_Days.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data4, main = paste(ct, "Stringency Index v. Days\n(New cases in blue normalised to 100)", sep = ": "), type="l", lwd = 3, xlab = "Days", ylab = "Stringency Index", ylim = c(0, 100))
    modelsvm = svm(data4$Y ~ data4$X, data4)
    predYsvm = predict(modelsvm, data4)
    
    points(data4$X, predYsvm, col = "red", pch = 3)
    lines(data4$X, new_cases_norm, col = "blue", lwd = 2)
    
    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data4$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("Stringency Index vs Days:", txt))
    ############################################################################
    
    # Positivity rate vs. Vaccinations
    X = total_vaccinations
    Y = positive_rate
    data5 = data.frame(X, Y)
    
    model = lm(data5$Y ~ data5$X, data5)
    pred <- predict(model, data5)
    RMSE = rmse(pred, data5$Y)
    
    png(file = paste(plt_dir, ct, "_PosRate_TotVax.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data5, main = paste(ct, "Positivity Rate vs. Total Vaccinations", sep = ": "), pch = 20, ylab = "Positivity Rate", xlab = "Total Vaccinations")
    modelsvm = svm(data5$Y ~ data5$X, data5)
    predYsvm = predict(modelsvm, data5)
    
    points(data5$X, predYsvm, col = "red", pch = 3)

    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data5$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("Positivity Rate vs. Total Vaccinations:", txt))
    ############################################################################
}

