# Rajat Mishra, 20BCE1251

library(hydroGOF)
library(e1071)

countries = list("AUS", "CUB", "IND", "FRA", "ZAF", "USA")


csv_dir = "/home/rajat/Rajat/VIT/5thSem_Fall22-23/CSE3505-FDA/archive/countries/"
plt_dir = "/home/rajat/Rajat/VIT/5thSem_Fall22-23/CSE3505-FDA/archive/SupportVectorRegression/plots_processed/"

for (ct in countries) {
    print(ct)
    
    data = read.csv(paste(csv_dir, ct, ".csv", sep = ""))
    subset = subset(data, select = c(total_cases, new_cases, total_deaths, new_deaths, total_tests, positive_rate, total_vaccinations, new_vaccinations, stringency_index))
    subset_orig = subset
    
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
    
    ####### Processing #######
    
    total_cases_orig = subset_orig$total_cases
    new_cases_orig = subset_orig$new_cases
    total_deaths_orig = subset_orig$total_deaths
    new_deaths_orig = subset_orig$new_deaths
    total_tests_orig = subset_orig$total_tests
    positive_rate_orig = subset_orig$positive_rate
    total_vaccinations_orig = subset_orig$total_vaccinations
    new_vaccinations_orig = subset_orig$new_vaccinations
    stringency_index_orig = subset_orig$stringency_index
    
    for (i in 1:25) {
        if (is.na(total_cases_orig[i])) total_cases_orig[i] = 0
        if (is.na(new_cases_orig[i])) new_cases_orig[i] = 0
        if (is.na(total_deaths_orig[i])) total_deaths_orig[i] = 0
        if (is.na(new_deaths_orig[i])) new_deaths_orig[i] = 0
        if (is.na(total_tests_orig[i])) total_tests_orig[i] = 0
        if (is.na(positive_rate_orig[i])) positive_rate_orig[i] = 0
        if (is.na(total_vaccinations_orig[i])) total_vaccinations_orig[i] = 0
        if (is.na(new_vaccinations_orig[i])) new_vaccinations_orig[i] = 0
        if (is.na(stringency_index_orig[i])) stringency_index_orig[i] = 0
    }
    
    for (i in 25:(length(new_cases_orig) - 25)) {
        if (is.na(new_cases_orig[i])) {
            val = (mean(na.omit(new_cases_orig[(i-25):(i-1)])) + mean(na.omit(new_cases_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            new_cases_orig[i] = val
        }
    }
    
    for (i in 25:(length(total_cases_orig) - 25)) {
        if (is.na(total_cases_orig[i])) {
            val = (mean(na.omit(total_cases_orig[(i-25):(i-1)])) + mean(na.omit(total_cases_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            total_cases_orig[i] = val
        }
    }
    
    for (i in 25:(length(total_deaths_orig) - 25)) {
        if (is.na(total_deaths_orig[i])) {
            val = (mean(na.omit(total_deaths_orig[(i-25):(i-1)])) + mean(na.omit(total_deaths_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            total_deaths_orig[i] = val
        }
    }
    
    for (i in 25:(length(new_deaths_orig) - 25)) {
        if (is.na(new_deaths_orig[i])) {
            val = (mean(na.omit(new_deaths_orig[(i-25):(i-1)])) + mean(na.omit(new_deaths_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            new_deaths_orig[i] = val
        }
    }
    
    for (i in 25:(length(total_tests_orig) - 25)) {
        if (is.na(total_tests_orig[i])) {
            val = (mean(na.omit(total_tests_orig[(i-25):(i-1)])) + mean(na.omit(total_tests_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            total_tests_orig[i] = val
        }
    }
    
    for (i in 25:(length(positive_rate_orig) - 25)) {
        if (is.na(positive_rate_orig[i])) {
            val = (mean(na.omit(positive_rate_orig[(i-25):(i-1)])) + mean(na.omit(positive_rate_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            positive_rate_orig[i] = val
        }
    }
    
    for (i in 25:(length(total_vaccinations_orig) - 25)) {
        if (is.na(total_vaccinations_orig[i])) {
            val = (mean(na.omit(total_vaccinations_orig[(i-25):(i-1)])) + mean(na.omit(total_vaccinations_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            total_vaccinations_orig[i] = val
        }
    }
    
    for (i in 25:(length(new_vaccinations_orig) - 25)) {
        if (is.na(new_vaccinations_orig[i])) {
            val = (mean(na.omit(new_vaccinations_orig[(i-25):(i-1)])) + mean(na.omit(new_vaccinations_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            new_vaccinations_orig[i] = val
        }
    }
    
    for (i in 25:(length(stringency_index_orig) - 25)) {
        if (is.na(stringency_index_orig[i])) {
            val = (mean(na.omit(stringency_index_orig[(i-25):(i-1)])) + mean(na.omit(stringency_index_orig[(i+1):(i+25)]))) / 2
            if (is.nan(val) | is.na(val)) val = 0
            stringency_index_orig[i] = val
        }
    }
    
    ############################################################################
    
    # total deaths vs. days
    
    X = days
    Y = total_deaths
    data1 = data.frame(X, Y)
    
    model = lm(data1$Y ~ data1$X, data1)
    pred <- predict(model, data1)
    RMSE = rmse(pred, data1$Y)
    
    modelsvm = svm(data1$Y ~ data1$X, data1)
    predYsvm = predict(modelsvm, data1)
    
    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data1$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    
    print(paste("Total Deaths v. Days pre-processing:", txt))
    
    ###### Post-Processing ######
    
    X = days
    Y = total_deaths_orig
    data1 = data.frame(X, Y)
    
    model = lm(data1$Y ~ data1$X, data1)
    pred <- predict(model, data1)
    RMSE = rmse(pred, data1$Y)
    
    png(file = paste(plt_dir, ct, "_TotDeaths_Days.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data1, main = paste(ct, "Total Deaths v. Days post_processing", sep = ": "), type="l", lwd = 3, xlab = "Days", ylab = "Total Deaths")
    modelsvm = svm(data1$Y ~ data1$X, data1)
    predYsvm = predict(modelsvm, data1)
    
    points(data1$X, predYsvm, col = "red", pch = 3)
    
    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data1$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("Total Deaths v. Days post-processing:", txt))
    
    
    ############################################################################
    
    # total Cases vs. total vaccinations
    X = total_vaccinations
    Y = total_cases
    data2 = data.frame(X, Y)

    model = lm(data2$Y ~ data2$X, data2)
    pred <- predict(model, data2)
    RMSE = rmse(pred, data2$Y)

    modelsvm = svm(data2$Y ~ data2$X, data2)
    predYsvm = predict(modelsvm, data2)

    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data2$Y)

    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    
    print(paste("Total Vaxx v. Cases pre-processing:", txt))
    
    ###### Post-Processing ######
    
    X = total_vaccinations_orig
    Y = total_cases_orig
    data2 = data.frame(X, Y)
    
    model = lm(data2$Y ~ data2$X, data2)
    pred <- predict(model, data2)
    RMSE = rmse(pred, data2$Y)
    
    png(file = paste(plt_dir, ct, "_Cases_Vaccinations.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data2, main = paste(ct, "total Cases v. total Vaccinations post-processing", sep = ": "), type="l", lwd = 3, xlab = "Total Vaccinations", ylab = "Total Cases")
    modelsvm = svm(data2$Y ~ data2$X, data2)
    predYsvm = predict(modelsvm, data2)
    
    points(data2$X[1:length(predYsvm)], predYsvm, col = "red", pch = 3)
    
    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data2$Y[1:length(predYsvm)])
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("Total Vaxx v. Cases post-processing:", txt))
    
    ############################################################################

    # New Cases vs. Days
    X = days
    Y = new_cases
    data3 = data.frame(X, Y)

    model = lm(data3$Y ~ data3$X, data3)
    pred <- predict(model, data3)
    RMSE = rmse(pred, data3$Y)
    
    modelsvm = svm(data3$Y ~ data3$X, data3)
    predYsvm = predict(modelsvm, data3)
    
    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data3$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    
    print(paste("New Cases vs Days pre-processing:", txt))
    
    ###### Post-Processing ######
    
    X = days
    Y = new_cases_orig
    data3 = data.frame(X, Y)
    
    model = lm(data3$Y ~ data3$X, data3)
    pred <- predict(model, data3)
    RMSE = rmse(pred, data3$Y)

    png(file = paste(plt_dir, ct, "_NewCases_Days.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data3, main = paste(ct, "New Cases v. Days post_processing", sep = ": "), type="l", lwd = 3, xlab = "Days", ylab = "New Cases")
    modelsvm = svm(data3$Y ~ data3$X, data3)
    predYsvm = predict(modelsvm, data3)

    points(data3$X, predYsvm, col = "red", pch = 3)

    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data3$Y)

    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("New Cases vs Days post_processing:", txt))
    
    ############################################################################
    
    # Stringency Index vs. Days
    normalize <- function(x) {
        return (((x - min(x)) / (max(x) - min(x))) * 100)
    }
    new_cases_norm <- normalize(new_cases_orig)

    X = days
    Y = stringency_index
    data4 = data.frame(X, Y)

    model = lm(data4$Y ~ data4$X, data4)
    pred <- predict(model, data4)
    RMSE = rmse(pred, data4$Y)
    
    modelsvm = svm(data4$Y ~ data4$X, data4)
    predYsvm = predict(modelsvm, data4)
    
    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data4$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    
    print(paste("Stringency Index vs Days pre-processing:", txt))
    
    ###### Post-Processing ######
    
    X = days
    Y = stringency_index_orig
    data4 = data.frame(X, Y)
    
    model = lm(data4$Y ~ data4$X, data4)
    pred <- predict(model, data4)
    RMSE = rmse(pred, data4$Y)

    png(file = paste(plt_dir, ct, "_StringencyIdx_Days.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data4, main = paste(ct, "Stringency Index v. Days post-processing\n(New cases in blue normalised to 100)", sep = ": "), type="l", lwd = 3, xlab = "Days", ylab = "Stringency Index", ylim = c(0, 100))
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
    print(paste("Stringency Index vs Days post-processing:", txt))

    ############################################################################

    # Positivity rate vs. Vaccinations
    X = total_vaccinations
    Y = positive_rate
    data5 = data.frame(X, Y)

    model = lm(data5$Y ~ data5$X, data5)
    pred <- predict(model, data5)
    RMSE = rmse(pred, data5$Y)
    
    modelsvm = svm(data5$Y ~ data5$X, data5)
    predYsvm = predict(modelsvm, data5)
    
    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data5$Y)
    
    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    
    print(paste("Positivity Rate vs. Total Vaccinations pre-processing:", txt))
    
    ###### Post-Processing ######
    
    X = total_vaccinations_orig
    Y = positive_rate_orig
    data5 = data.frame(X, Y)
    
    model = lm(data5$Y ~ data5$X, data5)
    pred <- predict(model, data5)
    RMSE = rmse(pred, data5$Y)

    png(file = paste(plt_dir, ct, "_PosRate_TotVax.png", sep=""), width = 1080, height = 1080, units = "px", pointsize = 24)
    plot(data5, main = paste(ct, "Positivity Rate vs. Total Vaccinations post_processing", sep = ": "), pch = 20, ylab = "Positivity Rate", xlab = "Total Vaccinations")
    modelsvm = svm(data5$Y ~ data5$X, data5)
    predYsvm = predict(modelsvm, data5)

    points(data5$X[1:length(predYsvm)], predYsvm, col = "red", pch = 3)

    W = t(modelsvm$coefs) %*% modelsvm$SV
    b = modelsvm$rho
    RMSEsvm = rmse(predYsvm, data5$Y[1:length(predYsvm)])

    txt = paste("W =" , W, ", b =" , b, ", RMSE =" , RMSEsvm)
    mtext(txt, side = 3)
    dev.off()
    print(paste("Positivity Rate vs. Total Vaccinations post-processing:", txt))
    
    ############################################################################
}

