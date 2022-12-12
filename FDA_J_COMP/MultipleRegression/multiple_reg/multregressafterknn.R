#data cleaning strategies

# we will be applying the KNN algorithm
library(car)
library(caret)
library(e1071)
library(hydroGOF)
library(dplyr)
countris <- c("IND","USA")

for ( nam in countris)
{ 
  par(mar=c(1,1,1,1))
  filename <- paste("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/countries/",nam,".csv",sep="");
  dataset <- read.csv(filename, stringsAsFactors = T)
  dataset <- as.data.frame(dataset)
  
  countryCases <- dataset$total_cases
  countryVaccinations <- dataset$total_vaccinations
  countryTests <- dataset$total_tests
  countryDeaths <- dataset$total_deaths
  #clean the total_cases collumn

  for (i in 25:(length(countryCases) - 25)) {
    if (is.na(countryCases[i])) {
      val = (mean(na.omit(countryCases[(i-25):(i-1)])) + mean(na.omit(countryCases[(i+1):(i+25)]))) / 2
      countryCases[i] = val
    }
  }
  
  #clean the total_deaths collumn

  for (i in 1:25) {
    if (is.na(countryDeaths[i])) countryDeaths[i] = 0
  }
  for (i in 25:(length(countryDeaths) - 25)) {
    if (is.na(countryDeaths[i])) {
      val = (mean(na.omit(countryDeaths[(i-25):(i-1)])) + mean(na.omit(countryDeaths[(i+1):(i+25)]))) / 2
      countryDeaths[i] = val
    }
  }
  
  #clean the total_tests_ collumn

  for (i in 1:25) {
    if (is.na(countryTests[i])) countryTests[i] = 0
  }
  for (i in 25:(length(countryTests))) {
    if (is.na(countryTests[i])) {
      val = (mean(na.omit(countryTests[(i-25):(i-1)]))+ mean(na.omit(countryTests[(i+1):(i+25)]))) / 2
      countryTests[i] = val
    }
  }
  
  #clean the total vaccinations collumn

  for (i in 1:25) {
    if (is.na(countryVaccinations[i])) 
      {
      countryVaccinations[i] = 0
    }
  }
  for (i in 25:(length(countryVaccinations))) {
    if (is.na(countryVaccinations[i])) {
      val = (mean(na.omit(countryVaccinations[(i-25):(i-1)]))+ mean(na.omit(countryVaccinations[(i+1):(i+25)]))) / 2
      countryVaccinations[i] = val
    }
  }
  cat("For the Country :: ",nam,"\n");
  # Cases Vs ( Tests + Vaccinations )
  cat("Relation b/n Cases & ( tests , vaccinations )\n");
  mod <- lm(total_cases ~ total_tests + total_vaccinations , data = dataset)
  print(mod)
  dataset<-mutate(dataset, pred_cases = mod$coefficients[1]+total_tests*mod$coefficients[2]+total_vaccinations*mod$coefficients[3])
  #cat("COeff-1 ->",mod$coefficients[1],"\n");
  #cat("COeff-2 ->",mod$coefficients[2],"\n");
  #cat("COeff-3 ->",mod$coefficients[3],"\n");
  #print(dataset$pred_cases)
  cat("RMSE :: ",rmse(dataset$pred_cases,dataset$total_cases,na.rm=T),"\n")
  cat("MSE :: ",mse(dataset$pred_cases,dataset$total_cases,na.rm = T),"\n")
  cat("MAE :: ",mae(dataset$pred_cases,dataset$total_cases,na.rm=T),"\n")
  # cat("R^2 :: ",R2(dataset$pred_cases,dataset$total_cases ,na.rm=T),"\n")
  ofile1 <- paste("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/visualization/multiple_reg_paper/Cases/",nam,".jpeg",sep="")
  jpeg(file = ofile1 ,width=600,height=400)
  avPlots(mod)
  dev.off()
  
  # Deaths Vs ( Cases + Vaccinations )
  cat("Relation b/n Deaths & ( Cases , vaccinations )\n");
  mod <- lm(total_deaths ~ total_cases + total_vaccinations, data = dataset)
  print(mod)
  dataset<-mutate(dataset, pred_deaths = mod$coefficients[1]+total_cases*mod$coefficients[2]+total_vaccinations*mod$coefficients[3])
  cat("RMSE :: ",rmse(dataset$pred_deaths,dataset$total_deaths,na.rm=T),"\n")
  cat("MSE :: ",mse(dataset$pred_deaths,dataset$total_deaths,na.rm = T),"\n")
  cat("MAE :: ",mae(dataset$pred_deaths,dataset$total_deaths,na.rm=T),"\n")
  cat("R^2 :: ",R2(dataset$pred_deaths,dataset$total_deaths ,na.rm=T),"\n")
  ofile2 <- paste("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/visualization/multiple_reg_paper/Deaths/",nam,".jpeg",sep="")
  jpeg(file = ofile2 ,width=600,height=400)
  avPlots(mod)
  dev.off()
  
}