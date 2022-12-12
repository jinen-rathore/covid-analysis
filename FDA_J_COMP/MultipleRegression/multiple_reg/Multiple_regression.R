# this is the multiple regression model for the covid analysis


countris <- c("IND","CUB","IDN","IRQ","POL","UKR")

for ( nam in countris)
{ 
  par(mar=c(1,1,1,1))
  filename <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code - github/FDA_J_COMP/countries/",nam,".csv",sep="");
  dataset <- read.csv(filename, stringsAsFactors = T)
  dataset <- as.data.frame(dataset)
  
  countryCases <- dataset$total_cases
  countryVaccinations <- dataset$total_vaccinations
  countryTests <- dataset$total_tests
  countryStringencyIndex <- dataset$stringency_index
  countryDeaths <- dataset$total_deaths
  
  cat("For the Country :: ",nam,"\n");
  # Cases Vs ( Tests + Vaccinations + StringencyIndex )
  cat("Relation b/n Cases & ( tests , vaccinations )\n");
  library(car)
  mod <- lm(countryCases ~ countryTests + countryVaccinations , data = dataset)
  
  print(mod)
  
  ofile1 <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code - github/FDA_J_COMP/MultipleRegression/multiple_reg/Cases/",nam,".jpeg",sep="")
  jpeg(file = ofile1 ,width=600,height=400)
  avPlots(mod)
  dev.off()
  
  # Deaths Vs ( Cases + Vaccinations + StringencyIndex)
  cat("Relation b/n Deaths & ( Cases , vaccinations )\n");
  mod <- lm(countryDeaths ~ countryCases + countryVaccinations, data = dataset)
  
  print(mod)
  
  ofile2 <- paste("D:/My_Stuff/VIT-20BCE1789/Sem 5/Materials/FDA/Project/Code - github/FDA_J_COMP/MultipleRegression/multiple_reg/Deaths/",nam,".jpeg",sep="")
  jpeg(file = ofile2 ,width=600,height=400)
  
  avPlots(mod)
  
  dev.off()

}