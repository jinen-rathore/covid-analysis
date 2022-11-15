#data cleaning strategies

# we will be applying the KNN algorithm

countris <- c("IND","CUB","IDN","IRQ","POL","UKR")

for ( nam in countris)
{ 
  par(mar=c(1,1,1,1))
  filename <- paste("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/countries/",nam,".csv",sep="");
  dataset <- read.csv(filename, stringsAsFactors = T)
  dataset <- as.data.frame(dataset)
  #clean the total_cases collumn
  cc<-0;
  itr <-1
  while(is.null(dataset$total_cases[itr]))
  {
    cc<-cc+1;
    itr->itr+1;
  }
  for (i in 1:cc) {
    if (is.null(dataset$total_cases[i])) dataset$total_cases[i] = 0
  }
  for (i in cc:(length(dataset$total_cases) - 25)) {
    if (is.null(dataset$total_cases[i])) {
      val = (mean(na.omit(dataset$total_cases[(i-25):(i-1)])) + mean(na.omit(dataset$total_cases[(i+1):(i+25)]))) / 2
      dataset$total_cases[i] = val
    }
  }
  
  #clean the total_deaths collumn
  dc<-0;
  itr <-1
  while(is.null(dataset$total_deaths[itr]))
  {
    dc<-dc+1;
    itr->itr+1;
  }
  for (i in 1:dc) {
    if (is.null(dataset$total_deaths[i])) dataset$total_deaths[i] = 0
  }
  for (i in dc:(length(dataset$total_deaths) - 25)) {
    if (is.null(dataset$total_deaths[i])) {
      val = (mean(na.omit(dataset$total_deaths[(i-25):(i-1)])) + mean(na.omit(dataset$total_deaths[(i+1):(i+25)]))) / 2
      dataset$total_deaths[i] = val
    }
  }
  
  #clean the total_tests_ collumn
  itr <-1
  tc<-0
  while(is.null(dataset$total_tests[itr]))
  {
    tc<-tc+1;
    itr->itr+1;
  }
  for (i in 1:tc) {
    if (is.null(dataset$total_tests[i])) dataset$total_tests[i] = 0
  }
  for (i in tc:(length(dataset$total_tests) - 25)) {
    if (is.null(dataset$total_tests[i])) {
      val = (mean(na.omit(dataset$total_tests[(i-25):(i-1)])) + mean(na.omit(dataset$total_tests[(i+1):(i+25)]))) / 2
      dataset$total_tests[i] = val
    }
  }
  
  #clean the total vaccinations collumn
  itr <-1
  vc<-0
  while(is.null(dataset$total_vaccinations[itr]))
  {
    vc<-vc+1;
    itr->itr+1;
  }
  for (i in 1:vc) {
    if (is.null(dataset$total_vaccinations[i])) 
      {
      dataset$total_vaccinations[i] = 0
    }
  }
  for (i in vc:(length(dataset$total_vaccinations) - 25)) {
    if (is.null(dataset$total_vaccinations[i])) {
      val = (mean(na.omit(dataset$total_vaccinations[(i-25):(i-1)])) + mean(na.omit(dataset$total_vaccinations[(i+1):(i+25)]))) / 2
      dataset$total_vaccinations[i] = val
    }
  }
  
  
  countryCases <- dataset$total_cases
  countryVaccinations <- dataset$total_vaccinations
  countryTests <- dataset$total_tests
  countryDeaths <- dataset$total_deaths
  
  cat("For the Country :: ",nam,"\n");
  # Cases Vs ( Tests + Vaccinations )
  cat("Relation b/n Cases & ( tests , vaccinations )\n");
  library(car)
  mod <- lm(countryCases ~ countryTests + countryVaccinations , data = dataset)
  
  print(mod)
  
  ofile1 <- paste("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/visualization/multiple_reg_after_impute/Cases/",nam,".jpeg",sep="")
  jpeg(file = ofile1 ,width=600,height=400)
  avPlots(mod)
  dev.off()
  
  # Deaths Vs ( Cases + Vaccinations )
  cat("Relation b/n Deaths & ( Cases , vaccinations )\n");
  mod <- lm(countryDeaths ~ countryCases + countryVaccinations, data = dataset)
  
  print(mod)
  
  ofile2 <- paste("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/visualization/multiple_reg_after_impute/Deaths/",nam,".jpeg",sep="")
  jpeg(file = ofile2 ,width=600,height=400)
  
  avPlots(mod)
  
  dev.off()
  
}