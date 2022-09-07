
#summary(res)
library(lattice)

dataset = read.csv("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/owid-covid-data.csv", stringsAsFactors = T)
dataset<- replace(dataset$new_deaths,dataset$new_deaths=='',0);
write.csv(dataset,file="/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/nowid-covid-data.csv");

#for (name in levels(dataset$iso_code)){
#  par(mar=c(1,1,1,1))
#  fn=paste("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/countries/",name,'.csv',sep="")
#  indata <- read.csv(fn)
#  loc<-indata$location
#  if(isFALSE(min(indata$new_deaths)=="NA" && max(indata$new_deaths)=="NA"))
#  {
#    ifile <- paste("/Users/vinaymoolya/Desktop/semester5/FDA/Jcomp/FDA_J_COMP/visualization/deaths/",name,".pdf",sep="")
#    pdf(file=ifile,width=7,height=5)
#    plot(indata$new_cases,type="l",col="blue",xlab="No_of_Days",ylab="New_cases",ylim=c(min(indata$new_deaths),max(indata$new_deaths)),main=paste("New Cases In ",name,sep = ""))
#    dev.off()
#  }
#}