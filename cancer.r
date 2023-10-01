library(readr)
# setwd("~/Library/CloudStorage/Box-Box/A_Penn/Coronavirus/website/vaccineas/CDC Data/Provisional_Death_DAta/MMWR Week 31 deaths")

### Download weekly death data by cause 2014-2019, keep total USA numbers here
url1="https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
download.file(url1, destfile = "./Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2019.csv",cacheOK=TRUE) 
deaths_2014_2019 <- read_csv("Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2019.csv")
names(deaths_2014_2019)=make.names(names(deaths_2014_2019),unique=TRUE)
deaths_2014_2019=deaths_2014_2019[(deaths_2014_2019$Jurisdiction.of.Occurrence=="United States"),]
names(deaths_2014_2019)[5:17]=c("All.Cause","Natural.Cause","Septicemia","Malignant.neoplasms","Diabetes.mellitus","Alzheimer.disease","Influenza.and.pneumonia","Chronic.lower.respiratory.disease",'Other.respiratory.disease',"Nephritis","Other","Heart.disease","Cerebrovascular.disease")
deaths_2014_2019$Week.Ending.Date <- as.Date(deaths_2014_2019$Week.Ending.Date,"%m/%d/%Y")
deaths_2014_2019=deaths_2014_2019[order(deaths_2014_2019$Week.Ending.Date),]
deaths_2014_2019=subset(deaths_2014_2019, select=c('MMWR.Week','MMWR.Year','Week.Ending.Date',"All.Cause","Natural.Cause","Septicemia","Malignant.neoplasms","Diabetes.mellitus","Alzheimer.disease","Influenza.and.pneumonia","Chronic.lower.respiratory.disease",'Other.respiratory.disease',"Nephritis","Other","Heart.disease","Cerebrovascular.disease"))

moving.average=function(Y,n=6)
{
  cx=c(0,cumsum(Y))
  rsum=(cx[(n+1):length(cx)]-cx[1:(length(cx)-n)])/n
}

library(tidyr)
long <- deaths_2014_2019 %>% gather(Cause,Deaths,-c(MMWR.Week, MMWR.Year, Week.Ending.Date))
Causes=sort(unique(long$Cause))
n=length(Causes)
Linear_results=data.frame(Cause=Causes,Int_2014=rep(0,n),Int_2020=rep(0,n),Slope_weekly=rep(0,n))
Seasonal.effects=list()
long$residual.linear=0
long$seasonal.effect=0
long$linear.trend=0
long$background=0
for (i in 1:n){
  select=(long$Cause==Causes[i])
  temp=lm(Deaths~Week.Ending.Date,data=long[select,])
  Linear_results[i,2]=temp$fitted.values[1]
  Linear_results[i,3]=temp$fitted.values[length(temp$fitted.values)]
  Linear_results[i,4]=temp$coefficients[2]*7
  long[select,]$residual.linear=long[select,]$Deaths-temp$fitted.values
}
long=merge(long,Linear_results,by="Cause")
for (i in 1:n){
  select=(long$Cause==Causes[i])
  long.select=long[select,]
  long.select=long.select[order(long.select$Week.Ending.Date),]
  long.select=long.select[order(long.select$MMWR.Week),]
  long.select=subset(long.select,select=-seasonal.effect)
  temp2=aggregate(residual.linear~MMWR.Week,data=long.select,FUN=mean)
  names(temp2)[2]="seasonal.effect"
  Seasonal.effects[[i]]=temp2$seasonal.effect
  temp3=merge(long.select,temp2,by="MMWR.Week")
  long.select$seasonal.effect=temp3$seasonal.effect
  long.select=long.select[order(long.select$Week.Ending.Date),]
  long.select$linear.trend=long.select$Int_2014+(1:sum(select))*long.select$Slope_weekly
  long.select$background=round(long.select$seasonal.effect+long.select$linear.trend)
  long[select,]$seasonal.effect=long.select$seasonal.effect
  long[select,]$linear.trend=long.select$linear.trend
  long[select,]$background=long.select$background
}

plot_2014_2019=function(long,Causes,i){
  df=long[long$Cause==Causes[i],]
  attach(df)
  sigma=sqrt(var(Deaths-background))
  plot(Week.Ending.Date,Deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Deaths"),main=c(paste(Causes[i],"Deaths (2014-2019)"),"With Linear plus Seasonal trend Fit",paste("s=",round(sigma,0))))
  lines(Week.Ending.Date[3:(length(Week.Ending.Date)-3)],moving.average(Deaths),lwd=4,col="black")
  lines(Week.Ending.Date,background,lwd=4,col="blue")
  #lines(Week.Ending.Date[3:(length(Week.Ending.Date)-3)],moving.average(background),lwd=4,col="blue")
  lines(Week.Ending.Date,linear.trend,lwd=4,col="yellow")
  detach(df)
}

### Read in provisional deaths 6/1/22 release 2020-2022
#release_date="2022-06-01"
#provisional.deaths<-read_tsv(paste("Select-Causes-muzy-jte6-",release_date,".tsv",sep=""))


### Read in provisional deaths 2020-2022
url1="https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
download.file(url1, destfile = "./Weekly_Provisional_Counts_of_Deaths_by_State_and_Select_Causes__2020-2022.csv",cacheOK=TRUE) 
provisional.deaths<- read_csv("Weekly_Provisional_Counts_of_Deaths_by_State_and_Select_Causes__2020-2022.csv")
names(provisional.deaths)<-make.names(names(provisional.deaths),unique=TRUE)
provisional.deaths=provisional.deaths[(provisional.deaths$Jurisdiction.of.Occurrence=="United States"),]
provisional.deaths$Week.Ending.Date <- as.Date(provisional.deaths$Week.Ending.Date)#,format="%y-%m-%d")
names(provisional.deaths)[6:20]=c("All.Cause","Natural.Cause","Septicemia","Malignant.neoplasms","Diabetes.mellitus","Alzheimer.disease","Influenza.and.pneumonia","Chronic.lower.respiratory.disease",'Other.respiratory.disease',"Nephritis","Other","Heart.disease","Cerebrovascular.disease","COVID.multiple","COVID.underlying")
provisional.deaths=subset(provisional.deaths,select=c("MMWR.Year","MMWR.Week","Week.Ending.Date","All.Cause","Natural.Cause","Septicemia","Malignant.neoplasms","Diabetes.mellitus","Alzheimer.disease","Influenza.and.pneumonia","Chronic.lower.respiratory.disease",'Other.respiratory.disease',"Nephritis","Other","Heart.disease","Cerebrovascular.disease","COVID.multiple","COVID.underlying"))

long2 <- provisional.deaths %>% gather(Cause,Deaths,-c(MMWR.Week, MMWR.Year, Week.Ending.Date))
long2.COVID=long2[long2$Cause=="COVID.underlying",]
Causes=sort(unique(long2$Cause))
Causes=Causes[c(1:4,7:15)]
n=length(Causes)
long2$seasonal.effect=0
long2$linear.trend=0
long2$background=0
long2=merge(long2,Linear_results,by="Cause")
long2=long2[order(long2$Cause),]
long2=long2[order(long2$Week.Ending.Date),]

for (i in 1:n){
  select=(long2$Cause==Causes[i])
  long2.select=long2[select,]
  long2.select=long2.select[order(long2.select$Week.Ending.Date),]
  long2.select=long2.select[order(long2.select$MMWR.Week),]
  long2.select=subset(long2.select,select=-seasonal.effect)
  temp2=data.frame(MMWR.Week=1:53,seasonal.effect=Seasonal.effects[[i]])
  temp3=merge(long2.select,temp2,by="MMWR.Week")
  long2.select$seasonal.effect=temp3$seasonal.effect
  long2.select=long2.select[order(long2.select$Week.Ending.Date),]
  long2.select$linear.trend=long2.select$Int_2020+(1:sum(select))*long2.select$Slope_weekly
  long2.select$background=round(long2.select$seasonal.effect+long2.select$linear.trend,0)
  long2[select,]$seasonal.effect=long2.select$seasonal.effect
  long2[select,]$linear.trend=long2.select$linear.trend
  long2[select,]$background=long2.select$background
}

plot_2020_2022=function(long2,Causes,i,long2.COVID=long2.COVID,yl){
  df=long2[long2$Cause==Causes[i],]
  attach(df)
  #plot(Week.Ending.Date,Deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Deaths"),main=c(paste(Causes[i],"Deaths (2020-2022)"),"With Linear plus Seasonal trend Fit"))
  plot(Week.Ending.Date,Deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Deaths"),main=c(paste(Causes[i],"Deaths (2020-2022)"),"With Linear plus Seasonal trend Fit"),ylim=c(11000,12500))
  min1=min(Deaths)
  max1=max(Deaths)
  range1=max1-min1
  min2=min(long2.COVID$Deaths)
  max2=max(long2.COVID$Deaths)
  range2=max2-min2
  scaled.COVID=((long2.COVID$Deaths-min2)/range2)*range1+min1
  #lines(Week.Ending.Date,scaled.COVID,lwd=4,col='gray')
  #temp=loess(Deaths~as.numeric(Week.Ending.Date),span=0.1)
  #lines(Week.Ending.Date,temp$fitted,lwd=4,col="black")
  lines(Week.Ending.Date[3:(length(Week.Ending.Date)-3)],moving.average(background),lwd=4,col="blue")
  #lines(Week.Ending.Date,temp$fitted,lwd=4,col="red")
  lines(Week.Ending.Date[3:(length(Week.Ending.Date)-3)],moving.average(Deaths),lwd=4,col="black")
  #lines(Week.Ending.Date,background,lwd=4,col="blue")
  lines(Week.Ending.Date,linear.trend,lwd=4,col="yellow")
  detach(df)
}
## Gray line is rescaled COVID deaths just to show timing of COVID surges
## Possibly add COVID to background vs. All Cause Deaths
## Also possible subtrack COVID from All Cause and Natural Deaths


#### Now combine 2014-2019 and 2020-2022 data sets and plot together with background linear+seasonal trend
deaths_2014_2019=subset(long, select=c('MMWR.Week','MMWR.Year','Week.Ending.Date','Cause','Deaths','linear.trend','background'))
deaths_2020_2022=subset(long2,select=c('MMWR.Week','MMWR.Year','Week.Ending.Date','Cause','Deaths','linear.trend','background'))
deaths_2014_2022=rbind(deaths_2014_2019,deaths_2020_2022)
deaths_2014_2022=deaths_2014_2022[order(deaths_2014_2022$Cause),]

plot_2014_2022=function(deaths_2014_2022,Causes,i){
  df=deaths_2014_2022[deaths_2014_2022$Cause==Causes[i],]
  df[is.na(df$Deaths),]$Deaths=df[is.na(df$Deaths),]$background
    attach(df)
  plot(Week.Ending.Date,Deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Deaths"),main=c(paste(Causes[i],"Deaths (2014-2022)"),"With Linear plus Seasonal trend Fit"),ylim=c(11000,12500))
  #plot(Week.Ending.Date,Deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Deaths"),main=c(paste(Causes[i],"Deaths (2014-2022)"),"With Linear plus Seasonal trend Fit"))
  temp=loess(Deaths~as.numeric(Week.Ending.Date),span=0.1)
  lines(Week.Ending.Date[3:(length(Week.Ending.Date)-3)],moving.average(background),lwd=4,col="blue")
  #lines(Week.Ending.Date,temp$fitted,lwd=4,col="red")
  lines(Week.Ending.Date[3:(length(Week.Ending.Date)-3)],moving.average(Deaths),lwd=4,col="black")
  lines(Week.Ending.Date,linear.trend,lwd=4,col="yellow")
  detach(df)
}

plot_2014_2022_excess=function(long2,Causes,i){
  df=long2[long2$Cause==Causes[i],]
  df[is.na(df$Deaths),]$Deaths=df[is.na(df$Deaths),]$background
  attach(df)
  excess_deaths=(Deaths-background)
  plot(Week.Ending.Date,excess_deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Weekly Excess Deaths"),main=c(paste(Causes[i],"Weekly Excess Deaths")))
  lines(Week.Ending.Date[3:(length(Week.Ending.Date)-3)],moving.average(excess_deaths),lwd=4,col="black")
  lines(Week.Ending.Date,rep(0,length(Week.Ending.Date)),lwd=4,col="yellow")
  detach(df)
}
plot_2014_2022_cum_excess=function(deaths_2014_2022,Causes,i){
  df=deaths_2014_2022[deaths_2014_2022$Cause==Causes[i],]
  df[is.na(df$Deaths),]$Deaths=df[is.na(df$Deaths),]$background
  attach(df)
  cum_excess_deaths=cumsum(Deaths-background)
  total_cum_excess_deaths=cum_excess_deaths[length(cum_excess_deaths)]
  plot(Week.Ending.Date,cum_excess_deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Cumulative Excess Deaths"),main=c(paste(Causes[i],"Cumulative Excess Deaths"),paste("Total cumulative excess=",total_cum_excess_deaths)))
  lines(Week.Ending.Date,cum_excess_deaths,lwd=4,col="black")
  lines(Week.Ending.Date,rep(0,length(Week.Ending.Date)),lwd=4,col="yellow")
  detach(df)
}

plot_2020_2022_excess=function(long2,Causes,i){
  df=long2[long2$Cause==Causes[i],]
  df[is.na(df$Deaths),]$Deaths=df[is.na(df$Deaths),]$background
  attach(df)
  excess_deaths=(Deaths-background)
  #plot(Week.Ending.Date,excess_deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Weekly Excess Deaths"),main=c(paste(Causes[i],"Weekly Excess Deaths")))
  plot(Week.Ending.Date,excess_deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Weekly Excess Deaths"),main=c(paste(Causes[i],"Weekly Excess Deaths")),ylim=c(-500,500))
  lines(Week.Ending.Date[3:(length(Week.Ending.Date)-3)],moving.average(excess_deaths),lwd=4,col="black")
  lines(Week.Ending.Date,rep(0,length(Week.Ending.Date)),lwd=4,col="yellow")
  detach(df)
}
plot_2020_2022_cum_excess=function(long2,Causes,i){
  df=long2[long2$Cause==Causes[i],]
  df[is.na(df$Deaths),]$Deaths=df[is.na(df$Deaths),]$background
  attach(df)
  cum_excess_deaths=cumsum(Deaths-background)
  total_cum_excess_deaths=cum_excess_deaths[length(cum_excess_deaths)]
  plot(Week.Ending.Date,cum_excess_deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Cumulative Excess Deaths"),main=c(paste(Causes[i],"Cumulative Excess Deaths"),paste("Total cumulative excess=",total_cum_excess_deaths)))
  lines(Week.Ending.Date,cum_excess_deaths,lwd=4,col="black")
  lines(Week.Ending.Date,rep(0,length(Week.Ending.Date)),lwd=4,col="yellow")
  detach(df)
}

release_date="2022-09-08"

# setwd(paste("~/Library/CloudStorage/Box-Box/A_Penn/Coronavirus/website/vaccineas/CDC Data/Provisional_Death_DAta/MMWR Week 31 deaths/TES/plots/",release_date,sep=""))


i=8
jpeg(filename=paste(Causes[i],"_Deaths_2014-2019.jpg"))
df=deaths_2014_2022[deaths_2014_2022$Cause==Causes[i],]
df[is.na(df$Deaths),]$Deaths=df[is.na(df$Deaths),]$background
attach(df)
plot(Week.Ending.Date,Deaths,pch=16,xlab='Date',ylab=paste(Causes[i],"Deaths"),main=paste(Causes[i],"Deaths (2014-2022)"),ylim=c(11000,12500))
detach(df)
dev.off()

ending.date=max(deaths_2014_2022$Week.Ending.Date)-(21*7)
jpeg(filename=paste(Causes[i],"2014-2019_background.jpg"))
plot_2014_2019(long[long$Week.Ending.Date<=ending.date,],Causes,i)
legend(min(long$Week.Ending.Date),12350,legend=c("Weekly Cancer Deaths","6wk Moving Average","Linear+Seasonal Trend","Linear Trend"),pch=c(16,NA,NA,NA),lty=c(0,1,1,1),lwd=c(0,4,4,4),col=c(1,1,4,7))
dev.off()

jpeg(filename=paste(Causes[i],"2020-2022_background.jpg"))
plot_2020_2022(long2[long2$Week.Ending.Date<=ending.date,],Causes,i,long2.COVID)
legend(min(long2$Week.Ending.Date),12350,legend=c("Weekly Cancer Deaths","6wk Moving Average","Linear+Seasonal Background Cancer Deaths","Linear Trend"),pch=c(16,NA,NA,NA),lty=c(0,1,1,1),lwd=c(0,4,4,4),col=c(1,1,4,7))
dev.off()

jpeg(filename=paste(Causes[i],"2020-2022_background_all.jpg"))
plot_2020_2022(long2,Causes,i,long2.COVID)
legend(min(long2$Week.Ending.Date),12350,legend=c("Weekly Cancer Deaths","6wk Moving Average","Linear+Seasonal Background Cancer Deaths","Linear Trend"),pch=c(16,NA,NA,NA),lty=c(0,1,1,1),lwd=c(0,4,4,4),col=c(1,1,4,7))
dev.off()

jpeg(filename=paste(Causes[i],"2014-2022_background.jpg"))
plot_2014_2022(deaths_2014_2022[deaths_2014_2022$Week.Ending.Date<=ending.date,],Causes,i)
legend(min(deaths_2014_2022$Week.Ending.Date),12500,legend=c("Weekly Cancer Deaths","6wk Moving Average","Linear+Seasonal Cancer Deaths","Linear Trend"),pch=c(16,NA,NA,NA),lty=c(0,1,1,1),lwd=c(0,4,4,4),col=c(1,1,4,7))
dev.off()

jpeg(filename=paste(Causes[i],"2020-2022_excess.jpg"))
plot_2020_2022_excess(long2[long2$Week.Ending.Date<=ending.date,],Causes,i)
legend(min(long2$Week.Ending.Date),500,legend=c("Weekly Cancer Excess Deaths","6wk Moving Average"),pch=c(16,NA),lty=c(0,1),lwd=c(0,4),col=c(1,1))
dev.off()

jpeg(filename=paste(Causes[i],"2020-2022_excess_all.jpg"))
plot_2020_2022_excess(long2,Causes,i)
legend(min(long2$Week.Ending.Date),500,legend=c("Weekly Cancer Excess Deaths","6wk Moving Average"),pch=c(16,NA),lty=c(0,1),lwd=c(0,4),col=c(1,1))
dev.off()

jpeg(filename=paste(Causes[i],"2020-2022_cumulative_excess.jpg"))
plot_2020_2022_cum_excess(long2[long2$Week.Ending.Date<=ending.date,],Causes,i)
#legend(min(long2$Week.Ending.Date),12500,legend=c("Weekly Cancer Deaths","6wk Moving Average","Linear+Seasonal Cancer Deaths","Linear Trend"),pch=c(16,NA,NA,NA),lty=c(0,1,1,1),lwd=c(0,4,4,4),col=c(1,1,4,7))
dev.off()

jpeg(filename=paste(Causes[i],"2020-2022_cumulative_excess_all.jpg"))
plot_2020_2022_cum_excess(long2,Causes,i)
#legend(min(long2$Week.Ending.Date),12500,legend=c("Weekly Cancer Deaths","6wk Moving Average","Linear+Seasonal Cancer Deaths","Linear Trend"),pch=c(16,NA,NA,NA),lty=c(0,1,1,1),lwd=c(0,4,4,4),col=c(1,1,4,7))
dev.off()

jpeg(filename=paste(Causes[i],"2014-2022_excess.jpg"))
plot_2014_2022_excess(deaths_2014_2022[deaths_2014_2022$Week.Ending.Date<=ending.date,],Causes,i)
legend(min(deaths_2014_2022$Week.Ending.Date),500,legend=c("Weekly Cancer Excess Deaths","6wk Moving Average"),pch=c(16,NA),lty=c(0,1),lwd=c(0,4),col=c(1,1))
dev.off()

jpeg(filename=paste(Causes[i],"2014-2022_cumulative_excess.jpg"))
plot_2014_2022_cum_excess(deaths_2014_2022[deaths_2014_2022$Week.Ending.Date<=ending.date,],Causes,i)
#legend(min(long2$Week.Ending.Date),12500,legend=c("Weekly Cancer Deaths","6wk Moving Average","Linear+Seasonal Cancer Deaths","Linear Trend"),pch=c(16,NA,NA,NA),lty=c(0,1,1,1),lwd=c(0,4,4,4),col=c(1,1,4,7))
dev.off()




