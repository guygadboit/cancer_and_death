library(readr)
#setwd("~/Box/A_Penn/Coronavirus/website/vaccineas/CDC Data/Provisional_Death_DAta/MMWR Week 31 deaths")

url2="https://data.cdc.gov/api/views/vsak-wrfu/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
download.file(url2,destfile="./Provisional_COVID-19_Deaths_by_Week__Sex__and_Age.csv")
Weekly_Death_totals <- read_csv("./Provisional_COVID-19_Deaths_by_Week__Sex__and_Age.csv")
names(Weekly_Death_totals)<-make.names(names(Weekly_Death_totals),unique=TRUE)
df=Weekly_Death_totals[Weekly_Death_totals$State=="United States",]
df=df[df$Sex=="All Sex",]
df$End.Week=as.Date(df$End.Week,format="%m/%d/%Y")

df_age=df[df$Age.Group=="25-34 Years",]

attach(df_age)
plot(End.Week,Total.Deaths,type='l',ylim=c(0,max(Total.Deaths)*1.5),lwd=4,col=1,xlab="Date",ylab="Weekly Deaths",main=c("Total Deaths, COVID-19 Deaths and non-COVID-19 Deaths",paste(Age.Group[1])))
lines(End.Week,COVID.19.Deaths,lwd=4,col=2)
lines(End.Week,Total.Deaths-COVID.19.Deaths,lwd=4,col=3)
legend(min(End.Week),max(Total.Deaths)*1.5,legend=c("Total Deaths","COVID-19 Deaths","non-COVID-19 Deaths"),lwd=c(4,4,4),col=c(1,2,3))
detach(df_age)

url1="https://data.cdc.gov/api/views/w56u-89fn/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
download.file(url1, destfile = "./AH_Deaths_by_Week_Sex_and_AGE_for_2018-2020.csv",cacheOK=TRUE) 
Weekly_Deaths_2018_2020<-read_csv("AH_Deaths_by_Week_Sex_and_AGE_for_2018-2020.csv")
names(Weekly_Deaths_2018_2020)<-make.names(names(Weekly_Deaths_2018_2020),unique=TRUE)
df2=Weekly_Deaths_2018_2020[Weekly_Deaths_2018_2020$MMWR.Year<2020,]
df2=df2[df2$Sex=="All Sexes",]
df2$End.Week=as.Date(df2$Week.Ending.Date,format="%m/%d/%Y")
df2$Total.Deaths=as.numeric(df2$Total.Deaths)
library(tidyr)
wide=spread(df2,Age.Group,Total.Deaths)

wide$"1-4 Years"=wide$`1-4 years`
wide$'5-14 Years'=wide$`5-9 years`+wide$`10-14 years`
wide$'15-24 Years'=wide$`15-19 years`+wide$`20-24 years`
wide$'25-34 Years'=wide$`25-29 years`+wide$`30-34 years`
wide$'35-44 Years'=wide$`35-39 years`+wide$`40-44 years`
wide$'45-54 Years'=wide$`45-49 years`+wide$`50-54 years`
wide$'55-64 Years'=wide$`55-59 years`+wide$`60-64 years`
wide$'65-74 Years'=wide$`65-69 years`+wide$`70-74 years`
wide$'75-84 Years'=wide$`75-79 years`+wide$`80-84 years`
wide$'85 Years and Over'=wide$`85 years and over`
wide$'All Ages'=wide$`All Ages`
wide$'Under 1 year'=wide$`0 years`
wide=subset(wide,select=c(MMWR.Week,MMWR.Year,`Under 1 year`,`1-4 Years`,`5-14 Years`,`15-24 Years`,`25-34 Years`,`35-44 Years`,`45-54 Years`,`55-64 Years`,`65-74 Years`,`75-84 Years`,`85 Years and Over`,`All Ages`))
long=gather(wide,Age.Group,Background.Deaths,"Under 1 year":"All Ages")


background_deaths_age_week=aggregate(Background.Deaths~MMWR.Week+Age.Group,data=long,FUN=mean)

df=subset(df,select=c("MMWR.Week","End.Week","Age.Group","Total.Deaths","COVID.19.Deaths"))

df=df[order(df$MMWR.Week),]
df=df[order(df$Age.Group),]
background_deaths_age_week=background_deaths_age_week[order(background_deaths_age_week$MMWR.Week),]
background_deaths_age_week=background_deaths_age_week[order(background_deaths_age_week$Age.Group),]
df3=merge(df,background_deaths_age_week,by=c("MMWR.Week","Age.Group"))

plot_age=function(df,AgeGroup){
  df_age=df[df$Age.Group==AgeGroup,]
  df_age=df_age[order(df_age$End.Week),]
  attach(df_age)
  plot(End.Week,Total.Deaths,type='l',ylim=c(0,max(Total.Deaths)*1.8),lwd=4,col=1,xlab="Date",ylab="Weekly Deaths",main=c("Total Deaths, COVID-19 Deaths and non-COVID-19 Deaths",paste(AgeGroup)))
  lines(End.Week,COVID.19.Deaths,lwd=4,col=2)
  lines(End.Week,Total.Deaths-COVID.19.Deaths,lwd=4,col=3)
  lines(End.Week,Background.Deaths,lwd=4,col=4)
  legend(min(End.Week),max(Total.Deaths)*1.8,legend=c("Total Deaths","COVID-19 Deaths","non-COVID-19 Deaths","pre-pandemic Death rate (2018-2019)"),lwd=c(4,4,4,4),col=c(1,2,3,4))
  detach(df_age)
}

AgeGroups=sort(unique(df3$Age.Group))
n=length(AgeGroups)
for (i in 1:n){
   jpeg(filename=paste("2020_2022_allcause_covid_background_deaths",AgeGroups[i],".jpg"))
   plot_age(df3,AgeGroups[i])
   dev.off()
}

### Compute total number of COVID-19 deaths and % of 2018-2019 baseline for given date (start.date, end.date)
summarize_range=function(df3,AgeGroupToPlot,start.date="2021-08-20",end.date="2021-10-03")
{
  C19=sum(df3[(df3$Age.Group==AgeGroupToPlot)&(df3$End.Week>start.date)&(df3$End.Week<end.date),]$COVID.19.Deaths)
  back=sum(df3[(df3$Age.Group==AgeGroupToPlot)&(df3$End.Week>start.date)&(df3$End.Week<end.date),]$Background.Deaths)
  Pexcess=100*(C19-back)/back*-1
  paste(round(C19,0)," COVID-19 deaths,",round(back,0)," background 2018-2019 deaths", round(Pexcess,1),"% over 2018-2019 Baseline",start.date,"-",end.date)
}
summarize_range(df3,"25-34 Years")
summarize_range(df3,"35-44 Years")
summarize_range(df3,"45-54 Years")

