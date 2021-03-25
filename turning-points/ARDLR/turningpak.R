#========Financial Turning Points for Pakistan========

#First of all we need Quarterly data to proceed further, i will get annual data and then convert it to quarters.
library(wbstats)
#getting data from world bank
creditgdp <- wb(country = c("PK","IN"), 
        indicator = c("NY.GDP.PCAP.KD.ZG", "NY.GDP.MKTP.CD", "FS.AST.PRVT.GD.ZS", "NE.CON.PETC.ZS"),
        startdate = 1971, enddate = 2015)

#getting data into a proper shape
library(reshape2)
data_wide <- dcast(creditgdp, country + date ~ indicatorID, value.var="value")
data_wide

##now delete irrelevent information
pakdata <- data_wide[46:90, ] 
##delete column number 7 from data
pakdata <- pakdata[ , -1 ] 

library(data.table)
##Renaming variables
setnames(pakdata, old = c("date","FS.AST.PRVT.GD.ZS","NE.CON.PETC.ZS","NY.GDP.MKTP.CD","NY.GDP.PCAP.KD.ZG" ), 
         new = c("Year" ,"Credit", "Consumption", "GDP", "GDPPCG"))

##create time series individual objects for each of the variables
myts <- ts(pakdata, start=c(1971), end=c(2015), frequency=1) ##it will set whole data but it will not work 

##i need to do something like this
credit <- pakdata[,2] 

credit <- ts(credit, start=c(1971), end=c(2015), frequency=1) 

##Now i am going to interpolate this series
require(tempdisagg)
m1 <- td(credit ~ 1, to = "quarterly", method = "denton-cholette")
##extract predicted quarterly series, we can plot it, also can plot(m1)
mm1 <- predict(m1)
plot(predict(m1))
#OR
plot(mm1)

##Turning Points, there are two methods based on BBQ package, simple and based on India.
#simple 
library(BCDating)

#Set your series into quarterly time series
credit <- ts(mm1,freq=4, start=c(1971,1))

#if you want to take log
log(mm1)

#Final step
dat <- BBQ(mm1, name="Dating Financial Cycles of Pakistan")
show(dat)
summary(dat)
plot(dat)
plot(dat,mm1)


##untill now we are done with the basic graphs...however if we want some more seggerigation
#by using function of cyclycal from the codes in file turning.r..
## Function for computing standardised cyclical component
library(mFilter)
library(BCDating)
library(xtable)
library(zoo)

standard.cyclical.con <- function(series, filter="cf"){
  if(filter=="hp"){
    series <- na.omit(series)
    filtered.ser <- hpfilter(log(na.omit(series)), type=c("lambda"), freq=1600)}
  else { if (filter=="cf"){
    
## Applying filter over the reference series
    filtered.ser <- cffilter(log(na.omit(series)),pl=8, pu=32, root=TRUE)}
    else {(filter=="bk")
      filtered.ser <- bkfilter(log(na.omit(series)),pl=8,pu=32, drift=FALSE)
    }}
  
## Extracting cyclical component
  cyclical <- filtered.ser$cycle
  tmp <- (cyclical-mean(cyclical,na.rm=TRUE))/sapply(cyclical,sd,na.rm=TRUE)
}

#here is the final command
tmp1 <- standard.cyclical.con(mm1)
dat <- BBQ(tmp1)
plot(dat)
plot(dat, tmp1)


##Next=we=redo=all=the=steps=for=GDP=per=capita=======================================

##i need to do something exctract gdpgrowth series
gdpg <- pakdata[,4] 

gdpg <- ts(gdpg, start=c(1971), end=c(2015), frequency=1) 

##Now i am going to interpolate this series
require(tempdisagg)
m2 <- td(gdpg ~ 1, to = "quarterly", method = "denton-cholette")
##extract predicted quarterly series, we can plot it, also can plot(m1)
mm2 <- predict(m2)
plot(predict(m2))
#OR
plot(mm2)

##Turning Points, there are two methods based on BBQ package, simple and based on India.
#simple 
library(BCDating)

#Set your series into quarterly time series
gdpg <- ts(mm2,freq=4, start=c(1971,1))

#if you want to take log
gdpg <- log(gdpg) #no need to take log as it is growth rate

#Final step
dat1 <- BBQ(gdpg, name="Dating Business Cycles of Pakistan")
show(dat1)
summary(dat1)
plot(dat1)
plot(dat1,gdpg)

##untill now we are done with the basic graphs...however if we want some more seggerigation
#by using function of cyclycal from the codes in file turning.r..
tmp2 <- standard.cyclical.con(gdpg)
dat1 <- BBQ(tmp2)
plot(dat1)
plot(dat1, tmp2)

##revised plot for final print
par(mfrow=c(2,2))
plot(dat,mm1, main = "Financial Turning points (Harding-Pagan)")
plot(dat, tmp1, main = "Financial Turning points (Hamilton-filter)")


##ploting combine
par(mfrow=c(2,2))
plot(dat,mm1, main = "Financial Turning points (Harding-Pagan)")
plot(dat, tmp1, main = "Financial Turning points (Hamilton-filter)")

plot(dat1,gdpg, main = "GDP Turning points (Harding-Pagan)")
plot(dat1, tmp2, main = "GDP Turning points (Hamilton-filter)")

##reset to default
dev.off()
results1["peaks"]
results1[[1]]

data=(beaver2)
head(beaver2)
shapiro.test()l


##Turning Points for GDP per capita.....

wbsearch('gdp.per.capita')
gdppcap <- wb(country = c("PK","IN"), 
              indicator = c("NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD"),
              startdate = 1971, enddate = 2015)
#getting data into a proper shape
library(reshape2)
data_gdppc <- dcast(gdppcap, country + date ~ indicatorID, value.var="value")
data_gdppc

##now delete irrelevent information
pakgdppc <- data_gdppc[46:90, ] 

library(data.table)
##Renaming variables
setnames(pakgdppc, old = c("date","NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD" ), 
         new = c("Year" ,"gdppcapcd", "gdppcapkd"))


##i need to do something like this to convert it to time series object
gdppcapcd <- pakgdppc[,3] 

gdppcapcd <- ts(gdppcapcd, start=c(1971), end=c(2015), frequency=1) 

##Now i am going to interpolate this series
require(tempdisagg)
m4 <- td(gdppcapcd ~ 1, to = "quarterly", method = "denton-cholette")
##extract predicted quarterly series, we can plot it, also can plot(m1)
mm4 <- predict(m4)
plot(predict(m4))
#OR
plot(mm4)

##Turning Points, there are two methods based on BBQ package, simple and based on India.
#simple 
library(BCDating)

#Set your series into quarterly time series
gdppcapcd <- ts(mm4,freq=4, start=c(1971,1))

#if you want to take log
log(gdppcapcd)

#Final step
dat <- BBQ(gdppcapcd, name="Dating Business Cycles in Pakistan")
show(dat)
summary(dat)
plot(dat)
plot(dat,mm4)


tmp1 <- standard.cyclical.con(mm4)
dat <- BBQ(tmp1)
plot(dat)
plot(dat, tmp1)

###Finanical Development Index...
setwd("D:/Utilisateurs/e0g411m05t7/Dropbox/ThesisResource/StructureBreaks")
fdi <- read.csv("fdi.csv", header = TRUE)
fdi <- fdi[,3] 

fdi <- ts(fdi, start=c(1980), end=c(2014), frequency=1) 
plot(fdi)


za.fdi <- ur.za(fdi, model = "both", lag = 3)
summary(za.fdi)
plot(za.fdi)




