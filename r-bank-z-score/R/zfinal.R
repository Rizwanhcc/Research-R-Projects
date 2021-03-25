#===========================================================
#===========Bank Risks and Financial Inclusion FINAL==========#@#$
#===========================================================

setwd("D:/Utilisateurs/e0g411m05t7/Dropbox/BankZscore/R") ##FOR LAB
getwd()
##For MAC
setwd("~/Dropbox/BankZscore/R")

zscore <- read.csv("mydat.csv", header= TRUE,check.names=F) # reading data from csv
dob <- read.csv("extrawork.csv", header= TRUE,check.names=F) # reading data from csv

attach(zscore)

#Reshape world bank data
library(reshape2)
library(data.table)
##declare as dataframe
as.data.frame(aaa)


#SET DATA AS PANEL optional

aa <- plm.data(aaa.ma, index("code","year"))


aaa<- dcast(melt(setDT(zscore), id.var=c("Indicator", "code", "id"), 
            variable.name="year"), 
            code+year~Indicator, value.var='value')

##subsetting dataset
panell <- subset(aaa, year==2012:2014)
subset14 <- subset(aaa, year==2014)
subset13 <- subset(aaa, year==2013)
subset12 <- subset(aaa, year==2012)
cros <- subset(aaa, year==2013)


##taking averages of panel data
library(plyr)
library(zoo)
library(pwt)

##Convert factor to numeric
as.character(year)
head(aaa$year)
str(aaa)
aaa$year <- as.numeric(as.character(aaa$year))
attach(aaa)

##Transforming variables
panell$loggdppc <- log(panell$loggdppc )
panell$logcredit <- log(panell$creditd)

cros$loggdppc <- log(cros$loggdppc)
cros$logcredit <- log(cros$creditd)

#merge two datasets
fdinx <- merge(new_data, fdind, by=c("country","year")) 
library(lattice)
attach(panell)
##taking averages of three years...Will creat a new dataset
aaa$period <- cut(aaa$year, seq(1900, 2100, 3)) 
aaa.ma <- ddply(aaa, .(code, period), numcolwise(mean))
aaa.ma

##Plots using ggplot and lattice packages

xyplot(log(gdppcap) ~ log(bbranch) | country, new_data, type = c("p","r"),
        xlab = "Bank branches per 100,000 adults", ylab = "GDP per capita",
        main = "Correlation between Bank Branches and GDP per capita")

xyplot(log(zcore) ~ log(credit) | country, new_data, type = c("p","r"),
       xlab = "Log of Domestic Credit to Private Sector", ylab = " Z score",
       main = "Correlation between Credit and Z score")


Bubble <- gvisBubbleChart(cros, idvar="code", 
                          xvar="logcredit", yvar="loggdppc",
                          colorvar="code", sizevar="bankbradult",
                          options=list(
                          hAxis='{minValue:2, maxValue:5}'))
plot(Bubble)

summary(logcredit)
Bubble <- gvisBubbleChart(panell, idvar="code", 
                          xvar="logcredit", yvar="loggdppc",
                          colorvar="year", sizevar="boc",
                          options=list(
                          hAxis='{minValue:2.5, maxValue:4.5}'))
plot(Bubble)
