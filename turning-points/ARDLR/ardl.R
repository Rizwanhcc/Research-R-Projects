#=============ARDL=====================!!
##installing package from github
#first get dvtools package and then the next command
##========SME World BANK Enterprise Survey=============

library(WDI)
getwd()
setwd("D:/Utilisateurs/e0g411m05t7/Dropbox/Rdatapaper1/ARDLR")
setwd("~/Dropbox/Rdatapaper1/ARDLR")

q() # quit R. You will be prompted to save the workspace.
##remove everything
rm(list = ls())
##SAVE everything in R, including DATA
save.image(file="timeseries.RData")


library(devtools)
library(ggplot2)

install_github("fcbarbi/ardl")

library(ardl)
?ardl

data(br_month)
data <- data.frame(br_month)


head(br_month)
m1 <- ardl( mpr~cpi+reer, data=br_month )
summary(m1)
m2 <- ardl( mpr~cpi+reer|d_lula, data=br_month, ylag=2, xlag=c(3,1), case=5, quiet=TRUE )  
summary(m2)
m3 <- ardl( mpr~cpi+prod+reer|d_lula, data=br_month, ylag=2, xlag=c(1,2,2), case=1 )
summary(m3)



##FOR LATEST WORLD BANK STATISTICS
devtools::install_github("GIST-ORNL/wbstats")
library(wbstats)
library(dplyr)
browseVignettes(package = "wbstats")
# default language is english
wb_cachelist
str(wb_cachelist, max.level = 1)
new_cache <- wbcache()
wbsearch("poverty")
domesticredit <- wbsearch(pattern = "domestic.credit")
head(domesticredit)
domesticredit <- wbsearch(pattern = "Liquid.Liabilities")
domesticredit <- wbsearch(pattern = "GDP")


#Queries with multiple indicators return the data in a long data format

creditgdp <- wb(country = c("PK","IN"), 
        indicator = c("NY.GDP.PCAP.PP.KD.ZG", "NE.TRD.GNFS.ZS","NE.CON.PRVT.PC.KD","NE.CON.PETC.ZS", "NY.GDP.MKTP.KD.ZG", 
        "FP.CPI.TOTL.ZG","NE.CON.GOVT.ZS","NE.GDI.FTOT.ZS", "FD.AST.PRVT.GD.ZS","GFDD.SI.01","GFDD.DI.05","GFDD.DI.14"),
        startdate = 1971, enddate = 2015)

gdppp <- wb(country = c("PK"), 
                indicator = c("NY.GDP.MKTP.CD"),
                startdate = 1971, enddate = 2015)


gdpcusd <- gdppp[, -c(3:6)]
gdpcusd <- gdpcusd[,order(date) ] 
gdpcusd <- gdpcusd[order(gdpcusd$date, decreasing = FALSE),]
gdpcusd$date <- NULL



head(creditgdp)

#http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
#http://www.statmethods.net/advstats/timeseries.html
#https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
#http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
##this option worked after a great struggle from long to wide by country and year

library(reshape2)

data_wide <- dcast(creditgdp, country + date ~ indicatorID, value.var="value")
data_wide

names(data_wide)

##renaming all variables simultanously
setnames(data_wide, old = c("country","date" ,"FD.AST.PRVT.GD.ZS","FP.CPI.TOTL.ZG","GFDD.DI.05","GFDD.DI.14",       
                   "GFDD.SI.01","NE.CON.GOVT.ZS","NE.CON.PETC.ZS","NE.CON.PRVT.PC.KD","NE.GDI.FTOT.ZS","NE.TRD.GNFS.ZS",   
                    "NY.GDP.MKTP.KD.ZG"), 
                      new = c("Country","Year" ,"Credit","Inflation","LL","Credit2",       
                  "zscore","Govcon","HHcon","HHconpc","Gfixed","Trade", "gdppcg"))

##now delete irrelevent information
pakdata <- data_wide[46:90, -7] 
##delete column number 7 from data
pakdata <- pakdata[ , -7 ] 
pakdata <- pakdata[ , -1 ] 

##ardl
m1 <- ardl( gdppcg~Credit+Inflation, data=pakdata )
summary(m1)

# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(pakdata, start=c(1971), end=c(2015), frequency=1) 
myts <- as.data.frame(myts)
myts <- myts[ , -1 ]
##just retain gdp
gdp <- pakdata[, -c(1:10)]
##set gdp as time series
gdp <- ts(gdp, start=c(1971), end=c(2015), frequency=1) 
#credit
credt <- pakdata[, -c(3:11)]
credt <- credt[, -c(1)]
credt <- ts(credt, start=c(1971), end=c(2015), frequency=1) 
gdpcusd <- ts(gdpcusd, start=c(1971), end=c(2015), frequency=1) 

attach(myts)
plot(myts)
plot.ts(gdppcg)
is.numeric(gdppcg)

install.packages("tempdisagg")

require(tempdisagg)

##you must have seperate time series objects
##simple way to convert annual to quarterly series
m1 <- td(gdp ~ 1, to = "quarterly", method = "denton-cholette")
predict(m1)

plot(predict(m1),col = "blue")
plot(gdp)
lines(predict(m1), col = "red")  # Denton-Cholette

##credit
m2 <- td(credt ~ 1, to = "quarterly", method = "denton-cholette")
qcredit <- predict(m2)
plot(predict(m2),col = "blue")
plot(m2)

##gdp at usd
m3 <- td(gdpcusd ~ 1, to = "quarterly", method = "denton-cholette")
plot(predict(m3),col = "blue")
plot(m3)


lfd <- log(val$x)
tmp <- standard.cyclical.con(xxx)

str(m3)
gdppp <- as.data.frame(m3$values)

lgdp <- log(gdppp)
lgdp <- as.data.frame(lgdp)


##time series plots
#http://wwwhihaho.synology.me/hoon/?p=164
#http://stackoverflow.com/questions/29648907/using-geom-rect-for-time-series-shading-in-r
#https://blog.rstudio.org/2015/04/14/interactive-time-series-with-dygraphs/S

library(ggplot2)
# Expand the following command with geom_rect() to draw the recess periods

# Expand the following command with geom_rect() to draw the recess periods
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line() +
  geom_rect(data = recess, inherit.aes = FALSE, aes(xmin =begin, xmax=end, ymin = -Inf, ymax =+Inf), fill = "red", alpha = 0.2)