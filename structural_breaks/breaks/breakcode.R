#========Structure Breaks and Finance Growth Relation in Pakistan========
#http://web.uvic.ca/~dgiles/downloads/johansen/index.html
#http://www.ssc.wisc.edu/~bhansen/progs/joe_96.html
#https://cran.r-project.org/web/packages/uroot/uroot.pdf
#First of all we need Quarterly data to proceed further, i will get annual data and then convert it to quarters.

library(wbstats)
#getting data from world bank
pakbreaks <- wb(country = c("PK","IN"), 
                indicator = c("FS.AST.PRVT.GD.ZS","FS.AST.DOMS.GD.ZS", "GFDD.DI.05","NY.GDP.PCAP.KD","NY.GDP.PCAP.CD","FP.CPI.TOTL.ZG", "FM.LBL.BMNY.GD.ZS","SI.POV.DDAY","SI.POV.GINI","NY.GDP.MKTP.KD.ZG","NY.GDP.PCAP.KD.ZG","NE.CON.PRVT.PC.KD", "NE.CON.PETC.ZS","NE.TRD.GNFS.ZS"),
                startdate = 1971, enddate = 2015)

#getting data into a proper shape
library(reshape2)
data_wide <- dcast(pakbreaks, country + date ~ indicatorID, value.var="value")
data_wide

##now delete irrelevent information
pakdata <- data_wide[46:90, ] 
##delete column number 7 from data
pakdata <- pakdata[ , -1 ] 

library(data.table)
##Renaming variables
setnames(pakdata, old = c("date","FS.AST.PRVT.GD.ZS","FS.AST.DOMS.GD.ZS", "GFDD.DI.05","NY.GDP.PCAP.KD","NY.GDP.PCAP.CD","FP.CPI.TOTL.ZG", "FM.LBL.BMNY.GD.ZS","SI.POV.DDAY","SI.POV.GINI","NY.GDP.MKTP.KD.ZG","NY.GDP.PCAP.KD.ZG","NE.CON.PRVT.PC.KD", "NE.CON.PETC.ZS", "NE.TRD.GNFS.ZS"), 
         new = c("Year" ,"credit", "financial", "ll", "gdppckd", "gdppccd" , "inf", "bmoney", "phcr", "gini", "gdpg","gdppcg","hhconpc", "hhcongdp", "trade"))

##create time series individual objects for each of the variables
myts <- ts(pakdata, start=c(1971), end=c(2015), frequency=1) ##it will set whole data but it will not work 

##i need to do something like this
credit <- pakdata[,5] 

credit <- ts(credit, start=c(1971), end=c(2015), frequency=1) 
#OR aTTACHE
attach(pakdata)
##Now i am going to interpolate this series
require(tempdisagg)
m1 <- td(credit ~ 1, to = "quarterly", method = "denton-cholette")
##extract predicted quarterly series, we can plot it, also can plot(m1)
plot(m1)
mm1 <- predict(m1)
plot(predict(m1))

##saving mm1 as data frame
creditgdp <- as.data.frame(mm1)

#NOW DISSAGGREGATING ALL OTHER SERIES by fist creating time series objects
bmoney     <- ts(bmoney, start=c(1971), end=c(2015), frequency=1) 
inf        <- ts(inf, start=c(1971), end=c(2015), frequency=1) 
financial  <- ts(financial, start=c(1971), end=c(2015), frequency=1) 
credit     <- ts(credit, start=c(1971), end=c(2015), frequency=1) 
ll         <- ts(ll, start=c(1971), end=c(2015), frequency=1) 
hhcongdp   <- ts(hhcongdp, start=c(1971), end=c(2015), frequency=1) 
hhconpc    <- ts(hhconpc, start=c(1971), end=c(2015), frequency=1) 
trade      <- ts(trade, start=c(1971), end=c(2015), frequency=1) 
gdpg       <- ts(gdpg, start=c(1971), end=c(2015), frequency=1) 
gdppccd    <- ts(gdppccd, start=c(1971), end=c(2015), frequency=1) 
gdppckd    <- ts(gdppckd, start=c(1971), end=c(2015), frequency=1) 
gdppcg     <- ts(gdppcg, start=c(1971), end=c(2015), frequency=1) 
phcr       <- ts(phcr, start=c(1971), end=c(2015), frequency=1) 
gini       <- ts(gini, start=c(1971), end=c(2015), frequency=1) 


##Now its time to dissaggregate ALL series to quarterly observations....
m1 <- td(bmoney ~ 1, to = "quarterly", method = "denton-cholette")
m2 <- td(inf ~ 1, to = "quarterly", method = "denton-cholette")
m3 <- td(financial ~ 1, to = "quarterly", method = "denton-cholette")
m4 <- td(credit ~ 1, to = "quarterly", method = "denton-cholette")
m5 <- td(ll ~ 1, to = "quarterly", method = "denton-cholette")
m6 <- td(hhcongdp ~ 1, to = "quarterly", method = "denton-cholette")
m7 <- td(hhconpc ~ 1, to = "quarterly", method = "denton-cholette")
m8 <- td(trade ~ 1, to = "quarterly", method = "denton-cholette")
m9 <- td(gdpg ~ 1, to = "quarterly", method = "denton-cholette")
m10 <- td(gdppccd ~ 1, to = "quarterly", method = "denton-cholette")
m11 <- td(gdppckd ~ 1, to = "quarterly", method = "denton-cholette")
m12 <- td(gdppcg ~ 1, to = "quarterly", method = "denton-cholette")
m13 <- td(phcr ~ 1, to = "quarterly", method = "denton-cholette")
m14 <- td(gini ~ 1, to = "quarterly", method = "denton-cholette")


##Now pridict 

qbmoney <- (m1$values)
qinf <- (m2$values)
qfinancial <- (m3$values)
qcredit <- (m4$values)
qll <- (m5$values)
qhhcongdp <- (m6$values)
qhhconpc <- (m7$values)
qtrade <- (m8$values)
qgdpg <- (m9$values)
qgdppccd <- (m10$values)
qgdppckd <- (m11$values)
qgdppcg <- (m12$values)
qphcr <- (m13$values)
qgini <- (m14$values)


#merging all interpolated series
moneyinf <- ts.union(qbmoney, qinf,qfinancial, qcredit,qll,qhhcongdp,qhhconpc,qtrade,qgdpg,qgdppccd,qgdppckd,qgdppcg)

creditogdp <- ts.union(qcredit,qgdpg)
creditogdp <- ts.union(qcredit,qgdpg)
creditogdp
as.data.frame(creditogdp)
creditogdp <- ts(creditogdp, start=c(1971, 1), end=c(2015, 4), frequency=4) ##it will set whole data but it will not work 


##create time series individual objects for each of the variables
myts <- ts(moneyinf, start=c(1971, 1), end=c(2015, 4), frequency=4) ##it will set whole data but it will not work 


##adding quarterly dates to the merged times series objects..........
library(zoo)
dat <- ts(data=plotting, frequency = 4, start = c(1971, 1))
data.frame(Y=as.matrix(dat), date=as.Date(as.yearmon(time(dat))))

plotting <- data.frame(date = as.Date(as.yearmon(time(dat))), Y = coredata(dat))
plot(plotting)

library(ggplot2)
mytsDF <- data.frame(data = plotting, date = date)
View(plotting)

ggplot(plotting, aes(date, Y.qbmoney)) + geom_line() +
  scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "4 months") + 
  xlab("") + ylab("y") + ggtitle("Time Series Plot")

dates_label = as.character(plotting$date)
plot(x = date, Y.qbmoney, las = 2, xaxt = "n", xlab = "", type = "l")
axis(1, at = date, labels = dates_label, las = 2, cex.axis = .85)

#Creating a loop to do a unit root test
nlag <- NULL  #creating null vectors/columsn
testvalue <- NULL
for (i in 1:2 ) { #for two columns
  x = ur.df(moneyinf[, i+1], type = "drift", selectlags = "AIC")
  a = x@lags #extract the lags and save as object a
  nlag = rbind(nlag, a) #put it into the empty vector created earlier
  b = x@teststat[1]
  testvalue = rbind(testvalue, b)
}

##producing a table
stationarytest <- cbind(colnames(moneyinf[-1]), nlag, testvalue)

##making a data frame
qbmoney <- ts(qbmoney, start=c(1971, 1), end=c(2015, 4), frequency=4) ##it will set whole data but it will not work 
qbmoney@value

##proceed to Unit root analysis
library(urca)

dff <- ur.df(qbmoney, type = "drift", selectlags = "AIC")
dff
summary(dff)
dff@cval

kp.qbmony <- ur.kpss(qbmoney)
plot(kp.qbmony)

za.qbmony <- ur.za(qbmoney, model = "both", lag = 3)
summary(za.qbmony)
plot(za.qbmony)


#filling missing values in poverty and gini
#https://stackoverflow.com/questions/4964255/interpolate-missing-values-in-a-time-series-with-a-seasonal-cycle

library(forecast)
iphcr <- na.interp(phcr)
igini <- na.interp(gini)

fit[10]  
plot(igini)

library(urca)
pakdata$Year <- as.numeric(pakdata$Year)
attach(pakdata)


##adding quarterly dates to the merged times series objects..........



library(zoo)
dat <- ts(data=creditogdp, frequency = 4, start = c(1971, 1))
data.frame(Y=as.matrix(dat), date=as.Date(as.yearmon(time(dat))))

creditogdp <- data.frame(date = as.Date(as.yearmon(time(dat))), Y = coredata(dat))
plot(creditogdp)


qcredit1 <- creditogdp[, c("Y.qcredit","date")]
za.qcredit <- ur.za(qcredit1$Y.qcredit, model = "both", lag = 3)
plot(za.qcredit)

summary(za.qcredit)

yrs <- qcredit1$date[-length(qcredit1$date)]
plot.ur.za(Time=yrs, x=za.qcredit)


#modified code for za plot
plot.ur.za <- function (Time, x, ...) 
{
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(1, 1))
  yvals <- sort(c(x@cval, x@tstats))
  #xvals <- pretty(1:n)
  plot.ts(Time, x@tstats, main = "Zivot and Andrews Unit Root Test", 
          ylab = "t-statistics for lagged endogenous variable", type="l",
          ylim = c(min(yvals), max(yvals)), xy.labels=F, xy.lines=T)
  abline(h = x@cval, col = c("red", "blue", "seagreen"))
  if (x@teststat < x@cval[3]) {
    abline(v = Time[x@bpoint], col = "red", lty = 2)
  }
  mtext(paste("Model type:", x@model, sep = " "), side = 1, 
        line = 4)
  n <- length(Time)
  legend(x = Time[n], y = max(yvals), c("1% c.v.", "2.5% c.v.", 
          "5% c.v."), col = c("red", "blue", "seagreen"), xjust = 1, 
         yjust = 1, lty = 1, horiz = TRUE, cex = 0.66, bty = "n")
}

#Financial Instability by taking sd of growth rate or average absolute value of the residual

finslm <- lm(log(credit) ~ log(gdpg), na.action=NULL)
summary(finslm)
fitted(finslm)
residuals(finslm)
plot(finslm)

#packages for time series
#https://cran.r-project.org/web/views/TimeSeries.html
library(dynlm)
logcredit <- log(qcredit)
loggdpc <- log(qgdppckd)
finslm <- dynlm(logcredit ~ L(logcredit,1) + trend(logcredit, scale = FALSE), na.action=NULL)
summary(finslm)

#Compute financial instability of credittogdp
finslm <- dynlm(loggdpc ~ L(logcredit,1) + trend(logcredit), na.action=NULL)
resfd <- (finslm$residuals)
absresfd <- abs(resfd)
fins <- absresfd/(179)
fins
plot(log(fins))

##compute gdp volatility
for(i in 4: length(qgdpg)) { 
  j[i] = sd(qgdpg[i-3:i]) 
} 

print(j)


##Markove switching...
library(MSwM)
mod=lm(qhhcongdp ~ qcredit)
plot(mod)
summary(mod)
plot(ts(qhhcongdp))
mod.mswm=msmFit(mod,k=2,p=1,sw=c(T,T,T,T),control=list(parallel=F))
summary(mod.mswm)
plotProb(mod.mswm,which=1)
plotProb(mod.mswm,which=2)
plotReg(mod.mswm,expl="qcredit")

mod=lm(qgdppckd ~ 1)
plot(mod)
plot(ts(qgdppckd))
mod.mswm=msmFit(mod,k=2,p=1)
mod.mswm=msmFit(mod, k=2, sw=c(T,T), p=0)
plotProb(mod.mswm,which=1)
plotProb(mod.mswm,which=2)


##Threshold cointegration and Non-linear Autoregression VAR....
#https://cran.r-project.org/web/packages/tsDyn/vignettes/ThCointOverview.pdf
data(IIPUs)
setar(IIPUs, m=16, thDelay=5, th=0.23)















#https://cran.r-project.org/web/packages/tsDyn/vignettes/tsDyn.pdf

library(tsDyn)
data(lynx)
grid<-selectSETAR(lynx, m=1, thDelay=0, trim=0.15, criterion="SSR")
grid<-selectSETAR(gdppcapcd, m=1, thDelay=0, trim=0.15, criterion="SSR")
print(grid)
plot(grid)
set<-setar(gdppcapcd, m=1, thDelay=0, th=grid$th)
summary(set)
selectSETAR(qhhcongdp, m=1, thDelay=0, trim=0.15, criterion="SSR", nthresh=2)
selectSETAR(qhhcongdp, m=6, thDelay=0, trim=0.15, criterion="AIC", same.lags=TRUE)
print(grid)
plot(grid)


data(zeroyld)
tvecm<-TVECM(zeroyld, nthresh=2,lag=1, ngridBeta=60, ngridTh=30, plot=TRUE,trim=0.05, beta=list(int=c(0.7, 1.1)))
data(IIPUs)
set<-setar(IIPUs, m=16, thDelay=5, th=0.23)
head(IIPUs)
Hansen.Test<-setarTest(lynx, m=1, nboot=1000)
data(zeroyld)
dat<-zeroyld
testSeo<-TVECM.SeoTest(dat, lag=1, beta=1, nboot=1000)
summary(testSeo)
head(qgdppckd)

#BDS Test of non-linearity....
library(fNonlinear)
bdsTest(qhhcongdp, m = 3, eps = NULL)  # i.i.d. example
plot(qhhcongdp, type = "l", main = "iid Time Series")

library(tsDyn)
#SETAR
par(mfrow=c(2,1))
ndx.acf = acf(log(qhhcongdp))
ndx.pacf = acf(log(qhhcongdp), type="partial")
bdsTest(log(fins), method="threshold", p=2, d=1:2)
setar(qhhcongdp, m=2, thDelay=1, th=2.3)

llynx <- log10(lynx)
selectSETAR(llynx, m=2)
#Suggested model is the following:
aa = setar(llynx, m=2, thDelay=1, th=3.4)

summary(aa)

