##################################################
####Financial Markets and Terorrism in Pakistan###
##################################################
getwd()
setwd("/Users/rizwanmushtaq/Dropbox/LJE_paper/data") ##For MAC
setwd("D:/Utilisateurs/e0g411m05t7/Dropbox/LJE_paper/data") ##FOR LAB


##Reading data from excel,We can do it using package
library(XLConnect)
##OR 
library(readxl)


paktr <- read.csv("gtd_pak.csv" , header= TRUE)  ## reading data from stata
paktr <- read.csv("gtd_pak.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
View(paktr)

##removing observations based on monthly data
paktr[-c(1:1742), ] ##lists without deleted observations
paktr <- paktr[-c(1:1741), ] ## Deletes the rows from 1 to 1741
paktr <- ts(paktr, start=c(1970, 11), end=c(2015, 12)) 

warnings() 
suppressWarnings()

plot(paktr[,98],type="l")

plot(paktr[,11:12], type="b",
     lwd=2, col="red", xlab="time", ylab="nwound", main="no kls", ylim=c(0,60) )

View(paktr)

##to check if data has missing values
is.na(paktr)

##Install package to load Financial Data
install.packages("quantmod")


getSymbols("QQQ")
head(QQQ)
tail(QQQ)

# use single quotes and specify data source:
getSymbols("YHOO", src = "google")  # src = "yahoo" is the default

#We can then extract the closing prices to an R vector:
# coerce from an xts object to a standard numerical R vector:
as.vector(YHOO[, "YHOO.Close"])  

#Finally, we can calculate the daily log returns in one fell swoop:
log.yahoo <- log(yhoo.close[-1]/yhoo.close[-length(yhoo.close])
# Check:
head(log.yahoo)
tail(log.yahoo)

##Loading Kse data
getSymbols("KSE")  # src = "yahoo" is the default

##"QUANTMOD" only extract the stock prices from 2007,
##Quandl
install.packages("Quandl")

Quandl.api_key("LDPM2fxQnp5jDRKGckdr")

# Oil futures price data from the National Stock Exchange of India:
mydata = Quandl("NSE/OIL")
head(mydata)
tail(mydata)
date()
mydata = Quandl("^KSE")
help(quantmod)
mydata = Quandl("OPEC/ORB")
mydata = Quandl("FRED/GDP", start_date="2001-12-31", end_date="2005-12-31")

##Downloading macroeconomic indicators from WDI directly

dat = WDI(indicator=c("BX.KLT.DINV.WD.GD.ZS", "NY.GDP.MKTP.KD.ZG" , "NE.TRD.GNFS.ZS"), country=c('PK'), start=1970, end=2015)
head(dat)


ggplot(dat, aes(year, NY.GDP.PCAP.KD, NY.GDP.MKTP.KD.ZG, color=country)) + geom_line() + xlab('Year') + ylab('GDP per capita')



##time series volatility plots
#https://plot.ly/ggplot2/time-series/

#do exammple
#http://yunus.hacettepe.edu.tr/~iozkan/eco665/archgarch.html


oil<-read.csv("http://yunus.hacettepe.edu.tr/~iozkan/data/oilcsv.csv", header=T, sep=";")

head(oil)
tail(oil)

# install.packages("zoo")
library(zoo)
oil<-oil[,-4]
oil.zoo=zoo(oil[,-1], order.by=as.Date(strptime(as.character(oil[,1]), "%d.%m.%Y")))
plot(oil.zoo, main="Brent and Crude Price Series")

#Let's import other series namely BIST100 index and TL/USD series.

xu100<-read.csv("http://yunus.hacettepe.edu.tr/~iozkan/data/endXU100.csv", header=T, sep=";")
head(xu100)
usd<-read.csv("http://yunus.hacettepe.edu.tr/~iozkan/data/usd.csv", header=T, sep=";")
head(usd)
#http://www.portfolioprobe.com/2012/07/06/a-practical-introduction-to-garch-modeling/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+PortfolioProbeRLanguage+%28Portfolio+Probe+%C2%BB+R+language%29
#http://www.analyticsresearch.net/Documents/R%20CODES%20FOR%20GARCH%20PROCESS.pdf
library(fGarch)
library(timeSeries)
john<-as.timeSeries(Dataset)

#http://www.analyticsresearch.net/Documents/2013AFEsession5.Practicals.pdf
##input your Dataset using "Rcmdr" package???
library(Rcmdr)
library(rmgarch)
library(timeSeries)
library(fAssets)
john=as.timeSeries(Dataset)
plot(john,main="Graphical Analysis of My Data")
# HISTOGRAM PLOTS OF ALL ASSETS IN THE PORTFOLIO

##http://unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf
install.packages("rugarch")
data(sp500ret)

library(tseries)
sp500.prices=get.hist.quote(
  instrument = "^GSPC",
  quote = "Adj",
  provider = c("yahoo"), method = NULL,
  origin = "1899-12-30", compression = "d",
  retclass = c("zoo"), quiet = FALSE, drop = FALSE
)
sp500=as.data.frame(sp500.prices)
N=length(sp500[,1])
sp500.returns=100*(log(sp500[2:N,])-log(sp500[1:(N-1),]))

modelfit=ugarchfit(spec=model,data=sp500ret)


##Another latest package for wb data download
library(wbstats)
library(data.table)
myDT <- data.table(
  wb(indicator = c("SP.POP.TOTL",
                   "SP.DYN.LE00.IN",
                   "SP.DYN.TFRT.IN"), country=("PK"), mrv = 60)
)

myDT <- wb(country = c("IN"), indicator =c( "EG.ELC.ACCS.ZS","SP.POP.TOTL"), mrv = 35, gapfill = TRUE)


ggplot(myDT, aes(date, SP.POP.TOTL, color=country)) + geom_line() + xlab('Year') + ylab('GDP per capita')

help("wbstats-package")
browseVignettes(package = "wbstats")

data(EuStockMarkets) # data on European stock markets
plot(EuStockMarkets[,"DAX"])  # data from Germany
dax <- diff(log(EuStockMarkets))[,"DAX"] 
plot(dax)
acf(dax)
acf(dax^2)
hist(dax)
qqnorm(dax)
qqline(dax)


##https://rpubs.com/VijayaG/110769



##http://shishirshakya.blogspot.fr/2015/07/garch-model-estimation-backtesting-risk.html
install.packages(c("tseries", "zoo", "forecast", "FinTS", "rugarch"))
Now, load these packages in your R.
library("tseries")
library("zoo")
library("forecast")
library("FinTS")
library("rugarch")

#Downloading data for KSE-100 Index and return and zoo object
KSE.data = get.hist.quote(instrument="^KSE", start="2000-01-01",  end="2015-12-31", quote="AdjClose", provider="yahoo", compression="d", retclass="zoo")
head(KSE.data)
##plot the data

par(mfrow=c(2,2)) ##two rows two columns 2by2 #par(mfrow=c(2,2))
plot(KSE.data, main = "Karachi Stock Exchange Closing Prices", ylab = "Price", xlab = "Date")

#STEP-3: Getting the Returns of the Data
#Let's take the log return of KSE.data and name the object ans KSE.ret and plot the data. You can see in the Fig-2 that high volatile days are followed by high volatile days and low volatile days by low volatile days.

KSE.ret <- diff(log(KSE.data)) * 100
plot(KSE.ret, main = "Daily Returns", xlab = "Date", ylab = "Return in percent")
par(mfrow=c(1,1))

#STEP-4: Finding Best Mean Model Using ARIMA
#Now, let's try to fit a best ARIMA (p,d,q) model based upon the Bayesian Information Criterions. Use the following command. Note that returns are always reverting to some mean returns hence you should expect the returns to be I(0). Hence, the ARIMA is actual only an ARMA (p,q) process. Get the intuition on stationary (here).


fit1 <- auto.arima(KSE.ret, trace=TRUE, test="kpss",  ic="bic")

#STEP-5: Test for ARCH Effect
#However to proceed ahead with GARCH model, the residual of ARIMA must need to have the ARCH effect. To test the ARCH, lets perform the Ljung-Box test on the first 12 lags of the squared residuals of the best ARIMA model under the null hypothesis of no ARCH effects.

Box.test(fit1$residuals^2,lag=12, type="Ljung-Box")

#If p-value of Ljung-Box test is smaller than 5% level of significance then there exist the ARCH effect which shows the green light to proceed ahead to GARCH.

STEP-6: Developing a GARCH Model
Instead to fit a GARCH(P,Q), in this blog, I will only fit the GARCH(1,1) for the sake of simplicity. For GARCH theory, check my blog (here).  Lets specify object called res_garch11_spec in which we want to develop a GARCH(1,1) on ARIMA(1,0,1) or in our case ARMA(1,1).

res_garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(3, 0)))

#Then, lets fit such specification in the data.

res_garch11_fit <- ugarchfit(spec = res_garch11_spec, data = KSE.ret)
res_garch11_fit

res_garch11_roll <- ugarchroll(res_garch11_spec, KSE.ret, n.start = 120, refit.every = 1, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)
report(res_garch11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)

STEP-9: Plotting the VaR Limits
A plot of the backtesting performance can also be generated easily using following command and selecting 2.

plot(res_garch11_fit)
selection: 2

STEP-10: Forecasting Risk and VaR
For forecasting we can implement following command. Here we just forecast for 12 time period ahead.

res_garch11_fcst <- ugarchforecast(res_garch11_fit, n.ahead = 12)
res_garch11_fcst


## combining graphs

grid <- matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE)
grid
layout(grid)
plot(KSE.data, main = "Karachi Stock Exchange Closing Prices", ylab = "Price", xlab = "Date")
plot(KSE.ret, main = "Daily Returns", xlab = "Date", ylab = "Return in percent")
plot(res_garch11_fit)

###End Code####












# econ589multivariateGarch.r
#
# R examples for lectures on multivariate GARCH models
#
# Eric Zivot
# May 8th, 2012
# update history


# load libraries
library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(car)
library(FinTS)
library(rmgarch)
options(digits=4)


# download data
symbol.vec = c("MSFT", "^GSPC")
getSymbols(symbol.vec, from ="2000-01-03", to = "2012-04-03")
colnames(MSFT)
start(MSFT)
end(MSFT)

# extract adjusted closing prices
MSFT = MSFT[, "MSFT.Adjusted", drop=F]
GSPC = GSPC[, "GSPC.Adjusted", drop=F]

# plot prices
plot(MSFT)
plot(GSPC)

# calculate log-returns for GARCH analysis
MSFT.ret = CalculateReturns(MSFT, method="log")
GSPC.ret = CalculateReturns(GSPC, method="log")


# remove first NA observation
MSFT.ret = MSFT.ret[-1,]
GSPC.ret = GSPC.ret[-1,]
colnames(MSFT.ret) ="MSFT"
colnames(GSPC.ret) = "GSPC"

# create combined data series
MSFT.GSPC.ret = merge(MSFT.ret,GSPC.ret)

# plot returns
plot(MSFT.ret)
plot(GSPC.ret)

# scatterplot of returns
plot( coredata(GSPC.ret), coredata(MSFT.ret), xlab="GSPC", ylab="MSFT",
      type="p", pch=16, lwd=2, col="blue")
abline(h=0,v=0)

#
# compute rolling correlations
#
# chart.RollingCorrelation(MSFT.ret, GSPC.ret, width=20)

cor.fun = function(x){
  cor(x)[1,2]
}

cov.fun = function(x){
  cov(x)[1,2]
}

roll.cov = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cov.fun, width=20,
                     by.column=FALSE, align="right")

roll.cor = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cor.fun, width=20,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="20-day rolling covariances",
     ylab="covariance", lwd=2, col="blue")

grid()

abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")

plot(roll.cor, main="20-day rolling correlations",
     ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))

#
# calculate EWMA covariances and correlations
#
lambda <- 0.94
cov.ewma <- covEWMA(as.data.frame(MSFT.GSPC.ret), lambda=.94, return.cor = T)

## 2. extract conditional variance and correlation
### conditional variance
MSFT.GSPC.cond.cov <- cov.ewma[,2,1];
### conditional correlation
t <- length(cov.ewma[,1,1]);
MSFT.GSPC.cond.cor<- rep(0,t);
for (i in 1:t) {
  MSFT.GSPC.cond.cor[i]<- cov2cor(cov.ewma[i,,])[1,2];
}
### Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cov,
     type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
     main="EWMA Covariance between MSFT and S&P500");
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cor,
     type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
     main="EWMA Correlation between MSFT and S&P500");
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))

# compute rolling covariances and correlations using longer window
roll.cov = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cov.fun, width=252,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cor.fun, width=252,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="252-day rolling covariances",
     ylab="covariance", lwd=2, col="blue")
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(roll.cor, main="252-day rolling correlations",
     ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))



# compute EWMA covariances and correlations using longer half-life
half.life = 125 
lambda = exp(log(0.5)/half.life)
cov.ewma <- covEWMA(MSFT.GSPC.ret, lambda=lambda)


## 2. extract conditional variance and correlation
### conditional variance
MSFT.GSPC.cond.cov <- cov.ewma[,2,1]
### conditional correlation
t <- length(cov.ewma[,1,1])
MSFT.GSPC.cond.cor<- rep(0,t)
for (i in 1:t) {
  MSFT.GSPC.cond.cor[i]<- cov2cor(cov.ewma[i,,])[1,2]
}
### Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cov,
     type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
     main="EWMA Covariance between MSFT and S&P500")
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cor,
     type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
     main="EWMA Correlation between MSFT and S&P500")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))


#
# DCC estimation
#

# univariate normal GARCH(1,1) for each series
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), 
                          distribution.model = "norm")

# dcc specification - GARCH(1,1) for conditional correlations
dcc.garch11.spec = dccspec(uspec = multispec( replicate(2, garch11.spec) ), 
                           dccOrder = c(1,1), 
                           distribution = "mvnorm")
dcc.garch11.spec

dcc.fit = dccfit(dcc.garch11.spec, data = MSFT.GSPC.ret)
class(dcc.fit)
slotNames(dcc.fit)
names(dcc.fit@mfit)
names(dcc.fit@model)

# many extractor functions - see help on DCCfit object
# coef, likelihood, rshape, rskew, fitted, sigma, 
# residuals, plot, infocriteria, rcor, rcov
# show, nisurface

# show dcc fit
dcc.fit

# plot method
plot(dcc.fit)
# Make a plot selection (or 0 to exit): 
#   
# 1:   Conditional Mean (vs Realized Returns)
# 2:   Conditional Sigma (vs Realized Absolute Returns)
# 3:   Conditional Covariance
# 4:   Conditional Correlation
# 5:   EW Portfolio Plot with conditional density VaR limits

# conditional sd of each series
plot(dcc.fit, which=2)

# conditional correlation
plot(dcc.fit, which=4)

# extracting correlation series
ts.plot(rcor(dcc.fit)[1,2,])

#
# forecasting conditional volatility and correlations
#

dcc.fcst = dccforecast(dcc.fit, n.ahead=100)
class(dcc.fcst)
slotNames(dcc.fcst)
class(dcc.fcst@mforecast)
names(dcc.fcst@mforecast)

# many method functions - see help on DCCforecast class
# rshape, rskew, fitted, sigma, plot, rcor, rcov, show

# show forecasts
dcc.fcst

# plot forecasts
4


###EVT Approach from book, An introduction to Quantitative Finance with R
install.packages("evir")
library(evir)
data(danish)
head(danish)
tail(danish)
summary(danish)
hist(danish, breaks = 200, xlim = c(0,20))
sum(danish>20) / length(danish)
sum(danish[danish>20]) / sum(danish)
emplot(danish)
emplot(danish, alog = "xy")
meplot(danish, omit = 4)
gpdfit <- gpd(danish, threshold = 10)
gpdfit$converged
gpdfit$par.ests
gpdfit$par.ses
plot(gpdfit)
