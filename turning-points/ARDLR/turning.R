###turning oints====

#http://macrofinance.nipfp.org.in/releases/PPS2016_india_dating.html
library(mFilter)
library(BCDating)
library(xtable)
library(zoo)
getwd()

##--Reading the seasonally adjusted old GDP series
gdp <- read.csv("/Users/rizwanmushtaq/Dropbox/ThesisOld/Rdatapaper1/ARDLR/PPS2016_india_gdp_sa.csv")
gdp$X <- NULL
ref.series.entire.gdp <- ts(gdp[,"in.gdp.fc.const.sa"], freq=4, start=c(1996,2))

## Function for computing standardised cyclical component
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

tmp <- standard.cyclical.con(ref.series.entire.gdp)

##--Turning point dates
dat <- BBQ(tmp)
peak <-  data.frame(as.yearqtr(index(tmp)[dat@peaks]))
trough <- data.frame(as.yearqtr(index(tmp)[dat@troughs]))
dating <- data.frame(matrix(NA,ncol=2,nrow=max(nrow(peak),nrow(trough))))
## if else to add  NA where the data for peak or trough is not available 
if (nrow(peak)==nrow(dating)){
  dating[,1] <- peak
}else {
  dating[,1] <- rbind(rep(NA,(nrow(dating)-nrow(peak))),peak)
}
if (nrow(trough)==nrow(dating)){
  dating[,2] <- trough
}else {
  dating[,2] <- rbind(trough,rep(NA,(nrow(dating)-nrow(trough))))
}
colnames(dating) <-c("peak", "trough")
dating <- zoo(dating)
xtable(dating)
head(dating)
plot(dating)
##--This gives the table showing dates of recession