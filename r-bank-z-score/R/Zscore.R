#===========================================================
#===========Bank Risks and Financial Inclusion==========#@#$
#===========================================================

setwd("D:/Utilisateurs/e0g411m05t7/Dropbox/BankZscore/R") ##FOR LAB
getwd()
##For MAC
setwd("~/Dropbox/BankZscore/R")

zscore <- read.csv("mydata.csv", header= TRUE,check.names=F) # reading data from csv
attach(zscore)

fdind <- read.csv("fdind.csv", header= TRUE,check.names=F) # reading data from csv

library(xlsx)
mydata <- read.xlsx("c:/myexcel.xlsx", 1)

# read in the worksheet named mysheet
mydata <- read.xlsx("~/Dropbox/BankZscore/Data/extrawork.xlsx", sheetName = "DOB",check.names=F)
str(dob)
#Reshape world bank data
library(reshape2)
library(data.table)
##declare as dataframe
as.data.frame(aaa)

dob <- dcast(melt(setDT(zscore), id.var=c("Indicator", "code"), 
      variable.name="year"), 
      code+year~Indicator, value.var='value')

##subsetting dataset
subset1 <- subset(aaa, year==2012:2014)

##taking averages of panel data
library(plyr)
library(zoo)
library(pwt)


as.character(year)
head(aaa$year)
str(aaa)
aaa$year <- as.numeric(as.character(aaa$year))
attach(aaa)
aaa$period <- cut(aaa$year, seq(1900, 2100, 3)) 
aaa.ma <- ddply(aaa, .(code, period), numcolwise(mean))
aaa.ma

aaa %>% 
  mutate(period = cut(aaa$year,seq(2000,2014,by=4),right=F)) %>% 
  group_by(code, period) %>% summarise(mean)


# Use rollmean() in zoo as basis for defining a rolling 5-period rolling mean
rollmean5 <- function(x){
  rollmean(x, 5)
}

# Use ddply() in plyr package to create rolling average per country
pwt.ma <- ddply(pwt, .(country), numcolwise(rollmean5))

class(aaa)

install.packages("plm")
library(plm)
library(foreign)
library(car)


attach(aaa)
class(aaa)

#SET DATA AS PANEL@@??

aa <- plm.data(aaa, index("code","year"))
(88.27323+87.74014+86.24752)/3

coplot(creditd ~ year|code, type="l", data=aaa.ma) # Lines
coplot(creditd ~ year|code, type="b", data=aaa.ma) # Lines
scatterplot(creditd ~ year|code, boxplots=FALSE, smooth=TRUE, reg.line=TRUE, data=aaa.ma)
plotmeans(aaa.ma$creditd ~ aaa.ma$code, main="Heterogeineityacross countries")
summary(aaa.ma)
##treat missing values
na.strings=c(".","","NA")

##pooled ols
pooling <- plm(gdppc~ creditd, data=aaa, model= "pooling", na.action = na.omit)
summary(pooling)

random <- plm(gdppc~ creditd , data=aaa, model= "random")
summary(random)
summary(olsreg <- lm(gdppc ~ bankaccntadult+creditd , data=aa))

plot(gdppc , bankaccntadult, main="Scatterplot", 
     xlab="Mobile Phone ", ylab="Credit Card", pch=19, cex=0.5,col="blue") ##cex,controls symbol size

is.na(creditd)

library(ggplot2)
library(lattice)
library() 
qplot(year, rating, data = movies, facets = Comedy ~ Action)
qplot(year, gdppc, data = aaa, facets = creditd )
xyplot(gdppc ~ year, data=aaa)
qplot(year, gdppc, data=aaa)


stripplot(~ creditd, data = aaa, jitter.data = TRUE)
qplot(creditd, 1, data = aaa, geom = "jitter")


histogram(~ creditd, data = aaa)
qplot(creditd, data = aaa, geom = "histogram")

bwplot(creditd ~ gdppc ,data = aaa)
qplot(factor(Comedy), rating, data = aaa, type = "boxplot")

xyplot(creditd ~ gdppc, aaa, type = c("p","smooth"))
qplot(creditd, gdppc, data = aaa, geom = c("point","smooth"))

xyplot( log(gdppc) ~ log(creditd) | code, aa, type = c("p","r"),
        xlab = "Log of Domestic Credit to Private Sector", ylab = "GDP per capita",
        main = "Correlation between Credit and GDP per capita")
      
qplot(log(creditd), log(gdppc), data = aa, geom = c("point","smooth"),
      method = "lm")

#############

library(ggplot2)
library(wbstats)

##search with wbsearch
wbsearch("gdp.*capita.*US\\$", cache = new_wb_cache)
wbsearch("Foreign.*direct.*investment", cache = new_wb_cache)
wbsearch("Total.*population", cache = new_wb_cache)

# country values: iso3c, iso2c, regionID, adminID, incomeID
new_data <- data.table(
          wb(country = c("PAK","IND", "BGD", "NPL", "LKA","EGY","BTN"),
          indicator = c("FB.CBK.BRWR.P3", "GFDD.SI.02","GFDD.EI.04","SP.POP.0024.TO.ZS","FS.AST.PRVT.GD.ZS","BN.KLT.DINV.CD.ZS","GFDD.OI.04","GFDD.SI.01","GC.DOD.TOTL.GD.ZS","GFDD.AI.01","GFDD.AI.02","FP.CPI.TOTL.ZG","NY.GDP.MKTP.KD.ZG","NY.GDP.PCAP.KD","BX.KLT.DINV.WD.GD.ZS","SP.POP.TOTL","IC.EC.COST","NE.TRD.GNFS.ZS","IC.LGL.CRED.XQ"), 
          startdate = 2005, enddate = 2015))
head(new_data)

##Stdep two cleanging up the data
wb_countries <- wbcountries() 
names(wb_countries)

##merge contries and regions data with earlier dataset
#https://cengel.github.io/gearup2016/worldbank.html

new_data <- merge(new_data, y = wb_countries[c("iso2c", "region")], by = "iso2c", all.x = TRUE)
new_data <- subset(new_data, region != "Aggregates") # this also removes NAs

##converting
library(reshape2)
new_data <- dcast(new_data, iso2c + country + date + region ~ indicatorID,  value.var = 'value')

# Another way to Reshape data into a wide format
wDT <- reshape(
  new_data[, list(
  country, date, value, indicator)], 
  v.names = "value", 
  idvar=c("date", "country"), 
  timevar="indicator", direction = "wide")

names(new_data)

# Turn date, here year, from character into integer
new_data[, date := as.integer(date)]

#Renaming Variabels
setnames(new_data, names(new_data),
         c("code", "country",  "year","region","fdigdp","fdinfl" ,"borcombnk","inf","credit",
           "govtdebt", "baccnt" ,"bbranch","bocost","lerner" ,"zcore","npl", 
           "enfcost", "strthlgl", "tradegdp","gdpg","gdppcap","popyouth" ,"poptot"))

##Another way to rename variables
new_data$indicatorID[new_data$indicatorID == "NY.GDP.PCAP.KD"] <- "GDP"

##plotinng dataaaaaaaaaaaaaaaaaaaaaaaaaa
ggplot(new_data, aes(year, credit, color=country)) + geom_line() 
+    xlab('Year') + ylab('Financial Development') 
+    labs(title = "Domestic Credit to Private Sector percentage of GDP")

##Ploting data for a particular year
ggplot(subset(new_data, year == "2014"), aes(x = credit, y = gdpg)) + geom_point()

##Plot with the name of countries on the graph
ggplot(subset(new_data, year == "2014"), aes(x = credit, y = gdpg, color = country == "Pakistan")) + 
  geom_point() +    
  geom_text(aes(label = country), size=3, nudge_y = 0.4) +
  scale_x_continuous(limits = c(0, 80))

##ggplot with xlab and ylab
ggplot(subset(new_data, year == "2014"), aes(x = credit, y = gdpg)) + geom_point() +
  xlab("Domestic Credit ratio of GDP") +
  ylab("GDP growth ") +
  ggtitle("Financial Development and Economic Growth")

##Plot with the name of countries on the graph COLOR
ggplot(subset(new_data, year == "2014"), aes(x = log(credit), y = gdpg, color = country == "Pakistan")) + 
  geom_point() +  
  geom_text(aes(label = country), size=3, nudge_y = 0.4) +
  scale_x_continuous(limits = c(2.5, 4.5)) +
        xlab("Domestic Credit ratio of GDP") +
        ylab("GDP growth ") +
        ggtitle("Financial Development and Economic Growth")
                     
##More Advanced Interactive plot of financial Development and Economic Growth

library(plotly)

g <- ggplot(subset(new_data, year >= "2010" & year <= "2015"), aes(x = credit, y = gdpg, color = country == "Pakistan", tooltip = country)) + 
  geom_point() + 
  geom_text(aes(label = country), size=3, nudge_y = 0.2) +
  facet_wrap(~ year) + 
  theme(legend.position="none")

ggplotly(g)

##download some additional series
forn_data <- data.table(
  wb(country = c("PAK","IND", "BGD", "NPL", "LKA","EGY","BTN"),
  indicator = c("GFDD.OI.15", "GFDD.OI.16"), 
  startdate = 2005, enddate = 2015))
head(forn_data)

forn_data <- merge(forn_data, y = wb_countries[c("iso2c", "region")], by = "iso2c", all.x = TRUE)
forn_data <- subset(forn_data, region != "Aggregates") # this also removes NAs

forn_data <- dcast(forn_data, iso2c + country + date + region ~ indicatorID,  value.var = 'value')

forn_data[, date := as.integer(date)]

setnames(forn_data, names(forn_data),
         c("code", "country",  "year","region","forbank","forbankasset"))

##merge two datasets

forn_data[, year := as.integer(year)]
new_data[, year := as.integer(year)]

verynew <- merge(new_data,forn_data, by=c("country","year")) 

mydata <- merge(mydata1, mydata2, by=c("country","year")) 


##OKALY NOW FINALLY HAVE TO DO THE ANALYSIS
ggplot(new_data, aes(year, zcore, color=country)) + geom_line() 
+    xlab('Year') + ylab('Bank Z Score') 
+    labs(title = "Bank distance-to-distress")


##Plot with the name of countries on the graph
ggplot(subset(new_data, year == "2014"), aes(x = credit, y = zcore, color = country == "Pakistan")) + 
  geom_point() +    
  geom_text(aes(label = country), size=3, nudge_y = 0.4) +
  scale_x_continuous(limits = c(0, 80))

##matrix Credit expansion and bank zscore
g <- ggplot(subset(newdata, year > "2010" & year <= "2014"), aes(x = log(credit), y = log(zcore), color = country == "Pakistan", tooltip = country)) + 
  geom_point() + 
  geom_text(aes(label = country), size=3, nudge_y = 0.2) +
  facet_wrap(~ year) + 
  scale_x_continuous(limits = c(2.5, 4.5)) +
  xlab('Credit Expansion') + ylab('Bank Z Score') +
  labs(title = " ") +
  theme(legend.position="none")
ggplotly(g)

##matrix Credit expansion and bank zscore
g <- ggplot(subset(newdata, year > "2010" & year <= "2014"), aes(x = bbranch, y = log(zcore), color = country == "Pakistan", tooltip = country)) + 
  geom_point() + 
  geom_text(aes(label = country), size=3, nudge_y = 0.2) +
  facet_wrap(~ year) + 
  scale_x_continuous(limits = c(0, 20)) +
  xlab('Bank branches per 100,000 adults') + ylab('Bank Z Score') +
  labs(title = " ") +
  theme(legend.position="none")
ggplotly(g)


panell <- subset(aaa, year==2012:2014)

newdata <- subset(mydata, country=="m" & age > 25)
names(country)

##new dataset by excluding rows 12 to 22
newdata <- new_data[c(-12:-22), ]
cros <- subset(new_data, year==2014)

##OLS using PLM package
data("Produc", package = "plm")
zz <- plm(gsp ~ unemp + lag(gsp), data = Produc, index = c("state","year"), method = "within", effect = "twoways")
summary(zz)

m_zscore <- plm(zcore ~ credit*enfcost + lag(gdpg)+ lag(tradegdp)+ lag(gdppcap)+ log(poptot), data = new_data, index = c("country","year"))
summary(m_zscore)

##Regression and plot of interact between credit expansion and zscore

m_enf <- lm(zcore ~ credit*enfcost + lag(gdpg)+ lag(tradegdp)+ lag(gdppcap)+ log(poptot), data = new_data, index = c("country","year"))
summary(m_enf)

m_enf <- lm(zcore ~ enfcost*credit + gdpg+ tradegdp+ gdppcap+ log(poptot), data = new_data, index = c("country","year"))
summary(m_enf)


interplot(m = m_zscore, var1 = "credit", var2 = "enfcost")
interplot(m = m_zscore, var1 = "credit", var2 = "enfcost") + 
  # Add labels for X and Y axes
  xlab(" Contract enforcement cost (% of claim)") +
  ylab("Estimated Coefficient for\nCredit Expansion") +
  # Change the background
  theme_bw() +
  # Add the title
  ggtitle("Estimated Coefficient of Credit Expansion \non Bank Z-score by Cost of contract enforcement") +
  theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")


m_enf <- lm(zcore ~ bbranch*enfcost +credit + gdpg+ tradegdp+ gdppcap+ log(poptot), data = fdinx, index = c("country","year"))
summary(m_enf)

interplot(m = m_enf, var1 = "bbranch", var2 = "enfcost")
interplot(m = m_enf, var1 = "bbranch", var2 = "enfcost") + 
  # Add labels for X and Y axes
  xlab(" Contract enforcement cost (% of claim)") +
  ylab("Estimated Coefficient for\nCredit Expansion") +
  # Change the background
  theme_bw() +
  # Add the title
  ggtitle("Estimated Coefficient of Credit Expansion \non Bank Z-score by Cost of contract enforcement") +
  theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")


data(mtcars)  #load the data
m_cyl <- lm(mpg ~ hp+drat+ disp+ wt*cyl, data = mtcars)
summary(m_cyl)
library(interplot)

interplot(m = m_cyl, var1 = "cyl", var2 = "wt")
interplot(m = m_cyl, var1 = "cyl", var2 = "wt") + 
  # Add labels for X and Y axes
    xlab("Automobile Weight (thousands lbs)") +
    ylab("Estimated Coefficient for\nNumber of Cylinders") +
  # Change the background
    theme_bw() +
  # Add the title
    ggtitle("Estimated Coefficient of Engine Cylinders \non Mileage by Automobile Weight") +
    theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
    geom_hline(yintercept = 0, linetype = "dashed")


##Another way to plot interaction term
library(sjPlot)
library(sjmisc)
sjp.int(m_enf, type = "cond")
sjp.int(m_enf, type = "eff")
m_cyl <- lm(mpg ~ hp+drat+ disp+ cyl*wt, data = mtcars)
sjp.int(m_enf, type = "eff", facet.grid = TRUE)
sjp.int(m_enf, type = "cond", show.values = TRUE)

fit <- lm(weight ~ Time * Diet, data=ChickWeight, x=T)
summary(fit)



xyplot(log(gdppcap) ~ log(bbranch) | country, new_data, type = c("p","r"),
       xlab = "Bank branches per 100,000 adults", ylab = "GDP per capita",
       main = "Correlation between Bank Branches and GDP per capita")

xyplot(log(zcore) ~ forbank | country, verynew, type = c("p","r"),
       xlab = "Log of Domestic Credit to Private Sector", ylab = " Z score",
       main = "Correlation between Credit and Z score")



xyplot(log(credit) ~ forbank | country, verynew, type = c("p","r"),
       xlab = "Log of Domestic Credit to Private Sector", ylab = " Z score",
       main = "Correlation between Credit and Z score")

##OKALY NOW FINALLY HAVE TO DO THE ANALYSIS
ggplot(verynew, aes(year, forbank, color=country)) + geom_line() 
+    xlab('Year') + ylab('Bank Z Score') 
+    labs(title = "Bank distance-to-distress")

ggplot(subset(verynew, year == 2013), aes(year, enfcost, color=country)) + geom_line() 
+    xlab('Year') + ylab('Bank Z Score') 
+    labs(title = "Bank distance-to-distress")

##Plot with the name of countries on the graph
ggplot(subset(verynew, year == 2013), aes(x = forbank, y = log(credit), color = country == "Pakistan")) + 
  geom_point() +    
  geom_text(aes(label = country), size=4, nudge_y = 0.2) +
  scale_x_continuous(limits = c(0, 60)) +
  xlab("Foreign banks among total banks (%)") +
  ylab("Domestic credit to GDP ") +
  ggtitle(" ") +
  theme(legend.position="none")

##Plot with the name of countries on the graph
ggplot(subset(verynew, year == 2013), aes(x = forbank, y = log(zcore), color = country == "Pakistan")) + 
  geom_point() +    
  geom_text(aes(label = country), size=4, nudge_y = 0.2) +
  scale_x_continuous(limits = c(0, 60)) +
  xlab("Foreign banks among total banks (%)") +
  ylab("Domestic credit to GDP ") +
  ggtitle(" ") +
  theme(legend.position="none")

##matrix Credit expansion and bank zscore
g <- ggplot(subset(verynew, year > "2010" & year <= "2013"), aes(x = forbank, y = log(credit), color = country == "Pakistan", tooltip = country)) + 
  geom_point() + 
  geom_text(aes(label = country), size=4, nudge_y = 0.1) +
  facet_wrap(~ year) + 
  scale_x_continuous(limits = c(0, 60)) +
  xlab('Foreign banks among total banks (%)') + ylab('Domestic credit to GDP') +
  labs(title = " ") +
  theme(legend.position="none")
ggplotly(g)