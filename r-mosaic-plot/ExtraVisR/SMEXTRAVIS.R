##========SME PAKISTAN DATA VISUALIZATION============@@@

#Circle diagrams
getwd()

setwd("~/Dropbox/SME/ExtraVisR")
setwd("D:/Utilisateurs/e0g411m05t7/Dropbox/SME/ExtraVisR")
save.image("~/Dropbox/SME/ExtraVisR/pivotmosaicsme.RData")

library(DescTools)

tab <- matrix(c(2,5,8,3,10,12,5,7,15), nrow=3, byrow=FALSE)
dimnames(tab) <- list(c("ABCDEFG","BCDEFGH","CDEFGHI"), c("D","E","F"))

PlotCirc( tab,
          acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
          rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)


##Mosaic plot using ggplots......
#https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
data(NHANES)
ggplot(data = NHANES) +
  geom_mosaic(aes(weight = Weight, x = product(SleepHrsNight), fill=factor(SleepHrsNight)), na.rm=TRUE) +
  labs(x="Hours of sleep a night ", title='f(SleepHrsNight)') + guides(fill=guide_legend(title = "SleepHrsNight", reverse = TRUE))

ggplot(data = rpvt2) +
  geom_mosaic(aes(weight = 1, x = product(Constraint), fill=factor(Constraint)), na.rm=TRUE) +
  labs(x="Hours of sleep a night ", title='f(SleepHrsNight)') + guides(fill=guide_legend(title = "SleepHrsNight", reverse = TRUE))

ggplot(data = rpvt2) +
  geom_mosaic(aes(weight = 1, x = product(Loan, Size), fill = Size), na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 0, hjust = .1)) +
  labs(x = "TheRange", y = "TheGroup")

ggplot(data = rpvt2) +
  geom_mosaic(aes( x = product(Size, Region), fill=factor(Size), 
        conds=product(Loan)), na.rm=TRUE, divider=mosaic("v")) +    theme(axis.text.x=element_text(angle=-25, 
        hjust= .1)) + labs(x="Age in Decades ", title='f(SleepHrsNight, AgeDecade | Gender)') + 
        guides(fill=guide_legend(title = "SleepHrsNight", reverse = TRUE))

ggplot(data = rpvt2) +
  geom_mosaic(aes( x = product(Size, Region), fill=factor(Size), 
  conds=product(Loan)), na.rm=TRUE, divider=mosaic("v")) +    
  theme(axis.text.x=element_text(angle=-25, hjust= .1)) + labs(x="Region", 
  title='f(Size, Region | Loan)') + guides(fill=guide_legend(title = "Size", reverse = TRUE))

ggplot(data = rpvt2) +
  geom_mosaic(aes( x = product(Size, Region), fill=factor(Size)), 
  na.rm=TRUE) +    theme(axis.text.x=element_text(angle=-25, hjust= .1)) +
  labs(x="Region of the Firm", title='f(Firm Size, Region | Loan Application Outcome)') + facet_grid(Loan ~.) + 
  guides(fill=guide_legend(title = "Firm Size", reverse = TRUE))

##Now here is an example of sankey multiple indicators
#https://developers.google.com/chart/interactive/docs/gallery/sankey
#https://cran.r-project.org/web/packages/alluvial/vignettes/alluvial.html
#http://www.imsbio.co.jp/RGM/R_rdfile?f=DescTools/man/PlotCirc.Rd&d=R_CC

library(DescTools)

##constraints by row total frequency in columns size
sizecon <- matrix(c(30,14,13,7,7,161,119,127,47,36,106,103,110,33,21,95,77,61,30,8), nrow=5, byrow=FALSE)
dimnames(sizecon) <- list(c("No","Minor","Moderate","Major","Severe"), c("Micro","Small","Medium","Large"))

PlotCirc(sizecon,
          acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
          rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

##constraints by row total percentage in columns size
sizecon <- matrix(c(8,4,4,6,10,41,38,41,40,50,27,33,35,28,29,24,25,20,26,11), nrow=5, byrow=FALSE)
dimnames(sizecon) <- list(c("No","Minor","Moderate","Major","Severe"), c("Micro","Small","Medium","Large"))

PlotCirc(sizecon,
         acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
         rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

##constraints by row total frequency in columns regions
sizecon <- matrix(c(184,169,196,56,36,89,47,38,14,15,82,59,40,10,5,20,16,11,8,4,17,22,26,29,12), nrow=5, byrow=FALSE)
dimnames(sizecon) <- list(c("No","Minor","Moderate","Major","Severe"), c("Punjab","Sindh","KPK","Balochistan","Islamabad"))

PlotCirc( sizecon,
          acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
          rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

##constraints by row total percentage in columns regions 
sizeper <- matrix(c(47,54,63,48,50,23,15,12,12,21,21,19,13,9,7,5,5,4,7,6,4,7,8,25,17), nrow=5, byrow=FALSE)
dimnames(sizeper) <- list(c("No","Minor","Moderate","Major","Severe"), c("Punjab","Sindh","KPK","Balochistan","Islamabad"))

PlotCirc( sizeper,
          acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
          rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

par(mfrow=c(1,2))
PlotCirc(sizecon, main = "by Firm Size")
PlotCirc(sizeper, main = "by Region")

PlotCirc(sizecon, main="weekday ~ operator")
PlotCirc(t(sizeper), main="operator ~ weekday")


#http://www.imsbio.co.jp/RGM/R_rdfile?f=DescTools/man/PlotCirc.Rd&d=R_CC


tab <- table(d.pizza$weekday, d.pizza$operator)
par(mfrow=c(1,2))
PlotCirc(tab, main="weekday ~ operator")
PlotCirc(t(tab), main="operator ~ weekday")


##dataset from STATA
library(haven)
ivprobit <- read_dta("D:/Utilisateurs/e0g411m05t7/Dropbox/ThesisResource/SME/RSME/ivprobit.dta")
View(ivprobit)

attach(ivprobit)
head(ivprobit, n=10)
rpvt <- ivprobit[, c("a2","a6b","k30","rej" )]


library(rpivotTable)  # No need to explicitly load htmlwidgets: this is done automatically
rpivotTable(rpvt2,rows="rej",cols=c("a2","a6b","k30"), width="100%", height="400px",na.rm=TRUE)
head(size,n=50)

# create new dataset without missing data 

rpvt2 <- na.omit(rpvt)

missing.values(smepak$account) <- NULL

library(plyr)

rename(rpvt2, c("Region"="a2", "a6b"="Size","k30"="Constraint", "rej"="Rejected"))
attach(rpvt2)
library(reshape)
rpvt2 <- rename(rpvt2, c(a2="Region"))
rpvt2 <- rename(rpvt2, c(a6b="Size"))
rpvt2 <- rename(rpvt2, c(k30="Constraint"))
rpvt2 <- rename(rpvt2, c(rej="Loan"))


attach(rpvt2)
names(rpvt2)
ifelse(Loan == "Approved", 1, ifelse(Loan == "Rejected", 2))
ifelse(gender == "MALE", 1, ifelse(gender == "FEMALE", 2, 3))

library(car)
Recode(x, "1:2='A'; 3='B'")
rpvt2$Loan <- Recode(rpvt2$Loan, "1='Approved'; 2='Rejected'")
rpvt2$Size <- Recode(rpvt2$Size, "0='Micro'; 1='Small'; 2='Medium'; 3='Large'")
rpvt2$Region <- Recode(rpvt2$Region, "1='Punjab'; 2='Sindh'; 3='KPK'; 4='Balochistan'; 5='Islamabad'")
rpvt2$Constraint <- Recode(rpvt2$Constraint , "0='No'; 1='Minor'; 2='Moderate'; 3='Major'; 4='Severe'")

library(plyr)
data$scode <- revalue(data$sex, c("1"="Approved", "2"="Rejected"))
rpivotTable(rpvt2)
##pivot table
rpivotTable(rpvt2,rows="Loan",cols=c("Size"))


library(alluvial)
##Alluvial Diagram for rejection of an application by bank
tit <- as.data.frame(Titanic, stringsAsFactors = FALSE)
head(tit)

alluvial(tit[,1:4], freq=tit$Freq,
         col = ifelse(tit$Survived == "Yes", "red", "lightskyblue"),
         border = ifelse(tit$Survived == "Yes", "grey", "grey"),
         hide = tit$Freq == 0,
         cex = 0.7,
         blocks=FALSE
)

##OKAY NOW I AM GONNA PLAY WITH MY OWN DATA
library(readxl)
extra <- read_excel("D:/Utilisateurs/e0g411m05t7/Dropbox/SME/ExtraVisR/extra.xlsx")
View(extra)

alluvial(extra[,1:3], freq=extra$Freq,
         col = ifelse(extra$Collateral == "No", "red2", "lightskyblue"),
         border = ifelse(extra$Collateral == "Yes", "grey", "grey"),
         hide = extra$Freq == 0,
         cex = 0.9,
         blocks=F
)

###Financial Inclusion Inidcators in Pakistan
mydata <- data.frame(x = c(20,35,45,55,70), n = rep(50,5), y = c(6,17,26,37,44))

finclusion <- data.frame(Year=c(2011,2012,2013,2014,2015), Microfinance=c(28,34,47,61,81),
          SME.Finance= c(271,258,234,253,261), Agr.Finance =c(263,294,336,391,516),                   
          Islamic.Finance= c(560,711,903,1089,1495) , Housing.Finance =c(62,57,52,53,59))                

Year <- c(2011,2012,2013,2014,2015,2011,2012,2013,2014,2015,2011,2012,2013,2014,2015,2011,2012,2013,2014,2015,2011,2012,2013,2014,2015)
Indicator <-  c("Microfinance","Microfinance","Microfinance","Microfinance","Microfinance","SME.Finance","SME.Finance","SME.Finance","SME.Finance","SME.Finance","Agr.Finance","Agr.Finance","Agr.Finance","Agr.Finance","Agr.Finance","Islamic.Finance","Islamic.Finance","Islamic.Finance","Islamic.Finance","Islamic.Finance","Housing.Finance","Housing.Finance","Housing.Finance","Housing.Finance","Housing.Finance")                               
Value <- c(28,34,47,61,81,271,258,234,253,261,263,294,336,391,516,560,711,903,1089,1495,62,57,52,53,59)                                
       
fix <- data.frame(Year,Indicator, Value)
ggplot(data = fix,
  mapping = aes(x = Year, y = Value, shape = Indicator, colour = Indicator)) +
  geom_point() +
  geom_line() +
  facet_grid(facets = Year ~ .)

##So finally this graph is finall to show financial inclusion indicators in Pakistan
qplot(Year, Value, data = fix, geom = "line", colour = Indicator) +
  facet_grid(Indicator ~ ., scales = "free_y")

##Plotting actual and expected goals.... using Likert scale
#https://strengejacke.wordpress.com/2013/07/17/plotting-likert-scales-net-stacked-distributions-with-ggplot-rstats/
#However its a lenthy process, i found plotly instead.
#https://plot.ly/r/bar-charts/
library(plotly)
#http://www.sbp.org.pk/events/2016/HMQueen.pdf
#page 4
x <- c('Banked<br>Adult', 'FAG<br>Adult', 'Banked<br>Female', 'FAG<br>Female', 'Banked<br>SMEs', 'FAS<br>SMEs')
y <- c(10, 50, 2, 25, 10, 2)
base <- c(0, 13, 0, 5, 0, 7)
revenue <- c(13, 50,5, 25, 7,15)
costs <- c(0, 0,5, 25, 0,0)
profit <- c(0, 0,0, 0, 7,15)
text <- c('13%', '50%', '5%', '25%', '7%', '15%')
data <- data.frame(x, base, revenue,costs,profit, text)
#text <- c('$430K', '$260K', '$690K', '$-120K', '$-200K', '$-320K', '$370K')
#data <- data.frame(x, base, revenue, costs, profit, text)


#using ggplot2
library(ggplot2)
library(ggthemes) # Load

Area    <- c(rep(c("Adult(%age)", "Female(%age)", "MSMEs(%age)", "A.Points(0000)", ("Microfinance(Mil)")), each = 2))
Status  <- c(rep(c("Gap","Actual"), times = 5))
Target  <- c(13, 37, 5, 20,7, 8, 35, 15,3.6, 6.4)
Data    <- data.frame(Area, Status, Target)
#Target  <- c(50, 13, 25, 5, 15, 7, 50, 35, 10, 3.6)


p1 <- ggplot(Data, aes(x = Area, y = Target, fill = Status, label = Target)) +
  geom_bar(stat = "identity") + scale_fill_manual(values=c("#00CCFF", "#FF9933")) +
  geom_rangeframe() + theme_tufte() +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

p1
7-15

p1 <- ggplot(Data, aes(x = Area, y = Target, fill = Status, label = Target)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("#00BFFF","#DB7093")) +
  labs(x = "Access to Finance") + labs(y = "Actual vs Gap") +
  geom_text(size = 3.5, position = position_stack(vjust = 0.5))

p1
p1 + scale_fill_discrete(name="Status",
                         breaks=c("Actual", "Gap"),
                         labels=c("Gap", "Actual"))

getwd()
#scale_colour_manual(values = c(rgb(207, 31, 46,

p1 + theme(legend.position = "bottom") + theme(legend.direction = "horizontal") 
p1 +  theme(legend.title = element_text(colour="blue", size=10, face="bold") + theme(legend.position = "top" ))
p1 + labs(x = "Access to Finance") + labs(y = "Actual vs Target") 
+
  theme(axis.text.x  = element_text(angle=360, vjust=0.5, size=8))

p1 + scale_fill_discrete(name="Status",
                    breaks=c("Actual", "Gap"),
                    labels=c("Gap", "Actual"), labs(x = FALSE))

library(ggpubr)
ggbarplot(Data, x = "Area", y = "Target",
          fill = "Status", color = "Status", palette = c("#00AFBB", "#E7B800"),
          label = TRUE, lab.col = "white", lab.pos = "in")


??ggplot2
# library
library(ggplot2)
head(mtcars)

# basic graph
p=ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p
# Add text on a specific positions:
p + annotate("text", x = c(2,4.5), y = c(20,25), label = c("Hello", "Ca va") , color="red", size=5 , angle=45, fontface="bold")
# Add rectangles
p + annotate("rect", xmin=c(2,4), xmax=c(3,5), ymin=c(20,10) , ymax=c(30,20), alpha=0.2, color="blue", fill="blue")

Year      <- c(rep(c("2006-07", "2007-08", "2008-09", "2009-10"), each = 4))
Category  <- c(rep(c("A", "B", "C", "D"), times = 4))
Frequency <- c(168, 259, 226, 340, 216, 431, 319, 368, 423, 645, 234, 685, 166, 467, 274, 251)
Data      <- data.frame(Year, Category, Frequency)
library(ggplot2)
ggplot(Data, aes(x = Year, y = Frequency, fill = Category, label = Frequency)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
