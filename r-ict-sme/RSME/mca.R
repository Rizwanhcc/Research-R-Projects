#===Multiple Correspondence Analysis (MCA)========###
#http://gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/
#http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining
install.packages("FactoMineR")
# install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library("FactoMineR")
library("factoextra")
library(questionr)
library(ade4)
getwd()
getwd()
library(readxl)
wbesmcaNA <- read_excel("~/Dropbox/SME/ExtraVisR/wbesmcaNA.xlsx")
View(wbesmcaNA)
head(wbesmcaNA)
str(wbesmcaNA)
wbesmcaNA <- as.data.frame(wbesmcaNA)
##Preparing Data for MCA for ICT and Finance
#calculating age of the firm
wbesmcaNA$age <- wbesmcaNA[,"b5"]
head(age, n=10)
head(b5, n=100)

attach(wbesmcaNA)
is.numeric(age)
is.numeric(age2)
is.numeric(year)
wbesmcaNA$year <- (2013)
head(year, n=10)
head(age2, n=10)
class(wbesmcaNA)
wbesmcaNA$age2 <- as.numeric(as.character(wbesmcaNA$age))
wbesmcaNA$age2 <- c(wbesmcaNA$year - wbesmcaNA$age2) 
age2 <- wbesmcaNA[, c(year - age), na.rm=TRUE]
names(wbesmcaNA)
##list down all the objects in the environment
list(objects())
rm("tea")

## Recoding wbesmcaNA$a6b into wbesmcaNA$size
wbesmcaNA$size <- wbesmcaNA$a6b
wbesmcaNA$size[wbesmcaNA$a6b == "Large >=100"] <- "Large"
wbesmcaNA$size[wbesmcaNA$a6b == "Medium >=20 and <=99"] <- "Medium"
wbesmcaNA$size[wbesmcaNA$a6b == "Small >=5 and <=19"] <- "Small"
wbesmcaNA$size[wbesmcaNA$a6b == "Micro <5"] <- "Small"
wbesmcaNA$size <- factor(wbesmcaNA$size)

##Variable of working capital proportion
## Recoding finict$k3bc into finict$workcap
finict$workcap <- finict$k3bc
finict$workcap[finict$k3bc == "Borrowed from banks: private and state-owned ()"] <- "Zero"
finict$workcap[finict$k3bc == "0"] <- "Zero"
finict$workcap[finict$k3bc == "20"] <- "Leq20"
finict$workcap[finict$k3bc == "50"] <- "leq50"
finict$workcap[finict$k3bc == "100"] <- "hundred"
finict$workcap[finict$k3bc == "40"] <- "leq50"
finict$workcap[finict$k3bc == "10"] <- "Leq20"
finict$workcap[finict$k3bc == "1"] <- "0"
finict$workcap[finict$k3bc == "30"] <- "leq50"
finict$workcap[finict$k3bc == "60"] <- "leq80"
finict$workcap[finict$k3bc == "70"] <- "leq80"
finict$workcap[finict$k3bc == "25"] <- "leq50"
finict$workcap[finict$k3bc == "45"] <- "leq50"
finict$workcap[finict$k3bc == "80"] <- "leq80"
finict$workcap[finict$k3bc == "11"] <- "Leq20"
finict$workcap[finict$k3bc == "17"] <- "Leq20"
finict$workcap[finict$k3bc == "8"] <- "Leq20"
finict$workcap[finict$k3bc == "14"] <- "Leq20"
finict$workcap[finict$k3bc == "35"] <- "leq50"
finict$workcap[finict$k3bc == "7"] <- "Leq20"
finict$workcap[finict$k3bc == "15"] <- "Leq20"
finict$workcap <- factor(finict$workcap)

##Variable of investment proportion
## Recoding finict$k5bc into finict$ivest
## Recoding finict$k5bc into finict$k5bc_rec
finict$k5bc_rec <- finict$k5bc
finict$k5bc_rec[finict$k5bc == "Borrowed from banks: private and state-owned ()"] <- "Zero"
finict$k5bc_rec[finict$k5bc == "60"] <- "Leq80"
finict$k5bc_rec[finict$k5bc == "0"] <- "Zero"
finict$k5bc_rec[finict$k5bc == "10"] <- "Leq20"
finict$k5bc_rec[finict$k5bc == "50"] <- "Leq50"
finict$k5bc_rec[finict$k5bc == "70"] <- "Leq80"
finict$k5bc_rec[finict$k5bc == "45"] <- "Leq50"
finict$k5bc_rec[finict$k5bc == "30"] <- "Leq50"
finict$k5bc_rec[finict$k5bc == "20"] <- "Leq20"
finict$k5bc_rec[finict$k5bc == "100"] <- "hundred"
finict$k5bc_rec <- factor(finict$k5bc_rec)

##cutting and regrouping age of the firm
## Cutting finict$age2 into finict$firmage
finict$firmage <- cut(finict$age2, include.lowest=FALSE,  right=FALSE,
                      breaks=c(0, 10, 80))
head(finict)
attach(finict)
head(firmage, n = 10)
## Recoding finict$firmage into finict$Age
finict$Age <- as.character(finict$firmage)
finict$Age[finict$firmage == "[0,10)"] <- "Young"
finict$Age[finict$firmage == "[10,80)"] <- "Mature"
finict$Age <- factor(finict$Age)
finict$investment <- rename(finict$k5bc_rec)
library(data.table)
setnames(DF, "oldName", "newName")
setnames(finict, "k5bc_rec", "investment")
wbesmcaNA$a2 <- factor(wbesmcaNA$a2)


##select the data
finict <- wbesmcaNA[, c("a2", "size","b7a","b5","age2","b8","e6","c22a","c22b","k21" ,"k6", "k7", "k8", "k3bc", "k5bc", "l10")]
##this data set contains all the concerened variables,,now i will only retain main variables
finict2 <- finict[, c("size","Age" ,"b7a","b8","e6","c22a","c22b","k21" ,"k6", "k7", "k8", "workcap", "investment", "l10")]


names(wbesmcaNA)
freq.na(finict2) ##to see missing values

head(finict$k3bc, n = 10)

##graphing
for (i in 1:ncol(finict2)) {
  plot(finict[,i], main=colnames(finict)[i],
       ylab = "Count", col="steelblue", las = 2)
}


#to compute MCA ----
acm <- dudi.acm(d %>% select(sexe, grape, etude, hard.rock, lecture.bd, peche.chasse, 
                             cuisine, bricol, cinema, sport), scannf = FALSE, nf = 5)
acm <- dudi.acm(finict2, scannf = FALSE, nf = 5)
str(finict)

# To do it for all names
df[finict] <- lapply(finict, factor) # the "[]" keeps the dataframe structure
finict3 <- lapply(finict2, class) == "character"
finict2[, finict3] <- lapply(finict2[, finict3], as.factor)
finict3 <- as.data.frame(finict3)
?dudi.acm

# number of categories per variable
cats = apply(finict2, 2, function(x) nlevels(as.factor(x)))

##Renaming Variablesm using plyr package
library(plyr)
rename(finict2, c("size"="Size", "b7a"="Female", "b8"= "Certification" , "e6"= "Licensed","c22a" =  "Email"  ,  "c22b"= "Website", "k21" = "Audit" ,
      "k6" = "Account",  "k7" = "Overdraft", "k8" = "LOC", "workcap" = "WorkingCap", "investment" = "Investment", "l10" = "Training"))
require(data.table)

setnames(finict2, c("size"="Size", "Age"= "Age", "b7a"="Female", "b8"= "Certification" , "e6"= "Licensed","c22a" =  "Email"  ,  "c22b"= "Website", "k21" = "Audit" ,
              "k6" = "Account",  "k7" = "Overdraft", "k8" = "LOC", "workcap" = "WorkingCap", "investment" = "Investment", "l10" = "Training"))

fini <- finict2[, c("Size", "Age","Certification" , "Licensed","Email","Website","Audit", "Training" ,
                 "Account", "Overdraft","LOC", "WorkingCap", "Investment")]
fini <- fini[, -c("type5")]
fini$type2 <- NULL                  

attach(fini)
acm <- dudi.acm(fini, scannf = FALSE, nf = 5)

#https://rstudio-pubs-static.s3.amazonaws.com/2120_cfbb7161b23e494ea47707f5a12032f1.html
# eigenvalues
acm$eig

# column coordinates
head(acm$co)
summary(acm)

# row coordinates
head(acm$li)

head(finict2$Certification, n=100)
names(finict2)
attach(finict2)


##plots
screeplot(acm)
summary(acm)
s.label(acm$li)
s.label(acm$li, clabel  = 0)
s.class(acm$li, finict2$Account)
s.class(acm$li, fini$Account, col = brewer.pal(4, "Set1"))
s.class(acm$li, finict2$Female, col = brewer.pal(4, "Set1"))
s.class(acm$li, wbesmcaNA$a2, col = brewer.pal(4, "Set1"))

par(mfrow = c(1, 2)) ##one row two columns
par(mfrow = c(1, 1)) ##reset to one graph per window

s.class(acm$li, finict2$LOC, col = brewer.pal(4, "Set1"))
s.class(acm$li, finict2$Size, col = brewer.pal(4, "Set1"))
s.class(acm$li, finict2$Age, col = brewer.pal(4, "Set1"))

##circle correlation plot
s.corcircle(acm$co, clabel = .4)

##boxplot
boxplot(acm)
boxplot(acm, 2)


#barplot for each dimension
barplot(acm$cr[, 1], names.arg = row.names(acm$cr))
barplot(acm$cr[, 2], names.arg = row.names(acm$cr))

##barplot for all 
par(mfrow = c(2, 2))
for (i in 1:4) barplot(acm$cr[, i], names.arg = row.names(acm$cr), 
                       las = 2, main = paste("Axe", i))
##factorial plane
s.label(acm$co, clabel = 0.7)
s.label(acm$co, 3, 4, clabel = 0.7, boxes = FALSE)
s.label(acm$li, clabel = 0, pch = 25)

##JLutils
library(JLutils)
s.freq(acm$li)
s.value(acm$li, acm$li[, 3], 1, 2, csi = 0.5)
s.arrow(acm$co, clabel = 0.7)
s.hist(acm$li, clabel = 0, pch = 25)
rm("d", "banque", "d2", "hdv2003")

##presentation
s.label(acm$co)
s.label(acm$co, 3, 4)
scatter(acm, col = brewer.pal(4, "Set1"))
?s.label
plotellipses(acm, keepvar="all")
dimdesc(acm, axes = 1:2)

?`ade4-package`
##cluster analysis
###
fini$type2 <- factor(cutree(arbre, 2))
fini$type5 <- cutree(arbre, 5)
freq(fini$type2)
s.class(acm$li, as.factor(fini$type2))
s.class(acm$li, as.factor(fini$type2), col = brewer.pal(5, "Set1"))
s.class(acm$li, as.factor(fini$type5), col = brewer.pal(5, "Set1"))
library(GGally)
ggpairs(fini, aes(colours = type5))
ggpairs(fini[, c("Age", "Size", "LOC", "type2")], aes(colour = type2))
ggpairs(fini[, c("Age", "Size", "LOC", "Overdraft","type2")], aes(colour = type2))

A2Rplot(arbre, k = 5, boxes = FALSE)


##distance of each individual with the other 
md <- dist.dudi(acm)
str(acm)
arbre <- hclust(md, method = "ward.D2")
