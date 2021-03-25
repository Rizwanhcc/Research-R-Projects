##========SME World BANK Enterprise Survey=============

library(WDI)
getwd()
setwd("D:/Utilisateurs/e0g411m05t7/Dropbox/SME/RSME")
setwd("~/Dropbox/SME/RSME")

q() # quit R. You will be prompted to save the workspace.
##remove everything
rm(list = ls())
##SAVE everything in R, including DATA
save.image(file="smepak.RData")

# export data frame to Stata binary format 
library(foreign)
write.dta(forcorr, "D:/Utilisateurs/e0g411m05t7/Dropbox/SME/RSME/ttra.dta")

wdi_dat <- WDI( country = c("PK","IN"),
  indicator = c("IC.FRM.FCHAR.CAR1", "IC.FRM.WRKF.WK12", "IC.FRM.FIN.FIN12", "IC.FRM.FIN.FIN13", "IC.FRM.FIN.FIN14", "IC.FRM.FIN.FIN15", "IC.FRM.FIN.FIN16","IC.FRM.INNOV.T5", "IC.FRM.INNOV.T6","IC.FRM.OBS.OBST9","IC.FRM.WRKF.WK8"), 
  start = 2000, end = 2015, extra = TRUE) 

# age, size,bnkinv,bankwc, loc,bankaccount, atofconst, web, email,obsedu,mngrexp
aa <- subset(wdi_dat, year==2007 | year==2013)
head(aa)
df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))

dff=data.frame(main=c("Investment","Investment", "Working Capital","Working Capital", "Line of Credit","Line of Credit","Bank Acount","Bank Acount"), 
              Year=c(2007,2008,2007,2008,2007,2008,2007,2008), 
              Age=c(20.2,21.8,20.2,21.8,20.2,21.8,20.2,21.8),
              Size=c(29.0, 81.1,29.0, 81.1,29.0, 81.1,29.0, 81.1),
              xxx=c(9.7,8.1,4.6,8.6,8.6,6.7,64.7,58.1))

head(aa)
Bubble <- gvisBubbleChart(dff, idvar="main", 
                          xvar="xxx", yvar="Year",
                          colorvar="Year", sizevar="Age",
                          options=list(
                          hAxis='{minValue:4, maxValue:60}'))
plot(Bubble)

library(googleVis)

Bubble <- gvisBubbleChart(Fruits, idvar="Fruit", 
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                          hAxis='{minValue:75, maxValue:125}'))
plot(Bubble)
head(Fruits)
Bar <- gvisBarChart(newdata)
plot(Bar)
newdata <- dff[c(-3,-4)]
newdata <- dff[c(-2,-3)]

dfff <- subset(dff, -c[, 3|4])

Year      <- c(rep(c("2006-07", "2007-08", "2008-09", "2009-10"), each = 4))
Category  <- c(rep(c("A", "B", "C", "D"), times = 4))
Frequency <- c(168, 259, 226, 340, 216, 431, 319, 368, 423, 645, 234, 685, 166, 467, 274, 251)
Data      <- data.frame(Year, Category, Frequency)
library(ggplot2)
p <- qplot(Year, Frequency, data = Data, geom = "bar", fill = Category,     theme_set(theme_bw()))
p + geom_text(aes(label = Frequency), size = 3, hjust = 0.5, vjust = 3, position =     "stack") 


##Indicators of financial access by SMEs in Pakistan
dff=data.frame(Indicator=c("Investment","Investment", "Working Capital","Working Capital", "Line of Credit","Line of Credit","Bank Acount","Bank Acount", "Financial Constraint","Financial Constraint"), 
               Year=c(2007,2013,2007,2013,2007,2013,2007,2013,2007,2013), 
               Age=c(20.2,21.8,20.2,21.8,20.2,21.8,20.2,21.8,20.2,21.8),
               Size=c(29.0, 81.1,29.0, 81.1,29.0, 81.1,29.0, 81.1,29.0, 81.1),
               Percentage=c(9.7,8.1,4.6,8.6,8.6,6.7,64.7,58.1,17.7,13.2))


ggplot(dff, aes(x = Year, y = Percentage, fill = Indicator, label = Percentage))  +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))


##ICT indicators of SMEs in Pakistan
myd <- data.frame(   Indicator  = c("Web","Web", "Email","Email","F.Tech","F.Tech","Experience","Experience"),   
                     Year = c(2007,2013,2007,2013,2007,2013,2007,2013),   
                     Percentage = c(16.6,46.6,26.8,54.4,2.7,22.1,20.2,13.2) ) 

ggplot(myd, aes(x = Year, y = Percentage, fill = Indicator, label = Percentage))  +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

# Use position=position_dodge()
ggplot(data=myd, aes(x=Year, y=Percentage, fill=Percentage)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(myd, aes(x=reorder(Seller, Num), y=Avg_Cost)) +
  geom_bar(stat='identity') +
  coord_flip()


#http://rgraphgallery.blogspot.fr/2013/04/rg-stacked-bar-chart-number-and-percent.html
#38: Stacked bar chart (number and percent)
myd <- data.frame(   var1  = c(1,1,2,2,3,3,4,4),   
                     samp = c("A","B","A","B","A","B","A","B"),   
                     Value1 = c(3.5,2,5,8,3,2,7,2), Value2 = c(1.5,0,5,5,3,0,4,5) ) 

# rshaping data to long form for ggplot2 
library(reshape2)
meltd<- melt(myd, id.vars=1:2) 

#plot 
library(ggplot2)
ggplot(meltd, aes(x=var1, y=value, fill=variable)) +
  geom_bar(stat="identity") + facet_grid(~samp) + theme_bw()

##ICT indicators of the firms have web and use of email
myd <- data.frame(   var1  = c("Web", "Email","F.Tech","Experience"),   
                     samp = c("2007","2013","2007","2013"),   
                     Value1 = c(16.6,26.8,2.7,20.2), Value2 = c(46.6,54.4,22.1,13.2) ) 

#http://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
myd <- data.frame(   var1  = c(1,1,2,2,3,3,4,4),   
                     samp = c("A","B","A","B","A","B","A","B"),   
                     Value1 = c(3.5,2,5,8,3,2,7,2), Value2 = c(1.5,0,5,5,3,0,4,5) ) 
# rshaping data to long form for ggplot2 
library(reshape2)
meltd<- melt(myd, id.vars=1:2) 

#plot 
library(ggplot2)
ggplot(meltd, aes(x=var1, y=value, fill=variable)) +
  geom_bar(stat="identity") + facet_grid(~samp) + theme_bw()

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(myd, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
head(counts)


myd <- data.frame(Indicator  = c("Web","Web", "Email","Email","F.Tech","F.Tech","Experience","Experience"),   
                     Year = c(2007,2013,2007,2013,2007,2013,2007,2013),   
                     Percentage = c(16.6,46.6,26.8,54.4,2.7,22.1,20.2,13.2)) 

##by combining ict and financial indicator
dff=data.frame(Indicator=c("Investment","Investment", "Working Capital","Working Capital", "Line of Credit","Line of Credit","Bank Acount","Bank Acount", "Financial Constraint","Financial Constraint","Web","Web", "Email","Email","Foreign Technology","Foreign Technology"), 
               Rounds=c("Finance2007","Finance2013","Finance2007","Finance2013","Finance2007","Finance2013","Finance2007","Finance2013","Finance2007","Finance2013", "ICT2007","ICT2013","ICT2007","ICT2013","ICT2007","ICT2013"), 
               Percentage=c(9.7,8.1,4.6,8.6,8.6,6.7,64.7,58.1,17.7,13.2,16.6,46.6,26.8,54.4,2.7,22.1))

ggplot(dff, aes(x = Rounds, y = Percentage, fill = Indicator, label = Percentage))  +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

##diagrams in R
install.packages('DiagrammeR')
library(DiagrammeR)

bank <- data.frame( Name=c("Pakistan", "Small","Medium","Large", "South-Asia","Low-Middle"),
Investment=c(2.4, 0.3, 3.9, 1.6, 13.8, 12.1),
LineofCredit=c(7.1, 3.2, 7.2, 17.2, 23.8, 30.5),
BankAccount=c(56.5, 44.1, 62.2, 77.3, 74.7, 84.9))

ict <- data.frame( Name=c("Pakistan", "Small","Medium","Large", "South-Asia","Low-Middle"),
Website=c(47.5, 29.5, 55.6, 79.2, 31.5, 43.5),
Email=c(54.5, 37.8, 59.9, 87.8, 52.1, 66.4))

#selecting only bank indicators
bank <- smepak[, 1:4] 
#subset ict indicators column 1 and 5,6
ict <- smepak[, c(1, 5, 6)]
ict
library(googleVis)
Line <- gvisLineChart(ict)
plot(Line)
Bar <- gvisBarChart(ict)
plot(Bar)

Column <- gvisColumnChart(ict)
plot(Column)

Area <- gvisAreaChart(ict)
plot(Area)

Line <- gvisLineChart(bank)
plot(Line)

Bar <- gvisBarChart(bank)
plot(Bar)

Column <- gvisColumnChart(bank)
plot(Column)

Area <- gvisAreaChart(bank, "Name", c("Investment","LineofCredit","BankAccount"),
                      options=list(gvis.editor="Edit me!"))
plot(Area)

Area <- gvisAreaChart(ict, "Name", c("Website","Email"),
                      options=list(gvis.editor="Edit me!"))
plot(Area)

bank

##more controlled chart
Line3 <-  gvisLineChart(ict, xvar="Name", yvar=c("website","email"),
                        options=list(
                          title="Hello World",
                          titleTextStyle="{color:'red', 
                          fontName:'Courier', 
                          fontSize:16}",                         
                          backgroundColor="#D3D3D3",                          
                          vAxis="{gridlines:{color:'red', count:3}}",
                          hAxis="{title:'Country', titleTextStyle:{color:'blue'}}",
                          series="[{color:'green', targetAxisIndex: 90}, 
                          {color: 'orange',targetAxisIndex:1}]",
                          vAxes="[{title:'val1'}, {title:'val2'}]",
                          legend="bottom",
                          curveType=" ",
                          width=800,
                          height=300                         
                        ))
plot(Line3)

# geom_bar is designed to make it easy to create bar charts that show
# counts (or sums of weights)
g <- ggplot(smepak, aes(Indicator))
# Number of cars in each class:
g + geom_bar()

###World bank loan to pakistan
wbloan=data.frame( Project=c("Total","FGDP","NSPPR","PTGP"),
                Amount=c(450, 300, 100, 50),
                Amount2=c(450, 300, 100, 50),
                des=c("Total Loan approved by WB", "Finance for growth development", "National Social Protection", "Punjab Tourism"))

Area <- gvisAreaChart(wbloan, "Project", c("Amount","Amount2"),
                      options=list(gvis.editor="Edit me!"))
plot(Area)


##Subsetting dataframe on the basis of selected columns...
df[,c("A","B","E")]
#or
df[,c(1,2,5)]
#or
library(dplyr)

df1 %>%
  select(A, B, E)

#This can also be written without the %>% pipe as:
select(df1, A, B, E)
#or
df2 <- subset(df1, select = c(1, 2, 5))

# data for reproducible example
# (and to avoid confusion from trying to subset `stats::df`)
df <- setNames(data.frame(as.list(1:5)), LETTERS[1:5])
# subset
df[,c("A","B","E")]

##Handling Missing values
is.na(x) # returns TRUE of x is missing
y <- c(1,2,3,NA)
is.na(y) # returns a vector (F F F T)
rm("y")
names(smepak)


##18 APRIL 2017
#Creating a new column of age 
Pakistan_2013_full_data$age <- Pakistan_2013_full_data$b5
attach(Pakistan_2013_full_data)
names(Pakistan_2013_full_data)

#Subtract 2013 from the age column to compute age in years
calculateage <- 2013
smepak$Age <- calculateage - smepak$Year
attach(smepak)

head(firmage, n=40)
Pakistan_2013_full_data$worktrng <- Pakistan_2013_full_data$l10
names(Pakistan_2013_full_data)

##after rename now come to contengency tables
library(rpivotTable)
smepak <- subset(Pakistan_2013_full_data, select = c("idstd", "ID","age","femonwer" ,"numempl","mangexp","topfem",     
"email", "website","cellphone", "telobs","reglcoc", "sale","region", "size","account","overdraft" ,"loc", "accesfobs",
"insttypeloc" ,"worcapbank","worcapmonyl", "workedu", "worktrng" ))

rpivotTable(ict,na.rm=TRUE )
head(account)
#Recoding Values to Missing
# recode 99 to missing for variable v1
# select rows where v1 is 99 and recode column v1 
smepak$account[smepak$account==99] <- -9

#Missing values with a package memisc
library(memisc)
missing.values(smepak$Account) <- c(-9)
missing.values(smepak$firmage) <- c(2022)

head(size,n=50)
# create new dataset without missing data 
newsme <- na.omit(smepak)
missing.values(smepak$account) <- NULL

##recoding bank account 1 with bank account and 0 otherwise
smepak$baccount <- ifelse(smepak$baccount==2, 0, ifelse(smepak$baccount==1, 1, ifelse(smepak$baccount==-9, 0, NA)))
smepak$eemail <- ifelse(smepak$email==2, 0, ifelse(smepak$email==1, 1, ifelse(smepak$email==-9, 0, NA)))
smepak$wweb <- ifelse(smepak$website==2, 0, ifelse(smepak$website==1, 1, ifelse(smepak$website==-9, 0, NA)))
smepak$cell <- ifelse(smepak$cellphone==2, 0, ifelse(smepak$cellphone==1, 1, ifelse(smepak$cellphone==-9, 0, NA)))
smepak$agee <- ifelse(smepak$firmage==2022, 0, ifelse(smepak$firmage== firmage))
smepak$agee <- recode(smepak$firmage,"2022=0") 
smepak$workeduc <- ifelse(smepak$workedu==2, 0, ifelse(smepak$workedu==1, 1, ifelse(smepak$workedu==-9, 0, NA)))
smepak$worktrngg <- ifelse(smepak$worktrng==2, 0, ifelse(smepak$worktrng==1, 1, ifelse(smepak$worktrng==-9, 0, NA)))
smepak$topfemm <- ifelse(smepak$topfem==2, 0, ifelse(smepak$topfem==1, 1, ifelse(smepak$topfem==-9, 0, NA)))
smepak$overdraftt <- ifelse(smepak$overdraft==2, 0, ifelse(smepak$overdraft==1, 1, ifelse(smepak$overdraft==-9, 0, NA)))
smepak$locc <- ifelse(smepak$loc==2, 0, ifelse(smepak$loc==1, 1, ifelse(smepak$loc==-9, 0, NA)))

##converting numeric to a factor variable
#convert a Factor column to a numeric column
breast$class <- as.numeric(as.character(breast$class))
smepak$region <- as.factor(as.numeric(smepak$region))
smepak$size <- as.factor(as.numeric(smepak$size))

##i have taken an incorrect proxy of size going again
firmsize <- Pakistan_2013_full_data[,c("idstd","a6b")]
as.data.frame(firmsize)
head(firmsize,n=500)
smenew <- merge(smepak,firmsize, by="idstd")
str(smenew)

##probit model for financial inclusion and ICT
#stargazer(myprobit1, myprobit2, myprobit3, title="Probit Results", align=TRUE)
#stargazer(myprobit1, myprobit2, myprobit3, myprobit4, myprobit5, myprobit6, type = "text", title="Probit Results", align=TRUE)

myprobit1 <- glm(baccount ~ agee+topfemm+factor(region)+ factor(a6b)+worktrngg+ eemail, family = binomial(link = "probit"), data = smenew)
probitrp1_me <-mfx_me(myprobit1)

myprobit2 <- glm(baccount ~ agee+topfemm+factor(region)+ factor(a6b)+worktrngg+ wweb, family = binomial(link = "probit"), data = smenew)
probitrp2_me <-mfx_me(myprobit2)

myprobit3 <- glm(baccount ~ agee+topfemm+factor(region)+ factor(a6b)+worktrngg+ cell, family = binomial(link = "probit"), data = smenew)
probitrp3_me <-mfx_me(myprobit3)

##Line of credit
myprobit4 <- glm(locc ~ agee+topfemm+factor(region)+factor(a6b)+worktrngg+eemail, family = binomial(link = "probit"), data = smenew)
probitrp4_me <-mfx_me(myprobit4)

myprobit5 <- glm(locc ~  agee+topfemm+factor(region)+ factor(a6b)+worktrngg+wweb, family = binomial(link = "probit"), data = smenew)
probitrp5_me <-mfx_me(myprobit5)

myprobit6 <- glm(locc ~  agee+topfemm+factor(region)+ factor(a6b)+worktrngg+cell, family = binomial(link = "probit"), data = smenew)
probitrp6_me <-mfx_me(myprobit6)

##Marginal Effects output
myprobit4 <- glm(locc ~ agee+topfemm+factor(region)+ factor(a6b)+eemail, family = binomial(link = "probit"), data = smenew)
probitrp4_me <-mfx_me(myprobit4)

myprobit5 <- glm(locc ~  agee+topfemm+factor(region)+ factor(a6b)+wweb, family = binomial(link = "probit"), data = smenew)
probitrp5_me <-mfx_me(myprobit5)

mtable(probitrp1_me,probitrp2_me,probitrp3_me,probitrp4_me, probitrp5_me,probitrp6_me)  ## produces a table with nice output
toLatex(mtable(probitrp1_me,probitrp2_me,probitrp3_me,probitrp4_me, probitrp5_me,probitrp6_me))  ## Produces LaTeX code

#Improting SME data from STATA mac
library(haven)
Paksmesecond <- read_dta("~/Dropbox/SME/RSME/Paksmesecond.dta")
View(Paksmesecond)
#Improting SME data from STATA LAB
library(haven)
Paksmesecond <- read_dta("D:/Utilisateurs/e0g411m05t7/Dropbox/SME/RSME/Paksmesecond.dta")
View(Paksmesecond)

##19 APRIL 2017
smepak <- subset(Paksmesecond, select = c("idstd", "ID","a3a","a6b","b1","b4","b5","b6","b7","b7a", "b8",
                                                     "c22a", "c22b", "c28", "c30a", "c30b","SARd24a","d2","k6","k7","k8","k3bc","k30", "l9b", "l10" ))

colnames(smepak) <- c("idstd", "ID","Region","Size","Status","FemMngr","Year","Employee","MngrExp","Female", "Quality",
                      "Email", "Website", "Cellphone", "Electricity", "Telecomm","CCommerce","Sales","Account","Overdraft","Loc","Wcap","Access", "Empedu", "Emptr" )

##Subsetting for canonical regression
attach(smepak)
head(Age)
#Subtract 2013 from the age column to compute age in years
calculateage <- 2013
smepak$Age <- calculateage - smepak$Year
attach(smepak)

str(forcorr)
class(forcorr)
smepak
missing.values(smepak) <- c(-9, -7)
missing.values(smepak$Account) <- (-9)
missing.values(smepak)

ict$datay[ict$datay == -9] <- NA
library(car)

# Recode grade -9 to NA
ict$Email<-recode(ict$Email,"-9=NA")
ict$Website<-recode(ict$Website,"-9=NA")
ict$Cellphone<-recode(ict$Cellphone,"-9=NA")
# Recode grade -9 to NA
fin$Account<-recode(fin$Account,"-9=NA")
fin$Overdraft<-recode(fin$Overdraft,"-9=NA")
fin$Loc<-recode(fin$Loc,"-9=NA")
fin$Wcap<-recode(fin$Wcap,"-9=NA")

# Or recode NA to 7
ict$Email <- recode(ict$Email,"NA=7")
SchoolData$Grade<-recode(SchoolData$Grade,"3=NA")

attach(fin)
attach(ict)
ict <- as.data.frame(ict)

# rename programmatically 
library(reshape)
smepak <- rename(smepak, c(b8="Quality"))


##Subsetting for bank and ict
ict <- smepak[, 12:14]
fin <- smepak[, c(42:44,50)]
fin <- fin[, -c(4:8)]

library("CCA")

correl <- matcor(ict, fin)
img.matcor(correl, type = 2)
correl
require(ggplot2)
require(GGally)
##another graph
ggpairs(ict)
ggpairs(fin)

res.regul <- estim.regul(ict, fin, plt = TRUE,
                            + grid1 = seq(0.0001, 0.2, l=3),
                            + grid2 = seq(0, 0.2, l=3))

cc1 <- cc(ict, fin)

# display the canonical correlations
cc1$cor

# raw canonical coefficients
cc1[3:4]

# compute canonical loadings
cc2 <- comput(ict, fin, cc1)

# display canonical loadings
cc2[3:6]
# tests of canonical dimensions
ev <- (1 - cc1$cor^2)

n <- dim(ict)[1]
p <- length(ict)
q <- length(fin)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2

w <- rev(cumprod(rev(ev)))

# initialize
d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}

pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))

#to eliminate e- values
options(scipen=999)

#to revert 
options(scipen=0)
str(dmat)
can <- as.matrix(dmat)
write.csv(dmat,file="cancor.csv")

write.csv(coresult.tetcor.r,'correlationmatrix.csv')

# standardized psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(ict))))
s1 %*% cc1$xcoef

str(s1)
regcof <- as.matrix(s1)
write.csv(s1,file="regcof.csv")

# standardized acad canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(fin))))
s2 %*% cc1$ycoef

##new dataset by excluding column number 4
fin <- fin[, -(4)]

library(dplyr)
##trying to recode -9 in all dataset
recode.function<- function(x){          
  x <- NA==-9 
}

recode.list <- c("idstd", "ID","Region","Size","Status","FemMngr","Year","Employee","MngrExp","Female", "Quality",
                 "Email", "Website", "Cellphone", "Electricity", "Telecomm","CCommerce","Sales","Account","Overdraft","Loc","Wcap","Access", "Empedu", "Emptr" )

require(dplyr)
smepak %>% mutate_each_(funs(recode.function), recode.list)

attach(smepak)



