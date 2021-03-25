#===========R script for SME Paper============

#Set working directory
setwd("D:/Utilisateurs/e0g411m05t7/Dropbox/ThesisResource/SME/RSME")

##====Entering Data for the plot of indicators of Financial Inclusion in Pakistan
Year <- c(2011,2012,2013,2014,2015,2011,2012,2013,2014,2015,2011,2012,2013,2014,2015,2011,2012,2013,2014,2015,2011,2012,2013,2014,2015)
Indicator <-  c("Microfinance","Microfinance","Microfinance","Microfinance","Microfinance","SME.Finance","SME.Finance","SME.Finance","SME.Finance","SME.Finance","Agr.Finance","Agr.Finance","Agr.Finance","Agr.Finance","Agr.Finance","Islamic.Finance","Islamic.Finance","Islamic.Finance","Islamic.Finance","Islamic.Finance","Housing.Finance","Housing.Finance","Housing.Finance","Housing.Finance","Housing.Finance")                               
Value <- c(28,34,47,61,81,271,258,234,253,261,263,294,336,391,516,560,711,903,1089,1495,62,57,52,53,59)                                

##save data as data.frame
fix <- data.frame(Year,Indicator, Value)

##So finally this graph is finall to show financial inclusion indicators in Pakistan
p <- qplot(Year, Value, data = fix, geom = "line", colour = Indicator) +
  facet_grid(Indicator ~ ., scales = "free_y")

#change the background theme
p1 <- p + theme_bw()

# Remove the plot legend
p1 + theme(legend.position='none')

##saving the plot
ggsave("p1.pdf", width = 7, height = 6, dpi = 500, limitsize = TRUE)
ggsave("p1.png", width = 7, height = 6)

##=====Now its time for second plot

##Access to and use of Finance and ICT in Pakistan
dff=data.frame(Indicator=c("Investment","Investment", "Working Capital","Working Capital", "Line of Credit","Line of Credit","Bank Acount","Bank Acount", "Financial Constraint","Financial Constraint","Website","Website", "Email","Email","Foreign Technology","Foreign Technology"), 
               Rounds=c("Finance2007","Finance2013","Finance2007","Finance2013","Finance2007","Finance2013","Finance2007","Finance2013","Finance2007","Finance2013", "ICT2007","ICT2013","ICT2007","ICT2013","ICT2007","ICT2013"), 
               Percentage=c(9.7,8.1,4.6,8.6,8.6,6.7,64.7,58.1,17.7,13.2,16.6,46.6,26.8,54.4,2.7,22.1))

p2 <- ggplot(dff, aes(x = Rounds, y = Percentage, fill = Indicator, label = Percentage))  +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme(legend.title=element_blank())            

# Horizontal legend box
p2 +theme(legend.position="right", legend.box = "vertical")

p2+guides(color = guide_legend(order=1),
         size = guide_legend(order=2),
         shape = guide_legend(order=3))

##saving the plot
ggsave("p2.pdf", width = 7, height = 6, dpi = 500, limitsize = TRUE)
ggsave("p2.png", width = 7, height = 6)


##=====Now its time for Third plot, GoogleVis produces interactive plots...
#Comparative Analysis of ICT and Financial Inclusion
#These two plots are made using googlevis package, it can be edited online 
library(googleVis)


#Entering Data...........................................

bank <- data.frame( Name=c("Pakistan", "Small","Medium","Large", "South-Asia","Low-Middle"),
                    Investment=c(2.4, 0.3, 3.9, 1.6, 13.8, 12.1),
                    LineofCredit=c(7.1, 3.2, 7.2, 17.2, 23.8, 30.5),
                    BankAccount=c(56.5, 44.1, 62.2, 77.3, 74.7, 84.9))

ict <- data.frame( Name=c("Pakistan", "Small","Medium","Large", "South-Asia","Low-Middle"),
                   Website=c(47.5, 29.5, 55.6, 79.2, 31.5, 43.5),
                   Email=c(54.5, 37.8, 59.9, 87.8, 52.1, 66.4))

#Plotting data..........................................
Area <- gvisAreaChart(bank, "Name", c("Investment","LineofCredit","BankAccount"),
                      options=list(gvis.editor="Edit me!"))
plot(Area)

#Second plot area 
Bar <- gvisBarChart(ict, "Name", c("Website","Email"),
                      options=list(gvis.editor="Edit me!"))
plot(Bar)

#It will take to the web browser and then edit it as you like.......:8:

##======Next we move to Circle Plot, we need a package for this
library(DescTools)

#Entering Data...........................................
##constraints by row total percentage in columns firm size
sizecon <- matrix(c(8,4,4,6,10,41,38,41,40,50,27,33,35,28,29,24,25,20,26,11), nrow=5, byrow=FALSE)
dimnames(sizecon) <- list(c("No","Minor","Moderate","Major","Severe"), c("Micro","Small","Medium","Large"))

#Plotting and coloring....
PlotCirc(sizecon) ##or we can customize colorsss

PlotCirc(sizecon,
         acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
         rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

#Second plot by region
##constraints by row total percentage in columns regions 
sizeper <- matrix(c(47,54,63,48,50,23,15,12,12,21,21,19,13,9,7,5,5,4,7,6,4,7,8,25,17), nrow=5, byrow=FALSE)
dimnames(sizeper) <- list(c("No","Minor","Moderate","Major","Severe"), c("Punjab","Sindh","KPK","Balochistan","Islamabad"))

PlotCirc(sizeper,
          acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
          rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

par(mfrow=c(1,2))
PlotCirc(sizecon, main = "by Firm Size")
PlotCirc(sizeper, main = "by Region")


##=====Now mosaic plot in the framework of ggplot, Loan Application Outcome
##This plot is produced by excluding all the missing values...........
##dataset from STATA
library(haven)
ivprobit <- read_dta("D:/Utilisateurs/e0g411m05t7/Dropbox/ThesisResource/SME/RSME/ivprobit.dta")
View(ivprobit)

#take concerned variables
rpvt <- ivprobit[, c("a2","a6b","k30","rej" )]

# create new dataset without missing data 
rpvt2 <- na.omit(rpvt)

#Renaming Vaibles
library(reshape)
rpvt2 <- rename(rpvt2, c(a2="Region"))
rpvt2 <- rename(rpvt2, c(a6b="Size"))
rpvt2 <- rename(rpvt2, c(k30="Constraint"))
rpvt2 <- rename(rpvt2, c(rej="Loan"))

##Converting them into factors
library(car)
rpvt2$Loan <- Recode(rpvt2$Loan, "1='Approved'; 2='Rejected'")
rpvt2$Size <- Recode(rpvt2$Size, "0='Micro'; 1='Small'; 2='Medium'; 3='Large'")
rpvt2$Region <- Recode(rpvt2$Region, "1='Punjab'; 2='Sindh'; 3='KPK'; 4='Balochistan'; 5='Islamabad'")
rpvt2$Constraint <- Recode(rpvt2$Constraint , "0='No'; 1='Minor'; 2='Moderate'; 3='Major'; 4='Severe'")


##Now we are all set to produce a beautifull mosaic plot
ggplot(data = rpvt2) +
  geom_mosaic(aes( x = product(Size, Region), fill=factor(Size), 
  conds=product(Loan)), na.rm=TRUE, divider=mosaic("v")) +    
  theme(axis.text.x=element_text(angle=-25, hjust= .1)) + labs(x="Region", 
   title='f(Size, Region | Loan)') + guides(fill=guide_legend(title = "Size", reverse = TRUE))

#Now edit to make it more beautiful
ggplot(data = rpvt2) +
  geom_mosaic(aes( x = product(Size, Region), fill=factor(Size)), 
  na.rm=TRUE) +    theme(axis.text.x=element_text(angle=-25, hjust= .1)) +
  labs(x="Region of the Firm", title='f(Firm Size, Region | Loan Application Outcome)') + facet_grid(Loan ~.) + 
  guides(fill=guide_legend(title = "Firm Size", reverse = TRUE))

##==Now we move to make an Alluvial Diagram 
#Type of Financial Institution, Firm Size and Collateral
library(alluvial)

##OKAY NOW BRINGING MY OWN DATA
library(readxl)
extra <- read_excel("D:/Utilisateurs/e0g411m05t7/Dropbox/ThesisResource/SME/ExtraVisR/extra.xlsx")
View(extra)

#Reset mfrow
par(mfrow=c(1,1))

#HERE WE GO WITH THE PLOT
alluvial(extra[,1:3], freq=extra$Freq,
         col = ifelse(extra$Collateral == "No", "red2", "lightskyblue"),
         border = ifelse(extra$Collateral == "Yes", "grey", "grey"),
         hide = extra$Freq == 0,
         cex = 0.9,
         blocks=F
)

##===Next we plot Figure 5.9: Access to Finance and Gap
#using ggplot2
library(ggplot2)
library(ggthemes) # Load

Area    <- c(rep(c("Adult(%age)", "Female(%age)", "MSMEs(%age)", "A.Points(0000)", ("Microfinance(Mil)")), each = 2))
Status  <- c(rep(c("Gap","Actual"), times = 5))
Target  <- c(13, 37, 5, 20,7, 8, 35, 15,3.6, 6.4)
Data    <- data.frame(Area, Status, Target)
#Target  <- c(50, 13, 25, 5, 15, 7, 50, 35, 10, 3.6)

#Now plottting
p1 <- ggplot(Data, aes(x = Area, y = Target, fill = Status, label = Target)) +
  geom_bar(stat = "identity") + scale_fill_manual(values=c("#00CCFF", "#FF9933")) +
  geom_rangeframe() + theme_tufte() +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

p1 + labs(x = "Access to Finance") + labs(y = "Actual vs Target") 
#p1 + guides(fill=guide_legend(title=NULL) + theme(legend.position="top"))

##Another way to plot the same information
p1 <- ggplot(Data, aes(x = Area, y = Target, fill = Status, label = Target)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("#00BFFF","#DB7093")) +
  labs(x = "Access to Finance") + labs(y = "Actual vs Gap") +
  geom_text(size = 3.5, position = position_stack(vjust = 0.5))

##customization
p1 + scale_fill_discrete(name="Status",
                         breaks=c("Actual", "Gap"),
                         labels=c("Gap", "Actual"))


##========Correlation Analysis=======================
#Tetracoric Correlation between  ICT and financial Inclusion
library(polycor)
library(psych)
##Dummy Variables SME
smepak$Micro <- as.numeric(Size==0)
smepak$Small <- as.numeric(Size==1)
smepak$Medium <- as.numeric(Size==2)
smepak$Large <- as.numeric(Size==3)

#Dummy for AGE
smepak$Young <- as.numeric(Age < 10)
smepak$Mature <- as.numeric(Age >= 10 & Age <= 100 )
smepak$Missing <- as.numeric(Age >= 100 )

#Dummy for Regions
smepak$Punjab <- as.numeric(Region == 1)
smepak$Sindh <- as.numeric(Region == 2)
smepak$KPK <- as.numeric(Region == 3)
smepak$Balochistan <- as.numeric(Region == 4)
smepak$Islamabad <- as.numeric(Region == 5)

smepak$Account<-recode(smepak$Account,"-9=NA")

#Dummy for Finance
smepak$Bank <- as.numeric(Account == 1)
smepak$BankOver <- as.numeric(Overdraft == 1)
smepak$BankLoc <- as.numeric(Loc == 1)
smepak$Bankwc20 <- as.numeric(Wcap > 0 & Wcap <= 20)
smepak$Bankwc40 <- as.numeric(Wcap > 20 & Wcap <= 40)
smepak$Bankwc60 <- as.numeric(Wcap > 40 & Wcap <= 60)
smepak$Bankwc80 <- as.numeric(Wcap > 60 & Wcap <= 80)
smepak$Bankwc100 <- as.numeric(Wcap > 80 & Wcap <= 100)
smepak$wcyes <- as.numeric(Wcap > 0 & Wcap <= 100)

##Dummies for ICT
smepak$ElectronicM <- as.numeric(Email == 1)
smepak$Wwweb <- as.numeric(Website == 1)
smepak$Mobile <- as.numeric(Cellphone == 1)

forcorr <- smepak[, c(27:49)]
forcorr <- forcorr[, c(1:18,21:23)]
tetrachoric(forcorr,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE,
            delete=TRUE)

tetcor <- tetrachoric(forcorr)


#structure of the object
str(tetcor)
tetcor <- as.matrix(tetcor)
write.csv(cor(tetcor),file="tetcor.csv")

##list down the objects in the structure
coresult.tetcor.r=data.frame(tetcor$r)
or
df.rcx.p=data.frame(rcx$P)

##Finally write it to the csv file
write.csv(coresult.tetcor.r,'correlationmatrix.csv')

##Subsetting.............
ict <- forcorr[, c(1:14)]
fin <- forcorr[, c(16:21)]
ict <- smepak[, c(4,36:38)]

##=====Next we applied Canonical Correlation

library(ggplot2)
library(GGally)
library(CCA)

#Improting SME data from STATA LAB
library(haven)
Paksmesecond <- read_dta("D:/Utilisateurs/e0g411m05t7/Dropbox/SME/RSME/Paksmesecond.dta")
View(Paksmesecond)

##Subsetting data for cca
smepak <- subset(Paksmesecond, select = c("idstd", "ID","a3a","a6b","b1","b4","b5","b6","b7","b7a", "b8",
                                          "c22a", "c22b", "c28", "c30a", "c30b","SARd24a","d2","k6","k7","k8","k3bc","k30", "l9b", "l10" ))
##Renaming selected variables
colnames(smepak) <- c("idstd", "ID","Region","Size","Status","FemMngr","Year","Employee","MngrExp","Female", "Quality",
                      "Email", "Website", "Cellphone", "Electricity", "Telecomm","CCommerce","Sales","Account","Overdraft","Loc","Wcap","Access", "Empedu", "Emptr" )

#Subtract 2013 from the age column to compute age in years
calculateage <- 2013
smepak$Age <- calculateage - smepak$Year

#missing values
missing.values(smepak) <- c(-9, -7)
missing.values(smepak$Account) <- (-9)
missing.values(smepak)

##Subsetting for bank and ict
ict <- smepak[, 12:14]
fin <- smepak[, c(42:44,50)]
fin <- fin[, -c(4:8)]

##Apply cannonical correlation
correl <- matcor(ict, fin)

#plotting
img.matcor(correl, type = 2)
correl

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


#======THIS IS PROBIT MODEL FOR ICT AND FINANCE=============
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
