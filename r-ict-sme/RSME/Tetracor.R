#=============Tetrachoric Polychoric Correlation========##
getwd()
#Tetrachoric/Polychoric Correlation Coefficient in R
install.packages("polycor")
library("polycor")
install.packages("psych")
library("psych")

x <- c(10, 20, 30, 40)
y <- matrix(x, 2, 2)
y
polychor(y, ML=T, std.err=T)

#It is also possible to supply raw data in the form of two vectors, x and y. For example, let x = c(1, 2, 1, 1, 2, 1, 1, 1, 2, 1), y = c(2, 1, 2, 2, 1, 1, 1, 2, 1, 2). Note that these are two variables measured on the same subjects/objects, so their lengths and orderings must match.

x <- c(1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 2, 2, 1, 1, 2, 2)
y <- c(1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 2, 1)
polychor(x, y, ML=T, std.err=T)
tetrachoric(x, y=NULL,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE,
            delete=TRUE)
lsat6

#if(require(mnormt)) {
data(bock)
tetrachoric(lsat6)
polychoric(lsat6) #values should be the same
tetrachoric(matrix(c(44268,193,14,0),2,2)) #MPLUS reports.24
#Do not apply continuity correction -- compare with previous analysis!
tetrachoric(matrix(c(44268,193,14,0),2,2),correct=0)
#the default is to add correct=.5 to 0 cells
tetrachoric(matrix(c(61661,1610,85,20),2,2)) #Mplus reports .35
tetrachoric(matrix(c(62503,105,768,0),2,2)) #Mplus reports -.10
tetrachoric(matrix(c(24875,265,47,0),2,2)) #Mplus reports 0
polychoric(matrix(c(61661,1610,85,20),2,2)) #Mplus reports .35
polychoric(matrix(c(62503,105,768,0),2,2)) #Mplus reports -.10
polychoric(matrix(c(24875,265,47,0),2,2)) #Mplus reports 0
#Do not apply continuity correction- compare with previous analysis
tetrachoric(matrix(c(24875,265,47,0),2,2), correct=0)
polychoric(matrix(c(24875,265,47,0),2,2), correct=0) #the same result

#examples from Kirk 1973
#note that Kirk's tables have joint probability followed by marginals, but
#tetrachoric needs marginals followed by joint probability
tetrachoric(c(.5,.5,.333333)) #should be .5
tetrachoric(c(.5,.5,.1150267)) #should be -.75
tetrachoric(c(.5,.5,.397584)) #should e .8
tetrachoric(c(.158655254,.158655254,.145003)) #should be .99


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

##checking normality of the data
## Have a look at the densities
plot(density(Mobile));plot(density(Wwweb))

## Perform the test
shapiro.test(Mobile); shapiro.test(Wwweb)

## Plot using a qqplot
qqnorm(Mobile);qqline(Wwweb, col = 2)
qqnorm(words2);qqline(words2, col = 2)

names(smepak)
names(forcorr)

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


ict <- forcorr[, c(1:14)]
fin <- forcorr[, c(16:21)]
ict <- smepak[, c(4,36:38)]


