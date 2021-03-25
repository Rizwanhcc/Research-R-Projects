##==============Smallholders=======================###

#This file contains smallholders farmers....!!
#Entering data directly
#smallholders farmers in three coutries


library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
charts.data <- read.csv("copper-data-for-tutorial.csv")
#http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html
#this graph is produced by following above link


##Finally decided this plot 
Main <- c("Mozambique","Mozambique","Mozambique", "Tanzania", "Tanzania", "Tanzania", "Pakistan", "Pakistan" , "Pakistan")
ICT <- c("Mobile", "Sim", "Use","Mobile", "Sim", "Use","Mobile", "Sim", "Use")
Value <- c(45,48,55,56,56,77,70,65,73)
smpak <- data.table(Main, ICT, Value)

p4 <- ggplot() + geom_bar(aes(y = Value, x = Main, fill = ICT), data = smpak,
                          stat="identity")
p4

##EDITING CHART
p4 <- p4 + geom_text(data=smpak, aes(x = Main, y = Value,
                                label = paste0(Value,"%")), size=4)
p4

##Adjusting value positions

smpak <- ddply(smpak, .(Main),
                     transform, pos = cumsum(Value) - (0.5 * Value))

p4 <- ggplot() + geom_bar(aes(y = Value, x = Main, fill = ICT), data = smpak,
                          stat="identity")

p4 <- p4 + geom_text(data=smpak, aes(x = Main, y = pos, label = paste0(Value,"%")),
                     size=4)
p4

##Changing Axix...
p4 <- p4 + labs(x="", y="") +
  ggtitle("Mobile Phone Access and Use by Smallholders (%)")
p4

p4 <- p4 + theme(legend.position="bottom", legend.direction="horizontal",
                 legend.title = element_blank())
p4





##Another way to do the same
##Finally decided this plot 
Main <- c("Mozambique","Mozambique","Mozambique", "Tanzania", "Tanzania", "Tanzania", "Pakistan", "Pakistan" , "Pakistan")
ICT <- c("Mobile", "Sim", "MMoney","Mobile", "Sim", "MMoney","Mobile", "Sim", "MMoney")
Value <- c(45,48,0,56,56,19,70,65,0)
smpak <- data.table(Main, ICT, Value)

#plot and save 
p7 <- ggplot(smpak, aes(Main, Value, fill = ICT)) + 
  geom_bar(stat="identity", position = "dodge") + scale_fill_brewer(palette = "Set3")

p7

#Adjust legends
p7 <- p7 + theme(legend.position="bottom", legend.direction="horizontal",
                 legend.title = element_blank())

##Changing Axix...
p7 <- p7 + labs(x="", y="") +
  scale_y_continuous(labels = dollar_format(suffix = "", prefix = "")) +
  ggtitle("ICT Access (%)")
p7

#============================================================================

##ICT Capability.....
Main <- c("Mozambique","Tanzania","Pakistan", "Mozambique","Tanzania","Pakistan","Mozambique","Tanzania","Pakistan")
ICT <- c("Low","Low","Low", "Moderate", "Moderate","Moderate", "High", "High", "High")
Value <- c(45,9,7,27,15,37,25,68,24)
smpak <- data.table(Main, ICT, Value)

#plot and save 
p5 <- ggplot(smpak, aes(Main, Value, fill = ICT)) + 
      geom_bar(stat="identity", position = "dodge") + scale_fill_brewer(palette = "Set2")

#Adjust legends
p5 <- p5 + theme(legend.position="bottom", legend.direction="horizontal",
                 legend.title = element_blank())

##Changing Axix...
p5 <- p5 + labs(x="", y="") +
  scale_y_continuous(labels = dollar_format(suffix = "", prefix = "")) +
  ggtitle("ICT Capability (%)")
p5

#============================================================================

##Financial Instruments.....
Main <- c("Mozambique","Mozambique","Mozambique","Tanzania","Tanzania","Tanzania","Pakistan","Pakistan","Pakistan")
ICT <- c("Total", "Savings","Credit","Total", "Savings","Credit","Total", "Savings","Credit")
Value <- c(3,2,1,12,5,6,18,3,14)
smpak <- data.table(Main, ICT, Value)

#plot and save 
p6 <- ggplot(smpak, aes(Main, Value, fill = ICT)) + 
  geom_bar(stat="identity", position = "dodge") + scale_fill_brewer(palette = "Set1")

#Adjust legends
p6 <- p6 + theme(legend.position="bottom", legend.direction="horizontal",
                 legend.title = element_blank())

##Changing Axix...
p6 <- p6 + labs(x="", y="") +
  scale_y_continuous(labels = dollar_format(suffix = "", prefix = "")) +
  ggtitle("Financial Portofolio")
p6

#combining plots
par(mfrow=c(1,3))


#with ggplot above doesnt work
library(gridExtra)
grid.arrange(p7, p5, p6, nrow=1, ncol=3)

