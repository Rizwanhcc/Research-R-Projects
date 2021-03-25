#==========SME DATA Directly from World Bank website==============

library(WDI)

wdi_dat <- WDI(indicator = c("IC.FRM.FCHAR.CAR1", "IC.FRM.WRKF.WK12", "IC.FRM.FIN.FIN12", "IC.FRM.FIN.FIN13", "IC.FRM.FIN.FIN14", "IC.FRM.FIN.FIN15", "IC.FRM.FIN.FIN16","IC.FRM.INNOV.T5", "IC.FRM.INNOV.T6","IC.FRM.OBS.OBST9","IC.FRM.WRKF.WK8"), 
                start = 2000, end = 2015, extra = TRUE) 


wdi_dat <- WDI( country = c("PK","BD"),
                indicator = c("IC.FRM.FCHAR.CAR1", "IC.FRM.WRKF.WK12", "IC.FRM.FIN.FIN12", "IC.FRM.FIN.FIN13", "IC.FRM.FIN.FIN14", "IC.FRM.FIN.FIN15", "IC.FRM.FIN.FIN16","IC.FRM.INNOV.T5", "IC.FRM.INNOV.T6","IC.FRM.OBS.OBST9","IC.FRM.WRKF.WK8"), 
                start = 2000, end = 2015, extra = TRUE) 

# age, size,bnkinv,bankwc, loc,bankaccount, atofconst, web, email,obsedu,mngrexp
aa <- subset(wdi_dat, year==2007 | year==2013)
head(aa)
df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))