#=================Dissaggregating Quarterly Data====================##
getwd()
#https://github.com/christophsax/tempdisagg/blob/master/demo/tempdisagg.R
#https://ec.europa.eu/eurostat/cros/content/download_en

require(tempdisagg)
data(swisspharma)
plot(sales.a)
head(swisspharma)

##simple way to convert annual to quarterly series
m1 <- td(sales.a ~ 1, to = "quarterly", method = "denton-cholette")
predict(m1)
plot(predict(m1))

plot(exports.q)
m2 <- td(sales.a ~ 0 + exports.q, method = "denton-cholette")
plot(predict(m2))
m3 <- td(sales.a ~ exports.q)

plot(sales.q)
lines(predict(m2), col = "blue")  # Denton-Cholette
lines(predict(m3), col = "red")   # Chow-Lin

library(tsDyn)
vignette('tsDyn')



xxx <- ts(lfd, freq=4, start=c(1971,1))
dat <- BBQ(xxx, name="Dating Business Cycles of Pakistan")
show(dat)
summary(dat)
plot(dat)
plot(dat, xxx, col="red")


lgdp <- ts(lgdp, freq=4, start=c(1971,1))




library(BCDating)
data("lgdp")
dat <- BBQ(lgdp, name="Dating Business Cycles of Pakistan")
show(dat)
summary(dat)
plot(dat)
plot(dat,Iran.non.Oil.GDP.Cycle)
data("lgdp")
plot(MBRI.Iran.Dating)


data("Iran.non.Oil.GDP.Quarterly.Growth")
data("MBRI.Iran.Dating")
avggrowth <- avgts(Iran.non.Oil.GDP.Quarterly.Growth,MBRI.Iran.Dating)
cbind(avggrowth,Iran.non.Oil.GDP.Quarterly.Growth)
plot(MBRI.Iran.Dating,avggrowth)
plot(MBRI.Iran.Dating,Iran.non.Oil.GDP.Quarterly.Growth,averages=TRUE)
data("Iran.non.Oil.GDP.Cycle")
dat <- BBQ(Iran.non.Oil.GDP.Cycle, name="Dating Business Cycles of Iran")
show(dat)
summary(dat)
plot(dat)
plot(dating)
data(MBRI.Iran.Dating)
plot(dat,MBRI.Iran.Dating)
