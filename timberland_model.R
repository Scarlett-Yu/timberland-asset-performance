load("timberland.Rdata")
library(tseries)
library(forecast)
library(dygraphs)
library(PerformanceAnalytics)
require(ggplot2)
require(GGally)
library(psych)
library(xts)
#####################descriptive statistics############################
#visualization
dygraph(allxts*100, main = "Index values of analyzed asset quarterly returns", ylab = "Return%")%>% 
  dyRangeSelector()%>%
  dyLegend(width = 800)

ggpairs(as.data.frame(allts))+theme_bw()
describe(allxts)[,-1]

VaR(allts, p=.95, method="modified")
ES(allts, p=.95, method="modified")
# normality
apply(allts,2,jarque.bera.test)
# stationarity
apply(allts,2,adf.test)
apply(allts,2,pp.test)

op = par(mfrow=c(1,2))

#test stationarity and normality
for(i in 1:ncol(allxts)){
  #plot acf
  acf(na.omit(allxts[,i]),lag.max = 100,xlab = colnames(allxts)[i], ylab = 'ACF', main=' ')
  #plot pacf
  pacf(na.omit(allxts[,i]),lag.max = 100,xlab = colnames(allxts)[i], ylab = 'PACF', main=' ')
}

##############GARCH model estimation, Backtesting the risk model and Forecasting#############
#Ncreif
xts.ncreif <- allxts[, "NCREIF"]
ts.ncreif <- allts[, "NCREIF"]
row.names(NCREIF) <- NCREIF$Date
ncreif = NCREIF["TMBERLND Index"]
REIT = as.data.frame(allts[,"REIT"])
row.names(REIT)=as.yearqtr(time(allts))
# ARIMA 
# ncreif
model.arima = auto.arima(xts.ncreif, stationary = TRUE , trace = T , ic = 'aic')

checkresiduals(model.arima)
# REIT
model.arima2 = auto.arima(REIT, stationary = TRUE , trace = T , ic = 'aic')
checkresiduals(model.arima2)

# box test
Box.test(model.arima$residuals^2,lag = 24,type="Ljung-Box")
Box.test(model.arima2$residuals^2, lag = 100, type="Ljung-Box")
#colnames(allts)
for(i in 1:ncol(allts)){
  y = allts[,i]
  res=Box.test((y - mean(y))^2, lag = 12,type="Ljung-Box")
  print(res$p.value)
}

# GARCH model
library("rugarch")
spec = ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model = list(armaOrder=c(1,1)),
                  distribution.model = "std")
# Nelson's egarch model
egarch.spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(0,0)),  
                         distribution.model="std")
garch.fit1 = ugarchfit(egarch.spec, xts.ncreif)
# simulation
set.seed(123)
tseq = seq(as.Date("1987/1/1"), as.Date("2020/6/1"), "weeks")
sim = ugarchsim(garch.fit1,n.sim=length(tseq), n.start=0, m.sim=1, startMethod="sample")
simseries = xts(sim@simulation$seriesSim, order.by = tseq)
auto.arima(simseries , trace = T , ic = 'bic')


plot(garch.fit1, which="all")

# backtesting model
garchroll1 <- ugarchroll(egarch.spec, data=simseries, n.start = 1000, refit.every = 100, refit.window = "moving",VaR.alpha = 0.01,solver="hybrid", fit.control = list(scale = 1))
report(garchroll1, type = "VaR",VaR.alpha = 0.01, conf.level = 0.99)

plot(garchroll1, which="all")
plot(garchroll1, which=4)
# Forecasting Risk and VaR
garchfcst <- ugarchforecast(garch.fit1, n.ahead = 12)
garchfcst
plot(garchfcst,which=1)
plot(garchfcst,which=3)

garch.fit2=ugarchfit(egarch.spec,data=xts.ncreif, solver="hybrid", out.sample=5)
garchfcst2<-ugarchforecast(garch.fit2, data = NULL, n.ahead = 10, n.roll = 5, external.forecasts = list(mregfor = NULL, vregfor = NULL))
garchfcst2
plot(garchfcst2,which=2)
plot(garchfcst2,which=4)









