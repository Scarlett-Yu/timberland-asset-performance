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


#test stationarity and normality
for(i in 1:ncol(allxts)){
  #plot acf
  acf(na.omit(allxts[,i]),lag.max = 100,xlab = colnames(allxts)[i], ylab = 'ACF', main=' ')
  #plot pacf
  pacf(na.omit(allxts[,i])^2,lag.max = 100,xlab = colnames(allxts)[i], ylab = 'PACF', main=' ')
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
model.arima = auto.arima(xts.ncreif , max.order = c(2, 0 ,2) , stationary = TRUE , trace = T , ic = 'bic')

checkresiduals(model.arima)
# REIT
model.arima = auto.arima(REIT, stationary = TRUE , trace = T , ic = 'aicc')
# box test
Box.test(model.arima$residuals^2,lag=12, type="Ljung-Box")
for(i in 1:ncol(allts)){
  y = allts[,i]
  res=Box.test((y - mean(y))^2, lag=12, type="Ljung-Box")
  print(res$p.value)
}

# GARCH model
library("rugarch")
spec = ugarchspec(variance.model = list(model="sGARCH",
                                        garchOrder=c(1,1)),
                  #mean.model = list(armaOrder=c(0,0)),
                  distribution.model = "std")
# Nelson's egarch model
egarch.spec = ugarchspec(variance.model=list(model="eGARCH",
                                               garchOrder=c(1,1)),
                           #mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                           distribution.model="jsu", fixed.pars=list())
garch.fit1 = ugarchfit(egarch.spec, xts.ncreif)

plot(garch.fit1, which="all")

# backtesting model
ctrl = list(tol = 1e-7, delta = 1e-9)
garchroll1 <- ugarchroll(egarch.spec, xts.ncreif, n.start = 30, refit.every = 4, refit.window = "moving", VaR.alpha = 0.01 ,solver.control = ctrl, fit.control = list(scale = 1))
report(garchroll1, type = "VaR")

plot(garchroll1, which="all")

# Forecasting Risk and VaR
garchfcst <- ugarchforecast(garch.fit1, n.ahead = 12)
garchfcst
plot(garchfcst,which=1)
plot(garchfcst,which=3)

garch.fit2=ugarchfit(spec,data=ncreif, solver="hybrid", out.sample=5)
garchfcst2<-ugarchforecast(garch.fit2, data = NULL, n.ahead = 10, n.roll = 5, external.forecasts = list(mregfor = NULL, vregfor = NULL))
garchfcst2
plot(garchfcst2,which=2)
plot(garchfcst2,which=4)














