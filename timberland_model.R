require(devtools)
load("timberland.Rdata")
library(tseries)
library(forecast)
library(dygraphs)
library(PerformanceAnalytics)
require(ggplot2)
require(GGally)
library(psych)
library("rugarch")
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
#visualization
dygraph(all.qtr*100, main = "Index values of analyzed asset quarterly returns", ylab = "Return%")%>%
  dyRangeSelector()%>%
  dyLegend(width = 800)
#colnames(all.qtr)

##############plot asset return series##############
png("latex_thesis_template/img/asset_return.png", width = 300, height = 250, units='mm', res = 500)
op = par(mfrow = c(2, 2))
fs = par(ps=18)
#plot(all.qtr[,1], main = "NCREIF timber index", ylab = "Quarterly Return %")
plot(all.qtr[,c(1,2,6)],
           main="Timber vs. Real Estate (a)",
           plot.type="single",
     col=c(1, 2, 3) ,ylab="Quarterly Return %")
legend("topright", legend = colnames(all.qtr)[c(1,2,6)], col = 1:3, lty=1,bty="n")
plot(all.qtr[,c(1,3,4)],
     main="Timber vs. Stock Market (b)",
     plot.type="single",
     col=1:3 ,ylab="Quarterly Return %")
legend("topright", legend = colnames(all.qtr)[c(1,3,4)], col = 1:3, lty=1,bty="n")
plot(all.qtr[,c(1,5,8)],
     main="Timber vs. Canadian Debt Securities (c)",
     plot.type="single",
     col=1:3 ,ylab="Quarterly Return %")
legend("topright", legend = colnames(all.qtr)[c(1,5,8)], col = 1:3, lty=1,bty="n")
plot(all.qtr[,c(1,7)],
     main = "Timber Private vs. Public (d)",
     plot.type = "single",
     col = 1:2,ylab="Quarterly Return %")
legend("topright", legend = colnames(all.qtr)[c(1,7)], col = 1:2, lty=1,bty="n")
dev.off()

############plot asset cov ##############
png("latex_thesis_template/img/correlation.png", width = 200, height = 200, units='mm', res = 600)
ggpairs(as.data.frame(all.qtr))+theme_bw()
dev.off()

########descriptive#########
d0 = describe(all.qtr)[,c(3,4,8,9,11,12)]

d1 = VaR(all.qtr, p=.95, method="modified")
d2 = ES(all.qtr, p=.95, method="modified")
d12 = t(rbind(d1,d2))
# normality
d3 = c()
for(i in 1:ncol(all.qtr)){
  p=jarque.bera.test(all.qtr[,i])$p.value
  d3 = c(d3,p)
}

d = cbind(d0, d12, d3, d4)
colnames(d)[9]<-"J-B"

xtable(d)
###################################

##########stationarity#######################
# stationarity
apply(all.qtr,2,adf.test)
#apply(all.qtr,2,pp.test)
d4 = c()
for(i in 1:ncol(all.qtr)){
  p=adf.test(all.qtr[,i],k=5)$p.value
  d4 = c(d4,p)
}
d4 = as.data.frame(d4)

png("latex_thesis_template/img/acf.png", width = 250, height = 400, units='mm', res = 500)
op = par(mfrow=c(4,2))
fs = par(ps=16)
#test stationarity and normality
for(i in 1:ncol(all.qtr)){
  #plot acf
  acf(na.omit(all.qtr[,i]),lag.max = 100,xlab = colnames(all.qtr)[i], ylab = 'ACF', main=' ')
  #plot pacf
  #pacf(na.omit(all.qtr[,i]),lag.max = 100,xlab = colnames(all.qtr)[i], ylab = 'PACF', main=' ')
}
dev.off()
##############GARCH model estimation, Backtesting the risk model and Forecasting#############
# test ARCH effect
#colnames(all.qtr)
res = c()
for(i in 1:ncol(all.qtr)){
  y = all.qtr[,i]
  #log return
  #y = log(y + 1)
  result=Box.test((y - mean(y))^2, lag = 12,type="Ljung-Box")
  #print(colnames(all.qtr)[i])
  res = c(res, result$p.value) 
}
res = as.data.frame(res)
row.names(res) <- colnames(all.qtr)
colnames(res) <- "p-value"
xtable(res)
# we found NTI NPI NAREIT has significant ARCH effect
#Ncreif
xts.nti <- all.qtr[, "NTI"]
xts.npi <- all.qtr[, "NPI"]
xts.nareit <- all.qtr[,"NAREIT"]


# ARMA mean model
# ncreif
model.arima = auto.arima(xts.nti, stationary = T,trace = F , ic = 'bic',max.P = 0,
                         max.Q = 0)
model.arima
summary(model.arima)
checkresiduals(model.arima)
#NPI
model.arima2 = auto.arima(xts.npi, max.Q = 0,max.P=0,stationary = T, trace = F , ic = 'bic')
model.arima2
summary(model.arima2)

checkresiduals(model.arima3)
#NAREIT
model.arima3 = auto.arima(xts.nareit,trace = F, stationary = T , ic = 'aic')
model.arima3
summary(model.arima3)
checkresiduals(model.arima3)
sink('latex_thesis_template//arimaoutput.txt')
cat("NTI\n")
summary(model.arima)
cat("\nNPI\n")
summary(model.arima2)
cat("\nNAREIT\n")
summary(model.arima3)
sink()

# box test
Box.test(model.arima$residuals^2,lag = 12,type="Ljung-Box")
Box.test(model.arima2$residuals^2, lag = 10, type="Ljung-Box")
Box.test(model.arima3$residuals^2, lag = 10, type="Ljung-Box")#<0.05


######### GARCH model #############
sink('latex_thesis_template//garchoutput1.txt')
cat("============NTI============")
garch.fit1
sink()
sink('latex_thesis_template//garchoutput2.txt')
cat("============NPI============")
garch.fit2
sink()
sink('latex_thesis_template//garchoutput3.txt')
cat("===========NAREIF==========")
garch.fit3
sink()

spec = ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model = list(armaOrder=c(2,2)),
                  distribution.model = "std")
# Nelson's egarch model
egarch.spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(2,2)),  
                         distribution.model="std")

garch.fit1 = ugarchfit(egarch.spec, log(xts.nti+1))
garch.fit1
plot(garch.fit1, which="all")

# simulation
set.seed(123)
tseq = seq(as.Date("1987/1/1"), as.Date("2020/6/1"), "weeks")
sim = ugarchsim(garch.fit1,n.sim=length(tseq), n.start=0, m.sim=1, startMethod="sample")
simseries = xts(sim@simulation$seriesSim, order.by = tseq)
auto.arima(simseries , trace = T , ic = 'bic')


# backtesting model
garchroll1 <- ugarchroll(egarch.spec, data=simseries, n.start = 1000, refit.every = 100, refit.window = "moving",VaR.alpha = 0.01,solver="hybrid", fit.control = list(scale = 1))
report(garchroll1, type = "VaR",VaR.alpha = 0.01, conf.level = 0.99)

plot(garchroll1, which="all")
plot(garchroll1, which=4)
# Forecasting Risk and VaR
garchfcst <- ugarchforecast(garch.fit1, n.ahead = 4)
garchfcst
plot(garchfcst,which=1)
plot(garchfcst,which=3)

garch.fit2=ugarchfit(egarch.spec,data=xts.nti, solver="hybrid", out.sample=5)
garchfcst2<-ugarchforecast(garch.fit2, data = NULL, n.ahead = 8, n.roll = 5, external.forecasts = list(mregfor = NULL, vregfor = NULL))
garchfcst2
plot(garchfcst2,which=2)
plot(garchfcst2,which=4)


#########################################################
spec = ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model = list(armaOrder=c(2,1)),
                  distribution.model = "std")
# Nelson's egarch model
egarch.spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(2,1)),  
                         distribution.model="std")
garch.fit2 = ugarchfit(egarch.spec, xts.npi)
# simulation
set.seed(123)
tseq = seq(as.Date("1987/1/1"), as.Date("2020/6/1"), "weeks")
sim = ugarchsim(garch.fit2,n.sim=length(tseq), n.start=0, m.sim=1, startMethod="sample")
simseries = xts(sim@simulation$seriesSim, order.by = tseq)
auto.arima(simseries , trace = T , ic = 'bic')

plot(garch.fit1, which="all")

# backtesting model
garchroll2 <- ugarchroll(egarch.spec, data=simseries, n.start = 1000, refit.every = 10, refit.window = "moving",VaR.alpha = 0.01,solver="hybrid", fit.control = list(scale = 1))
report(garchroll2, type = "VaR",VaR.alpha = 0.01, conf.level = 0.99)

plot(garchroll2, which="all")
plot(garchroll2, which=4)
# Forecasting Risk and VaR
garchfcst <- ugarchforecast(garch.fit2, n.ahead = 4)
garchfcst
plot(garchfcst,which=1)
plot(garchfcst,which=3)

garch.fit2=ugarchfit(egarch.spec,data=xts.npi, solver="hybrid", out.sample=5)
garchfcst2<-ugarchforecast(garch.fit2, data = NULL, n.ahead = 8, n.roll = 5, external.forecasts = list(mregfor = NULL, vregfor = NULL))
garchfcst2
plot(garchfcst2,which=2)
plot(garchfcst2,which=4)

# Nelson's egarch model
egarch.spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(0,0)),  
                         distribution.model="std")
garch.fit3 = ugarchfit(egarch.spec, xts.nareit)
#########################VAR############################
