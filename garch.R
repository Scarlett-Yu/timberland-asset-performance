library(rugarch)
garchSpec <- ugarchspec(
  variance.model=list(model="sGARCH",
                      garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(0,0)), 
  distribution.model="std")
garchFit <- ugarchfit(spec=garchSpec, data=all_return2$`TMBERLND Index`)
coef(garchFit)
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat)
library(astsa)
library(xts)
dat = diff(all_return2$`TMBERLND Index`)[-1]
acf2(djiar) # exhibits some autocorrelation (not shown)
acf2(djiar^2) # oozes autocorrelation (not shown)
library(fGarch)
summary(djia.g <- garchFit(~arma(1,0)+garch(1,1), data=dat,cond.dist='std'))
plot(djia.g)

library(fGarch)



