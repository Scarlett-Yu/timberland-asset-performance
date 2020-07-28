allxts = na.omit(all.qtr)

library(vars)
library(forecast)
fit = VAR(allxts[,-8], lag.max = 3, type = "none")
summary(fit, equation = "NTI")
serial.test(fit, lags.pt = 10, type = "PT.asymptotic")
plot(fit, names = "NTI")
pre = predict(fit, n.ahead = 40, ci=0.95)
pre$fcst$NTI
png("plots/var10y.png", width = 250, height = 400, units='mm', res = 500)
fs = par(ps=16)
plot(pre, plot.type = "single", names = "NTI")
dev.off()
