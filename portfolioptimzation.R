library(RColorBrewer)
library(fPortfolio)

pacman::p_load(matrixcalc,knitr,dygraphs,ggthemes,highcharter,viridis,tibbletime,timetk,tidyquant,tidyverse,fPortfolio,xts)
#0. Prepare data
allxts = na.omit(all.qtr)
returns_ts <- timeSeries(allxts, charvec = as.Date(index(allxts)))
returns_ts2 <- as.timeSeries(na.omit(all.qtr[,c(2:6,8)]))
colnames(returns_ts2)
#1. using equal weights in this case
spec <- portfolioSpec()
#the portfolio constraints set up to for 10%-40% real estate and timberland, at least 10% for treasury bonds, at least 10% for large-cap stocks, at least 5% for small-cap stocks and at least 2.5% for treasury bills.
groupConstraints = c("minsumW[c(1,2,6,7)] = 0.1",
                     "maxsumW[c(1,2,6,7)] = 0.4",
                     "minW[c(3,4,5,8)] = c(0.1,0.05,0.1,0.025)",
                    "maxW[c(3,4,5,8)] = c(0.5,0.5,0.5,0.15)"
)
groupConstraints2 = c("minW[c(2,3,4,6)] =  c(0.1,0.05,0.1,0.025)",
                      "maxW[c(2,3,4,6)] = c(0.5,0.5,0.5,0.15)"
)
nAssets <- ncol(returns_ts)
setWeights(spec)<-rep(1/nAssets, times = nAssets) 
constraints <- 'LongOnly'

# calculate the properties of the portfolio
# Now let us display the results from the equal weights portfolio, the assignment of weights, and the attribution of returns and risk.
ewPortfolio <- feasiblePortfolio(returns_ts, spec, constraints)
print(ewPortfolio)
# col <- divPalette(ncol(returns_ts), "RdBu")
# op=par(mfcol=c(3,2))
# weightsPie(ewPortfolio, radius = 0.7, col = col, box = F)
# mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
#         font = 2, cex = 0.7, adj = 0)
# weightedReturnsPie(ewPortfolio, radius = 0.7, col = col, box = F)
# mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
#         font = 2, cex = 0.7, adj = 0)
# covRiskBudgetsPie(ewPortfolio, radius = 0.7, col = col, box = F)
# mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
#         font = 2, cex = 0.7, adj = 0)

# COMPUTE A MINIMUM RISK EFFICIENT PORTFOLIO!
minriskSpec <- portfolioSpec()
#targetReturn <- getTargetReturn(ewPortfolio@portfolio)["mean"]
setTargetReturn(minriskSpec) <- 0.024
minriskPortfolio <- efficientPortfolio(
  data = returns_ts,
  spec = minriskSpec,
  constraints = #constraints
    groupConstraints
  )
print(minriskPortfolio)

# Target Returns and Risks:
#   mean    Cov   CVaR    VaR 
# 0.0210 0.0277 0.0449 0.0338 
minriskPortfolio2 <- efficientPortfolio(
  data = returns_ts2,
  spec = minriskSpec,
  constraints = groupConstraints2)
print(minriskPortfolio2)

##backtesting MV
returns_ts3 = as.xts(returns_ts)
returns_ts3$benchmark  = as.vector(returns_ts2%*%getWeights(minriskPortfolio2))[27:159]
returns_ts3 = timeSeries(returns_ts3, charvec = as.Date(index(allxts)))

timberPortfolios = portfolioBacktesting(formula = benchmark ~ NTI + NPI + SP500 + RU2000 + T.bonds10Y + NAREIT+Timber.REITs+T.bills3M,data = returns_ts3,spec = minriskSpec,constraints = groupConstraints)
setWindowsHorizon(timberPortfolios$backtest) = "12m"
setSmootherLambda(timberPortfolios$backtest) = "6m"
timbersmooth = portfolioSmoothing(timberPortfolios)
png("latex_thesis_template/img/mvback.png", width = 150, height = 200, units='mm', res = 500)
backtestPlot(timbersmooth,cex = 0.6,font=1,family = "mono")
dev.off()
#backtestStats(timbersmooth, FUN = "rollingVaR")
####################plot######################
png("latex_thesis_template/img/pie.png", width = 200, height = 300, units='mm', res = 500)
op=par(mfcol=c(3,2))
fs = par(ps=18)
col <- qualiPalette(ncol(returns_ts), "Dark2")
weightsPie(minriskPortfolio, col = col, box = F)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(minriskPortfolio, col = col, box = F)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(minriskPortfolio, col = col, box = F)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)

# globminSpec <- portfolioSpec()
# globminPortfolio <- minvariancePortfolio(
#   data = returns_ts,
#   spec = globminSpec,
#   constraints = groupConstraints)
# print(globminPortfolio)
# col <- seqPalette(ncol(returns_ts), "YlGn")
# weightsPie(globminPortfolio, box = FALSE, col = col)
# mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
#         line = 1.5, font = 2, cex = 0.7, adj = 0)
# weightedReturnsPie(globminPortfolio, box = FALSE, col = col)
# mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
#         line = 1.5, font = 2, cex = 0.7, adj = 0)
# covRiskBudgetsPie(globminPortfolio, box = FALSE, col = col)
# mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
#         line = 1.5, font = 2, cex = 0.7, adj = 0)

tgSpec <- portfolioSpec()
#for tangency portfolio, mean value of 3m t-bills are set as risk free rate
setRiskFreeRate(tgSpec) <- mean(returns_ts[,8])
tgPortfolio <- tangencyPortfolio(
  data = returns_ts,
  spec = tgSpec,
  constraints = groupConstraints)
print(tgPortfolio)
col <- seqPalette(ncol(returns_ts), "BuPu")
weightsPie(tgPortfolio, box = FALSE, col = col)
mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(tgPortfolio, box = FALSE, col = col)
mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(tgPortfolio, box = FALSE, col = col)
mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
dev.off()
#Now we are ready to start the optmization and pass data, specification and constraints to the function portfolioFrontier(). We can examine the result using a standard print() command.
rm(op)
# compute the efficient frontier

setNFrontierPoints(tgSpec) <-50
tgfrontier <- portfolioFrontier(returns_ts, tgSpec, groupConstraints)
print(tgfrontier)
setNFrontierPoints(tgSpec) <-50
tgfrontier1 <- portfolioFrontier(returns_ts, tgSpec1, groupConstraints)
print(tgfrontier1)
png("latex_thesis_template/img/frontier.png", width = 150, height = 200, units='mm', res = 500)
op=par(mfrow=c(2,1))
# plot efficient frontier
tailoredFrontierPlot(object = tgfrontier, risk = "Cov")
text <- "Mean-Variance Portfolio Efficient Frontiers"
mtext(text, side = 3, line = 0.5, font = 2, cex = 1)
tailoredFrontierPlot(object = tgfrontier1,risk = "CVaR")
text <- "Mean-CVaR Portfolio Efficient Frontiers"
mtext(text, side = 3, line = 0.5, font = 2, cex = 1)
dev.off()
# plot weights
weightsPlot(frontier, col=rainbow(dim(returns_ts)[2]))
weightsPlot(frontier)
text <- "Mean-Variance Portfolio -  Constraints"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(frontier)
covRiskBudgetsPlot(frontier)

set.seed(12)
frontierPlot(object = frontier, pch = 19, cex = 0.5)
monteCarloPoints(object = frontier, mcSteps = 5000, pch = 19,
                   cex = 1,col="#D53E4F")
#twoAssetsLines(object = frontier, col = "orange", lwd = 2)
frontierpts <- frontierPoints(object = frontier)
lines(frontierpts, col = "blue", lwd = 2)



#COMPUTE A LOWEST RETURN MEAN-CVAR PORTFOLIO GIVEN A RETURN
minriskSpec <- portfolioSpec()
setType(minriskSpec) <- "CVAR"
setSolver(minriskSpec) <- "solveRglpk.CVAR"
setTargetReturn(minriskSpec) <- 0.024
minriskPortfolio <- efficientPortfolio(
  data = returns_ts,
  spec = minriskSpec,
  constraints = groupConstraints
  )
print(minriskPortfolio)
png("latex_thesis_template/img/pie2.png", width = 200, height = 300, units='mm', res = 500)
op=par(mfcol=c(3,2))
fs = par(ps=18)
col <- qualiPalette(ncol(returns_ts), "Dark2")
weightsPie(minriskPortfolio, col = col, box = F)
mtext(text = "Minimal Risk M-CVaR Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(minriskPortfolio, col = col, box = F)
mtext(text = "Minimal Risk M-CVaR Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(minriskPortfolio, col = col, box = F)
mtext(text = "Minimal Risk M-CVaR Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)



tgSpec1 <- portfolioSpec()
setType(tgSpec1) <- "CVAR"
setSolver(tgSpec1) <- "solveRglpk.CVAR"

#for tangency portfolio, mean value of 3m t-bills are set as risk free rate
setRiskFreeRate(tgSpec1) <- mean(returns_ts[,-1])
tgPortfolio <- tangencyPortfolio(
  data = returns_ts,
  spec = tgSpec1,
  constraints = groupConstraints)
print(tgPortfolio)
col <- seqPalette(ncol(returns_ts), "BuPu")
weightsPie(tgPortfolio, box = FALSE, col = col)
mtext(text = "Tangency M-CVaR Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(tgPortfolio, box = FALSE, col = col)
mtext(text = "Tangency M-CVaR Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(tgPortfolio, box = FALSE, col = col)
mtext(text = "Tangency M-CVaR Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
dev.off()
##backtesting M-CVaR

timberPortfolios = portfolioBacktesting(formula = benchmark ~ NTI + NPI + SP500 + RU2000 + T.bonds10Y + NAREIT+Timber.REITs+T.bills3M,data = returns_ts3,spec = minriskSpec,constraints = groupConstraints)
setSmootherLambda(timberPortfolios$backtest) = "6m"
timbersmooth = portfolioSmoothing(timberPortfolios)
png("latex_thesis_template/img/mcarback.png", width = 150, height = 200, units='mm', res = 500)
backtestPlot(timbersmooth,cex = 0.6,font=1,family = "mono")
#backtestStats(timbersmooth, FUN = "rollingVaR")
dev.off()
























frontier <- portfolioFrontier(returns_ts, minriskSpec, constraints)



frontierPlot(object=frontier,type='l')

tailoredFrontierPlot(object=frontier)



weightsPlot(frontier, col=rainbow(assets))



# convert time series format
return.ts <- as.timeSeries(returns.data)

# specification: solver and efficient fronier 
spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
setNFrontierPoints(spec) <- 6

# constraints
constraints <- c('LongOnly')
portfolioConstraints(return.ts, spec, constraints)

# optimization
frontier <- portfolioFrontier(return.ts, spec, constraints)
print(frontier)

# plotting
tailoredFrontierPlot(frontier)
weightsPlot(frontier) 
weightsPlot(frontier, col=rainbow(ncol(return.ts))) 

# adding and changing constraints
portfolioConstraints(return.ts, spec, constraints)
frontier <- portfolioFrontier(return.ts, spec, constraints)
weightsPlot(frontier) 


frontierPlot(object=frontier,type='l') 
minvariancePoints(object=frontier, return ="mean", 
                  risk = "CVaR",col="#E6F598",pch=17)

cmlPoints(object=frontier, return = "mean", 
          risk =  "CVaR",col="#99D594",pch=15)


sharpeRatioLines(object=frontier, return ="mean", 
                 risk =  "CVaR")


equalWeightsPoints(object=frontier, return = "mean", 
                   risk =  "CVaR",col="#3288BD",pch=18)

tangencyPoints(object=frontier, return = "mean", 
               risk =  "Sigma")

frontierPlot(object=frontier,type='l') 

tangencyLines(object=frontier,col="#3288BD")


monteCarloPoints(object=frontier,mcSteps = 5000, return = "mean", pch = 19, col="#D53E4F" )

library(lpSolve)

names(returns.data)  #to see the contents of the data set

##Compute the sample mean of the returns of each stock 
## and the sample covariance matrix.



covData <- covEstimator(as.timeSeries(returns.data)) ; covData



# convert time series format
return.ts <- as.timeSeries(returns.data)


## Compute the unrestricted (long-short) Mean-Variance (MV) portfolio
shortSpec <- portfolioSpec()
setSolver(shortSpec) <- "solveRshortExact"
shortFrontier <- portfolioFrontier(return.ts,spec=shortSpec,
                                   constraints="Short")
print(shortFrontier) #report results for portfolio:1,13,25,37,50

##Plot the Efficient Frontier
Frontier <- shortFrontier  
frontierPlot(Frontier,frontier="both",risk="Sigma",type="l")

## Plot some portfolios 
minvariancePoints(Frontier,pch=19,col="red") #the MVP point
##Position of each asset in the sigma-mu plane
singleAssetPoints(Frontier,risk="Sigma",pch=19,cex=1.5,
                  col=topo.colors(6))

## To compute the minimum variance portfolio (MVP), 
## and a particular efficient portfolio for a given target return
##MVP: the minimum Variance. 
minvariancePortfolio(return.ts)
##EP(mu): an efficient portfolio for given target return
mu = 0.05; Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
setTargetReturn(Spec) = mu 
efficientPortfolio(Data, Spec) 

##To compute the global maximum return portfolio we use the 
##linear programming solver lp to resolve the optimization
## problem for a vector  of 5 unknown weights.
##MaxR: Global maximum return portfolio 
## maximize: (w1,w2,w3,w4,w5)*covData$mu
## subject to: w1+w2+w3+w4+w5 = 1
##Use the linear programming solver lp from lpSolve:
f.obj <- covData$mu
f.con <- matrix(c(1,1,1,1,1), nrow=1, byrow=TRUE)
f.dir <- "="
f.rhs <- 1
lp ("max", f.obj, f.con, f.dir, f.rhs)
lp ("max", f.obj, f.con, f.dir, f.rhs)$solution

##Result: a portfolio containing only FCEL

## A plot of all 50 portfolios along the Efficient Frontier for 
## the given assets. The bold vertical line marks the MVP

weightsPlot(Frontier)


tailoredFrontierPlot(object=shortFrontier)







# chart.RollingPerformance

