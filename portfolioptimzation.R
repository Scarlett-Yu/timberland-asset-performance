library(RColorBrewer)
library(fPortfolio)

pacman::p_load(matrixcalc,knitr,dygraphs,ggthemes,highcharter,viridis,tibbletime,timetk,tidyquant,tidyverse,fPortfolio,xts)
#0. Prepare data
returns.data <- na.omit(allts)
returns.data %>%tk_tbl()%>%head()
returns_ts <- as.timeSeries(returns.data)
spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
setNFrontierPoints(spec) <-25
#1. using equal weights in this case
nAssets <- ncol(returns_ts)
setWeights(spec)<-rep(1/nAssets, times = nAssets) 
constraints <- 'LongOnly'
portfolioConstraints(returns_ts, spec, constraints)

# calculate the properties of the portfolio
# Now let us display the results from the equal weights portfolio, the assignment of weights, and the attribution of returns and risk.
ewPortfolio <- feasiblePortfolio( returns_ts, spec, constraints)
print(ewPortfolio)
col <- divPalette(ncol(returns_ts), "RdBu")
weightsPie(ewPortfolio, radius = 0.7, col = col)
mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(ewPortfolio, radius = 0.7, col = col)
mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(ewPortfolio, radius = 0.7, col = col)
mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)

# COMPUTE A MINIMUM RISK EFFICIENT PORTFOLIO!
minriskSpec <- portfolioSpec()
targetReturn <- getTargetReturn(ewPortfolio@portfolio)["mean"]
setTargetReturn(minriskSpec) <- targetReturn
minriskPortfolio <- efficientPortfolio(
  data = returns_ts,
  spec = minriskSpec,
  constraints = "LongOnly")
print(minriskPortfolio)

col <- qualiPalette(ncol(returns_ts), "Dark2")
weightsPie(minriskPortfolio, radius = 0.7, col = col)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(minriskPortfolio, radius = 0.7, col = col)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(minriskPortfolio, radius = 0.7, col = col)
mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
        font = 2, cex = 0.7, adj = 0)

globminSpec <- portfolioSpec()
globminPortfolio <- minvariancePortfolio(
  data = returns_ts,
  spec = globminSpec,
  constraints = "LongOnly")
print(globminPortfolio)
col <- seqPalette(ncol(returns_ts), "YlGn")
weightsPie(globminPortfolio, box = FALSE, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
        line = 1.5, font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(globminPortfolio, box = FALSE, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
        line = 1.5, font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(globminPortfolio, box = FALSE, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
        line = 1.5, font = 2, cex = 0.7, adj = 0)

tgSpec <- portfolioSpec()
setRiskFreeRate(tgSpec) <- 0
tgPortfolio <- tangencyPortfolio(
  data = returns_ts,
  spec = tgSpec,
  constraints = "LongOnly")
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
#Now we are ready to start the optmization and pass data, specification and constraints to the function portfolioFrontier(). We can examine the result using a standard print() command.

# compute the efficient frontier
setNFrontierPoints(spec) <-25
frontier <- portfolioFrontier(returns_ts, spec, constraints)
print(frontier)

# plot efficient frontier
tailoredFrontierPlot(object = frontier, mText = "MV Portfolio - LongOnly Constraints",
                     risk = "Cov")

# plot weights
weightsPlot(frontier, col=rainbow(dim(returns.data)[2]))
weightsPlot(frontier)
text <- "Mean-Variance Portfolio - Long Only Constraints"
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



#COMPUTE A FEASIBLE MEAN-CVAR PORTFOLIO
cvarSpec <- portfolioSpec()
setType(cvarSpec) <- "CVAR"
nAssets <- ncol(returns_ts)
setWeights(cvarSpec) <- rep(1/nAssets, times = nAssets)
setSolver(cvarSpec) <- "solveRglpk.CVAR"
ewPortfolio <- feasiblePortfolio(
  data = returns_ts,
  spec = cvarSpec,
  constraints = "LongOnly")
print(ewPortfolio)





























# extended constraints: add upper investment limits
constraints <- c('minW[1:5]=0', 'maxW[1:5]=0.5')
portfolioConstraints(returns_ts, spec, constraints)

frontier <- portfolioFrontier(returns_ts, spec, constraints)



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
