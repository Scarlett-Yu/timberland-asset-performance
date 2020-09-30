library("Rblpapi")
library("xts")
library(vars)
library(PerformanceAnalytics)
require(devtools)
require(DatastreamDSWS2R)
# ticker names, input value privary
# NTI Timberland
# NTI National Council of Real Estate Investment Fiduciaries Property
# FTSE NAREIT All Equity REITS Total Return Index is a free float adjusted market capitalization weighted index that includes all tax qualified REITs listed in the NYSE, AMEX, and NASDAQ National Market.

# timberland companies
# 1. Weyerhaeuser*
# 2. CatchMark*
# 3. Potlatch*
# 4. Rayonier*
# 
#FNRE Index ##FTSE Nareit Equity REITS Index
# extracted data function
opt1 <- c("nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
         #"nonTradingDayFillMethod" ="NIL_VALUE",
         #"periodicitySelection" = "QUARTERLY",
         "currency"="CAD")
data_extract = function(ticker,opt = opt1){
  blpConnect()
  dat = bdh(securities=ticker, fields=c("PX_LAST"),start.date=as.Date("1980-01-01"),options=opt)
  dat = lapply(dat, function(d) xts(d[,-1],order.by = as.Date(d[,1])))
  dat = do.call(merge, dat)
  colnames(dat) <- ticker
  return(dat)
}

NCREIF = data_extract(c("TMBERLND Index","NPPITR Index"))/100
REITs = data_extract(c("CTT US Equity","PCH US Equity","RYN US Equity","WY US Equity"))


Stock_market = data_extract(c("SPX Index","RTY Index"))
Stock_market = Return.calculate(Stock_market)
Stock_market.d = na.omit(Stock_market)

opt2 <- c("nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
         #"nonTradingDayFillMethod" ="NIL_VALUE",
         "periodicitySelection" = "QUARTERLY",
         "currency"="CAD")
REITs.q = data_extract(c("CTT US Equity","PCH US Equity","RYN US Equity","WY US Equity"),opt2)
REITs.q = Return.calculate(REITs.q)
REITs.q = xts(rowMeans(REITs.q,na.rm = T) , index(REITs.q))
REITs.q = na.omit(REITs.q)
Stock_market.q = data_extract(c("SPX Index","RTY Index","LUGCTRUU Index","FNRE Index"),opt2)
Stock_market.q = Return.calculate(Stock_market.q)
Stock_market.q = na.omit(Stock_market.q)
blpConnect()
opt3 <- c("nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
          #"nonTradingDayFillMethod" ="NIL_VALUE",
          "periodicitySelection" = "MONTHLY",
          "currency"="CAD")
nandt = data_extract(c("FNRE Index", "LUGCTRUU Index"),opt3)
nandt = Return.calculate(nandt)
nareit.m = na.omit(nandt[,1])
tbonds.m = na.omit(nandt[,2])
reits.m = data_extract(c("CTT US Equity","PCH US Equity","RYN US Equity","WY US Equity"),opt3)
reits.m = Return.calculate(reits.m)
reits.m = xts(rowMeans(reits.m,na.rm = T) , index(reits.m))
reits.m = na.omit(reits.m)
index(reits.m) = as.yearmon(index(reits.m))
Stock_market.m = data_extract(c("SPX Index","RTY Index"),opt3)
Stock_market.m = Return.calculate(Stock_market.m)
Stock_market.m = na.omit(Stock_market.m)
index(Stock_market.m) = as.yearmon(index(Stock_market.m))
opt4 <- c("nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
          #"nonTradingDayFillMethod" ="NIL_VALUE",
          "periodicitySelection" = "DAILY",
          "currency"="CAD")

nareit.d = bdh(securities="FNRE Index", fields=c("PX_LAST"),start.date=as.Date("1999-01-01"),options=opt4)
nareit.d = xts(nareit.d$PX_LAST, order.by = nareit.d$date)
nareit.d = na.omit(Return.calculate(nareit.d))
tbonds.d = bdh(securities="LUGCTRUU Index", fields=c("PX_LAST"),start.date=as.Date("1989-01-01"),options=opt4)
tbonds.d = xts(tbonds.d$PX_LAST, order.by = tbonds.d$date)
tbonds.d = na.omit(Return.calculate(tbonds.d))
#NHSPSTOT Index:US New Privately Owned Housing Units Started by Structure Total SAAR »
housing = bdh(securities="NHSPSTOT Index", fields=c("PX_LAST"),start.date=as.Date("1986-01-01"),options=opt2)
housing = xts(housing$PX_LAST, order.by = housing$date)
housing = na.omit(Return.calculate(housing))
index(housing) = as.yearqtr(index(housing))
colnames(housing) = 'housing'
#CPI
CPI = bdh(securities="CPI YOY Index", fields=c("PX_LAST"),start.date=as.Date("1986-01-01"),options=opt2)

#US Producer Price Index Lumber & Plywood                                        
fdata = data_extract(c("NHSPSTOT Index", "CPI YOY Index", "PPIRLUP Index"),opt3)
index(fdata) = as.yearmon(index(fdata))
fdata[,3] = Return.calculate(fdata[,3])
fdata = cbind(fdata, reits.m, Stock_market.m)


fdataq = data_extract(c("NHSPSTOT Index", "CPI YOY Index", "PPIRLUP Index"),opt2)
index(fdataq) = as.yearqtr(index(fdataq))
fdataq[,3] = Return.calculate(fdataq[,3])

###############################3 month Treasury bills#######################################
options(Datastream.Username = "ZALB003")
options(Datastream.Password = "YOUNG607")
mydsws <- dsws$new()
tbill3mca.q <- mydsws$timeSeriesRequest(instrument = "SCMM91D",
                                    datatype = "RY",
                                    startDate = "-40Y",
                                    endDate = "-0D",
                                    frequency = "Q")
tbill3mca.q <- xts(tbill3mca.q/100, order.by = index(tbill3mca.q))
tbill3mca.m <- mydsws$timeSeriesRequest(instrument = "SCMM91D",
                                        datatype = "RY",
                                        startDate = "-40Y",
                                        endDate = "-0D",
                                        frequency = "M")
tbill3mca.m <- xts(tbill3mca.m/100, order.by = index(tbill3mca.m))

############################################################################################
# we only use quarterly data, so extract year and qtr from data
fun = function(d) as.yearqtr(index(d), format = "%Y-%m-%d")
#combination of all quarterly data
index(NCREIF)=fun(NCREIF)
index(REITs.q)=fun(REITs.q)
index(Stock_market.q) = fun(Stock_market.q)
index(tbill3mca.q) = fun(tbill3mca.q)

all.qtr = cbind(NCREIF,Stock_market.q ,REITs.q,  tbill3mca.q)
colnames(all.qtr) <- c("NTI","NPI", "SP500", "RU2000","T-bonds10Y","NAREIT","Timber REITs","T-bills3M")
allxts = na.omit(all.qtr)
fdataq = cbind(fdataq, all.qtr)

save.image(file = "timberland.RData")
