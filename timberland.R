library("Rblpapi")
library("xts")
library(vars)

# ticker names, input value privary
# NTI Timberland
# NTI National Council of Real Estate Investment Fiduciaries Property
# FTSE NAREIT All Equity REITS Total Return Index is a free float adjusted market capitalization weighted index that includes all tax qualified REITs listed in the NYSE, AMEX, and NASDAQ National Market.

# timberland companies
# 1. Pope Resources 
# 2. CatchMark
# 3. Potlatch
# 4. Rayonier
# 5. Weyerhaeuser
# 6. WEST FRASER TIMBER CO LTD

# extracted data function
data_extract = function(ticker){
  blpConnect()
  opt <- c("nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
           "nonTradingDayFillMethod"="PREVIOUS_VALUE",
           "periodicitySelection" = "QUARTERLY"
           #"currency"="CAD"
           )  
  dat = bdh(securities=ticker, fields=c("PX_LAST"),start.date=as.Date("1980-01-01"),options=opt)
  colnames(dat) <- c("Date",ticker)
  #blpDisconnect()
  dat = as.data.frame(dat)
  return(dat)
}
NTI = data_extract("TMBERLND Index")
NTI[,2] = NTI[,2]/100
NPI = data_extract("NPPITR Index")
NPI[,2] = NPI[,2]/100
Pope_data = data_extract("POPE US Equity")
CatchMark = data_extract("CTT US Equity")
Potlatch = data_extract("PCH US Equity")
Rayonier = data_extract("RYN US Equity")
Weyerhaeuser= data_extract("WY US Equity")
WEST= data_extract("WFT CN Equity")
SP500 = data_extract("SPX Index")
RU2000 = data_extract("RTY Index")
#Bloomberg Barclays US Agg Gov/Credit Total Return Value Unhedged USD
T_bonds = data_extract("LUGCTRUU Index")

###############################3 month Treasury bills#######################################
require(devtools)
require(DatastreamDSWS2R)
# extracted data function

options(Datastream.Username = "ZALB003")
options(Datastream.Password = "YOUNG607")
mydsws <- dsws$new()
tbill3mca <- mydsws$timeSeriesRequest(instrument = "SCMM91D",
                                    datatype = "RY",
                                    startDate = "-40Y",
                                    endDate = "-0D",
                                    frequency = "Q")
tbill3mca <- as.data.frame(tbill3mca/100)

############################################################################################
# we only use monthly data, so extract year and month from data
fun = function(d,f) format(as.Date(d,f), "%Y-%m")
NTI$Date=fun(NTI$Date,"%Y-%m-%d")
NPI$Date=fun(NPI$Date,"%Y-%m-%d")

Pope_data$Date=fun(Pope_data$Date,"%Y-%m-%d")
CatchMark$Date=fun(CatchMark$Date,"%Y-%m-%d")
Potlatch$Date=fun(Potlatch$Date,"%Y-%m-%d")
Rayonier$Date=fun(Rayonier$Date,"%Y-%m-%d")
Weyerhaeuser$Date=fun(Weyerhaeuser$Date,"%Y-%m-%d")
WEST$Date=fun(WEST$Date,"%Y-%m-%d")

SP500$Date=fun(SP500$Date,"%Y-%m-%d")
RU2000$Date = fun(RU2000$Date,"%Y-%m-%d")
T_bonds$Date = fun(T_bonds$Date,"%Y-%m-%d")
row.names(tbill3mca) = fun(row.names(tbill3mca) ,"%Y-%m-%d")  

#combination of all public data

all = merge(Pope_data,CatchMark,by="Date",all = TRUE)
all = merge(all,Potlatch,by="Date",all = TRUE)
all = merge(all,Rayonier,by="Date",all = TRUE)
all = merge(all,Weyerhaeuser,by="Date",all = TRUE)
all = merge(all,WEST,by="Date",all = TRUE)
all = merge(all,SP500[-c(1:2),],by="Date",all = TRUE)
all = merge(all,RU2000,by="Date",all = TRUE)
all = merge(all,T_bonds, by="Date", all = TRUE)
row.names(all) = all$Date
all$Date=NULL
#plot.ts(all)
pub_return = apply(all,2,function(x) diff(x)/head(x,-1))
Tbill = tbill3mca[row.names(tbill3mca)>=NTI$Date[1]&row.names(tbill3mca)<=NTI$Date[nrow(NTI)],]/100
NCREIF = merge(NTI, NPI, by="Date")
all_return = cbind(NCREIF,pub_return[row.names(pub_return)>=NTI$Date[1],],Tbill)
row.names(all_return) = all_return$Date
all_return$Date=NULL
# replace NA with 0
all_return[is.na(all_return)] <- 0
timberpub_n = 6
pub_w = na.omit(as.matrix(all_return[,c(3:8)])%*%rep(1/timberpub_n,timberpub_n))
colnames(pub_w)= c("REIT")
all_return2 = cbind(all_return[,-c(3:8)], pub_w)

#convert to time series
allts = ts(all_return2, frequency = 4, start = c(1987, 1))
colnames(allts) <- c("NTI","NPI", "SP500", "RU2000","T-bonds10Y","T-bills3M","REIT")
allxts <- as.xts(allts)

save.image(file = "timberland.RData")
