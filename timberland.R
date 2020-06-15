library("Rblpapi")
library("xts")
library(vars)

# ticker names, input value privary
# NCREIF Timberland
# NCREIF National Council of Real Estate Investment Fiduciaries Property
# FTSE NAREIT All Equity REITS Total Return Index is a free float adjusted market capitalization weighted index that includes all tax qualified REITs listed in the NYSE, AMEX, and NASDAQ National Market.
# LCA3TRUU Index The Bloomberg Barclays Aaa Corporate Index measures the Aaa-rated, fixed-rate, taxable corporate bond market. It includes USD denominated securities publicly issued by US and non-US industrial, utility and financial issuers. (Future Ticker: I08218US)
timber = c("TMBERLND Index","NPNCRE Index","FNERTR Index", "MOODCAAA Index")
# timberland companies
# 1. Pope Resources 
# 2. Cambium Global Timberland Ltd
# 3. Potlatch
# 4. Rayonier
# 5. Weyerhaeuser
# 6. WEST FRASER TIMBER CO LTD
company_index = c("POPE US Equity","TREE LN Equity","PCH US Equity","RYN US Equity","WY US Equity","WFT CN Equity")

# extracted data function
data_extract = function(ticker){
  blpConnect()
  opt <- c("nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
           "nonTradingDayFillMethod"="PREVIOUS_VALUE",
           "periodicitySelection" = "QUARTERLY",
           "currency"="CAD")  
  dat = bdh(securities=ticker, fields=c("PX_LAST"),start.date=as.Date("1980-01-01"),options=opt)
  colnames(dat) <- c("Date",ticker)
  #blpDisconnect()
  dat = as.data.frame(dat)
  return(dat)
}
NPI = data_extract("NPNCRE Index")
NCREIF = data_extract("TMBERLND Index")
NCREIF[,2] = NCREIF[,2]/100
NAREIT = data_extract("FNERTR Index")
LTC = data_extract("MOODCAAA Index") # long term corporate bond

Pope_data = data_extract("POPE US Equity")
Cambium = data_extract("TREE LN Equity")
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
data_extract = function(ticker){
  blpConnect()
  StartDate=as.Date("1980-01-01")
  opt <- c("nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
           "nonTradingDayFillMethod"="PREVIOUS_VALUE")  
  dat = bdh(securities=ticker, fields=c("PX_LAST"),start.date=StartDate,options=opt)
  colnames(dat) <- c("Date",ticker)
  #blpDisconnect()
  dat = as.data.frame(dat)
  return(dat)
}

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
NCREIF$Date=fun(NCREIF$Date,"%Y-%m-%d")
Pope_data$Date=fun(Pope_data$Date,"%Y-%m-%d")
Cambium$Date=fun(Cambium$Date,"%Y-%m-%d")
Potlatch$Date=fun(Potlatch$Date,"%Y-%m-%d")
Rayonier$Date=fun(Rayonier$Date,"%Y-%m-%d")
Weyerhaeuser$Date=fun(Weyerhaeuser$Date,"%Y-%m-%d")
WEST$Date=fun(WEST$Date,"%Y-%m-%d")
SP500$Date=fun(SP500$Date,"%Y-%m-%d")
RU2000$Date = fun(RU2000$Date,"%Y-%m-%d")
T_bonds$Date = fun(T_bonds$Date,"%Y-%m-%d")
row.names(tbill3mca) = fun(row.names(tbill3mca) ,"%Y-%m-%d")  

#combination of all public data

all = merge(Pope_data,Cambium,by="Date",all = TRUE)
all = merge(all,Potlatch,by="Date",all = TRUE)
all = merge(all,Rayonier,by="Date",all = TRUE)
all = merge(all,Weyerhaeuser,by="Date",all = TRUE)
all = merge(all,WEST,by="Date",all = TRUE)
all = merge(all,SP500[-c(1:2),],by="Date",all = TRUE)
all = merge(all,RU2000,by="Date",all = TRUE)
all = merge(all,T_bonds, by="Date", all = TRUE)
row.names(all) = all$Date
all$Date=NULL
plot.ts(NCREIF$`TMBERLND Index`) 
plot.ts(all)
pub_return = apply(all,2,function(x) diff(x)/head(x,-1))
Tbill = tbill3mca[row.names(tbill3mca)>=NCREIF$Date[1]&row.names(tbill3mca)<=NCREIF$Date[nrow(NCREIF)],]/100
all_return = cbind(NCREIF,pub_return[row.names(pub_return)>=NCREIF$Date[1],],Tbill)
row.names(all_return) = all_return$Date
all_return$Date=NULL
# replace NA with 0
all_return[is.na(all_return)] <- 0
timberpub_n = 6
pub_w = na.omit(as.matrix(all_return[,c(2:7)])%*%rep(1/timberpub_n,timberpub_n))
colnames(pub_w)= c("REIT")
all_return2 = cbind(all_return[,-c(2:7)], pub_w)

#convert to time series
allts = ts(all_return2, frequency = 4, start = c(1987, 1))
colnames(allts) <- c("NCREIF", "SP500", "RU2000","T-bonds10Y","T-bills3M","REIT")
allxts <- as.xts(allts)

save.image(file = "timberland.RData")
