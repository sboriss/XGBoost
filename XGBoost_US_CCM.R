
###########################################################################################################
#
# Apply XGBoost to data from CCM
#
###########################################################################################################

# Add-on: 


cat("\014")  # clear console
rm(list=ls(all=TRUE))


library(data.table)
# library(dplyr)
# library(tidyr)
#library(xgboost)
library(zoo)
library(lubridate) 
library(tfplot)
library(gdata)

G_OPT <- list()
G_OPT$setTransformation <- "CCM" #  "CCM": the same transformations as in CCM; "BBB" : my transformation


path <- list()
path$main          <- "c:/BBB/PROPACAD/US_CCM_XGBoost/"
# path$main          <- "Z:/NotesKOF/PAPERS/US-CCM-DATA-ACTL/"
path$code          <- paste0(path$main,"CODE/XGBoost/")
path$dataCCM_RTGDP <- paste0(path$main,"DATA/CCM/RTGDP/")
path$vint          <- paste0(path$main,"DATA/CCM/M2Q-",G_OPT$setTransformation,"/") #  vintages converted from monthly to quarterly frequency
path$resu          <- paste0(path$main,"RESU/")
path$plot          <- paste0(path$main,"PLOT/")

opt                <- list()
opt$list_fo        <- c("FO1","FO2","FO3","FO4") #
opt$list_trgt      <- seq( as.Date("1985-03-01"), as.Date("2011-09-01"), by = "3 month")

opt$DB_RTV_GDP     <- "realtimeGDP.allmonths"
opt$sy             <- "GDP" # "GDP"

### load 2nd revision to GDP
db_rt_gdp <- read.csv( paste0(path$dataCCM_RTGDP, opt$DB_RTV_GDP,".csv"), na.strings = "#N/A", stringsAsFactors = F )

head(db_rt_gdp)

tsdb_gdp <- ts( db_rt_gdp[,-1], start = c(1947,1), freq = 4)

tsy2ndRev <- ts(NA, start = c( year(       opt$list_trgt[1]       ), quarter(       opt$list_trgt[1]     ) ), 
                end = c( year( tail( opt$list_trgt, n = 1 ) ), quarter( tail( opt$list_trgt, n = 1) ) ), freq = 4)
tsy2ndRevDLN <- tsy2ndRev

window( tsy2ndRev, start = c( 1985, 1), end = c( 1985, 1 ) ) = 1

### collect actual values (after 2nd revision):
### released in 4 months after the end of the quarter
for( d in seq_along( opt$list_trgt ) ){} #   d = 30

grw_ann = sapply( seq_along( opt$list_trgt), function(d){
  
  dateTrgt   <- opt$list_trgt[d]; dateTrgt
  date2ndRev <- dateTrgt + months(4) # second revision
  
  sy <- gsub("m0","m", paste0( "gdp",format(as.Date(date2ndRev), "%ym%m") ) )
  tsy <- tsdb_gdp[,sy]
  
  tsy_qoq_ann <- 100 * ( ( tsy/stats::lag(tsy, k=-1  ) )**4  ) - 100
  tsy_dln_ann <- 400 * diff( log( tsy ), k = 1 )
  
  year_qrtr = c( year( dateTrgt ), quarter( dateTrgt ) ) 
  #print( year_qrtr)
  
  #print( tsy2ndRev )
  
  qoq_ann = window( tsy_qoq_ann, start = year_qrtr, end = year_qrtr )
  dln_ann = window( tsy_dln_ann, start = year_qrtr, end = year_qrtr )
 
  data.frame( qoq_ann = qoq_ann, dln_ann = dln_ann)
  #   window( tsy_qoq_ann, start = c( year( dateTrgt ), quarter( dateTrgt ) ), end = c( year( dateTrgt ), quarter( dateTrgt ) ) )  
})  

tsy2ndRev = unlist( t( grw_ann )[,'qoq_ann'] )
  
  # window( tsy2ndRev, start = c( year( dateTrgt ), quarter( dateTrgt ) ), end = c( year( dateTrgt ), quarter( dateTrgt ) ) ) = 
  #   window( tsy_qoq_ann, start = c( year( dateTrgt ), quarter( dateTrgt ) ), end = c( year( dateTrgt ), quarter( dateTrgt ) ) )  
  
  # window( tsy2ndRevDLN, start = c( year( dateTrgt ), quarter( dateTrgt ) ), end = c( year( dateTrgt ), quarter( dateTrgt ) ) ) = 
  #   window( tsy_dln_ann, start = c( year( dateTrgt ), quarter( dateTrgt ) ), end = c( year( dateTrgt ), quarter( dateTrgt ) ) )  


plot(tsy2ndRev, col = 1, lwd = 2)
lines(tsy2ndRevDLN, col = 2, lwd = 2)

