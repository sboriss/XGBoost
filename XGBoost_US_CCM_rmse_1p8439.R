
###########################################################################################################
#
# Apply XGBoost to data from CCM
#
# rmse = sqrt( mean( tsErro^2 ) ); rmse
# 1.843965
###########################################################################################################

# Add-on: 


cat("\014")  # clear console
rm(list=ls(all=TRUE))

set.seed( 17 )

library(data.table)
library(dplyr)
library(tidyr)
library(xgboost)
library(zoo)
library(lubridate) 
library(tfplot)
library(gdata)

G_OPT <- list()
G_OPT$setTransformation <- "CCM" #  "CCM": the same transformations as in CCM; "BBB" : my transformation
G_OPT$setModel          <- "XGBoost"
G_OPT$setSV             <- "OFF"
G_OPT$is_LRG            <- TRUE
G_OPT$is_SML            <- 1 - G_OPT$is_LRG

G_OPT$setValidSample    <- 0.3
G_OPT$setMaxDepth       <- 1               

G_OPT$is_AR0            <- FALSE
G_OPT$is_AR2            <- FALSE

if( length( grep("AR0" , G_OPT$setModel) ) > 0){ G_OPT$is_AR0   <- TRUE }
if( length( grep("AR2" , G_OPT$setModel) ) > 0){ G_OPT$is_AR2   <- TRUE }

if(G_OPT$is_LRG ){
  asx     = c("GDP_L1","ISM_m3"   ,"ISM_m2"   ,"ISM_m1"   ,"EMPLOY_m3","EMPLOY_m2","EMPLOY_m1",
              "SUPDEL_m3","SUPDEL_m2","SUPDEL_m1","ORDERS_m3","ORDERS_m2","ORDERS_m1",
              "HOURS_m3" ,"HOURS_m2" ,"HOURS_m1" ,"SP500_m3" ,"SP500_m2" ,"SP500_m1" ,
              "TBILL_m3" ,"TBILL_m2" ,"TBILL_m1" ,"TBOND_m3" ,"TBOND_m2" ,"TBOND_m1" ,
              "CLAIMS_m2","CLAIMS_m1",            "RSALES_m2","RSALES_m1",
              "IP_m2"    ,"IP_m1"    ,"STARTS_m2","STARTS_m1")
} 
if(G_OPT$is_SML){
  asx     = c("GDP_L1","ISM_m3"   ,"ISM_m2"      ,"ISM_m1",
              "EMPLOY_m3","EMPLOY_m2"   ,"EMPLOY_m1",
              "RSALES_m2"   ,"RSALES_m1",
              "IP_m2"       ,"IP_m1",
              "STARTS_m2"   ,"STARTS_m1")
}


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

### collect actual values (after 2nd revision):
### released in 4 months after the end of the quarter
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
 
  c( qoq_ann = c(qoq_ann), dln_ann = c(dln_ann) )
  #   window( tsy_qoq_ann, start = c( year( dateTrgt ), quarter( dateTrgt ) ), end = c( year( dateTrgt ), quarter( dateTrgt ) ) )  
})  

ts_grw_ann = ts( t( grw_ann ), start = c( year( opt$list_trgt[1] ), quarter( opt$list_trgt[1] ) ), frequency = 4)

tsy2ndRev    = ts_grw_ann[,'qoq_ann']
tsy2ndRevDLN = ts_grw_ann[,'dln_ann']

  
plot(tsy2ndRev, col = 1, lwd = 2)
lines(tsy2ndRevDLN, col = 2, lwd = 2)


### BEG ### LOOP OVER FORECAST ORIGINS
lapply( opt$list_fo, function(j_fo){ })#  

j_fo    <- "FO4"

sfile_ud <- paste0( "_MODEL-",G_OPT$setModel,"_",G_OPT$setSV,"_",j_fo ); sfile_ud

### BEG ### LOOP OVER VINTAGES FOR A GIVEN FORECAST ORIGIN
listINFO_FO <- lapply( opt$list_trgt, function(i_trgt){ #  #  ("2002-03-01") "1985-03-01" i_trgt  <- as.Date("2011-09-01")
  
  cat( "\ni_trgt: ", as.character( i_trgt ) )
  date <- list()
  date$trgt    <- i_trgt
  date$estnBeg <- as.Date( "1970-03-01" )
  if( j_fo == "FO1" ) date$estnEnd <- date$trgt %m-% months(6)
  if( j_fo != "FO1" ) date$estnEnd <- date$trgt %m-% months(3) 
  if( j_fo == "FO1" ) date$fcstOrg <- date$trgt %m-% months(2) 
  if( j_fo == "FO2" ) date$fcstOrg <- date$trgt %m-% months(1) 
  if( j_fo == "FO3" ) date$fcstOrg <- date$trgt 
  if( j_fo == "FO4" ) date$fcstOrg <- date$trgt %m+% months(1) 
  
  sfile_ud_vint <- paste0( sfile_ud,"_TQ-",date$trgt,"_",j_fo,"-",date$fcstOrg ); sfile_ud_vint
  
  sdb <- paste0( "tsyx_", date$fcstOrg, ".csv" ); sdb
  
  db <- read.csv( paste0( path$vint, sdb ), stringsAsFactors = F, na = "#N/A" ); head( db )
  
  ###substitute NA in IP in 1970Q1 with zero values
  db[ 1, which( is.na( head( db, n = 1 ) ) ) ] <- 0; head( db )
  
  tail( db )
  
  tsdb <- ts( db, start = c( db[1,"year"], db[1,"period"] ), frequency = 4 )
  colnames( tsdb )
  
  tsy <- window( tsdb[, opt$sy ], start = c( year(date$estnBeg), quarter(date$estnBeg) ), 
                 end = c( year(date$estnEnd), quarter(date$estnEnd) ) )
  
  if( j_fo == "FO1" ){ 
    date$estnBeg = date$estnBeg %m+% months(1); 
    tsdb = lag( tsdb, k = -1 )
  }
  
  ### check availability of X in tsdb
  asxIN <- names( unlist( sapply( asx, function(sx) grep( sx, colnames(tsdb) ) ) ) )
  asxIN
  
  if( !G_OPT$is_AR0 ){
    
    tsx <- cbind( 1, window( tsdb[, asxIN ], start = c( year(date$estnBeg), quarter(date$estnBeg) ), 
                             end = c( year(date$estnEnd), quarter(date$estnEnd) ) ) )
    tsx_fcst <- cbind( 1, window( tsdb[, asxIN ], start = c( year(date$trgt), quarter(date$trgt) ), 
                                  end = c( year(date$trgt), quarter(date$trgt) ) ) )
    colnames( tsx ) <- colnames( tsx_fcst ) <- c( "incpt", asxIN )
  }else{
    tsx <- ts( 1, start = c( year(date$estnBeg), quarter(date$estnBeg) ), 
               end = c( year(date$estnEnd), quarter(date$estnEnd) ), frequency = 4 )
    tsx_fcst <- ts( 1, start = c( year(date$trgt), quarter(date$trgt) ), 
                    end = c( year(date$trgt), quarter(date$trgt) ), frequency = 4 )
    tsx      <- as.matrix( tsx     , ncol = 1 )
    tsx_fcst <- as.matrix( tsx_fcst, ncol = 1 )
    colnames( tsx ) <- colnames( tsx_fcst ) <- c( "incpt" )
  }
  #plot( tsy )
  
  if( j_fo == "FO1" ) tsy = window( tsy, start = c( year(date$estnBeg), quarter(date$estnBeg) ), 
                                    end = end(tsy) )
  
  ols = lm( tsy ~ tsx - 1  ); summary( ols )
  pred_ols = ts( ols$fitted.values, start = start( tsy ), frequency = 4 )
  
  ### prepare data for XGBoost
  
  mdata_x = data.frame( tsx ); colnames( mdata_x ) = colnames( tsx )
  #remove intercept
  mdata_x = mdata_x %>% select( -incpt )
  
  vdata_y = data.frame( tsy );
  
  nobs = dim( mdata_x )[1]
  
  indx_vld = sort( sample( seq( nobs ), round( G_OPT$setValidSample * nobs  ) ) )
  
  train <- list()
  train$label = vdata_y %>% mutate( indx = seq( nobs ) ) %>% filter( !(indx %in% indx_vld ) ) %>% select(  -indx ) 
  train$data  = mdata_x %>% mutate( indx = seq( nobs ) ) %>% filter( !(indx %in% indx_vld ) ) %>% select(  -indx )
  
  valid <- list()
  valid$label = vdata_y %>% mutate( indx = seq( nobs ) ) %>% filter( indx %in% indx_vld ) %>% select(  -indx ) 
  valid$data  = mdata_x %>% mutate( indx = seq( nobs ) ) %>% filter( indx %in% indx_vld ) %>% select(  -indx )
  
  train_valid = mdata_x 
  
  test <- data.frame( tsx_fcst ) %>% select( -incpt )
  
  dtrain       = xgb.DMatrix( data = as.matrix( train$data ), label = as.matrix( train$label ) )
  dvalid       = xgb.DMatrix( data = as.matrix( valid$data ), label = as.matrix( valid$label ) )
  dtrain_valid = xgb.DMatrix( data = as.matrix( train_valid ) )
  dtest        = xgb.DMatrix( data = as.matrix( test ) )
  
  # Set xgboost parameters
  param <- list("objective" = "reg:linear",
                booster = "gbtree",  #gblinear 
                "eta" = 0.05,
                "min_child_weight" = 10,
                "subsample" = .8,
                "colsample_bytree" = .8,
                "scale_pos_weight" = 1.0,
                "max_depth" = G_OPT$setMaxDepth
                # ,"lambda" = 50
  )
  
  # setup watchlist to enable train and validation, validation must be first for early stopping
  watchlist <- list( train=dtrain,val=dvalid)
  # to train with watchlist, use xgb.train, which contains more advanced features
  
  # this will use default evaluation metric = rmse which we want to minimise
  xgb_train = xgb.train(params = param, data = dtrain, nround=1000, print_every_n = 20, watchlist=watchlist, early_stopping_rounds = 50, maximize = FALSE)
  
  colnames( mdata_x )
  
  importance_matrix = xgb.importance( colnames( mdata_x ), model = xgb_train )
  print(importance_matrix)
  xgb.ggplot.importance(importance_matrix)
  
  #check in-sample fit
  pred_xgb_insample = ts( predict( xgb_train, dtrain_valid ), start = start( tsx ), frequency = 4 )
  
  ### outturn of 2nd revision of GDP
  dateTrgtYear <- year( date$trgt )
  dateTrgtQrtr <- quarter( date$trgt )
  tsyActl = window( tsy2ndRev, start = c(dateTrgtYear, dateTrgtQrtr), end = c(dateTrgtYear, dateTrgtQrtr) ); tsyActl
  
  #out-of-sample forecast
  tsyFcst = ts( predict( xgb_train, dtest ), start = start( tsyActl ), frequency = 4 )

  sqrt( mean( (tsy - pred_xgb_insample)^2 ) )
  sqrt( mean( (tsy - pred_ols    )^2 ) )
  
  plot( tsy        , lwd = 2)
  lines( pred_xgb_insample, col = 'red'  , lwd = 2)
  lines( pred_ols         , col = 'green', lwd = 2)
  
  gain = importance_matrix$Gain; names( gain ) = importance_matrix$Feature; gain
  
  #collect results
  list( actl = tsyActl, fcst = tsyFcst, erro = c(tsyActl) - c(tsyFcst), gain = gain )
  
}) 

tsActl = ts( sapply( listINFO_FO, function( x ) x[["actl"]] ), start = c( year(opt$list_trgt[1]), quarter( opt$list_trgt[1]) ), frequency = 4)
tsFcst = ts( sapply( listINFO_FO, function( x ) x[["fcst"]] ), start = c( year(opt$list_trgt[1]), quarter( opt$list_trgt[1]) ), frequency = 4)
tsErro = ts( sapply( listINFO_FO, function( x ) x[["erro"]] ), start = c( year(opt$list_trgt[1]), quarter( opt$list_trgt[1]) ), frequency = 4)

plot( tsActl, lwd = 2 )
lines( tsFcst, col = 'red', lwd = 2)

rmse = sqrt( mean( tsErro^2 ) ); rmse 
