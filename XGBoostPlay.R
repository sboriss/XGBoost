
###########################################################################################################
#
# play with XGBoost for time series
#
###########################################################################################################

# Add-on: 


cat("\014")  # clear console
rm(list=ls(all=TRUE))


library(data.table)
library(dplyr)
library(tidyr)
library(xgboost)

#generate an AR(1) model

set.seed( 1234 )

obs = 300
obs_vld = 30
arc = c(1.4, -0.5)
k   = 10

tsy_0 <- arima.sim(list(order = c( length(arc), 0, 0), ar = arc ), n = obs + length(arc) + obs_vld )
ts.plot(tsy_0)

tsx = ts( matrix( rnorm( length(tsy_0) * k ), ncol = k ), start = start( tsy_0 ) )

data <- na.omit( cbind( tsy_0, stats::lag( tsy_0, k = -1), stats::lag( tsy_0, k = -2 ), tsx ) )

colnames( data ) <- c( paste0( "tsy_", seq( 0, length(arc) ) ), paste0( "x_", seq(k) ) )
head( data )

ols = lm( tsy_0 ~ ., data = data ); summary( ols )
pred_ols = ts( ols$fitted.values, start = start(data) )

mdata = data.frame( data ); colnames( mdata ) = colnames( data )

indx_vld = sort( sample( seq( dim( mdata )[1] ), obs_vld ) )

train <- list()
train$label = mdata %>% select(  tsy_0 ) %>% mutate( indx = seq( dim( mdata )[1]) ) %>% filter( !(indx %in% indx_vld ) ) %>% select(  -indx ) 
train$data  = mdata %>% select( -tsy_0 ) %>% mutate( indx = seq( dim( mdata )[1]) ) %>% filter( !(indx %in% indx_vld ) ) %>% select(  -indx )

valid <- list()
valid$label = mdata %>% select(  tsy_0 ) %>% mutate( indx = seq( dim( mdata )[1]) ) %>% filter( indx %in% indx_vld ) %>% select(  -indx ) 
valid$data  = mdata %>% select( -tsy_0 ) %>% mutate( indx = seq( dim( mdata )[1]) ) %>% filter( indx %in% indx_vld ) %>% select(  -indx )

train_valid = mdata %>% select( -tsy_0 )

dtrain       = xgb.DMatrix( data = as.matrix( train$data ), label = c( train$label )$tsy_0 )
dvalid       = xgb.DMatrix( data = as.matrix( valid$data ), label = c( valid$label )$tsy_0 )
dtrain_valid = xgb.DMatrix( data = as.matrix( train_valid ) )

# Set xgboost parameters
param <- list("objective" = "reg:linear",
              booster = "gbtree",  #gblinear 
              "eta" = 0.05,
              "min_child_weight" = 10,
              "subsample" = .8,
              "colsample_bytree" = .8,
              "scale_pos_weight" = 1.0,
              "max_depth" = 7
)

# setup watchlist to enable train and validation, validation must be first for early stopping
watchlist <- list( train=dtrain,val=dvalid)
# to train with watchlist, use xgb.train, which contains more advanced features

# this will use default evaluation metric = rmse which we want to minimise
xgb_train_valid = xgb.train(params = param, data = dtrain, nround=1000, print_every_n = 20, watchlist=watchlist, early_stopping_rounds = 50, maximize = FALSE)

pred_xgb = ts( predict( xgb_train_valid, dtrain_valid ), start = start( data ) )

plot( data[,"tsy_0"]);
lines( pred_ols, col = 'blue')
lines( pred_xgb, col = 'red' );

plot( data[,"tsy_0"], pred_ols )
plot( data[,"tsy_0"], pred_xgb )
plot( pred_ols, pred_xgb )

lm_ols_xgb = lm( pred_ols ~ pred_xgb ); summary( lm_ols_xgb )
lm_ols_act = lm( data[,"tsy_0"] ~ pred_ols ); summary( lm_ols_act )

sqrt( mean( (tsy_0 - pred_xgb)^2 ) )
sqrt( mean( (tsy_0 - pred_ols)^2 ) )


bst1$best_iteration
train_valid
predict( bst1, dtrain )







dtrain = xgb.DMatrix( data = as.matrix( train$data ), label = c( train$label )$tsy_0 )
dvalid = xgb.DMatrix( data = as.matrix( valid$data ), label = c( valid$label )$tsy_0 )
dtrain_valid = xgb.DMatrix( data = as.matrix( train_valid ) )

cv_max_depth = lapply( seq( 10 ), function( max_depth ){ # cat( 'max_depth', max_depth, '\n' )
  
  cv <- xgb.cv( data = dtrain, nround = 100, nthread = 8, nfold = 5, metrics = list( "rmse" ), max_depth = max_depth,
                eta = 1, objective = "reg:linear", early_stopping_rounds = 10 )
  
  #cat( "cv$best_iteration", cv$best_iteration, '\n' )
  #cat( "cv$evaluation_log\n"); print( cv$evaluation_log, '\n' )
  
   best_iter = cv$best_iteration
   
   data.frame( max_depth = max_depth, best_iter = best_iter,
               rmse_mean = cv$evaluation_log[ best_iter, 'test_rmse_mean' ],
               rmse_std  = cv$evaluation_log[ best_iter, 'test_rmse_std'  ] )

})

mcv_max_depth = do.call( rbind.data.frame,  cv_max_depth); mcv_max_depth

plot( mcv_max_depth[, 'max_depth'], mcv_max_depth[, 'test_rmse_mean'], main = "CV by max_depth" )

max_depth_opt = which.min( mcv_max_depth$test_rmse_mean ) 

# Set xgboost parameters
param <- list("objective" = "reg:linear",
              booster = "gbtree",  #gblinear 
              "eta" = 0.05,
              "min_child_weight" = 10,
              "subsample" = .8,
              "colsample_bytree" = .8,
              "scale_pos_weight" = 1.0,
              "max_depth" = 4
)

xgModel = xgboost(data = dtrain, nround = 100, params = param, print_every_n=5 )



# Set xgboost test and training and validation datasets
xgtest <- xgb.DMatrix(data = test_x)
xgtrain <- xgb.DMatrix(data = train_x[offset:nrow(train_x),], label= train_y[offset:nrow(train_x)])
xgval <-  xgb.DMatrix(data = train_x[1:offset,], label= train_y[1:offset])

# setup watchlist to enable train and validation, validation must be first for early stopping
watchlist <- list( train=dtrain,val=dvalid)
# to train with watchlist, use xgb.train, which contains more advanced features

# this will use default evaluation metric = rmse which we want to minimise
bst1 <- xgb.train(params = param, data = dtrain, nround=1000, print_every_n = 20, watchlist=watchlist, early_stopping_rounds = 50, maximize = FALSE)

bst1$best_iteration
train_valid
predict( bst1, dtrain )

pred_xgb = ts( predict( bst1, dtrain_valid ), start = start( data ) )



importance_matrix = xgb.importance(model = xgModel)
print(importance_matrix)
xgb.ggplot.importance(importance_matrix)

pred = ts( predict( xgModel, dtrain ), start = start(data) )
plot( tsy_0 ); lines( pred_xgb, col = "red" ); lines( pred_ols, col = 'blue')

sqrt( mean( (tsy_0 - pred_xgb)^2 ) )
sqrt( mean( (tsy_0 - pred_ols)^2 ) )
