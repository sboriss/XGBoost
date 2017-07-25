
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
arc = c(1.4, -0.5)
k   = 10

tsy_0 <- arima.sim(list(order = c( length(arc), 0, 0), ar = arc ), n = obs + length(arc))
ts.plot(tsy_0)

tsx = ts( matrix( rnorm( length(tsy_0) * k ), ncol = k ), start = start( tsy_0 ) )

data <- na.omit( cbind( tsy_0, stats::lag( tsy_0, k = -1), stats::lag( tsy_0, k = -2 ), tsx ) )

colnames( data ) <- c( paste0( "tsy_", seq( 0, length(arc) ) ), paste0( "x_", seq(k) ) )
head( data )

ols = lm( tsy_0 ~ ., data = data ); summary( ols )
pred_ols = ts( ols$fitted.values, start = start(data) )

mdata = data.frame( data ); colnames( mdata ) = colnames( data )

train <- list()
train$label = mdata %>% select(  tsy_0 )
train$data  = mdata %>% select( -tsy_0 )

dtrain = xgb.DMatrix( as.matrix( train$data ), label = c( train$label )$tsy_0 )

cv_max_depth = lapply( seq( 10 ), function( max_depth ){ # cat( 'max_depth', max_depth, '\n' )

  cv <- xgb.cv( data = dtrain, nround = 100, nthread = 4, nfold = 5, metrics = list( "rmse" ), max_depth = max_depth,
               eta = 1, objective = "reg:linear", early_stopping_rounds = 5 )
  
  #cat( "cv$best_iteration", cv$best_iteration, '\n' )
  #cat( "cv$evaluation_log\n"); print( cv$evaluation_log, '\n' )
  
  best_iter = cv$best_iteration
  
  data.frame( max_depth = max_depth, best_iter = best_iter,
              rmse_mean = cv$evaluation_log[ best_iter, 'test_rmse_mean' ],
                                             rmse_std  = cv$evaluation_log[ best_iter, 'test_rmse_std'  ] )
  
})

mcv_max_depth = do.call( rbind.data.frame,  cv_max_depth)

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
              "max_depth" = max_depth_opt
              )

xgModel = xgboost(data = dtrain, nround = 2000, params = param, print_every_n=5 )

importance_matrix = xgb.importance(model = xgModel)
print(importance_matrix)
xgb.ggplot.importance(importance_matrix)

pred = ts( predict( xgModel, dtrain ), start = start(data) )
plot( tsy_0 ); lines( pred, col = "red" ); lines( pred_ols, col = 'blue')

sqrt( mean( (tsy_0 - pred    )^2 ) )
sqrt( mean( (tsy_0 - pred_ols)^2 ) )
