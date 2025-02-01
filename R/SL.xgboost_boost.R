SL.xgboost_boost <- function (Y, X, newX, family, obsWeights = NULL, id = NULL, ntrees = 1000, 
                             max_depth = 4, eta = 0.1, minobspernode = 10, params = list(), 
                             nthread = 1, verb = 0, save_period = NULL, verbose=T, ...) 
{
  #
  if(verbose==T){cat("SL.xgboost (tree booster) started with max. tree depth of ", max_depth, ", ", ntrees, " trees, eta=", eta, ". ", sep="")}
  start_time <- Sys.time()
  SLbooster.require("xgboost")
  #
  if (!is.matrix(X)) {
    X = model.matrix(~. - 1, X)
  }
  xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)
  if (family$family == "gaussian") {
    if (packageVersion("xgboost") >= "1.1.1.1") {
      objective <- "reg:squarederror"
    }
    else {
      objective <- "reg:linear"
    }
    model = xgboost::xgboost(data = xgmat, objective = objective, 
                             nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
                             eta = eta, verbose = verb, nthread = nthread, 
                             params = params, save_period = save_period)
  }
  if (family$family == "binomial") {
    model = xgboost::xgboost(data = xgmat, objective = "binary:logistic", 
                             nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
                             eta = eta, verbose = verb, nthread = nthread, 
                             params = params, save_period = save_period, eval_metric = "logloss")
  }
  if (family$family == "multinomial") {
    model = xgboost::xgboost(data = xgmat, objective = "multi:softmax", 
                             nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
                             eta = eta, verbose = verb, num_class = length(unique(Y)), 
                             nthread = nthread, params = params, save_period = save_period)
  }
  if (!is.matrix(newX)) {
    newX = model.matrix(~. - 1, newX)
  }
  pred = predict(model, newdata = newX)
  fit = list(object = model)
  class(fit) = c("SL.xgboost")
  out = list(pred = pred, fit = fit)
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.xgboost finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}
