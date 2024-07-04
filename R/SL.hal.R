SL.hal <- function (Y, X, newX, verbose=T, family=stats::gaussian(),
                    obsWeights = rep(1,length(Y)), ...){
  #
  if(verbose==T){cat("SL.hal started. ")}
  start_time <- Sys.time()
  SuperLearner:::.SL.require("hal9001")
  #
  if(!is.matrix(X)){X <- as.matrix(X)}
  if(!is.null(newX) & !is.matrix(newX)){newX <- as.matrix(newX)}
  #
  fit.hal <- try(hal9001::fit_hal(X= X,Y=Y, family=family$family,
                                  num_knots = hal9001:::num_knots_generator(
                                    max_degree = ifelse(ncol(X)>=20,2,3),
                                    smoothness_order = 1,
                                    base_num_knots_0 = ifelse(sqrt(nrow(X)) < 200, sqrt(nrow(X)), 200),
                                    base_num_knots_1 = ifelse(sqrt(nrow(X)) < 50, sqrt(nrow(X)), 50)),
                                  fit_control = list(cv_select=T,
                                                     n_folds =10,
                                                     weights = obsWeights,
                                                     foldid =NULL,
                                                     use_min = T,
                                                     lambda.min.ratio = 1e-4,
                                                     prediction_bounds ="default"),
  ), silent=TRUE)
  #
  if(class(fit.hal)[1]=="try-error"){
    if(verbose==T){cat("Technical failure: GLM used instead of HAL.")}
    out <- SuperLearner::SL.glm(Y=Y,X=X,newX=newX,family=family, obsWeights=rep(1,length(Y)),...)
  }else{
    if (!is.null(newX)) {
      pred <- stats::predict(fit.hal, new_data = newX)
    }
    else {
      pred <- stats::predict(fit.hal, new_data = X)
    }
    fit <- list(object = fit.hal)
    class(fit) <- "SL.hal"
    out <- list(pred = pred, fit = fit)
  }
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.hal finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}