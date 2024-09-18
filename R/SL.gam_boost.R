SL.gam_boost <- function(Y, X, newX = NULL, family = list(), obsWeights = NULL, 
                        df.gam = 2, cts.num = 10, verbose=T,
                        ...) {
  #
  if(verbose==T){cat("SL.gam with ", df.gam," df for smoothing spline started (>", cts.num, " unique values = spline added). ", sep="")}
  start_time <- Sys.time()
  SuperLearner:::.SL.require("gam")
  #
  s <- gam:::s # s() is also used by 'mgcv' package - avoid clash
  # adjust model formula for metric and categorical predictors
  metric_var <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if (sum(metric_var) != 0 & sum(metric_var) != length(metric_var)) {
    # metric and categorical variables
    gam.model <- as.formula(paste("Y~", paste(paste("s(",
                                                    colnames(X[, metric_var, drop = FALSE]), ",", df.gam,
                                                    ")",
                                                    sep = ""
    ), collapse = "+"), "+", paste(colnames(X[,
                                              !metric_var,
                                              drop = FALSE
    ]), collapse = "+")))
  }else{
    if (all(metric_var)) {
      # metric variables only
      gam.model <- as.formula(paste("Y~", paste(paste("s(",
                                                      colnames(X[, metric_var, drop = FALSE]), ",", df.gam,
                                                      ")",
                                                      sep = ""
      ), collapse = "+")))
    } else {
      # all categorical
      gam.model <- as.formula(paste("Y~", paste(colnames(X),
                                                collapse = "+"
      ), sep = ""))
    }
  }
  fit.gam <- try(gam::gam(gam.model,
                          data = X, family = family,
                          control = gam::gam.control(maxit = 50, bf.maxit = 50),
                          weights = obsWeights, ...
  ))
  if(class(fit.gam)[1]=="try-error"){
    gam.model <- as.formula(paste("Y~1"))
    fit.gam <- try(gam::gam(gam.model,
                            data = X, family = family,
                            control = gam::gam.control(maxit = 50, bf.maxit = 50),
                            weights = obsWeights, ...
    ))
    if(verbose==T){cat("GAM failed with variables provided. Intercept-only GAM fitted.")}
  }
  # or predict.gam depending on version
  pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response")
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gam")
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.gam finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}
