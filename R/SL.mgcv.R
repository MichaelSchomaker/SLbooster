SL.mgcv <- function(Y, X, newX = NULL, family = family, obsWeights = NULL, 
                         cts.num = 10, by=NA, verbose=F,
                         ...) {
  #
  if(verbose==T){cat("SL.mgcv started (if >", cts.num, " unique values = spline added; it interacts with: ", by, "). ", sep="")}
  start_time <- Sys.time()
  requireNamespace("mgcv") # require("mgcv")
  #
  if(all(by %in% colnames(X))==FALSE){if(verbose==T){cat(paste(by, "not in column names of X. `by=NA' is used.\n"))};by<-NA}
  #
  s <- mgcv::s # s() is also used by 'gam' package - avoid clash
  # adjust model formula for metric and categorical predictors
  metric_var <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if (sum(metric_var) != 0 & sum(metric_var) != length(metric_var)) {
    # metric and categorical variables
    gam.model <- as.formula(paste("Y~", paste(paste("s(",
                                                    colnames(X[, metric_var, drop = FALSE]), ", by=", by,
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
                                                      colnames(X[, metric_var, drop = FALSE]), ", by=", by,
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
  fit.gam <- try(mgcv::gam(gam.model,
                           data = X, family = family,
                           weights = obsWeights, ...
  ))
  if(class(fit.gam)[1]=="try-error"){
    gam.model <- as.formula(paste("Y~1"))
    fit.gam <- try(mgcv::gam(gam.model,
                             data = X, family = family,
                             weights = obsWeights, ...
    ))
    if(verbose==T){cat("GAM failed with variables provided. Intercept-only GAM fitted.")}
  }
  # 
  pred <- mgcv::predict.gam(fit.gam, newdata = newX, type = "response")
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mgcv")
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.mgcv finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}