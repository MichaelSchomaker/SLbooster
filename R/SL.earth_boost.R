SL.earth_boost <- function(Y, X, newX, family, obsWeights = NULL, id = NULL, degree = 2, penalty = 3,
                          nk = max(21, 2 * ncol(X) + 1), pmethod = "backward", nfold = 0,
                          ncross = 1, minspan = 0, endspan = 0, verbose=T, ...) {
  #
  if(verbose==T){cat("SL.earth_boost started (degrees=", degree,"). ", sep="")}
  start_time <- Sys.time()
  SuperLearner:::.SL.require("earth")
  #
  if (family$family == "gaussian") {
    fit.earth <- earth::earth(
      x = X, y = Y, degree = degree,
      nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold,
      ncross = ncross, minspan = minspan, endspan = endspan
    )
  }
  if (family$family == "binomial") {
    fit.earth <- earth::earth(
      x = X, y = Y, degree = degree,
      nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold,
      ncross = ncross, minspan = minspan, endspan = endspan,
      glm = list(family = binomial)
    )
  }
  #
  pred <- predict(fit.earth, newdata = newX, type = "response")
  fit <- list(object = fit.earth)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.earth_boost")
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.earth_boost finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}
