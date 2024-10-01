SL.randomForest_boost <- function(Y, X, newX, family, verbose=T, 
                                 mtry = ifelse(family$family =="gaussian", max(floor(ncol(X) / 3), 1), floor(sqrt(ncol(X)))),
                                 ntree = 100, nodesize = ifelse(family$family == "gaussian",5, 1), 
                                 maxnodes = NULL, importance = FALSE, ...) {
  #
  if(verbose==T){cat("SL.randomForest with ", ntree," trees started. ", sep="")}
  start_time <- Sys.time()
  SuperLearner:::.SL.require("randomForest")
  # avoid infinite search for split points in trees
  if (all(suppressWarnings(apply(X,2,var) == 0))) {
    fit.rf <- "Empty"
    attr(fit.rf, "class") <- "try-error"
    pred <- rep(mean(Y), nrow(X))
    fit <- list(object = fit.rf)
  }
  #
  if (family$family == "gaussian" & !exists("fit.rf")) {
    fit.rf <- randomForest::randomForest(Y ~ .,
                                         data = X,
                                         ntree = ntree, xtest = newX, keep.forest = TRUE,
                                         mtry = mtry, nodesize = nodesize, maxnodes = maxnodes,
                                         importance = importance, ...
    )
    try(pred <- fit.rf$test$predicted, silent = TRUE)
    if (any(class(fit.rf) == "try-error")) {
      pred <- rep(mean(Y), nrow(X))
      if(verbose==T){"Random forest failed: simply predicting mean of Y."}
    }
    fit <- list(object = fit.rf)
  }
  if (family$family == "binomial" & !exists("fit.rf")) {
    fit.rf <- randomForest::randomForest(
      y = as.factor(Y),
      x = X, ntree = ntree, xtest = newX, keep.forest = TRUE,
      mtry = mtry, nodesize = nodesize, maxnodes = maxnodes,
      importance = importance, ...
    )
    try(pred <- fit.rf$test$votes[, 2], silent = TRUE)
    if (any(class(fit.rf) == "try-error")) {
      pred <- rep(mean(Y), nrow(X))
      if(verbose==T){"Random forest failed: simply predicting mean of Y."}
    }
    fit <- list(object = fit.rf)
  }
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.randomForest")
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.randomForest finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}
