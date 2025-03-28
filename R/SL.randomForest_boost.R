SL.randomForest_boost <- function(Y, X, newX, family, verbose=T, 
                                 mtry = ifelse(family$family =="gaussian", max(floor(ncol(X) / 3), 1), floor(sqrt(ncol(X)))),
                                 ntree = 100, nodesize = ifelse(family$family == "gaussian",5, 1), 
                                 maxnodes = ifelse(nrow(X)<1000,200,300), importance = FALSE, ...) {
  #
  if(verbose==T){cat("SL.randomForest with ", ntree," trees started. ", sep="")}
  start_time <- Sys.time()
  SLbooster.require("randomForest")
  
  # avoid infinite search for split points in trees
  if (all(suppressWarnings(apply(X,2,var) == 0))) {
    fit.rf <- "Empty"
    attr(fit.rf, "class") <- "try-error"
    pred <- rep(mean(Y), nrow(X))
    fit <- list(object = fit.rf)
  }
  #
  if (family$family == "gaussian" & !exists("fit.rf")) {
    
    fit.rf <- try(randomForest::randomForest(Y ~ .,
                                         data = X,
                                         ntree = ntree, xtest = newX, keep.forest = TRUE,
                                         mtry = mtry, nodesize = nodesize, maxnodes = maxnodes,
                                         importance = importance, ...),silent=T)
    
    if (any(class(fit.rf) == "try-error")){
      out <- SuperLearner::SL.glm(Y = Y, X = X, newX = newX, family = family, obsWeights = NULL, ...)
      if(verbose==T){"Random forest failed: simply using GLM."}
      }else {
      pred <- fit.rf$test$predicted; fit <- list(object = fit.rf)
      out <- list(pred = pred, fit = fit)
      class(out$fit) <- c("SL.randomForest_boost")
      }
  }
  if (family$family == "binomial" & !exists("fit.rf")) {
    
    fit.rf <- try(randomForest::randomForest(
      y = as.factor(Y),
      x = X, ntree = ntree, xtest = newX, keep.forest = TRUE,
      mtry = mtry, nodesize = nodesize, maxnodes = maxnodes,
      importance = importance, ...),silent=TRUE)
    
    if (any(class(fit.rf) == "try-error")){
      out <- SuperLearner::SL.glm(Y = Y, X = X, newX = newX, family = family, obsWeights = NULL, ...)
      if(verbose==T){"Random forest failed: simply using GLM."}
      }else {
      pred <-  fit.rf$test$votes[, 2]; fit <- list(object = fit.rf)
      out <- list(pred = pred, fit = fit)
      class(out$fit) <- c("SL.randomForest_boost")}
  }
  
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.randomForest finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}
