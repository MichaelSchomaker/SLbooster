SL.glmnet_boost <- function (Y, X, newX, family, obsWeights = NULL, id = NULL, 
                             alpha = 1, nfolds = 10, nlambda = 100, useMin = TRUE, loss = "deviance", 
                             verbose=T, ...) 
{
  #
  if(verbose==T){cat("SL.glmnet started with alpha=", alpha, ", ", nfolds, "-fold CV, and ", nlambda, " candidate lambdas. ", sep="")}
  start_time <- Sys.time()
  SLbooster.require("glmnet")
  # for ltmle 
  fam.init <- family$family
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- "binomial"
  } else {
    family$family <- "gaussian"
  }
  fam.end <- family$family
  #
  if (!is.matrix(X)) {
    X <- model.matrix(~-1 + ., X)
    newX <- model.matrix(~-1 + ., newX)
  }
  fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights, 
                             lambda = NULL, type.measure = loss, nfolds = nfolds, 
                             family = family$family, alpha = alpha, nlambda = nlambda)
  pred <- predict(fitCV, newx = newX, type = "response", 
                  s = ifelse(useMin, "lambda.min", "lambda.1se"))
  #
  if(fam.init=="binomial" & fam.end=="gaussian"){
    if(any(pred<0)){pred[pred<0]<-0}
    if(any(pred>1)){pred[pred>1]<-1}
    if((any(pred<0) | any(pred>1)) & verbose==T){cat("\n Note: predictions falling outside [0,1] have been set as 0/1 \n")}
    }
  #
  fit <- list(object = fitCV, useMin = useMin, fam.init=fam.init, fam.end=fam.end)
  class(fit) <- "SL.glmnet_boost"
  out <- list(pred = pred, fit = fit)
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.glmnet finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}
