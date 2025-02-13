SL.rpart_boost <- function (Y, X, newX, family, obsWeights = NULL, cp = 0.01, minsplit = 20, 
                            xval = 0L, maxdepth = 30, minbucket = round(minsplit/3), verbose=T,
                            ...) 
{
  #
  if(verbose==T){cat("SL.rpart started with minsplit=", minsplit, ", maxdepth=", maxdepth, ", and ", xval, "-fold CV (0=no CV). ", sep="")}
  start_time <- Sys.time()
  SLbooster.require("rpart")
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
  if (family$family == "gaussian") {
    fit.rpart <- try(rpart::rpart(Y ~ ., data = data.frame(Y, 
                                                       X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, 
                                                                                          xval = xval, maxdepth = maxdepth, minbucket = minbucket), 
                              method = "anova", weights = obsWeights),silent=TRUE)
    try(pred <- predict(fit.rpart, newdata = newX),silent=TRUE)
  }
  if (family$family == "binomial") {
    fit.rpart <- try(rpart::rpart(Y ~ ., data = data.frame(Y, 
                                                       X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, 
                                                                                          xval = xval, maxdepth = maxdepth, minbucket = minbucket), 
                              method = "class", weights = obsWeights),silent=TRUE)
    try(pred <- predict(fit.rpart, newdata = newX)[, 2],silent=TRUE)
  }
  if(any(class(fit.rpart) == "try-error")){
    pred <- rep(mean(Y), nrow(X))
    if(verbose==T){"rpart failed: simply predicting mean of Y."}
  }
  if(fam.init=="binomial" & fam.end=="gaussian"){if(any(pred<0)){pred[pred<0]<-0};if(any(pred>1)){pred[pred>1]<-1}}
  #
  fit <- list(object = fit.rpart)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.rpart_boost")
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.rpart finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}