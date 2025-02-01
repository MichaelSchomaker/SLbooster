screen.randomForest_boost <- function(Y, X, family = list(), nVar = 8, ntree = 200, verbose=T, 
                                     mtry = ifelse(family$family == "gaussian", floor(sqrt(ncol(X))), max(floor(ncol(X) / 3), 1)),
                                     nodesize = ifelse(family$family == "gaussian", 5, 1), maxnodes = NULL,
                                     ...) {
  #
  if(verbose==T){cat("screen.randomForest with ntree=", ntree, ", mtry=", mtry, " and selecting ", nVar,  " variables \n", sep="")}
  if(nVar<1){stop("nVar needs to be greater or equal to one")}
  start_time <- Sys.time()
  SLbooster.require("randomForest")
  # chose family dependent upon response variable
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- "binomial"
  } else {
    family$family <- "gaussian"
  }
  if (ncol(X) > nVar) try({
    t_ime <- {
      if (family$family == "gaussian") {
        rank.rf.fit <- randomForest::randomForest(Y ~ .,
                                                  data = X,
                                                  ntree = ntree, mtry = mtry, nodesize = nodesize,
                                                  keep.forest = FALSE, maxnodes = maxnodes, importance = TRUE,
                                                  ...
        )
        # variables with the largest %IncMSE are the most important ones
        # negative scores mean zero or low importance
        imp_measure <- rank.rf.fit$importance[, "%IncMSE"]
        whichVariable <- (rank(-imp_measure, ties.method = "random") <= nVar)
      }
      if (family$family == "binomial") {
        rank.rf.fit <- randomForest::randomForest(as.factor(Y) ~ .,
                                                  data = X,
                                                  ntree = ntree, mtry = mtry, nodesize = nodesize,
                                                  keep.forest = FALSE, maxnodes = maxnodes, importance = TRUE,
                                                  ...
        )
        # variables with the largest mean decrease in accuracy are the most important ones
        # negative scores mean zero or low importance
        imp_measure <- rank.rf.fit$importance[, "MeanDecreaseAccuracy"]
        whichVariable <- (rank(-imp_measure, ties.method = "random") <= nVar)
      }
    }
    
  })else{stop("number of variables to select is greater than number of columns")}
  if(class(rank.rf.fit)[1]=="try-error"){
    whichVariable <- screen.cramersv(Y,X)
    if(verbose==T){cat("Random forest failed and screening was based on Cramer's V\n")}}
  #
  if(verbose==T){cat("screened ",sum(whichVariable)," variables: ",paste(colnames(X)[whichVariable]),"\n",sep=" ");
    cat("sample size:",dim(X)[1],"; ")}
  end_time <- Sys.time()
  if(verbose==T){cat("time:", round(difftime(end_time, start_time, units="secs"), digits=4), "secs \n")}
  #
  return(whichVariable)
  #
}