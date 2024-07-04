SL.orm <- function (Y, X, newX, verbose=T,...){
  #
  if (all(Y == 0 | Y == 1)) {
    if(verbose==T){cat("SL.orm started.")}
    start_time <- Sys.time()
    if(verbose==T){("Binary outcome: GLM used instead of ORM.")}
    out <- SuperLearner::SL.glm(Y=Y,X=X,newX=newX,...)
  }else{
    #
    if(verbose==T){cat("SL.orm started.")}
    start_time <- Sys.time()
    SuperLearner:::.SL.require("rms")
    #
    if(length(unique(Y))<5){
      out <- SuperLearner::SL.mean(Y=Y,X=X,newX=newX,...)
      if(verbose==T){cat("Not enough different unique values in Y: mean used instead of ORM. ")}
    }else{
      XX <- as.data.frame(cbind(Y,X))
      fit.orm <- try(rms::orm(Y ~ ., data = XX), silent=TRUE)
      if(length(class(fit.orm))==2 & fit.orm$fail==FALSE){ # if length==1, then either error or technical problem
        if (is.matrix(newX)) {
          newX = as.data.frame(newX)
        }
        pred <- rms:::predict.orm(fit.orm, newdata = newX, type = "mean")
        fit <- list(object = fit.orm)
        class(fit) <- "SL.orm"
        out <- list(pred = pred, fit = fit)
      }else{
        out <- SuperLearner::SL.glm(Y=Y,X=X,newX=newX,...)
        if(verbose==T){cat("Technical problem with ORM: GLM used instead.") }
      }
    }
  }
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.orm finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}
