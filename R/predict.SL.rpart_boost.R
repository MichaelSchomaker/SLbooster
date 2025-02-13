predict.SL.rpart_boost <- function(object, newdata, family, verbose=T, ...){
  detected.family <- family$family
    if(all(object$object$y == 1 | object$object$y == 2)) {
      actual.family <- "binomial"
    }else{
      actual.family <- "gaussian"
    }  
  if(actual.family == "gaussian") {
    pred <- predict(object$object, newdata = newdata)
  }
  if(actual.family == "binomial") {
    pred <- predict(object$object, newdata = newdata)[, 2]
  }
  if(detected.family=="binomial" & actual.family=="gaussian"){
    if(any(pred<0)){pred[pred<0]<-0}
    if(any(pred>1)){pred[pred>1]<-1}
    if((any(pred<0) | any(pred>1)) & verbose==T){cat("\n Note: predictions falling outside [0,1] have been set as 0/1 \n")}
  }
  return(pred)
}
  
  
