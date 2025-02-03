predict.SL.rpart_boost <- function(object, newdata, family, verbose=T, ...){
  if(object$fam.end == "gaussian") {
    pred <- predict(object$object, newdata = newdata)
  }
  if(object$fam.end == "binomial") {
    pred <- predict(object$object, newdata = newdata)[, 2]
  }
  if(object$fam.init=="binomial" & object$fam.end=="gaussian"){
    if(any(pred<0)){pred[pred<0]<-0}
    if(any(pred>1)){pred[pred>1]<-1}
    if((any(pred<0) | any(pred>1)) & verbose==T){cat("\n Note: predictions falling outside [0,1] have been set as 0/1 \n")}
  }
  return(pred)
}
  
  
