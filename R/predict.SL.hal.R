predict.SL.hal <- function (object, newdata, verbose=T, ...) 
{
  preprocess_data <- function(data) {
    data <- data.frame(lapply(data, function(x) {
      as.numeric(x) 
    }), stringsAsFactors = FALSE)
    return(as.matrix(data)) 
  }
  newdata <- preprocess_data(newdata)
  pred <- stats::predict(object$object, new_data = newdata)
  if(object$fam.init=="binomial" & object$fam.end=="gaussian"){
    if(any(pred<0)){pred[pred<0]<-0}
    if(any(pred>1)){pred[pred>1]<-1}
    if((any(pred<0) | any(pred>1)) & verbose==T){cat("\n Note: predictions falling outside [0,1] have been set as 0/1 \n")}
  }
  return(pred)
}
