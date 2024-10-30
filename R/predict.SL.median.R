predict.SL.median <- function (object, newdata, ...) 
{
  pred <- rep(object$object, length = nrow(newdata))
  
  return(pred)
}