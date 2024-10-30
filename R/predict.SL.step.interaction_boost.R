predict.SL.step.interactoin_boost <- function (object, newdata, ...) 
{
  pred <- as.numeric(stats::predict(object = object$object, newdata = newdata, 
                            type = "response"))
  
  return(pred)
}