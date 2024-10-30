predict.SL.earth_boost <- function (object, newdata, ...) 
{
  pred <- as.vector(stats::predict(object = object$object, newdata = newdata, 
                            type = "response"))
  
  return(pred)
}