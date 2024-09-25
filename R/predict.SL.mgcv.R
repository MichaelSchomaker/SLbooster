predict.SL.mgcv <- function (object, newdata, ...) 
{
  requireNamespace("mgcv") # require("mgcv")
  pred <- mgcv::predict.gam(object = object$object, newdata = newdata, 
                            type = "response")
  
  return(pred)
}