predict.SL.orm <- function (object, newdata, ...) 
{
  require("rms")
  if (is.matrix(newdata)) {
    newdata = as.data.frame(newdata)
  }
  pred <- rms:::predict.orm(object = object$object, newdata = newdata, type = "mean")
  pred
}