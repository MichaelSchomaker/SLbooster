predict.SL.hal <- function (object, newdata, ...) 
{
  if(!is.matrix(newdata)){newdata <- as.matrix(newdata)}
  pred <- stats::predict(object$object, new_data = newdata)
  return(pred)
}
