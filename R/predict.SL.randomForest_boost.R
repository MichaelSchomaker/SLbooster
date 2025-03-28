predict.SL.randomForest_boost <- function (object, newdata, ...) 
{
  pred <- unname(predict(object = object$object, newdata = newdata))
  return(pred)
}
