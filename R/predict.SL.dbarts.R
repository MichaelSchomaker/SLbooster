predict.SL.dbarts <- 
  function (object, newdata,  ...) 
  {
    pred <- colMeans(predict(object$object, newdata = newdata))
    return(pred)
  }