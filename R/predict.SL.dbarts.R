predict.SL.dbarts <- 
  function (object, newdata, family = list(), ...) 
  {
    pred = colMeans(predict(object$object, newdata = newdata))
    return(pred)
  }
