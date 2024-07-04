predict.SL.dbarts <- 
  function (object, newdata, family, ...) 
  {
    pred = colMeans(predict(object$object, newdata = newdata))
    return(pred)
  }
