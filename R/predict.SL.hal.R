predict.SL.hal <- function (object, newdata, ...) 
{
  preprocess_data <- function(data) {
    data <- data.frame(lapply(data, function(x) {
      as.numeric(x) 
    }), stringsAsFactors = FALSE)
    return(as.matrix(data)) 
  }
  newdata <- preprocess_data(newdata)
  pred <- stats::predict(object$object, new_data = newdata)
  return(pred)
}
