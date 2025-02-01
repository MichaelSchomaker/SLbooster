predict.SL.orm <- function(object, newdata, ...) {
  # Load necessary package
  SLbooster.require("rms") # require("rms")
  
  # Check if the input model is of class "SL.orm"
  if (!inherits(object, "SL.orm")) {
    stop("Model input is not of type SL.orm")
  }
  
  # Convert newdata to data frame if it is a matrix
  if (is.matrix(newdata)) {
    newdata <- as.data.frame(newdata)
  }
  
  # Check the type of the response variable (yunique)
  y_type <- class(object$object$yunique)
  pred <- stats::predict(object = object$object, newdata = newdata)
  
  return(pred)
}