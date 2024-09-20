predict.SL.orm <- function(object, newdata, ...) {
  # Load necessary package
  require("rms")
  
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
  
  if (y_type == "numeric") {
    # If yunique is numeric, predict with type = "mean"
    pred <- rms:::predict.orm(object = object$object, newdata = newdata, type = "mean")
  } else {
    # Otherwise, predict with the default settings
    pred <- rms:::predict.orm(object = object$object, newdata = newdata)
  }
  
  return(pred)
}