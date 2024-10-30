SL.median <- function (Y, X, newX, family = list(), obsWeights = NULL, id = NULL, ...) 
{
  # Assign equal weights if obsWeights is NULL
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, length(Y))  # Default to equal weights
  }
  
  medianY <- matrixStats::weightedMedian(Y, w = obsWeights, ...)
  pred <- rep.int(medianY, times = nrow(newX))
  fit <- list(object = medianY)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.median")
  return(out)
}