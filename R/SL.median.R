SL.median <- function (Y, X, newX, family = list(), obsWeights = NULL, id = NULL, ...) 
{
  SuperLearner:::.SL.require("matrixStats")
  medianY <- matrixStats::weightedMedian(Y, w = obsWeights, ...)
  pred <- rep.int(medianY, times = nrow(newX))
  fit <- list(object = medianY)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mean")
  return(out)
}