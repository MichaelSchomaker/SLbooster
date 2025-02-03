\name{predict.SL.glmnet_boost}
\alias{predict.SL.glmnet_boost}
\title{
Prediction Method for SL.glmnet objects
}
\description{
This function provides a prediction method for objects of class \code{SL.glmnet_boost}. 
}
\usage{
\method{predict}{SL.glmnet_boost}(object, newdata, remove_extra_cols = T, add_missing_cols = T, verbose=T, ...)
}

\arguments{
  \item{object}{
Result object from SL.glmnet
}
  \item{newdata}{
  Dataframe or matrix that will generate predictions
}
\item{remove_extra_cols}{Remove any extra columns in the new data that were not part of the original model}
\item{add_missing_cols}{Add any columns from original data that do not exist in the new data, and set values to 0}
\item{verbose}{Logical. True if notes should be printed.}
  \item{\dots}{
Additional arguments (currently not used).
}
}

\references{
see \code{\link{SL.glmnet_boost}}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}


\seealso{
\code{\link{SL.glmnet_boost}}
}
\examples{
data(EFV)
result <- SL.glmnet_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                  newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
predict(object = result$fit, newdata = EFV[, c("efv.0", "metabolic", "weight.0")], family="binomial")
}
