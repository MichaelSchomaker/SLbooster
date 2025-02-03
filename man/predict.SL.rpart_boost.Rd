\name{predict.SL.rpart_boost}
\alias{predict.SL.rpart_boost}
\title{
Prediction Method for SL.rpart objects
}
\description{
This function provides a prediction method for objects of class \code{SL.rpart_boost}. 
}
\usage{
\method{predict}{SL.rpart_boost}(object, newdata, family, verbose=T, ...)
}

\arguments{
  \item{object}{
Result object from SL.rpart
}
\item{newdata}{
  Dataframe or matrix that will generate predictions
}
\item{family}{
  Family argument
}
\item{verbose}{Logical. True if notes should be printed.}
  \item{\dots}{
Additional arguments (currently not used).
}
}

\references{
see \code{\link{SL.rpart_boost}}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}


\seealso{
\code{\link{SL.rpart_boost}}
}
\examples{
\dontrun{data(EFV)
result <- SL.rpart_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                  newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
predict(object = result$fit, newdata = EFV[, c("efv.0", "metabolic", "weight.0")], family="binomial")
}
}
