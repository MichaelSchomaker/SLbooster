\name{SL.median}
\alias{SL.median}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper using the median
}
\description{
Wrapper for calculating the (weighted) median for (constant) predictions.
}
\usage{
SL.median(Y, X, newX, family, obsWeights, id, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
  A \code{numeric} vector of outcomes (dependent variable). For binary classification, it should be a binary factor or 0/1 values.
}
  \item{X}{
  A \code{matrix} or \code{data.frame} of predictor variables (independent variables). This argument is not used in the function but is required for compatibility with the \code{SuperLearner} package.
}
  \item{newX}{
  A \code{matrix} or \code{data.frame} of new observations on which to generate predictions. Predictions are constant and equal to the weighted median of \code{Y}.
}
  \item{family}{A \code{\link[stats]{family}} object (one that is required for compatibility with \code{SuperLearner}). Currently unused. Default is \code{list()}.
}
  \item{obsWeights}{
  A \code{numeric} vector of observation weights. These weights are used to compute the weighted median of \code{Y}. Defaults to equal weights if not specified. Default is \code{NULL}.
}
  \item{id}{
  Optional vector of IDs for clustering/grouping, currently unused in this function. Default is \code{NULL}.
}
  \item{\dots}{
  Additional arguments currently unused.
}
}

\value{
A \code{list} containing:
\item{pred}{A \code{numeric} vector of constant predictions equal to the weighted median of \code{Y} for each observation in \code{newX}.}
\item{fit}{An object of class \code{"SL.mean"} containing the computed weighted median of \code{Y}.}
}
\references{
Documentation for the \code{matrixStats} package: \url{https://cran.r-project.org/package=matrixStats}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}


\seealso{
\code{\link[matrixStats]{weightedMedian}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.median(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.median(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.median", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}
\keyword{Median}
