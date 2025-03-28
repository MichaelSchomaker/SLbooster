\name{predict.SL.median}
\alias{predict.SL.median}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prediction Method for SL.median Objects
}
\description{
This function provides a prediction method for objects of class \code{SL.median}.
}
\usage{
\method{predict}{SL.median}(object, newdata, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A fitted object of class \code{SL.median}, typically produced by the \code{SL.median} function within the \code{SuperLearner} framework.
}
  \item{newdata}{
  A \code{data.frame} of new observations on which to obtain predictions. 
}
  \item{\dots}{
Additional arguments (currently not used).
}
}
\value{
A \code{numeric} vector of predicted median values corresponding to the rows in \code{newdata}. 
}

\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}


\seealso{
\code{\link{SL.median}}
}
\examples{
data(EFV)
result <- SL.median(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                    newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
predict(object = result$fit, newdata = EFV[, c("efv.0", "metabolic", "weight.0")])
}
\keyword{median}
\keyword{regression}
\keyword{classification}

