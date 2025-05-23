\name{predict.SL.mgcv}
\alias{predict.SL.mgcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prediction Method for SL.mgcv Objects
}
\description{
This function provides a prediction method for objects of class \code{SL.mgcv}, which are fitted generalized additive models (GAM) with smooth terms wrapped within the \code{SuperLearner} framework. It uses the fitted GAM model to generate predictions on new data.
}
\usage{
\method{predict}{SL.mgcv}(object, newdata, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A fitted object of class \code{SL.mgcv}, typically produced by the \code{SL.mgcv} function within the \code{SuperLearner} framework.
}
  \item{newdata}{
  A \code{data.frame} of new observations on which to obtain predictions. 
}
  \item{\dots}{
Additional arguments (currently not used).
}
}
\value{
A \code{numeric} vector of predicted values corresponding to the rows in \code{newdata}. The predictions are generated by the fitted GAM model contained within the \code{SL.mgcv} object.
}
\references{
see \code{\link{SL.mgcv}}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}


\seealso{
\code{\link{SL.mgcv}}, \code{\link[mgcv]{gam}}
}
\examples{
data(EFV)
result <- SL.mgcv(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                  newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
predict(object = result$fit, newdata = EFV[, c("efv.0", "metabolic", "weight.0")])
}
  \keyword{gam}
  \keyword{regression}
  \keyword{classification}
  \concept{Generalized Additive Models}