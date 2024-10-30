\name{SL.earth_boost}
\alias{SL.earth_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SL.earth_boost: SuperLearner Wrapper for Boosted Earth Models
}
\description{
This function serves as a wrapper for the \code{earth} function within the \code{SuperLearner} framework, enabling the use of Multivariate Adaptive Regression Splines (MARS) with boosting for regression and classification tasks. It fits an \code{earth} model (MARS) to the data and generates predictions for new observations.
}
\usage{
SL.earth_boost(Y, X, newX, family, obsWeights = NULL, id = NULL, degree = 2, penalty = 3, 
               nk = max(21, 2 * ncol(X) + 1), pmethod = "backward", nfold = 0, ncross = 1, 
               minspan = 0, endspan = 0, verbose = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
  A \code{numeric} vector of outcomes (dependent variable). For binary classification, it should be a binary factor or 0/1 values.
}
  \item{X}{
  A \code{matrix} or \code{data.frame} of predictor variables (independent variables).
}
  \item{newX}{
  An optional \code{matrix} or \code{data.frame} of new data on which to predict outcomes using the fitted model. If not supplied, predictions will be generated on the training data.
}
  \item{family}{
  A \code{list} describing the error distribution and link function to be used in the model. For Gaussian regression, use \code{family = list(family = "gaussian")}; for binary classification, use \code{family = list(family = "binomial")}.
}
  \item{obsWeights}{
  Optional observation weights. Defaults to \code{NULL}, implying equal weights for all observations.
}
  \item{id}{
  Optional vector of IDs for clustering/grouping, currently unused in this function.
}
  \item{degree}{
  Maximum degree of interaction terms in the MARS model. Default is \code{2}.
}
  \item{penalty}{
  The penalty for adding a term to the model. Default is \code{3}.
}
  \item{nk}{
  Maximum number of model terms (including intercept). Defaults to \code{max(21, 2 * ncol(X) + 1)}.
}
  \item{pmethod}{
  Pruning method to use: \code{"backward"} (default), \code{"none"}, \code{"exhaustive"}, or \code{"forward"}. Determines how to simplify the model.
}
  \item{nfold}{
  Number of cross-validation folds. Default is \code{0}, which implies no cross-validation.
}
  \item{ncross}{
  Number of cross-validation repetitions. Default is \code{1}.
}
  \item{minspan}{
  Minimum number of observations between knots. Default is \code{0}.
}
  \item{endspan}{
  Minimum number of observations required between the end of the data and the nearest knot. Default is \code{0}.
}
  \item{verbose}{
  Logical; if \code{TRUE}, the function prints detailed progress messages. Default is \code{FALSE}.
}
  \item{\dots}{
  Additional arguments currently unused.
}
}
\details{
The \code{SL.earth_boost} function fits an \code{earth} model (MARS) to the data using specified parameters and generates predictions for new data. It supports both regression and classification tasks and provides a flexible set of options for model tuning, including boosting through cross-validation repetitions.
}
\value{
A \code{list} containing:
\item{pred}{A \code{numeric} vector of predicted values for the new data (\code{newX}).}
\item{fit}{An object of class \code{"SL.earth"}, which includes the fitted \code{earth} model and related information.}
}
\references{
Milborrow, S. (2021). earth: Multivariate Adaptive Regression Spline Models. R package version 5.3.0. \url{https://cran.r-project.org/package=earth}

Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1), 1-67.
}
\author{
  Original authors of the \code{earth} package: see reference above.

  Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
Ensure that \code{earth} is installed and loaded correctly, as this function relies on the MARS model implemented in the \code{earth} package.
}

\seealso{
\code{\link[earth]{earth}}, \code{\link[SuperLearner]{SuperLearner}}, \code{\link[SuperLearner]{SL.glm}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.earth_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.earth_boost(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.earth_boost", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}
\keyword{MARS}
\keyword{regression}
\keyword{classification}
\concept{Multivariate Adaptive Regression Splines}
\concept{SuperLearner}
