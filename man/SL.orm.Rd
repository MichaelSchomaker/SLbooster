\name{SL.orm}
\alias{SL.orm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for Ordinal Regression Models 
}
\description{
Wrapper for fitting ordinal regression models using \code{orm()} in package \pkg{rms}.}
\usage{
SL.orm(Y, X, newX, family, obsWeights = NULL, verbose = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The response variable. If the outcome is binary, a GLM is used instead of an ordinal regression model.
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{newX}{
A data frame or matrix of new predictor data used for generating predictions. If not provided, the model will only be fitted on \code{X}.
}
\item{family}{
A description of the error distribution and link function to be used in the model (e.g., \code{binomial} or \code{gaussian}). It is passed to the \code{gam} function for model fitting.
}

  \item{obsWeights}{
An optional vector of observation-specific weights to be used in the fitting process. Defaults to \code{NULL}.
}
  \item{verbose}{
A logical value indicating whether to print detailed information during the execution of the function. Defaults to \code{TRUE}.
}
  \item{\dots}{
Additional arguments currently unused
}
}
\details{
The \code{SL.orm} function is designed to work within the \code{SuperLearner} framework. It fits an ordinal regression model using the \code{orm} function from the \code{rms} package when the outcome variable \code{Y} is ordinal (more than two levels). If \code{Y} is binary, the function defaults to a generalized linear model (GLM). In cases where the number of unique levels in \code{Y} is insufficient for ordinal regression (less than 5 levels), the function defaults to using the \code{SL.mean} algorithm to avoid fitting issues.
}
\value{
A list containing:
  \item{pred}{Predicted values from the fitted model, based on \code{newX} if provided.}
  \item{fit}{The fitted ordinal regression model (if applicable) or the alternative model (GLM or mean).}
}
\references{
Harrell FE Jr. (2023). \emph{Regression Modeling Strategies}. Springer Series in Statistics.
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{orm}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.orm(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                  newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = "binomial")
result2 <- SL.orm(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                  newX = EFV[, c("sex", "metabolic", "log_age")], family = "gaussian")
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.orm", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{regression}
\concept{ordinal regression}
