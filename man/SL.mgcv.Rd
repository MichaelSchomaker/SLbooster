\name{SL.mgcv}
\alias{SL.mgcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for gam(), from mgcv
}
\description{
Fits a generalized additive model (GAM) to data with smooth terms, using the implementations from \pkg{mgcv}.
}
\usage{
SL.mgcv(Y, X, newX, family, obsWeights = NULL, cts.num = 10, by = NA, verbose = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The response variable, which can be numeric or binary.
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{newX}{
A data frame or matrix of new predictor data used for generating predictions. If not provided, the model will only be fitted on \code{X}. 
}
  \item{family}{This is a family object specifying the distribution and link to use in fitting etc.
As well as the standard families (of class \code{family}) documented in \code{family} (see also \code{glm}), \code{mgcv} also supplies some extra families. These are described in \code{?mgcv::family.mgcv}. 
}
  \item{obsWeights}{
An optional vector of observation-specific weights to be used in the fitting process. Defaults to \code{NULL}.
}
  \item{cts.num}{
Determines the threshold for treating a variable as continuous and therefore introducing a smooth term in the \code{formula}. Defaults to \code{10}.
}
  \item{by}{
  Either a string describing one variable that is passed to all smooth terms as an interaction term or a vector of strings providing an interaction variable for each smooth term. See more under \code{mgcv::s}.
Defaults to \code{NA}.
}
  \item{verbose}{
A logical value indicating whether to print detailed information during the execution of the function. Defaults to \code{TRUE}.
}
  \item{\dots}{
Additional arguments, currently unused.
}
}
\value{
A list containing:
  \item{pred}{Predicted values from the fitted model, based on \code{newX} if provided.}
  \item{fit}{The fitted GAM model.}
}
\references{
Wood, S.N., N. Pya and B. Saefken (2016), Smoothing parameter and model selection for general
smooth models (with discussion). Journal of the American Statistical Association 111, 1548-1575
doi:10.1080/01621459.2016.1180986

Wood, S. N. (2020) Inference and computation with generalized additive models and their extensions.
Test 29(2): 307-339. doi:10.1007/s11749020007115

The \code{mgcv} package: \url{https://cran.r-project.org/web/packages/mgcv/index.html}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
If fitting fails, an intercept only model is fitted.
}

\seealso{
\code{\link[mgcv]{mgcv}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.mgcv(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.mgcv(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.mgcv", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}

\keyword{mgcv}
\keyword{regression}
\concept{Generalized Additive Models}
