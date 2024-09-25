\name{SL.mgcv}
\alias{SL.mgcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generalized Additive Model (SL.mgcv)
}
\description{
Fits a generalized additive model (GAM) to data with smooth terms. If the fitting fails on the provided variables \code{X}, an intercept only model is fitted.
}
\usage{
SL.mgcv(Y, X, newX = NULL, family = family, obsWeights = NULL, cts.num = 10, by = NA, verbose = F, ...)
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
A data frame or matrix of new predictor data used for generating predictions. If not provided, the model will only be fitted on \code{X}. Defaults to \code{NULL}.
}
  \item{family}{This is a family object specifying the distribution and link to use in fitting etc.
As well as the standard families (of class \code{family}) documented in \code{family} (see also \code{glm}), \code{mgcv} also supplies some extra families. These are described in \code{?mgcv::family.mgcv}. Defaults to \code{list()}.
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
A logical value indicating whether to print detailed information during the execution of the function. Defaults to \code{FALSE}.
}
  \item{\dots}{
Additional arguments passed to \code{mgcv::gam()} and via this route to e.g. \code{gam.fit}.
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

The \code{SuperLearner} package: \url{https://cran.r-project.org/web/packages/SuperLearner/index.html}
}
\author{
original author of the mgcv package: Simon Wood

Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
Ensure that \code{mgcv} is installed and loaded correctly, as this function relies on the GAM model implemented in the \code{mgcv} package.
}

\seealso{
\code{\link[mgcv]{mgcv}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

  \keyword{gam}
  \keyword{regression}
  \keyword{classification}
  \concept{Generalized Additive Models}
  \concept{SuperLearner}