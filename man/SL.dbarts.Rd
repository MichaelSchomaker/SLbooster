\name{SL.dbarts}
\alias{SL.dbarts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Wrapper for dbarts}
\description{
Wrapper for Bayesian Additive Regression Trees using \code{dbarts}.
}

\usage{
SL.dbarts(Y, X, newX, family, obsWeights = NULL, id = NULL, sigest = NA, sigdf = 3, 
            sigquant = 0.9, k = 2, power = 2, base = 0.95, binaryOffset = 0, 
            ntree = 200, ndpost = 1000, nskip = 100, printevery = 100, 
            keepevery = 1, keeptrainfits = TRUE, usequants = FALSE, numcut = 100, 
            printcutoffs = 0, nthread = 1, keepcall = TRUE, verb=F, verbose = TRUE, 
            ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The response variable, which can be numeric or binary. In the numeric case errors are treated as normal. In the binary case, a probit link is fit. 
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{newX}{
A data frame or matrix of new predictor data used for generating predictions. If not provided, the model will only be fitted on \code{X}.
}
  \item{family}{
  A \code{list} describing how the prediction should be calculated. For numeric \code{Y}, use \code{family = list(family = "gaussian")}; for binary classification, use \code{family = list(family = "binomial")}.
}
  \item{obsWeights}{
  An optional vector of weights to be used in the fitting process. When present, BART fits a model with observations \eqn{y \mid x \sim N(f(x), \sigma^2 / w)}{y | x ~ N(f(x), \sigma^2 / w)}, where \eqn{f(x)} is the unknown function. Defaults to \code{NULL}.
}
  \item{id}{
  Optional vector of IDs for clustering/grouping, currently unused in this function. Defaults to \code{NULL}.
}
  \item{sigest}{
For continuous response models, an estimate of the error variance, \eqn{\sigma^2}, used to calibrate an inverse-chi-squared prior used on that parameter. If not supplied, the least-squares estimate is derived instead. See \code{sigquant} for more information. Not applicable when \eqn{y} is binary. Defaults to \code{NA}.
}
  \item{sigdf}{
     Degrees of freedom for error variance prior. Not applicable when \eqn{y} is binary. Defaults to \code{3}.
}
  \item{sigquant}{
     The quantile of the error variance prior that the rough estimate (\code{sigest}) is placed at. The closer the quantile is to 1, the more aggresive the fit will be as you are putting more prior weight on error standard deviations (\eqn{\sigma}) less than the rough estimate. Not applicable when \eqn{y} is binary. Defaults to \code{0.9}.
}
  \item{k}{
     For numeric \eqn{y}, \code{k} is the number of prior standard deviations \eqn{E(Y|x) = f(x)} is away from \eqn{\pm 0.5}{+/- 0.5}. The response (\code{y.train}) is internally scaled to range from \eqn{-0.5} to \eqn{0.5}. For binary \eqn{y}, \code{k} is the number of prior standard deviations \eqn{f(x)} is away from \eqn{\pm 3}{+/- 3}. In both cases, the bigger \eqn{k} is, the more conservative the fitting will be. The value can be either a fixed number, or the a \emph{hyperprior} of the form \code{chi(degreesOfFreedom = 1.25, scale = Inf)}. Defaults to \code{2}.
}
  \item{power}{
Power parameter for tree prior. Defaults to \code{2}.
}
  \item{base}{
Base parameter for tree prior.  Defaults to \code{0.95}.
}
  \item{binaryOffset}{
 Used for binary \eqn{y}. When present, the model is \eqn{P(Y = 1 \mid x) = \Phi(f(x) + \mathrm{binaryOffset})}{P(Y = 1 | x) = \Phi(f(x) + binaryOffset)}, allowing fits with probabilities shrunk towards values other than \eqn{0.5}.  Defaults to \code{0}.
}
  \item{ntree}{
The number of trees in the sum-of-trees formulation.  Defaults to \code{200}.
}
  \item{ndpost}{
The number of posterior draws after burn in, \code{ndpost / keepevery} will actually be returned.  Defaults to \code{1000}.
}
  \item{nskip}{
Number of MCMC iterations to be treated as burn in. Defaults to \code{100}.
}
  \item{printevery}{
As the MCMC runs, a message is printed every \code{printevery} draws. Defaults to \code{100}.
}
  \item{keepevery}{
Every \code{keepevery} draw is kept to be returned to the user. Useful for thinning samples. Defaults to \code{1}.
}
  \item{keeptrainfits}{
If \code{TRUE} the draws of \eqn{f(x)} for \eqn{x} corresponding to the rows of \code{x.train} are returned.  Defaults to \code{TRUE}.
}
  \item{usequants}{
When \code{TRUE}, determine tree decision rules using estimated quantiles derived from the \code{x.train} variables. When \code{FALSE}, splits are determined using values equally spaced across the range of a variable. See details for more information. Defaults to \code{TRUE}.
}
  \item{numcut}{
The maximum number of possible values used in decision rules (see \code{usequants}, details). If a single number, it is recycled for all variables; otherwise must be a vector of length equal to \code{ncol(x.train)}. Fewer rules may be used if a covariate lacks enough unique values. Defaults to \code{100}.
}
  \item{printcutoffs}{
The number of cutoff rules to printed to screen before the MCMC is run. Given a single integer, the same value will be used for all variables. If 0, nothing is printed. Defaults to \code{0}.
}
  \item{nthread}{
Integer specifying how many threads to use. Depending on the CPU architecture, using more than the number of chains can degrade performance for small/medium data sets. As such some calculations may be executed single threaded regardless. Defaults to \code{1}.
}
  \item{keepcall}{
Logical; if \code{FALSE}, returned object will have \code{call} set to \code{call("NULL")}, otherwise the call used to instantiate BART. Defaults to \code{TRUE}.
}
  \item{verb}{
Logical value indicating whether the underlying \code{bart} function is verbose; if \code{FALSE} supress printing. Defaults to \code{FALSE}.
}
  \item{verbose}{
A logical value indicating whether to print detailed information during the execution of the function. Defaults to \code{TRUE}.
}

  \item{\dots}{
Additional arguments currently unused.
}
}
  \value{
A list containing:
  \item{pred}{Predicted values from the fitted model, based on \code{newX} if provided.}
  \item{fit}{The fitted BART model.}
}
  \references{
Chipman, H., George, E., and McCulloch, R. (2009) BART: Bayesian Additive Regression Trees.

Chipman, H., George, E., and McCulloch R. (2006) Bayesian Ensemble Learning. Advances in
Neural Information Processing Systems 19, Scholkopf, Platt and Hoffman, Eds., MIT Press, Cambridge,
MA, 265-272.

Friedman, J.H. (1991) Multivariate adaptive regression splines. The Annals of Statistics, 19, 1-67.

The \code{dbarts} package: \url{https://cran.r-project.org/web/packages/dbarts/index.html}

The \code{SuperLearner} package: \url{https://cran.r-project.org/web/packages/SuperLearner/index.html}
}

  \author{
The wrapper is based on the wrapper from package \pkg{tmle}, writenn by Susan Gruber.

Extensions by Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr.
}


  \seealso{
\code{\link[dbarts]{dbarts}}, \code{\link[SuperLearner]{SuperLearner}}
}
  \examples{
library(SuperLearner)
data(EFV)
result1 <- SL.dbarts(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.dbarts(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.dbarts", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}
  \keyword{BART}
  \keyword{regression}
  \keyword{classification}
  \concept{Bayesian Additive Regression Trees}
  \concept{SuperLearner}