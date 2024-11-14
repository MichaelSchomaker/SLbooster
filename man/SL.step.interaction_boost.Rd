\name{SL.step.interaction_boost}
\alias{SL.step.interaction_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for stepwise model selection with information criteria
}
\description{
Wrapper fits generalized models including all possible interaction terms, and then performs model selection}
\usage{
SL.step.interaction_boost(Y, X, newX, family, direction = "both", trace = 0, k = 2, verbose = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
  A \code{numeric} vector of outcomes (dependent variable). For binary classification, it should be a binary factor or 0/1 values.
}
  \item{X}{
  A \code{data.frame} or \code{matrix} of predictor variables (independent variables).
}
  \item{newX}{
  A \code{data.frame} or \code{matrix} of new observations on which to generate predictions.
}
  \item{family}{
  A \code{\link[stats]{family}} object specifying the error distribution and link function to be used in the model, such as \code{gaussian} for continuous outcomes or \code{binomial} for binary outcomes.
}
  \item{direction}{
  A character string indicating the mode of stepwise search, with options \code{"both"}, \code{"forward"}, or \code{"backward"}. Default is \code{"both"}.
}
  \item{trace}{
  An \code{integer} controlling the amount of information printed during the model selection process. A value of 0 suppresses output.
}
  \item{k}{
  A numeric value specifying the penalty for the number of parameters in the model. The default is 2, which corresponds to the AIC criterion.
}
  \item{verbose}{
  A \code{logical} value. If \code{TRUE}, additional information about the function's progress is printed to the console.
}
  \item{\dots}{
  Additional arguments currently unused.
}
}
\details{
The \code{SL.step.interaction_boost} function first fits a generalized linear model (GLM) to the provided data using the specified family. It then applies the \code{step} function to perform stepwise model selection, including interaction terms (up to the second degree). 
To control the computational complexity, it is recommended to use this learner together with variable screening, e.g. \code{\link{screen.glmnet_nVar}}. 
}
\value{
A \code{list} containing:
\item{pred}{A \code{numeric} vector of predictions for the new observations in \code{newX}.}
\item{fit}{An object of class \code{"SL.step"} containing the fitted stepwise model.}
}
\references{
R Documentation for \code{step}: \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/step.html}
}
\author{
Wrapper from \pkg{SuperLearner} package, written by Eric Polley.  

Extensions by Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr.
}

\seealso{
\code{\link[stats]{step}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.step.interaction_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.step.interaction_boost(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.step.interaction_boost", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}
\keyword{regression}
\keyword{interaction}
\concept{Stepwise Selection}
\concept{Generalized Linear Models}
