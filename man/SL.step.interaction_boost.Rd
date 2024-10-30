\name{SL.step.interaction_boost}
\alias{SL.step.interaction_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SL.step.interaction_boost: Stepwise Regression with Interaction Terms for SuperLearner
}
\description{
The \code{SL.step.interaction_boost} function is a wrapper for performing stepwise regression with interaction terms within the \code{SuperLearner} framework. This function fits a generalized linear model (GLM) to the data, applies stepwise selection of main effects and interaction terms, and generates predictions for new data.
}
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
The \code{SL.step.interaction_boost} function first fits a generalized linear model (GLM) to the provided data using the specified family. It then applies the \code{step} function to perform stepwise model selection, including interaction terms (up to the second degree). The direction of stepwise selection can be controlled using the \code{direction} argument. The final model is used to generate predictions on the new data (\code{newX}). This approach allows for flexible modeling of complex relationships in the data by considering both main effects and interactions.
}
\value{
A \code{list} containing:
\item{pred}{A \code{numeric} vector of predictions for the new observations in \code{newX}.}
\item{fit}{An object of class \code{"SL.step"} containing the fitted stepwise model.}
}
\references{
R Documentation for \code{glm}: \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html}

R Documentation for \code{step}: \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/step.html}

SuperLearner Package: \url{https://cran.r-project.org/package=SuperLearner}
}
\author{
  Original authors of the \code{glm} and \code{step} package: see reference above.

  Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
The \code{SL.step.interaction_boost} function requires the \code{stats} package, which is included with base R.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats]{glm}}, \code{\link[stats]{step}}, \code{\link[SuperLearner]{SuperLearner}}
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
\keyword{SuperLearner}
\concept{Stepwise Selection}
\concept{Generalized Linear Models}
