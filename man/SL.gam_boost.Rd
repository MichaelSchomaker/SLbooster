\name{SL.gam_boost}
\alias{SL.gam_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generalized Additive Model (GAM) with Boosting for SuperLearner 
}
\description{
The \code{SL.gam_boost} function fits a Generalized Additive Model (GAM) using the \code{gam} package and adds boosting for model improvement. It automatically adjusts for metric and categorical predictors and applies smoothing splines to variables with more than \code{cts.num} unique values.
}
\usage{
SL.gam_boost(Y, X, newX, family, obsWeights = NULL, df.gam = 2, cts.num = 10, verbose = T, ...)
}
 \arguments{
  \item{Y}{
The response variable. This is typically a numeric vector or factor representing the outcome of interest.
}
  \item{X}{
A matrix or data frame of predictor variables. Each column represents a feature used in model fitting.
}
  \item{newX}{
A matrix or data frame of new predictor values for generating predictions.
}
  \item{family}{
A description of the error distribution and link function to be used in the model (e.g., \code{binomial()} or \code{gaussian()}). It is passed to the \code{gam} function for model fitting.
}
  \item{obsWeights}{
An optional vector of observation-specific weights to be used in the fitting process. It is passed to the \code{gam} function.
}
  \item{df.gam}{
Degrees of freedom for the smoothing splines applied to the metric variables in the GAM model. The default is 2.
}
  \item{cts.num}{
The minimum number of unique values a variable must have for it to be considered a continuous (metric) variable and have a spline added. The default is 10.
}
  \item{verbose}{
Logical flag indicating whether to print progress and timing information. The default is \code{TRUE}.
}
  \item{\dots}{
Additional arguments currently unused.
}
}
\details{
The \code{SL.gam_boost} function automatically determines whether each predictor in \code{X} should be treated as a categorical or metric variable based on the number of unique values. If a predictor has more than \code{cts.num} unique values, it is treated as metric, and a smoothing spline is added using the \code{s()} function from the \code{gam} package. For categorical variables, no splines are applied. If the model fails to converge, an intercept-only model is fitted as a fallback.
}
\value{
A list containing:
  \item{pred}{Predicted values for the observations in \code{newX}.}
  \item{fit}{The fitted GAM model object, which can be used for further examination or diagnostics.}
}
\references{
Hastie T., Tibshirani R. (1990). \emph{Generalized Additive Models}. Chapman & Hall.
The \code{gam} package documentation: \url{https://cran.r-project.org/web/packages/gam/index.html}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
This function requires the \code{gam} package to be installed and loaded. It uses smoothing splines for continuous variables with more than a specified number of unique values.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SuperLearner}}, \code{\link{gam}}, \code{\link{SL.mean}}, \code{\link{SL.glm}}
}
\examples{
## Example usage of SL.gam_boost:
library(SuperLearner)
library(gam)
data(EFV)
result1 <- SL.gam_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                            newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.gam_boost(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                            newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.gam_boost", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{regression}
\keyword{SuperLearner}
\concept{generalized additive models}
\concept{boosting}
