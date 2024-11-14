\name{SL.rpart_boost}
\alias{SL.rpart_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for recursive partitioning
}
\description{
Wrapper for recursive partitioning using the \code{rpart} package 
}
\usage{
SL.rpart_boost(Y, X, newX, family, obsWeights = NULL, cp = 0.01, minsplit = 20, xval = 0L, maxdepth = 30, minbucket = round(minsplit/3), verbose = T, ...)
}
\arguments{
  \item{Y}{
A response vector. 
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{newX}{
A data frame or matrix of new predictor data used for generating predictions. If not provided, the model will only be fitted on \code{X}.
}
  \item{family}{
A \code{list} describing how the fit and the prediction should be calculated. For numeric \code{Y}, use \code{family = list(family = "gaussian")}; for binary classification, use \code{family = list(family = "binomial")}.
}
  \item{obsWeights}{
An optional vector of weights to be used in the fitting process. Defaults to \code{NULL}.
}
  \item{cp}{
The complexity parameter. Any split that does not decrease the overall lack of fit by
a factor of cp is not attempted. This means
that the overall R-squared must increase by cp at each step. The main role of
this parameter is to save computing time by pruning off splits that are obviously
not worthwhile. Essentially,the user informs the program that any split which
does not improve the fit by cp will likely be pruned off by cross-validation, and
that hence the program need not pursue it. Defaults to \code{0.1}.
}
  \item{minsplit}{
The minimum number of observations that must exist in a node in order for a
split to be attempted. Defaults to \code{20}.
}
  \item{xval}{
Number of cross-validations. Defaults to \code{0L}.
}
  \item{maxdepth}{
Set the maximum depth of any node of the final tree, with the root node counted
as depth 0. For values greater than 30, rpart will give nonsense results on 32-bit
machines. Defaults to \code{30}.
}
  \item{minbucket}{
The minimum number of observations in any terminal leaf node. If only
one of \code{minbucket} or \code{minsplit} is specified, the code either sets \code{minsplit} to
\code{minbucket*3} or \code{minbucket} to \code{minsplit/3}, as appropriate. Defaults to \code{minsplit/3}.
}
  \item{verbose}{
A logical value indicating whether to print detailed information during the execution of the function. Defaults to \code{TRUE}.
}
  \item{\dots}{
Additional arguments currently unused.
}
}
\details{
If family is set as \code{family = list(family = "binomial")}, but \code{Y} consists not only of 0 and 1, the Gaussian family will be used for prediction, but predictions falling outside [0,1] will be set as 0/1.
}
\value{
A list containing:
  \item{pred}{Predicted values from the fitted model, based on \code{newX} if provided.}
  \item{fit}{The fitted Tree model.}
}
\references{
Breiman L., Friedman J. H., Olshen R. A., and Stone, C. J. (1984) Classification and Regression
Trees. Wadsworth.

The \code{rpart} package: \url{https://cran.r-project.org/web/packages/rpart/index.html}
}
\author{
Wrapper from \pkg{SuperLearner} package, written by Eric Polley.  

Extensions by Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr.
}
\note{
Ensure that \code{rpart} is installed and loaded correctly, as this function relies on the Tree model implemented in the \code{rpart} package.
}

\seealso{
\code{\link[rpart]{rpart}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.rpart_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.rpart_boost(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.rpart_boost", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}
  \keyword{rpart}
  \keyword{regression}
  \keyword{classification}
  \concept{Tree Models}
