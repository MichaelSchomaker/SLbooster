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
SL.earth_boost(Y, X, newX = NULL, family = list(), obsWeights = NULL, id = NULL, degree = 2, penalty = 3, 
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
  Additional arguments passed to \code{earth::earth()}.
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
Michael Schomaker, Han Bao
}
\note{
Ensure that \code{earth} is installed and loaded correctly, as this function relies on the MARS model implemented in the \code{earth} package.
}

\seealso{
\code{\link[earth]{earth}}, \code{\link[SuperLearner]{SuperLearner}}, \code{\link[SuperLearner]{SL.glm}}
}
\examples{
# Generate synthetic data
set.seed(123)
n <- 200 # Number of observations
p <- 10  # Number of predictors
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)
Y <- 3 * X[, 1] - 2 * X[, 2] + rnorm(n) # Generate response variable

# Split the data into training and testing sets
train_indices <- sample(1:n, size = 0.7 * n) # 70% for training
test_indices <- setdiff(1:n, train_indices) # Remaining 30% for testing

# Create training and testing datasets
X_train <- X[train_indices, ]
Y_train <- Y[train_indices]
X_test <- X[test_indices, ]

# Fit the model using SL.earth_boost
fit_result <- SL.earth_boost(Y = Y_train, X = X_train, newX = X_test, family = list(family = "gaussian"))

# Print predictions
print(fit_result$pred)
}
\keyword{MARS}
\keyword{regression}
\keyword{classification}
\concept{Multivariate Adaptive Regression Splines}
\concept{SuperLearner}
