\name{screen.glmnet_nVar}
\alias{screen.glmnet_nVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable Screening Using Elastic Net and Cross-Validation in SuperLearner
}
\description{
The \code{screen.glmnet_nVar} function performs variable selection using elastic net regularization with cross-validation. It selects a user-specified number of variables (\code{nVar}) based on the deviance from the \code{glmnet} model, with options for both continuous and categorical data.
}
\usage{
screen.glmnet_nVar(Y, X, family = list(), alpha = 1, nfolds = 5, nlambda = 150, nVar = 6, verbose = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The outcome variable, typically a numeric vector for continuous outcomes or a binary factor for classification tasks.
}
  \item{X}{
A matrix or data frame of predictor variables. Each column represents a feature to be considered for selection.
}
  \item{family}{
A list indicating the type of model to be fitted. Typically, it would be \code{binomial()} for binary classification or \code{gaussian()} for continuous outcomes.
}
  \item{alpha}{
The elastic net mixing parameter. A value of \code{alpha = 1} corresponds to the LASSO penalty, and \code{alpha = 0} corresponds to ridge regression. Defaults to 1.
}
  \item{nfolds}{
The number of cross-validation folds to be used in \code{cv.glmnet}. Defaults to 5.
}
  \item{nlambda}{
The number of lambda values used in the \code{glmnet} path. Higher values can provide a finer search for optimal lambda. Defaults to 150.
}
  \item{nVar}{
The desired number of variables to be selected. The function will attempt to select the specified number of variables, adjusting if necessary. Defaults to 6.
}
  \item{verbose}{
A logical value indicating whether to print progress messages during execution. Defaults to \code{TRUE}.
}
  \item{\dots}{
Additional arguments passed to the underlying \code{cv.glmnet} function for fine-tuning.
}
}
\details{
The \code{screen.glmnet_nVar} function uses elastic net regularization to perform variable selection. It performs \code{nfolds}-fold cross-validation to find the optimal lambda and selects \code{nVar} variables based on the deviance from the cross-validated model. If no variables are selected, it will adjust lambda values or use Cramer's V for screening categorical variables.
}
\value{
A logical vector indicating which variables in \code{X} were selected (\code{TRUE}) and which were not (\code{FALSE}).
}
\references{
Friedman J, Hastie T, Tibshirani R (2010). \emph{Regularization Paths for Generalized Linear Models via Coordinate Descent}. Journal of Statistical Software, 33(1), 1-22.
The \code{glmnet} package documentation: \url{https://cran.r-project.org/web/packages/glmnet/index.html}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
This function requires the \code{glmnet} package to be installed and loaded. It is designed to screen variables using elastic net regularization and cross-validation within the \code{SuperLearner} framework.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SuperLearner}}, \code{\link{cv.glmnet}}, \code{\link{glmnet}}
}
\examples{
## Example usage of screen.glmnet_nVar:
library(SuperLearner)
library(glmnet)

# Generate example data
set.seed(123)
n <- 100
X <- data.frame(X1 = rnorm(n), X2 = rnorm(n), X3 = sample(letters[1:3], n, replace = TRUE))
Y <- rbinom(n, 1, 0.5)

# Run the variable screening
selected_vars <- screen.glmnet_nVar(Y = Y, X = X, nVar = 2)

# Check the selected variables
print(selected_vars)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{variable selection}
\keyword{SuperLearner}
\concept{elastic net}
\concept{cross-validation}
