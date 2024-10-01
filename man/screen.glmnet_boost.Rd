\name{screen.glmnet_boost}
\alias{screen.glmnet_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable Screening using Elastic Net Regularization with Cross-Validation
}
\description{
Performs variable selection using the Elastic Net regularization via \code{glmnet} with cross-validation. If Elastic Net fails to select any variables, the function falls back on Cramer's V for screening.

}
\usage{
screen.glmnet_boost(Y, X, family = list(), alpha = 1, verbose = T, nfolds = 10, nlambda = 200, nscreen = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
    Response variable, either a numeric vector for regression or a binary vector (0/1) for classification.
  }
  \item{X}{
    Predictor variables in a data frame or matrix format. Factors are internally handled by creating dummy variables.
  }
  \item{family}{
    A list specifying the error distribution and link function to be used in the model. Default is \code{list()} and the family is determined based on the response variable.
  }
  \item{alpha}{
    Elastic Net mixing parameter, with \code{alpha = 1} being the Lasso (default) and \code{alpha = 0} being the Ridge regression.
  }
  \item{verbose}{
    Logical. If \code{TRUE}, prints detailed information during the execution. Default is \code{TRUE}.
  }
  \item{nfolds}{
    Number of folds used in cross-validation to select the best model. Default is 10.
  }
  \item{nlambda}{
    Number of lambda values considered during the cross-validation. Default is 200.
  }
  \item{nscreen}{
    Number of top variables to select if the fallback screening method (Cramer's V) is used. Default is 2.
  }
  \item{\dots}{
    Additional arguments passed to \code{glmnet::cv.glmnet}.
  }
}
\details{
The function first attempts variable selection using Elastic Net regularization via \code{glmnet}. If the model fails to select any variables (due to overly strong penalization), a fallback method based on Cramer's V is used to screen variables. The function accommodates factors by converting them into dummy variables and adjusts for variable selection by considering factor levels as a group.
}
\value{
Returns a logical vector indicating which variables are selected by the screening process. If a variable is selected, the corresponding position in the vector is \code{TRUE}.
}
\references{
\code{glmnet}: Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22.
}
\author{
Michael Schomaker, Han Bao
}
\note{
Use this function when performing feature selection in the context of regression or binary classification tasks.
}
\seealso{
\code{\link[glmnet]{cv.glmnet}}, \code{\link{screen.cramersv}}
}
\examples{
# Example usage:

# Generating sample data
set.seed(123)
n <- 100
p <- 10
X <- data.frame(matrix(rnorm(n * p), nrow = n))
Y <- rnorm(n)

# Running the screen.glmnet_boost function
selected_vars <- screen.glmnet_boost(Y, X, family = stats::gaussian())

# Display selected variables
print(selected_vars)
}
\keyword{variable selection}
\keyword{elastic net}
\keyword{cross-validation}