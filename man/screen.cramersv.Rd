\name{screen.cramersv}
\alias{screen.cramersv}
\title{
Screening using Cramer's V
}
\description{
The \code{screen.cramersv} function selects a subset of variables from \code{X} based on their association with the outcome \code{Y} using Cramer's V statistic. Continuous variables are discretized before calculating Cramer's V. The function allows selecting the top \code{nscreen} variables with the highest Cramer's V values.
}
\usage{
screen.cramersv(Y, X, nscreen = 4, num_cat = 10, verbose = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The response variable, typically a categorical variable or factor. It represents the outcome of interest.
}
  \item{X}{
A data frame or matrix of predictor variables. Each column represents a potential feature for screening.
}
  \item{nscreen}{
The number of variables to screen and retain based on the highest Cramer's V values. Defaults to 4.
}
  \item{num_cat}{
The threshold for the number of unique values a variable must have to be considered continuous. Variables with more unique values than \code{num_cat} are discretized into quantiles. Defaults to 10.
}
  \item{verbose}{
Logical indicating whether to print progress messages and timing information. Defaults to \code{TRUE}.
}
  \item{\dots}{
Additional arguments passed to the function, currently unused.
}
}
\details{
The function calculates Cramer's V statistic between each predictor in \code{X} and the response variable \code{Y}. Continuous variables are first discretized into categories using quantiles, ensuring they can be handled by Cramer's V. The top \code{nscreen} variables with the highest Cramer's V are selected, and the function returns a logical vector indicating which variables are retained.
}
\value{
\code{whichVariable} A logical vector with the length equal to the number of columns in \code{X}. \code{TRUE} indicates the variable (column of \code{X}) should be included.
}
\references{
Agresti A (2007). \emph{An Introduction to Categorical Data Analysis}. Wiley.

The \code{vcd} package documentation: \url{https://cran.r-project.org/web/packages/vcd/index.html}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
This function is particularly useful in situations where the relationship between categorical variables needs to be explored. It works with both categorical and continuous variables by first converting continuous variables to categories.
}
\seealso{
\code{\link{screen.glmnet_nVar}}, \code{\link{SuperLearner}}
}
\examples{
# Example usage of screen.cramersv:
library(SuperLearner)
library(vcd)

# Generate example data
set.seed(123)
n <- 100
X <- data.frame(X1 = rnorm(n), X2 = sample(letters[1:3], n, replace = TRUE), X3 = rnorm(n))
Y <- sample(letters[1:2], n, replace = TRUE)

# Run the screening function
selected_vars <- screen.cramersv(Y = Y, X = X, nscreen = 2)

# Check which variables were selected
print(selected_vars)
}
\keyword{Cramer's V}
\keyword{screening}
\concept{categorical data analysis}


