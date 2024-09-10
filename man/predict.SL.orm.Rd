\name{predict.SL.orm}
\alias{predict.SL.orm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict Method for SL.orm
}
\description{
This function generates predictions for new data based on a model fitted by \code{SL.orm}. It uses the \code{predict.orm} function from the \code{rms} package to return the expected means for ordinal outcomes.
}
\usage{
\method{predict}{SL.orm}(object, newdata, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
The fitted model object returned by \code{SL.orm}, containing the ordinal regression model or an alternative model (e.g., GLM or mean) depending on the original data.
}
  \item{newdata}{
A data frame or matrix of new predictor values for which predictions are to be made. The function automatically converts a matrix to a data frame if necessary.
}
  \item{\dots}{
Additional arguments passed to the underlying prediction function from the \code{rms} package, such as options related to the type of prediction.
}
}
\details{
The \code{predict.SL.orm} function works specifically with objects created by the \code{SL.orm} function. It uses the \code{predict.orm} method from the \code{rms} package to predict the expected mean of ordinal outcomes based on the new data provided. If the \code{SL.orm} model used a GLM or a mean-based model instead of an ordinal regression, the predictions will reflect that.
}
\value{
A vector of predicted values representing the expected means of the ordinal categories for each observation in \code{newdata}.
}
\references{
Harrell FE Jr. (2023). \emph{Regression Modeling Strategies}. Springer Series in Statistics.
The \code{rms} package: \url{https://cran.r-project.org/web/packages/rms/index.html}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
The function requires the \code{rms} package to be installed and loaded. It assumes that the \code{object} argument contains a valid ordinal regression model from the \code{SL.orm} function.
}
\seealso{
\code{\link{SL.orm}}, \code{\link{predict.orm}}, \code{\link{SuperLearner}}
}
\examples{
## Example usage of predict.SL.orm:
library(SuperLearner)
library(rms)

# Generate example data
set.seed(123)
n <- 100
X <- data.frame(X1 = rnorm(n), X2 = rnorm(n))
Y <- ordered(sample(c('low', 'medium', 'high'), n, replace = TRUE))

# Fit an SL.orm model
fit <- SL.orm(Y = Y, X = X, newX = NULL)

# Make predictions on new data
pred <- predict.SL.orm(fit, newdata = X)
print(pred)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{prediction}
% \keyword{regression}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ordinal regression}
% \concept{ensemble learning}
