\name{screen.cramersv}
\alias{screen.cramersv}
\title{
Screening using Cramer's V
}
\description{
The \code{screen.cramersv} screening algorithm selects a subset of variables from \code{X} based on their association with the outcome \code{Y} using Cramer's V statistic (=standardized chi-square-statistic). 
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
The function calculates Cramer's V statistic between each predictor in \code{X} and the response variable \code{Y}. Continuous variables are first discretized into categories using quintiles. The top \code{nscreen} variables with the highest Cramer's V statistic are selected.
}
\value{
\code{whichVariable} A logical vector with the length equal to the number of columns in \code{X}. \code{TRUE} indicates the variable (column of \code{X}) should be included.
}
\references{
Heumann, C., M. Schomaker, and Shalabh, Introduction to Statistics and Data Analysis - With Exercises, Solutions and Applications in R Second updated and extended edition ed. 2023, Cham: Springer.

The \code{vcd} package documentation: \url{https://cran.r-project.org/web/packages/vcd/index.html}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
This is a particularly stable screening algorithm, that can be used as an escape strategy if other -statistically more advanced- screening algorithms fail.
}
\seealso{
\code{\link{screen.glmnet_nVar}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- screen.cramersv(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")],
                           family = stats::binomial(), nscreen = 2)
result2 <- screen.cramersv(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                           family = stats::gaussian(), nscreen = 2)
print(result1)
}
\keyword{Cramer's V}
\keyword{screening}
\concept{categorical data analysis}


