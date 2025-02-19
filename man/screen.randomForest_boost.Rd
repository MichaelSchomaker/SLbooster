\name{screen.randomForest_boost}
\alias{screen.randomForest_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable Screening using Random Forests
}
\description{
Performs variable selection by identifying the most important predictor variables based on their contribution to predictive accuracy, using a random forest model. 
}
\usage{
screen.randomForest_boost(Y, X, family = list(), nVar = 6, ntree = 200, verbose = T,
                          mtry = ifelse(family$family == "gaussian", floor(sqrt(ncol(X))), max(floor(ncol(X)/3), 1)), nodesize = ifelse(family$family == "gaussian", 5, 1), maxnodes = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A response vector. 
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{family}{
Either \code{family = list(family = "binomial")} or \code{family = list(family = "gaussian")}. The family is internally inferred from the data, so this argument only influences the default values of \code{mtry} and \code{nodesize}.
}
  \item{nVar}{
How many variables should be selected. Default is \code{6}.
}
  \item{ntree}{
Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times. Default is \code{200}.
}
  \item{verbose}{
A logical value indicating whether to print detailed information during the execution of the function. Default is \code{TRUE}.
}
  \item{mtry}{
Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in \code{X}) and regression (p/3).
}
  \item{nodesize}{
Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for binary classification (1) and regression (5).
}
  \item{maxnodes}{
Maximum number of terminal nodes that trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by \code{nodesize}). If set larger than maximum possible, a warning is issued. Defaults to \code{NULL}.
}
  \item{\dots}{
Additional arguments passed to \code{randomForest::randomForest} or \code{randomForest.default}
}
}
\value{
Returns a logical vector indicating which variables are selected by the screening process. If a variable is selected, the corresponding position in the vector is TRUE.
}
\references{
Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32.

Breiman, L (2002), ``Manual On Setting Up, Using, And Understanding Random Forests V3.1'', https://www.stat.berkeley.edu/~breiman/Using_random_forests_V3.1.pdf.

The \code{randomForest} package: \url{https://cran.r-project.org/web/packages/randomForest/index.html}
}
\author{
Screening algorithm from \pkg{SuperLearner} package, written by Eric Polley.  

Extensions by Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}


\seealso{
\code{\link[randomForest]{randomForest}}
}
\examples{
data(EFV)
result1 <- screen.randomForest_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")],
                                     family = stats::binomial(), nVar=2)
result2 <- screen.randomForest_boost(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                     family = stats::gaussian(), nVar=2)
print(result1)
}
  \keyword{randomforest}
  \keyword{regression}
  \keyword{classification}
  \keyword{screening}
  \concept{Random Forest}
  \concept{SuperLearner}
  \concept{Variable Selection}
