\name{SL.randomForest_boost}
\alias{SL.randomForest_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for randomForest
}
\description{
Wrapper for random forests using the package \pkg{randomForest}.}
\usage{
SL.randomForest_boost(Y, X, newX, family, verbose = T,
mtry = ifelse(family$family == "gaussian", max(floor(ncol(X)/3), 1), floor(sqrt(ncol(X)))),
            ntree = 100, nodesize = ifelse(family$family == "gaussian", 5, 1), 
            maxnodes = ifelse(nrow(X)<1000,200,300), importance = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A response vector. 
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{newX}{
A data frame or matrix of new predictor data used for generating predictions.  
}
  \item{family}{
String. Either "binomial" or "gaussian". 
}
  \item{verbose}{
Logical. Indicates whether additional information and timing is printed. The default is \code{TRUE}.
}
  \item{mtry}{
Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in \code{X}) and regression (p/3).
}
  \item{ntree}{
Number of trees to grow. 
}
  \item{nodesize}{
Minimum size of terminal nodes. Setting this number to larger values causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5).
}
  \item{maxnodes}{
Maximum number of terminal nodes that trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by \code{nodesize}). If set larger than maximum possible, a warning is issued. Defaults to \code{NULL}.
}
  \item{importance}{
Should importance of predictors be assessed? Defaults to \code{FALSE}.
}
  \item{\dots}{
Additional arguments.
}
}
\value{
A list containing:
  \item{pred}{Predicted values from the fitted model, based on \code{newX} if provided.}
  \item{fit}{The fitted Random Forest model.}
}
\references{
Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32.

Breiman, L (2002), ``Manual On Setting Up, Using, And Understanding Random Forests V3.1'', https://www.stat.berkeley.edu/~breiman/Using_random_forests_V3.1.pdf.

The \code{randomForest} package: \url{https://cran.r-project.org/web/packages/randomForest/index.html}
}
\author{
Wrapper from \pkg{SuperLearner} package, written by Eric Polley.  

Extensions by Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
If the fitting procedure fails, the mean of \code{Y} is provided as the prediction.
}

\seealso{
\code{\link[randomForest]{randomForest}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.randomForest_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.randomForest_boost(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.randomForest_boost", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}
  \keyword{randomforest}
  \keyword{regression}
  \keyword{classification}
  \concept{Random Forest}
