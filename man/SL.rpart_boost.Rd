\name{SL.rpart_boost}
\alias{SL.rpart_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recursive Partitioning
}
\description{
Using the \code{rpart} package, fits a decision tree model using ANOVA for both regression and classification tasks, handles binary and continuous outcomes, and provides options for cross-validation and tree depth control. 
}
\usage{
SL.rpart_boost(Y, X, newX = NULL, family = list(), obsWeights, cp = 0.01, minsplit = 20, xval = 0L, maxdepth = 30, minbucket = round(minsplit/3), verbose = T, ...)
}
\arguments{
  \item{Y}{
A response vector. 
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{newX}{
A data frame or matrix of new predictor data used for generating predictions. If not provided, the model will only be fitted on \code{X}. Defaults to \code{NULL}.
}
  \item{family}{
A \code{list} describing how the prediction should be calculated. For numeric \code{Y}, use \code{family = list(family = "gaussian")}; for binary classification, use \code{family = list(family = "binomial")}.  Defaults to \code{list()} and is inferred from \code{Y} if not provided.
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
Additional arguments passed to \code{rpart::rpart()} or \code{rpart.control}.
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

The \code{SuperLearner} package: \url{https://cran.r-project.org/web/packages/SuperLearner/index.html}
}
\author{
rpart package:
Terry Therneau,
Beth Atkinson,
Brian Ripley

Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
Ensure that \code{rpart} is installed and loaded correctly, as this function relies on the Tree model implemented in the \code{rpart} package.
}

\seealso{
\code{\link[rpart]{rpart}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Y, X, newX, family, obsWeights, cp = 0.01, minsplit = 20, 
    xval = 0L, maxdepth = 30, minbucket = round(minsplit/3), 
    verbose = T, ...) 
{
    if (verbose == T) {
        cat("SL.rpart started with minsplit=", minsplit, ", maxdepth=", 
            maxdepth, ", and ", xval, "-fold CV (0=no CV). ", 
            sep = "")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("rpart")
    fam.init <- family$family
    Y <- as.vector(as.matrix(Y))
    if (all(Y == 0 | Y == 1)) {
        family$family <- "binomial"
    }
    else {
        family$family <- "gaussian"
    }
    fam.end <- family$family
    if (family$family == "gaussian") {
        fit.rpart <- rpart::rpart(Y ~ ., data = data.frame(Y, 
            X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, 
            xval = xval, maxdepth = maxdepth, minbucket = minbucket), 
            method = "anova", weights = obsWeights)
        pred <- predict(fit.rpart, newdata = newX)
    }
    if (family$family == "binomial") {
        fit.rpart <- rpart::rpart(Y ~ ., data = data.frame(Y, 
            X), control = rpart::rpart.control(cp = cp, minsplit = minsplit, 
            xval = xval, maxdepth = maxdepth, minbucket = minbucket), 
            method = "class", weights = obsWeights)
        pred <- predict(fit.rpart, newdata = newX)[, 2]
    }
    if (fam.init == "binomial" & fam.end == "gaussian") {
        if (any(pred < 0)) {
            pred[pred < 0] <- 0
        }
        if (any(pred > 1)) {
            pred[pred > 1] <- 1
        }
    }
    fit <- list(object = fit.rpart)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.rpart")
    end_time <- Sys.time()
    if (verbose == T) {
        cat("SL.rpart finished. Time:", round(difftime(end_time, 
            start_time, units = "mins"), digits = 4), "mins \n\n")
    }
    return(out)
  }
}
  \keyword{rpart}
  \keyword{regression}
  \keyword{classification}
  \concept{Tree Models}
  \concept{SuperLearner}
