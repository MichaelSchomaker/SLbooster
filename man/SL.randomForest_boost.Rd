\name{SL.randomForest_boost}
\alias{SL.randomForest_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Forest for SuperLearner
}
\description{
The \code{SL.randomForest_boost} function fits a Random Forest using the \code{randomForest} package and adds boosting for model improvement. If the fitting procedure fails, the mean of \code{Y} is provided as the prediction.
}
\usage{
SL.randomForest_boost(Y, X, newX = NULL, family = list(), verbose = T, mtry = ifelse(family$family == "gaussian", max(floor(ncol(X)/3), 1), floor(sqrt(ncol(X)))), ntree = 100, nodesize = ifelse(family$family == "gaussian", 5, 1), maxnodes = NULL, importance = FALSE, ...)
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
A data frame or matrix of new predictor data used for generating predictions. If not provided, the model will only be fitted on \code{X}. Defaults to \code{NULL}.
}
  \item{family}{
If \code{family = list(family = "binomial")}, classification is assumed, with \code{family = list(family = "gaussian")} regression is assumed. Defaults to \code{list()}. If no family is provided, no random forest is fitted.
}
  \item{verbose}{
A logical value indicating whether to print detailed information during the execution of the function. Defaults to \code{TRUE}.
}
  \item{mtry}{
Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in \code{X}) and regression (p/3).
}
  \item{ntree}{
Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times. Defaults to \code{100}.
}
  \item{nodesize}{
Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5).
}
  \item{maxnodes}{
Maximum number of terminal nodes that trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by \code{nodesize}). If set larger than maximum possible, a warning is issued. Defaults to \code{NULL}.
}
  \item{importance}{
Should importance of predictors be assessed? Defaults to \code{FALSE}.
}
  \item{\dots}{
Additional arguments passed to \code{randomForest::randomForest} or \code{randomForest.default}
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

The \code{SuperLearner} package: \url{https://cran.r-project.org/web/packages/SuperLearner/index.html}
}
\author{
Fortran original by Leo Breiman and Adele Cutler, R port by Andy Liaw and MatthewWiener.

Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
Ensure that \code{randomForest} is installed and loaded correctly, as this function relies on the Random Forest model implemented in the \code{randomForest} package.
}

\seealso{
\code{\link[randomForest]{randomForest}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Y, X, newX = NULL, family = list(), verbose = T, mtry = ifelse(family$family == 
    "gaussian", max(floor(ncol(X)/3), 1), floor(sqrt(ncol(X)))), 
    ntree = 100, nodesize = ifelse(family$family == "gaussian", 
        5, 1), maxnodes = NULL, importance = FALSE, ...) 
{
    if (verbose == T) {
        cat("SL.randomForest with ", ntree, " trees started. ", 
            sep = "")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("randomForest")
    if (all(apply(X, 2, var) == 0)) {
        fit.rf <- "Empty"
        attr(fit.rf, "class") <- "try-error"
        pred <- rep(mean(Y), nrow(Xnew))
        fit <- list(object = fit.rf)
    }
    if (family$family == "gaussian" & !exists("fit.rf")) {
        fit.rf <- randomForest::randomForest(Y ~ ., data = X, 
            ntree = ntree, xtest = newX, keep.forest = TRUE, 
            mtry = mtry, nodesize = nodesize, maxnodes = maxnodes, 
            importance = importance)
        try(pred <- fit.rf$test$predicted, silent = TRUE)
        if (any(class(fit.rf) == "try-error")) {
            pred <- rep(mean(Y), nrow(Xnew))
            if (verbose == T) {
                "Random forest failed: simply predicting mean of Y."
            }
        }
        fit <- list(object = fit.rf)
    }
    if (family$family == "binomial" & !exists("fit.rf")) {
        fit.rf <- randomForest::randomForest(y = as.factor(Y), 
            x = X, ntree = ntree, xtest = newX, keep.forest = TRUE, 
            mtry = mtry, nodesize = nodesize, maxnodes = maxnodes, 
            importance = importance)
        try(pred <- fit.rf$test$votes[, 2], silent = TRUE)
        if (any(class(fit.rf) == "try-error")) {
            pred <- rep(mean(Y), nrow(Xnew))
            if (verbose == T) {
                "Random forest failed: simply predicting mean of Y."
            }
        }
        fit <- list(object = fit.rf)
    }
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.randomForest")
    end_time <- Sys.time()
    if (verbose == T) {
        cat("SL.randomForest finished. Time:", round(difftime(end_time, 
            start_time, units = "mins"), digits = 4), "mins \n\n")
    }
    return(out)
  }
}
  \keyword{randomforest}
  \keyword{regression}
  \keyword{classification}
  \concept{Random Forest}
  \concept{SuperLearner}
