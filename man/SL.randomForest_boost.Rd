\name{SL.randomForest_boost}
\alias{SL.randomForest_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.randomForest_boost(Y, X, newX = NULL, family = list(), verbose = T, mtry = ifelse(family$family == "gaussian", max(floor(ncol(X)/3), 1), floor(sqrt(ncol(X)))), ntree = 100, nodesize = ifelse(family$family == "gaussian", 5, 1), maxnodes = NULL, importance = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{newX}{
%%     ~~Describe \code{newX} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{mtry}{
%%     ~~Describe \code{mtry} here~~
}
  \item{ntree}{
%%     ~~Describe \code{ntree} here~~
}
  \item{nodesize}{
%%     ~~Describe \code{nodesize} here~~
}
  \item{maxnodes}{
%%     ~~Describe \code{maxnodes} here~~
}
  \item{importance}{
%%     ~~Describe \code{importance} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
