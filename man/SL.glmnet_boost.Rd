\name{SL.glmnet_boost}
\alias{SL.glmnet_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.glmnet_boost(Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, nlambda = 100, useMin = TRUE, loss = "deviance", verbose = T, ...)
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
  \item{obsWeights}{
%%     ~~Describe \code{obsWeights} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{nfolds}{
%%     ~~Describe \code{nfolds} here~~
}
  \item{nlambda}{
%%     ~~Describe \code{nlambda} here~~
}
  \item{useMin}{
%%     ~~Describe \code{useMin} here~~
}
  \item{loss}{
%%     ~~Describe \code{loss} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, 
    nlambda = 100, useMin = TRUE, loss = "deviance", verbose = T, 
    ...) 
{
    if (verbose == T) {
        cat("SL.glmnet started with alpha=", alpha, ", ", nfolds, 
            "-fold CV, and ", nlambda, " candidate lambdas. ", 
            sep = "")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("glmnet")
    fam.init <- family$family
    Y <- as.vector(as.matrix(Y))
    if (all(Y == 0 | Y == 1)) {
        family$family <- "binomial"
    }
    else {
        family$family <- "gaussian"
    }
    fam.end <- family$family
    if (!is.matrix(X)) {
        X <- model.matrix(~-1 + ., X)
        newX <- model.matrix(~-1 + ., newX)
    }
    fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights, 
        lambda = NULL, type.measure = loss, nfolds = nfolds, 
        family = family$family, alpha = alpha, nlambda = nlambda, 
        ...)
    pred <- predict(fitCV, newx = newX, type = "response", s = ifelse(useMin, 
        "lambda.min", "lambda.1se"))
    if (fam.init == "binomial" & fam.end == "gaussian") {
        if (any(pred < 0)) {
            pred[pred < 0] <- 0
        }
        if (any(pred > 1)) {
            pred[pred > 1] <- 1
        }
        if (verbose == T) {
            cat("Note: predictions falling outside [0,1] have been set as 0/1")
        }
    }
    fit <- list(object = fitCV, useMin = useMin)
    class(fit) <- "SL.glmnet"
    out <- list(pred = pred, fit = fit)
    end_time <- Sys.time()
    if (verbose == T) {
        cat("SL.glmnet finished. Time:", round(difftime(end_time, 
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
