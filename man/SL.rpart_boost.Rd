\name{SL.rpart_boost}
\alias{SL.rpart_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.rpart_boost(Y, X, newX, family, obsWeights, cp = 0.01, minsplit = 20, xval = 0L, maxdepth = 30, minbucket = round(minsplit/3), verbose = T, ...)
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
  \item{cp}{
%%     ~~Describe \code{cp} here~~
}
  \item{minsplit}{
%%     ~~Describe \code{minsplit} here~~
}
  \item{xval}{
%%     ~~Describe \code{xval} here~~
}
  \item{maxdepth}{
%%     ~~Describe \code{maxdepth} here~~
}
  \item{minbucket}{
%%     ~~Describe \code{minbucket} here~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
