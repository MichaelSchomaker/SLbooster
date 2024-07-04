\name{screen.randomForest_boost}
\alias{screen.randomForest_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
screen.randomForest_boost(Y, X, family = list(), nVar = 8, ntree = 200, verbose = T, mtry = ifelse(family$family == "gaussian", floor(sqrt(ncol(X))), max(floor(ncol(X)/3), 1)), nodesize = ifelse(family$family == "gaussian", 5, 1), maxnodes = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{nVar}{
%%     ~~Describe \code{nVar} here~~
}
  \item{ntree}{
%%     ~~Describe \code{ntree} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{mtry}{
%%     ~~Describe \code{mtry} here~~
}
  \item{nodesize}{
%%     ~~Describe \code{nodesize} here~~
}
  \item{maxnodes}{
%%     ~~Describe \code{maxnodes} here~~
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
function (Y, X, family = list(), nVar = 8, ntree = 200, verbose = T, 
    mtry = ifelse(family$family == "gaussian", floor(sqrt(ncol(X))), 
        max(floor(ncol(X)/3), 1)), nodesize = ifelse(family$family == 
        "gaussian", 5, 1), maxnodes = NULL, ...) 
{
    if (verbose == T) {
        cat("screen.randomForest with ntree=", ntree, ", mtry=", 
            mtry, " and selecting ", nVar, " variables \n", sep = "")
    }
    if (nVar < 1) {
        stop("nVar needs to be greater or equal to one")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("randomForest")
    Y <- as.vector(as.matrix(Y))
    if (all(Y == 0 | Y == 1)) {
        family$family <- "binomial"
    }
    else {
        family$family <- "gaussian"
    }
    if (ncol(X) > nVar) 
        try({
            t_ime <- {
                if (family$family == "gaussian") {
                  rank.rf.fit <- randomForest::randomForest(Y ~ 
                    ., data = X, ntree = ntree, mtry = mtry, 
                    nodesize = nodesize, keep.forest = FALSE, 
                    maxnodes = maxnodes, importance = TRUE)
                  imp_measure <- rank.rf.fit$importance[, "\%IncMSE"]
                  whichVariable <- (rank(-imp_measure, ties.method = "random") <= 
                    nVar)
                }
                if (family$family == "binomial") {
                  rank.rf.fit <- randomForest::randomForest(as.factor(Y) ~ 
                    ., data = X, ntree = ntree, mtry = mtry, 
                    nodesize = nodesize, keep.forest = FALSE, 
                    maxnodes = maxnodes, importance = TRUE)
                  imp_measure <- rank.rf.fit$importance[, "MeanDecreaseAccuracy"]
                  whichVariable <- (rank(-imp_measure, ties.method = "random") <= 
                    nVar)
                }
            }
        })
    else {
        stop("number of variables to select is greater than number of columns")
    }
    if (class(rank.rf.fit)[1] == "try-error") {
        whichVariable <- screen.cramersv_base(Y, X)
        if (verbose == T) {
            cat("Random forest failed and screening was based on Cramer's V\n")
        }
    }
    if (verbose == T) {
        cat("screened ", sum(whichVariable), " variables: ", 
            paste(colnames(X)[whichVariable]), "\n", sep = " ")
        cat("sample size:", dim(X)[1], "; ")
    }
    end_time <- Sys.time()
    if (verbose == T) {
        cat("time:", round(difftime(end_time, start_time, units = "secs"), 
            digits = 4), "secs \n")
    }
    return(whichVariable)
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
