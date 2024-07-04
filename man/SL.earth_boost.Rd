\name{SL.earth_boost}
\alias{SL.earth_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.earth_boost(Y, X, newX = NULL, family = list(), obsWeights = NULL, id = NULL, degree = 2, penalty = 3, nk = max(21, 2 * ncol(X) + 1), pmethod = "backward", nfold = 0, ncross = 1, minspan = 0, endspan = 0, verbose = F, ...)
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
  \item{degree}{
%%     ~~Describe \code{degree} here~~
}
  \item{penalty}{
%%     ~~Describe \code{penalty} here~~
}
  \item{nk}{
%%     ~~Describe \code{nk} here~~
}
  \item{pmethod}{
%%     ~~Describe \code{pmethod} here~~
}
  \item{nfold}{
%%     ~~Describe \code{nfold} here~~
}
  \item{ncross}{
%%     ~~Describe \code{ncross} here~~
}
  \item{minspan}{
%%     ~~Describe \code{minspan} here~~
}
  \item{endspan}{
%%     ~~Describe \code{endspan} here~~
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
function (Y, X, newX = NULL, family = list(), obsWeights = NULL, 
    id = NULL, degree = 2, penalty = 3, nk = max(21, 2 * ncol(X) + 
        1), pmethod = "backward", nfold = 0, ncross = 1, minspan = 0, 
    endspan = 0, verbose = F, ...) 
{
    if (verbose == T) {
        cat("SL.earth started (degrees=", degree, "). ", sep = "")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("earth")
    if (family$family == "gaussian") {
        fit.earth <- earth::earth(x = X, y = Y, degree = degree, 
            nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold, 
            ncross = ncross, minspan = minspan, endspan = endspan)
    }
    if (family$family == "binomial") {
        fit.earth <- earth::earth(x = X, y = Y, degree = degree, 
            nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold, 
            ncross = ncross, minspan = minspan, endspan = endspan, 
            glm = list(family = binomial))
    }
    pred <- predict(fit.earth, newdata = newX, type = "response")
    fit <- list(object = fit.earth)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.earth")
    end_time <- Sys.time()
    if (verbose == T) {
        cat("SL.earth finished. Time:", round(difftime(end_time, 
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
