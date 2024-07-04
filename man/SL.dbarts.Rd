\name{SL.dbarts}
\alias{SL.dbarts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.dbarts(Y, X, newX, family, obsWeights, id, sigest = NA, sigdf = 3, sigquant = 0.9, k = 2, power = 2, base = 0.95, binaryOffset = 0, ntree = 200, ndpost = 1000, nskip = 100, printevery = 100, keepevery = 1, keeptrainfits = TRUE, usequants = FALSE, numcut = 100, printcutoffs = 0, nthread = 1, keepcall = TRUE, verb = F, verbose = TRUE, ...)
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
  \item{sigest}{
%%     ~~Describe \code{sigest} here~~
}
  \item{sigdf}{
%%     ~~Describe \code{sigdf} here~~
}
  \item{sigquant}{
%%     ~~Describe \code{sigquant} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{power}{
%%     ~~Describe \code{power} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{binaryOffset}{
%%     ~~Describe \code{binaryOffset} here~~
}
  \item{ntree}{
%%     ~~Describe \code{ntree} here~~
}
  \item{ndpost}{
%%     ~~Describe \code{ndpost} here~~
}
  \item{nskip}{
%%     ~~Describe \code{nskip} here~~
}
  \item{printevery}{
%%     ~~Describe \code{printevery} here~~
}
  \item{keepevery}{
%%     ~~Describe \code{keepevery} here~~
}
  \item{keeptrainfits}{
%%     ~~Describe \code{keeptrainfits} here~~
}
  \item{usequants}{
%%     ~~Describe \code{usequants} here~~
}
  \item{numcut}{
%%     ~~Describe \code{numcut} here~~
}
  \item{printcutoffs}{
%%     ~~Describe \code{printcutoffs} here~~
}
  \item{nthread}{
%%     ~~Describe \code{nthread} here~~
}
  \item{keepcall}{
%%     ~~Describe \code{keepcall} here~~
}
  \item{verb}{
%%     ~~Describe \code{verb} here~~
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
function (Y, X, newX, family, obsWeights, id, sigest = NA, sigdf = 3, 
    sigquant = 0.9, k = 2, power = 2, base = 0.95, binaryOffset = 0, 
    ntree = 200, ndpost = 1000, nskip = 100, printevery = 100, 
    keepevery = 1, keeptrainfits = TRUE, usequants = FALSE, numcut = 100, 
    printcutoffs = 0, nthread = 1, keepcall = TRUE, verb = F, 
    verbose = TRUE, ...) 
{
    if (verbose == T) {
        cat("SL.dbarts started with ", ntree, " trees and k=", 
            k, ". ", sep = "")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("dbarts")
    model <- try(dbarts::bart(x.train = X, y.train = Y, x.test = newX, 
        sigest = sigest, sigdf = sigdf, sigquant = sigquant, 
        k = k, power = power, base = base, binaryOffset = binaryOffset, 
        weights = obsWeights, ntree = ntree, ndpost = ndpost, 
        nskip = nskip, printevery = printevery, keepevery = keepevery, 
        keeptrainfits = keeptrainfits, usequants = usequants, 
        numcut = numcut, printcutoffs = printcutoffs, nthread = nthread, 
        keepcall = keepcall, keeptrees = TRUE, verbose = verb))
    if (family$family == "gaussian") {
        pred = model$yhat.test.mean
    }
    if (family$family == "binomial") {
        pred = colMeans(stats::pnorm(model$yhat.test))
    }
    fit = list(object = model)
    class(fit) = c("SL.dbarts")
    out = list(pred = pred, fit = fit)
    end_time <- Sys.time()
    if (verbose == T) {
        cat("SL.dbarts finished. Time:", round(difftime(end_time, 
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
