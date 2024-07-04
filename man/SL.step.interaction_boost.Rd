\name{SL.step.interaction_boost}
\alias{SL.step.interaction_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.step.interaction_boost(Y, X, newX, family, direction = "both", trace = 0, k = 2, verbose = T, ...)
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
  \item{direction}{
%%     ~~Describe \code{direction} here~~
}
  \item{trace}{
%%     ~~Describe \code{trace} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
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
function (Y, X, newX, family, direction = "both", trace = 0, 
    k = 2, verbose = T, ...) 
{
    if (verbose == T) {
        cat("SL.step.interaction started, direction = ", direction, 
            ". ", sep = "")
    }
    start_time <- Sys.time()
    fit.glm <- glm(Y ~ ., data = X, family = family)
    fit.step <- step(fit.glm, scope = Y ~ .^2, direction = direction, 
        trace = trace, k = k)
    pred <- predict(fit.step, newdata = newX, type = "response")
    fit <- list(object = fit.step)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.step")
    end_time <- Sys.time()
    if (verbose == T) {
        cat("SL.step.interaction finished. Time:", round(difftime(end_time, 
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
