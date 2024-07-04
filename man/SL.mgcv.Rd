\name{SL.mgcv}
\alias{SL.mgcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.mgcv(Y, X, newX = NULL, family = list(), obsWeights = NULL, cts.num = 10, by = NA, verbose = F, ...)
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
  \item{cts.num}{
%%     ~~Describe \code{cts.num} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
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
    cts.num = 10, by = NA, verbose = F, ...) 
{
    if (verbose == T) {
        cat("SL.mgcv started (if >", cts.num, " unique values = spline added; it interacts with: ", 
            by, "). ", sep = "")
    }
    start_time <- Sys.time()
    require("mgcv")
    if (all(by \%in\% colnames(X)) == FALSE) {
        if (verbose == T) {
            cat(paste(by, "not in column names of X. `by=NA' is used.\n"))
        }
        by <- NA
    }
    s <- mgcv:::s
    metric_var <- apply(X, 2, function(x) (length(unique(x)) > 
        cts.num))
    if (sum(metric_var) != 0 & sum(metric_var) != length(metric_var)) {
        gam.model <- as.formula(paste("Y~", paste(paste("s(", 
            colnames(X[, metric_var, drop = FALSE]), ", by=", 
            by, ")", sep = ""), collapse = "+"), "+", paste(colnames(X[, 
            !metric_var, drop = FALSE]), collapse = "+")))
    }
    else {
        if (all(metric_var)) {
            gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                colnames(X[, metric_var, drop = FALSE]), ", by=", 
                by, ")", sep = ""), collapse = "+")))
        }
        else {
            gam.model <- as.formula(paste("Y~", paste(colnames(X), 
                collapse = "+"), sep = ""))
        }
    }
    fit.gam <- try(mgcv::gam(gam.model, data = X, family = family, 
        weights = obsWeights))
    if (class(fit.gam)[1] == "try-error") {
        gam.model <- as.formula(paste("Y~1"))
        fit.gam <- try(mgcv::gam(gam.model, data = X, family = family, 
            weights = obsWeights))
        if (verbose == T) {
            cat("GAM failed with variables provided. Intercept-only GAM fitted.")
        }
    }
    pred <- mgcv::predict.gam(fit.gam, newdata = newX, type = "response")
    fit <- list(object = fit.gam)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.mgcv")
    end_time <- Sys.time()
    if (verbose == T) {
        cat("SL.mgcv finished. Time:", round(difftime(end_time, 
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
