\name{SL.hal}
\alias{SL.hal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.hal(Y, X, newX, verbose = T, family = stats::gaussian(), obsWeights = rep(1, length(Y)), ...)
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
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{obsWeights}{
%%     ~~Describe \code{obsWeights} here~~
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
function (Y, X, newX, verbose = T, family = stats::gaussian(), 
    obsWeights = rep(1, length(Y)), ...) 
{
    if (verbose == T) {
        cat("SL.hal started. ")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("hal9001")
    if (!is.matrix(X)) {
        X <- as.matrix(X)
    }
    if (!is.null(newX) & !is.matrix(newX)) {
        newX <- as.matrix(newX)
    }
    fit.hal <- try(hal9001::fit_hal(X = X, Y = Y, family = family$family, 
        num_knots = hal9001:::num_knots_generator(max_degree = ifelse(ncol(X) >= 
            20, 2, 3), smoothness_order = 1, base_num_knots_0 = ifelse(sqrt(nrow(X)) < 
            200, sqrt(nrow(X)), 200), base_num_knots_1 = ifelse(sqrt(nrow(X)) < 
            50, sqrt(nrow(X)), 50)), fit_control = list(cv_select = T, 
            n_folds = 10, weights = obsWeights, foldid = NULL, 
            use_min = T, lambda.min.ratio = 1e-04, prediction_bounds = "default"), 
        ), silent = TRUE)
    if (class(fit.hal)[1] == "try-error") {
        if (verbose == T) {
            cat("Technical failure: GLM used instead of HAL.")
        }
        out <- SuperLearner::SL.glm(Y = Y, X = X, newX = newX, 
            family = family, obsWeights = rep(1, length(Y)), 
            ...)
    }
    else {
        if (!is.null(newX)) {
            pred <- stats::predict(fit.hal, new_data = newX)
        }
        else {
            pred <- stats::predict(fit.hal, new_data = X)
        }
        fit <- list(object = fit.hal)
        class(fit) <- "SL.hal"
        out <- list(pred = pred, fit = fit)
    }
    end_time <- Sys.time()
    if (verbose == T) {
        cat("SL.hal finished. Time:", round(difftime(end_time, 
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
