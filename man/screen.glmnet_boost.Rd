\name{screen.glmnet_boost}
\alias{screen.glmnet_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
screen.glmnet_boost(Y, X, family, alpha = 1, verbose = T, nfolds = 10, nlambda = 200, nscreen = 2, ...)
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
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{nfolds}{
%%     ~~Describe \code{nfolds} here~~
}
  \item{nlambda}{
%%     ~~Describe \code{nlambda} here~~
}
  \item{nscreen}{
%%     ~~Describe \code{nscreen} here~~
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
function (Y, X, family, alpha = 1, verbose = T, nfolds = 10, 
    nlambda = 200, nscreen = 2, ...) 
{
    if (verbose == T) {
        cat("screen.glmnet with alpha=", alpha, " and ", nfolds, 
            " fold CV \n", sep = "")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("glmnet")
    X <- as.data.frame(X)
    Y <- as.vector(as.matrix(Y))
    if (all(Y == 0 | Y == 1)) {
        family$family <- "binomial"
    }
    else {
        family$family <- "gaussian"
    }
    saveY <- Y
    saveX <- X
    if (ncol(X) > 26 * 27) 
        stop("Find further column names for X!")
    let <- c(letters, sort(do.call("paste0", expand.grid(letters, 
        letters[1:26]))))
    names(X) <- let[1:ncol(X)]
    is_fact_var <- sapply(X, is.factor)
    X <- try(model.matrix(~-1 + ., data = X), silent = FALSE)
    successfulfit <- FALSE
    fitCV <- try(glmnet::cv.glmnet(x = X, y = Y, lambda = NULL, 
        type.measure = "deviance", nfolds = nfolds, family = family$family, 
        alpha = alpha, nlambda = nlambda, keep = T), silent = TRUE)
    if (all(fitCV$nzero == 0) | all(is.na(fitCV$nzero))) {
        fitCV <- try(glmnet::cv.glmnet(x = X, y = Y, lambda = log(fitCV$glmnet.fit$lambda + 
            1), type.measure = "deviance", nfolds = nfolds, family = family$family, 
            alpha = alpha, keep = T), silent = TRUE)
    }
    if (class(fitCV) == "try-error") {
        successfulfit <- FALSE
    }
    else {
        successfulfit <- TRUE
    }
    whichVariable <- NULL
    if (successfulfit == TRUE) {
        coefs <- coef(fitCV$glmnet.fit, s = fitCV$lambda.min)
        if (all(coefs[-1] == 0)) {
            whichVariable <- screen.cramersv_base(Y = saveY, 
                X = saveX, nscreen = nscreen)
            if (verbose == T) {
                cat("Lasso screened away all variables and screening was thus based on Cramer's V \n")
            }
        }
        else {
            var_nms <- coefs@Dimnames[[1]]
            if (any(is_fact_var)) {
                nms_fac <- names(which(is_fact_var))
                is_selected <- coefs[-1] != 0
                var_nms_sel <- gsub("[^::a-z::]", "", var_nms[-1][is_selected])
                sel_fac <- nms_fac[nms_fac \%in\% var_nms_sel]
                sel_numer <- var_nms_sel[!var_nms_sel \%in\% sel_fac]
                all_sel_vars <- c(sel_fac, sel_numer)
                whichVariable <- names(is_fact_var) \%in\% all_sel_vars
            }
            else {
                whichVariable <- coefs[-1] != 0
            }
        }
    }
    if (is.null(whichVariable)) {
        whichVariable <- screen.cramersv_base(Y, X)
        if (verbose == T) {
            cat("Lasso failed and screening was based on Cramer's V\n")
        }
    }
    if (verbose == T) {
        cat("screened ", sum(whichVariable), " variables: ", 
            paste(colnames(saveX)[whichVariable], sep = " "), 
            "\n")
        cat("sample size:", dim(X)[1], "; ")
    }
    end_time <- Sys.time()
    if (verbose == T) {
        cat("time:", round(difftime(end_time, start_time, units = "mins"), 
            digits = 4), "mins \n")
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
