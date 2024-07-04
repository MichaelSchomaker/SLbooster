\name{screen.glmnet_nVar}
\alias{screen.glmnet_nVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
screen.glmnet_nVar(Y, X, family = list(), alpha = 1, nfolds = 5, nlambda = 150, nVar = 6, verbose = T, ...)
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
  \item{nfolds}{
%%     ~~Describe \code{nfolds} here~~
}
  \item{nlambda}{
%%     ~~Describe \code{nlambda} here~~
}
  \item{nVar}{
%%     ~~Describe \code{nVar} here~~
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
function (Y, X, family = list(), alpha = 1, nfolds = 5, nlambda = 150, 
    nVar = 6, verbose = T, ...) 
{
    if (verbose == T) {
        cat("screen.glmnet_nVar with alpha=", alpha, ", ", nfolds, 
            " fold CV and selecting ", nVar, " variables \n", 
            sep = "")
    }
    if (nVar < 1) {
        stop("nVar needs to be greater or equal to one")
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
    if (ncol(X) <= nVar) {
        if (verbose == T) {
            cat("Note: ", nVar, "variables are not available in the data (modify setup!). Set nVar=1 for now. \n")
        }
        nVar <- 1
    }
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
        if (all(fitCV$nzero != nVar)) {
            lambda_index_with_nVar <- min(which(abs(fitCV$nzero - 
                nVar) == min(abs(fitCV$nzero - nVar))))
        }
        else if (any(fitCV$nzero == nVar)) {
            lambda_index_with_nVar <- min(which(fitCV$nzero == 
                nVar))
        }
        coefs <- coef(fitCV$glmnet.fit, s = fitCV$glmnet.fit$lambda[lambda_index_with_nVar])
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
        if (nVar != sum(whichVariable) & verbose == T) {
            cat("Note: exactly", nVar, "variables could not be screened, instead", 
                sum(whichVariable), "vars were screened. \n")
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
            paste(colnames(saveX)[whichVariable]), "\n", sep = " ")
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
