\name{SL.orm}
\alias{SL.orm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ordinal Regression Model (SL.orm)
}
\description{
The \code{SL.orm} function is a wrapper for the ordinal regression model (ORM) used within the \code{SuperLearner} framework. It fits an ORM when the response is ordinal and uses alternative models (GLM or mean) when the outcome is binary or insufficient for ORM.
}
\usage{
SL.orm(Y, X, newX, verbose = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The response variable, which can be binary or ordinal. If the outcome is binary, a GLM is used instead of an ordinal regression model.
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{newX}{
A data frame or matrix of new predictor data used for generating predictions. If not provided, the model will only be fitted on \code{X}.
}
  \item{verbose}{
A logical value indicating whether to print detailed information during the execution of the function. Defaults to \code{TRUE}.
}
  \item{\dots}{
Additional arguments passed to the underlying model functions, such as \code{SL.glm}, \code{SL.mean}, or \code{orm} from the \code{rms} package.
}
}
\details{
The \code{SL.orm} function is designed to work within the \code{SuperLearner} framework. It fits an ordinal regression model using the \code{orm} function from the \code{rms} package when the outcome variable \code{Y} is ordinal (more than two levels). If \code{Y} is binary, the function defaults to a generalized linear model (GLM). In cases where the number of unique levels in \code{Y} is insufficient for ordinal regression (less than 5 levels), the function defaults to using the \code{SL.mean} algorithm to avoid fitting issues.
}
\value{
A list containing:
  \item{pred}{Predicted values from the fitted model, based on \code{newX} if provided.}
  \item{fit}{The fitted ordinal regression model (if applicable) or the alternative model (GLM or mean).}
}
\references{
Harrell FE Jr. (2023). \emph{Regression Modeling Strategies}. Springer Series in Statistics.
The \code{SuperLearner} package: \url{https://cran.r-project.org/web/packages/SuperLearner/index.html}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
This function is a part of the \code{SuperLearner} package. It automatically selects the appropriate model (ORM, GLM, or mean) depending on the characteristics of the response variable \code{Y}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SuperLearner}}, \code{\link{SL.glm}}, \code{\link{orm}}, \code{\link{SL.mean}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{regression}
\keyword{SuperLearner}
\concept{ordinal regression}
\concept{ensemble learning}
