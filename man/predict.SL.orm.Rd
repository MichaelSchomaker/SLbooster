\name{predict.SL.orm}
\alias{predict.SL.orm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict Method for SL.orm
}
\description{
This function generates predictions for new data based on a model fitted by \code{SL.orm}. It uses the \code{predict.orm} function from the \code{rms} package to return the expected means for ordinal outcomes.
}
\usage{
\method{predict}{SL.orm}(object, newdata, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
The fitted model object returned by \code{SL.orm}, containing the ordinal regression model or an alternative model (e.g., GLM or mean) depending on the original data.
}
  \item{newdata}{
A data frame or matrix of new predictor values for which predictions are to be made. The function automatically converts a matrix to a data frame if necessary.
}
  \item{\dots}{
Additional arguments passed to the underlying prediction function from the \code{rms} package, such as options related to the type of prediction.
}
}
\details{
The \code{predict.SL.orm} function works with objects created by the \code{SL.orm} function. It uses the \code{predict.orm} method from the \code{rms} package to predict the expected mean of ordinal outcomes based on the new data provided. If \code{SL.orm} used a simple GLM (in case of failure of orm), the predictions will reflect that.
}
\value{
A vector of predicted values representing the expected means of the ordinal categories for each observation in \code{newdata}.
}
\references{
Harrell FE Jr. (2023). \emph{Regression Modeling Strategies}. Springer Series in Statistics.
The \code{rms} package: \url{https://cran.r-project.org/web/packages/rms/index.html}
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}

\seealso{
\code{\link{SL.orm}}, \code{\link{predict.orm}}
}

