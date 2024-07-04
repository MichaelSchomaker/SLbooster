\name{SL.xgboost_boost}
\alias{SL.xgboost_boost}
\title{
Test
}
\description{
Test
}
\usage{
SL.xgboost_boost(Y, X, newX, family, obsWeights, id, ntrees = 1000, max_depth = 4, eta = 0.1, minobspernode = 10, params = list(), nthread = 1, verb = 0, save_period = NULL, verbose = T, ...)
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
  \item{ntrees}{
%%     ~~Describe \code{ntrees} here~~
}
  \item{max_depth}{
%%     ~~Describe \code{max_depth} here~~
}
  \item{eta}{
%%     ~~Describe \code{eta} here~~
}
  \item{minobspernode}{
%%     ~~Describe \code{minobspernode} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{nthread}{
%%     ~~Describe \code{nthread} here~~
}
  \item{verb}{
%%     ~~Describe \code{verb} here~~
}
  \item{save_period}{
%%     ~~Describe \code{save_period} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
what is different to standard
}
\value{
A list with two elements:
\code{pred} The predicted values for the rows in \code{newX}
\code{fit} A list. Contains all objects necessary to get predictions for new observations from specific algorithm.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Based on the wrapper written by Eric Polley in the \pkg{SuperLearner} package. Changes as listed under details.
}
\note{
Any notes?
}

\seealso{
See also \code{\link{help}}, ~~~
}

%\examples{
%
%}
\keyword{xgboost}
