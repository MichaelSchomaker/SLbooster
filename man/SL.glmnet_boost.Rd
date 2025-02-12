\name{SL.glmnet_boost}
\alias{SL.glmnet_boost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for glmnet
}
\description{
Wrapper for fitting Generalized Linear Models (GLM) with Elastic Net (Lasso, Ridge) regularization. 
}
\usage{
SL.glmnet_boost(Y, X, newX, family, obsWeights = NULL, id = NULL, alpha = 1,
               nfolds = 10, nlambda = 100, useMin = TRUE, loss = "deviance",
               verbose = T, ...)
}
\arguments{
  \item{Y}{
The response variable, which can be numeric or binary.
}
  \item{X}{
A data frame or matrix of predictor variables used to fit the model. Each column represents a predictor.
}
  \item{newX}{
A data frame or matrix of new predictor data used for generating predictions. 
}
  \item{family}{
A \code{list} describing how the prediction should be calculated. For numeric \code{Y}, use \code{family = list(family = "gaussian")}; for binary classification, use \code{family = list(family = "binomial")}. 
}
  \item{obsWeights}{
An optional vector of weights to be used in the fitting process. Defaults to \code{NULL}.
}
  \item{id}{
Optional vector of IDs for clustering/grouping, currently unused in this function. Defaults to \code{NULL}.
}
  \item{alpha}{
The elastic net mixing parameter. Must be between 0 (Ridge) and 1 (Lasso). Defaults to \code{1}.
}
  \item{nfolds}{
Number of folds. Although nfolds can be as large as the sample
size (leave-one-out CV), it is not recommended for large datasets. Smallest value allowed is nfolds=3. Default is \code{10}.
}
  \item{nlambda}{
The number of lambda (controlling regularization) values. Default is \code{100}.
}
  \item{useMin}{
Boolean specifying whether to use the lambda with minimum cross-validated error (\code{lambda.min}) or the lambda within 1 standard error of the minimum (\code{lambda.1se}). Defaults to \code{TRUE}.
}
  \item{loss}{
Loss to use for cross-validation. Currently five options, not all available for all
models. The default is \code{type.measure="deviance"}, which uses squared-error
for gaussian models (a.k.a \code{type.measure="mse"} there), deviance for logistic
and poisson regression, and partial-likelihood for the Cox model. \code{type.measure="class"}
applies to binomial and multinomial logistic regression only, and gives misclassification
error. \code{type.measure="auc"} is for two-class logistic regression only,
and gives area under the ROC curve. \code{type.measure="mse"} or \code{type.measure="mae"}
(mean absolute error) can be used by all models except the "cox"; they measure
the deviation from the fitted mean to the response. \code{type.measure="C"} is Harrel's
concordance measure, only available for Cox models.
}
  \item{verbose}{
A logical value indicating whether to print detailed information during the execution of the function. Defaults to \code{TRUE}.
}
  \item{\dots}{
Additional arguments currently unused.
}
}
\details{
If family is set as \code{family = list(family = "binomial")}, but \code{Y} consists not only of 0 and 1, the Gaussian family will be used for prediction, but predictions falling outside [0,1] will be set as 0/1.
}
\value{
A list containing:
  \item{pred}{Predicted values from the fitted model, based on \code{newX} if provided.}
  \item{fit}{The fitted GLMNET model.}
}
\references{
Friedman, J., Hastie, T. and Tibshirani, R. (2008) Regularization Paths for Generalized Linear Models
via Coordinate Descent (2010), Journal of Statistical Software, Vol. 33(1), 1-22, doi:10.18637/
jss.v033.i01.

Simon, N., Friedman, J., Hastie, T. and Tibshirani, R. (2011) Regularization Paths for Cox's Proportional
Hazards Model via Coordinate Descent, Journal of Statistical Software, Vol. 39(5), 1-13,
doi:10.18637/jss.v039.i05.

The \code{glmnet} package: \url{https://cran.r-project.org/web/packages/glmnet/index.html}
}
\author{
Wrapper from \pkg{SuperLearner} package, written by Eric Polley.  

Extensions by Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr.
}

\seealso{
\code{\link[glmnet]{glmnet}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.glmnet_boost(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.glmnet_boost(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.glmnet_boost", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
}
  \keyword{glmnet}
  \keyword{regression}
  \keyword{classification}
  \concept{Elastic Net}
