\name{SL.hal}
\alias{SL.hal}
\title{Wrapper for HAL}
\description{
  Wrapper for the highly adaptive lasso using hal(), from the package hal9001}
\usage{
SL.hal(Y, X, newX = NULL, verbose = T, family = stats::gaussian(),
        runtime = c("default", "fast", "slow", "custom"),
        custom_params = NULL,
        obsWeights = rep(1, length(Y)), ...)
}
\arguments{
  \item{Y}{
    A \code{numeric} vector of observed outcome values.
  }
  \item{X}{
    A \code{matrix} or \code{data.frame} of predictor variables, where rows correspond to observations and columns correspond to predictors. The input will be converted to a matrix if it is not already in that format.
  }
  \item{newX}{
    A \code{matrix} or \code{data.frame} containing new observations for which predictions are required. If \code{NULL}, predictions will be generated for the original input \code{X}.
  }
  \item{verbose}{
    A \code{logical} value indicating whether to print progress and timing information to the console. Defaults to \code{TRUE}.
  }
  \item{family}{A \code{\link[stats]{family}} object (one that is supported by \code{\link[glmnet]{glmnet}}) specifying the error/link family for a generalized linear model.
  }
  \item{runtime}{
    A \code{character} vector specifying the runtime option for model fitting. Options include:
    \describe{
      \item{\code{"default"}}{Default parameter settings based on the dataset's characteristics.}
      \item{\code{"fast"}}{Optimized for faster performance with potentially less model complexity.}
      \item{\code{"slow"}}{Optimized for thorough fitting, potentially with increased computational cost.}
      \item{\code{"custom"}}{Allows the user to specify custom parameters for fitting.}
    }
  }
  \item{custom_params}{
    A \code{list} of custom parameters for the HAL fitting. Only used if \code{runtime = "customized"}. Should include parameters \code{max_degree}, \code{base_num_knots_0}, \code{base_num_knots_1}, and \code{smoothness_order}.
  }
  \item{obsWeights}{
    A \code{numeric} vector of observation weights, with the same length as \code{Y}. Defaults to equal weights for all observations.
  }
  \item{\dots}{
    Additional arguments currently unused.
  }
}
\details{
  This function fits a Highly Adaptive Lasso (HAL) model using the \code{hal9001} package. The HAL model adapts to complex relationships in the data through basis functions and penalization. If the HAL model fails to fit, the function falls back to using a standard generalized linear model (GLM) via \code{SL.glm}. The \code{runtime} parameter allows for different levels of computational efficiency and model complexity, with options for default, fast, slow, or fully customized settings.
}
\value{
  A \code{list} containing two components:
  \item{pred}{
    \code{numeric} vector of predicted values based on the fitted model.
  }
  \item{fit}{
    An object of class \code{SL.hal} containing the fitted HAL model, which can be used for further analysis or predictions.
  }
}
\references{
Benkeser, D., & van der Laan, M. J. (2016). The highly adaptive lasso estimator. 

The \code{hal9001} package: \url{https://github.com/tlverse/hal9001}
}
\author{
  Original wrapper from the \code{hal9001} package: Jeremy Coyle, Nima Hejazi, Rachael Phillips, Lars van der Laan, David Benkeser, Oleg Sofrygin, Weixin Cai, Mark van der Laan.

  Extensions by Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr
}
\note{
  The HAL model may be computationally intensive for large datasets or many predictor variables. Adjusting the \code{fit_control} settings or selecting an appropriate \code{runtime} option can help optimize performance.
}
\seealso{
  \code{\link[hal9001]{fit_hal}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
library(SuperLearner)
data(EFV)
result1 <- SL.hal(Y = EFV[, "VL.1"], X = EFV[, c("efv.0", "metabolic", "weight.0")], 
                                     newX = EFV[, c("efv.0", "metabolic", "weight.0")], family = stats::binomial())
result2 <- SL.hal(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")],
                                         newX = EFV[, c("sex", "metabolic", "log_age")], family = stats::gaussian())
result3 <- SuperLearner(Y = EFV[, "weight.0"], X = EFV[, c("sex", "metabolic", "log_age")], 
                        newX = EFV[, c("sex", "metabolic", "log_age")], SL.library="SL.hal", 
                        cvControl = SuperLearner.CV.control(V = 3L))
print(result1$pred)
  
}
\keyword{model}
\keyword{regression}
\concept{Highly Adaptive Lasso}

