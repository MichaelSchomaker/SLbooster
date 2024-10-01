\name{SL.hal}
\alias{SL.hal}
\title{SuperLearner Wrapper for Highly Adaptive Lasso Estimation}
\description{
  A wrapper function for the \code{SuperLearner} package that enables the use of the \code{hal9001} package for Highly Adaptive Lasso (HAL) estimation. This integration allows users to fit models and make predictions using HAL within the SuperLearner framework, facilitating advanced modeling techniques for various types of data.
}
\usage{
SL.hal(Y, X, newX = NULL, verbose = T, family = stats::gaussian(),
        runtime = c("default", "fast", "slow", "custom"),
        custom_params = NULL,
        obsWeights = rep(1, length(Y)), ...)
}
\arguments{
  \item{Y}{
    A \code{numeric} vector of observed outcome values. For \code{family = "mgaussian"}, \code{Y} should be a matrix of outcome observations.
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
  Michael Schomaker, Han Bao
}
\note{
  The HAL model may be computationally intensive for large datasets or many predictor variables. Adjusting the \code{fit_control} settings or selecting an appropriate \code{runtime} option can help optimize performance.
}
\seealso{
  \code{\link[hal9001]{fit_hal}}, \code{\link[SuperLearner]{SuperLearner}}
}
\examples{
  # Generate a synthetic dataset
  set.seed(123)
  n <- 200 # number of observations
  p <- 10  # number of predictors
  data_matrix <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(data_matrix) <- paste0("X", 1:p)
  response <- 3 * data_matrix[,1] - 2 * data_matrix[,2] + rnorm(n) # Generate response variable

  # Split the data into training and testing sets
  set.seed(123) # for reproducibility of the split
  train_indices <- sample(1:n, size = 0.7 * n) # 70% for training
  test_indices <- setdiff(1:n, train_indices) # Remaining 30% for testing

  # Create training and testing datasets
  X_train <- data_matrix[train_indices, ]
  Y_train <- response[train_indices]
  X_test <- data_matrix[test_indices, ]

  # Test the function with the split data
  result <- SL.hal(Y = Y_train, X = X_train, newX = X_test, family = stats::gaussian())

  # Print the predicted values
  print(result$pred)
  
}
\keyword{model}
\keyword{regression}
\concept{Highly Adaptive Lasso}
\concept{SuperLearner}
