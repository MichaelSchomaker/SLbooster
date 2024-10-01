\name{SL.xgboost_boost}
\alias{SL.xgboost_boost}
\title{
SL.xgboost_boost: Extreme Gradient Boosting for SuperLearner}
\description{
The \code{SL.xgboost_boost} function trains an XGBoost model for regression, binary classification, or multinomial classification tasks within the \code{SuperLearner} framework.
It includes additional features, such as printing progress information (e.g., tree depth, number of trees, and execution time), and provides flexibility in configuring parameters like the learning rate
and maximum tree depth. 
}
\usage{
SL.xgboost_boost(Y, X, newX, family, obsWeights = NULL, id = NULL, ntrees = 1000, max_depth = 4, eta = 0.1, minobspernode = 10, params = list(), nthread = 1, verb = 0, save_period = NULL, verbose = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The outcome variable. If \code{family} is set to \code{"gaussian"}, \code{Y} is a continuous variable for regression. If \code{family} is set to \code{"binomial"}, \code{Y} should be binary (0/1). For \code{"multinomial"}, \code{Y} should be a factor representing the classes. 
}
  \item{X}{
A matrix or data frame of predictor variables used to fit the model. The function will convert the data to a model matrix if it is not already a matrix.
}
  \item{newX}{
A matrix or data frame of new data for which to generate predictions. As with \code{X}, this will be converted to a model matrix if it is not already a matrix.
}
  \item{family}{
A description of the error distribution and link function to be used in the model. Can be \code{"gaussian"} for continuous outcomes, \code{"binomial"} for binary classification, or \code{"multinomial"} for classification with more than two classes.
}
  \item{obsWeights}{
Optional observation-level weights. A numeric vector of weights corresponding to each observation in \code{Y}. Default is \code{NULL}.
}
  \item{id}{
Optional. A numeric vector of IDs for each observation, which can be used for grouping during cross-validation or fitting. Default is \code{NULL}.
}
  \item{ntrees}{
The maximum number of boosting iterations (trees) to be fitted. Defaults to 1000.
}
  \item{max_depth}{
The maximum depth of each decision tree. Deeper trees can capture more complex patterns but may increase the risk of overfitting. Defaults to 4.
}
  \item{eta}{
The learning rate, also known as shrinkage, which controls the contribution of each tree to the final model. Smaller values make the boosting process more robust but require more trees. Defaults to 0.1.
}
  \item{minobspernode}{
The minimum number of observations required in a node to allow for a split in a decision tree. This is the equivalent of XGBoost's \code{min_child_weight}. Defaults to 10.
}
  \item{params}{
A list of additional parameters to be passed directly to the \code{xgboost} function. Can be used to fine-tune other aspects of the model beyond those explicitly exposed by \code{SL.xgboost_boost}.
}
  \item{nthread}{
The number of threads to use for parallel computation. This can speed up model fitting on multicore machines. Defaults to 1.
}
  \item{verb}{
Controls the verbosity of XGBoost. When \code{verb = 0}, no output is printed during model training. Higher values may result in more verbose output.
}
  \item{save_period}{
Optional. An integer that specifies how often the model should be saved during training. For example, if \code{save_period = 10}, the model will be saved after every 10 rounds.
}
  \item{verbose}{
If \code{TRUE}, progress information such as the number of trees, depth, and training time will be printed during model fitting. Defaults to \code{TRUE}.
}
  \item{\dots}{
Additional arguments currently unused.
}
}
\details{
This function extends the \code{SL.xgboost} wrapper from the \code{SuperLearner} package by including progress tracking and other customizable parameters. It provides more detailed control over XGBoost model parameters, including verbosity options, learning rate (\code{eta}), and more flexible handling of additional XGBoost parameters. The function also returns time statistics for the model training process.
}
\value{
A list with two elements:
\item{\code{pred}}{The predicted values for the rows in \code{newX}.}
\item{\code{fit}}{A list containing the fitted XGBoost model object, which can be used to generate predictions for new data.}
}
\references{
T. Chen and C. Guestrin (2016). "XGBoost: A Scalable Tree Boosting System". Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining, pp. 785-794.
}
\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr}
\note{
This function is specifically designed for integration into the \pkg{SuperLearner} ensemble learning framework, allowing XGBoost to be one of the candidate learners.
}
\seealso{
See also \code{\link{SuperLearner}}, \code{\link{xgboost}}.
}
\examples{
# Generate a synthetic dataset
set.seed(123)
n <- 300 # number of observations
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

# Fit the XGBoost model using the SL.xgboost_boost function
fit_result <- SL.xgboost_boost(Y = Y_train, X = X_train, newX = NULL, family = "gaussian", 
                               ntrees = 100, max_depth = 4, eta = 0.1, verbose = TRUE)

# Use the predict function to generate predictions on new data
predictions <- predict(fit_result$fit$object, newdata = X_test)

# Display the first few predictions
head(predictions)
}
\keyword{xgboost}
\concept{Extreme Gradient Boosting}
\concept{SuperLearner}
