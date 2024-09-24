\name{SLbooster-package}
\alias{SLbooster-package}
\alias{SLbooster}
\docType{package}
\title{
SLbooster: Enhanced Learning with Boosted Screening and GAM Models for SuperLearner 
}
\description{
The \code{SLbooster} package extends the \code{SuperLearner} framework by providing additional learners and screening functions. It integrates advanced algorithms like Generalized Additive Models (GAM), Elastic Net, and boosted learning techniques. The package also includes custom screening methods such as Cramer's V-based screening and variable selection using Elastic Net with cross-validation. These functions are designed to enhance predictive performance, particularly in settings with complex interactions between continuous and categorical variables.
}

\details{
\tabular{ll}{
Package: \tab SLbooster\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2024-07-03\cr
License: \tab GPL-2\cr
Depends: \tab R (>= 4.0), SuperLearner\cr
Imports: \tab mgcv, gam, rms, glmnet, dbarts, randomForest, xgboost, rpart, hal9001, vcd, earth, matrixStats\cr
}
This package provides a set of custom learners and screening methods for use within the \code{SuperLearner} ensemble modeling framework. It is particularly suited for scenarios involving complex data, where traditional learners may struggle with non-linear relationships or high-dimensional features. The primary goal of the package is to offer robust and efficient tools to boost model performance by combining different learners and incorporating specialized screening algorithms.
}

\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr

Maintainer: Michael Schomaker <michael.schomaker@stat.uni-muenchen.de>
}

\examples{
\donttest{
# Example of creating a custom learner with SLbooster
library(SuperLearner)

# Create a custom learner using glmnet with boosting
create_glmnet_boost <- create.Learner("SL.glmnet_boost", 
                                      params=list(nfolds=5),
                                      tune=list(alpha=c(0.8,1)),
                                      detailed_names = TRUE)

# Use the learner in the SuperLearner framework
set.seed(123)
n <- 100
X <- data.frame(X1 = rnorm(n), X2 = rnorm(n), X3 = rnorm(n))
Y <- rnorm(n)

# Define the SuperLearner library with the custom glmnet_boost learner
SL.library <- c("SL.mean", create_glmnet_boost$names)

# Fit the SuperLearner model
fit <- SuperLearner(Y = Y, X = X, SL.library = SL.library, method = "method.NNLS")

# Print the results
print(fit)

# Example using screen.cramersv
selected_vars <- screen.cramersv(Y = factor(sample(c(0, 1), n, replace = TRUE)), 
                                 X = X, nscreen = 2)
print(selected_vars)
}
}
