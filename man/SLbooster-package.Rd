\name{SLbooster-package}
\alias{SLbooster-package}
\alias{SLbooster}
\docType{package}
\title{
Title
}
\description{
Here we have to write something meaningful.
Many details.
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
}

\author{
Michael Schomaker, Philipp Baumann, Han Bao, Katharina Ring, Christoph Wiederkehr

Maintainer: Michael Schomaker <michael.schomaker@stat.uni-muenchen.de>
}

\examples{
create_glmnet_boost <- create.Learner("SL.glmnet_boost", params=list(nfolds=5),
                                tune=list(alpha=c(0.8,1)),
                                detailed_names = TRUE)
}
