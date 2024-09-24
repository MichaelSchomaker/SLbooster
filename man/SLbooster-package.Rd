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
library(SLbooster)
data(EFV)

# Create a custom learner using 'SL.glmnet' from SuperLearner package with SLbooster
create_glmnet_boost <- create.Learner("SL.glmnet_boost", 
                                      params=list(nfolds=5),
                                      tune=list(alpha=c(0.8,1)),
                                      detailed_names = TRUE)

# Define the library with the different customized 'glmnet_boost' learners and use original 'SL.mean' from SuperLearner
SL.library1 <- c("SL.mean", create_glmnet_boost$names)

# Fit the SuperLearner model on 'EFV' data with continous outcome 'EFV$weight.4'
fit1 <- SuperLearner(Y = EFV$weight.4, X = EFV[,1:20], verbose = FALSE, SL.library = SL.library1, family= "gaussian")
fit1$coef
fit1$whichScreen
# Same library but on binary outcome 'EFV$VL.4'
fit1B <- SuperLearner(Y = EFV$VL.4, X = EFV[,1:22], verbose = FALSE, SL.library = SL.library1, family = "binomial")
fit1B$coef
fit1B$whichScreen



# Create custom screening algorithm from SLbooster package (default: verbose=TRUE)
screen_glmnet_nvar <- create.Learner("screen.glmnet_nVar", tune = list(nVar=c(4:6)),
                                     detailed_names = T)

# Create custom 'SL.gam_boost' learners from SLbooster package
create_gam_boost <- create.Learner("SL.gam_boost",
               tune=list(df.gam=c(2,3,4)),
               detailed_names = T)

SL.library2 <- list(c(("SL.gam_boost_2"), "screen.glmnet_nVar_4"), 
                    c(("SL.gam_boost_3"), "screen.glmnet_nVar_5"), 
                    c(("SL.gam_boost_4"), "screen.glmnet_nVar_6"), 
                    c(("SL.mean"), "screen.glmnet_nVar_4"),
                    c("SL.glmnet_boost_1")
                    )


# Fit the SuperLearner model on 'EFV' data with continous outcome 'EFV$weight.4'
fit2 <- SuperLearner(Y = EFV$weight.4, X = EFV[,1:20], verbose = FALSE, SL.library = SL.library2, family = "gaussian")
fit2$coef
fit2$whichScreen
# Same library but on binary outcome 'EFV$VL.4'
fit2B <- SuperLearner(Y = EFV$VL.4, X = EFV[,1:22], verbose = FALSE, SL.library = SL.library2, family = "binomial")
fit2B$coef
fit2B$whichScreen


# usage in TMLE
# tmle only allows binary treatment
library(tmle)

# create custom 'SL.earth' learner with SLbooster
create_earth <- create.Learner("SL.earth_boost",
                               tune=list(degree=c(2,3),
                                         penalty=c(2,3)),
                               detailed_names = T)

# Define library for Q and g functions
# 'Sl.median', 'SL.mean', 'Sl.glm' are from SuperLearner package
ll <- list(Q=list(
  "SL.mean",
  c("SL.glm","screen.glmnet_nVar_6"),  # 'screen.glmnet_nVar_6' needs to be created first (see code above)
  c("SL.earth_boost_2_2")
),
g=list(c(("SL.earth_boost_2_2"), "screen.glmnet_nVar_4"),
       c(("SL.earth_boost_3_2"), "screen.glmnet_nVar_4"),
       "SL.glm", "SL.median")
)



# ontinous outcome 'EFV$weight.4'
tmle_1 <- tmle(Y = EFV$weight.4,
               A = EFV$VL.3,
               W = EFV[,1:18],
               Q.SL.library = ll$Q,
               g.SL.library = ll$g
)
tmle_1

# binary outcome 'EFV$efv.4'
tmle_2 <- tmle(Y = EFV$efv.4,
               A = EFV$VL.3,
               W = EFV[,1:18],
               Q.SL.library = ll$Q,
               g.SL.library = ll$g
)
tmle_2
}
}
