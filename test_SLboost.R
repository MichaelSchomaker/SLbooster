library(SuperLearner)
library(SLbooster)
library(CICI) # for example data
data(EFV)
library(ltmle)
library(lmtp)
library(tmle)
#library(tmle3)

################################################################################

#
create_glmnet <- create.Learner("SL.glmnet", params=list(nfolds=5),
                                            tune=list(alpha=c(0.8,1)),
                                            detailed_names = T)
create_glmnet
create_glmnet$names
create_glmnet_boost <- create.Learner("SL.glmnet_boost", params=list(nfolds=5),
                                tune=list(alpha=c(0.8,1)),
                                detailed_names = T)
screen_glmnet_nvar <- create.Learner("screen.glmnet_nVar", tune = list(nVar=c(4:6)),
                                     detailed_names = T)
#

################################################################################

ll <- list(Q=list(
  c("SL.median"),
  c("SL.glm"),
  c("SL.glm","screen.glmnet_nVar_6"),
  c("SL.glmnet_boost_1")
),
g=list("SL.glm")
)
attr(ll$Q, "return.fit") <- TRUE

################################################################################

# Super Learning Test (family=gaussian)
sl1 <- SuperLearner(Y=EFV$efv.4, X=EFV[,1:21],verbose=F,SL.library=ll$Q)
sl1$coef
sl1$whichScreen

# Super Learning Test (family=binomial)
sl2 <- SuperLearner(Y=EFV$VL.4, X=EFV[,1:22],verbose=F,SL.library=ll$Q, family="binomial")
sl2$coef
sl2$whichScreen

################################################################################
# ltmle Test
EFV2 <- EFV
EFV2$efv.0 <- as.numeric(EFV2$efv.0 > 4)
EFV2$efv.1<- as.numeric(EFV2$efv.1 > 4)
EFV2$efv.2 <- as.numeric(EFV2$efv.2 > 4)
EFV2$efv.3 <- as.numeric(EFV2$efv.3 > 4)
EFV2$efv.4 <- as.numeric(EFV2$efv.4 > 4)
  
ltmle1 <- ltmle(EFV2,
                         Lnodes  = c("adherence.1","weight.1",
                                     "adherence.2","weight.2",
                                     "adherence.3","weight.3",
                                     "adherence.4","weight.4"
                         ),
                         Ynodes  = c("VL.0","VL.1","VL.2","VL.3","VL.4"),
                         Anodes  = c("efv.0","efv.1","efv.2","efv.3","efv.4"),
                         abar=rep(0,5), survivalOutcome = F, SL.cvControl = list(V=5),
                         estimate.time=F, gcomp=F,
                         SL.library= ll
                )
# 30 models. Why? It is 5 Q models. For each Q-model we have V=5 splits and 1 on the full data. 6*5=30. 
ltmle1$estimates
ltmle1$fit$Q

# small test for cont. Y
ltmle2 <- ltmle(EFV[,1:10],
                Lnodes  = c("adherence.1","weight.1"),
                Anodes  = c("VL.0"),
                Ynodes  = c("efv.1"),
                abar=0, SL.cvControl = list(V=5),
                estimate.time=F, gcomp=F,
                SL.library= ll
)
ltmle2$estimates
ltmle2$fit$Q

################################################################################
# lmtp test

#@Katy: ToDo
# multiple time points
lmtp1 <- lmtp_sdr(data = EFV,
                  trt = c("efv.0","efv.1","efv.2","efv.3","efv.4"), 
                  outcome = c("VL.4"), 
                  baseline = c("sex", "metabolic", "log_age", "NRTI"),
                  time_vary = list(c("weight.0"),
                                   c("adherence.1","weight.1", "VL.0"),
                                   c("adherence.2","weight.2", "VL.1"),
                                   c("adherence.3","weight.3", "VL.2"),
                                   c("adherence.4","weight.4", "VL.3")),
                  shift = static_binary_off, # abar = c(0,0,0,0,0) 
                  verbose = F,
                  mtp = TRUE, 
                  folds = 3, 
                  outcome_type = "continuous",
                  learners_trt = ll$g, 
                  learners_outcome = ll$Q)

lmtp1

# single time point
lmtp2 <- lmtp_sdr(data = EFV, 
                  trt = "efv.0", 
                  outcome = "VL.0", 
                  baseline = c("sex", "metabolic", "log_age", "NRTI", "weight.0"),
                  shift = static_binary_off, # abar = 0
                  verbose = F,
                  mtp = TRUE, 
                  folds = 3, 
                  outcome_type = "continuous",
                  learners_trt = ll$g, 
                  learners_outcome = ll$Q)

lmtp2

################################################################################
# tmle test
# @Han: ToDo

#
create_hal <- create.Learner("SL.hal",
                                tune=list(runtime=c("default", "fast")),
                                detailed_names = T)
create_hal

create_earth <- create.Learner("SL.earth_boost",
                             tune=list(degree=c(2,3),
                                       penalty=c(2,3)),
                             detailed_names = T)
create_earth

create_step <- create.Learner("SL.step.interaction_boost",
                               tune=list(direction=c("both","forward"),
                                         steps=c(500,1000)),
                               detailed_names = T)
create_step

screen_glmnet_boost <- create.Learner("screen.glmnet_boost", params=list(nfolds=5),
                                     tune = list(
                                       nscreen = c(4,6),
                                       alpha=c(0.8,1)),
                                     detailed_names = T)
screen_glmnet_boost
#


ll <- list(Q=list(
  c("SL.median"),
  c("SL.hal", "screen.glmnet_boost_4_0.8"),
  c("SL.hal_fast"),
  c("SL.glm","screen.glmnet_nVar_6"),
  c("SL.earth_boost_2_2")
),
g=list("SL.glm",
       "SL.step.interaction_boost_both_500"
       )
)
attr(ll$Q, "return.fit") <- TRUE

# tmle only allows binary treatment
# A: VL.0
# Y: efv.1
# L: sex, metabolic, log_age, NRTI, weight.0
tmle_1 <- tmle(Y = EFV[["efv.1"]],
               A = EFV[["VL.0"]],
               W = EFV[,c("sex", "metabolic", "log_age", "NRTI", "weight.0")],
               Q.SL.library = ll$Q,
               g.SL.library = ll$g
)
tmle_1

# A: VL.1
# Y: efv.2
# L: sex, metabolic, log_age, NRTI, weight.0, adherence.1, weight.1, efv.1, VL.0
tmle_2 <- tmle(Y = EFV[["efv.2"]],
               A = EFV[["VL.1"]],
               W = EFV[,c("sex", "metabolic", "log_age", "NRTI", "weight.0",
                          "adherence.1", "weight.1", "efv.1", "VL.0")],
               Q.SL.library = ll$Q,
               g.SL.library = ll$g
)
tmle_2

################################################################################
# tmle3 test
# @Christoph: ToDo

## clone the repositories 

# git2r::clone("https://github.com/tlverse/tlverse", "tlverse")
# git2r::clone("https://github.com/tlverse/sl3", "sl3")
# git2r::clone("https://github.com/tlverse/tmle3", "tmle3")

# make sure that all dependencies are installed -> look at the error messages
# run the installation in the following order since they depend on each other

# install.packages("sl3", repos = NULL, type = "source")
# install.packages("tmle3", repos = NULL, type = "source")
# install.packages("tlverse", repos = NULL, type = "source")

# library(tlverse) -> not necessary
library(sl3)
library(tmle3)

data_EFV <- EFV[, 1:7]
summary(data_EFV$efv.0)
data_EFV$efv.0 <- ifelse(data_EFV$efv.0 > median(data_EFV$efv.0), 1, 0)

node_list <- list(
  W = c("sex", "metabolic", "log_age", "NRTI", "weight.0"),
  A = "efv.0",
  Y = "VL.0"
)

# specify treatment and outcome
ate_spec <- tmle_ATE(
  treatment_level = 1,
  control_level = 0
)

# choose base learners
lrnr_mean <- Lrnr_pkg_SuperLearner$new("SL.mean")  # import from SuperLearner
Lrnr_glm <- Lrnr_pkg_SuperLearner$new("SL.glm")  # import from SuperLearner
lrnr_xgboost <- Lrnr_pkg_SuperLearner$new("SL.glmnet_boost_1") # import from Slbooster -> success, but later can`t find it?
# screen c("SL.glm","screen.glmnet_nVar_6")

# define metalearners appropriate to data types
ls_metalearner <- make_learner(Lrnr_nnls)
mn_metalearner <- make_learner(
  Lrnr_solnp, metalearner_linear_multinomial,
  loss_loglik_multinomial
)
sl_Y <- Lrnr_sl$new(
  learners = list(lrnr_mean, Lrnr_glm, lrnr_xgboost),   # here screen as c(Lrnr_glm, Lrnr_screen_glmnet_nVar_6)  ..., lrnr_xgboost
  metalearner = ls_metalearner,
  outcome_type = 'binomial'
)
sl_A <- Lrnr_sl$new(
  learners = list(lrnr_mean, Lrnr_glm, lrnr_xgboost),
  metalearner = ls_metalearner,
  outcome_type = 'binomial'
)
learner_list <- list(A = sl_A, Y = sl_Y) # implement in 'learner_ist'

# define likelihoods in specific
bounded_factor_list <- list(
  define_lf(LF_emp, "W"), # uses empirical distribution
  define_lf(LF_fit, "A", learner = learner_list[["A"]], bound = c(0.01, 0.99)), # 'bound' sets truncation for'g'
  define_lf(LF_fit, "Y", learner = learner_list[["Y"]], type = 'mean') # type = 'mean' , because we want conditional mean instead of density
)
# create likelihood
bounded_likelihood_def <- Likelihood$new(bounded_factor_list)
#create task
tmle_task <- ate_spec$make_tmle_task(data_EFV, node_list, folds=5) # folds=5 ... default = 10 -> for CVTMLE and SL

bounded_likelihood <- bounded_likelihood_def$train(tmle_task) # calculate first step of tmle

# Lrnr_pkg_SuperLearner_SL.glmnet_boost_1 -> will be dropped

# Error in SL.glmnet_boost(..., alpha = 1, nfolds = 5) : 
# could not find function "SL.glmnet_boost"
# Error in SL.glmnet_boost(..., alpha = 1, nfolds = 5) : 
#  could not find function "SL.glmnet_boost"
# In addition: Warning message:
#  In private$.train(processed_task, trained_sublearners) :
#  Lrnr_pkg_SuperLearner_SL.glmnet_boost_1 failed with message: Error in SL.glmnet_boost(..., alpha = 1, nfolds = 5) : 
#  could not find function "SL.glmnet_boost"
# . It will be removed from the stack

# look in source code for 'sl3' if there are other implementations possible since 'Lrnr_pkg_SuperLearner$new' works only with 'SuperLearner'

print(bounded_likelihood_def)

# check the likelihood values
bounded_likelihood_values <- bounded_likelihood$get_likelihoods(tmle_task) 
head(bounded_likelihood_values)

# 1. compute CV-TMLE
targeted_likelihood <- Targeted_Likelihood$new(bounded_likelihood, updater = list(cvtmle = TRUE))  # implement updater for 2nd step
tmle_params <- ate_spec$make_params(tmle_task, targeted_likelihood) 

cvtmle_fit <- fit_tmle3(tmle_task, targeted_likelihood,
                        tmle_params,targeted_likelihood$updater)
print(cvtmle_fit)
cvtmle_fit$summary
cvtmle_fit$updater

# 2. compute TMLE
targeted_likelihood_no_cv <- Targeted_Likelihood$new(bounded_likelihood, 
                                                     updater = list(cvtmle = FALSE))
tmle_params_no_cv <- ate_spec$make_params(tmle_task, targeted_likelihood_no_cv)

tmle_fit_no_cv <- fit_tmle3(tmle_task, targeted_likelihood_no_cv, 
                            tmle_params_no_cv,targeted_likelihood_no_cv$updater)
print(tmle_fit_no_cv)



                
               
