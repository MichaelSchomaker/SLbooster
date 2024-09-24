library(SuperLearner)
library(SLbooster)
data(EFV)

 
################################################################################

# create Learner
# from SuperLearner package
create_glmnet <- create.Learner("SL.glmnet", params=list(nfolds=5),
                                            tune=list(alpha=c(0.8,1)),
                                            detailed_names = T)
create_glmnet
create_glmnet$names

# updated version from SLbooster package (default: verbose=TRUE)
create_glmnet_boost <- create.Learner("SL.glmnet_boost", params=list(nfolds=5),
                                tune=list(alpha=c(0.8,1)),
                                detailed_names = T)

# create screening algorithm from SLbooster package (default: verbose=TRUE)
screen_glmnet_nvar <- create.Learner("screen.glmnet_nVar", tune = list(nVar=c(4:6)),
                                     detailed_names = T)


################################################################################

# define learner list
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
sl1 <- SuperLearner(Y=EFV$weight.4, X=EFV[,1:20],verbose=F,SL.library=ll$Q)
sl1$coef
sl1$whichScreen

# Super Learning Test (family=binomial)
sl2 <- SuperLearner(Y=EFV$VL.4, X=EFV[,1:22],verbose=F,SL.library=ll$Q, family="binomial")
sl2$coef
sl2$whichScreen

################################################################################
# ltmle test
# binary outcome
library(ltmle)
ltmle1 <- ltmle(EFV,
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
# The g-models (SL.glm) give no output
# Note: with longitudinal TMLE, as of the second iteration, the iterated outcome is proportional data rather than 0/1 data
ltmle1$estimates
ltmle1$fit$Q

# small test for cont. Y (illustrative, no treatment effect)
ltmle2 <- ltmle(EFV[,1:9],
                Lnodes  = c("VL.0","adherence.1"),
                Anodes  = c("efv.0"),
                Ynodes  = c("weight.1"),
                abar=0, SL.cvControl = list(V=5),
                estimate.time=F, gcomp=F,
                SL.library= ll
)
ltmle2$estimates
ltmle2$fit$Q

################################################################################
# lmtp test
library(lmtp)

# multiple time points
lmtp1 <- lmtp_tmle(data = EFV2,
                   trt = c("efv.0","efv.1","efv.2","efv.3","efv.4"), 
                   outcome = c("VL.4"), 
                   baseline = c("sex", "metabolic", "log_age", "NRTI"),
                   time_vary = list(c("weight.0"),
                                    c("adherence.1","weight.1", "VL.0"),
                                    c("adherence.2","weight.2", "VL.1"),
                                    c("adherence.3","weight.3", "VL.2"),
                                    c("adherence.4","weight.4", "VL.3")),
                   shift = static_binary_off, # abar = c(0,0,0,0,0) 
                   folds = 1,
                   outcome_type = "binomial",
                   learners_trt = ll$g, 
                   learners_outcome = ll$Q,
                   control = lmtp_control(.learners_outcome_folds = 5, .learners_trt_folds = 5)
)
lmtp1
# results differ quite a lot from ltmle, but confidence interval is wide, so it might be okay?
# Michael: to discuss after update

# continuous Y
lmtp2 <- lmtp_sdr(data = EFV[,1:10], 
                  trt = "VL.0", 
                  outcome = "efv.1", 
                  baseline = c("sex", "metabolic", "log_age", "NRTI", "weight.0"),
                  time_vary = list(c("adherence.1","weight.1")),
                  shift = static_binary_off, # abar = 0
                  folds = 1, 
                  outcome_type = "continuous",
                  learners_trt = ll$g, 
                  learners_outcome = ll$Q,
                  control = lmtp_control(.learners_outcome_folds = 5, .learners_trt_folds = 5)
)
lmtp2

################################################################################
# tmle test

library(tmle)
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

  # @Han: ToDo

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







