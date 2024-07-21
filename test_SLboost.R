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

################################################################################
# tmle3 test
# @Christoph: ToDo





                
               