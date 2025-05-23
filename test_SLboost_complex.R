library(SuperLearner)
library(SLbooster)
library(CICI); library(CICIplus) #remotes::install_github("MichaelSchomaker/CICIplus")
library(simcausal)
data(EFV)

###########################
# Data generating process #
###########################
t.end <- 2 
# initialize the DAG
D <- DAG.empty()

# everything at baseline (t = 0)
D.base <- D +
  node("L1",
       t = 0,
       distr = "rbern",
       prob=0.3) +
  node("L2",
       t = 0,
       distr = "rnorm",
       mean = 2*L1[0]-1,
       sd = 1) +
  node("A",
       t = 0,
       distr = "rnorm",
       mean = 7 + L1[0] + 0.7*L2[0],
       sd=1 ) +
  node("Y",
       t = 0,
       distr = "rbern",
       prob = plogis(-4 + 0.5*L2[t] + 0.2*A[t]),
       EFU = T)

#time-dependent variables at later time-points
D <- D.base +
  node("L1",
       t = 1:t.end,
       distr = "rbern",
       prob = plogis(-4 + L1[t-1] + 0.15*L2[t-1] + 0.15*A[t-1] + Y[t-1])) +
  node("L2",
       t = 1:t.end,
       distr = "rnorm",
       mean = 0.5*L1[t]+0.25*L2[t-1]+0.5*A[t-1]+0.2*Y[t-1],
       sd = 1) +
  node("A",
       t = 1:t.end,
       distr = "rnorm",
       mean =  A[t-1] + L1[t] - .15*L2[t],
       sd=0.25) +
  node("C",                                   
       t = 1:t.end,
       distr = "rbern",
       prob = plogis(-2 + 0.5*L1[t] - 0.2*A[t]),
       EFU = T) + 
  node("Y",
       t = 1:t.end,
       distr = "rbern",
       prob = plogis(-2 - 0.5*L2[t] + 0.2*A[t]),
       EFU = T)

Dset <- set.DAG(D) 

#######################
# define learner list #
#######################
create.Learner("SL.hal",params=list(runtime="fast"),detailed_names = F, verbose=T)
create.Learner("SL.mgcv",params=list(by="L1_0"),detailed_names = F, verbose=T)

ll <- list(
  c("SL.median"),
  c("SL.mean"),
  c("SL.glm"),
  c("SL.glm","screen.glmnet_nVar"),
  c("SL.glm","screen.glmnet_boost"),
  c("SL.glm","screen.cramersv"),
  c("SL.glm","screen.randomForest_boost"), 
  c("SL.glmnet_boost"),
  c("SL.dbarts"), 
  c("SL.earth_boost"),
  c("SL.gam_boost"),
  c("SL.hal_1","screen.cramersv"), 
  c("SL.mgcv_1","screen.cramersv"), 
  c("SL.orm"), 
  c("SL.randomForest_boost"),  
  c("SL.rpart_boost"),
  c("SL.step.interaction_boost","screen.cramersv"),
  c("SL.xgboost_boost","screen.cramersv") 
)

# needed when using parallelization (and learners not part of SuperLearner)
base.learners <- c("screen.glmnet_nVar", "screen.cramersv", "screen.glmnet_boost",
                   "screen.randomForest_boost",
                   #
                   "SL.glmnet_boost","SL.mgcv", "SL.earth_boost","SL.orm",
                   "SL.rpart_boost","SL.randomForest_boost", "SL.xgboost_boost",
                   "SL.step.interaction_boost","SL.dbarts","SL.gam_boost",
                   "SL.hal",
                   #
                   "predict.SL.mgcv", "predict.SL.orm","predict.SL.rpart_boost",
                   "predict.SL.dbarts","predict.SL.step.interaction_boost",
                   "predict.SL.glmnet_boost","predict.SL.hal","predict.SL.median",
                   "predict.SL.earth_boost"
)

##################
### estimation ###
##################

# draw data
N=500
simdat   <- suppressWarnings(simcausal::sim(DAG = Dset, n = N, verbose=F)[,-1])

cpus=1 # output messages with cpus=1, faster with >1
est_sgf <- try(sgf(X=simdat,
                      Lnodes  = colnames(simdat)[grep("L",colnames(simdat))][-c(1,2)],
                      Ynodes  = colnames(simdat)[grep("Y",colnames(simdat))],
                      Anodes  = colnames(simdat)[grep("A",colnames(simdat))],
                      Cnodes  = colnames(simdat)[grep("C",colnames(simdat))],
                      survivalY=TRUE,
                      abar =  seq(4,8,1), 
                      SL.library = ll, SL.export=base.learners,
                      Yweights = NULL, 
                      ncores=cpus, verbose=FALSE, seed=NULL
))
est_sgf
plot(est_sgf) # side note: some "complex learners" make things worse (in this example) 
est_sgf$SL.weights




# BONUS: weighted outcome version
cs            <- c(2,1,0.01,0.001) 
w.d <- calc.weights(X=simdat,
                    Lnodes  = colnames(simdat)[grep("L",colnames(simdat))],
                    Ynodes  = colnames(simdat)[grep("Y",colnames(simdat))],
                    Anodes  = colnames(simdat)[grep("A",colnames(simdat))],
                    Cnodes  = colnames(simdat)[grep("C",colnames(simdat))],
                    abar=seq(4,8,1), c=cs,
                    survival = TRUE, 
                    d.method=c("parametric")
)


est_wsgf <- try(sgf(X=simdat,
                   Lnodes  = colnames(simdat)[grep("L",colnames(simdat))][-c(1,2)],
                   Ynodes  = colnames(simdat)[grep("Y",colnames(simdat))],
                   Anodes  = colnames(simdat)[grep("A",colnames(simdat))],
                   Cnodes  = colnames(simdat)[grep("C",colnames(simdat))],
                   survivalY=TRUE,
                   abar =  seq(4,8,1), 
                   SL.library = ll, SL.export=base.learners,
                   Yweights = w.d$`0.01`, 
                   ncores=cpus, verbose=FALSE, seed=NULL
))

est_wsgf
plot(est_wsgf)
est_wsgf$SL.weights


