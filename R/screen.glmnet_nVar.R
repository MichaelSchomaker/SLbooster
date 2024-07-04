screen.glmnet_nVar <- function(Y, X, family = list(), alpha = 1, nfolds = 5,
                                    nlambda = 150, nVar = 6, verbose = T, ...) {
  if(verbose==T){cat("screen.glmnet_nVar with alpha=", alpha, ", ", nfolds, " fold CV and selecting ", nVar,  " variables \n", sep="")}
  if(nVar<1){stop("nVar needs to be greater or equal to one")}
  start_time <- Sys.time()
  SuperLearner:::.SL.require("glmnet")
  # relevant for column names but shouldnt be a matrix anyways
  X <- as.data.frame(X)
  
  # chose family dependent upon response
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- "binomial"
  } else {
    family$family <- "gaussian"
  }
  saveY<-Y;saveX<-X
  # needed for var names to select from levels of factors later on
  if (ncol(X) > 26 * 27) stop("Find further column names for X!")
  let <- c(letters, sort(do.call("paste0", expand.grid(letters, letters[1:26]))))
  names(X) <- let[1:ncol(X)]
  
  # factors are coded as dummies which are standardized in cv.glmnet()
  # intercept is not in model.matrix() because its already in cv.glmnet()
  is_fact_var <- sapply(X, is.factor)
  X <- try(model.matrix(~ -1 + ., data = X), silent = FALSE)
  
  # cv.glmnet() calls glmnet(), thus arguments are given to glmnet()
  if (ncol(X) <= nVar) {if(verbose==T){cat("Note: ",nVar,"variables are not available in the data (modify setup!). Set nVar=1 for now. \n")}; nVar<- 1}
  successfulfit <- FALSE 
  fitCV <- try(glmnet::cv.glmnet(
    x = X, y = Y, lambda = NULL, type.measure = "deviance",
    nfolds = nfolds, family = family$family, alpha = alpha,
    nlambda = nlambda, keep = T
  ), silent = TRUE)
  # if no variable was selected, penalization might have been too strong, try log(lambda)
  if (all(fitCV$nzero == 0) | all(is.na(fitCV$nzero))) {
    fitCV <- try(glmnet::cv.glmnet(
      x = X, y = Y, lambda = log(fitCV$glmnet.fit$lambda + 1), type.measure = "deviance",
      nfolds = nfolds, family = family$family, alpha = alpha, keep = T
    ), silent = TRUE)
  }
  if(class(fitCV)=="try-error"){successfulfit <- FALSE}else{successfulfit <- TRUE}
  whichVariable <- NULL
  if(successfulfit==TRUE){
    # if nVar is not available, take the closest to nVar available
    if (all(fitCV$nzero != nVar)) {
      lambda_index_with_nVar <- min(which(abs(fitCV$nzero - nVar) == min(abs(fitCV$nzero - nVar))))
      # nVar is available
    } else if (any(fitCV$nzero == nVar)) {
      lambda_index_with_nVar <- min(which(fitCV$nzero == nVar))
    }
    coefs <- coef(fitCV$glmnet.fit, s = fitCV$glmnet.fit$lambda[lambda_index_with_nVar])
    var_nms <- coefs@Dimnames[[1]]
    
    # Instead of Group Lasso:
    # If any level of a dummy coded factor is selected, the whole factor is selected
    if (any(is_fact_var)) {
      nms_fac <- names(which(is_fact_var))
      is_selected <- coefs[-1] != 0 # drop intercept
      # model.matrix adds numbers to dummy coded factors which we need to get rid of
      var_nms_sel <- gsub("[^::a-z::]", "", var_nms[-1][is_selected])
      sel_fac <- nms_fac[nms_fac %in% var_nms_sel]
      sel_numer <- var_nms_sel[!var_nms_sel %in% sel_fac]
      all_sel_vars <- c(sel_fac, sel_numer)
      whichVariable <- names(is_fact_var) %in% all_sel_vars
    } else {
      # metric variables only
      whichVariable <- coefs[-1] != 0
    }
    if (nVar != sum(whichVariable) & verbose==T) {cat("Note: exactly", nVar, "variables could not be screened, instead", sum(whichVariable), "vars were screened. \n")}
  }
  #
  if(is.null(whichVariable)){
    whichVariable<-screen.cramersv(Y,X)
    if(verbose==T){cat("Lasso failed and screening was based on Cramer's V\n")}}
  #
  if(verbose==T){cat("screened ",sum(whichVariable)," variables: ",paste(colnames(saveX)[whichVariable]),"\n",sep=" ");
    cat("sample size:",dim(X)[1],"; ")}
  end_time <- Sys.time()
  if(verbose==T){cat("time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n")}
  #
  return(whichVariable)
}