SL.hal <- function (Y, X, newX = NULL, verbose=T, family=stats::gaussian(),
                    runtime=c("default", "fast", "slow", "custom"),
                    custom_params = NULL,
                    obsWeights = rep(1,length(Y)), ...){
  #
  if(verbose==T){cat("SL.hal started. ")}
  start_time <- Sys.time()
  SuperLearner:::.SL.require("hal9001")
  #
  if (all(Y == 0 | Y == 1)) {
    family$family <- "binomial"
  } else {
    family$family <- "gaussian"
  }
  #
  preprocess_data <- function(data) {
    data <- data.frame(lapply(data, function(x) {
        as.numeric(x) 
    }), stringsAsFactors = FALSE)
    return(as.matrix(data)) 
  }
  #
  X <- preprocess_data(X)
  if (!is.null(newX)) {
    newX <- preprocess_data(newX)
  }
  #
  runtime <- match.arg(runtime, choices = c("default", "fast", "slow", "custom"))
  runtime_params <- switch(runtime,
                           "default" = list(
                             max_degree = 2,
                             base_num_knots_0 = 200,
                             base_num_knots_1 = 50,
                             smoothness_order = 1
                           ),
                           "fast" = list(
                             max_degree = 1,
                             base_num_knots_0 = min(nrow(X), 200),
                             smoothness_order = 0
                           ),
                           "slow" = list(
                             max_degree = ifelse(ncol(X) >= 20, 2, 3), 
                             base_num_knots_0 = 500,
                             base_num_knots_1 = 200,
                             smoothness_order = 2
                           ),
                           "custom" = custom_params  # Allow user-defined parameters
    )
  #
  fit.hal <- try(
    hal9001::fit_hal(
      X = X, 
      Y = Y, 
      family = family$family,
      smoothness_order = runtime_params$smoothness_order,
      max_degree = runtime_params$max_degree,
      num_knots = hal9001:::num_knots_generator(
        max_degree = runtime_params$max_degree,
        smoothness_order = runtime_params$smoothness_order,
        base_num_knots_0 = runtime_params$base_num_knots_0,
        base_num_knots_1 = runtime_params$base_num_knots_1
      ),
      fit_control = list(
        cv_select = TRUE,
        nfolds = 10,
        weights = obsWeights,
        foldid = NULL,
        use_min = TRUE,
        lambda.min.ratio = 1e-04,
        prediction_bounds = "default"
      )
    ), 
    silent = FALSE
  )
  #
  if(class(fit.hal)[1]=="try-error"){
    if(verbose==T){cat("Technical failure: GLM used instead of HAL.")}
    out <- SuperLearner::SL.glm(Y=Y,X=X,newX=newX,family=family, obsWeights=rep(1,length(Y)))
  }else{
    if (!is.null(newX)) {
      pred <- stats::predict(fit.hal, new_data = newX)
    }
    else {
      pred <- stats::predict(fit.hal, new_data = X)
    }
    fit <- list(object = fit.hal)
    class(fit) <- "SL.hal"
    out <- list(pred = pred, fit = fit)
  }
  #
  end_time <- Sys.time()
  if(verbose==T){cat("SL.hal finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
  #
  return(out)
}