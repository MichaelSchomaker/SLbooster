SL.orm <- function (Y, X, newX, verbose = TRUE, ...) {
  # Check if outcome is binary
  if (all(Y == 0 | Y == 1)) {
    if (verbose == TRUE) {
      cat("SL.orm started.\n")
    }
    start_time <- Sys.time()
    if (verbose == TRUE) {
      cat("Binary outcome: GLM used instead of ORM.\n")
    }
    obsWeights = rep(1, length(Y))
    out <- SuperLearner::SL.glm(Y = Y, X = X, newX = newX, family = binomial(), obsWeights = obsWeights, ...)
  } else {
    if (verbose == TRUE) {
      cat("SL.orm started.\n")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("rms")
    
    # Check if there are enough unique values
    if (length(unique(Y)) < 5) {
      # Use SL.mean if not enough unique values in Y
      out <- SuperLearner::SL.mean(Y = Y, X = X, newX = newX, obsWeights = obsWeights, ...)
      
      if (verbose == TRUE) {
        cat("Not enough different unique values in Y: mean used instead of ORM.\n")
      }
    } else {
      # Combine outcome and predictors into one dataframe
      XX <- as.data.frame(cbind(Y, X))
      
      # Fit the ORM model
      fit.orm <- try(rms::orm(Y ~ ., data = XX), silent = TRUE)
      
      # Check if ORM model fit was successful
      if (length(class(fit.orm)) == 2 && fit.orm$fail == FALSE) {
        if (is.matrix(newX)) {
          newX <- as.data.frame(newX)
        }
        
        # Predict with fitted ORM model
        pred <- rms:::predict.orm(fit.orm, newdata = newX, type = "mean")
        fit <- list(object = fit.orm)
        class(fit) <- "SL.orm"
        out <- list(pred = pred, fit = fit)
      } else {
        # If ORM fails, fallback to GLM
        out <- SuperLearner::SL.glm(Y = Y, X = X, newX = newX, obsWeights = obsWeights, ...)
        
        if (verbose == TRUE) {
          cat("Technical problem with ORM: GLM used instead.\n")
        }
      }
    }
  }
  
  end_time <- Sys.time()
  if (verbose == TRUE) {
    cat("SL.orm finished. Time:", round(difftime(end_time, start_time, units = "mins"), digits = 4), "mins\n\n")
  }
  
  return(out)
}



