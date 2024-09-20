
SL.orm <- function (Y, X, newX = NULL, verbose = T, ...) {
  # Check if outcome is binary numeric or binary factor
  is_binary_numeric <- is.numeric(Y) && all(Y == 0 | Y == 1)
  is_binary_factor <- is.factor(Y) && length(levels(Y)) == 2
  
  if (is_binary_numeric || is_binary_factor) {
    if (verbose == T) {
      cat("SL.orm started.\n")
      if (is_binary_numeric) {
        cat("Binary numeric outcome: GLM used instead of ORM.\n")
      } else {
        cat("Binary factor outcome: GLM used instead of ORM.\n")
      }
    }
    start_time <- Sys.time()
    
    obsWeights <- rep(1, length(Y))
    out <- SuperLearner::SL.glm(Y = Y, X = X, newX = newX, family = binomial(), obsWeights = obsWeights, ...)
  } else {
    if (verbose == T) {
      cat("SL.orm started.\n")
    }
    start_time <- Sys.time()
    SuperLearner:::.SL.require("rms")
    
    # Determine the number of unique values based on Y's type
    if (is.factor(Y)) {
      unique_vals <- length(levels(Y))
    } else {
      unique_vals <- length(unique(Y))
    }
    
    # Check if there are enough unique values
    if (unique_vals < 3) {
      # Use SL.mean if not enough unique values in Y
      obsWeights <- rep(1, length(Y))
      out <- SuperLearner::SL.mean(Y = Y, X = X, newX = newX, obsWeights = obsWeights, ...)
      
      if (verbose == T) {
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
        if(is.factor(Y)) {
          pred <- rms:::predict.orm(fit.orm, newdata = newX) 
        } else {
          pred <- rms:::predict.orm(fit.orm, newdata = newX, type = "mean") 
        }
        fit <- list(object = fit.orm)
        class(fit) <- "SL.orm"
        out <- list(pred = pred, fit = fit)
      } else {
        # If ORM fails, fallback to GLM
        out <- SuperLearner::SL.glm(Y = Y, X = X, newX = newX, obsWeights = rep(1, length(Y)), ...)
        
        if (verbose == T) {
          cat("Technical problem with ORM: GLM used instead.\n")
        }
      }
    }
  }
  
  end_time <- Sys.time()
  if (verbose == T) {
    cat(
      "SL.orm finished. Time:", 
      round(difftime(end_time, start_time, units = "mins"), digits = 4), 
      "mins\n\n"
    )
  }
  
  return(out)
}


