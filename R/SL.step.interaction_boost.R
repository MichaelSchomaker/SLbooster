SL.step.interaction_boost <- 
  function (Y, X, newX, family, direction = "both", trace = 0, 
            k = 2, verbose=T, ...) 
  {
    if(verbose==T){cat("SL.step.interaction started, direction = ", direction, ". ", sep="")}
    start_time <- Sys.time()
    #
    fit.glm <- glm(Y ~ ., data = X, family = family)
    fit.step <- step(fit.glm, scope = Y ~ .^2, direction = direction, 
                     trace = trace, k = k, ...)
    pred <- predict(fit.step, newdata = newX, type = "response")
    fit <- list(object = fit.step)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.step")
    #
    end_time <- Sys.time()
    if(verbose==T){cat("SL.step.interaction finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
    #
    return(out)
  }