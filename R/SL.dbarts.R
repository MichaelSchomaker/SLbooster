SL.dbarts <- 
  function (Y, X, newX, family, obsWeights = NULL, id = NULL, sigest = NA, sigdf = 3, 
            sigquant = 0.9, k = 2, power = 2.5, base = 0.9, binaryOffset = 0, 
            ntree = 75, ndpost = 200, nskip = 100, printevery = 100, 
            keepevery = 5, keeptrainfits = TRUE, usequants = FALSE, numcut = 100, 
            printcutoffs = 0, nthread = 1, keepcall = TRUE, verb=T, verbose = TRUE, 
            ...) 
  {
    #
    if(verbose==T){cat("SL.dbarts started with ", ntree, " trees and k=", k, ". ", sep="")}
    start_time <- Sys.time()
    SLbooster.require("dbarts")
    #
    model <- suppressWarnings(dbarts::bart(x.train = X, y.train = Y, x.test = newX, 
                              sigest = sigest, sigdf = sigdf, sigquant = sigquant, 
                              k = k, power = power, base = base, binaryOffset = binaryOffset, 
                              weights = obsWeights, ntree = ntree, ndpost = ndpost, 
                              nskip = nskip, printevery = printevery, keepevery = keepevery, 
                              keeptrainfits = keeptrainfits, usequants = usequants, 
                              numcut = numcut, printcutoffs = printcutoffs, nthread = nthread, 
                              keepcall = keepcall, keeptrees = TRUE, verbose = F))

    if (family$family == "gaussian") {
      pred = model$yhat.test.mean
    }
    if (family$family == "binomial") {
      pred = colMeans(stats::pnorm(model$yhat.test))
    }
    fit = list(object = model)
    class(fit) = c("SL.dbarts")
    out = list(pred = pred, fit = fit)
    #
    end_time <- Sys.time()
    if(verbose==T){cat("SL.dbarts finished. Time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n\n")}
    #
    return(out)
  }
