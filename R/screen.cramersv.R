screen.cramersv <- function(Y, X, nscreen = 4, num_cat = 10, verbose = T, ...) {
  if(verbose==T){cat("screen.cramersv for", nscreen, "variables; ")}
  start_time <- Sys.time()
  SuperLearner:::.SL.require("vcd")
  #
  if (ncol(X) > nscreen) {
    dat <- cbind(Y, X)
    contin_var <- apply(dat, 2, function(var) length(unique(var)) > num_cat)
    make_categ <- function(var) {
      num_qu <- length(unique(quantile(var, prob = seq(0, 1, 0.2))))
      if(num_qu >2){ret<-cut(var, unique(quantile(var, prob = seq(0, 1, 0.2))), include.lowest = T)}
      if(num_qu<=2){ret<-cut(var, c(-Inf,unique(quantile(var, prob = seq(0, 1, 0.2))),Inf), include.lowest = T)}
      ret
    }
    if (any(contin_var)) {
      dat[, contin_var] <- apply(dat[, contin_var, drop = FALSE], 2, make_categ)
    }
    calc_cram_v <- function(x_var, y_var) vcd::assocstats(table(y_var, x_var))$cramer
    cramers_v <- apply(dat[, !colnames(dat) %in% "Y"], 2, calc_cram_v, y_var = dat[, "Y"])
    if(verbose==T){cat("screened:",colnames(X)[unname(rank(-cramers_v) <= nscreen)],"\n",sep=" ");
      cat("sample size:",dim(X)[1],"; "); 
    }
    whichVariable <- unname(rank(-cramers_v) <= nscreen)
  }else{
    if(verbose==T){cat("screened all", ncol(X), "variables \n")}
    whichVariable <- rep(TRUE, ncol(X))}
  end_time <- Sys.time()
  if(verbose==T){cat("time:", round(difftime(end_time, start_time, units="mins"), digits=4), "mins \n")}
  return(whichVariable)
}
