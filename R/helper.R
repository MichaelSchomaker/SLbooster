SLbooster.require <- function(package, message = paste("loading required package (", 
                                                       package, ") failed", sep = "")) {
  if (!requireNamespace(package, quietly = FALSE)) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

