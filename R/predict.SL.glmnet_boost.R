predict.SL.glmnet_boost <- function(object, newdata, remove_extra_cols = T, add_missing_cols = T, 
                                     verbose=T, ...){
    SLbooster.require("glmnet")
    if (!is.matrix(newdata)) {
      newdata <- model.matrix(~-1 + ., newdata)
    }
    original_cols = rownames(object$object$glmnet.fit$beta)
    if (remove_extra_cols) {
      extra_cols = setdiff(colnames(newdata), original_cols)
      if (length(extra_cols) > 0) {
        warning(paste("Removing extra columns in prediction data:", 
                      paste(extra_cols, collapse = ", ")))
        newdata = newdata[, !colnames(newdata) %in% extra_cols, 
                          drop = FALSE]
      }
    }
    if (add_missing_cols) {
      missing_cols = setdiff(original_cols, colnames(newdata))
      if (length(missing_cols) > 0) {
        warning(paste("Adding missing columns in prediction data:", 
                      paste(missing_cols, collapse = ", ")))
        new_cols = matrix(0, nrow = nrow(newdata), ncol = length(missing_cols))
        colnames(new_cols) = missing_cols
        newdata = cbind(newdata, new_cols)
        newdata = newdata[, original_cols, drop = FALSE]
      }
    }
    pred <- predict(object$object, newx = newdata, type = "response", 
                    s = ifelse(object$useMin, "lambda.min", "lambda.1se"))
    if(object$fam.init=="binomial" & object$fam.end=="gaussian"){
      if(any(pred<0)){pred[pred<0]<-0}
      if(any(pred>1)){pred[pred>1]<-1}
      if((any(pred<0) | any(pred>1)) & verbose==T){cat("\n Note: predictions falling outside [0,1] have been set as 0/1 \n")}
    }
    return(pred)
  }